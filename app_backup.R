# ==================================================================
#   Sentinel Garden Survey + iNaturalist integration
# ==================================================================

# --- 1. Load Libraries ---
library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(DT)
library(tibble)
library(httr)
library(jsonlite)
library(readr)
library(leaflet)
library(bslib)

# --- 2. Configuration & Helper Functions ---
db_path <- "plant_records.db"

connect_db <- function() {
  dbConnect(RSQLite::SQLite(), db_path)
}

ensure_db <- function() {
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"observations" %in% dbListTables(con)) {
    dbExecute(
      con,
      "CREATE TABLE observations (
        timestamp TEXT,
        observer_name TEXT,
        include_name TEXT,
        observer_email TEXT,
        site_name TEXT,
        grid_cell TEXT,
        latitude REAL,
        longitude REAL,
        species_name TEXT,
        spread_beyond TEXT,
        spread_mode TEXT,
        control_effectiveness TEXT,
        control_methods TEXT,
        disposal_methods TEXT,
        introduction_routes TEXT,
        source_of_plant TEXT,
        outside_garden TEXT,
        warning_label TEXT,
        outcompeted TEXT,
        coverage_dafor TEXT,
        notes TEXT,
        inat_id TEXT
      );"
    )
  }
}

load_data <- function() {
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)
  
  if (!"observations" %in% dbListTables(con)) {
    return(data.frame())
  }
  
  df <- dbReadTable(con, "observations")
  as.data.frame(df, stringsAsFactors = FALSE)
}

KEY_FILE <- file.path("data", "plantnet_key.txt")
PLANTNET_KEY <- if (file.exists(KEY_FILE)) trimws(readr::read_file(KEY_FILE)) else ""

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0) a else b
}

plantnet_identify <- function(img_path, api_key) {
  res <- httr::POST(
    url   = "https://my-api.plantnet.org/v2/identify/all",
    query = list(
      `api-key`                = api_key,
      `include-related-images` = "true",
      lang                     = "en"
    ),
    body  = list(
      images = httr::upload_file(img_path)
    ),
    encode  = "multipart",
    httr::timeout(45)
  )
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
}

plantnet_parse <- function(payload, top_n = 5) {
  res <- payload$results
  if (is.null(res) || nrow(res) == 0) return(tibble())
  
  res <- head(res, top_n)
  
  common_names <- vapply(
    res$species$commonNames,
    function(x) if (is.null(x) || length(x) == 0) "-" else paste(x, collapse = ", "),
    FUN.VALUE = character(1)
  )
  
  family_names <- if (!is.null(res$species$family$scientificNameWithoutAuthor)) {
    res$species$family$scientificNameWithoutAuthor
  } else {
    rep("", nrow(res))
  }
  
  image_urls <- vapply(
    seq_len(nrow(res)),
    function(i) {
      imgs <- res$images[[i]]
      if (is.null(imgs) || length(imgs) == 0) return(NA_character_)
      vals <- try(unlist(imgs, use.names = FALSE), silent = TRUE)
      if (inherits(vals, "try-error") || is.null(vals) || length(vals) == 0) {
        return(NA_character_)
      }
      char_vals <- as.character(vals)
      http_vals <- char_vals[grepl("^https?://", char_vals)]
      if (length(http_vals) == 0) return(NA_character_)
      http_vals[1]
    },
    FUN.VALUE = character(1)
  )
  
  tibble(
    Score           = round(res$score, 3),
    Scientific.Name = res$species$scientificNameWithoutAuthor,
    Common.Names    = common_names,
    Family          = family_names,
    ImageURL        = image_urls
  )
}

# --- iNaturalist OAuth config ---
INAT_CLIENT_ID     <- Sys.getenv("INAT_CLIENT_ID")
INAT_CLIENT_SECRET <- Sys.getenv("INAT_CLIENT_SECRET")
INAT_REDIRECT_URI  <- Sys.getenv("INAT_REDIRECT_URI")

INAT_AUTHORIZE_URL <- "https://www.inaturalist.org/oauth/authorize"
INAT_TOKEN_URL     <- "https://www.inaturalist.org/oauth/token"
INAT_OBS_URL       <- "https://www.inaturalist.org/observations.json"
INAT_OBS_PHOTO_URL <- "https://www.inaturalist.org/observation_photos.json"
INAT_ADMIN_CODE    <- Sys.getenv("INAT_ADMIN_CODE")
INAT_PROJECT_ID    <- Sys.getenv("INAT_PROJECT_ID")

# --------------------------------------------------------------------------
# Create an iNaturalist observation and optionally upload a photo
# Uses the OAuth access token
# --------------------------------------------------------------------------
push_to_inaturalist <- function(token,
                                species_name,
                                loc   = NULL,
                                notes = NULL,
                                photo = NULL) {
  if (is.null(token)) {
    warning("No iNaturalist token (NULL), skipping upload")
    return(NA_character_)
  }
  
  access_token <- tryCatch(
    {
      at <- token$access_token
      if (is.null(at) || length(at) == 0) return("")
      as.character(at[1])
    },
    error = function(e) ""
  )
  
  if (!nzchar(access_token)) {
    warning("No iNaturalist access_token, skipping upload")
    return(NA_character_)
  }
  
  body <- list(
    "observation[species_guess]"      = species_name,
    "observation[observed_on_string]" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "observation[time_zone]"          = "UTC",
    "observation[description]"        = notes %||% ""
  )
  
  if (!is.null(loc) && !is.null(loc$lat) && !is.null(loc$lon)) {
    body[["observation[latitude]"]]  <- loc$lat
    body[["observation[longitude]"]] <- loc$lon
  }
  
  res <- try(
    httr::POST(
      url    = INAT_OBS_URL,
      httr::add_headers(Authorization = paste("Bearer", access_token)),
      body   = body,
      encode = "form",
      httr::timeout(30)
    ),
    silent = TRUE
  )
  
  if (inherits(res, "try-error")) {
    warning("iNaturalist /observations POST failed (network / httr error)")
    return(NA_character_)
  }
  
  status      <- httr::status_code(res)
  content_txt <- httr::content(res, "text", encoding = "UTF-8")
  
  if (status >= 300L) {
    warning(
      sprintf(
        "iNaturalist /observations POST failed (%s): %s",
        status,
        substr(content_txt, 1, 200)
      )
    )
    return(NA_character_)
  }
  
  obs_id   <- NA_character_
  obs_json <- NULL
  
  if (nzchar(content_txt)) {
    obs_json <- try(jsonlite::fromJSON(content_txt), silent = TRUE)
  }
  
  if (!inherits(obs_json, "try-error") && !is.null(obs_json)) {
    if (!is.null(obs_json$id)) {
      obs_id <- as.character(obs_json$id[1])
    } else if (is.list(obs_json) &&
               length(obs_json) > 0 &&
               !is.null(obs_json[[1]]$id)) {
      obs_id <- as.character(obs_json[[1]]$id)
    }
  }
  
  if (!nzchar(obs_id)) {
    message("Created iNaturalist observation but couldn’t parse ID")
    return(NA_character_)
  } else {
    message("Created iNaturalist observation ID: ", obs_id)
  }
  
  if (!is.null(photo) &&
      !is.null(photo$datapath) &&
      nzchar(photo$datapath) &&
      file.exists(photo$datapath)) {
    
    photo_body <- list(
      "observation_photo[observation_id]" = obs_id,
      file = httr::upload_file(photo$datapath)
    )
    
    res2 <- try(
      httr::POST(
        url    = INAT_OBS_PHOTO_URL,
        httr::add_headers(Authorization = paste("Bearer", access_token)),
        body   = photo_body,
        encode = "multipart",
        httr::timeout(30)
      ),
      silent = TRUE
    )
    
    if (inherits(res2, "try-error")) {
      warning("iNaturalist /observation_photos POST failed (network / httr error)")
    } else {
      status2      <- httr::status_code(res2)
      content_txt2 <- httr::content(res2, "text", encoding = "UTF-8")
      if (status2 >= 300L) {
        warning(
          sprintf(
            "iNaturalist /observation_photos POST failed (%s): %s",
            status2,
            substr(content_txt2, 1, 200)
          )
        )
      } else {
        message("Attached photo to iNaturalist observation ID: ", obs_id)
      }
    }
  } else {
    message("No photo available or file not found; skipping photo upload")
  }
  
  return(obs_id)
}

# --- 3. UI ---
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('inat_redirect', function(url) {
        window.location = url;
      });
    "))
  ),
  
  fluidRow(
    column(
      12,
      div(
        class = "welcome-header",
        tags$img(src = "logo.png", height = "80px"),
        h3("Sentinel Garden Survey")
      ),
      p(
        "Help us understand how garden plants behave and spread. ",
        "This app lets you identify plants with PlantNet, answer a few questions ",
        "about how they behave in your garden, and share anonymised records."
      ),
      hr()
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "User",
      value = "user_details",
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Step 1 – Connect with iNaturalist"),
            p(
              "If you connect your iNaturalist account, each record you submit here ",
              "will also be uploaded as an observation to your own iNaturalist account."
            ),
            div(
              class = "inat-connect-block",
              actionButton(
                "inat_login_btn",
                "Connect to iNaturalist",
                icon  = icon("leaf"),
                class = "btn-success btn-full-width"
              ),
              br(),
              div(
                class = "inat-status-container",
                uiOutput("inat_status_ui")
              ),
              br(),
              div(
                class = "inat-project-container",
                uiOutput("inat_project_ui")
              )
            ),
            br(),
            p(
              "Don’t have an iNaturalist account yet? ",
              tags$a(
                "Create one here",
                href   = "https://www.inaturalist.org/signup",
                target = "_blank"
              )
            ),
            p(
              class = "text-muted",
              "You can also continue without connecting – your records will be stored ",
              "in this project only and not uploaded to iNaturalist."
            ),
            
            hr(),
            
            h4("Step 2 – About you (optional)"),
            p(
              "You can optionally share your name and email with the project. ",
              "This helps us follow up about your records but is not required."
            ),
            textInput("observer_name", "Your name (optional)"),
            textInput("observer_email", "Your email address (optional)"),
            radioButtons(
              "include_name",
              "Can we include your name in our archive of plant records?",
              choices  = c("Yes", "No, I’d prefer my records to be anonymous"),
              selected = "Yes",
              inline   = TRUE
            ),
            hr(),
            
            h4("Terms & conditions"),
            p("Please read the terms and conditions before using the app."),
            tags$a(
              "Download terms and conditions (PDF)",
              href   = "terms_and_conditions.pdf",
              target = "_blank"
            ),
            br(), br(),
            div(
              class = "fullwidth-checkbox",
              checkboxInput(
                "accept_terms",
                "I have read and agree to the terms and conditions",
                value = FALSE
              )
            ),
            hr(),
            div(
              class = "button-container-full-width",
              actionButton("welcome_next", "Next ▶", class = "btn-full-width")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "ID",
      value = "get_id",
      fluidRow(
        column(
          12,
          div(
            id = "record_inputs",
            wellPanel(
              h4("Upload or take a photo"),
              div(
                class = "photo-input-buttons",
                fileInput(
                  "photo_capture",
                  label = NULL,
                  multiple = FALSE,
                  accept = "image/*",
                  capture = "environment",
                  buttonLabel = tagList(icon("camera"), "Take Photo")
                ),
                fileInput(
                  "photo_upload",
                  label = NULL,
                  multiple = FALSE,
                  accept = "image/*",
                  buttonLabel = tagList(icon("images"), "Upload from Library")
                )
              ),
              div(
                class = "button-container-full-width",
                actionButton(
                  "id_btn",
                  "Identify with PlantNet",
                  class = "btn-success btn-full-width",
                  icon  = icon("leaf")
                )
              ),
              hr(),
              h5("Identification Results"),
              uiOutput("id_results_ui"),
              hr(),
              
              h4("Location & plant name"),
              textInput(
                "site_name",
                "Where did you survey? (town/village and site name)",
                placeholder = "e.g., North Meadow, Truro"
              ),
              textInput(
                "grid_cell",
                "Grid cell (optional)",
                placeholder = "e.g., SW1234"
              ),
              textInput(
                "species_name",
                "Plant name (edit if you disagree with the PlantNet ID)",
                placeholder = "Select from PlantNet results or enter manually"
              ),
              div(
                class = "button-container-full-width",
                actionButton(
                  "get_location_btn",
                  "Use my GPS location",
                  class = "btn-outline-info btn-full-width",
                  icon  = icon("location-arrow")
                ),
                actionButton(
                  "show_location_map",
                  "Select location on map",
                  class = "btn-outline-secondary btn-full-width",
                  icon  = icon("map-marker-alt")
                )
              ),
              div(
                class = "location-box",
                verbatimTextOutput("location_status")
              ),
              hr(),
              div(
                class = "button-container-space-between",
                actionButton("back_get_id", "◀ Back", class = "btn-secondary"),
                actionButton("next_get_id", "Next ▶", class = "btn-primary")
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          shinyjs::hidden(
            div(
              id = "location_map_panel",
              wellPanel(
                h4("Select location on map"),
                leafletOutput("location_map", height = "250px"),
                tags$p(
                  class = "text-muted mb-0",
                  "Tap on the map to set the plant location."
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "preview-panel",
            wellPanel(
              h4("Image Preview"),
              imageOutput("photo_preview", height = "auto")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Survey1",
      value = "survey1",
      fluidRow(
        column(
          12,
          wellPanel(
            div(
              id = "survey1_inputs",
              h4("Spread, control and disposal"),
              radioButtons(
                "spread_beyond",
                "Has the plant spread beyond where you initially planted it, or would it spread without control?",
                choices = c("Yes", "No", "I don't know")
              ),
              checkboxGroupInput(
                "spread_mode",
                "How does the plant spread in your garden? (tick all that apply)",
                choices = c(
                  "it doesn’t spread",
                  "seeds",
                  "Underground rhizomes/roots",
                  "Aboveground runners",
                  "bulbs",
                  "don’t know",
                  "other"
                )
              ),
              radioButtons(
                "control_effectiveness",
                "How effective is your effort to control the plant?",
                choices = c(
                  "I don’t control the plant",
                  "Ineffective (expansion)",
                  "Poorly effective (status quo)",
                  "Effective (area reduction)",
                  "Very effective (eradication)"
                )
              ),
              checkboxGroupInput(
                "control_methods",
                "How do you control this plant? (tick all that apply)",
                choices = c("digging", "pulling", "chemical", "cutting", "mulching", "other")
              ),
              checkboxGroupInput(
                "disposal_methods",
                "How do you dispose of this plant? (tick all that apply)",
                choices = c("home composting", "green waste", "other waste collection", "other")
              )
            ),
            hr(),
            div(
              class = "button-container-space-between",
              actionButton("back_survey1", "◀ Back", class = "btn-secondary"),
              actionButton("next_survey1", "Next ▶", class = "btn-primary")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Survey2",
      value = "survey2",
      fluidRow(
        column(
          12,
          wellPanel(
            div(
              id = "survey2_inputs",
              h4("Origin, impacts and comments"),
              checkboxGroupInput(
                "introduction_routes",
                "How do you think this plant came into your garden? (tick all that apply)",
                choices = c(
                  "It was already in the garden",
                  "I introduced it",
                  "It spread from the direct vicinity of my garden",
                  "I don’t know",
                  "other"
                )
              ),
              checkboxGroupInput(
                "source_of_plant",
                "Where did you get it? (tick all that apply)",
                choices = c(
                  "Garden center",
                  "Plant fair",
                  "I got it through a friend or another gardener",
                  "I bought it online from a nursery",
                  "I bought it on an online e-trade platform (e.g. ebay, tweedehands, etsy, …)",
                  "I got it from a big retailer (e.g. supermarket, DIY store)",
                  "Other, namely…"
                )
              ),
              radioButtons(
                "outside_garden",
                "Is the plant growing locally outside your garden?",
                choices = c("yes", "no", "I don’t know")
              ),
              radioButtons(
                "warning_label",
                "Should the plant be sold with a warning label about control difficulties?",
                choices = c("yes", "no", "I don’t know")
              ),
              radioButtons(
                "outcompeted",
                "Has this plant outcompeted other plants in your garden?",
                choices = c("yes", "no", "I don’t know")
              ),
              selectInput(
                "coverage_dafor",
                "Personal assessment of coverage (DAFOR scale)",
                choices = c(
                  "",
                  "Dominant",
                  "Abundant",
                  "Frequent",
                  "Occasional",
                  "Rare",
                  "Don’t know"
                )
              ),
              textAreaInput(
                "notes",
                "Any other comments about this plant?",
                rows = 3
              )
            ),
            hr(),
            div(
              class = "button-container-space-between",
              actionButton("back_survey2", "◀ Back", class = "btn-secondary"),
              actionButton(
                "save_btn",
                "Save & Go to Results",
                class = "btn-success btn-lg",
                icon  = icon("save")
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Results",
      value = "results",
      fluidRow(
        column(
          12,
          wellPanel(
            h4("All Saved Observations"),
            DT::DTOutput("records_table")
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Observation Map"),
            leaflet::leafletOutput("obs_map", height = "400px")
          )
        )
      ),
      hr(),
      div(
        class = "button-container-left",
        actionButton("back_results", "◀ Start new record", class = "btn-secondary")
      ),
      hr(),
      h5("Admin tools (optional)"),
      p(
        class = "text-muted",
        "If you are a project administrator, enter the admin code below and click ",
        "\"Refresh IDs from iNaturalist\"."
      ),
      div(
        class = "admin-tools-inline",
        div(
          class = "admin-code-wrapper",
          passwordInput(
            "admin_code",
            "Admin code",
            placeholder = "Enter admin code"
          )
        ),
        div(
          class = "admin-sync-wrapper",
          actionButton(
            "sync_inat_btn",
            "Refresh IDs from iNaturalist",
            class = "btn-outline-primary btn-sm"
          )
        )
      ),
      hr()
    )
  ),
  
  fluidRow(
    column(
      12,
      hr(),
      div(
        class = "app-footer",
        fluidRow(
          column(
            6,
            div(
              class = "footer-left",
              p(
                "This app was developed by Dr Dave Hudson, University of Exeter. ",
                "For questions, please contact ",
                tags$a(href = "mailto:d.hudson2@exeter.ac.uk", "d.hudson2@exeter.ac.uk"),
                "."
              ),
              tags$img(
                src   = "ExeLogo.png",
                class = "footer-logo",
                alt   = "App logo"
              )
            )
          ),
          column(
            6,
            div(
              class = "footer-right",
              p(
                "Plant identification powered by ",
                tags$a(
                  "Pl@ntNet",
                  href   = "https://plantnet.org/",
                  target = "_blank"
                )
              ),
              tags$img(
                src   = "plantnet_logo.png",
                class = "footer-logo",
                alt   = "Pl@ntNet logo"
              )
            )
          )
        )
      )
    )
  )
)

# --- 4. Server Logic ---
server <- function(input, output, session) {
  ensure_db()
  print(paste("Total records in DB:", nrow(load_data())))
  print(paste("INAT_PROJECT_ID =", Sys.getenv("INAT_PROJECT_ID")))
  
  inat_token <- reactiveVal(NULL)
  inat_jwt <- reactiveVal(NULL)
  inat_project_member <- reactiveVal(FALSE)
  
  observe({
    if (is.null(inat_token())) {
      inat_project_member(FALSE)
      inat_jwt(NULL)
      shinyjs::show("inat_login_btn")
    } else {
      shinyjs::hide("inat_login_btn")
    }
  })
  
  inat_get_taxon_name <- function(obs_id) {
    if (is.na(obs_id) || !nzchar(obs_id)) return(NA_character_)
    
    url <- paste0("https://api.inaturalist.org/v1/observations/", obs_id)
    
    res <- try(
      httr::GET(url, query = list(locale = "en"), httr::timeout(20)),
      silent = TRUE
    )
    if (inherits(res, "try-error")) return(NA_character_)
    if (httr::status_code(res) >= 300L) return(NA_character_)
    
    j <- try(httr::content(res, as = "parsed", type = "application/json"), silent = TRUE)
    if (inherits(j, "try-error") || is.null(j$results) || length(j$results) == 0) {
      return(NA_character_)
    }
    
    o <- j$results[[1]]
    
    if (!is.null(o$community_taxon) && !is.null(o$community_taxon$name)) {
      return(o$community_taxon$name)
    }
    if (!is.null(o$taxon) && !is.null(o$taxon$name)) {
      return(o$taxon$name)
    }
    if (!is.null(o$species_guess)) {
      return(o$species_guess)
    }
    
    NA_character_
  }
  
  get_inaturalist_jwt <- function(token) {
    if (is.null(token)) {
      return(list(ok = FALSE, status = NA_integer_, body = "Missing token", jwt = NULL))
    }
    
    access_token <- tryCatch(
      {
        at <- token$access_token
        if (is.null(at) || length(at) == 0) return("")
        as.character(at[1])
      },
      error = function(e) ""
    )
    
    if (!nzchar(access_token)) {
      return(list(ok = FALSE, status = NA_integer_, body = "Missing OAuth access token", jwt = NULL))
    }
    
    res <- try(
      httr::GET(
        url = "https://www.inaturalist.org/users/api_token",
        httr::add_headers(
          Authorization = paste("Bearer", access_token),
          Accept = "application/json"
        ),
        httr::timeout(20)
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error")) {
      return(list(ok = FALSE, status = NA_integer_, body = as.character(res), jwt = NULL))
    }
    
    status <- httr::status_code(res)
    body_txt <- httr::content(res, "text", encoding = "UTF-8")
    
    if (status < 200 || status >= 300) {
      return(list(ok = FALSE, status = status, body = body_txt, jwt = NULL))
    }
    
    parsed <- try(jsonlite::fromJSON(body_txt), silent = TRUE)
    
    jwt <- NULL
    if (!inherits(parsed, "try-error") && !is.null(parsed)) {
      jwt <- parsed$api_token %||% parsed$token %||% NULL
    }
    
    list(
      ok = !is.null(jwt) && nzchar(jwt),
      status = status,
      body = body_txt,
      jwt = jwt
    )
  }
  
  check_inaturalist_project_membership <- function(jwt, project_id) {
    if (is.null(jwt) || !nzchar(jwt) || is.null(project_id) || !nzchar(project_id)) {
      return(list(ok = FALSE, status = NA_integer_, body = "Missing jwt or project_id"))
    }
    
    res <- try(
      httr::GET(
        url = paste0("https://api.inaturalist.org/v2/projects/", project_id, "/membership"),
        httr::add_headers(
          Authorization = paste("Bearer", jwt),
          Accept = "application/json"
        ),
        httr::timeout(20)
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error")) {
      return(list(ok = FALSE, status = NA_integer_, body = as.character(res)))
    }
    
    list(
      ok = httr::status_code(res) >= 200 && httr::status_code(res) < 300,
      status = httr::status_code(res),
      body = httr::content(res, "text", encoding = "UTF-8")
    )
  }
  
  join_inaturalist_project <- function(jwt, project_id) {
    if (is.null(jwt) || !nzchar(jwt) || is.null(project_id) || !nzchar(project_id)) {
      return(list(ok = FALSE, status = NA_integer_, body = "Missing jwt or project_id"))
    }
    
    res <- try(
      httr::POST(
        url = paste0("https://api.inaturalist.org/v2/projects/", project_id, "/membership"),
        httr::add_headers(
          Authorization = paste("Bearer", jwt),
          Accept = "application/json"
        ),
        httr::timeout(20)
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error")) {
      return(list(ok = FALSE, status = NA_integer_, body = as.character(res)))
    }
    
    list(
      ok = httr::status_code(res) >= 200 && httr::status_code(res) < 300,
      status = httr::status_code(res),
      body = httr::content(res, "text", encoding = "UTF-8")
    )
  }
  
  shinyjs::runjs("
    $(document).on('click', '#get_location_btn, #show_location_map', function() {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(function(position) {
          Shiny.setInputValue('geolocation', {
            lat: position.coords.latitude,
            lon: position.coords.longitude
          }, { priority: 'event' });
        });
      } else {
        alert('Geolocation is not supported by this browser.');
      }
    });
  ")
  
  id_results       <- reactiveVal(NULL)
  records_data     <- reactiveVal(load_data())
  current_location <- reactiveVal(NULL)
  active_photo     <- reactiveVal(NULL)
  
  observeEvent(input$id_btn, {
    if (!nzchar(PLANTNET_KEY)) {
      showNotification(
        "PlantNet API key is not configured on the server.",
        type = "error"
      )
      return()
    }
    
    photo <- active_photo()
    if (is.null(photo) || is.null(photo$datapath) || !file.exists(photo$datapath)) {
      showNotification(
        "Please take or upload a photo before running PlantNet.",
        type = "error"
      )
      return()
    }
    
    showNotification("Querying PlantNet…", type = "message", duration = 3)
    
    tryCatch({
      raw_res <- plantnet_identify(photo$datapath, PLANTNET_KEY)
      df      <- plantnet_parse(raw_res, top_n = 5)
      
      if (nrow(df) == 0) {
        id_results(NULL)
        showNotification("No matches returned by PlantNet.", type = "warning")
      } else {
        id_results(df)
        updateTextInput(
          session,
          "species_name",
          value = df$Scientific.Name[1]
        )
      }
    },
    error = function(e) {
      id_results(NULL)
      showNotification(
        paste("PlantNet request failed:", conditionMessage(e)),
        type = "error"
      )
    })
  })
  
  output$inat_status_ui <- renderUI({
    tok <- inat_token()
    
    if (is.null(tok)) {
      tags$div(
        class = "inat-status inat-status-disconnected",
        icon("circle-xmark"),
        " Not connected to iNaturalist"
      )
    } else if (isTRUE(inat_project_member())) {
      tags$div(
        class = "inat-status inat-status-connected",
        icon("circle-check"),
        " Connected to iNaturalist · Already joined project"
      )
    } else {
      tags$div(
        class = "inat-status inat-status-connected",
        icon("circle-check"),
        " Connected to iNaturalist · Not yet joined project"
      )
    }
  })
  
  output$inat_project_ui <- renderUI({
    tok <- inat_token()
    
    if (is.null(tok)) {
      return(NULL)
    }
    
    if (!nzchar(INAT_PROJECT_ID)) {
      return(
        tags$div(
          class = "text-muted",
          "Project join is not configured on the server."
        )
      )
    }
    
    if (isTRUE(inat_project_member())) {
      return(
        tags$div(
          class = "text-success",
          icon("check"),
          " You are already a member of the project."
        )
      )
    }
    
    tagList(
      p(
        class = "text-muted",
        "Join the project so your account is linked as a participant."
      ),
      actionButton(
        "join_project_btn",
        "Join iNaturalist Project",
        class = "btn-primary btn-full-width",
        icon  = icon("users")
      )
    )
  })
  
  observe({
    query <- parseQueryString(isolate(session$clientData$url_search %||% ""))
    if (is.null(query$code)) return()
    
    isolate({
      if (!is.null(inat_token())) return()
    })
    
    code <- query$code
    
    if (INAT_CLIENT_ID == "" || INAT_CLIENT_SECRET == "" || INAT_REDIRECT_URI == "") {
      showNotification("iNaturalist credentials not configured on server.", type = "error")
      return()
    }
    
    res <- try(
      httr::POST(
        INAT_TOKEN_URL,
        body = list(
          client_id     = INAT_CLIENT_ID,
          client_secret = INAT_CLIENT_SECRET,
          grant_type    = "authorization_code",
          code          = code,
          redirect_uri  = INAT_REDIRECT_URI
        ),
        encode = "form"
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error") || httr::status_code(res) >= 400) {
      showNotification("iNaturalist login failed.", type = "error")
      return()
    }
    
    tok <- httr::content(res, as = "parsed", type = "application/json")
    if (is.null(tok$access_token)) {
      showNotification("iNaturalist login failed: no access token returned.", type = "error")
      return()
    }
    
    inat_token(tok)
    
    jwt_res <- get_inaturalist_jwt(tok)
    print("JWT exchange result:")
    print(jwt_res)
    
    if (!isTRUE(jwt_res$ok)) {
      inat_jwt(NULL)
      showNotification(
        paste0("Connected to iNaturalist, but failed to obtain API JWT. HTTP ", jwt_res$status %||% "unknown"),
        type = "error",
        duration = 10
      )
      return()
    }
    
    inat_jwt(jwt_res$jwt)
    
    is_member <- FALSE
    
    if (nzchar(INAT_PROJECT_ID)) {
      membership_res <- check_inaturalist_project_membership(inat_jwt(), INAT_PROJECT_ID)
      print("Membership check result:")
      print(membership_res)
      
      if (isTRUE(membership_res$ok)) {
        is_member <- TRUE
      }
    }
    
    inat_project_member(is_member)
    
    if (isTRUE(is_member)) {
      showNotification(
        "Connected to iNaturalist. You are already a project member.",
        type = "message"
      )
    } else {
      showNotification("Connected to iNaturalist!", type = "message")
    }
  })
  
  observeEvent(input$inat_login_btn, {
    if (INAT_CLIENT_ID == "" || INAT_CLIENT_SECRET == "" || INAT_REDIRECT_URI == "") {
      showNotification(
        "iNaturalist client ID, client secret, or redirect URI not configured.",
        type = "error"
      )
      return()
    }
    
    state <- paste(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
    message("iNaturalist state token: ", state)
    
    auth_url <- paste0(
      INAT_AUTHORIZE_URL, "?",
      "client_id=", URLencode(INAT_CLIENT_ID),
      "&redirect_uri=", URLencode(INAT_REDIRECT_URI),
      "&response_type=code",
      "&scope=write",
      "&state=", state
    )
    
    session$sendCustomMessage("inat_redirect", auth_url)
  })
  
  observeEvent(input$join_project_btn, {
    jwt <- inat_jwt()
    req(jwt)
    
    if (!nzchar(INAT_PROJECT_ID)) {
      showNotification("Project ID is not configured on the server.", type = "error")
      return()
    }
    
    membership_res <- check_inaturalist_project_membership(jwt, INAT_PROJECT_ID)
    print("Membership check before join:")
    print(membership_res)
    
    if (isTRUE(membership_res$ok)) {
      inat_project_member(TRUE)
      showNotification("You are already a member of the iNaturalist project.", type = "message")
      return()
    }
    
    join_res <- join_inaturalist_project(jwt, INAT_PROJECT_ID)
    print("Join result:")
    print(join_res)
    
    if (isTRUE(join_res$ok)) {
      inat_project_member(TRUE)
      showNotification("You have joined the iNaturalist project.", type = "message")
    } else {
      showNotification(
        paste0(
          "Could not join the iNaturalist project. HTTP ",
          join_res$status %||% "unknown"
        ),
        type = "error",
        duration = 10
      )
    }
  })
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs != "user_details" && !isTRUE(input$accept_terms)) {
      showNotification("Please accept the terms and conditions first.", type = "error")
      updateTabsetPanel(session, "main_tabs", selected = "user_details")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$welcome_next, {
    if (!isTRUE(input$accept_terms)) {
      showNotification("Please accept the T&Cs first.", type = "error")
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "get_id")
  })
  
  observeEvent(input$back_get_id, { updateTabsetPanel(session, "main_tabs", selected = "user_details") })
  observeEvent(input$next_get_id, { updateTabsetPanel(session, "main_tabs", selected = "survey1") })
  observeEvent(input$back_survey1, { updateTabsetPanel(session, "main_tabs", selected = "get_id") })
  observeEvent(input$next_survey1, { updateTabsetPanel(session, "main_tabs", selected = "survey2") })
  observeEvent(input$back_survey2, { updateTabsetPanel(session, "main_tabs", selected = "survey1") })
  observeEvent(input$back_results, { updateTabsetPanel(session, "main_tabs", selected = "get_id") })
  
  observeEvent(input$photo_capture, {
    req(input$photo_capture)
    active_photo(input$photo_capture)
  })
  
  observeEvent(input$photo_upload, {
    req(input$photo_upload)
    active_photo(input$photo_upload)
  })
  
  output$photo_preview <- renderImage({
    req(active_photo())
    list(
      src         = active_photo()$datapath,
      contentType = active_photo()$type,
      width       = "100%"
    )
  }, deleteFile = FALSE)
  
  observeEvent(input$geolocation, {
    loc <- input$geolocation
    if (is.null(loc)) return()
    
    current_location(loc)
    
    leafletProxy("location_map") %>%
      clearMarkers() %>%
      addMarkers(lng = loc$lon, lat = loc$lat) %>%
      setView(lng = loc$lon, lat = loc$lat, zoom = 14)
  })
  
  output$location_status <- renderText({
    loc <- current_location()
    if (is.null(loc)) "Location not set." else paste0("Lat: ", round(loc$lat, 5), ", Lon: ", round(loc$lon, 5))
  })
  
  observeEvent(input$show_location_map, {
    shinyjs::show("location_map_panel")
  })
  
  output$location_map <- renderLeaflet({
    loc <- current_location()
    if (!is.null(loc)) {
      lat <- loc$lat
      lon <- loc$lon
    } else {
      lat <- 51.5
      lon <- -0.1
    }
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = lon, lat = lat, zoom = 12)
  })
  
  observeEvent(input$location_map_click, {
    click <- input$location_map_click
    if (is.null(click)) return()
    
    current_location(list(lat = click$lat, lon = click$lng))
    
    leafletProxy("location_map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  output$id_results_ui <- renderUI({
    df <- id_results()
    if (is.null(df) || nrow(df) == 0) {
      return(helpText("Results will appear here after you run PlantNet."))
    }
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      thumb <- NULL
      if (!is.null(df$ImageURL) &&
          length(df$ImageURL) >= i &&
          !is.na(df$ImageURL[i]) &&
          nzchar(df$ImageURL[i])) {
        thumb <- tags$img(
          src   = df$ImageURL[i],
          class = "id-thumbnail",
          alt   = paste("Photo for", df$Scientific.Name[i])
        )
      }
      
      fluidRow(
        class = "id-result-row",
        column(
          width = 3,
          thumb
        ),
        column(
          width = 6,
          strong(df$Scientific.Name[i]), br(),
          em(df$Common.Names[i]), br(),
          tags$small(paste0("Score: ", df$Score[i], " · ", df$Family[i]))
        ),
        column(
          width = 3,
          div(
            class = "text-end",
            actionButton(
              inputId = paste0("select_sp_", i),
              label   = "Use this",
              class   = "btn-sm btn-primary"
            )
          )
        )
      )
    })
    
    tagList(rows)
  })
  
  observe({
    df <- id_results()
    req(df)
    lapply(seq_len(nrow(df)), function(i) {
      observeEvent(input[[paste0("select_sp_", i)]], {
        updateTextInput(session, "species_name", value = df$Scientific.Name[i])
      })
    })
  })
  
  observeEvent(input$save_btn, {
    if (!nzchar(input$species_name %||% "")) {
      showNotification("Please enter a plant name before saving.", type = "error")
      return()
    }
    if (!nzchar(input$site_name %||% "")) {
      showNotification("Please enter a site name before saving.", type = "error")
      return()
    }
    
    loc <- current_location()
    
    new_record <- tibble(
      timestamp             = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      observer_name         = input$observer_name %||% NA_character_,
      include_name          = input$include_name %||% NA_character_,
      observer_email        = input$observer_email %||% NA_character_,
      site_name             = input$site_name,
      grid_cell             = input$grid_cell %||% NA_character_,
      latitude              = if (!is.null(loc)) loc$lat else NA_real_,
      longitude             = if (!is.null(loc)) loc$lon else NA_real_,
      species_name          = input$species_name,
      spread_beyond         = input$spread_beyond %||% NA_character_,
      spread_mode           = if (length(input$spread_mode)) paste(input$spread_mode, collapse = "; ") else "",
      control_effectiveness = input$control_effectiveness %||% NA_character_,
      control_methods       = if (length(input$control_methods)) paste(input$control_methods, collapse = "; ") else "",
      disposal_methods      = if (length(input$disposal_methods)) paste(input$disposal_methods, collapse = "; ") else "",
      introduction_routes   = if (length(input$introduction_routes)) paste(input$introduction_routes, collapse = "; ") else "",
      source_of_plant       = if (length(input$source_of_plant)) paste(input$source_of_plant, collapse = "; ") else "",
      outside_garden        = input$outside_garden %||% NA_character_,
      warning_label         = input$warning_label %||% NA_character_,
      outcompeted           = input$outcompeted %||% NA_character_,
      coverage_dafor        = input$coverage_dafor %||% NA_character_,
      notes                 = input$notes,
      inat_id               = NA_character_
    )
    
    new_record_df <- as.data.frame(new_record, stringsAsFactors = FALSE)
    
    tryCatch({
      con <- connect_db()
      on.exit(dbDisconnect(con), add = TRUE)
      
      tok <- inat_token()
      if (!is.null(tok)) {
        new_record_df$inat_id[1] <- push_to_inaturalist(
          token        = tok,
          species_name = input$species_name,
          loc          = loc,
          notes        = input$notes,
          photo        = active_photo()
        )
      }
      
      DBI::dbWriteTable(
        con,
        name      = "observations",
        value     = new_record_df,
        append    = TRUE,
        row.names = FALSE
      )
      
      shinyjs::reset("survey1_inputs")
      shinyjs::reset("survey2_inputs")
      updateTextInput(session, "site_name", value = "")
      updateTextInput(session, "grid_cell", value = "")
      updateTextInput(session, "species_name", value = "")
      
      active_photo(NULL)
      shinyjs::reset("photo_capture")
      shinyjs::reset("photo_upload")
      
      current_location(NULL)
      id_results(NULL)
      
      records_data(load_data())
      
      if (!is.na(new_record_df$inat_id[1]) && nzchar(new_record_df$inat_id[1])) {
        showNotification(
          paste0("Record saved and uploaded to iNaturalist (ID ", new_record_df$inat_id[1], ")."),
          type = "message"
        )
      } else {
        showNotification("Record saved locally.", type = "message")
      }
      
      updateTabsetPanel(session, "main_tabs", selected = "results")
      
    }, error = function(e) {
      showNotification(paste("Database Error:", conditionMessage(e)), type = "error")
    })
  })
  
  output$records_table <- renderDT({
    df <- records_data()
    req(nrow(df) > 0)
    
    display_df <- data.frame(
      Date    = as.character(as.Date(df$timestamp)),
      User    = as.character(df$observer_name),
      Site    = as.character(df$site_name),
      Species = as.character(df$species_name),
      stringsAsFactors = FALSE
    )
    
    datatable(display_df, options = list(dom = "tp", scrollX = TRUE), rownames = FALSE)
  })
  
  output$obs_map <- renderLeaflet({
    df <- records_data()
    
    m <- leaflet() %>% addTiles()
    
    if (is.null(df) || nrow(df) == 0) {
      return(m %>% setView(lng = -3, lat = 54, zoom = 5))
    }
    
    df_clean <- df[complete.cases(df[, c("latitude", "longitude")]), , drop = FALSE]
    df_clean$latitude  <- as.numeric(df_clean$latitude)
    df_clean$longitude <- as.numeric(df_clean$longitude)
    
    df_clean <- df_clean[
      !is.na(df_clean$latitude) & !is.na(df_clean$longitude),
      ,
      drop = FALSE
    ]
    
    if (nrow(df_clean) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = df_clean,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste0("<b>", species_name, "</b><br>", site_name),
          radius = 5
        ) %>%
        fitBounds(
          min(df_clean$longitude), min(df_clean$latitude),
          max(df_clean$longitude), max(df_clean$latitude)
        )
    } else {
      m <- m %>% setView(lng = -3, lat = 54, zoom = 5)
    }
    
    m
  })
  
  observeEvent(input$sync_inat_btn, {
    if (!nzchar(INAT_ADMIN_CODE)) {
      showNotification(
        "Admin sync code is not configured on the server.",
        type = "error"
      )
      return()
    }
    
    if (!identical(input$admin_code, INAT_ADMIN_CODE)) {
      showNotification(
        "Incorrect admin code. Sync from iNaturalist is restricted.",
        type = "error"
      )
      return()
    }
    
    df <- records_data()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No records to sync.", type = "warning")
      return()
    }
    
    idx <- which(!is.na(df$inat_id) & nzchar(df$inat_id))
    if (length(idx) == 0) {
      showNotification("No records have iNaturalist IDs yet.", type = "warning")
      return()
    }
    
    con <- connect_db()
    on.exit(dbDisconnect(con), add = TRUE)
    
    n_updated <- 0L
    
    for (i in idx) {
      obs_id   <- df$inat_id[i]
      new_name <- inat_get_taxon_name(obs_id)
      
      if (is.na(new_name) || !nzchar(new_name)) next
      
      old_name <- df$species_name[i]
      if (!identical(old_name, new_name)) {
        dbExecute(
          con,
          "UPDATE observations SET species_name = ? WHERE inat_id = ?",
          params = list(new_name, obs_id)
        )
        n_updated <- n_updated + 1L
      }
    }
    
    records_data(load_data())
    
    if (n_updated > 0) {
      showNotification(
        paste("Updated", n_updated, "record(s) from iNaturalist."),
        type = "message"
      )
    } else {
      showNotification("No changes detected in iNaturalist IDs.", type = "message")
    }
  })
}

# --- 5. Run the App ---
shinyApp(ui = ui, server = server)