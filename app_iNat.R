# ==================================================================
#                       Plant Recorder App
#              Single-File Shiny Application (app.R)
# ==================================================================

# ------------------------------------------------------------------
# 1. GLOBAL SETUP: Load packages and define global variables
# ------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DT)
library(uuid)

# --- iNaturalist API Credentials ---
# IMPORTANT: Replace with your actual iNaturalist App credentials
# Register your app at: https://www.inaturalist.org/oauth/applications/new
INAT_APP_ID <- "YOUR_INATURALIST_APP_ID"
INAT_APP_SECRET <- "YOUR_INATURALIST_APP_SECRET"
# The redirect URI must match your iNaturalist app settings EXACTLY.
# For local testing, http://127.0.0.1:PORT is common. The port can change.
# Let's use the port from your error message as an example.
INAT_REDIRECT_URI <- "http://127.0.0.1:5736"

# Define the iNaturalist OAuth2.0 endpoint
inat_oauth_app <- oauth_app("iNaturalist",
                            key = INAT_APP_ID,
                            secret = INAT_APP_SECRET,
                            redirect_uri = INAT_REDIRECT_URI)

inat_api_endpoint <- oauth_endpoint(
  authorize = "https://www.inaturalist.org/oauth/authorize",
  access = "https://www.inaturalist.org/oauth/token"
)

# --- PlantNet API Credentials ---
# IMPORTANT: Replace with your actual PlantNet API key
# Register for a key at: https://my.plantnet.org/
PLANTNET_API_KEY <- "YOUR_PLANTNET_API_KEY"
PLANTNET_API_URL <- "https://my-api.plantnet.org/v2/identify/all"

# --- Data Storage ---
RECORDS_FILE_PATH <- "plant_records.csv"

# --- Helper Functions ---
load_records <- function() {
  if (file.exists(RECORDS_FILE_PATH)) {
    read.csv(RECORDS_FILE_PATH)
  } else {
    data.frame(
      record_id = character(),
      inat_observation_id = character(),
      species_name = character(),
      latitude = numeric(),
      longitude = numeric(),
      timestamp = character(),
      notes = character(),
      image_path = character(),
      stringsAsFactors = FALSE
    )
  }
}

save_record <- function(record) {
  write.table(record, RECORDS_FILE_PATH, sep = ",", append = TRUE, row.names = FALSE, col.names = !file.exists(RECORDS_FILE_PATH))
}

# Custom JavaScript for fetching GPS coordinates
js_code <- "
shinyjs.geoloc = function() {
  navigator.geolocation.getCurrentPosition(function(position) {
    var coords = {
      lat: position.coords.latitude,
      lng: position.coords.longitude
    };
    Shiny.setInputValue('user_location', coords);
  });
}
"


# ------------------------------------------------------------------
# 2. UI: Define the User Interface
# ------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = js_code, functions = c("geoloc")),
  
  titlePanel("Plant Observation Recorder"),
  
  # Conditional UI: Show a login view or the main app view
  uiOutput("app_ui")
)


# ------------------------------------------------------------------
# 3. SERVER: Define the Application Logic
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Reactive Values for State Management ---
  user <- reactiveValues(token = NULL, logged_in = FALSE)
  records <- reactiveVal(load_records())
  
  # --- Authentication Logic ---
  auth_url <- oauth2.0_authorize_url(inat_api_endpoint, inat_oauth_app, scope = "write")
  
  output$app_ui <- renderUI({
    if (!user$logged_in) {
      fluidPage(
        h2("Welcome"),
        p("Please log in with your iNaturalist account to continue."),
        p("By logging in, you agree to the terms and conditions of this application."),
        hr(),
        actionButton("login_button", "Login with iNaturalist", 
                     onclick = paste0("window.location.href = '", auth_url, "';"))
      )
    } else {
      navbarPage("Main Menu",
                 tabPanel("New Record",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. Upload Plant Photo"),
                              fileInput("plant_image", "Choose image file", accept = c("image/png", "image/jpeg")),
                              actionButton("identify_button", "Identify Plant"),
                              hr(),
                              h4("2. Confirm Details"),
                              uiOutput("species_selector_ui"),
                              textInput("notes", "Notes"),
                              actionButton("get_location_button", "Get Current GPS Location"),
                              verbatimTextOutput("gps_coords"),
                              hr(),
                              actionButton("submit_record_button", "Submit Record", class = "btn-primary")
                            ),
                            mainPanel(
                              h4("Identification Results"),
                              imageOutput("uploaded_image_preview"),
                              tableOutput("plantnet_results")
                            )
                          )
                 ),
                 tabPanel("My Records",
                          h3("Locally Saved Observations"),
                          DTOutput("records_table"),
                          actionButton("sync_button", "Sync with iNaturalist")
                 )
      )
    }
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code)) {
      token_response <- oauth2.0_access_token(inat_api_endpoint, inat_oauth_app, code = query$code)
      if (http_status(token_response)$category == "Success") {
        user$token <- token_response$access_token
        user$logged_in <- TRUE
        updateQueryString(session = session, queryString = "")
      } else {
        showNotification("Authentication failed. Please try again.", type="error")
      }
    }
  })
  
  # --- New Record Tab Logic ---
  output$uploaded_image_preview <- renderImage({
    req(input$plant_image)
    list(src = input$plant_image$datapath, contentType = input$plant_image$type, width = "100%")
  }, deleteFile = TRUE)
  
  plantnet_data <- eventReactive(input$identify_button, {
    req(input$plant_image)
    withProgress(message = 'Identifying plant...', value = 0.5, {
      response <- POST(
        url = PLANTNET_API_URL,
        query = list(`api-key` = PLANTNET_API_KEY), # Note: API key parameter name correction
        body = list(images = upload_file(input$plant_image$datapath)),
        encode = "multipart"
      )
      incProgress(0.5)
      fromJSON(content(response, "text", encoding = "UTF-8"))$results
    })
  })
  
  output$plantnet_results <- renderTable({
    req(plantnet_data())
    data.frame(
      Species = plantnet_data()$species$scientificNameWithoutAuthor,
      Score = round(plantnet_data()$score, 3),
      Common_Names = sapply(plantnet_data()$species$commonNames, paste, collapse = ", ")
    )
  })
  
  output$species_selector_ui <- renderUI({
    req(plantnet_data())
    selectInput("selected_species", "Select Species", 
                choices = plantnet_data()$species$scientificNameWithoutAuthor)
  })
  
  observeEvent(input$get_location_button, {
    js$geoloc()
  })
  
  output$gps_coords <- renderText({
    req(input$user_location)
    paste("Lat:", input$user_location$lat, "\nLng:", input$user_location$lng)
  })
  
  observeEvent(input$submit_record_button, {
    req(input$selected_species, input$user_location, input$plant_image)
    
    api_body <- list(
      "observation[species_guess]" = input$selected_species,
      "observation[latitude]" = input$user_location$lat,
      "observation[longitude]" = input$user_location$lng,
      "observation[observed_on_string]" = as.character(Sys.time()),
      "observation[description]" = input$notes
    )
    
    withProgress(message = 'Submitting to iNaturalist...', value = 0.3, {
      obs_response <- POST(
        "https://api.inaturalist.org/v1/observations",
        body = api_body,
        add_headers(Authorization = paste("Bearer", user$token))
      )
      
      incProgress(0.3)
      obs_content <- content(obs_response)
      
      if (http_status(obs_response)$category == "Success") {
        photo_response <- POST(
          "https://api.inaturalist.org/v1/observation_photos",
          body = list(
            "observation_photo[observation_id]" = obs_content$id,
            "file" = upload_file(input$plant_image$datapath)
          ),
          add_headers(Authorization = paste("Bearer", user$token))
        )
        
        new_record <- data.frame(
          record_id = UUIDgenerate(),
          inat_observation_id = obs_content$id,
          species_name = input$selected_species,
          latitude = input$user_location$lat,
          longitude = input$user_location$lng,
          timestamp = as.character(Sys.time()),
          notes = input$notes,
          image_path = input$plant_image$name,
          stringsAsFactors = FALSE
        )
        save_record(new_record)
        records(load_records())
        
        incProgress(0.4)
        showNotification("Record successfully submitted to iNaturalist and saved locally.", type = "message")
      } else {
        showNotification("Failed to submit record to iNaturalist.", type = "error")
        print(obs_content)
      }
    })
  })
  
  # --- My Records Tab Logic ---
  output$records_table <- renderDT({
    datatable(records(), options = list(pageLength = 10), selection = 'single')
  })
  
  observeEvent(input$sync_button, {
    showNotification("Sync functionality is a future enhancement.", duration = 5)
  })
}

# ------------------------------------------------------------------
# 4. RUN APP: Start the Shiny Application
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)