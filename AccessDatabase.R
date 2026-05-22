library(DBI)
library(pool)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT", "26257")),
  dbname = Sys.getenv("DB_NAME", "defaultdb"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode = Sys.getenv("DB_SSLMODE", "verify-full"),
  sslrootcert = Sys.getenv("DB_SSLROOTCERT", "system")
)

df <- dbGetQuery(con, "SELECT * FROM observations ORDER BY timestamp DESC;")
print(df)

dbDisconnect(con)

df <- DBI::dbGetQuery(
  db_pool,
  "SELECT id, inat_id, species_name, timestamp
   FROM observations
   WHERE inat_upload_mode = 'fallback'
     AND inat_id IS NOT NULL
   ORDER BY timestamp DESC
   LIMIT 5;"
)

print(df)
