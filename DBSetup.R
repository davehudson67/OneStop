library(DBI)
library(RSQLite)

db_path <- "plant_records.db"

# 1. Remove old DB if it exists (optional, but keeps things clean)
if (file.exists(db_path)) {
  file.remove(db_path)
}

# 2. Create new DB and table with full schema
con <- dbConnect(SQLite(), db_path)

dbExecute(con, "
CREATE TABLE observations (
  timestamp           TEXT,
  observer_name       TEXT,
  include_name        TEXT,
  observer_email      TEXT,
  site_name           TEXT,
  grid_cell           TEXT,
  latitude            REAL,
  longitude           REAL,
  species_name        TEXT,
  spread_beyond       TEXT,
  spread_mode         TEXT,
  control_effectiveness TEXT,
  control_methods     TEXT,
  disposal_methods    TEXT,
  introduction_routes TEXT,
  source_of_plant     TEXT,
  outside_garden      TEXT,
  warning_label       TEXT,
  outcompeted         TEXT,
  coverage_dafor      TEXT,
  notes               TEXT,
  inat_id             TEXT
);
")

dbDisconnect(con)

cat("Fresh observations table created in", db_path, "\n")
