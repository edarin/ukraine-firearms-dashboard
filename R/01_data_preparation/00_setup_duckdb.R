# Load required packages
library(duckdb)
library(here)

# Ensure /data directory exists
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# Path to DuckDB database
db_path <- file.path(data_dir, 'database', "ukr_firearms_dashboard.duckdb")

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

# Example: Define schema for ukr_socialMedia (adjust columns as needed)
dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS ukr_socialMedia (
    post_id INTEGER,
    post_source TEXT,
    post_date DATE,
    post_date_month DATE,
    post_link TEXT,
    post_author_ukr TEXT,
    post_author_eng TEXT,
    post_title_ukr TEXT,
    post_title_eng TEXT,
    post_content_ukr TEXT,
    post_content_eng TEXT,
    post_oblast_ukr TEXT,
    post_oblast_eng TEXT,
    post_settlement_ukr TEXT,
    post_settlement_eng TEXT,
    post_criminal_code_ukr TEXT,
    post_criminal_code_eng TEXT,
    post_item_eng TEXT,
    post_item_ukr TEXT,
    geocode_oblast TEXT,
  )
  "
)
#delete table if exists
# dbExecute(con, "DROP TABLE IF EXISTS ukr_socialMedia;")

# Example: Define schema for ukr_socialMedia_summary (adjust columns as needed)
dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS ukr_socialMedia_summary (
    post_oblast_eng TEXT,
    post_oblast_ukr TEXT,
    post_item_eng TEXT,
    post_item_ukr TEXT,
    post_oblast_latitude DOUBLE,
    post_oblast_longitude DOUBLE
  )
  "
)

# Define schema for sotring processed files
dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS processed_censs_files (
    file_name TEXT PRIMARY KEY,
    processed_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
  "
)


# Disconnect when done
dbDisconnect(con, shutdown = TRUE)
