# UKR DASHBOARD ####
# SET UP ####

## LIBRARIES ####
# silence fun
shhh <- suppressPackageStartupMessages
# remove dplyr massage
options(dplyr.summarise.inform = FALSE)
# load libraries
shhh(library(shiny))
shhh(library(tidyverse))
shhh(library(shinyjs))
shhh(library(bslib))
shhh(library(bsicons))
shhh(library(waiter))
shhh(library(data.table))
shhh(library(gridlayout))
shhh(library(shinyWidgets))
shhh(library(plotly))
shhh(library(shinydisconnect))
shhh(library(htmltools))
shhh(library(sf))
shhh(library(duckdb))
shhh(library(leaflet))
shhh(library(DT))
shhh(library(writexl))
shhh(library(shinyscreenshot))
shhh(library(googledrive))
shhh(library(jsonlite))
shhh(library(httr))
shhh(library(dotenv))
# remove silence fun
rm(shhh)
## DATA ####

# load resources
resource_name <- "resource_list.csv"
resource_path <- file.path('data/text', resource_name)
resource <- read_csv(resource_path)

# load about
about_name <- "about.csv"
about_path <- file.path('data/text', about_name)
about <- read_csv(about_path)

# load popup
popup_name <- "popup_start.csv"
popup_path <- file.path('data/text', popup_name)
popup <- read_csv(popup_path)

# load users
if (grepl('Users/EdithD/Documents/git/ukraine-firearms-dashboard', getwd())) {
  dotenv::load_dot_env(file = '.env')
}

GOOGLE_TOKEN <- Sys.getenv('GOOGLE_TOKEN')
# authenticate googledrive
drive_auth(path = GOOGLE_TOKEN, scopes = 'drive')
dir.create('googledrive-temp', showWarnings = FALSE)
users_name <- "users.csv"
users_path <- file.path('googledrive-temp', users_name)
id_users_file <- drive_get("users.csv")$id


## DB CONNECTION ####
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = "data/database/ukr_firearms_dashboard.duckdb"
)

firearm_table <- tbl(con, "ukr_socialMedia") |>
  filter(post_source == 'main cases') |>
  filter(!is.na(post_date)) |>
  filter(!grepl('None', post_item_eng)) |>
  filter(!is.na(post_oblast_eng))

firearm_summary_table <- tbl(con, "ukr_socialMedia_summary") |>
  filter(!grepl('None', post_item_eng))

# environment for firearms data

## COLORS ####
# set seed
set.seed(132)
# get items
firearm_col <-
  firearm_summary_table %>%
  select(post_item_eng, post_item_ukr) %>%
  distinct() %>%
  collect() %>%
  mutate(
    post_item_color = c(
      "#CF2734",
      "#A3082A",
      "#174574",
      "#19798B",
      "#EFB050",
      "#EC8744",
      "#E75546",
      "#F3D567",
      "#3F1E55"
    )
  ) %>%
  pivot_longer(
    !post_item_color,
    values_to = "post_item",
    names_to = "language"
  ) %>%
  select(-language) %>%
  distinct()

# create color palette
palette_color <- firearm_col$post_item_color %>%
  set_names(firearm_col$post_item)
palette_factor <- colorFactor(
  palette = palette_color %>% unname(),
  levels = palette_color %>% names()
)
rm(firearm_col)
