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
shhh(library(gmailr))
shhh(library(shinydisconnect))
shhh(library(htmltools))
shhh(library(sf))
shhh(library(sodium))
shhh(library(duckdb))
shhh(library(leaflet))
shhh(library(MoMAColors))
shhh(library(DT))
shhh(library(writexl))
shhh(library(shinyscreenshot))
shhh(library(MESS))
# remove silence fun
rm(shhh)

## DATA ####
# load resources
resource <- fread("data/text/resource_list.csv") %>%
  mutate(resource_date = as.Date(resource_date %>% paste0("-01"))) %>%
  arrange(desc(resource_date))
# load about
about <- fread("data/text/about.csv", quote = "", fill = TRUE) %>%
  mutate(
    about_content = str_replace_all(about_content, '["]', ""),
    about_content = str_squish(about_content)
  )


## DB CONNECTION ####
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = "data/database/ukr_firearms_dashboard.duckdb"
)

firearms_table <- tbl(con, "ukr_socialMedia")
firearm_summary_table <- tbl(con, "ukr_socialMedia_summary")

# environment for firearms data

## COLORS ####
# set seed
set.seed(132)
# get items
firearm_col <-
  firearm_summary_table %>%
  select(item_list_eng, item_list_ukr) %>%
  distinct() %>%
  collect() %>%
  mutate(
    item_list_color = MoMAColors::moma.colors(
      "OKeeffe",
      n = nrow(.),
      type = "continuous"
    ) %>%
      sample()
  ) %>%
  pivot_longer(
    !item_list_color,
    values_to = "post_item",
    names_to = "language"
  ) %>%
  select(-language) %>%
  distinct()

# create color palette
palette_color <- firearm_col$item_list_color %>%
  set_names(firearm_col$post_item)
palette_factor <- colorFactor(
  palette = palette_color %>% unname(),
  levels = palette_color %>% names()
)
rm(firearm_col)
