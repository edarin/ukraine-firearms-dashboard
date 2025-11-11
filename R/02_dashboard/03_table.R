# UKR TABLE ####
# TABLE UI INTERFACE ####
firearm_table_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Content"),
    card_body(
      DT::DTOutput(ns("firearm_table")),
      fillable = TRUE,
      fill = T,
      min_height = "70vh"
    ),
    full_screen = T,
    fill = T
  ) %>%
    tags$div(class = "firearm_card")
}
firearm_table_download_ui <-
  function(id) {
    ns <- NS(id)
    downloadButton(
      ns("download_table"),
      icon = shiny::icon("download"),
      class = "bttn-simple bttn-primary"
    ) %>%
      tags$div(class = "download_div")
  }
