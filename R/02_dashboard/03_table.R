# UKR TABLE ####
# TABLE UI INTERFACE ####
firearm_database_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "80vh",
    card_header(h3(
      "Details of posts mentioning weapons/Деталі постів, що згадують зброю"
    )),
    card_body(
      DT::DTOutput(ns("firearm_table")),
      fillable = TRUE,
      fill = T,
      min_height = "80vh"
    ),
    full_screen = T,
    fill = T
  ) %>%
    tags$div(class = "firearm_card")
}

firearm_database_download_ui <- function(id) {
  ns <- NS(id)
  downloadButton(
    ns("download_table"),
    icon = shiny::icon("download"),
    class = "bttn-simple bttn-primary"
  ) %>%
    tags$div(class = "download_div")
}
