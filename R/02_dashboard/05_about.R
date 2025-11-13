#### RESOURCE MAIN UI ####
#### ABOUT UI ####
about_ui <- function(id) {
  ns <- NS(id)
  tagList(htmlOutput(ns("about_content"))) %>% tags$div(class = "about_content")
}

#### ABOUT SERVER ####
about_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$about_content <- renderText({
        data %>% unlist() %>% unname() %>% paste(collapse = "<br><br>")
      })
    }
  )
}
