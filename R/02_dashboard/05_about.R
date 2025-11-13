#### RESOURCE MAIN UI ####
#### ABOUT UI ####
about_ui <- function(id) {
  ns <- NS(id)
  tagList(htmlOutput(ns("about_content"))) %>% tags$div(class = "about_content")
}

#### ABOUT SERVER ####
about_server <- function(id, parent_session = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      req(parent_session)
      observeEvent(parent_session$input$dashboard, {
        if (parent_session$input$dashboard == "About") {
          about_name <- "about.csv"
          about_path <- file.path('googledrive-temp', about_name)
          drive_download(about_name, path = about_path, overwrite = T)
          about <- fread(about_path, quote = "", fill = TRUE) %>%
            mutate(
              about_content = str_replace_all(about_content, '["]', ""),
              about_content = str_squish(about_content)
            )
          output$about_content <- renderText({
            about %>% unlist() %>% unname() %>% paste(collapse = "<br><br>")
          })
        }
      })
    }
  )
}
