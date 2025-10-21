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
# TABLE SERVER ####
firearm_table_server <- function(
  id,
  firearm_table,
  firearm_summary_table,
  palette_color
) {
  moduleServer(
    id,
    function(input, output, session) {
      # DOWNLOAD TABLE ####
      output$download_table <- downloadHandler(
        filename = function() {
          paste0("ukr_media_data_", language_react(), ".xlsx")
        },
        content = function(file) {
          write_xlsx(
            table_init() %>%
              rowwise() %>%
              mutate(
                across(
                  any_of(c("Item", "Стаття")),
                  ~ paste(
                    na.omit(unlist(str_extract_all(
                      string = .,
                      pattern = palette_color %>%
                        names() %>%
                        str_remove_all("[.]")
                    ))),
                    collapse = "; "
                  )
                ),
                across(
                  any_of(c("Source", "Джерело")),
                  ~ unlist(str_match(
                    string = .,
                    pattern = 'href="\\s*(.*?)\\s*" target'
                  ))[2]
                ),
                across(
                  any_of(c("Oblast", "Область")),
                  ~ str_replace_all(
                    string = .,
                    pattern = ";",
                    replacement = "; "
                  )
                )
              ) %>%
              ungroup(),
            path = file
          )
        }
      )
    }
  )
}
