# UKR TABLE ####
# TABLE UI INTERFACE ####
firearm_table_ui <- function(id) {
  ns <- NS(id)
  # conect to mongodb
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
fireram_table_download_ui <-
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
      ## LANGUAGE ####
      # create reactive values
      language_react <- reactiveVal("eng")
      # select language
      observeEvent(input$firearm_language_eng, {
        language_react("eng")
      })
      observeEvent(input$firearm_language_ukr, {
        language_react("ukr")
      })

      # update filters and reactive values according to language
      observeEvent(language_react(), ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_oblast_filter")
        shinyjs::disable("firearm_item_filter")

        # language
        language <- language_react()

        #update filters language
        updatePickerInput(
          "firearm_item_filter",
          choices = firearm_summary_table %>%
            distinct(!!sym(paste0("item_list_", language))) %>%
            arrange(!!sym(paste0("item_list_", language))) %>%
            mutate(
              clean_name = str_replace_all(
                !!sym(paste0("item_list_", language)),
                "[.]",
                ""
              )
            ) %>%
            collect() %>%
            {
              setNames(.[[paste0("item_list_", language)]], .[["clean_name"]])
            },
          selected = character(0),
          session = session
        )
        updatePickerInput(
          "firearm_oblast_filter",
          choices = firearm_summary_table %>%
            distinct(!!sym(paste0("location_oblast_", language))) %>%
            arrange(!!sym(paste0("location_oblast_", language))) %>%
            mutate(
              clean_name = str_replace_all(
                !!sym(paste0("location_oblast_", language)),
                "[.]",
                ""
              )
            ) %>%
            collect() %>%
            {
              setNames(
                .[[paste0("location_oblast_", language)]],
                .[["clean_name"]]
              )
            },
          selected = character(0),
          session = session
        )
        # fail to change date picker locale
        #updateAirDateInput("firearm_date_filter", options=list(locale=ifelse(language=="eng", 8714, 7257)), session=session)

        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_oblast_filter")
        shinyjs::enable("firearm_item_filter")
      })

      ## FILTERS ####
      ### ITEM ####
      observeEvent(input$firearm_item_filter, ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_item_filter")
        shinyjs::disable("firearm_oblast_filter")

        # language
        item_language <- language_react()

        # get items from menu or all
        item <- paste0('"', input$firearm_item_filter, '"')
        if ("\"\"" %in% item) {
          item <- paste0(
            '"',
            firearm_summary_table %>%
              distinct(!!sym(paste0("item_list_", item_language))) %>%
              pull(!!sym(paste0("item_list_", item_language))) %>%
              sort() %>%
              collect(),
            '"'
          )
        }

        # get oblasts for items
        item_oblast <-
          firearm_summary_table %>%
          filter(
            !!sym(paste0("item_list_", item_language)) %in%
              input$firearm_item_filter
          ) %>%
          collect() |>
          select(c(contains(paste0("location_oblast_", item_language)))) %>%
          rename_with(~ str_replace(., paste0("_", item_language), "")) %>%
          distinct() %>%
          pull(location_oblast) %>%
          sort()

        # get date for items
        if (length(input$firearm_date_filter) == 2) {
          item_date <- data.frame(
            date_min_filter = as.Date(input$firearm_date_filter[1]),
            date_max_filter = as.Date(input$firearm_date_filter[2])
          )
        } else {
          item_date <- data.frame(
            date_min_filter = as.Date(input$firearm_date_filter[1]),
            date_max_filter = as.Date(input$firearm_date_filter[1])
          )
        }

        item_date_data <- firearm_table %>%
          filter(
            !!sym(paste0("item_list_", item_language)) %in%
              input$firearm_item_filter
          ) %>%
          summarise(
            date_min = min(date_publication, na.rm = TRUE),
            date_max = max(date_publication, na.rm = TRUE)
          ) %>%
          mutate(across(starts_with("date_"), as.Date), .keep = "used") %>%
          collect()

        if (item_date_data %>% nrow() == 1) {
          item_date <- item_date %>%
            cbind(item_date_data) %>%
            mutate(
              date_min = ifelse(
                date_min > date_min_filter,
                as.character(date_min),
                as.character(date_min_filter)
              ) %>%
                as.Date(),
              date_max = ifelse(
                date_max < date_max_filter,
                as.character(date_max),
                as.character(date_max_filter)
              ) %>%
                as.Date()
            )
        } else {
          item_date
        }

        #update filters item
        updatePickerInput(
          "firearm_oblast_filter",
          choices = item_oblast %>%
            sort() %>%
            setNames(item_oblast %>% sort() %>% str_replace_all("[.]", "")),
          selected = isolate(input$firearm_oblast_filter),
          session = session
        )

        updateAirDateInput(
          "firearm_date_filter",
          value = c(item_date$date_min[1], item_date$date_max[1]),
          session = session
        )

        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_item_filter")
        shinyjs::enable("firearm_oblast_filter")
      })

      ### OBLAST ####
      observeEvent(input$firearm_oblast_filter, ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_oblast_filter")
        shinyjs::disable("firearm_item_filter")

        oblast_language <- language_react()

        oblast <-
          paste0('"', input$firearm_oblast_filter, '"')

        if ("\"\"" %in% oblast) {
          oblast <- paste0(
            '"',
            firearm_summary_table %>%
              distinct(!!sym(paste0("location_oblast_", oblast_language))) %>%
              sort() %>%
              collect(),
            '"'
          )
        }
        oblast_item <-
          firearm_summary_table %>%
          filter(
            !!sym(paste0("location_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) %>%
          collect() %>%
          select(contains(paste0("item_list_", oblast_language))) %>%
          rename_with(~ str_replace(., paste0("_", oblast_language), "")) %>%
          distinct() %>%
          pull(item_list) %>%
          sort()

        # get date for oblasts
        if (length(input$firearm_date_filter) == 2) {
          oblast_date <- data.frame(
            date_min_filter = as.Date(input$firearm_date_filter[1]),
            date_max_filter = as.Date(input$firearm_date_filter[2])
          )
        } else {
          oblast_date <- data.frame(
            date_min_filter = as.Date(input$firearm_date_filter[1]),
            date_max_filter = as.Date(input$firearm_date_filter[1])
          )
        }

        oblast_date_data <- firearm_table %>%
          filter(
            !!sym(paste0("location_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) %>%
          summarise(
            date_min = min(date_publication, na.rm = TRUE),
            date_max = max(date_publication, na.rm = TRUE)
          ) %>%
          mutate(across(starts_with("date_"), as.Date), .keep = "used") %>%
          collect()

        if (oblast_date_data %>% nrow() == 1) {
          oblast_date <- oblast_date %>%
            cbind(oblast_date_data) %>%
            mutate(
              date_min = ifelse(
                date_min > date_min_filter,
                as.character(date_min),
                as.character(date_min_filter)
              ) %>%
                as.Date(),
              date_max = ifelse(
                date_max < date_max_filter,
                as.character(date_max),
                as.character(date_max_filter)
              ) %>%
                as.Date()
            )
        }

        updatePickerInput(
          "firearm_item_filter",
          choices = oblast_item %>%
            sort() %>%
            setNames(oblast_item %>% sort() %>% str_replace_all("[.]", "")),
          selected = isolate(input$firearm_item_filter),
          session = session
        )

        updateAirDateInput(
          "firearm_date_filter",
          value = c(oblast_date$date_min[1], oblast_date$date_max[1]),
          session = session
        )
        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_oblast_filter")
        shinyjs::enable("firearm_item_filter")
      })

      update_react <- reactiveVal(0)

      ### RESET ####
      observeEvent(input$firearm_reset_filter, ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_oblast_filter")
        shinyjs::disable("firearm_item_filter")

        reset_language <- language_react()
        updatePickerInput(
          "firearm_oblast_filter",
          selected = character(0),
          choices = firearm_summary_table %>%
            distinct(!!sym(paste0("location_oblast_", reset_language))) %>%
            arrange(!!sym(paste0("location_oblast_", reset_language))) %>%
            mutate(
              clean_name = str_replace_all(
                !!sym(paste0("location_oblast_", reset_language)),
                "[.]",
                ""
              )
            ) %>%
            collect() %>%
            {
              setNames(
                .[[paste0("location_oblast_", reset_language)]],
                .[["clean_name"]]
              )
            },
          session = session
        )
        updatePickerInput(
          "firearm_item_filter",
          selected = character(0),
          choices = firearm_summary_table %>%
            distinct(!!sym(paste0("item_list_", reset_language))) %>%
            arrange(!!sym(paste0("item_list_", reset_language))) %>%
            mutate(
              clean_name = str_replace_all(
                !!sym(paste0("item_list_", reset_language)),
                "[.]",
                ""
              )
            ) %>%
            collect() %>%
            {
              setNames(
                .[[paste0("item_list_", reset_language)]],
                .[["clean_name"]]
              )
            },
          session = session
        )
        updateAirDateInput(
          "firearm_date_filter",
          value = c(
            firearm_table %>%
              distinct(date_publication) %>%
              collect() %>%
              pull(date_publication) %>%
              as.Date() %>%
              min(),
            firearm_table %>%
              distinct(date_publication) %>%
              collect() %>%
              pull(date_publication) %>%
              as.Date() %>%
              max()
          ),
          session = session
        )
        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_oblast_filter")
        shinyjs::enable("firearm_item_filter")
      })

      observeEvent(
        c(
          input$firearm_oblast_filter,
          input$firearm_item_filter,
          input$firearm_date_filter,
          language_react()
        ),
        ignoreInit = T,
        ignoreNULL = F,
        {
          if (
            is.null(input$firearm_oblast_filter) &
              is.null(input$firearm_item_filter) &
              str_detect(input$firearm_date_filter, "2024-02-31") %>% min() ==
                0 &
              update_react() > 0
          ) {
            update_react(update_react() + 1)
          }
        }
      )

      ### SUBMIT ####
      observeEvent(input$firearm_submit_filter, ignoreInit = T, {
        update_react(update_react() + 1)
      })

      ## TABLE INT ####
      fields <- c(
        "date_publication",
        "post_url",
        "post_author_eng",
        "post_title_eng",
        "post_content_eng",
        "location_oblast_eng",
        "item_list_eng"
      ) %>%
        map_vec(function(x) paste0('"', x, '"', ": true")) %>%
        paste(collapse = ", ")

      field_names <- c(
        "date_publication",
        "post_url",
        "post_author",
        "post_title",
        "post_content",
        "location_oblast",
        "item_list"
      ) %>%
        setNames(c(
          "Date",
          "Source",
          "Author",
          "Title",
          "Content",
          "Oblast",
          "Item"
        ))

      table_init <- reactiveVal(
        firearm_table %>%
          select(date_publication, post_url, ends_with("eng")) %>%
          mutate(date_publication = as.Date(date_publication)) %>%
          collect() |>
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          mutate(
            location_oblast = location_oblast %>% str_remove_all("[.]")
          ) %>%
          drop_na(post_url) %>%
          rowwise() %>%
          mutate(
            item_list = item_list %>%
              {
                if (!is.na(.)) {
                  str_split(., ";") %>%
                    map_vec(function(x) {
                      paste0(
                        "<span class='item_tag'; style='background-color:",
                        x %>%
                          str_split(";") %>%
                          str_replace_all(fixed(palette_color)),
                        ";'>",
                        x %>% str_split(";"),
                        "</span>",
                        collapse = " "
                      )
                    }) %>%
                    paste(collapse = " ")
                } else {
                  .
                }
              }
          ) %>%
          ungroup() %>%
          mutate(
            location_oblast = location_oblast %>% str_remove_all("[.]"),
            location_oblast = location_oblast %>%
              str_replace_all(pattern = ";", replacement = "; ")
          ) %>%
          mutate(item_list = item_list %>% str_remove_all("[.]")) %>%
          rowwise() %>%
          mutate(
            post_url = if (str_detect(post_url, "facebook")) {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("facebook"))),
                href = post_url,
                target = "_blank"
              )))
            } else if (str_detect(post_url, "t.me")) {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("telegram"))),
                href = post_url,
                target = "_blank"
              )))
            } else {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("globe"))),
                href = post_url,
                target = "_blank"
              )))
            }
          ) %>%
          ungroup() %>%
          rename(field_names) %>%
          relocate(2, .after = last_col()) %>%
          arrange(1)
      )

      output$firearm_table <- renderDT({
        table_init() %>%
          datatable(
            rownames = FALSE,
            escape = FALSE,
            filter = "none",
            selection = "none",
            options = list(
              dom = 'ftp',
              fixedHeader = TRUE,
              autoWidth = TRUE,
              pageLength = 10,
              ordering = F,
              scrollX = TRUE,
              scrollY = "70vh"
            )
          ) %>%
          formatDate(
            columns = 1,
            method = "toLocaleDateString",
            params = list(
              'en-UK',
              list(year = 'numeric', month = 'numeric', day = "numeric")
            )
          )
      })

      # UPDATE TABLE ####
      observeEvent(update_react(), ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_oblast_filter")
        shinyjs::disable("firearm_item_filter")

        firearm_proxy <- DT::dataTableProxy("firearm_table", session = session)

        # update table
        filter_language <- language_react()

        if (is.null(input$firearm_oblast_filter) %>% sum() == 0) {
          filter_oblast <- input$firearm_oblast_filter
        } else {
          filter_oblast <- firearm_summary_table %>%
            distinct(!!sym(paste0("location_oblast_", filter_language))) %>%
            pull(!!sym(paste0("location_oblast_", filter_language))) %>%
            collect()
        }

        if (is.null(input$firearm_item_filter) %>% sum() == 0) {
          filter_item <- input$firearm_item_filter
        } else {
          filter_item <- firearm_summary_table %>%
            distinct(!!sym(paste0("item_list_", filter_language))) %>%
            pull(!!sym(paste0("item_list_", filter_language))) %>%
            collect()
        }

        filter_date_min <- input$firearm_date_filter[1]
        if (is.null(input$firearm_date_filter[1])) {
          filter_date_min <- firearm_table %>%
            distinct(date_publication) %>%
            collect() %>%
            pull(date_publication) %>%
            as.Date() %>%
            min()
        }

        filter_date_max <- input$firearm_date_filter[2]
        if (is.null(input$firearm_date_filter[2])) {
          filter_date_max <- firearm_table %>%
            distinct(date_publication) %>%
            collect() %>%
            pull(date_publication) %>%
            as.Date() %>%
            max()
        }

        if (is.na(input$firearm_date_filter[2])) {
          filter_date_max <- filter_date_min
        }

        fields <- c(
          "date_publication",
          "post_url",
          "post_author_eng",
          "post_title_eng",
          "post_content_eng",
          "location_oblast_eng",
          "item_list_eng"
        ) %>%
          map_vec(function(x) paste0('"', x, '"', ": true")) %>%
          paste(collapse = ", ") %>%
          str_replace_all("_eng", paste0("_", filter_language))

        #upadte table
        table_submit <- firearm_table %>%
          select(date_publication, post_url, ends_with(filter_language)) %>%
          collect() |>
          mutate(date_publication = date_publication %>% as.Date()) %>%
          rename_with(~ str_replace(., paste0("_", filter_language), "")) %>%
          drop_na(post_url) %>%
          filter(str_detect(
            location_oblast,
            filter_oblast %>% paste(collapse = "|")
          )) %>%
          filter(str_detect(
            item_list,
            filter_item %>% paste(collapse = "|")
          )) %>%
          filter(
            date_publication >= filter_date_min &
              date_publication <= filter_date_max
          ) %>%
          rowwise() %>%
          mutate(
            item_list = item_list %>%
              {
                if (!is.na(.)) {
                  str_split(., ";") %>%
                    map_vec(function(x) {
                      paste0(
                        "<span class='item_tag'; style='background-color:",
                        x %>%
                          str_split(";") %>%
                          str_replace_all(fixed(palette_color)),
                        ";'>",
                        x %>% str_split(";"),
                        "</span>",
                        collapse = " "
                      )
                    }) %>%
                    paste(collapse = " ")
                } else {
                  .
                }
              }
          ) %>%
          ungroup() %>%
          mutate(
            location_oblast = location_oblast %>% str_remove_all("[.]"),
            location_oblast = location_oblast %>%
              str_replace_all(pattern = ";", replacement = "; ")
          ) %>%
          mutate(item_list = item_list %>% str_remove_all("[.]")) %>%
          rowwise() %>%
          mutate(
            post_url = if (str_detect(post_url, "facebook")) {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("facebook"))),
                href = post_url,
                target = "_blank"
              )))
            } else if (str_detect(post_url, "t.me")) {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("telegram"))),
                href = post_url,
                target = "_blank"
              )))
            } else {
              HTML(as.character(tags$a(
                HTML(as.character(bsicons::bs_icon("globe"))),
                href = post_url,
                target = "_blank"
              )))
            }
          ) %>%
          ungroup() %>%
          relocate(2, .after = last_col()) %>%
          arrange(1)

        if (filter_language == "eng") {
          colnames(table_submit) <- c(
            "Date",
            "Author",
            "Title",
            "Content",
            "Oblast",
            "Item",
            "Source"
          )
        } else {
          colnames(table_submit) <- c(
            "Дата",
            "Автор",
            "Назва",
            "Зміст",
            "Область",
            "Стаття",
            "Джерело"
          )
        }
        table_init(table_submit)

        DT::replaceData(
          proxy = firearm_proxy,
          data = table_init(),
          rownames = FALSE,
          resetPaging = TRUE
        )

        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_oblast_filter")
        shinyjs::enable("firearm_item_filter")
      })

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
