# UKR DASHBOARD ####
# MAIN UI INTERFACE ####
firearm_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # grid layout
    layout_columns(
      col_widths = c(8, 4),
      row_heights = c("41vh", "31vh"),
      # map
      card(
        card_header("Locations"),
        card_body(leafletOutput(ns("firearm_map"))),
        full_screen = T
      ),
      # summary boxes
      tagList(
        # date last record
        value_box(
          title = "Last update",
          value = firearm_table |>
            distinct(post_date) |>
            collect() %>%
            pull() |>
            max() %>%
            as.Date() %>%
            format("%d/%m/%Y"),
          showcase = icon("calendar-days"),
          theme = "primary",
          fill = TRUE,
          heigth = "3wv"
        ) %>%
          tags$div(class = "show_card"),
        # number of posts
        value_box(
          title = "Sources",
          value = textOutput(ns("box_sources_value")),
          showcase = icon("file"),
          theme = "primary",
          fill = TRUE,
          heigth = "3wv"
        ) %>%
          tags$div(class = "show_card"),
        # number of items
        value_box(
          title = "Mentions",
          value = textOutput(ns("box_mentions_value")),
          showcase = icon("person-rifle"),
          theme = "primary",
          fill = TRUE,
          heigth = "3wv"
        ) %>%
          tags$div(class = "show_card")
      ) %>%
        tags$div(class = "hide_card"),
      # time series
      card(
        card_header("Time series"),
        card_body(plotlyOutput(ns("firearm_hist"))),
        full_screen = T
      ),
      # proportions
      card(
        card_header("Proportions"),
        card_body(plotlyOutput(ns("firearm_pie"))),
        full_screen = T
      )
    )
  ) %>%
    tags$div(class = "firearm_card")
}
# SIDE UI INTERFACE ####
firearm_side_ui <- function(id) {
  ns <- NS(id)
  # sidebar layout
  tagList(
    h3("Filters"),
    tags$div(
      tags$div("Language", class = ".shiny-input-container .control-label"),
      # language selection
      tags$div(
        shinyWidgets::actionBttn(
          inputId = ns("firearm_language_eng"),
          label = "English",
          style = "simple",
          color = "primary",
          size = "sm",
          block = FALSE
        ),
        shinyWidgets::actionBttn(
          inputId = ns("firearm_language_ukr"),
          label = "Українська",
          style = "simple",
          color = "primary",
          size = "sm",
          block = FALSE
        ),
        class = "filter"
      )
    ),
    # filters
    # item
    pickerInput(
      inputId = ns("firearm_item_filter"),
      label = "Item",
      multiple = T,
      width = "100%",
      choices = {
        choices <- firearm_summary_table |>
          distinct(post_item_eng) |>
          collect() |>
          pull() |>
          sort()
        setNames(choices, str_replace_all(choices, "[.]", ""))
      },
      selected = character(0)
    ),
    # oblast
    pickerInput(
      inputId = ns("firearm_oblast_filter"),
      label = "Oblast",
      multiple = T,
      width = "100%",
      choices = {
        choices <- firearm_summary_table |>
          distinct(post_oblast_eng) |>
          collect() |>
          pull(post_oblast_eng) |>
          sort()
        setNames(choices, str_replace_all(choices, "[.]", ""))
      },
      selected = character(0)
    ),
    # date
    airDatepickerInput(
      inputId = ns("firearm_date_filter"),
      label = "Date",
      addon = "none",
      clearButton = T,
      autoClose = T,
      width = "100%",
      update = "close",
      dateFormat = "dd/MM/yyyy",
      range = T,
      view = c("months"),
      minView = c("months"),
      value = c(
        firearm_table |>
          distinct(post_date_month) |>
          collect() %>%
          pull() |>
          min() |>
          floor_date("month"),
        firearm_table |>
          distinct(post_date_month) |>
          collect() %>%
          pull() |>
          max()
      ),
      minDate = firearm_table |>
        distinct(post_date_month) |>
        collect() %>%
        pull() |>
        min(),
      maxDate = firearm_table |>
        distinct(post_date_month) |>
        collect() %>%
        pull() |>
        max()
    ),
    # filters apply
    tags$div(
      shinyWidgets::actionBttn(
        inputId = ns("firearm_submit_filter"),
        icon = icon("rotate"),
        label = "Update",
        style = "simple",
        color = "primary",
        size = "sm",
        block = FALSE
      ),
      shinyWidgets::actionBttn(
        inputId = ns("firearm_reset_filter"),
        icon = icon("ban"),
        label = "Reset",
        style = "simple",
        color = "primary",
        size = "sm",
        block = FALSE
      ),
      class = "filter"
    )
  )
}
firearm_summary_download_ui <- function(id) {
  ns <- NS(id)
  shinyWidgets::actionBttn(
    inputId = ns("download_screenshot"),
    icon = icon("download"),
    label = "Download",
    style = "simple",
    color = "primary",
    size = "sm",
    block = FALSE
  )
}
# SUMMARY SERVER ####
firearm_summary_server <- function(
  id,
  firearm_table,
  firearm_summary_table,
  palette_color,
  palette_factor
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
        shinyjs::disable("firearm_item_filter")
        shinyjs::disable("firearm_oblast_filter")

        # language
        language <- language_react()

        #update filters language
        updatePickerInput(
          "firearm_item_filter",
          choices = {
            choices <- firearm_summary_table |>
              select(contains(paste0(
                "post_item_",
                language
              ))) |>
              distinct() %>%
              collect() |>
              pull() |>
              sort()
            setNames(choices, str_replace_all(choices, "[.]", ""))
          },
          selected = character(0),
          session = session
        )
        updatePickerInput(
          "firearm_oblast_filter",
          choices = {
            choices <- firearm_summary_table |>
              select(contains(paste0(
                "post_oblast_",
                language
              ))) |>
              collect() |>
              pull() |>
              sort()
            setNames(choices, str_replace_all(choices, "[.]", ""))
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
        shinyjs::enable("firearm_item_filter")
        shinyjs::enable("firearm_oblast_filter")
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
        if (is.null(input$firearm_item_filter) %>% sum() == 1) {
          item <- paste0(
            '"',
            firearm_summary_table |>
              select(contains(paste0("post_item_", item_language))) |>
              distinct() |>
              pull() %>%
              sort(),
            '"'
          )
        }

        # get oblasts for items
        item_oblast <- firearm_summary_table |>
          filter(
            !!sym(paste0("post_item_", item_language)) %in%
              input$firearm_item_filter
          ) |>
          select(contains(paste0("post_oblast_", item_language))) |>
          rename_with(~ str_replace(., paste0("_", item_language), "")) |>
          distinct() |>
          collect() |>
          pull(post_oblast) |>
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

        firearm_item_filter_flattened <-
          str_flatten(input$firearm_item_filter, collapse = "|")

        item_date_data <- firearm_table |>
          filter(
            str_detect(
              !!sym(paste0("post_item_", item_language)),
              firearm_item_filter_flattened
            )
          ) |>
          summarize(
            date_min = min(post_date_month, na.rm = TRUE),
            date_max = max(post_date_month, na.rm = TRUE)
          ) |>
          mutate(across(starts_with("date_"), as.Date), .keep = "used") |>
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
        req(item_oblast)
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
            firearm_summary_table |>
              select(contains(paste0(
                "post_oblast_",
                oblast_language
              ))) |>
              distinct() %>%
              collect() |>
              pull() |>
              sort(),
            '"'
          )
        }
        oblast_item <- firearm_summary_table |>
          filter(
            !!sym(paste0("post_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) |>
          select(contains(paste0("post_item_", oblast_language))) |>
          rename_with(~ str_replace(., paste0("_", oblast_language), "")) |>
          distinct() |>
          collect() |>
          pull(post_item) |>
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

        oblast_date_data <- firearm_table |>
          filter(
            !!sym(paste0("post_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) |>
          summarize(
            date_min = min(post_date, na.rm = TRUE),
            date_max = max(post_date, na.rm = TRUE)
          ) |>
          mutate(across(starts_with("date_"), as.Date), .keep = "used") |>
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
        req(oblast_item)
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
          choices = {
            choices <- firearm_summary_table |>
              distinct(!!sym(paste0("post_oblast_", reset_language))) |>
              collect() |>
              pull() |>
              sort()
            setNames(choices, str_replace_all(choices, "[.]", ""))
          },
          session = session
        )
        updatePickerInput(
          "firearm_item_filter",
          selected = character(0),
          choices = {
            choices <- firearm_summary_table |>
              distinct(!!sym(paste0("post_item_", reset_language))) |>
              collect() |>
              pull() |>
              sort()
            setNames(choices, str_replace_all(choices, "[.]", ""))
          },
          session = session
        )
        updateAirDateInput(
          "firearm_date_filter",
          value = c(
            firearm_table |>
              distinct(post_date) |>
              collect() %>%
              pull() |>
              as.Date() %>%
              min(),
            firearm_table |>
              distinct(post_date) |>
              collect() %>%
              pull() |>
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

      ## INITIAL PLOTS ####
      output$box_sources_value <- renderText({
        paste0(
          firearm_table |>
            distinct(post_link) %>%
            collect() %>%
            pull() %>%
            length() %>%
            format(big.mark = ",", scientific = FALSE),
          " posts"
        )
      })
      output$box_mentions_value <- renderText({
        paste0(
          firearm_table |>
            select(contains("post_item_")) %>%
            collect() %>%
            separate_longer_delim(post_item_eng, "; ") %>%
            drop_na(post_item_eng) %>%
            nrow() %>%
            format(big.mark = ",", scientific = FALSE),
          " items"
        )
      })
      ### MAP ####
      output$firearm_map <- renderLeaflet({
        #get coords
        map_coords <-
          firearm_summary_table %>%
          collect() %>%
          select(c(
            ends_with("eng"),
            "post_oblast_latitude",
            "post_oblast_longitude"
          )) %>%
          rename_with(~ str_replace(., paste0("_", "eng"), ""))
        # create map in leaflet
        map_init <- firearm_table %>%
          select(post_date, post_oblast_eng, post_item_eng) %>%
          collect() %>%
          mutate(post_date = as.Date(post_date)) %>%
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(post_item, "; ") %>%
          separate_longer_delim(post_oblast, "; ") %>%
          drop_na(post_item) %>%
          drop_na(post_oblast) %>%
          filter(!is.na(post_date)) %>%
          group_by(post_oblast, post_item) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          left_join(map_coords, by = join_by(post_oblast, post_item)) %>%
          drop_na(post_oblast_latitude) %>%
          sf::st_as_sf(
            coords = c("post_oblast_longitude", "post_oblast_latitude"),
            remove = F
          ) %>%
          mutate(col = post_item %>% str_replace_all(fixed(palette_color))) %>%
          arrange(desc(post_mention), col)

        # plot map
        map_init %>%
          leaflet(
            options = leafletOptions(
              attributionControl = FALSE,
              minZoom = 4,
              maxZoom = 10
            )
          ) %>%
          addCircleMarkers(
            group = "firearm_markers",
            radius = ~ ifelse(
              sum(map_init$post_mention) > 50,
              post_mention / sum(map_init$post_mention) * 5000,
              post_mention
            ),
            label = ~ paste0(
              "<b>Oblast: </b>",
              post_oblast %>% str_remove_all("[.]"),
              "<br><b>Item: </b>",
              post_item %>% str_remove_all("[.]"),
              "<br><b>Mentions: </b>",
              post_mention
            ) %>%
              lapply(htmltools::HTML),
            stroke = T,
            color = ~ palette_factor(post_item),
            weight = 3,
            opacity = 1,
            fill = T,
            fillColor = ~ palette_factor(post_item),
            fillOpacity = 0.2,
            labelOptions = labelOptions(
              style = list(
                "border-radius" = "0px",
                "border-color" = "transparent",
                "padding" = "1px"
              )
            )
          ) %>%
          addProviderTiles(providers$CartoDB.DarkMatter)
      })

      ### HISTOGRAM ####
      output$firearm_hist <- renderPlotly({
        # create histogram in plotly
        hist_init <-
          firearm_table %>%
          select(post_date_month, post_oblast_eng, post_item_eng) |>
          collect() %>%
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(post_item, "; ") %>%
          separate_longer_delim(post_oblast, "; ") %>%
          drop_na(post_item) %>%
          drop_na(post_oblast) %>%
          filter(!is.na(post_date_month)) %>%
          group_by(post_date_month, post_item) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          filter(post_mention > 0)

        hist_plot <-
          hist_init %>%
          ggplot(aes(
            x = post_date_month,
            y = post_mention,
            fill = reorder(post_item, post_mention),
            text = paste0(
              "<b>Date:</b> ",
              post_date_month %>% format("%m/%Y"),
              "<br><b>Item:</b> ",
              post_item %>% str_remove_all("[.]"),
              "<br><b>Mentions:</b> ",
              post_mention
            )
          )) +
          geom_bar(stat = 'identity') +
          scale_x_date(date_labels = "%m/%Y") +
          scale_fill_manual(values = palette_color) +
          theme(
            legend.position = 'none',
            plot.margin = unit(c(0, -20, -40, -20), "points"),
            panel.spacing = unit(-10, "points"),
            panel.grid.major.y = element_line(
              color = "grey92",
              linewidth = 0.25
            ),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = 'transparent'),
            plot.background = element_rect(fill = 'transparent', color = NA)
          )

        # plot histogram
        hist_plot %>%
          ggplotly(tooltip = HTML("text")) %>%
          config(displayModeBar = FALSE) %>%
          layout(
            hoverlabel = list(
              bgcolor = "white",
              bordercolor = "white",
              font = list(color = "black", family = "Comfortaa", size = 10),
              align = "left"
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = T),
            xaxis = list(fixedrange = TRUE),
            yaxis = list(fixedrange = TRUE)
          )
      })
      ### PIE ####
      output$firearm_pie <- renderPlotly({
        # create pie in plotly
        pie_init <- firearm_table %>%
          select(post_item_eng) |>
          collect() |>
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(post_item, "; ") %>%
          drop_na(post_item) %>%
          group_by(post_item) %>%
          summarize(post_mention = n()) %>%
          ungroup() |>
          mutate(
            post_mention_percent = round(
              post_mention / sum(post_mention) * 100,
              1
            )
          )

        # plot pie
        pie_init %>%
          mutate(.color = palette_color[post_item]) %>%
          mutate(post_item = post_item %>% str_replace_all("[.]", "")) %>%
          mutate(
            .text = paste0(
              "<b>Mentions:</b> ",

              post_mention,
              "%<br>",
              "<b>Proportion:</b> ",
              format(post_mention_percent, nsmall = 1, big.mark = ","),
              "%<br>"
            )
          ) %>%
          plot_ly(
            x = ~post_mention,
            y = ~ reorder(post_item, post_mention),
            text = ~.text,
            type = "bar",
            orientation = "h",
            marker = list(color = ~.color),
            textposition = "none",
            hovertemplate = paste("%{text}", "<extra></extra>")
          ) %>%
          config(displayModeBar = FALSE) %>%
          layout(
            font = list(family = "Comfortaa", color = 'white'),
            hoverlabel = list(
              bgcolor = "white",
              bordercolor = "white",
              font = list(color = "black", family = "Comfortaa", size = 10),
              align = "left"
            ),
            margin = list(
              l = 0,
              r = 0,
              b = 0,
              t = 0,
              pad = 0,
              autoexpand = TRUE
            ),
            autosize = TRUE,
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent',
            showlegend = FALSE,
            xaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE,
              title = ""
            ),
            yaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = TRUE,

              title = ""
            )
          )
      })

      ## UPDATE PLOTS ####
      observeEvent(update_react(), ignoreInit = T, {
        shinyjs::disable("firearm_submit_filter")
        shinyjs::disable("firearm_reset_filter")
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_item_filter")
        shinyjs::disable("firearm_oblast_filter")

        filter_language <- language_react()
        filter_oblast <- firearm_summary_table |>
          distinct(!!sym(paste0("post_oblast_", filter_language))) |>
          collect() |>
          pull() |>
          sort()
        if (input$firearm_oblast_filter %>% length() > 0) {
          filter_oblast <- input$firearm_oblast_filter
        }

        filter_item <- firearm_summary_table |>
          distinct(!!sym(paste0("post_item_", filter_language))) |>
          collect() |>
          pull() |>
          sort()

        if (input$firearm_item_filter %>% length() > 0) {
          filter_item <- input$firearm_item_filter
        }

        filter_date_min <- input$firearm_date_filter[1]
        if (is.null(input$firearm_date_filter[1])) {
          filter_date_min <- firearm_table |>
            distinct(post_date) |>
            collect() |>
            pull() |>
            as.Date() |>
            min()
        }

        filter_date_max <- input$firearm_date_filter[2]
        if (is.null(input$firearm_date_filter[2])) {
          filter_date_max <- firearm_table |>
            distinct(post_date) |>
            collect() |>
            pull(post_date) |>
            as.Date() |>
            max()
        }
        if (is.na(input$firearm_date_filter[2])) {
          filter_date_max <- filter_date_min
        }
        # filtered table
        firearm_table_filtered <- firearm_table %>%
          select(
            post_link,
            post_date,
            post_date_month,
            !!sym(paste0("post_oblast_", filter_language)),
            !!sym(paste0("post_item_", filter_language))
          ) %>%
          collect() %>%
          rename_with(~ str_replace(., paste0("_", filter_language), "")) %>%
          separate_longer_delim(post_item, "; ") %>%
          separate_longer_delim(post_oblast, "; ") %>%
          drop_na(post_item) %>%
          drop_na(post_oblast) %>%
          #filter(post_oblast %in% filter_oblast) %>%
          filter(post_item %in% filter_item) %>%
          # filter(
          #   post_date_month >= filter_date_min &
          #     post_date_month <= filter_date_max
          # ) %>%
          filter(!is.na(post_date_month))

        ### BOXES ####
        output$box_sources_value <- renderText({
          paste0(
            firearm_table_filtered |>
              distinct(post_link) %>%
              pull() %>%
              length() %>%
              format(big.mark = ",", scientific = FALSE),
            " posts"
          )
        })

        output$box_mentions_value <- renderText({
          paste0(
            firearm_table_filtered %>%
              nrow() %>%
              format(big.mark = ",", scientific = FALSE),
            " items"
          )
        })

        ### MAP ####
        map_coords <-
          firearm_summary_table %>%
          select(c(
            ends_with(language_react()),
            "post_oblast_latitude",
            "post_oblast_longitude"
          )) %>%
          rename_with(~ str_replace(., paste0("_", filter_language), "")) |>
          collect()

        map_submit <- firearm_table_filtered |>
          group_by(post_oblast, post_item) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          left_join(map_coords, by = join_by(post_oblast, post_item)) %>%
          drop_na(post_oblast_latitude) %>%
          sf::st_as_sf(
            coords = c("post_oblast_longitude", "post_oblast_latitude"),
            remove = F
          ) %>%
          mutate(col = post_item %>% str_replace_all(fixed(palette_color))) %>%
          arrange(desc(post_mention), col)

        leafletProxy("firearm_map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addCircleMarkers(
            data = map_submit,
            group = "firearm_markers",
            radius = ~ ifelse(
              sum(map_submit$post_mention) > 50,
              post_mention / sum(map_submit$post_mention) * 5000,
              post_mention
            ),
            label = ~ paste0(
              ifelse(
                filter_language == "eng",
                "<b>Oblast: </b>",
                "<b>Область: </b>"
              ),
              post_oblast %>% str_remove_all("[.]"),
              ifelse(
                filter_language == "eng",
                "<br><b>Item: </b>",
                "<br><b>Пункт: </b>"
              ),
              post_item %>% str_remove_all("[.]"),
              ifelse(
                filter_language == "eng",
                "<br><b>Mentions: </b>",
                "<br><b>Згадки: </b>"
              ),
              post_mention
            ) %>%
              lapply(htmltools::HTML),
            stroke = T,
            color = ~ palette_factor(post_item),
            weight = 3,
            opacity = 1,
            fill = T,
            fillColor = ~ palette_factor(post_item),
            fillOpacity = 0.2,
            labelOptions = labelOptions(
              style = list(
                "border-radius" = "0px",
                "border-color" = "transparent",
                "padding" = "1px"
              )
            )
          )

        ##  HISTOGRAM ####
        output$firearm_hist <- renderPlotly({
          # create histogram in plotly
          hist_submit <- firearm_table_filtered %>%
            group_by(post_date_month, post_item) %>%
            summarize(post_mention = n()) %>%
            ungroup() %>%
            filter(post_mention > 0)

          hist_plot <-
            hist_submit %>%
            ggplot(aes(
              x = post_date_month,
              y = post_mention,
              fill = reorder(post_item, post_mention),
              #fill=post_item,
              text = paste0(
                ifelse(
                  filter_language == "eng",
                  "<b>Date: </b>",
                  "<b>Дата: </b>"
                ),
                post_date_month %>% format("%m/%Y"),
                ifelse(
                  filter_language == "eng",
                  "<br><b>Item: </b>",
                  "<br><b>Пункт: </b>"
                ),
                post_item %>% str_remove_all("[.]"),
                ifelse(
                  filter_language == "eng",
                  "<br><b>Mentions: </b>",
                  "<br><b>Згадки: </b>"
                ),
                post_mention
              )
            )) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(values = palette_color) +
            scale_x_date(date_labels = "%m/%Y") +
            theme(
              legend.position = 'none',
              plot.margin = unit(c(0, -20, -40, -20), "points"),
              panel.spacing = unit(-10, "points"),
              panel.grid.major.y = element_line(
                color = "grey92",
                linewidth = 0.25
              ),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(colour = "white"),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = 'transparent'),
              plot.background = element_rect(fill = 'transparent', color = NA)
            )

          # plot histogram
          hist_plot %>%
            ggplotly(tooltip = HTML("text")) %>%
            config(displayModeBar = FALSE) %>%
            layout(
              hoverlabel = list(
                bgcolor = "white",
                bordercolor = "white",
                font = list(color = "black", family = "Comfortaa", size = 10),
                align = "left"
              ),
              margin = list(
                l = 0,
                r = 0,
                b = 0,
                t = 0,
                pad = 0,
                autoexpand = T
              ),
              xaxis = list(fixedrange = TRUE),
              yaxis = list(fixedrange = TRUE)
            )
        })

        ## PIE ####
        output$firearm_pie <- renderPlotly({
          # create pie in plotly
          pie_submit <- firearm_table_filtered %>%
            group_by(post_item) %>%
            summarize(post_mention = n()) %>%
            ungroup() |>
            mutate(
              post_mention_percent = round(
                post_mention / sum(post_mention) * 100,
                1
              )
            )

          # plot pie
          pie_submit %>%
            mutate(.color = palette_color[post_item]) %>%
            mutate(post_item = post_item %>% str_replace_all("[.]", "")) %>%
            mutate(
              .text = paste0(
                "<b>Mentions:</b> ",
                post_mention,
                "<br>"
              ),
              .text = ifelse(
                nrow(pie_submit) > 1,
                paste0(
                  .text,
                  "<b>Proportion:</b> ",
                  format(post_mention_percent, nsmall = 1, big.mark = ","),
                  "%<br>"
                ),
                .text
              )
            ) %>%
            plot_ly(
              x = ~post_mention,
              y = ~ reorder(post_item, post_mention),
              text = ~.text,
              type = "bar",
              orientation = "h",
              marker = list(color = ~.color),
              textposition = ifelse(
                nrow(pie_submit) == 1,
                "bottom center",
                "none"
              ),
              hovertemplate = ifelse(
                nrow(pie_submit) == 1,
                paste("<extra></extra>"),
                paste("%{text}", "<extra></extra>")
              )
            ) %>%
            config(displayModeBar = FALSE) %>%
            layout(
              font = list(family = "Comfortaa", color = "white"),
              hoverlabel = list(
                bgcolor = "white",
                bordercolor = "white",
                font = list(color = "black", family = "Comfortaa", size = 10),
                align = "left"
              ),
              margin = list(
                l = 0,
                r = 0,
                b = 0,
                t = 0,
                pad = 0,
                autoexpand = TRUE
              ),
              autosize = TRUE,
              plot_bgcolor = 'transparent',
              paper_bgcolor = 'transparent',
              showlegend = FALSE,
              xaxis = list(
                showgrid = FALSE,
                zeroline = FALSE,
                showticklabels = FALSE,
                title = ""
              ),
              yaxis = list(
                showgrid = FALSE,
                zeroline = FALSE,
                showticklabels = TRUE,
                title = ""
              )
            )
        })
        shinyjs::enable("firearm_submit_filter")
        shinyjs::enable("firearm_reset_filter")
        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_item_filter")
        shinyjs::enable("firearm_oblast_filter")
      })
      ## DOWNLOAD SCREENSHOTS ####
      observeEvent(input$download_screenshot, {
        list(
          data.frame(id = "firearm_hist", name = "ukr_media_time"),
          data.frame(id = "firearm_pie", name = "ukr_media_proportion"),
          data.frame(id = "firearm_map", name = "ukr_media_map")
        ) %>%
          walk(function(x) {
            screenshot(id = x$id, filename = x$name, scale = 2)
          })
      })
    }
  )
}
