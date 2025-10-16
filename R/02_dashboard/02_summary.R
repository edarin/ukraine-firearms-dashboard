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
          title = "Updated",
          value = firearms_table |>
            distinct("date_publication") |>
            pull() |>
            collect() %>%
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
          value = paste0(
            firearms_table |>
              distinct("post_url") %>%
              pull() %>%
              collect() %>%
              length() %>%
              format(big.mark = ",", scientific = FALSE),
            " posts"
          ),
          showcase = icon("file"),
          theme = "primary",
          fill = TRUE,
          heigth = "3wv"
        ) %>%
          tags$div(class = "show_card"),
        # number of items
        value_box(
          title = "Mentions",
          value = paste0(
            firearms_table |>
              select(contains("item_list_")) %>%
              separate_longer_delim(item_list_eng, ";") %>%
              drop_na(item_list_eng) %>%
              collect() %>%
              nrow() %>%
              format(big.mark = ",", scientific = FALSE),
            " items"
          ),
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
      choices = firearm_summary_table |>
        distinct(item_list_eng) |>
        collect() %>%
        sort() %>%
        setNames(
          .,
          firearm_summary_table |>
            distinct(item_list_eng) |>
            collect() %>%
            sort() %>%
            str_replace_all("[.]", "")
        ),
      selected = character(0)
    ),
    # oblast
    pickerInput(
      inputId = ns("firearm_oblast_filter"),
      label = "Oblast",
      multiple = T,
      width = "100%",
      choices = firearm_summary_table |>
        distinct(location_oblast_eng) |>
        collect() %>%
        sort() %>%
        setNames(
          .,
          firearm_summary_table |>
            distinct(location_oblast_eng) |>
            collect() %>%
            sort() %>%
            str_replace_all("[.]", "")
        ),
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
      view = c("day"),
      minView = c("days"),
      value = c(
        firearms_table |>
          distinct(date_publication) |>
          collect() %>%
          as.Date() %>%
          min(),
        firearms_table |>
          distinct(date_publication) |>
          collect() %>%
          as.Date() %>%
          max()
      ),
      minDate = firearms_table |>
        distinct(date_publication) |>
        collect() %>%
        as.Date() %>%
        min(),
      maxDate = firearms_table |>
        distinct(date_publication) |>
        collect() %>%
        as.Date() %>%
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
  firearms_table,
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
          choices = firearm_summary_table |>
            select(contains(paste0(
              "item_list_",
              language
            ))) |>
            distinct() %>%
            pull() |>
            collect() |>
            sort() %>%
            setNames(
              .,
              firearm_summary_table |>
                select(contains(paste0(
                  "item_list_",
                  language
                ))) |>
                distinct() %>%
                pull() |>
                collect() |>
                sort() %>%
                str_replace_all("[.]", "")
            ),
          selected = character(0),
          session = session
        )
        updatePickerInput(
          "firearm_oblast_filter",
          choices = firearm_summary_table |>
            select(contains(paste0(
              "location_oblast_",
              language
            ))) |>
            distinct() %>%
            collect() %>%
            sort() %>%
            setNames(
              .,
              firearm_summary_table |>
                select(contains(paste0(
                  "location_oblast_",
                  language
                ))) |>
                distinct() %>%
                collect() |>
                sort() %>%
                str_replace_all("[.]", "")
            ),
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
              select(contains(paste0("item_list_", item_language))) |>
              distinct() |>
              pull() %>%
              sort(),
            '"'
          )
        }

        # get oblasts for items
        item_oblast <- firearm_summary_table |>
          filter(
            !!sym(paste0("item_list_", item_language)) %in%
              input$firearm_item_filter
          ) |>
          select(contains(paste0("location_oblast_", item_language))) |>
          rename_with(~ str_replace(., paste0("_", item_language), "")) |>
          distinct() |>
          pull(location_oblast) |>
          collect() |>
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

        item_date_data <- firearms_table |>
          filter(
            !!sym(paste0("item_list_", item_language)) %in%
              input$firearm_item_filter
          ) |>
          summarize(
            date_min = min(date_publication, na.rm = TRUE),
            date_max = max(date_publication, na.rm = TRUE)
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
                "location_oblast_",
                oblast_language
              ))) |>
              distinct() %>%
              pull() |>
              collect() |>
              sort(),
            '"'
          )
        }
        oblast_item <- firearm_summary_table |>
          filter(
            !!sym(paste0("location_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) |>
          select(contains(paste0("item_list_", oblast_language))) |>
          rename_with(~ str_replace(., paste0("_", oblast_language), "")) |>
          distinct() |>
          pull(item_list) |>
          collect() |>
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

        oblast_date_data <- firearms_table |>
          filter(
            !!sym(paste0("location_oblast_", oblast_language)) %in%
              input$firearm_oblast_filter
          ) |>
          summarize(
            date_min = min(date_publication, na.rm = TRUE),
            date_max = max(date_publication, na.rm = TRUE)
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
          choices = firearm_summary_table |>
            distinct(!!sym(paste0("location_oblast_", reset_language))) |>
            collect() %>%
            sort() %>%
            setNames(
              .,
              firearm_summary_table |>
                distinct(!!sym(paste0("location_oblast_", reset_language))) |>
                collect() %>%
                sort() %>%
                str_replace_all("[.]", "")
            ),
          session = session
        )
        updatePickerInput(
          "firearm_item_filter",
          selected = character(0),
          choices = firearm_summary_table |>
            distinct(!!sym(paste0("item_list_", reset_language))) |>
            collect() %>%
            sort() %>%
            setNames(
              .,
              firearm_summary_table |>
                distinct(!!sym(paste0("item_list_", reset_language))) |>
                collect() %>%
                sort() %>%
                str_replace_all("[.]", "")
            ),
          session = session
        )
        updateAirDateInput(
          "firearm_date_filter",
          value = c(
            firearms_table |>
              distinct(date_publication) |>
              collect() %>%
              as.Date() %>%
              min(),
            firearms_table |>
              distinct(date_publication) |>
              collect() %>%
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
      ### MAP ####
      output$firearm_map <- renderLeaflet({
        #get coords
        map_coords <-
          firearm_summary_table %>%
          collect() %>%
          select(c(ends_with("eng"), "lat", "long")) %>%
          rename_with(~ str_replace(., paste0("_", "eng"), ""))
        # create map in leaflet
        map_init <- firearms_table %>%
          select(date_publication, location_oblast_eng, item_list_eng) %>%
          collect() %>%
          mutate(date_publication = as.Date(date_publication)) %>%
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(item_list, ";") %>%
          separate_longer_delim(location_oblast, ";") %>%
          drop_na(item_list) %>%
          drop_na(location_oblast) %>%
          filter(!is.na(date_publication)) %>%
          group_by(location_oblast, item_list) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          left_join(map_coords, by = join_by(location_oblast, item_list)) %>%
          drop_na(lat) %>%
          sf::st_as_sf(coords = c("long", "lat"), remove = F) %>%
          mutate(col = item_list %>% str_replace_all(fixed(palette_color))) %>%
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
            radius = ~post_mention,
            label = ~ paste0(
              "<b>Oblast: </b>",
              location_oblast %>% str_remove_all("[.]"),
              "<br><b>Item: </b>",
              item_list %>% str_remove_all("[.]"),
              "<br><b>Mentions: </b>",
              post_mention
            ) %>%
              lapply(htmltools::HTML),
            stroke = T,
            color = ~ palette_factor(item_list),
            weight = 3,
            opacity = 1,
            fill = T,
            fillColor = ~ palette_factor(item_list),
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
          firearms_table %>%
          select(date_publication, location_oblast_eng, item_list_eng) |>
          collect() %>%
          mutate(date_publication = date_publication %>% as.Date()) %>%
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(item_list, ";") %>%
          separate_longer_delim(location_oblast, ";") %>%
          drop_na(item_list) %>%
          drop_na(location_oblast) %>%
          filter(!is.na(date_publication)) %>%
          group_by(date_publication, item_list) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          filter(post_mention > 0)

        hist_plot <-
          hist_init %>%
          ggplot(aes(
            x = date_publication,
            y = post_mention,
            fill = reorder(item_list, post_mention),
            text = paste0(
              "<b>Date:</b> ",
              date_publication %>% format("%d/%m/%Y"),
              "<br><b>Item:</b> ",
              item_list %>% str_remove_all("[.]"),
              "<br><b>Mentions:</b> ",
              post_mention
            )
          )) +
          geom_bar(stat = 'identity') +
          scale_x_date(date_labels = "%d/%m/%Y") +
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
        pie_init <- firearms_table %>%
          select(item_list_eng, location_oblast_eng) |>
          collect() |>
          rename_with(~ str_replace(., paste0("_", "eng"), "")) %>%
          separate_longer_delim(item_list, ";") %>%
          separate_longer_delim(location_oblast, ";") %>%
          drop_na(item_list) %>%
          group_by(item_list) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          mutate(
            post_mention_total = sum(post_mention),
            post_mention_percent = MESS::round_percent(
              post_mention / post_mention_total * 100,
              decimals = 1
            )
          ) %>%
          arrange(post_mention_percent)

        pie_pal <- palette_color[order(factor(
          names(palette_color),
          levels = pie_init$item_list
        ))]

        # plot pie
        pie_init %>%
          plot_ly(
            labels = ~ item_list %>% str_remove_all("[.]"),
            values = ~post_mention
          ) %>%
          add_pie(
            hole = 0.6,
            textposition = "none",
            # outsidetextfont = list(color = '#FFFFFF'),
            textinfo = "label",
            text = ~ paste0(
              "<b>Proportion:</b> ",
              format(post_mention_percent, nsmall = 1, big.mark = ","),
              "%<br>",
              "<b>Item:</b> ",
              item_list %>% str_remove_all("[.]"),
              "<br>",
              "<b>Mentions:</b> ",
              post_mention
            ),
            hovertemplate = paste("%{text}", "<extra></extra>"),
            marker = list(
              colors = pie_pal[pie_pal %>% names() %in% pie_init$item_list]
            )
          ) %>%
          config(displayModeBar = FALSE) %>%
          layout(
            hoverlabel = list(
              bgcolor = "white",
              bordercolor = "white",
              font = list(color = "black", family = "Comfortaa", size = 10),
              align = "left"
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = T),
            autosize = T,
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent',
            showlegend = FALSE,
            xaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
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
          distinct(!!sym(paste0("location_oblast_", filter_language))) |>
          pull() |>
          collect() |>
          sort()
        if (input$firearm_oblast_filter %>% length() > 0) {
          filter_oblast <- input$firearm_oblast_filter
        }

        filter_item <- firearm_summary_table |>
          distinct(!!sym(paste0("item_list_", filter_language))) |>
          pull() |>
          collect() |>
          sort()
        if (input$firearm_item_filter %>% length() > 0) {
          filter_item <- input$firearm_item_filter
        }

        filter_date_min <- input$firearm_date_filter[1]
        if (is.null(input$firearm_date_filter[1])) {
          filter_date_min <- firearms_table |>
            distinct(date_publication) |>
            collect() |>
            as.Date() |>
            min()
        }

        filter_date_max <- input$firearm_date_filter[2]
        if (is.null(input$firearm_date_filter[2])) {
          filter_date_max <- firearms_table |>
            distinct(date_publication) |>
            collect() |>
            pull(date_publication) |>
            as.Date() |>
            max()
        }
        if (is.na(input$firearm_date_filter[2])) {
          filter_date_max <- filter_date_min
        }

        ### MAP ####
        map_coords <-
          firearm_summary_table %>%
          select(c(ends_with(language_react()), "lat", "long")) %>%
          rename_with(~ str_replace(., paste0("_", filter_language), "")) |>
          collect()

        map_submit <- firearms_table %>%
          select(
            date_publication,
            !!sym(paste0("location_oblast_", filter_language)),
            !!sym(paste0("item_list_", filter_language))
          ) %>%
          collect() |>
          mutate(date_publication = date_publication %>% as.Date()) %>%
          rename_with(~ str_replace(., paste0("_", filter_language), "")) %>%
          separate_longer_delim(item_list, ";") %>%
          separate_longer_delim(location_oblast, ";") %>%
          drop_na(item_list) %>%
          drop_na(location_oblast) %>%
          filter(location_oblast %in% filter_oblast) %>%
          filter(item_list %in% filter_item) %>%
          filter(
            date_publication >= filter_date_min &
              date_publication <= filter_date_max
          ) %>%
          group_by(location_oblast, item_list) %>%
          summarize(post_mention = n()) %>%
          ungroup() %>%
          left_join(map_coords, by = join_by(location_oblast, item_list)) %>%
          drop_na(lat) %>%
          sf::st_as_sf(coords = c("long", "lat"), remove = F) %>%
          mutate(col = item_list %>% str_replace_all(fixed(palette_color))) %>%
          arrange(desc(post_mention), col)

        leafletProxy("firearm_map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addCircleMarkers(
            data = map_submit,
            group = "firearm_markers",
            radius = ~post_mention,
            label = ~ paste0(
              ifelse(
                filter_language == "eng",
                "<b>Oblast: </b>",
                "<b>Область: </b>"
              ),
              location_oblast %>% str_remove_all("[.]"),
              ifelse(
                filter_language == "eng",
                "<br><b>Item: </b>",
                "<br><b>Пункт: </b>"
              ),
              item_list %>% str_remove_all("[.]"),
              ifelse(
                filter_language == "eng",
                "<br><b>Mentions: </b>",
                "<br><b>Згадки: </b>"
              ),
              post_mention
            ) %>%
              lapply(htmltools::HTML),
            stroke = T,
            color = ~ palette_factor(item_list),
            weight = 3,
            opacity = 1,
            fill = T,
            fillColor = ~ palette_factor(item_list),
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
          hist_submit <-
            firearms_table %>%
            select(
              date_publication,
              !!sym(paste0("location_oblast_", filter_language)),
              !!sym(paste0("item_list_", filter_language))
            ) %>%
            collect() %>%
            mutate(date_publication = date_publication %>% as.Date()) %>%
            rename_with(~ str_replace(., paste0("_", filter_language), "")) %>%
            separate_longer_delim(item_list, ";") %>%
            separate_longer_delim(location_oblast, ";") %>%
            drop_na(item_list) %>%
            drop_na(location_oblast) %>%
            filter(location_oblast %in% filter_oblast) %>%
            filter(item_list %in% filter_item) %>%
            filter(
              date_publication >= filter_date_min &
                date_publication <= filter_date_max
            ) %>%
            filter(!is.na(date_publication)) %>%
            group_by(date_publication, item_list) %>%
            summarize(post_mention = n()) %>%
            ungroup() %>%
            filter(post_mention > 0)

          hist_plot <-
            hist_submit %>%
            ggplot(aes(
              x = date_publication,
              y = post_mention,
              fill = reorder(item_list, post_mention),
              #fill=post_item,
              text = paste0(
                ifelse(
                  filter_language == "eng",
                  "<b>Date: </b>",
                  "<b>Дата: </b>"
                ),
                date_publication %>% format("%d/%m/%Y"),
                ifelse(
                  filter_language == "eng",
                  "<br><b>Item: </b>",
                  "<br><b>Пункт: </b>"
                ),
                item_list %>% str_remove_all("[.]"),
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
            scale_x_date(date_labels = "%d/%m/%Y") +
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
          pie_submit <- firearms_table %>%
            select(
              date_publication,
              !!sym(paste0("location_oblast_", filter_language)),
              !!sym(paste0("item_list_", filter_language))
            ) %>%
            collect() |>
            mutate(date_publication = date_publication %>% as.Date()) %>%
            rename_with(~ str_replace(., paste0("_", filter_language), "")) %>%
            separate_longer_delim(item_list, ";") %>%
            separate_longer_delim(location_oblast, ";") %>%
            filter(location_oblast %in% filter_oblast) %>%
            filter(item_list %in% filter_item) %>%
            filter(
              date_publication >= filter_date_min &
                date_publication <= filter_date_max
            ) %>%
            drop_na(item_list) %>%
            drop_na(location_oblast) %>%
            filter(!is.na(date_publication)) %>%
            group_by(item_list) %>%
            summarize(post_mention = n()) %>%
            ungroup() %>%
            mutate(
              post_mention_total = sum(post_mention),
              post_mention_percent = MESS::round_percent(
                post_mention / post_mention_total * 100,
                decimals = 1
              )
            ) %>%
            arrange(post_mention_percent)

          pie_pal <- palette_color[order(factor(
            names(palette_color),
            levels = pie_submit$item_list
          ))]

          # plot pie
          pie_submit %>%
            plot_ly(
              labels = ~ item_list %>% str_remove_all("[.]"),
              values = ~post_mention
            ) %>%
            add_pie(
              hole = 0.6,
              textposition = "none",
              #  outsidetextfont = list(color = '#FFFFFF'),
              textinfo = "label",
              text = ~ paste0(
                ifelse(
                  filter_language == "eng",
                  "<b>Proportion:</b> ",
                  "<b>Пропорція:</b> "
                ),
                format(post_mention_percent, nsmall = 1, big.mark = ","),
                "%",
                ifelse(
                  filter_language == "eng",
                  "<br><b>Item: </b>",
                  "<br><b>Пункт: </b>"
                ),
                item_list %>% str_remove_all("[.]"),
                ifelse(
                  filter_language == "eng",
                  "<br><b>Mentions: </b>",
                  "<br><b>Згадки: </b>"
                ),
                post_mention
              ),
              hovertemplate = paste("%{text}", "<extra></extra>"),
              marker = list(
                colors = pie_pal[pie_pal %>% names() %in% pie_submit$item_list]
              )
            ) %>%
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
              autosize = T,
              plot_bgcolor = 'transparent',
              paper_bgcolor = 'transparent',
              showlegend = FALSE,
              xaxis = list(
                showgrid = FALSE,
                zeroline = FALSE,
                showticklabels = FALSE
              ),
              yaxis = list(
                showgrid = FALSE,
                zeroline = FALSE,
                showticklabels = FALSE
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
