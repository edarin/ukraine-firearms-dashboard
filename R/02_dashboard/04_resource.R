#### RESOURCE MAIN UI ####
#### RESOURCE SIDE UI ####
resource_side_ui <- function(id) {
  ns <- NS(id)
  resource_type_init <- resource %>% select(resource_type) %>% lapply(str_split, " \\| ") %>%  unlist() %>% unname() %>% sort() %>% unique()
  resource_keyword_init <- resource %>% select(resource_keyword) %>% lapply(str_split, " \\| ") %>%  unlist() %>% unname() %>% sort() %>% unique()
  resource_date_min_init <- resource %>% .$resource_date %>% min() %>% unique()
  resource_date_max_init <- resource %>% .$resource_date %>% max() %>% unique()
  
  tagList(
  h3("Filters"),
  shinyWidgets::pickerInput(inputId=ns("type_filter"),  label="Type",  multiple=T, width = "100%",
                            choices = resource_type_init, 
                            selected= resource_type_init),
  shinyWidgets::pickerInput(inputId=ns("keyword_filter"), label="Keyword",  multiple=T, width = "100%",
                            choices = resource_keyword_init,
                            selected= resource_keyword_init),
  shinyWidgets::airDatepickerInput(inputId=ns("date_filter"), label = "Date", addon="none", clearButton=T, autoClose=T, width="100%",
                                   dateFormat = "MM/yyyy", range = T, view = c("year"), minView=c("months"),
                                   value=c(resource_date_min_init, resource_date_max_init),
                                   minDate=resource_date_min_init, maxDate=resource_date_max_init),
  tags$div(
    actionBttn(inputId = ns("submit_filter"), icon = icon("filter"), label = "Filter", style = "simple",  color = "primary", size="sm", block = FALSE),
    actionBttn(inputId = ns("reset_filter"), icon = icon("refresh"), label = "Reset", style = "simple",  color = "primary", size="sm", block = FALSE),
    class="filter")
)
}

#### RESOURCE MAIN UI ####
resource_main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resource_cards"))
  )
}

#### RESOURCE SERVER ####
resource_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      resource_react <- reactiveVal(data)
      
      observeEvent(input$submit_filter, priority=1, { 
        resource_react(data %>%
          filter(resource_type %in% unlist(input$type_filter)) %>%
          filter(resource_keyword %>% grepl(pattern=paste(unlist(input$keyword_filter), collapse="|"))) %>%
          filter(between(resource_date, as.Date(input$date_filter[1]), as.Date(input$date_filter[2]))) %>%
          tryCatch(error = function(e) data)
          )
      })
        
      observeEvent(input$reset_filter, { 
          resource_react(data)
        })

        observeEvent(list(input$submit_filter, input$reset_filter), priority=0, {
          updatePickerInput(session=session, inputId="type_filter", selected=resource_react() %>%
                            select(resource_type) %>% lapply(str_split, " \\| ") %>%  unlist() %>% unname() %>% sort() %>% unique())
          updatePickerInput(session=session, inputId="keyword_filter", selected=resource_react() %>%
                            select(resource_keyword) %>% lapply(str_split, " \\| ") %>%  unlist() %>% unname() %>% sort() %>% unique())
          updateAirDateInput(session=session, inputId="date_filter", value=c(resource_react() %>% .$resource_date %>% min() %>% unique() %>%  tryCatch(error = function(e) resource_date_min_init),
                                                                             resource_react() %>% .$resource_date %>% max() %>% unique() %>%  tryCatch(error = function(e) resource_date_max_init)))
        })
        
      output$resource_cards <- renderUI({
        args <- resource_react() %>%
          purrr::transpose() %>% 
          map(function(x) card(
            card_header(
              h3(x$resource_title),
              div(x$resource_author, class="aut_res"),
              div(x$resource_keyword %>% str_split_1(" \\| ") %>% str_squish() %>% map(function(x) span(x, class = 'key')), class="key_res")),
            card_body(
              span(x$resource_abstract %>% str_split_1(" \\| ") %>% str_squish() %>% map(function(x) span(x, class = 'abs_res')))
              ),
            card_footer(
              span(a(href=x$resource_url, HTML(paste0(icon("eye")," Open")), target="_blank"), class="open_res")
              ), min_height="500px", max_height="500px", class="resource"
            , full_screen = T) %>% tags$div(class="resource_card")
            )
    args$cellArgs <- list(style = "width: 321px; margin: 5px;")
    do.call(shiny::flowLayout, args)
    })
    })}