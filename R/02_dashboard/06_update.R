###
run_workflow <- function(repo, workflow) {
  gh_pat <- Sys.getenv("GITHUB_TOKEN")
  branch <- "main"
  owner <- owner
  url <- paste0(
    "https://api.github.com/repos/",
    owner,
    "/",
    repo,
    "/actions/workflows/",
    workflow,
    "/dispatches"
  )

  body <- toJSON(
    list(
      ref = branch
    ),
    auto_unbox = TRUE
  )

  res <- POST(
    url,
    add_headers(
      Authorization = paste("Bearer", gh_pat),
      Accept = "application/vnd.github.v3+json"
    ),
    body = body,
    encode = "json"
  )

  status_txt <- if (status_code(res) == 204) {
    paste0(
      "GitHub workflow dispatched successfully! (",
      workflow,
      ") for branch ",
      branch,
      ")"
    )
  } else {
    paste0(
      "Error (",
      status_code(res),
      "): ",
      content(res, "text", encoding = "UTF-8")
    )
  }
  print(status_txt)
}


update_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      full_screen = T,
      card_header("Update text"),
      card_body(
        markdown(
          "Use the buttons below to update the dashboard text content or to trigger a full dashboard update."
        ),
        actionButton(ns("text_update"), "Update text")
      )
    ),
    card_spacer(),
    card(
      full_screen = T,
      card_header("Data update"),
      card_body(
        markdown(
          "This will update the dashboard data. The data needs to be stored in the google drive under `/data/Official withdrawal_Month_YY`. It needs to contains the xlsx file and a folder called `main cases` with the screenshots. This process can take up to 10 minutes. Once the update is complete, please refresh the page to see the changes."
        ),
        actionButton(ns("data_update"), "Update data"
      )
    ),
    card(
      full_screen = T,
      title = "Update dashboard",
      actionButton(ns("app_update"), "Update dashboard")
    )
  )
}

#### UPDATE SERVER ####
update_server <- function(id, parent_session = NULL) {
  moduleServer(
    id,
    function(input, output, parent_session) {
      observeEvent(input$text_update, {
        showNotification("Text updating…", type = "message")
      })

      observeEvent(input$data_update, {
        run_workflow(
          repo = "ukraine-firearms-dashboard",
          workflow = "update_censs_content.yml"
        )
        withProgress(
          message = 'Data update in progress',
          detail = 'This will take approximatively 10min',
          value = 0,
          {
            n <- 77L
            for (i in seq_len(n)) {
              incProgress(1 / n)
              Sys.sleep(10)
            }
          }
        )
        showNotification(
          'Data update done. Please refresh the page',
          type = "message"
        )
      })
    }
  )
}
