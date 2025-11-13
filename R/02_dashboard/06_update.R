###
run_workflow <- function(repo, workflow) {
  gh_pat <- Sys.getenv("GITHUB_TOKEN")
  branch <- "main"
  owner <- "ukraine-firearms-dashboard"
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
      card_header("Text update"),
      card_body(
        markdown(
          "Use the button below to update the dashboard text content from the Google drive. This includes the resources, about and popup content. <br> This process can take up to 5 minutes. Once the update is complete, please refresh the page to see the changes."
        ),
        actionButton(ns("text_update"), "Update text")
      )
    ),
    card(
      full_screen = T,
      card_header("Data update"),
      card_body(
        markdown(
          "Use the button below to update the dashboard data. The data needs to be stored in `ukraine.firearms.dashboard` Google drive in the folder `/data/Official withdrawal_Month_YY`. The folder needs to contains the `Official withdrawal_Month_YY.xlsx` file and a folder called `main cases` with the screenshots. <br> This process can take up to 10 minutes. Once the update is complete, please refresh the page to see the changes."
        ),
        actionButton(ns("data_update"), "Update data")
      )
    )
  )
}

#### UPDATE SERVER ####
update_server <- function(id, parent_session = NULL) {
  moduleServer(
    id,
    function(input, output, parent_session) {
      observeEvent(input$text_update, {
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
