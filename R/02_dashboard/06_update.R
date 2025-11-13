###
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
        # load resources
        resource_name <- "resource_list.csv"
        resource_path <- file.path('googledrive-temp', resource_name)
        drive_download(resource_name, path = resource_path, overwrite = T)
        resource <- fread(resource_path) %>%
          mutate(resource_date = as.Date(resource_date %>% paste0("-01"))) %>%
          arrange(desc(resource_date))

        # load about
        about_name <- "about.csv"
        about_path <- file.path('googledrive-temp', about_name)
        drive_download(about_name, path = about_path, overwrite = T)
        about <- fread(about_path, quote = "", fill = TRUE) %>%
          mutate(
            about_content = str_replace_all(about_content, '["]', ""),
            about_content = str_squish(about_content)
          )

        # load information
        popup_name <- "popup_start.csv"
        popup_path <- file.path('googledrive-temp', popup_name)
        drive_download(popup_name, path = popup_path, overwrite = T)
        popup <- fread(popup_path, quote = "", fill = TRUE) %>%
          mutate(
            popup_content = str_replace_all(popup_content, '["]', ""),
            popup_content = str_squish(popup_content)
          )

        showNotification("Text updating…", type = "message")
      })

      observeEvent(input$app_update, {
        if (
          grepl(
            'Users/EdithD/Documents/git/ukraine-firearms-dashboard',
            getwd()
          )
        ) {
          dotenv::load_dot_env(file = '.env')
        }
        owner <- "ukraine-firearms-dashboard"
        gh_pat <- Sys.getenv("PAT_GITHUB")
        repo <- "ukraine-firearms-dashboard"
        workflow <- 206374641 #"deploy.yml" # filename in .github/workflows/
        branch <- "main" # branch or tag to run against

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
          "Deploy workflow dispatched successfully!"
        } else {
          paste0(
            "Error (",
            status_code(res),
            "): ",
            content(res, "text", encoding = "UTF-8")
          )
        }
        print(status_txt)
        withProgress(
          message = 'Dashboard update in progress',
          detail = 'This will take approximatively 12min',
          value = 0,
          {
            n <- 75L
            for (i in seq_len(n)) {
              incProgress(1 / n)
              Sys.sleep(10)
            }
          }
        )
        showNotification(
          'Dashboard update done. Please refresh the page',
          type = "message"
        )
      })
    }
  )
}
