#### UKR DASHBOARD ####
#### LOG IN / SIGN IN UI INTERFACE ####
login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # page title
    div(
      "Ukraine" %>% tags$h1(),
      "Firearm Knowledge Portal" %>% tags$h2(),
      class = "login_title"
    ),
    # page panel
    navset_card_tab(
      id = "login_card",
      title = "",
      # log in
      nav_panel(
        "Log in",
        textInput(
          inputId = ns("login_user_email"),
          label = "Email",
          width = "100%"
        ),
        textOutput(ns("login_info")),
        actionBttn(
          inputId = ns("login_button"),
          label = "Enter",
          style = "simple",
          color = "primary",
          size = "sm",
          block = F
        )
      ),
      # sign in
      nav_panel(
        "Sign in",
        textInput(
          inputId = ns("signin_user_email"),
          label = "Email",
          width = "100%"
        ),
        textInput(
          inputId = ns("signin_user_name"),
          label = "Name",
          width = "100%"
        ),
        textInput(
          inputId = ns("signin_user_organisation"),
          label = "Organisation",
          width = "100%"
        ),
        selectInput(
          inputId = ns("signin_user_industry"),
          label = "Industry sector",
          selected = "Other",
          choices = c(
            "Agriculture",
            "Construction",
            "Education",
            "Energy",
            "Finance",
            "Government",
            "Healthcare",
            "IT",
            "Manufacturing",
            "Media",
            "Non-profit",
            "Retail",
            "Services",
            "Transportation",
            "Other"
          ),
          width = "100%"
        ),
        textOutput(ns("signin_info")),
        actionBttn(
          inputId = ns("signin_button"),
          label = "Enter",
          style = "simple",
          color = "primary",
          size = "sm",
          block = F
        )
      ),
    ) %>%
      div(class = "login_pannel")
  )
}
#### LOG IN / SIGN IN SERVER ####
login_server <- function(id, parent_session, data = popup) {
  moduleServer(
    id,
    function(input, output, session) {
      #### log in #####
      # log in initial values

      output$login_info <- renderText({
        login_text()
      })
      login_text <- reactiveVal(
        "Please log in using your email address /n будь ласка, увійдіть, використовуючи свою електронну адресу."
      )

      # action on log in
      observeEvent(input$login_button, {
        Sys.sleep(0.1)
        shinyjs::disable("login_button")
        drive_download(id_users_file, path = users_path, overwrite = T)
        users <- fread(users_path)
        # check credentials
        tryCatch(
          {
            if (input$login_user_email %in% users$email) {
              # show tabs on successful checks
              nav_show(
                id = "dashboard",
                "Overview/Огляд",
                select = T,
                session = parent_session
              )
              nav_show(id = "dashboard", "Database", session = parent_session)
              # nav_show(
              #   id = "dashboard",
              #   "Documentation",
              #   session = parent_session
              # )
              nav_show(id = "dashboard", "About/Про", session = parent_session)
              nav_hide(
                id = "dashboard",
                target = "Authentification/Аутентифікація",
                session = parent_session
              )
              if (
                input$login_user_email == "ukraine.firearms.dashboard@gmail.com"
              ) {
                nav_show(
                  id = "dashboard",
                  target = "Update/Оновлення",
                  session = parent_session
                )
              }
              # show welcome message
              showModal(modalDialog(
                title = "Welcome to the Ukraine Firearm Knowledge Portal/Ласкаво просимо до Порталу знань про вогнепальну зброю України",
                HTML(
                  data %>%
                    unlist() %>%
                    unname() %>%
                    paste(collapse = "<br><br>")
                ),
                easyClose = TRUE,
                footer = tagList(
                  modalButton(
                    "Continue to dashboard/Продовжити до панелі управління"
                  )
                )
              ))
              # restore original values
              updateTextInput(
                "login_user_email",
                value = "",
                session = getDefaultReactiveDomain()
              )
              login_text(
                "Please enter your email./n Будь ласка, введіть свою електронну адресу."
              )
              shinyjs::enable("login_button")

              print(
                "Dashboard accessed by ",
                input$login_email,
                " on ",
                Sys.time()
              )
            } else {
              shinyjs::enable("login_button")
              updateTextInput(
                "login_user_email",
                value = "",
                session = getDefaultReactiveDomain()
              )
              login_text(
                "Please enter a valid email address or sign in./n Будь ласка, введіть дійсну електронну адресу або увійдіть."
              )
            }
          },
          error = function(err) {
            shinyjs::enable("login_button")
            login_text(
              "Please enter a valid email address or sign in./n Будь ласка, введіть дійсну електронну адресу або увійдіть."
            )
          }
        )
      })
      #### sign in #####
      # sign in initial values
      output$signin_info <- renderText({
        signin_text()
      })
      signin_text <- reactiveVal(
        "Please register to access the Ukraine Firearm Knowledge Portal. Your information will be kept confidential./n Будь ласка, зареєструйтесь, щоб отримати доступ до Порталу знань про вогнепальну зброю України. Ваша інформація буде зберігатися в конфіденційності."
      )

      # action on sign in
      observeEvent(input$signin_button, {
        Sys.sleep(0.1)
        shinyjs::disable("signin_button")
        drive_download(id_users_file, path = users_path, overwrite = T)
        users <- fread(users_path)
        # basic check credentials
        if (
          input$signin_user_email %>% nchar() <= 5 |
            str_detect(input$signin_user_email, "@", negate = T)
        ) {
          signin_text(
            "Please enter a valid email address./n Будь ласка, введіть дійсну електронну адресу."
          )
          shinyjs::enable("signin_button")
        } else if (input$signin_user_email %in% users$email) {
          login_text(
            "You are already registered. Please log in with your email address./n Ви вже зареєстровані. Будь ласка, увійдіть за допомогою своєї електронної адреси."
          )
          nav_hide(
            id = "login_card",
            target = "Sign in/Реєстрація",
            session = parent_session
          )
          nav_select(
            id = "login_card",
            selected = "Log in/Увійти",
            session = parent_session
          )
        } else {
          tryCatch(
            {
              login_text(
                "Please now log in with your email address./n Тепер увійдіть за допомогою своєї електронної адреси."
              )
              users <- read_csv(users_path, show_col_types = FALSE) |>
                mutate(date_signed_in = as.Date(date_signed_in))

              # action on credentials checked
              signin_entry <- tibble(
                email = input$signin_user_email,
                institution = input$signin_user_instituion,
                industry = input$signin_user_industry,
                name = input$signin_user_name,
                date_signed_in = Sys.Date()
              )
              users <- bind_rows(users, signin_entry)
              write.csv(
                users,
                file.path('googledrive-temp', 'users.csv'),
                row.names = F,
                quote = F
              )
              drive_update(
                file = as_id(id_users_file),
                media = file.path('googledrive-temp', 'users.csv')
              )
              # action on credentials checked
              rm(signin_entry)
              gc()
              nav_hide(
                id = "login_card",
                target = "Sign in/Реєстрація",
                session = parent_session
              )
              nav_select(
                id = "login_card",
                selected = "Log in/Увійти",
                session = parent_session
              )
              updateTextInput(
                "singin_user_name",
                value = "",
                session = getDefaultReactiveDomain()
              )
              shinyjs::enable("signin_button")
            },
            error = function(err) {
              print(err)
              signin_text(
                "Please enter a valid email address./n Будь ласка, введіть дійсну електронну адресу."
              )
              shinyjs::enable("signin_button")
            }
          )
        }
      })
    }
  )
}

### LOG OUT SERVER ####
# log out
logout_server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      nav_hide(id = "dashboard", target = "Summary", session = parent_session)
      nav_hide(id = "dashboard", target = "Database", session = parent_session)
      nav_hide(
        id = "dashboard",
        target = "Documentation",
        session = parent_session
      )
      nav_hide(id = "dashboard", target = "Log out", session = parent_session)
      nav_show(
        id = "dashboard",
        target = "Log in",
        select = T,
        session = parent_session
      )
      nav_insert(
        id = "login_card",
        nav = nav_panel(
          title = "",
          value = "logged_out",
          "You have successfully logged out."
        ),
        select = T,
        position = "before",
        session = parent_session
      )
      nav_show(id = "dashboard", target = "About", session = parent_session)
      gc()
    }
  )
}
