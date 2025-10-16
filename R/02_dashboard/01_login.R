#### UKR DASHBOARD ####
#### LOG IN / SIGN IN UI INTERFACE ####
login_ui <- function(id) {
  ns <- NS(id)
    tagList(
      # page title 
      div("Ukraine" %>% tags$h1(), "Firearm Knowledge Portal" %>% tags$h2(), class="login_title"),
      # page panel 
      navset_card_tab(id="login_card", title = "", 
      # log in                
        nav_panel("Log in",
                  textInput(inputId=ns("login_user_name"), label="Email", width="100%"),
                  passwordInput(inputId=ns("login_user_password"), label="Password", width="100%"),
                  textOutput(ns("login_info")),
                  actionBttn(inputId = ns("login_button"), label = "Enter", style = "simple",  color = "primary", size="sm", block=F)),
      # sign in                
        nav_panel("Sign in",
                  textInput(inputId=ns("signin_user_name"), label="Email", width="100%"),
                  passwordInput(inputId=ns("signin_user_password"), label="Password", width="100%"),
                  textOutput(ns("signin_info")),
                  actionBttn(inputId = ns("signin_button"), label = "Enter", style = "simple",  color = "primary", size="sm", block=F)),
        ) %>% div(class="login_pannel"))
}
#### LOG IN / SIGN IN SERVER ####
login_server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      #### log in #####
      # log in initial values
      credential_env <- mongo(collection="ukr_credentials", 
                              db="ukr_dashboard_prod", 
                              url = "mongodb+srv://sasuser:d4fZf188N1ubQrcc@sas-cluster.rhgpxth.mongodb.net/?retryWrites=true&w=majority")
      output$login_info <- renderText({login_text()})
      login_text <- reactiveVal("Please log in using your email address and password.")
      
      # action on log in
      observeEvent(input$login_button, {
        Sys.sleep(0.5)
        shinyjs::disable("login_button")
        # check credentials
        tryCatch({
        if(sodium::password_verify(hash=credential_env$find() %>% 
                                   filter(user_access=="granted") %>% 
                                   filter(user_name==isolate(input$login_user_name)) %>% .$user_password, 
                                  password=isolate(input$login_user_password))){
        # show tabs on successful checks
                nav_show(id = "dashboard", "Summary", select = T, session = parent_session)
                nav_show(id = "dashboard", "Sources", session = parent_session)
                nav_show(id = "dashboard", "Documentation", session = parent_session)
                nav_show(id = "dashboard", "Log out", session = parent_session)
                nav_hide(id="dashboard", target="Log in", session = parent_session)
        # restore original values
                updateTextInput("login_user_name", value="", session=getDefaultReactiveDomain())
                updateTextInput("login_user_password", value="", session=getDefaultReactiveDomain())
                login_text("Please enter your user name and password.")
                shinyjs::enable("login_button")
                print("Dashboard accessed by ", input$login_user_name, " on ", Sys.time())
        } else{
          shinyjs::enable("login_button")
          updateTextInput("login_user_password", value="", session=getDefaultReactiveDomain())
          login_text("Please enter a valid email address or password.")  
              }
        }, error=function(err) {
          shinyjs::enable("login_button")
          updateTextInput("login_user_password", value="", session=getDefaultReactiveDomain())
          login_text("Please enter a valid email address or password.")
        })
        })
      #### sign in #####
      # sign in initial values
      output$signin_info <- renderText({signin_text()})
      signin_text <- reactiveVal("Please sign in using your email address and password.")
      
      # action on sign in
      observeEvent(input$signin_button, {
        Sys.sleep(0.5)
        shinyjs::disable("signin_button")
      # basic check credentials
        if(input$signin_user_name %>% nchar() <=5 | 
           str_detect(input$signin_user_name, "@", negate=T) |
           str_detect(input$signin_user_name, ".", negate=T)){
          signin_text("Please enter a valid email address.")
          shinyjs::enable("signin_button")
          } else if (input$signin_user_password %>% nchar() <=5){
            updateTextInput("singin_user_password", value="", session=getDefaultReactiveDomain())
            signin_text("Please enter a password of at least six characters.")
            shinyjs::enable("signin_button")
            } else {
              tryCatch({
                # send email confirmation
                gm_mime() %>%  
                  gm_from("data@smallarmssurvey.org") %>% gm_to(input$signin_user_name) %>% 
                  gm_bcc("data@smallarmssurvey.org") %>% gm_subject("Access Request") %>%
                  gm_html_body("Dear user,<br><br>
                  Thank you for submitting your access request to the <a href='https://smallarmssurvey.shinyapps.io/ukr_dashboard'>Ukraine Firearms Knowledge Portal</a>.<br><br>
                  Your request is currently under review, and we anticipate completing the process within the next 48 hours. 
                  We understand the importance of timely access and will do our best to expedite the approval.
                  If further information is required or if there are any updates on the status of your request, we will reach out to you promptly.<br><br>
                  We appreciate your patience and look forward to welcoming you to the Ukraine Firearms Knowledge Portal.<br><br>
                  Best regards,<br><br>
                  data@smallarmssurvey.org") %>% 
                  gm_send_message()
                login_text("Please check your email for additional information.")
        
                # action on credentials checked
                signin_entry <- c(paste0('{"user_name" : "', input$signin_user_name, '", ',
                                         '"user_password" : "', sodium::password_store(input$signin_user_password), '", ',
                                         '"group" : "user", ', '"user_access" : "pending"}'))
                credential_env$insert(signin_entry)
                # action on credentials checked
                rm(signin_entry); gc()
                nav_hide(id="login_card", target="Sign in", session=parent_session)
                nav_select(id="login_card", selected="Log in", session=parent_session)
                updateTextInput("singin_user_name", value="", session=getDefaultReactiveDomain())
                updateTextInput("singin_user_password", value="", session=getDefaultReactiveDomain())
                shinyjs::enable("signin_button")
              },
                error=function(err) {
                  print(err)
                  updateTextInput("singin_user_password", value="", session=getDefaultReactiveDomain())
                  signin_text("Please enter a valid email address.")
                  shinyjs::enable("signin_button")
                })
            }
        })
      })
}

### LOG OUT SERVER ####
# log out
logout_server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      nav_hide(id="dashboard", target="Summary", session = parent_session)
      nav_hide(id="dashboard", target="Sources", session = parent_session)
      nav_hide(id="dashboard", target="Documentation", session = parent_session)
      nav_hide(id="dashboard", target="Log out", session = parent_session)
      nav_show(id="dashboard", target="Log in", select=T, session = parent_session)
      nav_insert(id="login_card", nav=nav_panel(title="", value="logged_out", "You have successfully logged out."), select=T, position="before", session = parent_session)
      nav_show(id="dashboard", target="About", session = parent_session)
      gc()
    }
)}



