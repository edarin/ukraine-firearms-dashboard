library(rsconnect)

if (grepl('Users/EdithD/Documents/git/ukraine-firearms-dashboard', getwd())) {
  library(dotenv)
  dotenv::load_dot_env(file = '.env')
  SHINYAPP_TOKEN <- Sys.getenv('SHINYAPP_TOKEN')
  SHINYAPP_SECRET <- Sys.getenv('SHINYAPP_SECRET')
}

rsconnect::setAccountInfo(
  name = 'ukraine-firearms-dashboard',
  token = SHINYAPP_TOKEN,
  secret = SHINYAPP_SECRET
)

rsconnect::deployApp(
  '.',
  logLevel = 'verbose',
  appName = 'ukraine-firearms-dashboard',
  envManagement = T
)
