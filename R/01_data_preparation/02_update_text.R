library(googledrive)
library(data.table)
library(dplyr)
library(stringr)

if (grepl('Users/EdithD/Documents/git/ukraine-firearms-dashboard', getwd())) {
  library(dotenv)
  dotenv::load_dot_env(file = '.env')
}

GOOGLE_TOKEN <- Sys.getenv('GOOGLE_TOKEN')
# authenticate googledrive
drive_auth(path = GOOGLE_TOKEN, scopes = 'drive')

# load resources
resource_name <- "resource_list.csv"
resource_path <- file.path('data/text/', resource_name)
drive_download(resource_name, path = resource_path, overwrite = T)

resource <- fread(resource_path) %>%
  mutate(resource_date = as.Date(resource_date %>% paste0("-01"))) %>%
  arrange(desc(resource_date))

write.csv(
  resource,
  file.path('data/text/', 'resource_list.csv'),
  row.names = F,
  quote = F
)

# update about
about_name <- "about.csv"
about_path <- file.path('data/text', about_name)
drive_download(about_name, path = about_path, overwrite = T)
about <- fread(about_path, quote = "", fill = TRUE) %>%
  mutate(
    about_content = str_replace_all(about_content, '["]', ""),
    about_content = str_squish(about_content)
  )
write.csv(
  about,
  file.path('data/text/', 'about.csv'),
  row.names = F,
  quote = F
)

# load information
popup_name <- "popup_start.csv"
popup_path <- file.path('data/text/', popup_name)
drive_download(popup_name, path = popup_path, overwrite = T)
popup <- fread(popup_path, quote = "", fill = TRUE) %>%
  mutate(
    popup_content = str_replace_all(popup_content, '["]', ""),
    popup_content = str_squish(popup_content)
  )
write.csv(
  popup,
  file.path('data/text/', 'popup_start.csv'),
  row.names = F,
  quote = F
)
