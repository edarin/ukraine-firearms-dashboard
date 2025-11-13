#### LOAD LIBRARY ####
library(tidyverse)
library(readxl)
library(janitor)
library(tidygeocoder)
library(googledrive)
library(here)
library(duckdb)
library(polyglotr)

if (grepl('Users/EdithD/Documents/git/ukraine-firearms-dashboard', getwd())) {
  dotenv::load_dot_env(file = '.env')
}

GOOGLE_TOKEN <- Sys.getenv('GOOGLE_TOKEN')
# authenticate googledrive
drive_auth(path = GOOGLE_TOKEN, scopes = 'drive')

db_path <- file.path(
  here::here("data"),
  'database',
  "ukr_firearms_dashboard_test.duckdb"
)

# Prepare processing ----
censs_files <- drive_find(type = 'xlsx') |>
  pull(name)

# get already processed files
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
censs_files_names_old <- dbGetQuery(
  con,
  "SELECT file_name FROM processed_censs_files"
) %>%
  pull(file_name)
dbDisconnect(con, shutdown = TRUE)

files_to_process <- censs_files[
  !match(censs_files, censs_files_names_old, nomatch = 0)
]


## Map of oblast names ------
ukr_eng_map <- tibble(
  post_oblast_ukr = c(
    "Вінницька",
    "Волинська",
    "Дніпропетровська",
    "Донецька",
    "Житомирська",
    "Закарпатська",
    "Запорізька",
    "Івано-Франківська",
    "Київська",
    "Кіровоградська",
    "Луганська",
    "Львівська",
    "Миколаївська",
    "Одеська",
    "Полтавська",
    "Рівненська",
    "Сумська",
    "Тернопільська",
    "Харківська",
    "Херсонська",
    "Хмельницька",
    "Черкаська",
    "Чернівецька",
    "Чернігівська",
    "Київ",
    "невідомо"
  ),
  post_oblast_eng = c(
    "Vinnytsia Oblast",
    "Volyn Oblast",
    "Dnipropetrovsk Oblast",
    "Donetsk Oblast",
    "Zhytomyr Oblast",
    "Zakarpattia Oblast",
    "Zaporizhia Oblast",
    "Ivano-Frankivsk Oblast",
    "Kyiv Oblast",
    "Kirovohrad Oblast",
    "Luhansk Oblast",
    "Lviv Oblast",
    "Mykolaiv Oblast",
    "Odesa Oblast",
    "Poltava Oblast",
    "Rivne Oblast",
    "Sumy Oblast",
    "Ternopil Oblast",
    "Kharkiv Oblast",
    "Kherson Oblast",
    "Khmelnytskyi Oblast",
    "Cherkasy Oblast",
    "Chernivtsi Oblast",
    "Chernihiv Oblast",
    "Kyiv City",
    "Unknown"
  )
)


# Process new files ----

if (length(files_to_process) > 0) {
  #plan(multisession, workers = 4)
  geocode_map <- ukr_eng_map %>%
    filter(post_oblast_eng != "Unknown") %>%
    mutate(
      geocode_oblast = paste0(post_oblast_eng, ", Ukraine")
    ) %>%
    geocode(
      geocode_oblast,
      method = 'osm',
      lat = latitude,
      long = longitude
    ) %>%
    select(
      post_oblast_eng,
      post_oblast_latitude = latitude,
      post_oblast_longitude = longitude
    )

  lapply(files_to_process[1], function(df_name) {
    #df_name <- files_to_process[1]
    print(df_name)

    ## Download file from Google Drive -----
    df_path <- here::here('data/database', df_name)
    drive_download(df_name, path = df_path, overwrite = T)
    df <- bind_rows(
      df_path %>%
        # read file
        read_excel(sheet = 'main cases') |>
        mutate(post_source = 'main cases')
      # ,
      # df_path %>%
      #   read_excel(sheet = 'additional cases') |>
      #   mutate(post_source = 'additional cases')
    ) %>%
      # clean names
      clean_names()
    df_cols <- colnames(df)
    if (
      !"qualification_according_to_the_criminal_code_of_ukraine" %in% df_cols
    ) {
      df <- df %>%
        mutate(qualification_according_to_the_criminal_code_of_ukraine = NA)
    }
    df <- df %>%
      # rename columns
      rename(c(
        post_id = no,
        post_date = date_of_publication,
        post_link = link,
        post_author_ukr = author,
        post_title_ukr = title,
        post_content_ukr = content,
        post_oblast_ukr = oblast,
        post_settlement_ukr = settlement_place,
        post_criminal_code_ukr = qualification_according_to_the_criminal_code_of_ukraine
      )) %>%
      # recompute columns
      mutate(
        mention_atgm = rowSums(select(., starts_with("atgm")), na.rm = T),
        mention_javelin = rowSums(
          select(., starts_with("javelin_")),
          na.rm = T
        ),
        mention_grenade = rowSums(
          select(., starts_with("grenade_g")),
          na.rm = T
        ),
        mention_grenade_launcher = rowSums(
          select(., starts_with("grenade_launcher")),
          na.rm = T
        ),
        mention_m2 = rowSums(select(., starts_with("m2_")), na.rm = T),
        mention_m72 = rowSums(select(., starts_with("m72_")), na.rm = T),
        mention_machine_gun = rowSums(
          select(., starts_with("machine_gun_")),
          na.rm = T
        ),
        mention_matador = rowSums(
          select(., starts_with("matador_")),
          na.rm = T
        ),
        mention_manpads = rowSums(
          select(., starts_with("manpads_")),
          na.rm = T
        ),
        mention_mortar = rowSums(
          select(., starts_with("mortar_")),
          na.rm = T
        ),
        mention_rifle = rowSums(select(., starts_with("rifle_g")), na.rm = T),
        mention_rifle_assault = rowSums(
          select(., starts_with("assault_rifle_")),
          na.rm = T
        ),
        mention_stinger = rowSums(
          select(., starts_with("stinger_")),
          na.rm = T
        ),
        mention_trophy = rowSums(
          select(., starts_with("trophy_")),
          na.rm = T
        ),
        mention_none = 1
      ) %>%
      # select posts and mentions
      select(c(starts_with("post_"), starts_with("mention_"))) %>%
      # extract mentions only
      pivot_longer(
        starts_with("mention_"),
        names_to = "item_eng",
        values_to = "item_mention"
      ) %>%
      filter(item_mention > 0) %>%
      mutate(
        item_eng = item_eng %>%
          str_remove_all("mention_") %>%
          str_replace_all("_", " ") %>%
          str_to_title(),
        item_ukr = case_when(
          item_eng == "Grenade" ~ "Гранат",
          item_eng == "Grenade Launcher" ~ "Гранатомет",
          item_eng == "Machine Gun" ~ "Кулемет",
          item_eng == "Rifle" ~ "Гвинтівк",
          item_eng == "Rifle Assault" ~ "Автомат",
          item_eng == "Mortar" ~ "Міномет",
          item_eng == "Trophy" ~ "трофе",
          item_eng == "Atgm" ~ "ПТРК",
          item_eng == "Javelin" ~ "Джавелін",
          item_eng == "Matador" ~ "Матадор",
          item_eng == "Manpads" ~ "ПЗРК",
          item_eng == "None" ~ "Hi",
          TRUE ~ item_eng
        )
      ) %>%
      group_by(across(starts_with("post_"))) %>%
      summarize(
        post_item_eng = item_eng %>%
          paste(collapse = "; ") %>%
          str_replace_all("; None", ""),
        post_item_ukr = item_ukr %>%
          paste(collapse = "; ") %>%
          str_replace_all("; Hi", ""),
        .groups = "drop"
      )

    # Translate ----
    df <- df %>%
      rowwise() |>
      mutate(
        post_author_eng = ifelse(
          !is.na(post_author_ukr),
          suppressWarnings(
            tryCatch(
              expr = google_translate(
                post_author_ukr,
                target = "en",
                source = "uk"
              ),
              error = function(e) NA
            )
          ),
          NA
        ),
        .after = post_author_ukr
      ) %>%
      mutate(
        post_title_eng = ifelse(
          !is.na(post_title_ukr),
          tryCatch(
            expr = google_translate(
              post_title_ukr,
              target = "en",
              source = "uk"
            ),
            error = function(e) NA
          ),
          NA
        ),
        .after = post_title_ukr
      ) %>%
      mutate(
        post_content_eng = ifelse(
          !is.na(post_content_ukr),
          tryCatch(
            google_translate(
              post_content_ukr |> str_sub(1, 3000),
              target = "en",
              source = "uk"
            ),
            error = function(e) NA
          ),
          NA
        ),
        .after = post_content_ukr
      )

    # Map oblast names ----
    df <- df |>
      mutate(
        post_oblast_ukr = ifelse(
          is.na(post_oblast_ukr),
          "невідомо",
          post_oblast_ukr
        ),
        post_oblast_ukr = post_oblast_ukr %>% str_replace_all(", ", "; "),
        post_oblast_list = str_split(post_oblast_ukr, "\\s*(;)\\s*"),
        post_oblast_eng = list(purrr::map_chr(
          post_oblast_list,
          ~ {
            engs <- ukr_eng_map %>%
              filter(post_oblast_ukr %in% .x) %>%
              pull(post_oblast_eng)
            if (length(engs) == 0) {
              engs <- NA
            }
            return(engs)
          }
        )),
        post_oblast_eng = paste(post_oblast_eng, collapse = "; "),
        .before = post_oblast_ukr
      ) %>%
      select(-post_oblast_list) |>
      ungroup() %>%
      mutate(
        post_item_ukr = post_item_ukr %>%
          str_replace_all(";", ".;") %>%
          paste0("."),
        post_item_eng = post_item_eng %>%
          str_replace_all(";", ".;") %>%
          paste0(".")
      )

    df <- df |>
      mutate(
        post_date_month = ceiling_date(post_date, "month")
      )
    # Connect to DuckDB ----
    con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
    # add data to duckdb table ukr_socialMedia
    dbWriteTable(
      con,
      "ukr_socialMedia",
      df,
      append = TRUE
    )

    df_summary <- df %>%
      select(post_oblast_eng, post_oblast_ukr, post_item_eng, post_item_ukr) %>%
      separate_longer_delim(c(post_oblast_eng, post_oblast_ukr), "; ") %>%
      separate_longer_delim(c(post_item_eng, post_item_ukr), "; ") %>%
      distinct() %>%
      filter(post_oblast_eng != "Unknown") %>%
      filter(post_item_eng != "None") %>%
      left_join(geocode_map, by = "post_oblast_eng")

    # add data to duckdb table ukr_socialMedia_summary
    dbWriteTable(
      con,
      "ukr_socialMedia_summary",
      df_summary,
      append = TRUE
    )

    # save processed files names
    dbWriteTable(
      con,
      "processed_censs_files",
      tibble(file_name = df_name),
      append = TRUE
    )

    # Disconnect when done
    dbDisconnect(con, shutdown = TRUE)

    file.remove(df_path)
  })
} else {
  print("No new CENSS files to process")
}
