#### LOAD LIBRARY ####
library(tidyverse)
library(readxl)
library(janitor)
library(tidygeocoder)
library(sf)
library(leaflet)
library(deeplr)
library(googledrive)
library(here)
library(duckdb)
library(polyglotr)
library(future.apply)

if (file.exists(".env")) {
  library('dotenv')
  load_dot_env(file = ".env")
}
options(
  googledrive_quiet = TRUE,
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


gg_secret <- Sys.getenv('GOOGLE_CREDENTIALS')
dir.create(here::here(".googledrive-temp"), showWarnings = FALSE)
write(gg_secret, file = here::here(".googledrive-temp", "secrets.json"))


db_path <- file.path(
  here::here("data"),
  'database',
  "ukr_firearms_dashboard.duckdb"
)

#### CENSS DATA ####
censs_files <- drive_find(type = 'xlsx') |>
  pull(name)

censs_files_names_old <- read_rds(here::here(
  'data/database/censs_files_names.rds'
))

files_to_process <- censs_files[
  !match(censs_files, censs_files_names_old, nomatch = 0)
]

files_to_process <- censs_files

if (length(files_to_process) > 0) {
  # get db connection
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  plan(sequential)

  censs_data <- future_lapply(files_to_process[3], function(df_name) {
    df_path <- here::here('data/database', df_name)
    drive_download(df_name, path = df_path, overwrite = T)
    df <- bind_rows(
      df_path %>%
        # read file
        read_excel(sheet = 'main cases') |>
        mutate(post_source = 'main cases'),
      df_path %>%
        read_excel(sheet = 'additional cases') |>
        mutate(post_source = 'additional cases')
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

    # translate
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
      ) %>%
      mutate(
        post_oblast_eng = ifelse(
          !is.na(post_oblast_ukr),
          google_translate(post_oblast_ukr, target = "en", source = "uk"),
          NA
        ),
        .before = post_oblast_ukr
      ) %>%
      mutate(
        post_settlement_eng = ifelse(
          !is.na(post_settlement_ukr),
          google_translate(post_settlement_ukr, target = "en", source = "uk"),
          NA
        ),
        .before = post_settlement_ukr
      ) %>%
      ungroup() %>%
      # geocode using osm
      mutate(
        geocode_oblast = ifelse(
          !is.na(post_oblast_ukr),
          paste0(post_oblast_ukr, ", україна"),
          NA
        )
      ) %>%
      # code as unknown
      mutate(
        post_oblast_ukr = ifelse(
          is.na(post_oblast_ukr),
          "невідомо",
          post_oblast_ukr
        )
      ) %>%
      mutate(
        post_oblast_eng = ifelse(
          is.na(post_oblast_eng),
          "Unknown",
          post_oblast_eng
        )
      )

    file.remove(df_path, verbose = F)
  })

  censs_data <- censs_data %>%
    list_rbind() %>%
    mutate(
      post_oblast_eng = post_oblast_eng %>% str_replace_all(", ", "; "),
      post_oblast_ukr = post_oblast_ukr %>% str_replace_all(", ", "; ")
    ) %>%
    mutate(
      post_item_ukr = post_item_ukr %>%
        str_replace_all(";", ".;") %>%
        paste0("."),
      post_item_eng = post_item_eng %>%
        str_replace_all(";", ".;") %>%
        paste0(".")
    )

  # add data to duckdb table ukr_socialMedia
  dbWriteTable(
    con,
    "ukr_socialMedia",
    censs_data,
    append = TRUE
  )

  # extract summary and geocode
  censs_data_summary <- censs_data %>%
    select(post_oblast_eng, post_oblast_ukr, post_item_eng, post_item_ukr) %>%
    separate_longer_delim(c(post_oblast_eng, post_oblast_ukr), "; ") %>%
    separate_longer_delim(c(post_item_eng, post_item_ukr), "; ") %>%
    distinct() %>%
    filter(post_oblast_eng != "Unknown") %>%
    filter(post_item_eng != "None") %>%
    rowwise() %>%
    geocode(
      post_oblast_ukr,
      method = 'osm',
      lat = post_oblast_latitude,
      long = post_oblast_longitude
    ) %>%
    ungroup()

  # add data to duckdb table ukr_socialMedia_summary
  dbWriteTable(
    con,
    "ukr_socialMedia_summary",
    censs_data_summary,
    append = TRUE
  )

  # save processed files names
  write_rds(
    censs_files,
    here::here('data/database/censs_files_names.rds')
  )
  # Disconnect when done
  dbDisconnect(con, shutdown = TRUE)
}
