# UKR DASHBOARD ####
# APP ####

## LOAD MODULES ####
source("R/02_dashboard/00_setup.R")
source("R/02_dashboard/01_login.R")
source("R/02_dashboard/02_summary.R")
source("R/02_dashboard/03_table.R")
# source("R/02_dashboard/04_resource.R")
source("R/02_dashboard/05_about.R")
source("R/02_dashboard/06_update.R")

## UI ####
ui <- page_fixed(
  ### PARAMETERS ####
  # wait and disconnect ui
  useWaiter(),
  waiterPreloader(
    html = spin_loaders(id = 8, color = "white"),
    color = "#262626"
  ),
  waiterShowOnLoad(
    html = spin_loaders(id = 8, color = "white"),
    color = "#262626"
  ),
  disconnectMessage(
    text = "Your session has timed out. Please reload the page. \n Ваша сесія закінчилась. Будь ласка, оновіть сторінку.",
    refresh = "",
    background = "#262626",
    size = 36,
    width = "full",
    top = "center",
    colour = "white",
    overlayColour = "#262626",
    overlayOpacity = 1,
    css = "box-shadow: none;"
  ),
  # google analytics and css
  tags$head(HTML(
    "<!-- Google tag (gtag.js) --><script async src='https://www.googletagmanager.com/gtag/js?id=G-HV2EQ4DDG8'></script>
                 <script>window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'G-HV2EQ4DDG8');</script>"
  )),
  tags$head(HTML(
    '<script src="https://kit.fontawesome.com/c22d0aa69c.js" crossorigin="anonymous"></script>'
  )),
  tags$head(tags$link(
    rel = "icon",
    type = "image/png",
    href = "www/img/sas_bw.png"
  )),
  tags$head(
    includeCSS("www/css/00_dashboard.css"),
    includeCSS("www/css/00_dashboard_small.css"),
    includeCSS("www/css/01_resource.css")
  ),
  # bootstrap theme
  theme = bs_theme(
    bg = "#262626",
    fg = "white",
    primary = "white",
    secondary = "#4A90E2",
    success = "white",
    info = NULL,
    warning = NULL,
    danger = NULL,
    heading_font = bslib::font_google(
      "Montserrat",
      wght = "100;200;300;500;700;900",
      local = F
    ),
    base_font = bslib::font_google(
      "Montserrat",
      wght = "100;200;300;500;700;900",
      local = F
    )
  ),
  # title
  title = "Ukraine Firearm Knowledge Portal",
  lang = "en",
  # ### SECTIONS ####
  navset_tab(
    id = "dashboard",
    nav_spacer(),
    #### LOG IN ####
    nav_panel("Authentification", login_ui("ukr_dashboard")),
    #### SUMMARY ####
    nav_panel(
      "Overview/Огляд",
      page_sidebar(
        fillable = T,
        fill = T,
        sidebar = tagList(
          firearm_side_ui("ukr_dashboard"),
          firearm_summary_download_ui("ukr_dashboard")
        ),
        firearm_summary_ui("ukr_dashboard")
      )
    ),
    #### Database ####
    nav_panel(
      "Database/База даних",
      page_sidebar(
        fillable = T,
        fill = T,
        sidebar = tagList(
          firearm_side_ui("ukr_dashboard_db"),
          firearm_database_download_ui("ukr_dashboard_db")
        ),
        firearm_database_ui("ukr_dashboard_db")
      )
    ),
    # #### DOCUMENTATION ####
    # nav_panel(
    #   "Documentation",
    #   h1("Documentation"),
    #   layout_sidebar(
    #     fillable = T,
    #     fill = T,
    #     sidebar = resource_side_ui("ukr_dashboard"),
    #     resource_main_ui("ukr_dashboard")
    #   )
    # ),
    #### ABOUT ####
    nav_panel(
      "About/Про",
      h1("Dashboard description / Опис приладової панелі"),
      about_ui("ukr_dashboard")
    ),
    ## UPDATE ###
    nav_panel(
      "Update/Оновлення",
      update_ui('ukr_dashboard')
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  #### LOG IN ####
  #hide  panels
  nav_hide(id = "dashboard", "Overview")
  nav_hide(id = "dashboard", "Database")
  nav_hide(id = "dashboard", "Documentation")
  nav_hide(id = "dashboard", "About")
  nav_hide(id = "dashboard", "Update")
  #### MODULES ####
  # # log in module
  login_server("ukr_dashboard", parent_session = session)
  # # summary module
  firearm_summary_server(
    "ukr_dashboard",
    firearm_table = firearm_table,
    firearm_summary_table = firearm_summary_table,
    palette_factor = palette_factor,
    palette_color = palette_color
  )
  firearm_summary_server(
    "ukr_dashboard_db",
    firearm_table = firearm_table,
    firearm_summary_table = firearm_summary_table,
    palette_factor = palette_factor,
    palette_color = palette_color
  )
  # about module
  about_server("ukr_dashboard", data = about)
  # resource module
  #resource_server("ukr_dashboard", data = resource)
  # update module
  update_server("ukr_dashboard", parent_session = session)
  #onStop(function() dbDisconnect(con, shutdown = T))
}

shinyApp(ui = ui, server = server)
