box::use(
  shiny[NS, moduleServer, tags, tagList, icon],
  bslib[page_navbar, nav_panel, nav_spacer, bs_theme, font_google, navbar_options],
  bsicons[bs_icon],
  app/logic/data[load_data, prepare_filters],
  app/view/dashboard[ui_dashboard, server_dashboard],
  app/view/risk_map[ui_risk_map, server_risk_map],
  app/view/profile[ui_profile, server_profile],
  app/view/heatmap[ui_heatmap, server_heatmap],
  app/view/facteurs[ui_facteurs, server_facteurs],
  app/view/analyse[ui_analyse, server_analyse],
  app/view/about[ui_about, server_about],
)

.app_data    <- load_data("data_propre")
.app_filters <- prepare_filters(.app_data$accidents, .app_data$dept_filters)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title   = tags$strong("CrashAlert v4.0"),
    theme   = bs_theme(
      version   = 5, bg = "#ffffff", fg = "#1a1a2e",
      primary   = "#1b3a6b", secondary = "#4a6fa5",
      success   = "#28a745", warning = "#ffc107", danger = "#dc3545",
      base_font = font_google("Inter"),
      "card-border-radius" = "0.5rem",
      "card-box-shadow"    = "0 2px 8px rgba(0,0,0,0.08)"
    ),
    navbar_options = navbar_options(bg = "#1b3a6b"),
    fillable = FALSE,
    nav_panel(
      title = tagList(bs_icon("speedometer2"), " Tableau de bord"),
      ui_dashboard(ns("dash"))
    ),
    nav_panel(
      title = tagList(bs_icon("map-fill"), " Carte des risques"),
      ui_risk_map(ns("risk"))
    ),
    nav_panel(
      title = tagList(bs_icon("people-fill"), " Profil accidentologique"),
      ui_profile(ns("profile"))
    ),
    nav_panel(
      title = tagList(bs_icon("clock-fill"), " Temporalite"),
      ui_heatmap(ns("heatmap"))
    ),
    nav_panel(
      title = tagList(bs_icon("shield-exclamation"), " Facteurs de risque"),
      ui_facteurs(ns("facteurs"))
    ),
    nav_panel(
      title = tagList(bs_icon("graph-up-arrow"), " Analyse avancee"),
      ui_analyse(ns("analyse"))
    ),
    nav_spacer(),
    nav_panel(
      title = tagList(bs_icon("info-circle"), " \u00c0 propos"),
      ui_about(ns("about"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    server_dashboard("dash", .app_data, .app_filters)
    server_risk_map("risk", .app_data)
    server_profile("profile", .app_data)
    server_heatmap("heatmap", .app_data)
    server_facteurs("facteurs", .app_data)
    server_analyse("analyse", .app_data)
    server_about("about", .app_data)
  })
}
