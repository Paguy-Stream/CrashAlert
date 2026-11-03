# app/view/analyse.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI,
        textOutput, renderText, HTML, div, span, p, selectInput,
        checkboxInput, updateSelectInput, observe, reactive, req,
        withProgress, incProgress, downloadButton, downloadHandler],
  bslib[layout_columns, card, card_header, card_body, value_box,
        navset_card_tab, nav_panel],
  bsicons[bs_icon],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace,
         add_bars, add_markers],
  DT[DTOutput, renderDT, datatable, formatStyle, styleInterval,
     styleEqual, styleColorBar],
  dplyr[filter, mutate, group_by, summarise, n, n_distinct, arrange,
        desc, slice_head, ungroup, pull, case_when, across, left_join,
        inner_join, select, rename, first, distinct, ntile],
  rmarkdown[render],
  tibble[column_to_rownames],
  leaflet[leafletOutput, renderLeaflet, leaflet, addProviderTiles,
          providers, setView, addPolygons, addLegend, colorFactor,
          highlightOptions, labelOptions],
  sf[st_read],
  cluster[silhouette],
  FactoMineR[MCA],
  stats[kmeans, setNames, dist, lm, coef, fitted, cor, median, quantile, loess, predict],
)

# Codes DOM-TOM
.DOMTOM <- c("971","972","973","974","975","976","977","978",
             "984","985","986","987","988","989")

# Groupes issus du clustering des 4 variables structurelles (mortalité non utilisée)
# 1 = score structurel moyen le plus élevé, 3 = le plus bas
.CLUSTER_NOMS <- c(
  "1" = "Réseau rural dominant",
  "2" = "Réseau mixte",
  "3" = "Réseau urbain dominant"
)
.CLUSTER_COULEURS <- c(
  "1" = "#e74c3c",
  "2" = "#f39c12",
  "3" = "#27ae60"
)

#' @export
ui_analyse <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .ana-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        flex-wrap: wrap; gap: 10px;
      }
      .ana-content { padding: 16px; }
      .section-title {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6c757d; margin: 20px 0 12px;
        display: flex; align-items: center; gap: 8px;
      }
      .section-title::after { content: ''; flex: 1; height: 1px; background: #e9ecef; }
      .cluster-badge {
        display: inline-block; padding: 3px 12px; border-radius: 12px;
        font-size: 12px; font-weight: 700; color: #fff; margin: 2px;
      }
      .methodo-box {
        background: #f8f9fa; border-left: 4px solid #4a6fa5;
        border-radius: 6px; padding: 14px 18px;
        font-size: 13px; line-height: 1.8; color: #2c3e50;
      }
      .insight-box {
        background: linear-gradient(135deg, #f8f9ff 0%, #e8edf5 100%);
        border-left: 4px solid #1b3a6b; border-radius: 6px;
        padding: 12px 16px; font-size: 13px; line-height: 1.7; color: #2c3e50;
      }
      .lecture-box {
        background: #fff8e1; border-left: 4px solid #f39c12;
        border-radius: 6px; padding: 12px 16px; margin-top: 12px;
        font-size: 13px; line-height: 1.8; color: #2c3e50;
      }
      .resultat-box {
        background: #eafaf1; border-left: 4px solid #27ae60;
        border-radius: 6px; padding: 12px 16px; margin-top: 12px;
        font-size: 13px; line-height: 1.8; color: #2c3e50;
      }
    ")),

    div(class = "ana-topbar",
      div(style="display:flex;align-items:center;gap:16px;",
        div(style="font-weight:700;color:#1b3a6b;font-size:14px;",
            bs_icon("graph-up-arrow"), " Statistiques approfondies — France metropolitaine 2015-2025"),
        div(style="color:#6c757d;font-size:12px;",
            bs_icon("info-circle"),
            " Clustering k-means | Score de risque structurel | ACM")
      ),
      div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          uiOutput(ns("data_summary"), inline=TRUE),
          tags$span(style="color:#dee2e6;", "|"),
          selectInput(ns("region_rapport"),
            label    = NULL,
            choices  = c("— Choisir une région —" = ""),
            selected = "",
            width    = "220px"
          ),
          downloadButton(ns("dl_rapport_html"),
            label = " Rapport HTML",
            icon  = shiny::icon("file-code"),
            class = "btn btn-sm btn-primary",
            style = "font-size:11px;padding:3px 9px;"
          ),
          downloadButton(ns("dl_rapport_pdf"),
            label = " Rapport PDF",
            icon  = shiny::icon("file-pdf"),
            class = "btn btn-sm btn-danger",
            style = "font-size:11px;padding:3px 9px;"
          )
      )
    ),

    div(class = "ana-content",

      # ── Section 1 : Score de risque composite ──────────────────────────────
      div(class="section-title",
          bs_icon("shield-fill-exclamation"), " Score de risque structurel par département"),

      uiOutput(ns("methodo_score")),

      layout_columns(
        col_widths=c(7,5),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("map-fill"), " Carte score de risque")),
          card_body(padding=0, leafletOutput(ns("carte_risque"), height="460px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("trophy-fill"), " Top 15 departements a risque")),
          card_body(DTOutput(ns("top15_risque")))
        )
      ),

      div(class="lecture-box",
        tags$b(bs_icon("eye"), " Comment lire cette carte :"), tags$br(),
        "Chaque département est coloré selon son score structurel, du vert (faible) au rouge foncé (élevé). ",
        "Il résume quatre caractéristiques des accidents du département : routes départementales, hors agglomération, nuit sans éclairage public et sorties de route. ",
        tags$b("Survolez un département"), " pour voir son score et sa classe. ",
        "Les classes sont des quartiles : elles indiquent une position relative, chacune regroupe environ un quart des départements.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Limite : sans données de trafic (véhicule-km), les parts décrivent la composition des accidents et non celle du réseau routier.")
      ),

      uiOutput(ns("resultat_score")),

      tags$div(class="mt-3"),

      # ── Section 2 : Clustering ─────────────────────────────────────────────
      div(class="section-title",
          bs_icon("diagram-3-fill"), " Clustering k-means — Profils territoriaux"),

      uiOutput(ns("methodo_cluster")),

      layout_columns(
        col_widths=c(6,6),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("map-fill"), " Carte clusters territoriaux")),
          card_body(padding=0, leafletOutput(ns("carte_clusters"), height="400px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("bar-chart-fill"), " Profil moyen par cluster")),
          card_body(plotlyOutput(ns("radar_clusters"), height="400px"))
        )
      ),

      div(class="lecture-box",
        tags$b(bs_icon("eye"), " Comment lire ces deux graphiques :"), tags$br(),
        "La carte regroupe les 96 départements en ", tags$b("3 groupes"),
        " selon la structure de leurs accidents, sans a priori géographique. ",
        "Le radar montre la part moyenne de chaque groupe sur les 4 variables. ",
        tags$b("La mortalité n'est pas utilisée pour former les groupes : elle sert à les valider.")
      ),

      uiOutput(ns("resultat_cluster")),

      tags$div(class="mt-3"),

      layout_columns(
        col_widths=c(12),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("table"), " Tableau des clusters")),
          card_body(DTOutput(ns("table_clusters")))
        )
      ),

      tags$div(class="mt-3"),

      # ── Section 3 : ACM ────────────────────────────────────────────────────
      div(class="section-title",
          bs_icon("diagram-3"), " ACM — Analyse des Correspondances Multiples"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("graph-up"), " Methodologie :"), tags$br(),
        "ACM sur échantillon aléatoire de 20 000 accidents métropolitains. Variables actives : ",
        tags$b("tranche d\u2019\u00e2ge, sexe, cat\u00e9gorie v\u00e9hicule, m\u00e9t\u00e9o, luminosit\u00e9, type de route."),
        " La gravité n'est pas une variable de l'ACM. ",
        "Les modalit\u00e9s proches sur le biplot sont statistiquement associ\u00e9es."
      ),

      layout_columns(
        col_widths=c(8,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("diagram-3"),
                              " Biplot ACM — Dim 1 x Dim 2")),
          card_body(plotlyOutput(ns("acm_biplot"), height="500px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("percent"),
                              " Variance expliquee par dimension")),
          card_body(
            plotlyOutput(ns("acm_variance"), height="240px"),
            tags$hr(),
            div(class="methodo-box", style="margin-top:8px;font-size:12px;",
              tags$b("Lecture du biplot :"), tags$br(),
              "\u2022 Modalit\u00e9s proches = souvent co-pr\u00e9sentes", tags$br(),
              "\u2022 Modalit\u00e9s \u00e9loign\u00e9es = profils oppos\u00e9s", tags$br(),
              "\u2022 Distance \u00e0 l\u2019origine = caract\u00e8re discriminant"
            )
          )
        )
      ),

      div(class="lecture-box",
        tags$b(bs_icon("eye"), " Comment lire ce graphique :"), tags$br(),
        "Ce graphique, appel\u00e9 biplot, place chaque ",
        tags$b("profil d\u2019accident"), " dans un espace \u00e0 deux dimensions. ",
        "R\u00e8gle de lecture : ", tags$b("deux points proches = deux situations fr\u00e9quemment observ\u00e9es ensemble"),
        " dans les m\u00eames accidents. Plus un point est \u00e9loign\u00e9 du centre, ",
        "plus ce profil est ", tags$b("atypique"), ", c\u2019est-\u00e0-dire tr\u00e8s diff\u00e9rent du profil moyen. ",
        "Les points proches du centre (V\u00e9hicule l\u00e9ger, Route d\u00e9partementale) repr\u00e9sentent ",
        tags$b("l\u2019accident banal et fr\u00e9quent."), tags$br(),
        "Chaque axe explique environ 6% de la variabilit\u00e9 totale. C\u2019est attendu en ACM : ",
        "la r\u00e9alit\u00e9 est multidimensionnelle. ",
        tags$b("L\u2019ACM d\u00e9crit des profils, pas des niveaux de gravit\u00e9.")
      ),

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        tags$b("Axe 1 (horizontal) : hors agglomération rapide vs milieu urbain."),
        " Les modalités qui contribuent le plus à cet axe sont ",
        tags$b("Nuit sans éclairage (21,8 %), Autoroute (17,6 %) et Poids lourd (9,5 %)"),
        " à droite, opposées à ",
        tags$b("Urbaine (15,3 %) et Cyclomoteur (8,3 %)"), " à gauche. ",
        "Ce sont des contextes structurellement différents : routes rapides et non éclairées d'un côté, voirie urbaine et deux-roues légers de l'autre.",
        tags$br(), tags$br(),
        tags$b("Axe 2 (vertical) : seniors et femmes vs jeunes et hommes."),
        " Cet axe est structuré par ",
        tags$b("65 ans et plus (17,3 %) et Féminin (14,7 %)"),
        " en haut, opposés à ",
        tags$b("Masculin (9,9 %), Nuit avec éclairage (9,3 %), Cyclomoteur (6,7 %) et 18-24 ans (5,0 %)"),
        " en bas.",
        tags$br(), tags$br(),
        tags$b("Points notables :"), tags$br(),
        "• ", tags$b("Poids lourd"), " est la modalité la plus à droite (Dim1 = 1,92), proche de Nuit sans éclairage et d'Autoroute : ces contextes sont souvent associés dans l'échantillon. ",
        tags$em("L'ACM ne dit rien de leur mortalité ; voir l'onglet Facteurs de risque."), tags$br(),
        "• ", tags$b("Vélo et Cyclomoteur sont opposés sur l'axe vertical"),
        " : le vélo (Dim2 = 1,51) est associé aux seniors, le cyclomoteur (Dim2 = -0,91) aux plus jeunes.", tags$br(),
        "• ", tags$b("Les EDP motorisés"), " (Dim1 = -1,61) et le vélo à assistance électrique (Dim1 = -1,50) sont parmi les profils les plus à gauche, c'est-à-dire les plus urbains.",
        tags$br(), tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Note : l\u2019ACM identifie des associations entre contextes d\u2019accidents. ",
          "Pour la mortalit\u00e9 par profil, croiser avec les onglets ",
          tags$b("Profil accidentologique"), " et ", tags$b("Facteurs de risque"), ".")
      ),

      tags$div(class="mt-3"),

      # ── Section 4 : Évolution temporelle ──────────────────────────────────────
      div(class="section-title", bs_icon("graph-up"), " \u00c9volution temporelle — Mortalit\u00e9 2015-2025"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("info-circle"), " Lecture :"), tags$br(),
        "Taux de mortalit\u00e9 annuel (% accidents mortels) pour les d\u00e9partements s\u00e9lectionn\u00e9s. ",
        "La ligne France repr\u00e9sente la moyenne nationale m\u00e9tropolitaine. ",
        tags$b("2020 : ann\u00e9e COVID"), " — baisse du trafic, taux non comparable aux autres ann\u00e9es.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("exclamation-triangle"),
          " Les tendances (badges) ne sont affich\u00e9es que si R\u00b2 \u2265 0.4 sur la r\u00e9gression lin\u00e9aire 2015-2025. ",
          "Un R\u00b2 inf\u00e9rieur indique une \u00e9volution irr\u00e9guli\u00e8re, non qualifiable comme tendance.")
      ),

      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(tagList(bs_icon("funnel"), " S\u00e9lection")),
          card_body(
            selectInput(ns("dept_evol"),
              label    = "D\u00e9partements (3 max)",
              choices  = NULL,
              multiple = TRUE,
              width    = "100%"
            ),
            tags$hr(),
            checkboxInput(ns("show_france"),
              label = "Afficher moyenne France",
              value = TRUE
            ),
            checkboxInput(ns("show_tendance"),
              label = "Afficher ligne de tendance",
              value = FALSE
            ),
            tags$hr(),
            uiOutput(ns("badges_tendance"))
          )
        ),
        card(full_screen = TRUE,
          card_header(tagList(bs_icon("graph-up"), " \u00c9volution du taux de mortalit\u00e9")),
          card_body(plotlyOutput(ns("evol_temporelle"), height = "420px"))
        )
      ),

      div(class="lecture-box",
        tags$b(bs_icon("eye"), " Comment lire ce graphique :"), tags$br(),
        "Chaque courbe repr\u00e9sente le taux de mortalit\u00e9 annuel d\u2019un d\u00e9partement : ",
        "proportion d\u2019accidents mortels parmi tous les accidents enregistr\u00e9s. ",
        "Un d\u00e9partement dont la courbe descend sous la moyenne France s\u2019am\u00e9liore relativement. ",
        "Les petits d\u00e9partements (< 100 accidents/an) ont des courbes plus irr\u00e9guli\u00e8res ",
        "en raison de leur faible volume statistique."
      ),

      uiOutput(ns("evol_resultat_box")),

      tags$div(class="mt-3"),

      # ── Section 5 : Corrélation score × évolution ─────────────────────────────
      div(class="section-title", bs_icon("arrow-left-right"),
          " Correlation score de risque \u00d7 \u00e9volution temporelle"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("info-circle"), " Lecture :"), tags$br(),
        "Chaque point est un d\u00e9partement. L\u2019axe X repr\u00e9sente le score structurel (0-100), ",
        "l\u2019axe Y la pente de r\u00e9gression lin\u00e9aire du taux de mortalit\u00e9 sur 2015-2025 (pt/an). ",
        "Une pente n\u00e9gative indique une am\u00e9lioration, positive une d\u00e9gradation. ",
        "La taille du point est proportionnelle \u00e0 l\u2019amplitude (max-min) sur 11 ans.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("exclamation-triangle"),
          " Couleur : tendance qualifi\u00e9e (R\u00b2\u22650.4) ou instable (R\u00b2<0.4). ",
          "Les d\u00e9partements instables ont un faible volume d\u2019accidents ou une \u00e9volution non lin\u00e9aire.")
      ),

      layout_columns(
        col_widths = c(12),
        card(full_screen = TRUE,
          card_header(tagList(bs_icon("arrow-left-right"),
                              " Score de risque \u00d7 pente d\u2019\u00e9volution")),
          card_body(plotlyOutput(ns("scatter_score_evol"), height = "500px"))
        )
      ),

      div(class="lecture-box",
        tags$b(bs_icon("eye"), " Comment lire ce graphique :"), tags$br(),
        "La courbe loess (ligne sombre) résume la tendance moyenne entre le score structurel et la pente d'évolution de la mortalité. Le détail par quartile de score est calculé automatiquement dans la boîte ci-dessous."
      ),

      uiOutput(ns("corr_resultat_box")),

      tags$div(class="mt-3"),

      div(class="section-title", bs_icon("lightbulb-fill"), " Synthèse"),
      uiOutput(ns("synthese_box"))
    )
  )
}

#' @export
server_analyse <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    # Calcul features (une seule fois)
    features <- reactive({ app_data$features_clustering })

    # Données temporelles pré-calculées (une seule fois)
    evol_data <- reactive({
      app_data$agg_evol_dept
    })

    # Tendance nationale
    evol_france <- reactive({
      app_data$agg_evol_france
    })

    # Tendances par département (pente + R²)
    tendances <- reactive({
      evol_data() |>
        group_by(dep_clean, departement) |>
        filter(n_distinct(annee) >= 8) |>
        summarise(
          pente = round(coef(lm(taux_mort ~ annee))[2], 4),
          r2    = round(summary(lm(taux_mort ~ annee))$r.squared, 3),
          mort_debut = round(mean(taux_mort[annee <= 2016]), 1),
          mort_fin   = round(mean(taux_mort[annee >= 2023]), 1),
          .groups="drop"
        ) |>
        mutate(
          tendance = case_when(
            r2 < 0.4                ~ "instable",
            pente <= -0.3           ~ "amelioration",
            pente >= 0.3            ~ "degradation",
            TRUE                    ~ "stable"
          )
        )
    })

    # Jointure score × tendances pour Section 5
    corr_data <- reactive({
      f  <- features()
      td <- tendances()

      amp <- evol_data() |>
        group_by(dep_clean) |>
        summarise(amplitude = round(max(taux_mort) - min(taux_mort), 2),
                  .groups = "drop")

      # Utiliser merge base R pour éviter les conflits de select avec box::use
      # departement est dans td (via tendances), pas besoin de le dupliquer depuis f
      f_sel <- f[, intersect(c("dep_clean","score_risque","taux_mortalite"), names(f))]
      merged <- merge(as.data.frame(f_sel), as.data.frame(td),
                      by="dep_clean", all=FALSE)
      merged <- merge(merged, as.data.frame(amp),
                      by="dep_clean", all.x=TRUE)
      merged$amplitude[is.na(merged$amplitude)] <- 0
      merged$quartile <- dplyr::ntile(merged$score_risque, 4)
      merged
    })

    # Initialiser le selectInput avec les départements disponibles
    observe({
      ed <- evol_data()
      choix <- ed |>
        dplyr::select(dep_clean, departement) |>
        distinct() |>
        arrange(departement) |>
        mutate(label = paste0(departement, " (", dep_clean, ")"))
      updateSelectInput(session, "dept_evol",
        choices  = stats::setNames(choix$dep_clean, choix$label),
        selected = c("81","04","86")   # exemples parlants au démarrage
      )
    })

    # ── Graphique évolution temporelle ──────────────────────────────────────────
    output$evol_temporelle <- renderPlotly({
      req(input$dept_evol)
      sel  <- input$dept_evol[seq_len(min(3, length(input$dept_evol)))]
      ed   <- evol_data() |> filter(dep_clean %in% sel)
      ef   <- evol_france()

      couleurs <- c("#e74c3c","#3498db","#f39c12","#27ae60")

      p <- plot_ly()

      # Courbe France
      if (isTRUE(input$show_france)) {
        p <- p |> add_trace(
          data = ef, x = ~annee, y = ~taux_mort,
          type = "scatter", mode = "lines",
          name = "France (moyenne)",
          line = list(color="#95a5a6", dash="dot", width=2),
          hovertemplate = "France %{x} : %{y:.2f}%<extra></extra>"
        )
        # Annotation COVID
        p <- p |> layout(
          shapes = list(list(
            type="line", x0=2020, x1=2020,
            y0=0, y1=1, yref="paper",
            line=list(color="#bdc3c7", dash="dot", width=1)
          )),
          annotations = list(list(
            x=2020, y=1, yref="paper", xanchor="left",
            text="COVID", showarrow=FALSE,
            font=list(size=10, color="#95a5a6")
          ))
        )
      }

      # Courbes départements
      for (i in seq_along(sel)) {
        dept_d <- ed |> filter(dep_clean == sel[i])
        if (nrow(dept_d) == 0) next
        nom <- dept_d$departement[1]
        col <- couleurs[i]

        p <- p |> add_trace(
          data = dept_d, x = ~annee, y = ~taux_mort,
          type = "scatter", mode = "lines+markers",
          name = paste0(nom, " (", sel[i], ")"),
          line = list(color=col, width=2.5),
          marker = list(color=col, size=6),
          hovertemplate = paste0(nom, " %{x} : %{y:.2f}% (n=%{customdata})<extra></extra>"),
          customdata = dept_d$n
        )

        # Ligne de tendance optionnelle
        if (isTRUE(input$show_tendance) && nrow(dept_d) >= 5) {
          fit  <- lm(taux_mort ~ annee, data=dept_d)
          pred <- data.frame(annee=dept_d$annee,
                             fitted=round(fitted(fit), 3))
          p <- p |> add_trace(
            data = pred, x = ~annee, y = ~fitted,
            type = "scatter", mode = "lines",
            name = paste0("Tendance ", nom),
            line = list(color=col, dash="dash", width=1),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
        }
      }

      p |> layout(
        xaxis = list(title="Ann\u00e9e", tickmode="linear", dtick=1,
                     tickfont=list(size=11)),
        yaxis = list(title="Taux de mortalit\u00e9 (%)",
                     ticksuffix="%", rangemode="tozero"),
        legend = list(orientation="h", y=-0.2),
        hovermode = "x unified",
        margin = list(t=20, r=20)
      )
    })

    # ── Badges tendance ──────────────────────────────────────────────────────────
    output$badges_tendance <- renderUI({
      req(input$dept_evol)
      sel <- input$dept_evol[seq_len(min(3, length(input$dept_evol)))]
      td  <- tendances() |> filter(dep_clean %in% sel)
      if (nrow(td) == 0) return(NULL)

      couleurs_badge <- c(
        "amelioration" = "#d4edda",
        "degradation"  = "#f8d7da",
        "stable"       = "#fff3cd",
        "instable"     = "#e2e3e5"
      )
      icones_badge <- c(
        "amelioration" = "arrow-down-circle",
        "degradation"  = "arrow-up-circle",
        "stable"       = "dash-circle",
        "instable"     = "question-circle"
      )
      labels_badge <- c(
        "amelioration" = "Am\u00e9lioration",
        "degradation"  = "D\u00e9gradation",
        "stable"       = "Stable",
        "instable"     = "Instable (R\u00b2<0.4)"
      )

      tagList(
        tags$p(tags$b("Tendances 2015-2025 :"),
               style="margin:0 0 6px;font-size:12px;"),
        lapply(seq_len(nrow(td)), function(i) {
          r <- td[i,]
          bg <- couleurs_badge[r$tendance]
          tags$div(
            style = paste0("background:", bg,
                           ";border-radius:6px;padding:6px 8px;",
                           "margin-bottom:6px;font-size:11px;"),
            tags$b(paste0(r$departement, " (", r$dep_clean, ")")),
            tags$br(),
            bs_icon(icones_badge[r$tendance]), " ",
            labels_badge[r$tendance],
            tags$span(
              style="color:#666;",
              paste0(" | pente ", ifelse(r$pente>0,"+",""), r$pente,
                     "pt/an | R\u00b2=", r$r2)
            ), tags$br(),
            tags$span(
              style="color:#555;",
              paste0(r$mort_debut, "% \u2192 ", r$mort_fin, "%")
            )
          )
        })
      )
    })

    # ── Résultat box évolution — dynamique selon sélection ───────────────────────
    output$evol_resultat_box <- renderUI({
      req(input$dept_evol)
      sel <- input$dept_evol[seq_len(min(3, length(input$dept_evol)))]
      td  <- tendances() |> filter(dep_clean %in% sel)
      ef  <- evol_france()

      # Tendance nationale (fixe)
      mort_debut_fr <- round(mean(ef$taux_mort[ef$annee <= 2016]), 2)
      mort_fin_fr   <- round(mean(ef$taux_mort[ef$annee >= 2023]), 2)
      r2_fr         <- round(summary(lm(taux_mort ~ annee, data = ef))$r.squared, 2)

      # Construire les phrases sur les depts sélectionnés
      phrases_amelio <- td |>
        filter(tendance == "amelioration") |>
        arrange(pente) |>
        mutate(phrase = paste0(departement, " (", dep_clean, ") : ",
                               mort_debut, "% \u2192 ", mort_fin,
                               "% (pente ", pente, "pt/an, R\u00b2=", r2, ")"))

      phrases_degrad <- td |>
        filter(tendance == "degradation") |>
        arrange(desc(pente)) |>
        mutate(phrase = paste0(departement, " (", dep_clean, ") : ",
                               mort_debut, "% \u2192 ", mort_fin,
                               "% (pente +", pente, "pt/an, R\u00b2=", r2, ")"))

      phrases_stable <- td |>
        filter(tendance == "stable") |>
        mutate(phrase = paste0(departement, " (", dep_clean,
                               ") : stable (pente ",
                               ifelse(pente > 0, "+", ""), pente,
                               "pt/an, R\u00b2=", r2, ")"))

      phrases_instab <- td |>
        filter(tendance == "instable") |>
        mutate(phrase = paste0(departement, " (", dep_clean,
                               ") : \u00e9volution irr\u00e9guli\u00e8re (R\u00b2=", r2, ")"))

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        "Tendance nationale : ",
        tags$b(paste0(mort_debut_fr, "%")), " (2015-2016) ",
        "\u2192 ", tags$b(paste0(mort_fin_fr, "%")), " (2023-2025). ",
        "Une droite ajuste mal la s\u00e9rie nationale (R\u00b2=", r2_fr, ") : creux en 2020-2021, remontée ensuite.",
        tags$br(), tags$br(),

        if (nrow(phrases_amelio) > 0) tagList(
          tags$b("Am\u00e9lioration marquée (R\u00b2 \u2265 0.4) :"), tags$br(),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(phrases_amelio$phrase, tags$li)
          )
        ),

        if (nrow(phrases_degrad) > 0) tagList(
          tags$b("D\u00e9gradation marquée (R\u00b2 \u2265 0.4) :"), tags$br(),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(phrases_degrad$phrase, tags$li)
          )
        ),

        if (nrow(phrases_stable) > 0) tagList(
          tags$b("Stable (R\u00b2 \u2265 0.4, pente faible) :"), tags$br(),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(phrases_stable$phrase, tags$li)
          )
        ),

        if (nrow(phrases_instab) > 0) tagList(
          tags$b("\u00c9volution irr\u00e9guli\u00e8re (R\u00b2 < 0.4) :"), tags$br(),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(phrases_instab$phrase, tags$li)
          )
        ),

        tags$em(style="color:#7f8c8d;font-size:12px;margin-top:8px;display:block;",
          bs_icon("info-circle"),
          " Ces \u00e9volutions refl\u00e8tent les accidents enregistr\u00e9s, pas n\u00e9cessairement le risque r\u00e9el. ",
          "L\u2019absence de donn\u00e9es de trafic (v\u00e9hicule-km) emp\u00eache de distinguer ",
          "une baisse d\u2019accidents d\u2019une baisse du trafic.")
      )
    })

    # ── Section 5 : scatter score × pente ────────────────────────────────────────
    output$scatter_score_evol <- renderPlotly({
      cd <- corr_data()

      # Couleurs par tendance
      pal <- c(
        "amelioration" = "#27ae60",
        "degradation"  = "#e74c3c",
        "stable"       = "#f39c12",
        "instable"     = "#bdc3c7"
      )
      labels_tend <- c(
        "amelioration" = "Am\u00e9lioration (R\u00b2\u22650.4)",
        "degradation"  = "D\u00e9gradation (R\u00b2\u22650.4)",
        "stable"       = "Stable (R\u00b2\u22650.4)",
        "instable"     = "Instable (R\u00b2<0.4)"
      )

      # Outliers à annoter
      outliers <- cd |>
        filter(
          (r2 >= 0.4 & abs(pente) >= 0.6) |
          dep_clean %in% c("06","31","13","42")
        )

      p <- plot_ly()

      # Points par tendance
      for (tend in c("amelioration","degradation","stable","instable")) {
        sub <- cd |> filter(tendance == tend)
        if (nrow(sub) == 0) next
        p <- p |> add_trace(
          data = sub,
          x = ~score_risque, y = ~pente,
          type = "scatter", mode = "markers",
          name = labels_tend[tend],
          marker = list(
            color  = pal[tend],
            size   = ~pmax(6, pmin(18, amplitude * 1.2)),
            opacity = 0.75,
            line   = list(color="white", width=0.5)
          ),
          text = paste0(
            "<b>", sub$departement, " (", sub$dep_clean, ")</b><br>",
            "Score : ", round(sub$score_risque, 1), "<br>",
            "Pente : ", sprintf("%+.4f", sub$pente), " pt/an<br>",
            "R\u00b2 : ", sub$r2, "<br>",
            "Amplitude : ", round(sub$amplitude, 1), " pts<br>",
            sub$mort_debut, "% \u2192 ", sub$mort_fin, "%"
          ),
          hovertemplate = "%{text}<extra></extra>"
        )
      }

      # Courbe loess sur tous les points
      loess_fit <- stats::loess(pente ~ score_risque, data=cd, span=0.6)
      loess_seq <- data.frame(score_risque = seq(min(cd$score_risque),
                                                  max(cd$score_risque), length.out=80))
      loess_seq$pente_fit <- predict(loess_fit, newdata=loess_seq)

      p <- p |> add_trace(
        data = loess_seq, x = ~score_risque, y = ~pente_fit,
        type = "scatter", mode = "lines",
        name = "Tendance (loess)",
        line = list(color="#2c3e50", width=2, dash="solid"),
        hoverinfo = "skip",
        showlegend = TRUE
      )

      # Ligne y=0
      p <- p |> layout(
        shapes = list(list(
          type="line", x0=0, x1=100, y0=0, y1=0,
          line=list(color="#7f8c8d", dash="dot", width=1)
        ))
      )

      # Annotations outliers
      annots <- lapply(seq_len(nrow(outliers)), function(i) {
        r <- outliers[i,]
        list(x=r$score_risque, y=r$pente,
             text=paste0(r$departement," (",r$dep_clean,")"),
             showarrow=TRUE, arrowhead=2, arrowsize=0.7,
             ax=ifelse(r$score_risque > 60, -55, 45),
             ay=ifelse(r$pente < 0, -20, 20),
             font=list(size=10, color="#2c3e50"),
             bgcolor="rgba(255,255,255,0.8)",
             bordercolor="#ccc", borderwidth=1)
      })

      p |> layout(
        xaxis = list(title="Score structurel", range=c(0,100),
                     ticksuffix="", zeroline=FALSE),
        yaxis = list(title="Pente du taux de mortalit\u00e9 (pt/an)",
                     ticksuffix="", zeroline=FALSE),
        legend = list(orientation="h", y=-0.2),
        hovermode = "closest",
        annotations = annots,
        margin = list(t=20, r=30)
      )
    })

    # ── Résultat box corrélation — statique (résultats globaux) ──────────────────
    output$corr_resultat_box <- renderUI({
      cd  <- corr_data()
      r_score_pente <- round(cor(cd$score_risque, cd$pente), 3)
      r_mort_pente  <- round(cor(cd$mort_debut,   cd$pente), 3)
      r_score_amp   <- round(cor(cd$score_risque, cd$amplitude, use="complete.obs"), 3)

      # Comptages par quartile
      qq <- cd |>
        mutate(q = dplyr::ntile(score_risque, 4)) |>
        group_by(q) |>
        summarise(
          score_moy  = round(mean(score_risque), 0),
          pente_med  = round(stats::median(pente), 3),
          n_amelio   = sum(pente < -0.3 & r2 >= 0.4),
          n_degrad   = sum(pente >  0.3 & r2 >= 0.4),
          n_instable = sum(r2 < 0.4),
          .groups="drop"
        )

      # Outliers notables
      top_amelio <- cd |> filter(tendance=="amelioration") |>
        arrange(pente) |> slice_head(n=2) |>
        mutate(txt = paste0(departement," (",dep_clean,") : ",
                            pente,"pt/an, R\u00b2=",r2))
      top_degrad <- cd |> filter(tendance=="degradation") |>
        arrange(desc(pente)) |> slice_head(n=2) |>
        mutate(txt = paste0(departement," (",dep_clean,") : +",
                            pente,"pt/an, R\u00b2=",r2))

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        "Corrélation entre le score et la pente d'évolution (r =",
        tags$b(r_score_pente), "). ",
        "Par ailleurs, le niveau de d\u00e9part pr\u00e9dit mieux l\u2019\u00e9volution (R mortalit\u00e9 initiale \u00d7 pente = ",
        tags$b(r_mort_pente), ") : ",
        "les d\u00e9partements qui partaient haut ont davantage baiss\u00e9. ",
        "Les départements à fort score ont aussi une plus grande amplitude de variation annuelle (r=",
        tags$b(r_score_amp), "), mais ils comptent peu d'accidents : leurs taux annuels sont plus instables, donc cette amplitude reflète en partie le bruit statistique.",
        tags$br(), tags$br(),

        tags$b("Par quartile de score (pente m\u00e9diane) :"),
        tags$ul(style="margin:4px 0;padding-left:18px;",
          lapply(seq_len(nrow(qq)), function(i) {
            r <- qq[i,]
            signe <- if (r$pente_med > 0) "+" else ""
            tags$li(
              tags$b(paste0("Q", r$q, " (score ~", r$score_moy, ")")), " : pente m\u00e9diane ",
              tags$b(paste0(signe, r$pente_med, " pt/an")), " | ",
              r$n_amelio, " am\u00e9liorations, ",
              r$n_degrad, " d\u00e9gradations, ",
              r$n_instable, " instables"
            )
          })
        ),

        if (nrow(top_amelio) > 0) tagList(
          tags$b("Meilleures am\u00e9liorations (R\u00b2\u22650.4) :"),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(top_amelio$txt, tags$li))
        ),

        if (nrow(top_degrad) > 0) tagList(
          tags$b("D\u00e9gradations les plus marqu\u00e9es (R\u00b2\u22650.4) :"),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(top_degrad$txt, tags$li))
        ),

        tags$em(style="color:#7f8c8d;font-size:12px;margin-top:8px;display:block;",
          bs_icon("info-circle"),
          " L\u2019effet de r\u00e9gression vers la moyenne est attendu statistiquement. ",
          "Il ne prouve pas l\u2019efficacit\u00e9 de politiques de s\u00e9curit\u00e9 routi\u00e8re sp\u00e9cifiques.")
      )
    })

    output$data_summary <- renderUI({
      f <- features()
      tagList(bs_icon("database"), " ",
              tags$strong(format(attr(app_data$features_clustering, "meta")$n_accidents, big.mark="\u00a0")),
              " accidents | ",
              tags$strong(nrow(f)), " departements")
    })

    # GeoJSON
    geo_sf <- reactive({
      st_read("data_propre/geo/departements.geojson", quiet=TRUE)
    })

    # ── Carte score de risque ────────────────────────────────────────────────
    output$carte_risque <- renderLeaflet({
      f  <- features()
      g  <- geo_sf()
      g  <- merge(g, f, by.x="code", by.y="dep_clean", all.x=TRUE)
      gm <- g[!g$code %in% .DOMTOM, ]

      vals <- gm$score_risque
      vals[is.na(vals)] <- 0

      pal <- leaflet::colorNumeric(
        palette  = c("#d4edda","#fff3cd","#ffd6a5","#f8d7da","#dc3545"),
        domain   = vals, na.color="#e9ecef")

      leaflet(gm) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        setView(2.2137, 46.5, zoom=6) |>
        addPolygons(
          fillColor   = ~pal(vals),
          fillOpacity = 0.8,
          color="#fff", weight=1.5,
          highlight = highlightOptions(weight=3, color="#1b3a6b",
                                       fillOpacity=0.9, bringToFront=TRUE),
          label = ~paste0(
            ifelse(is.na(departement), nom, departement),
            " — Score : ", round(vals, 1),
            " | Classe : ", ifelse(is.na(classe_risque),"N/A",classe_risque)
          ),
          labelOptions = labelOptions(
            style=list("font-weight"="600","font-size"="13px"),
            direction="auto")
        ) |>
        addLegend("bottomright", pal=pal, values=vals,
                  title="Score risque", opacity=0.9)
    })

    # ── Top 15 risque ─────────────────────────────────────────────────────────
    output$top15_risque <- renderDT({
      f <- features() |>
        arrange(desc(score_risque)) |>
        slice_head(n=15) |>
        mutate(across(c(part_route_dept, part_hors_agglo, taux_mortalite), ~ round(.x, 1))) |>
        select(departement, region, score_risque, classe_risque,
               part_route_dept, part_hors_agglo, taux_mortalite) |>
        rename(Departement=departement, Region=region,
               Score=score_risque, Classe=classe_risque,
               `Route dép.%`=part_route_dept, `Hors agglo%`=part_hors_agglo,
               `Mortalité observée%`=taux_mortalite)

      datatable(f, rownames=FALSE,
        options=list(pageLength=15, dom="t", ordering=FALSE),
        class="table table-hover table-sm") |>
        formatStyle("Score",
          background=styleColorBar(c(0,100),"#f8d7da"),
          backgroundSize="100% 90%", backgroundRepeat="no-repeat",
          backgroundPosition="center") |>
        formatStyle("Classe",
          color=styleEqual(c("Faible","Modéré","Élevé","Très élevé"),
            c("#27ae60","#f39c12","#e74c3c","#c0392b")))
    })

    # ── Carte clusters ────────────────────────────────────────────────────────
    output$carte_clusters <- renderLeaflet({
      f  <- features()
      g  <- geo_sf()
      g  <- merge(g, f, by.x="code", by.y="dep_clean", all.x=TRUE)
      gm <- g[!g$code %in% .DOMTOM, ]

      pal_cluster <- colorFactor(
        palette = unname(.CLUSTER_COULEURS),
        levels  = names(.CLUSTER_COULEURS),
        na.color= "#e9ecef")

      leaflet(gm) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        setView(2.2137, 46.5, zoom=6) |>
        addPolygons(
          fillColor   = ~pal_cluster(cluster),
          fillOpacity = 0.8,
          color="#fff", weight=1.5,
          highlight = highlightOptions(weight=3, color="#1b3a6b",
                                       fillOpacity=0.9, bringToFront=TRUE),
          label = ~paste0(
            ifelse(is.na(departement), nom, departement), " — ",
            ifelse(is.na(cluster_nom), "N/A", cluster_nom)
          ),
          labelOptions = labelOptions(
            style=list("font-weight"="600","font-size"="13px"),
            direction="auto")
        ) |>
        addLegend("bottomright",
          colors = unname(.CLUSTER_COULEURS),
          labels = unname(.CLUSTER_NOMS),
          title  = "Profil territorial", opacity=0.9)
    })

    # ── Radar clusters ────────────────────────────────────────────────────────
    output$radar_clusters <- renderPlotly({
      vars <- c("part_route_dept","part_sans_collision","part_nuit_sans_eclairage","part_hors_agglo")
      lab  <- c("Route départementale","Sorties de route","Nuit sans éclairage","Hors agglomération")
      f <- features() |>
        group_by(cluster, cluster_nom) |>
        summarise(dplyr::across(dplyr::all_of(vars), ~ round(mean(.x), 1)), .groups="drop")
      p <- plot_ly(type="scatterpolar", fill="toself", mode="lines")
      for (i in seq_len(nrow(f))) {
        vals <- as.numeric(f[i, vars])
        col  <- .CLUSTER_COULEURS[[f$cluster[i]]]
        p <- p |> add_trace(
          r = c(vals, vals[1]), theta = c(lab, lab[1]),
          name = f$cluster_nom[i], mode = "lines",
          line = list(color = col), fillcolor = paste0(col, "33"))
      }
      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, ticksuffix = "%")),
        legend = list(orientation = "h", y = -0.15),
        paper_bgcolor = "transparent",
        margin = list(t = 20, b = 60))
    })

    # ── Tableau clusters ──────────────────────────────────────────────────────
    output$table_clusters <- renderDT({
      f <- features() |>
        arrange(cluster, desc(score_risque)) |>
        mutate(across(c(part_route_dept, part_sans_collision, part_nuit_sans_eclairage, part_hors_agglo,
                        part_accidents_18_24, part_accidents_2rm, part_accidents_weekend, taux_mortalite),
                      ~ round(.x, 1))) |>
        select(cluster_nom, departement, region, score_risque,
               part_route_dept, part_sans_collision, part_nuit_sans_eclairage, part_hors_agglo,
               part_accidents_18_24, part_accidents_2rm, part_accidents_weekend,
               taux_mortalite, nb_accidents) |>
        rename(Groupe=cluster_nom, Departement=departement, Region=region, Score=score_risque,
               `Route dép.%`=part_route_dept, `Sans collision%`=part_sans_collision,
               `Nuit sans éclairage%`=part_nuit_sans_eclairage, `Hors agglo%`=part_hors_agglo,
               `Hors score : 18-24 ans%`=part_accidents_18_24, `Hors score : 2RM%`=part_accidents_2rm,
               `Hors score : week-end%`=part_accidents_weekend,
               `Mortalité observée%`=taux_mortalite, `Nb acc.`=nb_accidents)

      datatable(f, rownames=FALSE, filter="top",
        options=list(pageLength=15, scrollX=TRUE, scrollY="350px", dom="ftp"),
        class="table table-hover table-sm") |>
        formatStyle("Groupe",
          backgroundColor = styleEqual(unname(.CLUSTER_NOMS), c("#f8d7da","#fff3cd","#d4edda")))
    })

    # ── ACM ───────────────────────────────────────────────────────────────────
    acm_result <- reactive({ app_data$acm_result })

    output$acm_biplot <- renderPlotly({
      acm <- acm_result()
      res <- acm$res

      # Coordonnées modalités
      coords_mod <- as.data.frame(res$var$coord[, 1:2])
      colnames(coords_mod) <- c("Dim1","Dim2")
      coords_mod$modalite <- rownames(coords_mod)
      coords_mod$variable <- gsub("_.*","", sub("^([^_]+)_.*","\\1",
        sapply(rownames(coords_mod), function(x) {
          # Récupérer la variable d'origine
          for (v in colnames(res$call$X)) {
            if (startsWith(x, paste0(v,"_")) || x %in% paste0(v,"_",levels(factor(res$call$X[[v]])))) {
              return(v)
            }
          }
          return("autre")
        })
      ))

      # Couleurs par variable
      vars_uniq <- unique(coords_mod$variable)
      palette <- c("#1b3a6b","#e74c3c","#27ae60","#f39c12","#9b59b6","#1abc9c","#e67e22")
      col_map  <- setNames(palette[seq_along(vars_uniq)], vars_uniq)

      # Cos2 pour taille des points
      cos2 <- rowSums(res$var$cos2[,1:2])

      p <- plot_ly()

      # Points modalités
      for (v in vars_uniq) {
        dd <- coords_mod[coords_mod$variable==v, ]
        cos2_v <- cos2[rownames(coords_mod)[coords_mod$variable==v]]
        p <- p |> add_markers(
          data=dd, x=~Dim1, y=~Dim2,
          name=v,
          marker=list(
            color=col_map[v], size=10+cos2_v*15,
            line=list(color="#fff",width=1)),
          text=~modalite, hoverinfo="text"
        )
        # Labels
        p <- p |> add_trace(
          data=dd, x=~Dim1, y=~Dim2,
          type="scatter", mode="text",
          text=~gsub(paste0(v,"_"),"",modalite),
          textposition="top center",
          textfont=list(size=9, color=col_map[v]),
          showlegend=FALSE, hoverinfo="none"
        )
      }

      # Axes
      eig <- res$eig
      pct1 <- round(eig[1,2],1)
      pct2 <- round(eig[2,2],1)

      p |> layout(
        xaxis=list(title=paste0("Dim 1 (",pct1,"%)"),
                   zeroline=TRUE, zerolinecolor="#dee2e6"),
        yaxis=list(title=paste0("Dim 2 (",pct2,"%)"),
                   zeroline=TRUE, zerolinecolor="#dee2e6"),
        legend=list(title=list(text="Variable"), orientation="v"),
        paper_bgcolor="transparent", plot_bgcolor="transparent",
        margin=list(t=10))
    })

    output$acm_variance <- renderPlotly({
      acm <- acm_result()
      eig <- as.data.frame(acm$res$eig[1:8,])
      eig$dim <- paste0("Dim",seq_len(nrow(eig)))

      plot_ly(eig, x=~dim, y=~`percentage of variance`,
              type="bar",
              marker=list(color=c("#1b3a6b","#4a6fa5","#7eb3ff",
                                  rep("#dee2e6",5))),
              text=~paste0(round(`percentage of variance`,1),"%"),
              textposition="outside") |>
        layout(xaxis=list(title=""),
               yaxis=list(title="% variance"),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=25,b=5))
    })

    # ── Synthèse ──────────────────────────────────────────────────────────────
    # ── Rapport région ─────────────────────────────────────────────────────────

    # Peupler le sélecteur région
    observe({
      regions <- app_data$accidents_dashboard |>
        filter(!is.na(region)) |>
        distinct(region) |>
        arrange(region) |>
        pull(region) |>
        as.character()
      shiny::updateSelectInput(session, "region_rapport",
        choices  = c("— Choisir une région —" = "", regions),
        selected = "")
    })

    # Fonction de génération
    .gen_rapport_ana <- function(format_out) {
      req(input$region_rapport, input$region_rapport != "")
      region_sel <- input$region_rapport
      acc_region <- app_data$accidents_dashboard |>
        dplyr::filter(as.character(region) == region_sel)

      ext    <- if (format_out == "html_document") "html" else "pdf"
      tmpdir <- tempdir()
      rmd_src <- "app/report/rapport_region.Rmd"
      css_src <- "app/report/rapport_style.css"
      rmd_tmp <- file.path(tmpdir, "rapport_region.Rmd")
      css_tmp <- file.path(tmpdir, "rapport_style.css")
      file.copy(rmd_src, rmd_tmp, overwrite = TRUE)
      if (file.exists(css_src)) file.copy(css_src, css_tmp, overwrite = TRUE)

      out_file <- file.path(tmpdir, paste0(
        "CrashAlert_", gsub("[^A-Za-z0-9]", "_", region_sel), ".", ext
      ))

      taux_fr_val <- round(mean(app_data$accidents_dashboard$gravite_accident=="Mortel",na.rm=TRUE)*100,2)
      withProgress(message = paste("Generation rapport", toupper(ext), "..."),
                   value = 0.2, {
        incProgress(0.3, detail = region_sel)
        rmarkdown::render(
          input         = rmd_tmp,
          output_format = format_out,
          output_file   = out_file,
          params        = list(
            region    = region_sel,
            taux_fr   = taux_fr_val,
            accidents = acc_region
          ),
          envir         = new.env(parent = globalenv()),
          quiet         = TRUE
        )
        incProgress(0.5)
      })
      out_file
    }

    output$dl_rapport_html <- downloadHandler(
      filename = function() {
        paste0("CrashAlert_", gsub("[^A-Za-z0-9]", "_",
               input$region_rapport), ".html")
      },
      content = function(file) {
        out <- .gen_rapport_ana("html_document")
        file.copy(out, file)
      },
      contentType = "text/html"
    )

    output$dl_rapport_pdf <- downloadHandler(
      filename = function() {
        paste0("CrashAlert_", gsub("[^A-Za-z0-9]", "_",
               input$region_rapport), ".pdf")
      },
      content = function(file) {
        out <- .gen_rapport_ana("pdf_document")
        file.copy(out, file)
      },
      contentType = "application/pdf"
    )

    # ── Textes dynamiques (valeurs lues dans les méta-données du fichier) ─────
    .meta <- function() attr(app_data$features_clustering, "meta")

    output$methodo_score <- renderUI({
      m <- .meta()
      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("calculator"), " Méthodologie du score structurel :"), tags$br(),
        "Moyenne simple de 4 parts standardisées (z-scores), ramenée sur 0-100 : ",
        tags$b("routes départementales"), ", ", tags$b("accidents sans collision (sorties de route)"), ", ",
        tags$b("nuit sans éclairage public"), " et ", tags$b("hors agglomération"), ". ",
        "Poids égaux ; variables et sens du risque choisis sur un critère conceptuel (caractéristiques du territoire, pas des usagers), avant de calculer la corrélation avec la mortalité. ",
        tags$b("La mortalité n'entre pas dans le score"), " : elle sert uniquement à le valider.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          if (!is.null(m)) paste0(" Validation : corrélation de Pearson r = ", m$pearson, " (R² = ", m$r2,
            ", Spearman = ", m$spearman, ") avec le taux de mortalité observé, sur 96 départements métropolitains, ", m$periode, ". "),
          if (!is.null(m)) paste0("Les quatre variables sont corrélées : la 1re composante d'une ACP en porte ", m$pc1_var,
            " % de la variance, elles mesurent surtout une même dimension. "),
          "Association descriptive entre départements, non causale. ",
          "Indicateurs d'exposition (part d'accidents dont la tranche d'âge dominante est 18-24 ans, deux-roues motorisés, week-end) : affichés dans le tableau, hors score.")
      )
    })

    output$methodo_cluster <- renderUI({
      m <- .meta(); sil <- if (!is.null(m)) m$silhouette else NULL
      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("diagram-3"), " Méthodologie :"), tags$br(),
        "K-means (k=3, nstart=25, seed=42) sur les 4 variables standardisées du score. La mortalité n'est pas utilisée pour former les groupes. ",
        if (!is.null(sil)) paste0("Silhouette : k=2 = ", sil[["k2"]], ", k=3 = ", sil[["k3"]], ", k=4 = ", sil[["k4"]], ". "),
        tags$b("96 départements"), " métropolitains répartis en 3 groupes.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " k=2 est le meilleur choix mathématique ; k=3 est conservé de la version initiale de l'analyse, pour l'interprétation. ",
          "Les groupes découpent un gradient continu plutôt que des classes nettement séparées.")
      )
    })

    output$resultat_cluster <- renderUI({
      res <- features() |> group_by(cluster, cluster_nom) |>
        summarise(n = n(), rd = round(mean(part_route_dept), 0), ha = round(mean(part_hors_agglo), 0),
                  mort = round(mean(taux_mortalite), 1), .groups = "drop") |> arrange(cluster)
      lignes <- lapply(seq_len(nrow(res)), function(i) { r <- res[i, ]
        tagList(tags$span(style=paste0("color:", .CLUSTER_COULEURS[[r$cluster]], ";font-size:16px;"), "●"), " ",
                tags$b(r$cluster_nom), " (", r$n, " départements) : routes départementales ", r$rd,
                " % des accidents, hors agglomération ", r$ha, " %. Mortalité observée (validation) : ",
                tags$b(paste0(r$mort, " %")), ".", tags$br()) })
      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les données nous disent :"), tags$br(),
        tagList(lignes),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " La mortalité moyenne suit l'ordre des groupes sans avoir servi à les former. ",
          "Les parts sont calculées parmi les accidents, pas parmi le réseau routier."))
    })

    output$resultat_score <- renderUI({
      f <- features(); m <- .meta()
      haut <- f |> arrange(desc(score_risque)) |> slice_head(n = 5)
      bas  <- f |> arrange(score_risque) |> slice_head(n = 3)
      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les données nous disent :"), tags$br(),
        "Scores les plus élevés : ", tags$b(paste(haut$departement, collapse = ", ")), ". ",
        "Scores les plus bas : ", tags$b(paste(bas$departement, collapse = ", ")), ". ",
        if (!is.null(m)) paste0("Le score, construit sans la mortalité, est corrélé à la mortalité observée (r = ",
                                m$pearson, ", R² = ", m$r2, ")."),
        tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Corrélation entre départements : elle décrit une association, elle ne prouve pas de causalité. ",
          "Une partie du lien est mécanique, car les accidents hors agglomération sont plus graves."))
    })

    output$synthese_box <- renderUI({
      f <- features()

      # Calculs factuels
      top3       <- f |> arrange(desc(score_risque)) |> slice_head(n=3)
      bottom1    <- f |> arrange(score_risque) |> slice_head(n=1)

      meta <- attr(app_data$features_clustering, "meta")
      cl_haut <- f |> filter(cluster == "1")
      cl_bas  <- f |> filter(cluster == "3")
      mort_haut  <- round(mean(cl_haut$taux_mortalite, na.rm=TRUE), 1)
      mort_bas   <- round(mean(cl_bas$taux_mortalite,  na.rm=TRUE), 1)
      ratio_mort <- round(mort_haut / mort_bas, 1)
      nb_classe  <- table(factor(f$classe_risque, levels=c("Faible","Modéré","Élevé","Très élevé")))
      sc_max <- round(max(f$score_risque), 1); sc_min <- round(min(f$score_risque), 1)
      dept_max_sc <- f$departement[which.max(f$score_risque)]
      dept_max_scol <- f |> arrange(desc(part_sans_collision)) |> slice_head(n=1)
      rd_haut <- round(mean(cl_haut$part_route_dept), 1); rd_bas <- round(mean(cl_bas$part_route_dept), 1)
      ha_haut <- round(mean(cl_haut$part_hors_agglo), 1); ha_bas <- round(mean(cl_bas$part_hors_agglo), 1)

      div(class="insight-box",
        tags$b(bs_icon("lightbulb"), " Enseignements calculés depuis les données :"),
        tags$ul(style="margin:10px 0 0;padding-left:20px;line-height:2.1;",
          tags$li("Mortalité observée moyenne : ", tags$b(paste0(mort_haut, "%")), " dans le groupe ",
                  tags$b("Réseau rural dominant"), ", contre ", tags$b(paste0(mort_bas, "%")),
                  " dans le groupe ", tags$b("Réseau urbain dominant"), " (rapport de ",
                  tags$b(paste0(ratio_mort, "×")), "). La mortalité n'a pas servi à former les groupes."),
          tags$li("Écart entre départements : la part de routes départementales va de ",
                  tags$b(paste0(round(min(f$part_route_dept), 0), "%")), " (", f$departement[which.min(f$part_route_dept)], ") à ",
                  tags$b(paste0(round(max(f$part_route_dept), 0), "%")), " (", f$departement[which.max(f$part_route_dept)], ")."),
          if (!is.null(meta)) tags$li("Validation : le score, construit sans la mortalité, est corrélé au taux de mortalité observé (r = ",
                  tags$b(meta$pearson), ", R² = ", tags$b(meta$r2), "). Association descriptive entre ",
                  nrow(f), " départements, non causale."),
          tags$li("Routes départementales : ", tags$b(paste0(rd_haut, "%")), " des accidents dans le groupe rural, contre ",
                  tags$b(paste0(rd_bas, "%")), " dans le groupe urbain. Hors agglomération : ",
                  tags$b(paste0(ha_haut, "%")), " contre ", tags$b(paste0(ha_bas, "%")), "."),
          tags$li("Accidents sans collision (sorties de route) : maximum dans le département ",
                  tags$b(dept_max_scol$departement), " (", round(dept_max_scol$part_sans_collision, 1), "%)."),
          tags$li("Classes par quartiles : ", paste(names(nb_classe), nb_classe, sep = " = ", collapse = ", "), "."),
          tags$li("Top 3 du score : ",
                  tags$b(paste(paste0(top3$departement, " (", top3$score_risque, ")"), collapse=" › ")), ".")
        ),

        tags$p(style="margin:10px 0 0;font-size:12px;color:#7f8c8d;",
          bs_icon("info-circle"),
          " Ces chiffres sont calculés directement depuis les ",
          format(attr(app_data$features_clustering, "meta")$n_accidents, big.mark="\u00a0"),
          " accidents de la base BAAC 2015-2025. Ils se mettent à jour",
          " dès que le pipeline de données est relancé."
        )
      )
    })
  })
}
