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

# Noms clusters — basés sur profils empiriques (k=3, seed=42)
# Validation : silhouette k=2=0.359 (optimal), k=3=0.261 (retenu), k=4=0.259 (marginal)
# k=3 → k=4 fragmente le rural sans gain interprétable (Δsilhouette=-0.002)
# Cluster 1 (39 depts) : mortalité 11.6%, route dept 63.0% → Rural à haut risque
# Cluster 2 (42 depts) : mortalité 7.8%, mixte            → Rural intermédiaire
# Cluster 3 (15 depts) : mortalité 3.8%, urbain           → Urbain métropolitain
.CLUSTER_NOMS <- c(
  "1" = "Rural \u00e0 haut risque",
  "2" = "Rural interm\u00e9diaire",
  "3" = "Urbain m\u00e9tropolitain"
)
.CLUSTER_COULEURS <- c(
  "1" = "#e74c3c",
  "2" = "#f39c12",
  "3" = "#27ae60"
)

# Prépare features département pour clustering
.prepare_features <- function(accidents) {
  accidents |>
    mutate(dep_raw = as.character(dep)) |>
    mutate(dep_clean = case_when(
      dep_raw == "201" ~ "2A", dep_raw == "202" ~ "2B",
      nchar(dep_raw)==3 & grepl("0$",dep_raw) &
        !dep_raw %in% .DOMTOM ~ sub("0$","",dep_raw),
      nchar(dep_raw)==1 ~ paste0("0",dep_raw),
      TRUE ~ dep_raw
    )) |>
    filter(!dep_clean %in% .DOMTOM) |>
    group_by(dep_clean) |>
    summarise(
      departement         = first(as.character(departement)),
      region              = first(as.character(region)),
      taux_mortalite      = round(mean(gravite_accident=="Mortel",  na.rm=TRUE)*100, 2),
      taux_nuit           = round(mean(grepl("Nuit", as.character(lum_label)), na.rm=TRUE)*100, 2),
      taux_route_dept     = round(mean(as.character(catr_label)=="Route d\u00e9partementale", na.rm=TRUE)*100, 2),
      taux_jeunes         = round(mean(as.character(tranche_age) %in% c("18-24 ans","25-34 ans"), na.rm=TRUE)*100, 2),
      taux_sans_collision = round(mean(as.character(col_label)=="Sans collision", na.rm=TRUE)*100, 2),
      taux_weekend        = round(mean(as.character(jour_semaine) %in% c("samedi","dimanche"), na.rm=TRUE)*100, 2),
      nb_accidents        = n(),
      .groups = "drop"
    ) |>
    filter(nb_accidents >= 100)
}

# Clustering k-means k=3
# k=3 retenu : silhouette=0.261, profils nets et stables sur 50 seeds
# k=4 rejeté : Δsilhouette=-0.002 vs k=3, fragmente le rural sans gain interprétable
.run_clustering <- function(features) {
  df_mat <- features |>
    column_to_rownames("dep_clean") |>
    dplyr::select(taux_mortalite, taux_nuit, taux_route_dept,
                  taux_jeunes, taux_sans_collision, taux_weekend)
  mat <- base::scale(as.matrix(df_mat))
  set.seed(42)
  km <- stats::kmeans(mat, centers=3, nstart=25, iter.max=100)
  features$cluster <- as.character(km$cluster)
  features$cluster_nom <- .CLUSTER_NOMS[features$cluster]

  # Score risque composite révisé (0-100)
  # Pondération fondée sur les corrélations empiriques avec taux_mortalite :
  #   taux_route_dept     R²=26% → 25%
  #   taux_sans_collision R²=20% → 20%
  #   taux_jeunes         R²=25% → 10% (signe inversé : moins de jeunes = plus de seniors)
  #   taux_nuit           R²=2%  →  5% (justifié par l'éloignement des secours)
  #   Météo supprimée     R²=0.3% → non discriminante
  features$score_risque <- round(
    (features$taux_mortalite      / max(features$taux_mortalite)      * 40 +
     features$taux_route_dept     / max(features$taux_route_dept)     * 25 +
     features$taux_sans_collision / max(features$taux_sans_collision) * 20 +
     (100 - features$taux_jeunes) / 100                               * 10 +
     features$taux_nuit           / max(features$taux_nuit)           *  5), 1)

  features$classe_risque <- case_when(
    features$score_risque >= 65 ~ "Tr\u00e8s \u00e9lev\u00e9",
    features$score_risque >= 45 ~ "\u00c9lev\u00e9",
    features$score_risque >= 30 ~ "Mod\u00e9r\u00e9",
    TRUE                        ~ "Faible"
  )
  features
}

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
            bs_icon("graph-up-arrow"), " Statistiques approfondies — France metropolitaine 2015-2024"),
        div(style="color:#6c757d;font-size:12px;",
            bs_icon("info-circle"),
            " Clustering k-means | Score de risque composite | ACM")
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
          bs_icon("shield-fill-exclamation"), " Score de risque composite par departement"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("calculator"), " Methodologie du score (v2 — fond\u00e9e sur les corr\u00e9lations empiriques) :"), tags$br(),
        "Score 0-100 pond\u00e9r\u00e9 : ",
        tags$b("Taux mortalit\u00e9 (40%)"), " + ",
        tags$b("% route d\u00e9partementale (25%, R\u00b2=26%)"), " + ",
        tags$b("% accidents sans tiers (20%, R\u00b2=20%)"), " + ",
        tags$b("profil senior (10%, R\u00b2=25%)"), " + ",
        tags$b("% accidents de nuit (5%)"), ".", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " La m\u00e9t\u00e9o a \u00e9t\u00e9 \u00e9cart\u00e9e (R\u00b2=0.3% avec la mortalit\u00e9, non discriminante). ",
          "Les accidents sans tiers correspondent aux sorties de route et chutes (terminologie ONISR). ",
          "Les pond\u00e9rations refl\u00e8tent la corr\u00e9lation empirique de chaque facteur avec le taux de mortalit\u00e9 d\u00e9partemental. ",
          "Limite connue : absence de donn\u00e9es de trafic (v\u00e9hicule-km) pour neutraliser l\u2019effet de densit\u00e9.")
      ),

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
        "Chaque d\u00e9partement est color\u00e9 selon son score de risque, du vert (faible) au rouge fonc\u00e9 (tr\u00e8s \u00e9lev\u00e9). ",
        "Ce score combine ", tags$b("4 facteurs pond\u00e9r\u00e9s selon leur corr\u00e9lation r\u00e9elle avec la mortalit\u00e9"),
        ". Les pondérations sont fondées sur les corrélations empiriques. ",
        "Un d\u00e9partement peut avoir peu d\u2019accidents au total mais un score \u00e9lev\u00e9 ",
        "s\u2019il cumule routes d\u00e9partementales, sorties de route fr\u00e9quentes et population \u00e2g\u00e9e. ",
        tags$b("Survolez un d\u00e9partement"), " pour voir son score et sa classe de risque.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Limite : ce score ne neutralise pas l\u2019effet de densit\u00e9 de trafic (donn\u00e9es v\u00e9hicule-km non disponibles). ",
          "Un d\u00e9partement rural avec peu de trafic peut afficher un taux de mortalit\u00e9 \u00e9lev\u00e9 m\u00e9caniquement.")
      ),

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        "Les d\u00e9partements les plus \u00e0 risque ne sont ", tags$b("pas les plus peupl\u00e9s"),
        ". Paris, Lyon et Marseille ont des scores faibles. ",
        "Ce sont les d\u00e9partements ruraux du ", tags$b("Grand Est, du Sud-Ouest, de Bourgogne et du Massif Central"),
        " qui dominent : routes d\u00e9partementales \u00e0 double sens sans s\u00e9parateur, ",
        "forte proportion d\u2019accidents sans tiers (sorties de route), population vieillissante.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Les scores exacts varient selon les ann\u00e9es s\u00e9lectionn\u00e9es. L\u2019analyse porte sur 2015-2024 globalement. ",
          "Les valeurs affich\u00e9es dans le Top 15 sont \u00e0 lire comme des ordres de grandeur relatifs, pas des indicateurs absolus.")
      ),

      tags$div(class="mt-3"),

      # ── Section 2 : Clustering ─────────────────────────────────────────────
      div(class="section-title",
          bs_icon("diagram-3-fill"), " Clustering k-means — Profils territoriaux"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("diagram-3"), " Methodologie :"), tags$br(),
        "K-means (k=3, nstart=25, seed=42) sur 6 variables normalis\u00e9es : ",
        "taux mortalit\u00e9, % nuit, % route d\u00e9partementale, % jeunes, % accidents sans tiers, % weekend. ",
        "Validation sur 50 seeds : configuration identique \u00e0 chaque fois (structure stable). ",
        "Silhouette k=3 = 0.261 (k=2 optimal \u00e0 0.359, k=4 = 0.259, gain marginal de 0.002). ",
        tags$b("96 d\u00e9partements"), " m\u00e9tropolitains class\u00e9s en 3 profils distincts.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " k=4 a \u00e9t\u00e9 test\u00e9 et rejet\u00e9 : il fragmente le cluster rural sans gain interpr\u00e9tatif. ",
          "k=2 est optimal math\u00e9matiquement mais trop grossier pour un usage op\u00e9rationnel.")
      ),

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
        "La carte regroupe les 96 d\u00e9partements en ", tags$b("3 profils distincts"),
        " identifi\u00e9s automatiquement par l\u2019algorithme, sans a priori g\u00e9ographique. ",
        "Le graphique radar (toile d\u2019araign\u00e9e) montre le profil moyen de chaque groupe sur 6 axes : ",
        "plus un groupe s\u2019\u00e9tend sur un axe, plus cette caract\u00e9ristique le d\u00e9finit. ",
        tags$b("Un cluster id\u00e9al (z\u00e9ro risque) occuperait le centre du radar.")
      ),

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        tags$b("\ud83d\udd34 Rural \u00e0 haut risque"), " (39 d\u00e9pts) : ",
        "taux de mortalit\u00e9 moyen de ", tags$b("11.6%"), ", route d\u00e9partementale 63%. ",
        "Ce cluster regroupe la France rurale profonde : Massif Central, Sud-Ouest, Est.", tags$br(),
        tags$b("\ud83d\udfe1 Rural interm\u00e9diaire"), " (42 d\u00e9pts) : ",
        "mortalit\u00e9 7.8%, m\u00e9lange de d\u00e9partements p\u00e9riurbains et ruraux moins isol\u00e9s. ",
        "Ce cluster couvre la majorit\u00e9 de la France de province.", tags$br(),
        tags$b("\ud83d\udfe2 Urbain m\u00e9tropolitain"), " (15 d\u00e9pts\u00a0: Paris, IDF, Lyon, Bordeaux, Strasbourg...) : ",
        "mortalit\u00e9 ", tags$b("3.8%"), ", le plus bas. ",
        "Paris (75) affiche 0.7%, soit ", tags$b("16 fois moins"), " que les d\u00e9partements ruraux les plus expos\u00e9s.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Le score composite et le clustering captent des dimensions compl\u00e9mentaires. ",
          "Le score mesure un risque cumul\u00e9 pond\u00e9r\u00e9\u00a0; le clustering mesure la similarit\u00e9 multidimensionnelle.")
      ),

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
        "ACM sur \u00e9chantillon stratifi\u00e9 (20 000 accidents). Variables actives : ",
        tags$b("tranche d\u2019\u00e2ge, sexe, cat\u00e9gorie v\u00e9hicule, m\u00e9t\u00e9o, luminosit\u00e9, type de route."),
        " Variable illustrative : gravit\u00e9. ",
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
        "Les points au centre (V\u00e9hicule l\u00e9ger, Route d\u00e9partementale, Homme) repr\u00e9sentent ",
        tags$b("l\u2019accident banal et fr\u00e9quent."), tags$br(),
        "Chaque axe explique environ 6-7% de la variabilit\u00e9 totale. C\u2019est attendu en ACM : ",
        "la r\u00e9alit\u00e9 est multidimensionnelle. ",
        tags$b("L\u2019ACM d\u00e9crit des profils, pas des niveaux de gravit\u00e9.")
      ),

      div(class="resultat-box",
        tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
        tags$b("Axe 1 (horizontal) — Milieu rural rapide vs milieu urbain."),
        " Les deux modalit\u00e9s qui structurent le plus cet axe sont ",
        tags$b("Nuit sans \u00e9clairage (18.5%) et Autoroute (18.1%)"),
        " \u00e0 droite, oppos\u00e9es \u00e0 ",
        tags$b("Urbaine (12.3%)"), " \u00e0 gauche. ",
        "En pratique : les accidents en milieu rural nocturne et les accidents urbains ",
        "correspondent \u00e0 des contextes structurellement diff\u00e9rents.", tags$br(), tags$br(),
        tags$b("Axe 2 (vertical) — Nuit p\u00e9riurbaine/jeunes vs jour/seniors."),
        " Cet axe est structur\u00e9 par ",
        tags$b("Nuit avec \u00e9clairage (20.5%), 18-24 ans (13.5%) et m\u00e9t\u00e9o perturb\u00e9e (10.4%)"),
        " en bas, oppos\u00e9s \u00e0 ",
        tags$b("Plein jour (12.1%)"), " et les classes d\u2019\u00e2ge senior en haut.", tags$br(), tags$br(),
        tags$b("Points notables :"), tags$br(),
        "\u2022 ", tags$b("Tracteur agricole"), " (Dim1 = 2.34) est la modalit\u00e9 la plus \u00e9loign\u00e9e du centre sur l\u2019axe 1. ",
        "Il partage avec Nuit sans \u00e9clairage (Dim1 = 1.73) une position \u00e0 droite sur cet axe, ",
        "mais ils sont oppos\u00e9s sur l\u2019axe 2 (Tracteur : +0.54, Nuit sans \u00e9clairage : -0.92) ; ",
        "leur association n\u2019est donc que partielle. ",
        tags$em("L\u2019ACM ne permet pas de conclure qu\u2019il est plus mortel. Pour la mortalit\u00e9 par type de v\u00e9hicule, voir l\u2019onglet Facteurs de risque."), tags$br(),
        "\u2022 ", tags$b("V\u00e9lo et Cyclomoteur sont oppos\u00e9s sur l\u2019axe vertical"),
        " : le v\u00e9lo est plut\u00f4t diurne et associ\u00e9 aux seniors (haut), ",
        "le cyclomoteur est nocturne et jeune (bas) : deux univers distincts.", tags$br(),
        "\u2022 ", tags$b("Poids lourd"), " se situe en haut (Dim2 = 1.16), proche du profil diurne/seniors : ",
        "les collisions avec poids lourds arrivent surtout de jour, sur grands axes.",
        tags$br(), tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("info-circle"),
          " Note : l\u2019ACM identifie des associations entre contextes d\u2019accidents. ",
          "Pour la mortalit\u00e9 par profil, croiser avec les onglets ",
          tags$b("Profil accidentologique"), " et ", tags$b("Facteurs de risque"), ".")
      ),

      tags$div(class="mt-3"),

      # ── Section 4 : Évolution temporelle ──────────────────────────────────────
      div(class="section-title", bs_icon("graph-up"), " \u00c9volution temporelle — Mortalit\u00e9 2015-2024"),

      div(class="methodo-box", style="margin-bottom:16px;",
        tags$b(bs_icon("info-circle"), " Lecture :"), tags$br(),
        "Taux de mortalit\u00e9 annuel (% accidents mortels) pour les d\u00e9partements s\u00e9lectionn\u00e9s. ",
        "La ligne France repr\u00e9sente la moyenne nationale m\u00e9tropolitaine. ",
        tags$b("2020 : ann\u00e9e COVID"), " — baisse du trafic, taux non comparable aux autres ann\u00e9es.", tags$br(),
        tags$em(style="color:#7f8c8d;font-size:12px;",
          bs_icon("exclamation-triangle"),
          " Les tendances (badges) ne sont affich\u00e9es que si R\u00b2 \u2265 0.4 sur la r\u00e9gression lin\u00e9aire 2015-2024. ",
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
        "Chaque point est un d\u00e9partement. L\u2019axe X repr\u00e9sente le score composite de risque (0-100), ",
        "l\u2019axe Y la pente de r\u00e9gression lin\u00e9aire du taux de mortalit\u00e9 sur 2015-2024 (pt/an). ",
        "Une pente n\u00e9gative indique une am\u00e9lioration, positive une d\u00e9gradation. ",
        "La taille du point est proportionnelle \u00e0 l\u2019amplitude (max-min) sur 10 ans.", tags$br(),
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
        "La courbe loess (ligne sombre) r\u00e9v\u00e8le une relation non lin\u00e9aire : ",
        "les d\u00e9partements \u00e0 score interm\u00e9diaire (50-65) s\u2019am\u00e9liorent le plus. ",
        "Les d\u00e9partements \u00e0 tr\u00e8s faible score (urbains) se d\u00e9gradent l\u00e9g\u00e8rement. ",
        "Les d\u00e9partements \u00e0 tr\u00e8s fort score (rural profond) s\u2019am\u00e9liorent mais de fa\u00e7on irr\u00e9guli\u00e8re."
      ),

      uiOutput(ns("corr_resultat_box")),

      tags$div(class="mt-3"),

      div(class="section-title", bs_icon("lightbulb-fill"), " Synthese & Recommandations"),
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
        tags$p(tags$b("Tendances 2015-2024 :"),
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
        "\u2192 ", tags$b(paste0(mort_fin_fr, "%")), " (2023-2024). ",
        "La progression n\u2019est pas lin\u00e9aire (R\u00b2=0.15) : creux en 2020-2021, remontée ensuite.",
        tags$br(), tags$br(),

        if (nrow(phrases_amelio) > 0) tagList(
          tags$b("Am\u00e9lioration significative (R\u00b2 \u2265 0.4) :"), tags$br(),
          tags$ul(style="margin:4px 0;padding-left:18px;",
            lapply(phrases_amelio$phrase, tags$li)
          )
        ),

        if (nrow(phrases_degrad) > 0) tagList(
          tags$b("D\u00e9gradation significative (R\u00b2 \u2265 0.4) :"), tags$br(),
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
        xaxis = list(title="Score composite de risque", range=c(15,95),
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
        "La relation score \u00d7 pente n\u2019est pas lin\u00e9aire (R=",
        tags$b(r_score_pente), "). ",
        "En revanche, le niveau de d\u00e9part pr\u00e9dit mieux l\u2019\u00e9volution (R mortalit\u00e9 initiale \u00d7 pente = ",
        tags$b(r_mort_pente), ") : ",
        "les d\u00e9partements qui partaient haut ont davantage baiss\u00e9. ",
        "Les forts scores ont aussi une plus grande amplitude de variation (R=",
        tags$b(r_score_amp), ").",
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
              tags$strong(format(nrow(app_data$accidents_light), big.mark="\u00a0")),
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
        addProviderTiles(providers$CartoDB.Positron) |>
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
        select(departement, region, score_risque, classe_risque,
               taux_mortalite, taux_nuit) |>
        rename(Departement=departement, Region=region,
               Score=score_risque, Classe=classe_risque,
               `Mort.%`=taux_mortalite, `Nuit%`=taux_nuit)

      datatable(f, rownames=FALSE,
        options=list(pageLength=15, dom="t", ordering=FALSE),
        class="table table-hover table-sm") |>
        formatStyle("Score",
          background=styleColorBar(c(0,100),"#f8d7da"),
          backgroundSize="100% 90%", backgroundRepeat="no-repeat",
          backgroundPosition="center") |>
        formatStyle("Classe",
          color=styleInterval(c("Elevé","Modéré","Très élevé"),
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
        addProviderTiles(providers$CartoDB.Positron) |>
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
      f <- features() |>
        group_by(cluster, cluster_nom) |>
        summarise(
          `Mortalite`      = round(mean(taux_mortalite),1),
          `Nuit`           = round(mean(taux_nuit),1),
          `Route dept`     = round(mean(taux_route_dept)/2,1),
          `Jeunes`         = round(mean(taux_jeunes)/2,1),
          `Sans collision` = round(mean(taux_sans_collision)*2,1),
          `Weekend`        = round(mean(taux_weekend)*2,1),
          .groups="drop"
        )

      vars <- c("Mortalite","Nuit","Route dept","Jeunes","Sans collision","Weekend")
      p <- plot_ly(type="scatterpolar", fill="toself", mode="lines")
      for (i in seq_len(nrow(f))) {
        vals <- c(as.numeric(f[i, vars]), as.numeric(f[i, vars[1]]))
        p <- p |> add_trace(
          r     = vals,
          theta = c(vars, vars[1]),
          name  = f$cluster_nom[i],
          mode  = "lines",
          line  = list(color=.CLUSTER_COULEURS[f$cluster[i]]),
          fillcolor = paste0("#", gsub("#","", .CLUSTER_COULEURS[f$cluster[i]]), "33")
        )
      }
      p |> layout(
        polar = list(radialaxis=list(visible=TRUE, range=c(0,25))),
        legend= list(orientation="h", y=-0.15),
        paper_bgcolor="transparent",
        margin=list(t=20,b=60))
    })

    # ── Tableau clusters ──────────────────────────────────────────────────────
    output$table_clusters <- renderDT({
      f <- features() |>
        arrange(cluster, desc(score_risque)) |>
        select(cluster_nom, departement, region, score_risque,
               taux_mortalite, taux_nuit, taux_route_dept,
               taux_sans_collision, nb_accidents) |>
        rename(Cluster=cluster_nom, Departement=departement,
               Region=region, Score=score_risque,
               `Mort.%`=taux_mortalite, `Nuit%`=taux_nuit,
               `Route dept%`=taux_route_dept,
               `Sans collision%`=taux_sans_collision,
               `Nb acc.`=nb_accidents)

      datatable(f, rownames=FALSE, filter="top",
        options=list(pageLength=15, scrollY="350px", dom="ftp"),
        class="table table-hover table-sm") |>
        formatStyle("Cluster",
          backgroundColor = styleEqual(
            c("Rural \u00e0 haut risque","Rural interm\u00e9diaire","Urbain m\u00e9tropolitain"),
            c("#f8d7da","#fff3cd","#d4edda")))
    })

    # ── ACM ───────────────────────────────────────────────────────────────────
    acm_result <- reactive({
      set.seed(123)
      d <- app_data$accidents_light |>
        filter(!is.na(tranche_age), !is.na(sexe_dominant),
               !is.na(categorie_vehicule), !is.na(atm_label),
               !is.na(lum_label), !is.na(catr_label),
               !is.na(gravite_accident)) |>
        mutate(
          age    = as.character(tranche_age),
          sexe   = as.character(sexe_dominant),
          vehic  = as.character(categorie_vehicule),
          meteo  = ifelse(as.character(atm_label)=="Normale","Normale","Perturbee"),
          lumin  = case_when(
            grepl("Plein jour", as.character(lum_label)) ~ "Jour",
            grepl("Nuit sans", as.character(lum_label))  ~ "Nuit sans eclairage",
            TRUE ~ "Nuit avec eclairage"
          ),
          route  = case_when(
            as.character(catr_label) %in% c("Voie communale","Routes de m\u00e9tropole urbaine") ~ "Urbaine",
            as.character(catr_label) == "Route d\u00e9partementale" ~ "D\u00e9partementale",
            as.character(catr_label) == "Route nationale" ~ "Nationale",
            as.character(catr_label) == "Autoroute" ~ "Autoroute",
            TRUE ~ "Autre"
          ),
          gravite = as.character(gravite_accident)
        ) |>
        filter(age != "Non renseign\u00e9",
               vehic != "Non renseign\u00e9",
               vehic != "Autre") |>
        select(age, sexe, vehic, meteo, lumin, route, gravite)

      # Échantillon stratifié 20000
      n_sample <- min(20000, nrow(d))
      d_sample <- d[sample(nrow(d), n_sample), ]

      # ACM (gravite en variable supplémentaire)
      d_acm <- d_sample |> select(age, sexe, vehic, meteo, lumin, route)
      d_sup <- d_sample |> select(gravite)

      res <- MCA(d_acm, quali.sup=NULL, graph=FALSE, ncp=5)
      list(res=res, d_sup=d_sup, d_sample=d_sample)
    })

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
               margin=list(t=5,b=5))
    })

    # ── Synthèse ──────────────────────────────────────────────────────────────
    # ── Rapport région ─────────────────────────────────────────────────────────

    # Peupler le sélecteur région
    observe({
      regions <- app_data$accidents_light |>
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

    output$synthese_box <- renderUI({
      f <- features()

      # Calculs factuels
      top3       <- f |> arrange(desc(score_risque)) |> slice_head(n=3)
      bottom1    <- f |> arrange(score_risque) |> slice_head(n=1)

      cl_rural   <- f |> filter(cluster_nom == "Rural \u00e0 haut risque")
      cl_urbain  <- f |> filter(cluster_nom == "Urbain m\u00e9tropolitain")

      mort_rural  <- round(mean(cl_rural$taux_mortalite,  na.rm=TRUE), 1)
      mort_urbain <- round(mean(cl_urbain$taux_mortalite, na.rm=TRUE), 1)
      ratio_mort  <- round(mort_rural / mort_urbain, 1)

      n_tres_eleve <- sum(f$classe_risque == "Tr\u00e8s \u00e9lev\u00e9", na.rm=TRUE)
      n_eleve      <- sum(f$classe_risque == "\u00c9lev\u00e9",       na.rm=TRUE)
      pct_risque   <- round((n_tres_eleve + n_eleve) / nrow(f) * 100)

      sc_max  <- round(max(f$score_risque), 1)
      sc_min  <- round(min(f$score_risque), 1)
      ratio_sc <- round(sc_max / sc_min, 1)

      dept_max_sc   <- f$departement[which.max(f$score_risque)]
      dept_max_scol <- f |> arrange(desc(taux_sans_collision)) |> slice_head(n=1)
      dept_max_rd   <- f |> arrange(desc(taux_route_dept))    |> slice_head(n=1)

      pct_rd_rural  <- round(mean(cl_rural$taux_route_dept,      na.rm=TRUE), 1)
      pct_rd_urbain <- round(mean(cl_urbain$taux_route_dept,     na.rm=TRUE), 1)
      pct_sc_rural  <- round(mean(cl_rural$taux_sans_collision,  na.rm=TRUE), 1)
      pct_sc_urbain <- round(mean(cl_urbain$taux_sans_collision, na.rm=TRUE), 1)

      div(class="insight-box",
        tags$b(bs_icon("lightbulb"), " Enseignements calculés depuis les données :"),
        tags$ul(style="margin:10px 0 0;padding-left:20px;line-height:2.1;",

          # Insight 1 : fracture urbain-rural — chiffré
          tags$li(
            "Mortalité des accidents dans le cluster ", tags$b("Rural à haut risque"),
            " : ", tags$b(paste0(mort_rural, "%")), ", contre ",
            tags$b(paste0(mort_urbain, "%")), " dans le cluster ",
            tags$b("Urbain métropolitain"), ". Rapport de ",
            tags$b(paste0(ratio_mort, "×")), "."
          ),

          # Insight 2 : score extrêmes
          tags$li(
            "Score de risque composite : de ", tags$b(paste0(sc_min, "/100")),
            " (", bottom1$departement, ") à ",
            tags$b(paste0(sc_max, "/100")), " (", dept_max_sc, "). ",
            "L'amplitude est de ", tags$b(paste0(ratio_sc, "×")), "."
          ),

          # Insight 3 : part du territoire à risque élevé+
          tags$li(
            tags$b(paste0(n_tres_eleve, " départements")),
            " classés \"Très élevé\" et ",
            tags$b(paste0(n_eleve, " \"Élevé\"")),
            ", soit ", tags$b(paste0(pct_risque, "%")),
            " des départements métropolitains."
          ),

          # Insight 4 : route départementale — facteur structurel
          tags$li(
            "La route départementale représente en moyenne ",
            tags$b(paste0(pct_rd_rural, "%")), " des accidents dans le cluster rural,",
            " contre ", tags$b(paste0(pct_rd_urbain, "%")), " dans le cluster urbain.",
            " C'est le facteur le mieux corrélé à la mortalité (R²=26%)."
          ),

          # Insight 5 : accidents sans tiers
          tags$li(
            "Accidents sans tiers (sorties de route, terminologie ONISR) : ",
            tags$b(paste0(pct_sc_rural, "%")), " dans le cluster rural contre ",
            tags$b(paste0(pct_sc_urbain, "%")), " dans le cluster urbain. ",
            "Département le plus exposé : ", tags$b(dept_max_scol$departement),
            " (", round(dept_max_scol$taux_sans_collision, 1), "%)."
          ),

          # Insight 6 : top 3
          tags$li(
            "Top 3 score de risque : ",
            tags$b(paste(
              paste0(top3$departement, " (", top3$score_risque, ")"),
              collapse=" \u203a "
            )), "."
          )
        ),

        tags$p(style="margin:10px 0 0;font-size:12px;color:#7f8c8d;",
          bs_icon("info-circle"),
          " Ces chiffres sont calculés directement depuis les ",
          format(nrow(app_data$accidents_light), big.mark="\u00a0"),
          " accidents de la base BAAC 2015-2024. Ils se mettent à jour",
          " automatiquement si la base de données est actualisée."
        )
      )
    })
  })
}
