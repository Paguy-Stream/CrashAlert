# app/view/about.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI,
        HTML, div, span, p, h3, h4, strong, em],
  bslib[layout_columns, card, card_header, card_body, navset_card_tab, nav_panel],
  bsicons[bs_icon],
)

#' @export
ui_about <- function(id) {
  ns <- NS(id)

  tagList(

    # ── CSS ──────────────────────────────────────────────────────────────────
    tags$style(HTML("
      .about-wrap { padding: 20px 8px; max-width: 1100px; margin: 0 auto; }

      .about-hero {
        background: linear-gradient(135deg, #1b3a6b 0%, #2c5282 100%);
        border-radius: 12px; padding: 32px 36px; margin-bottom: 24px;
        color: #fff; display: flex; align-items: center; gap: 28px;
        flex-wrap: wrap;
      }
      .about-hero-icon { font-size: 52px; opacity: 0.9; flex-shrink: 0; }
      .about-hero-title {
        font-size: 22px; font-weight: 700; margin: 0 0 6px;
      }
      .about-hero-sub {
        font-size: 14px; opacity: 0.85; margin: 0; line-height: 1.6;
      }
      .about-badge {
        display: inline-flex; align-items: center; gap: 6px;
        background: rgba(255,255,255,0.15); border-radius: 20px;
        padding: 4px 14px; font-size: 12px; font-weight: 600;
        margin: 4px 4px 0 0;
      }

      .about-section-title {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6c757d; margin: 28px 0 14px;
        display: flex; align-items: center; gap: 8px;
      }
      .about-section-title::after {
        content: ''; flex: 1; height: 1px; background: #e9ecef;
      }

      .about-info-box {
        background: #f8f9fa; border-left: 4px solid #4a6fa5;
        border-radius: 6px; padding: 16px 20px;
        font-size: 13px; line-height: 1.85; color: #2c3e50; margin-bottom: 12px;
      }
      .about-warning-box {
        background: #fff8e1; border-left: 4px solid #f39c12;
        border-radius: 6px; padding: 14px 18px;
        font-size: 13px; line-height: 1.8; color: #2c3e50; margin-top: 12px;
      }
      .about-ok-box {
        background: #eafaf1; border-left: 4px solid #27ae60;
        border-radius: 6px; padding: 14px 18px;
        font-size: 13px; line-height: 1.8; color: #2c3e50; margin-top: 12px;
      }
      .about-credit-box {
        background: linear-gradient(135deg, #f8f9ff 0%, #e8edf5 100%);
        border-left: 4px solid #1b3a6b; border-radius: 6px;
        padding: 16px 20px; font-size: 13px; line-height: 1.85; color: #2c3e50;
      }

      .var-table { width: 100%; border-collapse: collapse; font-size: 13px; }
      .var-table th {
        background: #1b3a6b; color: #fff; padding: 8px 12px;
        text-align: left; font-weight: 600; font-size: 12px;
      }
      .var-table td { padding: 8px 12px; border-bottom: 1px solid #e9ecef; vertical-align: top; }
      .var-table tr:nth-child(even) td { background: #f8f9fa; }
      .var-table tr:hover td { background: #eaf2ff; }
      .var-cat {
        display: inline-block; padding: 2px 8px; border-radius: 10px;
        font-size: 11px; font-weight: 700; background: #e9ecef; color: #495057;
      }
      .var-cat.cle    { background: #d4edda; color: #155724; }
      .var-cat.baac   { background: #cce5ff; color: #004085; }
      .var-cat.recode { background: #e2d9f3; color: #4a235a; }
      .var-cat.infra  { background: #fff3cd; color: #856404; }
      .var-cat.grav   { background: #f8d7da; color: #721c24; }
      .var-cat.geo    { background: #d1ecf1; color: #0c5460; }

      .glossaire-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
        gap: 10px; margin-top: 4px;
      }
      .glossaire-item {
        background: #f8f9fa; border-radius: 8px; padding: 12px 14px;
        border: 1px solid #e9ecef;
      }
      .glossaire-term {
        font-weight: 700; color: #1b3a6b; font-size: 13px; margin-bottom: 4px;
      }
      .glossaire-def { font-size: 12px; color: #495057; line-height: 1.6; }

      .onglet-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
        gap: 10px; margin-top: 4px;
      }
      .onglet-card {
        background: #fff; border-radius: 8px; padding: 14px 16px;
        border: 1px solid #dee2e6; transition: box-shadow 0.15s;
      }
      .onglet-card:hover { box-shadow: 0 3px 10px rgba(0,0,0,0.09); }
      .onglet-card-title {
        font-weight: 700; font-size: 13px; color: #1b3a6b; margin-bottom: 5px;
        display: flex; align-items: center; gap: 6px;
      }
      .onglet-card-desc { font-size: 12px; color: #6c757d; line-height: 1.55; }

      .stat-chips { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 6px; }
      .stat-chip {
        background: #e8edf5; border-radius: 20px; padding: 5px 14px;
        font-size: 12px; font-weight: 600; color: #1b3a6b;
        display: inline-flex; align-items: center; gap: 5px;
      }

      .btn-datagouv {
        display: inline-flex; align-items: center; gap: 8px;
        background: #003189; color: #fff !important;
        border-radius: 6px; padding: 10px 20px;
        font-size: 13px; font-weight: 700;
        text-decoration: none !important;
        transition: background 0.15s, box-shadow 0.15s;
        box-shadow: 0 2px 6px rgba(0,49,137,0.25);
      }
      .btn-datagouv:hover {
        background: #001f6b;
        box-shadow: 0 4px 12px rgba(0,49,137,0.35);
      }
    ")),

    div(class = "about-wrap",

      # ── HERO ───────────────────────────────────────────────────────────────
      div(class = "about-hero",
        div(class = "about-hero-icon", bs_icon("car-front-fill")),
        div(
          tags$h1(class = "about-hero-title",
            "CrashAlert — Accidentologie routi\u00e8re en France (2015\u20132024)"),
          tags$p(class = "about-hero-sub",
            "Exploration interactive de 564\u00a0198\u00a0accidents de la route issus de la base ",
            tags$b("BAAC"), " (Bulletin d\u2019Analyse des Accidents Corporels de la circulation). ",
            "Ce tableau de bord est publi\u00e9 sur ", tags$b("data.gouv.fr"),
            " dans le cadre d\u2019une d\u00e9marche open data."),
          div(style = "margin-top: 12px;",
            span(class = "about-badge", bs_icon("calendar3"), " 2015\u20132024"),
            span(class = "about-badge", bs_icon("geo-alt-fill"), " France m\u00e9tropolitaine + DOM-TOM"),
            span(class = "about-badge", bs_icon("database-fill"), " 564\u00a0198 accidents"),
            span(class = "about-badge", bs_icon("people-fill"), " Donn\u00e9es ONISR / data.gouv.fr")
          )
        )
      ),

      # ── ONGLETS NAVIGATION ─────────────────────────────────────────────────
      navset_card_tab(
        id = ns("about_tabs"),

        # ════════════════════════════════════════════════════
        # ONGLET 1 — PRÉSENTATION
        # ════════════════════════════════════════════════════
        nav_panel(
          title = tagList(bs_icon("house-fill"), " Pr\u00e9sentation"),

          card_body(

            div(class = "about-section-title", bs_icon("question-circle-fill"), " Quel est l\u2019objectif de ce projet ?"),

            div(class = "about-info-box",
              tags$b(bs_icon("bullseye"), " En une phrase :"), tags$br(),
              "CrashAlert permet \u00e0 tout citoyen, journaliste, \u00e9lu ou chercheur de comprendre ",
              tags$b("o\u00f9, quand et comment"), " les accidents de la route surviennent en France, ",
              "d\u2019identifier les territoires et profils les plus \u00e0 risque, et de suivre l\u2019\u00e9volution dans le temps.", tags$br(), tags$br(),
              "Les donn\u00e9es proviennent du registre officiel BAAC tenu par l\u2019",
              tags$b("Observatoire National Intermin\u00e9rist\u00e9riel de la S\u00e9curit\u00e9 Routi\u00e8re (ONISR)"),
              " et sont disponibles en open data sur ", tags$b("data.gouv.fr"), "."
            ),

            div(class = "about-section-title", bs_icon("map-fill"), " Ce que contient le tableau de bord"),

            div(class = "onglet-grid",
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("speedometer2"), " Tableau de bord"),
                div(class = "onglet-card-desc",
                    "Vue synth\u00e9tique : indicateurs cl\u00e9s, r\u00e9partition par ann\u00e9e, cartographie rapide.")
              ),
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("map"), " Carte de risque"),
                div(class = "onglet-card-desc",
                    "Score de risque composite par d\u00e9partement. Filtrage par p\u00e9riode et type de route.")
              ),
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("person-fill"), " Profil accidentologique"),
                div(class = "onglet-card-desc",
                    "\u00c9tude des victimes : \u00e2ge, sexe, type de v\u00e9hicule, gravit\u00e9 selon les profils.")
              ),
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("grid-3x3-gap-fill"), " Heatmap"),
                div(class = "onglet-card-desc",
                    "Accidents selon l\u2019heure et le jour de la semaine. Visualisation des pics de sinistralité.")
              ),
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("bar-chart-fill"), " Facteurs de risque"),
                div(class = "onglet-card-desc",
                    "Impact de la lumi\u00e8re, de la météo, de la surface, du type de route sur la gravit\u00e9.")
              ),
              div(class = "onglet-card",
                div(class = "onglet-card-title", bs_icon("graph-up-arrow"), " Analyse avanc\u00e9e"),
                div(class = "onglet-card-desc",
                    "Segmentation territoriale (k-means), score composite fond\u00e9 sur les corr\u00e9lations empiriques, ACM.")
              )
            ),

            div(class = "about-section-title", bs_icon("shield-check"), " P\u00e9rim\u00e8tre des donn\u00e9es"),

            div(class = "about-ok-box",
              tags$b(bs_icon("check-circle-fill"), " Ce qui est couvert :"), tags$br(),
              "\u2022 Tous les accidents ", tags$b("corporels"), " enregistr\u00e9s en France entre 2015 et 2024 ", tags$br(),
              "\u2022 France m\u00e9tropolitaine (96 d\u00e9partements) — les DOM-TOM sont exclus des analyses territoriales", tags$br(),
              "\u2022 Tous les modes de d\u00e9placement : voiture, deux-roues motoris\u00e9s, v\u00e9lo, pi\u00e9tons, poids lourds, tracteurs", tags$br(),
              "\u2022 Toutes les tranches d\u2019\u00e2ge, toutes les p\u00e9riodes (nuit/jour, weekend/semaine, saisons)"
            ),

            div(class = "about-warning-box",
              tags$b(bs_icon("exclamation-triangle-fill"), " Ce qui n\u2019est pas dans la base :"), tags$br(),
              "\u2022 Les ", tags$b("accidents mat\u00e9riels seuls"), " (sans bless\u00e9 ni tu\u00e9) — ils ne sont pas d\u00e9clar\u00e9s au BAAC", tags$br(),
              "\u2022 Les ", tags$b("deux-roues non motoris\u00e9s"), " (v\u00e9los) impliquant uniquement une chute sans tiers n\u2019y figurent qu\u2019en partie", tags$br(),
              "\u2022 Les donn\u00e9es de ", tags$b("trafic"), " (v\u00e9hicule-km parcourus), absentes de la base BAAC — cela \u00e9carte un d\u00e9nominateur relatif qui changerait les taux d\u2019exposition"
            ),

            div(class = "stat-chips",
              span(class = "stat-chip", bs_icon("calendar3"), "10 ann\u00e9es"),
              span(class = "stat-chip", bs_icon("geo-alt"), "96 d\u00e9partements"),
              span(class = "stat-chip", bs_icon("file-earmark-text"), "564\u00a0198 accidents"),
              span(class = "stat-chip", bs_icon("person"), "Toutes victimes"),
              span(class = "stat-chip", bs_icon("car-front"), "Tous modes")
            )
          )
        ),

        # ════════════════════════════════════════════════════
        # ONGLET 2 — MÉTHODOLOGIE
        # ════════════════════════════════════════════════════
        nav_panel(
          title = tagList(bs_icon("gear-fill"), " M\u00e9thodologie"),

          card_body(

            div(class = "about-section-title", bs_icon("database"), " Source des donn\u00e9es"),

            div(class = "about-info-box",
              "Les donn\u00e9es sont issues des fichiers BAAC annuels publi\u00e9s par l\u2019ONISR sur ",
              tags$a(href="https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2022/",
                     target="_blank", "data.gouv.fr"),
              ". Chaque ligne repr\u00e9sente un accident corporel d\u00e9clar\u00e9 par les forces de l\u2019ordre.",
              tags$br(), tags$br(),
              "Les fichiers ont \u00e9t\u00e9 concat\u00e9n\u00e9s, nettoy\u00e9s (harmonisation des codes, recodage des modalit\u00e9s entre versions du formulaire) et enrichis de variables calcul\u00e9es (taux de mortalit\u00e9, tranches d\u2019\u00e2ge standardis\u00e9es, indicateur nuit/jour). ",
              "Le fichier final contient ", tags$b("564\u00a0198 accidents"), " et ", tags$b("37 colonnes"), "."
            ),

            div(class = "about-section-title", bs_icon("calculator"), " Score de risque composite"),

            div(class = "about-info-box",
              tags$b("Formule (version 2, fond\u00e9e sur les corr\u00e9lations empiriques) :"),
              tags$pre(style="background:#fff;border:1px solid #dee2e6;border-radius:6px;padding:12px;font-size:12px;margin:10px 0 6px;",
                "Score = taux_mortalit\u00e9      / max \u00d7 40\n",
                "      + taux_route_dept     / max \u00d7 25\n",
                "      + taux_sans_collision / max \u00d7 20\n",
                "      + (100 - taux_jeunes) / 100 \u00d7 10\n",
                "      + taux_nuit           / max \u00d7  5"
              ),
              "Les pond\u00e9rations refl\u00e8tent les corr\u00e9lations mesur\u00e9es sur les 96 d\u00e9partements m\u00e9tropolitains :", tags$br(),
              "\u2022 ", tags$b("40\u00a0%\u00a0— mortalit\u00e9"), " : indicateur principal, objectif", tags$br(),
              "\u2022 ", tags$b("25\u00a0%\u00a0— route d\u00e9partementale"), " : corr\u00e9lation R\u00b2\u00a0=\u00a026\u00a0% avec la mortalit\u00e9 d\u00e9partementale", tags$br(),
              "\u2022 ", tags$b("20\u00a0%\u00a0— accidents sans tiers"), " : proxy de l\u2019isolement et des sorties de route (R\u00b2\u00a0=\u00a020\u00a0%)", tags$br(),
              "\u2022 ", tags$b("10\u00a0%\u00a0— proportion de seniors"), " : profil 75+ ans = taux de mortalit\u00e9 le plus \u00e9lev\u00e9", tags$br(),
              "\u2022 ", tags$b("5\u00a0%\u00a0— accidents nocturnes"), " : accessibilit\u00e9 des secours", tags$br(),
              "\u2022 ", tags$b("M\u00e9t\u00e9o supprim\u00e9e"), " : R\u00b2\u00a0=\u00a00,3\u00a0% avec la mortalit\u00e9 d\u00e9partementale — non discriminant"
            ),

            div(class = "about-section-title", bs_icon("diagram-3-fill"), " Segmentation territoriale (clustering k-means)"),

            div(class = "about-info-box",
              "L\u2019algorithme k-means regroupe automatiquement les 96 d\u00e9partements en profils similaires, ",
              "sans imposer a priori une d\u00e9finition de \u00ab\u00a0rural\u00a0\u00bb ou \u00ab\u00a0urbain\u00a0\u00bb.", tags$br(), tags$br(),
              "\u2022 Variables utilis\u00e9es : taux de mortalit\u00e9, % nuit, % route d\u00e9partementale, % jeunes conducteurs, ",
              "% accidents sans tiers, % weekend — toutes normalis\u00e9es.", tags$br(),
              "\u2022 Nombre de clusters : k\u00a0=\u00a03 retenu apr\u00e8s validation par score silhouette (k\u00a0=\u00a02 optimal math\u00e9matiquement, ",
              "k\u00a0=\u00a03 retenu pour la richesse interpr\u00e9tative).", tags$br(),
              "\u2022 Initialisation : 25 tirages al\u00e9atoires, seed\u00a042 — r\u00e9sultats stables.",
              tags$br(), tags$br(),
              tags$em(style = "color:#7f8c8d;font-size:12px;",
                bs_icon("info-circle"),
                " Le clustering d\u00e9crit des similarit\u00e9s multidimensionnelles entre territoires. ",
                "Il ne d\u00e9termine pas de hi\u00e9rarchie de causalit\u00e9.")
            ),

            div(class = "about-section-title", bs_icon("diagram-3"), " Analyse des correspondances multiples (ACM)"),

            div(class = "about-info-box",
              "L\u2019ACM est une m\u00e9thode statistique qui visualise les associations entre variables cat\u00e9gorielles. ",
              "Elle r\u00e9pond \u00e0 la question : \u00ab\u00a0quels profils d\u2019usager, de v\u00e9hicule et de contexte ont tendance \u00e0 se retrouver dans les m\u00eames accidents\u00a0?\u00a0\u00bb", tags$br(), tags$br(),
              "\u2022 \u00c9chantillon : 20\u00a0000 accidents stratifi\u00e9s (seed\u00a0=\u00a0123)", tags$br(),
              "\u2022 Variables actives : tranche d\u2019\u00e2ge, sexe, cat\u00e9gorie de v\u00e9hicule, m\u00e9t\u00e9o, luminosit\u00e9, type de route", tags$br(),
              "\u2022 2 axes affich\u00e9s sur 8 calcul\u00e9s (12,8\u00a0% de la variabilit\u00e9 totale — normal en ACM)", tags$br(), tags$br(),
              tags$em(style = "color:#7f8c8d;font-size:12px;",
                bs_icon("exclamation-circle"),
                " L\u2019ACM d\u00e9crit des associations entre profils, pas des niveaux de gravit\u00e9. ",
                "Pour la mortalit\u00e9 par type de v\u00e9hicule ou d\u2019usager, voir les onglets Profil et Facteurs de risque.")
            ),

            div(class = "about-section-title", bs_icon("exclamation-diamond-fill"), " Limites connues"),

            div(class = "about-warning-box",
              tags$b("1. Absence de donn\u00e9es de trafic"), tags$br(),
              "Le score et les taux de mortalit\u00e9 sont calcul\u00e9s sur le nombre d\u2019accidents, pas sur les kilom\u00e8tres parcourus. ",
              "Un d\u00e9partement rural peu circul\u00e9 peut afficher un taux \u00e9lev\u00e9 m\u00e9caniquement. ",
              "L\u2019ONISR corrige cet effet avec les donn\u00e9es v\u00e9hicule-km, non disponibles dans la base BAAC.", tags$br(), tags$br(),
              tags$b("2. D\u00e9claration non exhaustive"), tags$br(),
              "Seuls les accidents d\u00e9clar\u00e9s aux forces de l\u2019ordre sont enregistr\u00e9s. Certains accidents l\u00e9gers, ",
              "notamment impliquant des cyclistes ou des pi\u00e9tons, peuvent \u00eatre sous-repr\u00e9sent\u00e9s.", tags$br(), tags$br(),
              tags$b("3. Changements de codage entre ann\u00e9es"), tags$br(),
              "Le formulaire BAAC a \u00e9volu\u00e9 plusieurs fois entre 2005 et 2024. Des harmonisations ont \u00e9t\u00e9 appliqu\u00e9es, ",
              "mais de l\u00e9g\u00e8res discontinuit\u00e9s restent possibles, notamment pour certaines modalit\u00e9s de type de route.", tags$br(), tags$br(),
              tags$b("4. Les associations ne sont pas des causalit\u00e9s"), tags$br(),
              "Ce tableau de bord d\u00e9crit des corr\u00e9lations et des profils statistiques. ",
              "Il ne permet pas de conclure qu\u2019une variable \u00ab\u00a0cause\u00a0\u00bb les accidents."
            )
          )
        ),

        # ════════════════════════════════════════════════════
        # ONGLET 3 — VARIABLES
        # ════════════════════════════════════════════════════
        nav_panel(
          title = tagList(bs_icon("table"), " Variables"),

          card_body(

            div(class = "about-info-box", style = "margin-bottom: 16px;",
              bs_icon("info-circle"), " ",
              "Les variables utilis\u00e9es dans CrashAlert proviennent de ", tags$b("trois origines distinctes"), " :", tags$br(),
              tags$b("\u2022 Variables source BAAC"), " : champs bruts tels que fournis par l\u2019ONISR.", tags$br(),
              tags$b("\u2022 Variables recodées / construites"), " : d\u00e9riv\u00e9es du BAAC lors du traitement (ex\u00a0: ",
              tags$code("jour_semaine"), " calcul\u00e9 depuis la date, ", tags$code("tranche_age"), " depuis l\u2019\u00e2ge).", tags$br(),
              tags$b("\u2022 Variables d\u2019environnement et d\u2019infrastructure"), " : issues du BAAC mais d\u00e9crivant le contexte routier (type de route, \u00e9clairage, m\u00e9t\u00e9o, chaussée).", tags$br(), tags$br(),
              "Pour la documentation compl\u00e8te du formulaire BAAC source, voir le ",
              tags$a(href="https://www.onisr.securite-routiere.gouv.fr/outils-de-connaissance/bases-de-donnees-sur-les-accidents",
                     target="_blank", "site de l\u2019ONISR"), "."
            ),

            # ── Légende des badges ─────────────────────────────────────────
            div(style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:16px;font-size:12px;",
              span(class="var-cat cle",    "Cl\u00e9 / Identifiant"),
              span(class="var-cat baac",   "Source BAAC"),
              span(class="var-cat recode", "Recodée / construite"),
              span(class="var-cat infra",  "Environnement & infrastructure"),
              span(class="var-cat grav",   "Gravit\u00e9 & victimes"),
              span(class="var-cat geo",    "G\u00e9ographique")
            ),

            # ── SECTION 1 : Identification & clés ─────────────────────────
            div(class = "about-section-title", bs_icon("key-fill"), " Identification & p\u00e9riode"),
            tags$div(style = "overflow-x: auto;",
              tags$table(class = "var-table",
                tags$thead(tags$tr(
                  tags$th("Variable"), tags$th("Libell\u00e9"), tags$th("Type"), tags$th("Description")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("num_acc")),
                    tags$td("Num\u00e9ro d\u2019accident"),
                    tags$td(span(class="var-cat cle", "Cl\u00e9")),
                    tags$td("Identifiant unique de l\u2019accident dans la base BAAC.")),
                  tags$tr(tags$td(tags$code("an")),
                    tags$td("Ann\u00e9e"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Ann\u00e9e de survenue (2015\u20132024).")),
                  tags$tr(tags$td(tags$code("mois")),
                    tags$td("Mois"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Mois de l\u2019accident (1 = janvier, 12 = d\u00e9cembre).")),
                  tags$tr(tags$td(tags$code("heure")),
                    tags$td("Heure"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Heure de l\u2019accident (0\u201323).")),
                  tags$tr(tags$td(tags$code("annee")),
                    tags$td("Ann\u00e9e (format\u00e9e)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Ann\u00e9e en format charact\u00e8re pour les graphiques temporels.")),
                  tags$tr(tags$td(tags$code("jour_semaine")),
                    tags$td("Jour de la semaine"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Lundi \u00e0 dimanche — construit depuis la date compl\u00e8te.")),
                  tags$tr(tags$td(tags$code("moment_journee")),
                    tags$td("Moment de la journ\u00e9e"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Matin / Apr\u00e8s-midi / Soir / Nuit — construit depuis l\u2019heure.")),
                  tags$tr(tags$td(tags$code("saison")),
                    tags$td("Saison"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Printemps / \u00c9t\u00e9 / Automne / Hiver — construit depuis le mois."))
                )
              )
            ),

            # ── SECTION 2 : Géographie ────────────────────────────────────
            div(class = "about-section-title", bs_icon("geo-alt-fill"), " G\u00e9ographie"),
            tags$div(style = "overflow-x: auto;",
              tags$table(class = "var-table",
                tags$thead(tags$tr(
                  tags$th("Variable"), tags$th("Libell\u00e9"), tags$th("Type"), tags$th("Description")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("dep")),
                    tags$td("Code d\u00e9partement"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code officiel INSEE du d\u00e9partement (01, 2A, 75, 971\u2026). N\u00e9cessite un nettoyage : zéro-padding et cas 2A/2B.")),
                  tags$tr(tags$td(tags$code("departement")),
                    tags$td("Nom du d\u00e9partement"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Libell\u00e9 complet du d\u00e9partement — joint depuis un r\u00e9f\u00e9rentiel INSEE.")),
                  tags$tr(tags$td(tags$code("region")),
                    tags$td("R\u00e9gion"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("R\u00e9gion administrative (r\u00e9forme 2016) — joint depuis un r\u00e9f\u00e9rentiel INSEE.")),
                  tags$tr(tags$td(tags$code("lat")),
                    tags$td("Latitude"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Coordonn\u00e9e GPS WGS84. Disponible depuis 2019 ; absent ou impr\u00e9cis avant.")),
                  tags$tr(tags$td(tags$code("long")),
                    tags$td("Longitude"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Coordonn\u00e9e GPS WGS84. Disponible depuis 2019 ; absent ou impr\u00e9cis avant."))
                )
              )
            ),

            # ── SECTION 3 : Gravité & victimes ───────────────────────────
            div(class = "about-section-title", bs_icon("heart-pulse-fill"), " Gravit\u00e9 & victimes"),
            tags$div(style = "overflow-x: auto;",
              tags$table(class = "var-table",
                tags$thead(tags$tr(
                  tags$th("Variable"), tags$th("Libell\u00e9"), tags$th("Type"), tags$th("Description")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("grav")),
                    tags$td("Gravit\u00e9 individuelle (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut BAAC : 1=Indemne, 2=Tu\u00e9, 3=Bless\u00e9 hospitalis\u00e9, 4=Bless\u00e9 l\u00e9ger.")),
                  tags$tr(tags$td(tags$code("grav_label")),
                    tags$td("Gravit\u00e9 individuelle (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Version libell\u00e9e de grav : Tu\u00e9 / Bless\u00e9 hospitalis\u00e9 / Bless\u00e9 l\u00e9ger / Indemne.")),
                  tags$tr(tags$td(tags$code("gravite_accident")),
                    tags$td("Gravit\u00e9 de l\u2019accident"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Gravit\u00e9 la plus \u00e9lev\u00e9e parmi toutes les victimes de l\u2019accident : Mortel / Bless\u00e9 hospitalis\u00e9 / Bless\u00e9 l\u00e9ger.")),
                  tags$tr(tags$td(tags$code("nb_tues")),
                    tags$td("Nombre de tu\u00e9s"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Compt\u00e9 par accident depuis les enregistrements usagers (d\u00e9c\u00e8s dans les 30 jours).")),
                  tags$tr(tags$td(tags$code("nb_blesses_hospitalises")),
                    tags$td("Bless\u00e9s hospitalis\u00e9s"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Compt\u00e9 par accident depuis les enregistrements usagers.")),
                  tags$tr(tags$td(tags$code("nb_victimes")),
                    tags$td("Nombre total de victimes"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Somme tu\u00e9s + bless\u00e9s hospitalis\u00e9s + bless\u00e9s l\u00e9gers.")),
                  tags$tr(tags$td(tags$code("accident_mortel")),
                    tags$td("Accident mortel"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Indicateur binaire (TRUE/FALSE) : TRUE si nb_tues \u2265 1."))
                )
              )
            ),

            # ── SECTION 4 : Profil usager & véhicule ─────────────────────
            div(class = "about-section-title", bs_icon("person-fill"), " Profil usager & v\u00e9hicule"),
            tags$div(style = "overflow-x: auto;",
              tags$table(class = "var-table",
                tags$thead(tags$tr(
                  tags$th("Variable"), tags$th("Libell\u00e9"), tags$th("Type"), tags$th("Description")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("age")),
                    tags$td("\u00c2ge (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("\u00c2ge en ann\u00e9es de l\u2019usager au moment de l\u2019accident.")),
                  tags$tr(tags$td(tags$code("tranche_age")),
                    tags$td("Tranche d\u2019\u00e2ge"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("0\u201317 / 18\u201324 / 25\u201334 / 35\u201344 / 45\u201354 / 55\u201364 / 65\u201374 / 75+ — construit depuis l\u2019\u00e2ge.")),
                  tags$tr(tags$td(tags$code("sexe")),
                    tags$td("Sexe (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut BAAC : 1=Masculin, 2=F\u00e9minin.")),
                  tags$tr(tags$td(tags$code("sexe_dominant")),
                    tags$td("Sexe dominant par accident"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Sexe majoritaire parmi les usagers de l\u2019accident : Homme / Femme / Mixte.")),
                  tags$tr(tags$td(tags$code("catv")),
                    tags$td("Cat\u00e9gorie v\u00e9hicule (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut BAAC (01=VTT, 02=cyclomoteur, 07=VL, 10=PL, 30=pi\u00e9ton\u2026).")),
                  tags$tr(tags$td(tags$code("categorie_vehicule")),
                    tags$td("Cat\u00e9gorie v\u00e9hicule (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("V\u00e9hicule l\u00e9ger / Deux-roues motoris\u00e9s / V\u00e9lo / Pi\u00e9ton / Poids lourd / Tracteur agricole / Autre.")),
                  tags$tr(tags$td(tags$code("catu")),
                    tags$td("Cat\u00e9gorie usager (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("1=Conducteur, 2=Passager, 3=Pi\u00e9ton.")),
                  tags$tr(tags$td(tags$code("catu_principal")),
                    tags$td("R\u00f4le usager principal"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Conducteur / Passager / Pi\u00e9ton — libell\u00e9 du catu du premier usager enregistr\u00e9."))
                )
              )
            ),

            # ── SECTION 5 : Environnement & infrastructure ────────────────
            div(class = "about-section-title", bs_icon("signpost-fill"), " Environnement & infrastructure"),
            tags$div(style = "overflow-x: auto;",
              tags$table(class = "var-table",
                tags$thead(tags$tr(
                  tags$th("Variable"), tags$th("Libell\u00e9"), tags$th("Type"), tags$th("Description")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$code("lum")),
                    tags$td("Luminosit\u00e9 (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut : 1=Plein jour, 2=Cr\u00e9puscule/aube, 3=Nuit sans \u00e9clairage, 4=Nuit avec \u00e9clairage non allum\u00e9, 5=Nuit avec \u00e9clairage allum\u00e9.")),
                  tags$tr(tags$td(tags$code("lum_label")),
                    tags$td("Luminosit\u00e9 (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Version libell\u00e9e de lum, regroup\u00e9e en 3 modalit\u00e9s pour l\u2019ACM : Jour / Nuit avec \u00e9clairage / Nuit sans \u00e9clairage.")),
                  tags$tr(tags$td(tags$code("atm")),
                    tags$td("Conditions atmosph\u00e9riques (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut : 1=Normale, 2=Pluie l\u00e9g\u00e8re, 3=Pluie forte, 4=Neige/gr\u00eale, 5=Brouillard, 6=Vent fort\u2026")),
                  tags$tr(tags$td(tags$code("atm_label")),
                    tags$td("Conditions m\u00e9t\u00e9o (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Normale / Perturb\u00e9e (toutes conditions d\u00e9grad\u00e9es regroup\u00e9es).")),
                  tags$tr(tags$td(tags$code("catr")),
                    tags$td("Cat\u00e9gorie de route (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut : 1=Autoroute, 2=Nationale, 3=D\u00e9partementale, 4=Communale, 5=Hors r\u00e9seau\u2026")),
                  tags$tr(tags$td(tags$code("catr_label")),
                    tags$td("Type de route (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Autoroute / Route nationale / Route d\u00e9partementale / Voie communale / Hors r\u00e9seau.")),
                  tags$tr(tags$td(tags$code("col")),
                    tags$td("Type de collision (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut : 1=Frontale, 2=Arri\u00e8re, 3=C\u00f4t\u00e9, 6=Sans collision (sortie de route)\u2026")),
                  tags$tr(tags$td(tags$code("col_label")),
                    tags$td("Type de collision (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Frontale / Arri\u00e8re / C\u00f4t\u00e9 / Encha\u00een\u00e9e / Sans collision.")),
                  tags$tr(tags$td(tags$code("surf")),
                    tags$td("\u00c9tat de la chauss\u00e9e (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Code brut : 1=Normale, 2=Mouill\u00e9e, 3=Flaques, 4=Enneig\u00e9e, 5=Verglas\u00e9e\u2026")),
                  tags$tr(tags$td(tags$code("surf_label")),
                    tags$td("\u00c9tat de la chauss\u00e9e (libell\u00e9)"),
                    tags$td(span(class="var-cat recode", "Recodée")),
                    tags$td("Normale / Mouill\u00e9e / Flaques / Enneig\u00e9e / Verglas\u00e9e / Corps gras / Autre.")),
                  tags$tr(tags$td(tags$code("vma")),
                    tags$td("Vitesse max. autoris\u00e9e"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Limite l\u00e9gale de vitesse au point de l\u2019accident (km/h). Pr\u00e9sent depuis 2019.")),
                  tags$tr(tags$td(tags$code("infra")),
                    tags$td("Localisation sp\u00e9ciale (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Tunnel / Pont / Giratoire / Voie ferrée / \u00c9changeur / Zone piétonne\u2026")),
                  tags$tr(tags$td(tags$code("manv")),
                    tags$td("Manœuvre principale (BAAC)"),
                    tags$td(span(class="var-cat baac", "Source BAAC")),
                    tags$td("Derni\u00e8re manœuvre avant l\u2019accident : tourne \u00e0 gauche, d\u00e9passement, marche arri\u00e8re\u2026"))
                )
              )
            )
          )
        ),

        # ════════════════════════════════════════════════════
        # ONGLET 4 — GLOSSAIRE
        # ════════════════════════════════════════════════════
        nav_panel(
          title = tagList(bs_icon("book-fill"), " Glossaire"),

          card_body(

            div(class = "about-info-box", style = "margin-bottom: 16px;",
              bs_icon("lightbulb-fill"), " ",
              "Ce glossaire explique les termes techniques utilis\u00e9s dans CrashAlert ",
              "pour les lecteurs non sp\u00e9cialistes."
            ),

            div(class = "about-section-title", bs_icon("bar-chart"), " Statistiques & indicateurs"),
            div(class = "glossaire-grid",
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Taux de mortalit\u00e9"),
                div(class = "glossaire-def",
                    "Proportion d\u2019accidents ayant caus\u00e9 au moins un d\u00e9c\u00e8s. ",
                    "Exemple\u00a0: un taux de 8\u00a0% signifie que 8 accidents sur 100 ont \u00e9t\u00e9 mortels.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Score de risque composite"),
                div(class = "glossaire-def",
                    "Indicateur synth\u00e9tique (0\u2013100) combinant plusieurs facteurs pond\u00e9r\u00e9s par leur corr\u00e9lation avec la mortalit\u00e9. ",
                    "Un score de 80 ne signifie pas 80\u00a0% de risque d\u2019accident, mais que ce d\u00e9partement est ",
                    "parmi les plus expos\u00e9s de la base.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Corr\u00e9lation (R\u00b2)"),
                div(class = "glossaire-def",
                    "Mesure de la force du lien statistique entre deux variables. ",
                    "R\u00b2\u00a0=\u00a026\u00a0% signifie qu\u2019une variable explique 26\u00a0% de la variation d\u2019une autre. ",
                    "Une corr\u00e9lation n\u2019implique pas n\u00e9cessairement une causalit\u00e9.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Quartile"),
                div(class = "glossaire-def",
                    "Division d\u2019une distribution en 4 groupes \u00e9gaux. Q1 = 25\u00a0% des valeurs les plus basses, ",
                    "Q4 = 25\u00a0% des valeurs les plus \u00e9lev\u00e9es."))
            ),

            div(class = "about-section-title", bs_icon("diagram-3"), " Algorithmes & m\u00e9thodes"),
            div(class = "glossaire-grid",
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Clustering k-means"),
                div(class = "glossaire-def",
                    "Algorithme qui regroupe automatiquement des observations similaires en k groupes (",
                    "\u00ab clusters \u00bb). Ici, les 96 d\u00e9partements sont regroup\u00e9s en 3 profils selon 6 indicateurs de risque.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Score silhouette"),
                div(class = "glossaire-def",
                    "Indicateur de qualit\u00e9 du clustering (0 \u00e0 1). Plus il est proche de 1, plus les groupes sont ",
                    "bien s\u00e9par\u00e9s et coh\u00e9rents en interne.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "ACM (Analyse des Correspondances Multiples)"),
                div(class = "glossaire-def",
                    "M\u00e9thode statistique qui visualise les associations entre variables cat\u00e9gorielles sur un graphique 2D (biplot). ",
                    "Deux points proches = deux situations souvent observ\u00e9es ensemble.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Biplot"),
                div(class = "glossaire-def",
                    "Graphique produit par l\u2019ACM. Chaque point repr\u00e9sente une modalit\u00e9 (ex\u00a0: \u00ab\u00a0Nuit sans \u00e9clairage\u00a0\u00bb). ",
                    "La distance au centre indique \u00e0 quel point ce profil est atypique.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Loess (courbe de tendance)"),
                div(class = "glossaire-def",
                    "Courbe de r\u00e9gression locale qui r\u00e9sume la tendance g\u00e9n\u00e9rale d\u2019un nuage de points ",
                    "sans imposer de forme lin\u00e9aire. Utilis\u00e9e dans la section analyse avanc\u00e9e.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Normalisation (scale)"),
                div(class = "glossaire-def",
                    "Transformation qui centre et r\u00e9duit des variables sur une m\u00eame \u00e9chelle, ",
                    "afin qu\u2019aucun indicateur ne domine l\u2019algorithme de clustering simplement parce qu\u2019il a des valeurs plus grandes."))
            ),

            div(class = "about-section-title", bs_icon("sign-stop"), " Termes routiers & BAAC"),
            div(class = "glossaire-grid",
              div(class = "glossaire-item",
                div(class = "glossaire-term", "BAAC"),
                div(class = "glossaire-def",
                    "Bulletin d\u2019Analyse des Accidents Corporels. Formulaire rempli par les forces de l\u2019ordre pour chaque accident. ",
                    "Aliment\u00e9 depuis 1970, disponible en open data depuis 2005.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "ONISR"),
                div(class = "glossaire-def",
                    "Observatoire National Intermin\u00e9rist\u00e9riel de la S\u00e9curit\u00e9 Routi\u00e8re. Organisme officiel charg\u00e9 de collecter, ",
                    "analyser et publier les statistiques d\u2019accidents en France.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Accident corporel"),
                div(class = "glossaire-def",
                    "Accident impliquant au moins une victime (tu\u00e9, bless\u00e9 hospitalis\u00e9 ou bless\u00e9 l\u00e9ger). ",
                    "Les accidents mat\u00e9riels seuls ne sont pas dans la base BAAC.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Tu\u00e9 \u00e0 30 jours"),
                div(class = "glossaire-def",
                    "D\u00e9finition officielle : personne d\u00e9c\u00e9d\u00e9e dans les 30 jours suivant l\u2019accident. ",
                    "Adopt\u00e9e en 2005 pour harmoniser les statistiques europ\u00e9ennes (anciennement 6 jours en France).")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "Accident sans collision"),
                div(class = "glossaire-def",
                    "Accident impliquant un seul v\u00e9hicule, sans impact avec un autre usager. ",
                    "Souvent une sortie de route. Proxy de l\u2019isolement du r\u00e9seau routier.")),
              div(class = "glossaire-item",
                div(class = "glossaire-term", "v\u00e9hicule-km"),
                div(class = "glossaire-def",
                    "Indicateur de trafic (distance parcourue × nombre de v\u00e9hicules). Utilis\u00e9 par l\u2019ONISR pour calculer ",
                    "des taux d\u2019accidents par kilom\u00e8tre parcouru. Non disponible dans le BAAC public."))
            )
          )
        ),

        # ════════════════════════════════════════════════════
        # ONGLET 5 — CRÉDITS
        # ════════════════════════════════════════════════════
        nav_panel(
          title = tagList(bs_icon("person-fill"), " Cr\u00e9dits"),

          card_body(

            div(class = "about-section-title", bs_icon("person-badge-fill"), " Auteur"),
            div(class = "about-credit-box",
              tags$b(bs_icon("person-circle"), " Emmanuel Paguiel Bouendo"),
              tags$br(),
              "Projet personnel d\u00e9velopp\u00e9 dans le cadre d\u2019une formation data analyst.", tags$br(),
              "Tableau de bord construit avec R, Shiny, bslib et rhino.", tags$br(), tags$br(),
              tags$b(bs_icon("envelope-fill"), " Contact :"), tags$br(),
              tags$a(href = "mailto:emmanuelpaguiel@gmail.com",
                     bs_icon("envelope"), " emmanuelpaguiel@gmail.com"), tags$br(),
              tags$a(href = "https://github.com/Paguy-Stream", target = "_blank",
                     bs_icon("github"), " github.com/Paguy-Stream")
            ),

            div(class = "about-section-title", bs_icon("file-earmark-check-fill"), " Sources de donn\u00e9es"),
            div(class = "about-info-box",
              tags$b(bs_icon("database"), " Base BAAC (2015\u20132024)"), tags$br(),
              "Publi\u00e9e par le Minist\u00e8re de l\u2019Int\u00e9rieur / ONISR sous ",
              tags$b("Licence Ouverte Etalab v2.0."), tags$br(),
              "La base source officielle est t\u00e9l\u00e9chargeable directement depuis data.gouv.fr \u2014 ",
              "c\u2019est la r\u00e9f\u00e9rence pour tout usage officiel ou acad\u00e9mique.", tags$br(), tags$br(),
              tags$a(
                href   = "https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2022/",
                target = "_blank",
                class  = "btn-datagouv",
                bs_icon("box-arrow-up-right"), " T\u00e9l\u00e9charger la base BAAC sur data.gouv.fr"
              ),
              tags$br(), tags$br(),
              tags$b(bs_icon("info-circle-fill"), " Pourquoi un lien externe et pas un t\u00e9l\u00e9chargement direct ?"), tags$br(),
              "La base compl\u00e8te p\u00e8se environ 50\u00a0Mo. La proposer depuis cette application ",
              "surchargerait le serveur et cr\u00e9erait un doublon avec la source officielle. ",
              "data.gouv.fr garantit la version \u00e0 jour, la tra\u00e7abilit\u00e9 et la licence officielle.", tags$br(), tags$br(),
              tags$b(bs_icon("geo-alt"), " Fonds de carte d\u00e9partementaux"), tags$br(),
              "IGN / Geofla \u2014 disponibles en open data sur ",
              tags$a(href = "https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/",
                     target = "_blank", "data.gouv.fr"), ".", tags$br(), tags$br(),
              tags$em(style = "color:#7f8c8d;font-size:12px;",
                bs_icon("clock"),
                " Les donn\u00e9es BAAC sont mises \u00e0 jour annuellement par l\u2019ONISR. ",
                "Derni\u00e8re ann\u00e9e int\u00e9gr\u00e9e dans cette version : 2024.")
            ),

            div(class = "about-section-title", bs_icon("tools"), " Stack technique"),
            div(class = "about-info-box",
              div(class = "stat-chips",
                span(class = "stat-chip", bs_icon("code-slash"), " R 4.3+"),
                span(class = "stat-chip", bs_icon("layout-wtf"), " Shiny"),
                span(class = "stat-chip", bs_icon("columns-gap"), " bslib"),
                span(class = "stat-chip", bs_icon("box-seam"), " rhino"),
                span(class = "stat-chip", bs_icon("graph-up"), " plotly"),
                span(class = "stat-chip", bs_icon("map"), " leaflet"),
                span(class = "stat-chip", bs_icon("table"), " DT"),
                span(class = "stat-chip", bs_icon("calculator"), " FactoMineR"),
                span(class = "stat-chip", bs_icon("diagram-3"), " cluster")
              ),
              tags$br(),
              tags$em(style = "color:#7f8c8d;font-size:12px;",
                "D\u00e9pendances g\u00e9r\u00e9es via renv. Code source disponible sur demande.")
            ),

            div(class = "about-section-title", bs_icon("shield-lock-fill"), " Licence & r\u00e9utilisation"),
            div(class = "about-ok-box",
              tags$b(bs_icon("check-circle"), " Les donn\u00e9es sources (BAAC) sont publi\u00e9es sous Licence Ouverte Etalab v2.0."), tags$br(),
              "Vous pouvez les r\u00e9utiliser librement, y compris \u00e0 des fins commerciales, ",
              "sous r\u00e9serve de mentionner la source des donn\u00e9es (",
              tags$b("ONISR / Minist\u00e8re de l\u2019Int\u00e9rieur"), " via data.gouv.fr) et la date de t\u00e9l\u00e9chargement.", tags$br(), tags$br(),
              tags$b(bs_icon("exclamation-circle"), " Ce tableau de bord"), " est une r\u00e9alisation personnelle de ",
              tags$b("Emmanuel Paguiel Bouendo"), ". Il ne constitue pas une source officielle et ",
              "ne saurait engager la responsabilit\u00e9 de son auteur. ",
              "Pour des usages officiels ou r\u00e9glementaires, se r\u00e9f\u00e9rer directement aux publications de l\u2019ONISR."
            ),

            div(class = "about-section-title", bs_icon("clock-history"), " Versions"),
            div(class = "about-info-box",
              tags$b("v4.0"), " — Mars 2026 — Refactorisation compl\u00e8te vers architecture rhino + bslib, ",
              "analyse avanc\u00e9e (clustering, ACM, \u00e9volution temporelle), insights calcul\u00e9s dynamiquement.", tags$br(),
              tags$b("v3.x"), " — 2025 — Architecture bs4Dash monolithique.", tags$br(),
              tags$b("v1\u20132"), " — 2024 — Prototypes exploratoires."
            )

          )
        )

      ) # navset_card_tab
    ) # about-wrap
  )
}

#' @export
server_about <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    # Pas de calculs serveur nécessaires pour cet onglet statique
  })
}
