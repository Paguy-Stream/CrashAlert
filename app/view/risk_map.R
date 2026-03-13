# app/view/risk_map.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI, p, strong,
        textOutput, renderText, HTML, div, span, h5, selectInput,
        updateSelectInput, observe, reactive, observeEvent, req,
        withProgress, incProgress, showModal, modalDialog, modalButton,
        downloadButton, downloadHandler],
  bslib[layout_columns, card, card_header, card_body],
  bsicons[bs_icon],
  leaflet[leafletOutput, renderLeaflet, leaflet, addProviderTiles,
          providers, setView, addLegend, colorNumeric, addPolygons,
          highlightOptions, labelOptions],
  leaflet.extras[addFullscreenControl],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable, formatStyle, styleInterval, styleEqual],
  dplyr[filter, mutate, arrange, desc, slice_head, case_when,
        group_by, summarise, first, left_join, select, rename, pull, n,
        distinct],
  sf[st_read],
  rmarkdown[render],
)

# Prépare les données département
.normalise_dep <- function(dep) {
  # Supprimer espaces et mettre en majuscule
  d <- trimws(toupper(as.character(dep)))
  # Cas Corse : 201/2A et 202/2B
  d <- dplyr::case_when(
    d %in% c("201", "2A", "20A") ~ "2A",
    d %in% c("202", "2B", "20B") ~ "2B",
    # DOM-TOM : laisser tel quel (971-976, 986, 987, 988)
    nchar(d) >= 3 & grepl("^9[7-8]", d) ~ d,
    # Codes à 3 chiffres finissant par 0 : ex "750"→"75", "130"→"13"
    nchar(d) == 3 & grepl("0$", d) ~ substr(d, 1, 2),
    # Codes à 1 chiffre : ajouter zéro
    nchar(d) == 1 ~ paste0("0", d),
    TRUE ~ d
  )
  d
}

.prepare_dept <- function(agg_dept) {
  agg_dept |>
    dplyr::mutate(dep_clean = .normalise_dep(dep)) |>
    dplyr::group_by(dep_clean) |>
    dplyr::summarise(
      departement    = dplyr::first(departement),
      region         = dplyr::first(region),
      nb_accidents   = sum(nb_accidents),
      nb_mortels     = sum(nb_mortels),
      nb_graves      = sum(nb_graves),
      nb_tues        = sum(nb_tues),
      taux_mortalite = round(sum(nb_mortels) / sum(nb_accidents) * 100, 2),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      classe_gravite = dplyr::case_when(
        taux_mortalite >= 10 ~ "Très élevée",
        taux_mortalite >=  5 ~ "Élevée",
        taux_mortalite >=  2 ~ "Moyenne",
        TRUE                 ~ "Faible"
      ),
      indice_surmortalite = round(taux_mortalite / 5.66 * 100, 1)
    )
}

#' @export
ui_risk_map <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .risk-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        flex-wrap: wrap; gap: 10px;
      }
      .risk-metric-btns { display: flex; gap: 8px; flex-wrap: wrap; }
      .metric-btn {
        border: 2px solid #dee2e6; background: #fff; border-radius: 20px;
        padding: 5px 14px; font-size: 12px; font-weight: 600;
        cursor: pointer; transition: all 0.15s; color: #6c757d;
      }
      .metric-btn.active, .metric-btn:hover {
        border-color: #1b3a6b; background: #1b3a6b; color: #fff;
      }
      .risk-content { padding: 16px; }
      .dept-info-box {
        background: #f8f9fa; border-radius: 8px; padding: 14px;
        border-left: 4px solid #1b3a6b; margin-bottom: 12px;
        font-size: 13px; line-height: 1.8;
      }
    ")),

    # Topbar metriques
    div(class = "risk-topbar",
      div(style="display:flex;align-items:center;gap:10px;margin-bottom:8px;",
        bs_icon("calendar3"),
        tags$span(style="font-size:13px;font-weight:600;color:#1b3a6b;", "Periode :"),
        selectInput(ns("annee_global"), label=NULL,
          choices=c("Toute la periode"="all"), selected="all", width="180px")
      ),
      div(class = "risk-metric-btns",
        tags$button(
          id = ns("m_mortalite"), class = "metric-btn active",
          onclick = sprintf("Shiny.setInputValue('%s','taux_mortalite')", ns("metric")),
          bs_icon("percent"), " Taux mortalite"
        ),
        tags$button(
          id = ns("m_accidents"), class = "metric-btn",
          onclick = sprintf("Shiny.setInputValue('%s','nb_accidents')", ns("metric")),
          bs_icon("exclamation-triangle"), " Volume accidents"
        ),
        tags$button(
          id = ns("m_mortels"), class = "metric-btn",
          onclick = sprintf("Shiny.setInputValue('%s','nb_mortels')", ns("metric")),
          bs_icon("heart-pulse"), " Nb deces"
        ),
        tags$button(
          id = ns("m_tues"), class = "metric-btn",
          onclick = sprintf("Shiny.setInputValue('%s','nb_tues')", ns("metric")),
          bs_icon("person-x"), " Tues (30j)"
        ),
        tags$button(
          id = ns("m_indice"), class = "metric-btn",
          onclick = sprintf("Shiny.setInputValue('%s','indice_surmortalite')", ns("metric")),
          bs_icon("graph-up-arrow"), " Indice surmortalite"
        )
      ),
      div(style = "color:#6c757d;font-size:12px;display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          bs_icon("info-circle"), " Cliquez sur un departement pour le detail",
          downloadButton(ns("dl_csv"), label=" CSV",
            icon = shiny::icon("download"),
            class = "btn btn-sm btn-outline-secondary",
            style = "font-size:11px;padding:3px 9px;"),
          # Sélecteur région + boutons rapport
          div(style="display:flex;align-items:center;gap:8px;margin-left:8px;",
            selectInput(ns("region_rapport"),
              label = NULL,
              choices = c("— Choisir une région —" = ""),
              selected = "",
              width = "220px"
            ),
            selectInput(ns("annee_rapport"),
              label = NULL,
              choices = c("Toute la période" = "all"),
              selected = "all",
              width = "160px"
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
      )
    ),

    # JS boutons actifs
    tags$script(HTML(
      "$(document).on('click', '.metric-btn', function() {
         $('.metric-btn').removeClass('active');
         $(this).addClass('active');
       });"
    )),

    div(class = "risk-content",

      layout_columns(
        col_widths = c(8, 4),

        card(full_screen = TRUE,
          card_header(tagList(bs_icon("map-fill"), " Carte des risques par departement")),
          card_body(padding = 0, leafletOutput(ns("choropleth"), height = "520px"))
        ),

        tags$div(
          card(
            card_header(tagList(bs_icon("geo-alt-fill"), " Departement selectionne")),
            card_body(uiOutput(ns("dept_detail")))
          ),
          tags$div(class = "mt-3"),
          card(
            card_header(tagList(bs_icon("trophy-fill"), " Top 5 — Taux mortalite")),
            card_body(uiOutput(ns("top5")))
          )
        )
      ),

      tags$div(class = "mt-3"),

      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
          card_header(tagList(bs_icon("bar-chart-fill"), " Taux mortalite par region")),
          card_body(plotlyOutput(ns("region_bar"), height = "320px"))
        ),
        card(full_screen = TRUE,
          card_header(tagList(bs_icon("table"), " Classement — metropole")),
          card_body(DTOutput(ns("dept_table")))
        )
      ),

      tags$div(class = "mt-3"),

      card(
        card_header(tagList(bs_icon("globe"), " Outre-mer")),
        card_body(DTOutput(ns("domtom_table")))
      ),

      tags$div(class = "mt-3"),

      card(full_screen = TRUE,
        card_header(tagList(bs_icon("arrow-left-right"), " Comparaison de 2 departements")),
        card_body(
          div(style = "display:flex;gap:16px;flex-wrap:wrap;margin-bottom:12px;",

            div(style = "flex:1;min-width:200px;",
              tags$label("Departement A", style="font-weight:600;font-size:13px;"),
              selectInput(ns("comp_dept_a"), label=NULL, choices=NULL, width="100%")
            ),
            div(style = "flex:1;min-width:200px;",
              tags$label("Departement B", style="font-weight:600;font-size:13px;"),
              selectInput(ns("comp_dept_b"), label=NULL, choices=NULL, width="100%")
            )
          ),
          plotlyOutput(ns("comp_radar"), height = "380px"),
          tags$div(class = "mt-3"),
          uiOutput(ns("comp_summary"))
        )
      )
    )
  )
}

#' @export
server_risk_map <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    # Init filtre année global
    observe({
      ann_vals <- as.character(sort(unique(app_data$accidents_dashboard$annee)))
      annees <- c(c("Toute la periode"="all"), stats::setNames(ann_vals, ann_vals))
      updateSelectInput(session, "annee_global", choices=annees, selected="all")
    })

    dept_data <- reactive({
      ann <- input$annee_global
      d <- if (is.null(ann) || ann == "all") {
        app_data$agg_dept_annee |>
          dplyr::group_by(dep_clean) |>
          dplyr::summarise(
            departement    = dplyr::first(departement),
            region         = dplyr::first(region),
            nb_accidents   = sum(nb_accidents),
            nb_mortels     = sum(nb_mortels),
            nb_graves      = sum(nb_graves),
            nb_tues        = sum(nb_tues),
            taux_mortalite = round(sum(nb_mortels) / sum(nb_accidents) * 100, 2),
            .groups        = "drop"
          )
      } else {
        app_data$agg_dept_annee |>
          dplyr::filter(as.character(annee) == ann)
      }
      d |> dplyr::mutate(
        classe_gravite = dplyr::case_when(
          taux_mortalite >= 10 ~ "Très élevée",
          taux_mortalite >=  5 ~ "Élevée",
          taux_mortalite >=  2 ~ "Moyenne",
          TRUE                 ~ "Faible"
        ),
        indice_surmortalite = round(taux_mortalite / 5.66 * 100, 1)
      )
    })

    # Peupler le sélecteur région
    observe({
      regions <- app_data$accidents_dashboard |>
        filter(!is.na(region)) |>
        distinct(region) |>
        arrange(region) |>
        pull(region) |>
        as.character()
      ann_vals2 <- as.character(sort(unique(app_data$accidents_dashboard$annee)))
      annees2 <- c(c("Toute la periode"="all"), stats::setNames(ann_vals2, ann_vals2))
      shiny::updateSelectInput(session, "annee_rapport", choices=annees2, selected="all")
      ann_vals2 <- as.character(sort(unique(app_data$accidents_dashboard$annee)))
      annees2 <- c(c("Toute la periode"="all"), stats::setNames(ann_vals2, ann_vals2))
      shiny::updateSelectInput(session, "region_rapport",
        choices  = c("— Choisir une région —" = "", regions),
        selected = "")
    })

    # Fonction commune de génération du rapport
    .gen_rapport <- function(format_out) {
      req(input$region_rapport, input$region_rapport != "")
      region_sel <- input$region_rapport
      acc_region <- app_data$accidents_dashboard |>
        filter(as.character(region) == region_sel)
      req(nrow(acc_region) > 0)

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

      withProgress(message = paste("Generation rapport", toupper(ext), "..."),
                   value = 0.2, {
        incProgress(0.3, detail = region_sel)
        rmarkdown::render(
          input         = rmd_tmp,
          output_format = format_out,
          output_file   = out_file,
          params        = list(
            region    = region_sel,
            annee     = input$annee_rapport,
            accidents = if(!is.null(input$annee_rapport) && input$annee_rapport != "all") {
              acc_region |> dplyr::filter(as.character(annee) == input$annee_rapport)
            } else { acc_region }
          ),
          envir         = new.env(parent = globalenv()),
          quiet         = TRUE
        )
        incProgress(0.5)
      })
      out_file
    }

    # Download Rapport HTML
    output$dl_rapport_html <- downloadHandler(
      filename = function() {
        paste0("CrashAlert_", gsub("[^A-Za-z0-9]", "_",
               input$region_rapport), ".html")
      },
      content = function(file) {
        out <- .gen_rapport("html_document")
        file.copy(out, file)
      },
      contentType = "text/html"
    )

    # Download Rapport PDF
    output$dl_rapport_pdf <- downloadHandler(
      filename = function() {
        paste0("CrashAlert_", gsub("[^A-Za-z0-9]", "_",
               input$region_rapport), ".pdf")
      },
      content = function(file) {
        out <- .gen_rapport("pdf_document")
        file.copy(out, file)
      },
      contentType = "application/pdf"
    )

    geo_data <- reactive({
      sf::st_read("data_propre/geo/departements.geojson", quiet = TRUE)
    })

    metric <- reactive({
      if (is.null(input$metric)) "taux_mortalite" else input$metric
    })

    metric_label <- reactive({
      switch(metric(),
        taux_mortalite      = "Taux mortalite (%)",
        nb_accidents        = "Nb accidents",
        nb_mortels          = "Nb mortels",
        nb_tues             = "Tues (30j)",
        indice_surmortalite = "Indice surmortalite (France=100)"
      )
    })

    # Choroplèthe
    output$choropleth <- renderLeaflet({
      d    <- dept_data()
      geo  <- geo_data()
      m    <- metric()

      # Jointure sf + données département sur le code
      geo  <- merge(geo, d, by.x = "code", by.y = "dep_clean", all.x = TRUE)
      vals_num <- suppressWarnings(as.numeric(geo[[m]]))

      # Palette adaptée : indice centré sur 100, autres métriques classiques
      palette_colors <- if (m == "indice_surmortalite") {
        c("#2166ac","#92c5de","#f7f7f7","#f4a582","#d6604d","#b2182b")
      } else {
        c("#d4edda","#fff3cd","#ffd6a5","#f8d7da","#dc3545")
      }
      pal <- colorNumeric(
        palette  = palette_colors,
        domain   = vals_num,
        na.color = "#cccccc"
      )

      leaflet(geo) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(2.2137, 46.5, zoom = 6) |>
        addFullscreenControl() |>
        addPolygons(
          fillColor   = ~pal(vals_num),
          fillOpacity = 0.75,
          color       = "#ffffff",
          weight      = 1,
          label       = ~paste0(
            ifelse(is.na(geo$departement), geo$code, geo$departement),
            " — ", metric_label(), " : ",
            ifelse(is.na(vals_num), "N/A", round(vals_num, 2))
          ),
          highlightOptions = highlightOptions(
            weight = 2, color = "#1b3a6b",
            fillOpacity = 0.9, bringToFront = TRUE
          ),
          layerId = ~code
        ) |>
        addLegend("bottomright",
          pal     = pal,
          values  = vals_num[!is.na(vals_num)],
          title   = metric_label(),
          opacity = 0.9
        )
    })


    # Detail departement au clic
    output$dept_detail <- renderUI({
      click <- input$choropleth_shape_click
      if (is.null(click)) {
        return(div(style = "color:#6c757d;text-align:center;padding:20px;",
                   bs_icon("cursor"), tags$br(), "Cliquez sur un departement"))
      }
      d   <- dept_data()
      row <- d[d$dep_clean == click$id, ]
      if (nrow(row) == 0) return(div("Aucune donnee"))
      # Forcer scalaires pour eviter "length-one character vector"
      nom_dept    <- as.character(row$departement[1])
      nom_region  <- as.character(row$region[1])
      nb_acc      <- as.integer(row$nb_accidents[1])
      nb_mort     <- as.integer(row$nb_mortels[1])
      taux_mort   <- as.numeric(row$taux_mortalite[1])
      classe      <- as.character(row$classe_gravite[1])
      div(class = "dept-info-box",
        div(tags$b(bs_icon("geo-alt-fill"), " ", nom_dept),
            style = "font-size:15px;margin-bottom:8px;"),
        div(bs_icon("map"), " Region : ", tags$b(nom_region)), tags$br(),
        div(bs_icon("exclamation-triangle"), " Accidents : ",
            tags$b(format(nb_acc, big.mark = "\u00a0"))), tags$br(),
        div(bs_icon("heart-pulse"), " Deces : ",
            tags$b(format(nb_mort, big.mark = "\u00a0"))), tags$br(),
        div(bs_icon("percent"), " Taux mortalite : ",
            tags$b(paste0(round(taux_mort, 2), " %"))), tags$br(),
        div(bs_icon("flag-fill"), " Classe : ", tags$b(classe))
      )
    })

    # Top 5
    output$top5 <- renderUI({
      dom_tom <- c("971","972","973","974","975","976","977","978","986","987","988")
      d <- dept_data() |>
        filter(!is.na(taux_mortalite), nb_accidents >= 100,
               !dep_clean %in% dom_tom) |>
        arrange(desc(taux_mortalite)) |>
        slice_head(n = 5)
      couleurs <- c("#dc3545", "#e74c3c", "#f39c12", "#ffc107", "#ffd43b")
      tags$ol(style = "padding-left:20px;margin:0;",
        tagList(lapply(seq_len(nrow(d)), function(i) {
          tags$li(style = "margin-bottom:8px;",
            tags$span(style = paste0("color:", couleurs[i], ";font-weight:700;"),
                      d$departement[i]),
            tags$span(style = "color:#6c757d;font-size:12px;margin-left:6px;",
                      paste0(d$taux_mortalite[i], "% mortalite"))
          )
        }))
      )
    })

    # Bar region
    output$region_bar <- renderPlotly({
      d <- dept_data() |>
        filter(!is.na(taux_mortalite),
               !dep_clean %in% c("971","972","973","974","975",
                                  "976","977","978","986","987","988")) |>
        group_by(region) |>
        summarise(taux_moy = round(mean(taux_mortalite, na.rm = TRUE), 2),
                  .groups = "drop") |>
        arrange(taux_moy)

      plot_ly(d,
        x = ~taux_moy, y = ~region,
        type = "bar", orientation = "h",
        marker = list(
          color = ~taux_moy,
          colorscale = list(c(0, "#d4edda"), c(0.5, "#fff3cd"), c(1, "#dc3545")),
          showscale = FALSE
        ),
        text = ~paste0(taux_moy, "%"), textposition = "outside"
      ) |>
        layout(
          xaxis = list(title = "Taux mortalite moyen (%)"),
          yaxis = list(title = ""),
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          margin = list(t = 5, r = 60, l = 160)
        )
    })

    # Tableau metropole
    output$dept_table <- renderDT({
      d <- dept_data() |>
        filter(!dep_clean %in% c("971","972","973","974","975",
                                  "976","977","978","986","987","988")) |>
        arrange(desc(taux_mortalite)) |>
        select(departement, region, nb_accidents, nb_mortels,
               taux_mortalite, classe_gravite) |>
        rename(Departement = departement, Region = region,
               Accidents = nb_accidents, Deces = nb_mortels,
               `Taux mort.` = taux_mortalite, Classe = classe_gravite)
      datatable(d, rownames = FALSE,
        options = list(pageLength = 10, scrollY = "250px",
                       dom = "ftp", ordering = TRUE),
        class = "table table-hover table-sm") |>
        formatStyle("Taux mort.",
          background = styleInterval(c(5, 8, 12),
            c("#d4edda", "#fff3cd", "#ffd6a5", "#f8d7da")))
    })

    # Tableau DOM-TOM
    output$domtom_table <- renderDT({
      d <- dept_data() |>
        filter(dep_clean %in% c("971","972","973","974","975",
                                 "976","977","978","986","987","988")) |>
        arrange(desc(taux_mortalite)) |>
        dplyr::mutate(
          fiabilite = dplyr::case_when(
            nb_accidents <  30 ~ "⚠️ Très faible effectif",
            nb_accidents < 100 ~ "⚠️ Faible effectif",
            TRUE               ~ "✅ Représentatif"
          )
        ) |>
        select(departement, region, nb_accidents, nb_mortels, taux_mortalite, fiabilite) |>
        rename(Territoire = departement, Zone = region,
               Accidents = nb_accidents, Deces = nb_mortels,
               `Taux mort.` = taux_mortalite, Fiabilite = fiabilite)
      datatable(d, rownames = FALSE,
        options = list(pageLength = 15, dom = "t"),
        class = "table table-hover table-sm") |>
        formatStyle("Taux mort.",
          background = styleInterval(c(5, 8, 12),
            c("#d4edda", "#fff3cd", "#ffd6a5", "#f8d7da"))) |>
        formatStyle("Fiabilite",
          color = styleEqual(
            c("⚠️ Très faible effectif", "⚠️ Faible effectif", "✅ Représentatif"),
            c("#dc3545", "#fd7e14", "#28a745")
          ))
    })


    output$dl_csv <- downloadHandler(
      filename = function() {
        anns <- if (!is.null(input$annees) && length(input$annees) > 0)
                  paste(input$annees, collapse="-") else "tout"
        paste0("crashalert_carte_departements_", anns, ".csv")
      },
      content = function(file) {
        utils::write.csv(dept_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )


    # Comparaison 2 departements — init selects
    observe({
      ann_vals <- as.character(sort(unique(app_data$accidents_dashboard$annee)))
      annees <- c(c("Toute la periode"="all"), stats::setNames(ann_vals, ann_vals))
      updateSelectInput(session, "comp_annee", choices=annees, selected="all")
      d <- dept_data()
      depts <- sort(unique(d$departement))
      depts <- depts[!is.na(depts)]
      updateSelectInput(session, "comp_dept_a", choices=depts, selected=depts[1])
      updateSelectInput(session, "comp_dept_b", choices=depts, selected=depts[2])
    })



    output$comp_radar <- renderPlotly({
      req(input$comp_dept_a, input$comp_dept_b)
      d <- dept_data()
      .row <- function(dept) {
        r <- d[d$departement == dept, ]
        if (nrow(r) == 0) return(NULL)
        list(dept=dept, taux_mortalite=round(r$taux_mortalite[1],2),
             indice=round(r$indice_surmortalite[1],1),
             nb_accidents=r$nb_accidents[1], nb_mortels=r$nb_mortels[1],
             nb_tues=r$nb_tues[1])
      }
      ra <- .row(input$comp_dept_a)
      rb <- .row(input$comp_dept_b)
      req(!is.null(ra), !is.null(rb))
      max_acc  <- max(d$nb_accidents,        na.rm=TRUE)
      max_mort <- max(d$nb_mortels,          na.rm=TRUE)
      max_taux <- max(d$taux_mortalite,      na.rm=TRUE)
      max_tues <- max(d$nb_tues,             na.rm=TRUE)
      max_ind  <- max(d$indice_surmortalite, na.rm=TRUE)
      cats <- c("Taux mortalite","Indice surmortalite","Nb accidents",
                "Nb deces","Tues (30j)","Taux mortalite")
      norm_a <- c(ra$taux_mortalite/max_taux*100, ra$indice/max_ind*100,
                  ra$nb_accidents/max_acc*100, ra$nb_mortels/max_mort*100,
                  ra$nb_tues/max_tues*100, ra$taux_mortalite/max_taux*100)
      norm_b <- c(rb$taux_mortalite/max_taux*100, rb$indice/max_ind*100,
                  rb$nb_accidents/max_acc*100, rb$nb_mortels/max_mort*100,
                  rb$nb_tues/max_tues*100, rb$taux_mortalite/max_taux*100)
      plot_ly(type="scatterpolar", fill="toself") |>
        add_trace(r=norm_a, theta=cats, name=ra$dept, mode="lines",
                  line=list(color="#1b3a6b"), fillcolor="rgba(27,58,107,0.2)") |>
        add_trace(r=norm_b, theta=cats, name=rb$dept, mode="lines",
                  line=list(color="#dc3545"), fillcolor="rgba(220,53,69,0.15)") |>
        layout(
          polar  = list(radialaxis=list(visible=TRUE, range=c(0,100))),
          legend = list(orientation="h", x=0.3, y=-0.1),
          margin = list(t=20, b=40)
        )
    })

    output$comp_summary <- renderUI({
      req(input$comp_dept_a, input$comp_dept_b)
      d <- dept_data()
      ra <- d[d$departement == input$comp_dept_a, ]
      rb <- d[d$departement == input$comp_dept_b, ]
      req(nrow(ra) > 0, nrow(rb) > 0)
      .badge <- function(val, ref) {
        diff <- val - ref
        col  <- if (diff > 0) "#dc3545" else "#28a745"
        span(style=paste0("color:",col,";font-weight:600;"),
             sprintf("%+.1f%%", diff))
      }
      div(style="display:grid;grid-template-columns:1fr 1fr;gap:12px;",
        div(style="background:#f8f9fa;border-radius:8px;padding:12px;",
          tags$h6(style="color:#1b3a6b;font-weight:700;", input$comp_dept_a),
          p(strong("Taux mortalite : "), sprintf("%.2f%%", ra$taux_mortalite[1])),
          p(strong("Indice surmortalite : "), sprintf("%.0f", ra$indice_surmortalite[1]), " (France=100)"),
          p(strong("Nb accidents : "), format(ra$nb_accidents[1], big.mark=" ")),
          p(strong("Deces : "), ra$nb_mortels[1])
        ),
        div(style="background:#f8f9fa;border-radius:8px;padding:12px;",
          tags$h6(style="color:#dc3545;font-weight:700;", input$comp_dept_b),
          p(strong("Taux mortalite : "), sprintf("%.2f%%", rb$taux_mortalite[1]),
            .badge(rb$taux_mortalite[1], ra$taux_mortalite[1])),
          p(strong("Indice surmortalite : "), sprintf("%.0f", rb$indice_surmortalite[1]), " (France=100)"),
          p(strong("Nb accidents : "), format(rb$nb_accidents[1], big.mark=" ")),
          p(strong("Deces : "), rb$nb_mortels[1])
        )
      )
    })
  })
}
