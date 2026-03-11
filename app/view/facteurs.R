# app/view/facteurs.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI,
        textOutput, renderText, HTML, div, span, p, selectInput,
        updateSelectInput, observe, reactive, req],
  bslib[layout_columns, card, card_header, card_body, value_box],
  bsicons[bs_icon],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, add_bars],
  dplyr[filter, mutate, group_by, summarise, n, arrange, desc,
        slice_head, ungroup, pull, case_when],
)

# Helper : calcule taux mortalite + volume par variable
.taux_var <- function(d, col, min_n = NULL) {
  if (is.null(min_n)) min_n <- max(5, round(nrow(d) * 0.03))
  d |>
    filter(!is.na(.data[[col]]),
           as.character(.data[[col]]) != "Non renseign\u00e9") |>
    mutate(val = as.character(.data[[col]])) |>
    group_by(val) |>
    summarise(
      n      = n(),
      mortels = sum(gravite_accident == "Mortel", na.rm = TRUE),
      taux   = round(mortels / n * 100, 2),
      .groups = "drop"
    ) |>
    filter(n >= min_n) |>
    arrange(desc(taux))
}

# Palette gravite
.COULEURS <- c("Mortel" = "#e74c3c", "Grave" = "#f39c12", "L\u00e9ger" = "#27ae60")

#' @export
ui_facteurs <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .fac-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        flex-wrap: wrap; gap: 10px;
      }
      .fac-filters { display: flex; align-items: center; gap: 16px; flex-wrap: wrap; }
      .fac-label { font-size: 12px; font-weight: 600; color: #6c757d;
                   margin: 0; white-space: nowrap; }
      .fac-content { padding: 16px; }
      .section-title {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6c757d; margin: 20px 0 12px;
        display: flex; align-items: center; gap: 8px;
      }
      .section-title::after { content: ''; flex: 1; height: 1px; background: #e9ecef; }
      .insight-box {
        background: linear-gradient(135deg, #f8f9ff 0%, #e8edf5 100%);
        border-left: 4px solid #1b3a6b; border-radius: 6px;
        padding: 12px 16px; font-size: 13px; line-height: 1.7; color: #2c3e50;
      }
      .risk-badge {
        display: inline-block; padding: 2px 10px; border-radius: 12px;
        font-size: 12px; font-weight: 700; color: #fff;
      }
    ")),

    div(class = "fac-topbar",
      div(class = "fac-filters",
        div(tags$label(class="fac-label", bs_icon("calendar3"), " Annees"),
            selectInput(ns("annees"), label=NULL, choices=NULL,
                        multiple=TRUE, width="160px")),
        div(tags$label(class="fac-label", bs_icon("geo-alt"), " Region"),
            selectInput(ns("regions"), label=NULL, choices=NULL,
                        multiple=TRUE, width="180px")),
        div(tags$label(class="fac-label", bs_icon("signpost-split"), " Département"),
            selectInput(ns("deps"), label=NULL, choices=NULL,
                        multiple=TRUE, width="180px")),
        div(tags$label(class="fac-label", bs_icon("signpost-split"), " Type route"),
            selectInput(ns("type_route"), label=NULL, choices=NULL,
                        multiple=TRUE, width="200px"))
      ),
      div(style="color:#6c757d;font-size:12px;",
          uiOutput(ns("data_summary"), inline=TRUE))
    ),

    div(class = "fac-content",

      # ── KPI facteurs ────────────────────────────────────────────────────────
      div(class="section-title", bs_icon("exclamation-triangle-fill"), " Facteurs les plus meurtriers"),
      layout_columns(
        col_widths=c(3,3,3,3), fill=FALSE,
        value_box("Meteo la + meurtriere",
          textOutput(ns("kpi_meteo"), inline=TRUE),
          showcase=bs_icon("cloud-lightning-rain-fill"), theme="danger",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_meteo_sub"), inline=TRUE))),
        value_box("Route la + meurtriere",
          textOutput(ns("kpi_route"), inline=TRUE),
          showcase=bs_icon("signpost-fill"), theme="danger",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_route_sub"), inline=TRUE))),
        value_box("Luminosite la + risquee",
          textOutput(ns("kpi_lum"), inline=TRUE),
          showcase=bs_icon("moon-fill"), theme="warning",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_lum_sub"), inline=TRUE))),
        value_box("Collision la + meurtriere",
          textOutput(ns("kpi_col"), inline=TRUE),
          showcase=bs_icon("car-front-fill"), theme="warning",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_col_sub"), inline=TRUE)))
      ),

      # ── Meteo & Luminosite ─────────────────────────────────────────────────
      div(class="section-title", bs_icon("cloud-rain-fill"), " Conditions environnementales"),
      layout_columns(
        col_widths=c(4,4,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("cloud-fill"), " Meteo — Taux mortalite")),
          card_body(plotlyOutput(ns("meteo_taux"), height="300px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("brightness-high-fill"), " Luminosite — Taux mortalite")),
          card_body(plotlyOutput(ns("lum_taux"), height="300px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("droplet-fill"), " Etat chaussee — Taux mortalite")),
          card_body(plotlyOutput(ns("surf_taux"), height="300px"))
        )
      ),

      tags$div(class="mt-3"),

      # ── Type route & Collision ─────────────────────────────────────────────
      div(class="section-title", bs_icon("signpost-split-fill"), " Infrastructure & Collision"),
      layout_columns(
        col_widths=c(5,7),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("signpost-fill"), " Type de route × gravite")),
          card_body(plotlyOutput(ns("route_gravite"), height="320px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("car-front-fill"), " Type de collision × gravite")),
          card_body(plotlyOutput(ns("collision_gravite"), height="320px"))
        )
      ),

      tags$div(class="mt-3"),

      # ── Combinaisons risque ────────────────────────────────────────────────
      div(class="section-title", bs_icon("layer-forward"), " Combinaisons a haut risque"),
      layout_columns(
        col_widths=c(6,6),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("moon-stars-fill"),
                              " Nuit × etat chaussee — Taux mortalite")),
          card_body(plotlyOutput(ns("nuit_surf"), height="280px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("signpost-2-fill"),
                              " Type route × meteo — Taux mortalite")),
          card_body(plotlyOutput(ns("route_meteo"), height="280px"))
        )
      ),

      tags$div(class="mt-3"),

      div(class="section-title", bs_icon("lightbulb-fill"), " Insights cles"),
      uiOutput(ns("insights_box"))
    )
  )
}

#' @export
server_facteurs <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      annees  <- sort(unique(app_data$accidents_light$annee))
      regions <- sort(unique(as.character(app_data$accidents_light$region)))
      regions <- regions[!is.na(regions)]
      routes  <- sort(unique(as.character(app_data$accidents_light$catr_label)))
      routes  <- routes[!routes %in% c("Non renseigné","")]
      deps    <- sort(unique(as.character(app_data$accidents_light$departement)))
      deps    <- deps[!is.na(deps)]
      updateSelectInput(session, "annees",     choices=as.character(annees), selected=character(0))
      updateSelectInput(session, "regions",    choices=regions,              selected=character(0))
      updateSelectInput(session, "type_route", choices=routes,               selected=character(0))
      updateSelectInput(session, "deps",       choices=deps,                 selected=character(0))
    })

    observe({
      req(length(input$regions) > 0)
      deps_f <- app_data$accidents_light |>
        dplyr::filter(as.character(region) %in% input$regions) |>
        dplyr::pull(departement) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "deps", choices=deps_f, selected=character(0))
    })

    observe({
      req(length(input$deps) > 0)
      regs_f <- app_data$accidents_light |>
        dplyr::filter(as.character(departement) %in% input$deps) |>
        dplyr::pull(region) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "regions", choices=regs_f, selected=input$regions)
    })

    filtered <- reactive({
      d <- app_data$accidents_light
      if (length(input$annees)     > 0) d <- d |> filter(annee %in% as.numeric(input$annees))
      if (length(input$regions)    > 0) d <- d |> filter(as.character(region) %in% input$regions)
      if (length(input$type_route) > 0) d <- d |> filter(as.character(catr_label) %in% input$type_route)
      if (length(input$deps)       > 0) d <- d |> filter(as.character(departement) %in% input$deps)
      d
    })


    .taux_to_hex <- function(taux, max_val = NULL) {
      if (length(taux) == 0 || all(is.na(taux))) return(character(0))
      mv <- if (is.null(max_val)) max(taux, na.rm=TRUE) else max_val
      if (mv == 0) return(rep("#27ae60", length(taux)))
      t <- pmin(pmax(taux / mv, 0), 1)
      r <- ifelse(t < 0.4, round(39  + (255-39)*t/0.4),
           ifelse(t < 0.7, 255, round(255-(255-220)*(t-0.7)/0.3)))
      g <- ifelse(t < 0.4, round(174 - (174-196)*t/0.4),
           ifelse(t < 0.7, round(196-(196-111)*(t-0.4)/0.3),
                  round(111-(111-53)*(t-0.7)/0.3)))
      b <- ifelse(t < 0.4, round(96 - 96*t/0.4), 0)
      sprintf("#%02X%02X%02X", pmin(r,255), pmin(g,255), pmin(b,255))
    }
    output$data_summary <- renderUI({
      tagList(bs_icon("database"), " ",
              tags$strong(format(nrow(filtered()), big.mark="\u00a0")), " accidents")
    })

    # ── KPI ──────────────────────────────────────────────────────────────────
    output$kpi_meteo <- renderText({
      d <- .taux_var(filtered(), "atm_label")
      if (nrow(d)==0) return("N/A")
      d$val[1]
    })
    output$kpi_meteo_sub <- renderText({
      d <- .taux_var(filtered(), "atm_label")
      if (nrow(d)==0) return("")
      paste0("taux mortalite : ", d$taux[1], "%")
    })

    output$kpi_route <- renderText({
      d <- .taux_var(filtered(), "catr_label")
      if (nrow(d)==0) return("N/A")
      d$val[1]
    })
    output$kpi_route_sub <- renderText({
      d <- .taux_var(filtered(), "catr_label")
      if (nrow(d)==0) return("")
      paste0("taux mortalite : ", d$taux[1], "%")
    })

    output$kpi_lum <- renderText({
      d <- .taux_var(filtered(), "lum_label")
      if (nrow(d)==0) return("N/A")
      # Raccourcir le label
      gsub("Nuit avec \u00e9clairage public ", "Nuit - \u00e9cl. ", d$val[1])
    })
    output$kpi_lum_sub <- renderText({
      d <- .taux_var(filtered(), "lum_label")
      if (nrow(d)==0) return("")
      paste0("taux mortalite : ", d$taux[1], "%")
    })

    output$kpi_col <- renderText({
      d <- .taux_var(filtered(), "col_label")
      if (nrow(d)==0) return("N/A")
      # Raccourcir
      gsub("Deux v\u00e9hicules - ", "", d$val[1])
    })
    output$kpi_col_sub <- renderText({
      d <- .taux_var(filtered(), "col_label")
      if (nrow(d)==0) return("")
      paste0("taux mortalite : ", d$taux[1], "%")
    })

    # ── Helper plot barres horizontales taux ──────────────────────────────────
    .bar_taux <- function(d, titre_x = "Taux mortalite (%)") {
      if (nrow(d) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      d <- d |> arrange(taux)
      d <- d |> dplyr::mutate(.col = .taux_to_hex(taux))
      plot_ly(d, x=~taux, y=~val,
              type="bar", orientation="h",
              marker=list(color=~.col),
              text=~paste0(taux,"%"), textposition="outside",
              hovertemplate=~paste0(val,"<br>",taux,"% mortalite<br>",
                                    format(n,big.mark="\u00a0")," accidents<extra></extra>")) |>
        layout(xaxis=list(title=titre_x),
               yaxis=list(title=""),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=5, r=60, l=180))
    }

    # ── Meteo taux ────────────────────────────────────────────────────────────
    output$meteo_taux <- renderPlotly({
      req(nrow(filtered()) > 0)
      .bar_taux(.taux_var(filtered(), "atm_label", min_n=100))
    })

    # ── Luminosite taux ───────────────────────────────────────────────────────
    output$lum_taux <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- .taux_var(filtered(), "lum_label", min_n=100) |>
        mutate(val = gsub("avec \u00e9clairage public ", "\u00e9cl. ", val))
      .bar_taux(d)
    })

    # ── Surface taux ──────────────────────────────────────────────────────────
    output$surf_taux <- renderPlotly({
      req(nrow(filtered()) > 0)
      .bar_taux(.taux_var(filtered(), "surf_label", min_n=100))
    })

    # ── Route x Gravite (empile 100%) ─────────────────────────────────────────
    output$route_gravite <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(catr_label),
               as.character(catr_label) != "Non renseign\u00e9") |>
        mutate(route=as.character(catr_label), grav=as.character(gravite_accident)) |>
        group_by(route, grav) |> summarise(n=n(), .groups="drop") |>
        group_by(route) |> mutate(pct=round(n/sum(n)*100,1), total=sum(n)) |>
        ungroup() |> filter(total >= 200)

      ordre <- d |> filter(grav=="Mortel") |> arrange(pct) |> pull(route)

      grav_ordre <- c("Léger","Grave","Mortel")
      grav_present <- grav_ordre[grav_ordre %in% unique(d$grav)]
      if (length(grav_present) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      dd1 <- d |> dplyr::filter(grav==grav_present[1])
      p <- plot_ly(data=dd1, x=~pct, y=~route, type="bar", orientation="h",
                   name=grav_present[1], marker=list(color=.COULEURS[grav_present[1]]),
                   text=~ifelse(pct>4, paste0(pct,"%"), ""), textposition="inside")
      if (length(grav_present) > 1) {
        for (g in grav_present[-1]) {
          dd <- d |> dplyr::filter(grav==g)
          if (nrow(dd) == 0) next
          p <- p |> add_bars(data=dd, x=~pct, y=~route, name=g, orientation="h",
                             marker=list(color=.COULEURS[g]),
                             text=~ifelse(pct>4, paste0(pct,"%"), ""), textposition="inside")
        }
      }
      p |> layout(barmode="stack",
                  xaxis=list(title="% accidents", range=c(0,100)),
                  yaxis=list(title="", categoryorder="array", categoryarray=ordre),
                  legend=list(orientation="h", y=-0.15),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5, l=180))
    })

    # ── Collision x Gravite (empile 100%) ─────────────────────────────────────
    output$collision_gravite <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(col_label),
               as.character(col_label) != "Non renseign\u00e9") |>
        mutate(col=as.character(col_label), grav=as.character(gravite_accident)) |>
        # Raccourcir les labels
        mutate(col = gsub("Deux v\u00e9hicules - ", "2 veh. - ", col)) |>
        mutate(col = gsub("Trois v\u00e9hicules et plus - ", "3+ veh. - ", col)) |>
        group_by(col, grav) |> summarise(n=n(), .groups="drop") |>
        group_by(col) |> mutate(pct=round(n/sum(n)*100,1), total=sum(n)) |>
        ungroup() |> filter(total >= 200)

      ordre <- d |> filter(grav=="Mortel") |> arrange(pct) |> pull(col)

      grav_ordre <- c("Léger","Grave","Mortel")
      grav_present <- grav_ordre[grav_ordre %in% unique(d$grav)]
      if (length(grav_present) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      dd1 <- d |> dplyr::filter(grav==grav_present[1])
      p <- plot_ly(data=dd1, x=~pct, y=~col, type="bar", orientation="h",
                   name=grav_present[1], marker=list(color=.COULEURS[grav_present[1]]),
                   text=~ifelse(pct>4, paste0(pct,"%"), ""), textposition="inside")
      if (length(grav_present) > 1) {
        for (g in grav_present[-1]) {
          dd <- d |> dplyr::filter(grav==g)
          if (nrow(dd) == 0) next
          p <- p |> add_bars(data=dd, x=~pct, y=~col, name=g, orientation="h",
                             marker=list(color=.COULEURS[g]),
                             text=~ifelse(pct>4, paste0(pct,"%"), ""), textposition="inside")
        }
      }
      p |> layout(barmode="stack",
                  xaxis=list(title="% accidents", range=c(0,100)),
                  yaxis=list(title="", categoryorder="array", categoryarray=ordre),
                  legend=list(orientation="h", y=-0.15),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5, l=180))
    })

    # ── Nuit x Surface ────────────────────────────────────────────────────────
    output$nuit_surf <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(lum_label), !is.na(surf_label),
               as.character(surf_label) != "Non renseign\u00e9") |>
        mutate(
          nuit = grepl("Nuit", as.character(lum_label)),
          surf = as.character(surf_label)
        ) |>
        group_by(surf, nuit) |>
        summarise(n=n(), taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,2),
                  .groups="drop") |>
        filter(n >= 100)

      surfs <- d |> group_by(surf) |>
        summarise(taux_moy=mean(taux)) |>
        arrange(taux_moy) |> pull(surf)

      dd_jour <- d |> dplyr::filter(nuit==FALSE)
      dd_nuit <- d |> dplyr::filter(nuit==TRUE)
      p <- plot_ly(data=dd_jour, x=~taux, y=~surf, type="bar", orientation="h",
                   name="Jour", marker=list(color="#f39c12"),
                   text=~paste0(taux,"%"), textposition="outside")
      if (nrow(dd_nuit) > 0) {
        p <- p |> add_bars(data=dd_nuit, x=~taux, y=~surf, name="Nuit",
                           orientation="h", marker=list(color="#1b3a6b"),
                           text=~paste0(taux,"%"), textposition="outside")
      }
      p |> layout(barmode="group",
                  xaxis=list(title="Taux mortalite (%)"),
                  yaxis=list(title="", categoryorder="array", categoryarray=surfs),
                  legend=list(orientation="h", y=-0.15),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5, r=60, l=130))
    })

    # ── Route x Meteo ────────────────────────────────────────────────────────
    output$route_meteo <- renderPlotly({
      req(nrow(filtered()) > 0)
      # Top 4 meteos + top 4 routes pour lisibilite
      top_meteo <- filtered() |>
        filter(!is.na(atm_label),
               as.character(atm_label) != "Non renseign\u00e9") |>
        mutate(m=as.character(atm_label)) |>
        group_by(m) |> summarise(n=n(), .groups="drop") |>
        filter(n >= 500) |> arrange(desc(n)) |>
        slice_head(n=5) |> pull(m)

      top_route <- filtered() |>
        filter(!is.na(catr_label),
               as.character(catr_label) != "Non renseign\u00e9") |>
        mutate(r=as.character(catr_label)) |>
        group_by(r) |> summarise(n=n(), .groups="drop") |>
        filter(n >= 500) |> arrange(desc(n)) |>
        slice_head(n=5) |> pull(r)

      d <- filtered() |>
        filter(as.character(atm_label) %in% top_meteo,
               as.character(catr_label) %in% top_route) |>
        mutate(meteo=as.character(atm_label), route=as.character(catr_label)) |>
        group_by(meteo, route) |>
        summarise(n=n(), taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,2),
                  .groups="drop") |>
        filter(n >= 100)

      if (nrow(d) == 0) return(plot_ly() |> layout(title="Pas assez de donnees"))

      couleurs_meteo <- c(
        "Normale"="#27ae60", "Pluie l\u00e9g\u00e8re"="#4a6fa5",
        "Temps couvert"="#7eb3ff", "Pluie forte"="#f39c12",
        "Temps \u00e9blouissant"="#ffd43b"
      )

      p <- plot_ly()
      for (m in top_meteo) {
        dd <- d |> filter(meteo==m)
        col <- if (m %in% names(couleurs_meteo)) couleurs_meteo[m] else "#95a5a6"
        p <- p |> add_trace(data=dd,
          x=~route, y=~taux, type="scatter", mode="lines+markers",
          name=m, line=list(color=col, width=2),
          marker=list(color=col, size=8),
          text=~paste0(m," - ",route,"<br>",taux,"% mortalite"),
          hoverinfo="text")
      }
      p |> layout(
        xaxis=list(title="", tickangle=-25),
        yaxis=list(title="Taux mortalite (%)"),
        legend=list(orientation="h", y=-0.35, font=list(size=10)),
        paper_bgcolor="transparent", plot_bgcolor="transparent",
        margin=list(t=5, b=100))
    })

    # ── Insights ──────────────────────────────────────────────────────────────
    output$insights_box <- renderUI({
      d <- filtered()
      if (nrow(d)==0) return(NULL)

      meteo_top  <- .taux_var(d, "atm_label")
      route_top  <- .taux_var(d, "catr_label")
      lum_top    <- .taux_var(d, "lum_label")
      surf_top   <- .taux_var(d, "surf_label")
      col_top    <- .taux_var(d, "col_label")

      # Taux nuit vs jour
      taux_nuit <- d |>
        mutate(nuit=grepl("Nuit", as.character(lum_label))) |>
        group_by(nuit) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop")
      t_nuit <- taux_nuit[taux_nuit$nuit==TRUE,  "taux"][[1]]
      t_jour <- taux_nuit[taux_nuit$nuit==FALSE, "taux"][[1]]

      # Route nationale vs voie communale
      taux_route_nat <- d |>
        filter(as.character(catr_label)=="Route nationale") |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1)) |>
        pull(taux)
      taux_voie_com <- d |>
        filter(as.character(catr_label)=="Voie communale") |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1)) |>
        pull(taux)

      div(class="insight-box",
        tags$ul(style="margin:0;padding-left:20px;line-height:2;",
          if (nrow(meteo_top)>0)
            tags$li("La condition météo la plus meurtrière est ",
                    tags$b(meteo_top$val[1]), " avec ",
                    tags$b(paste0(meteo_top$taux[1],"%")), " de taux mortalité"),
          if (nrow(lum_top)>0)
            tags$li("Sans éclairage public la nuit : taux mortalité de ",
                    tags$b(paste0(t_nuit,"%")), " vs ",
                    tags$b(paste0(t_jour,"%")), " en plein jour"),
          if (nrow(surf_top)>0)
            tags$li("Chaussée ", tags$b(surf_top$val[1]),
                    " est la plus meurtrière : ",
                    tags$b(paste0(surf_top$taux[1],"%"))),
          if (nrow(route_top)>0)
            tags$li(tags$b(route_top$val[1]),
                    " est le type de route le plus mortel (",
                    tags$b(paste0(route_top$taux[1],"%")), ")",
                    if (length(taux_route_nat)>0 && length(taux_voie_com)>0)
                      tags$span(" — Route nationale : ", tags$b(paste0(taux_route_nat,"%")),
                                " vs Voie communale : ", tags$b(paste0(taux_voie_com,"%")))),
          if (nrow(col_top)>0)
            tags$li("Le type de collision le plus meurtrier : ",
                    tags$b(col_top$val[1]), " — ",
                    tags$b(paste0(col_top$taux[1],"%")))
        )
      )
    })
  })
}
