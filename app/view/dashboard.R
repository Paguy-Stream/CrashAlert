# app/view/dashboard.R
box::use(
  shiny[NS, moduleServer, tags, tagList, icon, uiOutput, renderUI,
        textOutput, renderText, observeEvent, observe, reactive,
        eventReactive, updateSelectInput, actionButton, selectInput,
        req, HTML, div, span, h5, h6, hr, p, strong,
        downloadButton, downloadHandler],
  bslib[layout_columns, card, card_header, card_body, card_footer,
        value_box, navset_card_tab, nav_panel],
  bsicons[bs_icon],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, add_bars],
  leaflet[leafletOutput, renderLeaflet, leaflet, addProviderTiles,
          providers, setView, addCircleMarkers, addLegend, colorFactor,
          markerClusterOptions],
  DT[DTOutput, renderDT, datatable, formatStyle, styleInterval],
  dplyr[filter, mutate, group_by, summarise, n, slice_sample, ungroup,
        arrange, desc, slice_head, left_join, select, rename],
  app/logic/data[filter_accidents, compute_kpis],
)

# Décodage infra
decode_infra <- function(x) {
  codes <- c("-1"="Non renseigné","1"="Souterrain","2"="Pont","3"="Échangeur",
             "4"="Voie ferrée","5"="Carrefour aménagé","6"="Zone piétonne",
             "7"="Zone de péage","8"="Chantier","9"="Autre")
  ifelse(as.character(x) %in% names(codes), codes[as.character(x)], as.character(x))
}

# ── UI ───────────────────────────────────────────────────────────────────────
#' @export
ui_dashboard <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .ca-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      .ca-topbar-left { display: flex; align-items: center; gap: 12px; }
      .ca-topbar-right { color: #6c757d; font-size: 13px; }
      .btn-filter-toggle {
        background: #1b3a6b; color: #fff; border: none; border-radius: 6px;
        padding: 7px 16px; font-size: 13px; font-weight: 500;
        display: inline-flex; align-items: center; gap: 7px;
        cursor: pointer; transition: background 0.15s;
      }
      .btn-filter-toggle:hover { background: #15305a; color: #fff; }
      .ca-active-tags { display: flex; gap: 6px; flex-wrap: wrap; }
      .ca-tag {
        background: #e8edf5; color: #1b3a6b; border-radius: 12px;
        padding: 2px 10px; font-size: 11px; font-weight: 600;
      }
      .offcanvas-start { width: 320px !important; }
      .offcanvas-header { background: #1b3a6b; color: #fff; padding: 16px 20px; }
      .offcanvas-header .btn-close { filter: invert(1); opacity: 0.8; }
      .offcanvas-title { font-weight: 600; font-size: 15px; }
      .offcanvas-body { padding: 20px; background: #fafbfc; }
      .filter-section { margin-bottom: 20px; }
      .filter-label {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.05em; color: #6c757d; margin-bottom: 6px;
      }
      .filter-footer {
        position: sticky; bottom: 0; background: #fafbfc;
        border-top: 1px solid #e9ecef; padding: 14px 20px;
        display: flex; gap: 8px;
      }
      .ca-content { padding: 16px; }
      .kpi2 .value-box-title { font-size: 11px !important; }
      .kpi2 .value-box-value { font-size: 1.4rem !important; font-weight: 700 !important; }
      .section-title {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6c757d; margin: 20px 0 12px;
        display: flex; align-items: center; gap: 8px;
      }
      .section-title::after {
        content: ''; flex: 1; height: 1px; background: #e9ecef;
      }
    ")),

    # ── Offcanvas ────────────────────────────────────────────────────────────
    div(class = "offcanvas offcanvas-start", tabindex = "-1",
        id = ns("filterOffcanvas"),
        `aria-labelledby` = ns("filterOffcanvasLabel"),
      div(class = "offcanvas-header",
        h5(class = "offcanvas-title", id = ns("filterOffcanvasLabel"),
           bs_icon("funnel-fill"), " Filtres"),
        tags$button(type="button", class="btn-close",
                    `data-bs-dismiss`="offcanvas", `aria-label`="Close")
      ),
      div(class = "offcanvas-body",
        div(class="filter-section",
          div(class="filter-label", "Années"),
          selectInput(ns("annees"), label=NULL, choices=NULL, multiple=TRUE, width="100%")
        ),
        div(class="filter-section",
          div(class="filter-label", "Gravité"),
          selectInput(ns("gravite"), label=NULL,
            choices=c("Mortel","Grave","Léger"), selected=NULL,
            multiple=TRUE, width="100%")
        ),
        div(class="filter-section",
          div(class="filter-label", "Régions"),
          selectInput(ns("regions"), label=NULL, choices=NULL, multiple=TRUE, width="100%")
        ),
        div(class="filter-section",
          div(class="filter-label", "Départements"),
          selectInput(ns("deps"), label=NULL, choices=NULL, multiple=TRUE, width="100%")
        )
      ),
      div(class="filter-footer",
        actionButton(ns("apply"), "Appliquer",
          class="btn btn-primary flex-fill",
          `data-bs-dismiss`="offcanvas", icon=icon("check")),
        actionButton(ns("reset"), "Réinitialiser",
          class="btn btn-outline-secondary", icon=icon("rotate-left"))
      )
    ),

    # ── Topbar ────────────────────────────────────────────────────────────────
    div(class="ca-topbar",
      div(class="ca-topbar-left",
        tags$button(class="btn-filter-toggle",
          `data-bs-toggle`="offcanvas",
          `data-bs-target`=paste0("#", ns("filterOffcanvas")),
          `aria-controls`=ns("filterOffcanvas"),
          bs_icon("sliders"), " Filtres"),
        uiOutput(ns("active_tags"))
      ),
      div(class="ca-topbar-right",
        uiOutput(ns("filter_summary"), inline=TRUE),
        downloadButton(ns("dl_csv"), label=" Exporter CSV",
          icon = shiny::icon("download"),
          class = "btn btn-sm btn-outline-secondary ms-2",
          style = "font-size:12px;padding:4px 10px;")
      )
    ),

    # ── Contenu ───────────────────────────────────────────────────────────────
    div(class="ca-content",

      # ── KPI Ligne 1 ────────────────────────────────────────────────────────
      div(class="section-title", bs_icon("bar-chart-fill"), " Vue d'ensemble"),
      layout_columns(
        col_widths=c(3,3,3,3), fill=FALSE,
        value_box("Total accidents",
          textOutput(ns("kpi_total"), inline=TRUE),
          showcase=bs_icon("exclamation-triangle-fill"), theme="primary"),
        value_box("Décès",
          textOutput(ns("kpi_morts"), inline=TRUE),
          showcase=bs_icon("heart-pulse-fill"), theme="danger"),
        value_box("Blessés graves",
          textOutput(ns("kpi_graves"), inline=TRUE),
          showcase=bs_icon("bandaid-fill"), theme="warning"),
        value_box("Blessés légers",
          textOutput(ns("kpi_legers"), inline=TRUE),
          showcase=bs_icon("activity"), theme="success")
      ),

      # ── KPI Ligne 2 ────────────────────────────────────────────────────────
      tags$div(class="mt-2"),
      layout_columns(
        col_widths=c(3,3,3,3), fill=FALSE,
        div(class="kpi2",
          value_box("Taux de mortalité",
            textOutput(ns("kpi_taux"), inline=TRUE),
            showcase=bs_icon("percent"), theme="light",
            p(class="mb-0 text-muted", style="font-size:11px;",
              "accidents mortels / total"))),
        div(class="kpi2",
          value_box("Accidents de nuit",
            textOutput(ns("kpi_nuit"), inline=TRUE),
            showcase=bs_icon("moon-stars-fill"), theme="light",
            p(class="mb-0 text-muted", style="font-size:11px;",
              "20h–6h du matin"))),
        div(class="kpi2",
          value_box("Route mouillée",
            textOutput(ns("kpi_mouille"), inline=TRUE),
            showcase=bs_icon("cloud-rain-fill"), theme="light",
            p(class="mb-0 text-muted", style="font-size:11px;",
              "surface dégradée"))),
        div(class="kpi2",
          value_box("Âge moyen",
            textOutput(ns("kpi_age"), inline=TRUE),
            showcase=bs_icon("person-fill"), theme="light",
            p(class="mb-0 text-muted", style="font-size:11px;",
              "conducteur impliqué")))
      ),

      # ── Carte + Pie ────────────────────────────────────────────────────────
      div(class="section-title", bs_icon("map-fill"), " Géographie"),
      layout_columns(
        col_widths=c(8,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("map"), " Carte des accidents")),
          card_body(padding=0, leafletOutput(ns("map"), height="400px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("pie-chart-fill"), " Répartition gravité")),
          card_body(plotlyOutput(ns("pie"), height="400px"))
        )
      ),

      # ── Évolution + Véhicules ──────────────────────────────────────────────
      div(class="section-title", bs_icon("graph-up"), " Tendances"),
      layout_columns(
        col_widths=c(7,5),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("graph-up"), " Évolution annuelle")),
          card_body(plotlyOutput(ns("evolution"), height="280px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("car-front-fill"), " Véhicules impliqués")),
          card_body(plotlyOutput(ns("vehicules"), height="280px"))
        )
      ),

      # ── Facteurs + Top depts ───────────────────────────────────────────────
      div(class="section-title", bs_icon("layers-fill"), " Facteurs & Zones à risque"),
      layout_columns(
        col_widths=c(4,4,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("sign-intersection"), " Type de collision")),
          card_body(plotlyOutput(ns("collision"), height="250px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("cloud-drizzle"), " Conditions météo")),
          card_body(plotlyOutput(ns("meteo"), height="250px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("signpost-split"), " Type de route")),
          card_body(plotlyOutput(ns("route"), height="250px"))
        )
      ),

      tags$div(class="mt-3"),

      # ── Top 10 départements ────────────────────────────────────────────────
      card(full_screen=TRUE,
        card_header(tagList(bs_icon("trophy-fill"), " Top 15 départements — accidentalité")),
        card_body(DTOutput(ns("top_dept")))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
#' @export
server_dashboard <- function(id, app_data, app_filters) {
  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectInput(session, "annees",
        choices=as.character(app_filters$annees), selected=character(0))
      updateSelectInput(session, "regions",
        choices=app_filters$regions, selected=character(0))
      deps <- sort(unique(as.character(app_data$accidents_dashboard$departement)))
      deps <- deps[!is.na(deps)]
      updateSelectInput(session, "deps", choices=deps, selected=character(0))
    })

    observe({
      req(length(input$regions) > 0)
      deps_f <- app_data$accidents_dashboard |>
        dplyr::filter(as.character(region) %in% input$regions) |>
        dplyr::pull(departement) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "deps", choices=deps_f, selected=character(0))
    })

    observe({
      req(length(input$deps) > 0)
      regs_f <- app_data$accidents_dashboard |>
        dplyr::filter(as.character(departement) %in% input$deps) |>
        dplyr::pull(region) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "regions", choices=regs_f, selected=input$regions)
    })

    filtered <- eventReactive(input$apply, {
      annees_sel  <- if (length(input$annees)==0)  NULL else as.numeric(input$annees)
      regions_sel <- if (length(input$regions)==0) NULL else input$regions
      gravite_sel <- if (length(input$gravite)==0) NULL else input$gravite
      dept_sel <- if (length(input$deps)==0) NULL else input$deps
      d <- filter_accidents(app_data$accidents_dashboard,
        annees=annees_sel, regions=regions_sel, gravites=gravite_sel)
      if (!is.null(dept_sel)) d <- d |> dplyr::filter(as.character(departement) %in% dept_sel)
      d
    }, ignoreNULL=FALSE)

    observeEvent(input$reset, {
      updateSelectInput(session, "annees",   selected=character(0))
      updateSelectInput(session, "regions",  selected=character(0))
      updateSelectInput(session, "gravite",  selected=character(0))
    })

    # Tags actifs
    output$active_tags <- renderUI({
      tl <- list()
      if (length(input$annees)>0)
        tl <- c(tl, list(span(class="ca-tag",
          if(length(input$annees)<=3) paste(input$annees,collapse=", ")
          else paste0(length(input$annees)," années"))))
      if (length(input$gravite)>0 && length(input$gravite)<3)
        tl <- c(tl, list(span(class="ca-tag", paste(input$gravite,collapse=", "))))
      if (length(input$regions)>0)
        tl <- c(tl, list(span(class="ca-tag",
          if(length(input$regions)==1) input$regions
          else paste0(length(input$regions)," régions"))))
      if (length(tl)==0)
        span(class="ca-tag", style="background:#f0f0f0;color:#999;", "Aucun filtre actif")
      else div(class="ca-active-tags", tagList(tl))
    })

    output$filter_summary <- renderUI({
      d <- filtered()
      tagList(bs_icon("database"), " ",
        tags$strong(format(nrow(d), big.mark="\u00a0")), " accidents")
    })

    # KPI
    kpis <- reactive({ compute_kpis(filtered()) })
    output$kpi_total  <- renderText(format(kpis()$total,  big.mark="\u00a0"))
    output$kpi_morts  <- renderText(format(kpis()$morts,  big.mark="\u00a0"))
    output$kpi_graves <- renderText(format(kpis()$graves, big.mark="\u00a0"))
    output$kpi_legers <- renderText(format(kpis()$legers, big.mark="\u00a0"))

    output$kpi_taux <- renderText({
      d <- filtered()
      pct <- round(mean(d$gravite_accident == "Mortel", na.rm=TRUE)*100, 1)
      paste0(pct, " %")
    })
    output$kpi_nuit <- renderText({
      d <- filtered()
      nuit <- sum(d$moment_journee %in% c("Nuit (20h-0h)","Nuit profonde (0h-6h)"), na.rm=TRUE)
      paste0(round(nuit/nrow(d)*100,1), " %")
    })
    output$kpi_mouille <- renderText({
      d <- filtered()
      mouille <- sum(d$surf_label %in% c("Mouillée","Flaques","Inondée"), na.rm=TRUE)
      paste0(round(mouille/nrow(d)*100,1), " %")
    })
    output$kpi_age <- renderText({
      d <- filtered()
      age_col <- if("age_moyen_reel" %in% names(d)) d$age_moyen_reel else d$age_moyen
      paste0(round(mean(age_col, na.rm=TRUE),1), " ans")
    })

    # Carte
    output$map <- renderLeaflet({
      d <- filtered()
      d_geo <- d |> filter(!is.na(lat), !is.na(long), coords_valides==TRUE,
                            lat>=41, lat<=51, long>=-5, long<=10)
      if (nrow(d_geo)>5000) d_geo <- slice_sample(d_geo, n=5000)
      base <- leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
              setView(2.2137, 46.2276, zoom=6)
      if (nrow(d_geo)==0) return(base)
      d_geo <- d_geo |> mutate(
        lum_label=as.character(lum_label), atm_label=as.character(atm_label),
        catr_label=as.character(catr_label),
        categorie_vehicule=as.character(categorie_vehicule),
        col_label=as.character(col_label), surf_label=as.character(surf_label))
      couleurs <- c("Mortel"="#e74c3c","Grave"="#f39c12","Léger"="#27ae60")
      presents <- intersect(c("Mortel","Grave","Léger"), unique(d_geo$gravite_accident))
      pal <- colorFactor(palette=unname(couleurs[presents]),
                         domain=presents, na.color="#6c757d")
      base |>
        addCircleMarkers(data=d_geo, lng=~long, lat=~lat, radius=4,
          color=~pal(gravite_accident), fillColor=~pal(gravite_accident),
          fillOpacity=0.75, stroke=FALSE,
          clusterOptions=markerClusterOptions(maxClusterRadius=40),
          popup=~paste0(
            "<div style='font-family:Inter,sans-serif;min-width:210px;border-radius:6px;overflow:hidden;border:1px solid #dee2e6;'>",
            "<div style='background:#1b3a6b;color:white;padding:8px 12px;'>",
            "<b>", gravite_accident, "</b></div>",
            "<div style='padding:10px 12px;line-height:1.9;font-size:13px;'>",
            "<b>\U0001F4C5</b> ", jour, "/", mois, "/", annee,
            ifelse(!is.na(heure), paste0(" \u00e0 ", heure, "h",
              ifelse(minute<10, paste0("0",minute), minute)), ""), "<br>",
            "<b>\U0001F4CD</b> ", departement, " (", dep, ")<br>",
            "<b>\U0001F697</b> ", categorie_vehicule, "<br>",
            "<b>\U0001F4A5</b> ", col_label, "<br>",
            "<b>\U0001F31F</b> ", lum_label, " \u2014 ",
            "<b>\U0001F326</b> ", atm_label, "<br>",
            "<b>\U0001F6E3</b> ", catr_label, " \u2014 ",
            "<b>\U0001F4A7</b> ", surf_label, "<br>",
            "<b>\U0001F91D</b> Victimes : </b>", nb_victimes,
            " (", nb_tues, " tué(s), ", nb_blesses_hospitalises, " hosp.)",
            "</div></div>"
          )) |>
        addLegend("bottomright", pal=pal, values=d_geo$gravite_accident,
                  title="Gravité", opacity=0.9)
    })

    # Pie
    output$pie <- renderPlotly({
      d <- filtered() |> group_by(gravite_accident) |>
           summarise(n=n(), .groups="drop")
      couleurs_pie <- c("Mortel"="#e74c3c","Grave"="#f39c12",
                        "Léger"="#27ae60","Inconnu"="#95a5a6")
      plot_ly(d, labels=~gravite_accident, values=~n, type="pie", hole=0.45,
        marker=list(colors=unname(couleurs_pie[as.character(d$gravite_accident)])),
        textinfo="label+percent") |>
        layout(showlegend=TRUE, margin=list(t=10,b=10),
               paper_bgcolor="transparent", plot_bgcolor="transparent")
    })

    # Évolution
    output$evolution <- renderPlotly({
      d <- app_data$agg_evolution |> group_by(annee) |>
        summarise(Accidents=sum(nb_accidents,na.rm=TRUE),
                  Mortels=sum(nb_mortels,na.rm=TRUE),
                  Graves=sum(nb_graves,na.rm=TRUE), .groups="drop")
      plot_ly(d, x=~annee, y=~Accidents, name="Accidents",
              type="scatter", mode="lines+markers",
              line=list(color="#1b3a6b",width=2),
              marker=list(color="#1b3a6b",size=6)) |>
        add_trace(y=~Mortels, name="Mortels",
                  line=list(color="#e74c3c",width=2),
                  marker=list(color="#e74c3c",size=6)) |>
        add_trace(y=~Graves, name="Graves",
                  line=list(color="#f39c12",width=2),
                  marker=list(color="#f39c12",size=6)) |>
        layout(xaxis=list(title="Année",showgrid=FALSE),
               yaxis=list(title="Nombre",gridcolor="#f0f0f0"),
               legend=list(orientation="h",y=-0.3),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=5,b=5))
    })

    # Véhicules
    output$vehicules <- renderPlotly({
      d <- filtered() |>
        filter(!is.na(categorie_vehicule),
               as.character(categorie_vehicule) != "Non renseigné") |>
        mutate(categorie_vehicule=as.character(categorie_vehicule)) |>
        group_by(categorie_vehicule) |>
        summarise(n=n(), mortels=sum(gravite_accident=="Mortel",na.rm=TRUE),
                  .groups="drop") |>
        mutate(taux=round(mortels/n*100,1)) |>
        arrange(desc(n)) |> slice_head(n=8)
      d <- d |> dplyr::arrange(n)
      plot_ly(d, x=~categorie_vehicule, y=~n,
              type="bar", marker=list(color="#1b3a6b"),
              name="Accidents",
              text=~paste0(taux,"% mortels"), textposition="outside") |>
        layout(xaxis=list(title="", tickangle=-30),
               yaxis=list(title="Nb accidents"),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=10,b=80))
    })

    # Collision
    output$collision <- renderPlotly({
      d <- filtered() |>
        filter(!is.na(col_label), col_label!="Non renseigné") |>
        mutate(col_label=as.character(col_label)) |>
        group_by(col_label) |> summarise(n=n(),.groups="drop") |>
        arrange(desc(n)) |> slice_head(n=6)
      plot_ly(d, labels=~col_label, values=~n, type="pie", hole=0.4,
        marker=list(colors=c("#1b3a6b","#4a6fa5","#7eb3ff",
                              "#b8d4ff","#e8f0ff","#f0f4ff")),
        textinfo="percent", hoverinfo="label+value+percent") |>
        layout(showlegend=TRUE, legend=list(font=list(size=10)),
               margin=list(t=5,b=5,l=5,r=5),
               paper_bgcolor="transparent", plot_bgcolor="transparent")
    })

    # Météo
    output$meteo <- renderPlotly({
      d <- app_data$agg_meteo |>
        arrange(taux_mortalite) |> slice_head(n=6)
      plot_ly(d, x=~taux_mortalite,
              y=~conditions_meteo,
              type="bar", orientation="h",
              marker=list(color=~taux_mortalite,
                          colorscale=list(c(0,"#fff3cd"),c(1,"#dc3545")),
                          showscale=FALSE),
              text=~paste0(round(taux_mortalite,1),"%"),
              textposition="outside") |>
        layout(xaxis=list(title="Taux mortalité (%)"),
               yaxis=list(title=""),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=5,r=60))
    })

    # Route
    output$route <- renderPlotly({
      d <- filtered() |>
        filter(!is.na(type_route)) |>
        mutate(type_route=as.character(type_route)) |>
        group_by(type_route) |>
        summarise(n=n(), mortels=sum(gravite_accident=="Mortel",na.rm=TRUE),
                  .groups="drop") |>
        mutate(taux=round(mortels/n*100,1)) |>
        arrange(desc(n)) |> slice_head(n=6)
      d <- d |> dplyr::arrange(desc(n))
      plot_ly(d, x=~type_route, y=~n,
              type="bar", name="Total",
              marker=list(color="#4a6fa5")) |>
        add_bars(y=~mortels, name="Mortels",
                 marker=list(color="#e74c3c")) |>
        layout(barmode="overlay",
               xaxis=list(title="", tickangle=-25),
               yaxis=list(title="Nb accidents"),
               legend=list(orientation="h",y=-0.4),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=5,b=90))
    })

    # Top 15 départements
    output$top_dept <- renderDT({
      d <- app_data$agg_departement |>
        # Re-grouper pour éliminer les doublons de codes/périodes dans le RDS
        group_by(departement, region) |>
        summarise(
          nb_accidents   = sum(nb_accidents,   na.rm=TRUE),
          nb_mortels     = sum(nb_mortels,     na.rm=TRUE),
          nb_graves      = sum(nb_graves,      na.rm=TRUE),
          taux_mortalite = round(sum(nb_mortels)/sum(nb_accidents)*100, 2),
          .groups = "drop"
        ) |>
        mutate(classe_gravite = dplyr::case_when(
          taux_mortalite >= 10 ~ "Très élevée",
          taux_mortalite >=  5 ~ "Élevée",
          taux_mortalite >=  2 ~ "Moyenne",
          TRUE                 ~ "Faible"
        )) |>
        arrange(desc(nb_accidents)) |> slice_head(n=15) |>
        select(departement, region, nb_accidents, nb_mortels,
               nb_graves, taux_mortalite, classe_gravite) |>
        rename(Département=departement, Région=region,
               Accidents=nb_accidents, Mortels=nb_mortels,
               Graves=nb_graves, `Taux mortalité (%)`=taux_mortalite,
               `Classe gravité`=classe_gravite)
      datatable(d, rownames=FALSE,
        options=list(pageLength=15, dom="t", ordering=TRUE),
        class="table table-hover table-sm") |>
        formatStyle("Taux mortalité (%)",
          background=styleInterval(c(5,8,12),
            c("#d4edda","#fff3cd","#ffd6a5","#f8d7da"))) |>
        formatStyle("Classe gravité",
          color=styleInterval(0, c("white","white")),
          backgroundColor=styleInterval(0, c("white","white")))
    })

    output$dl_csv <- downloadHandler(
      filename = function() {
        anns <- if (length(input$annees) > 0) paste(input$annees, collapse="-") else "tout"
        regs <- if (length(input$regions) > 0) "filtre" else "france"
        paste0("crashalert_dashboard_", regs, "_", anns, ".csv")
      },
      content = function(file) {
        utils::write.csv(filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

  })
}
