# app/view/profile.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI,
        textOutput, renderText, HTML, div, span, h5, selectInput,
        updateSelectInput, observe, observeEvent, reactive, reactiveVal, req,
        downloadButton, downloadHandler, p],
  bslib[layout_columns, card, card_header, card_body, value_box],
  bsicons[bs_icon],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, add_bars],
  dplyr[filter, mutate, group_by, summarise, n, arrange, desc,
        slice_head, case_when, first, select, rename, ungroup, pull, count],
)

#' @export
ui_profile <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .profile-layout { display: flex; gap: 16px; align-items: flex-start; }
      .profile-main   { flex: 1; min-width: 0; }
      .profile-panel  {
        width: 0; overflow: hidden; transition: width 0.35s cubic-bezier(.4,0,.2,1);
        background: #fff; border-left: 3px solid #1b3a6b;
        border-radius: 8px; box-shadow: 0 2px 12px rgba(0,0,0,0.12);
        flex-shrink: 0;
      }
      .profile-panel.open { width: 300px; }
      .profile-panel-inner { padding: 16px; width: 300px; }
      .panel-close {
        float: right; background: none; border: none;
        font-size: 18px; cursor: pointer; color: #6c757d; line-height: 1;
      }
      .panel-close:hover { color: #1b3a6b; }
      .panel-title { font-size:13px; font-weight:700; color:#1b3a6b;
        text-transform:uppercase; letter-spacing:0.05em;
        margin-bottom:12px; padding-right:24px; }
      .panel-content { font-size:13px; line-height:1.6; color:#444; }
      .panel-value { font-size:22px; font-weight:700; color:#1b3a6b; margin:8px 0; }
      .panel-badge { display:inline-block; padding:2px 10px; border-radius:12px;
        font-size:11px; font-weight:600; margin-bottom:8px; }
      .panel-badge.danger  { background:#fde8e8; color:#c0392b; }
      .panel-badge.warning { background:#fef9e7; color:#d68910; }
      .panel-badge.success { background:#eafaf1; color:#1e8449; }
      .panel-hint { margin-top:10px; padding:8px; background:#f8f9fa;
        border-radius:6px; font-size:11px; color:#6c757d;
        border-left:3px solid #4a6fa5; }
    ")),
    tags$script(HTML(
      "function openProfilePanel(id) {
        var p = document.getElementById(id);
        if(p) p.classList.add('open');
      }
      function closeProfilePanel(id) {
        var p = document.getElementById(id);
        if(p) p.classList.remove('open');
      }
      Shiny.addCustomMessageHandler('openPanel', function(msg) {
        openProfilePanel(msg.id);
      });
      "
    )),
    tags$style(HTML("
      .profile-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        flex-wrap: wrap; gap: 10px;
      }
      .profile-filters { display: flex; align-items: center; gap: 16px; flex-wrap: wrap; }
      .pf-label { font-size: 12px; font-weight: 600; color: #6c757d;
                  margin: 0; white-space: nowrap; }
      .profile-content { padding: 16px; }
      .section-title {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6c757d; margin: 20px 0 12px;
        display: flex; align-items: center; gap: 8px;
      }
      .section-title::after { content: ''; flex: 1; height: 1px; background: #e9ecef; }
      .insight-box {
        background: linear-gradient(135deg, #f8f9ff 0%, #e8edf5 100%);
        border-left: 4px solid #1b3a6b; border-radius: 6px;
        padding: 12px 16px; font-size: 13px; line-height: 1.7;
        color: #2c3e50;
      }
    ")),

    div(class = "profile-topbar",
      div(class = "profile-filters",
        div(
          tags$label(class="pf-label", bs_icon("calendar3"), " Annees"),
          selectInput(ns("annees"), label=NULL, choices=NULL,
                      multiple=TRUE, width="160px")
        ),
        div(
          tags$label(class="pf-label", bs_icon("geo-alt"), " Region"),
          selectInput(ns("regions"), label=NULL, choices=NULL,
                      multiple=TRUE, width="180px")
        ),
        div(
          tags$label(class="pf-label", bs_icon("signpost-split"), " Département"),
          selectInput(ns("deps"), label=NULL, choices=NULL,
                      multiple=TRUE, width="180px")
        ),
        div(
          tags$label(class="pf-label", bs_icon("exclamation-triangle"), " Gravite"),
          selectInput(ns("gravite"), label=NULL,
                      choices=c("Mortel","Grave","Léger"),
                      selected=NULL, multiple=TRUE, width="160px")
        )
      ),
      div(style="color:#6c757d;font-size:12px;display:flex;align-items:center;gap:10px;",
          uiOutput(ns("data_summary"), inline=TRUE),
          downloadButton(ns("dl_csv"), label=" CSV",
            icon = shiny::icon("download"),
            class = "btn btn-sm btn-outline-secondary",
            style = "font-size:11px;padding:3px 9px;")
      )
    ),

    div(class = "profile-layout",
      div(class = "profile-main",

      div(class="section-title", bs_icon("people-fill"), " Profil des accidentes"),
      layout_columns(
        col_widths=c(3,3,3,3), fill=FALSE,
        value_box("Conducteurs",
          textOutput(ns("kpi_conducteurs"), inline=TRUE),
          showcase=bs_icon("car-front-fill"), theme="primary",
          p(class="mb-0 text-muted", style="font-size:11px;", "% des impliques")),
        value_box("Hommes",
          textOutput(ns("kpi_hommes"), inline=TRUE),
          showcase=bs_icon("gender-male"), theme="primary",
          p(class="mb-0 text-muted", style="font-size:11px;", "sexe dominant")),
        value_box("18-34 ans",
          textOutput(ns("kpi_jeunes"), inline=TRUE),
          showcase=bs_icon("person-fill-exclamation"), theme="warning",
          p(class="mb-0 text-muted", style="font-size:11px;", "tranche la + accidentee")),
        value_box("75+ ans mortalite",
          textOutput(ns("kpi_seniors"), inline=TRUE),
          showcase=bs_icon("person-fill"), theme="danger",
          p(class="mb-0 text-muted", style="font-size:11px;", "taux mortalite seniors"))
      ),
      layout_columns(
        col_widths=c(6,6), fill=FALSE,
        value_box(
          uiOutput(ns("kpi_mode_title")),
          textOutput(ns("kpi_mode_pct"), inline=TRUE),
          showcase=bs_icon("bicycle"), theme="success",
          p(class="mb-0 text-muted", style="font-size:11px;", "mode le plus accidente")),
        value_box("Onglet recommande",
          uiOutput(ns("kpi_onglet_rec")),
          showcase=bs_icon("arrow-right-circle-fill"), theme="info",
          p(class="mb-0 text-muted", style="font-size:11px;", "selon votre selection"))
      ),

      div(class="section-title", bs_icon("gender-ambiguous"), " Genre & Age"),
      layout_columns(
        col_widths=c(5,7),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("gender-ambiguous"), " Sexe & gravite")),
          card_body(plotlyOutput(ns("sexe_gravite"), height="300px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("bar-chart"), " Accidents par tranche d age")),
          card_body(plotlyOutput(ns("age_bar"), height="300px"))
        )
      ),

      tags$div(class="mt-3"),

      layout_columns(
        col_widths=c(6,6),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("percent"), " Taux mortalite par age")),
          card_body(plotlyOutput(ns("age_mortalite"), height="280px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("person-fill-exclamation"), " Role dans l accident")),
          card_body(plotlyOutput(ns("catu_pie"), height="280px"))
        )
      ),

      div(class="section-title", bs_icon("car-front-fill"), " Vehicule & Securite"),
      layout_columns(
        col_widths=c(6,6),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("car-front-fill"), " Vehicule x gravite")),
          card_body(plotlyOutput(ns("vehicule_gravite"), height="320px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("shield-check"), " Equipement securite")),
          card_body(plotlyOutput(ns("secu"), height="320px"))
        )
      ),

      tags$div(class="mt-3"),

      div(class="section-title", bs_icon("lightbulb-fill"), " Insights cles"),
      uiOutput(ns("insights_box"))
      ),  # fin profile-main
      div(id = "profile-side-panel", class = "profile-panel",
        div(class = "profile-panel-inner",
          tags$button(class = "panel-close",
            onclick = "closeProfilePanel('profile-side-panel')", "×"),
          uiOutput(ns("side_panel_content"))
        )
      )
    )  # fin profile-layout
  )
}

#' @export
server_profile <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      annees  <- sort(unique(app_data$accidents_dashboard$annee))
      regions <- sort(unique(as.character(app_data$accidents_dashboard$region)))
      regions <- regions[!is.na(regions)]
      deps    <- sort(unique(as.character(app_data$accidents_dashboard$departement)))
      deps    <- deps[!is.na(deps)]
      updateSelectInput(session, "annees",   choices=as.character(annees), selected=character(0))
      updateSelectInput(session, "regions",  choices=regions,              selected=character(0))
      updateSelectInput(session, "deps",     choices=deps,                 selected=character(0))
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

    filtered <- reactive({
      d <- app_data$accidents_dashboard
      if (length(input$annees)  > 0) d <- d |> filter(annee %in% as.numeric(input$annees))
      if (length(input$regions) > 0) d <- d |> filter(as.character(region) %in% input$regions)
      if (length(input$deps)    > 0) d <- d |> filter(as.character(departement) %in% input$deps)
      if (length(input$gravite) > 0) d <- d |> filter(gravite_accident %in% input$gravite)
      d
    })

    # Sans filtre gravité — pour calculer les taux contextuels (mortalité/gravité réels)
    filtered_all <- reactive({
      d <- app_data$accidents_dashboard
      if (length(input$annees)  > 0) d <- d |> filter(annee %in% as.numeric(input$annees))
      if (length(input$regions) > 0) d <- d |> filter(as.character(region) %in% input$regions)
      if (length(input$deps)    > 0) d <- d |> filter(as.character(departement) %in% input$deps)
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
      d <- filtered()
      tagList(bs_icon("database"), " ",
        tags$strong(format(nrow(d), big.mark="\u00a0")), " accidents")
    })

    output$kpi_conducteurs <- renderText({
      d <- filtered()
      pct <- round(mean(as.character(d$catu_principal)=="Conducteur",na.rm=TRUE)*100,1)
      paste0(pct," %")
    })

    output$kpi_hommes <- renderText({
      d <- filtered()
      pct <- round(mean(as.character(d$sexe_dominant)=="Masculin",na.rm=TRUE)*100,1)
      paste0(pct," %")
    })

    output$kpi_jeunes <- renderText({
      d <- filtered()
      pct <- round(mean(as.character(d$tranche_age) %in%
                          c("18-24 ans","25-34 ans"),na.rm=TRUE)*100,1)
      paste0(pct," %")
    })

    output$kpi_seniors <- renderText({
      d <- filtered() |> filter(as.character(tranche_age)=="65 ans et plus")
      if (nrow(d)==0) return("N/A")
      pct <- round(mean(d$gravite_accident=="Mortel",na.rm=TRUE)*100,1)
      paste0(pct," %")
    })

    # ── Mode de déplacement le plus accidenté ──
    .mode_icons <- c(
      "Voiture"        = "car-front-fill",
      "Deux-roues"     = "bicycle",
      "Poids lourd"    = "truck-front-fill",
      "Pieton"         = "person-walking",
      "Autre"          = "question-circle"
    )

    output$kpi_mode_title <- renderUI({
      d <- filtered()
      if (nrow(d) == 0) return(tags$span("Mode de deplacement"))
      mode_top <- d |>
        dplyr::filter(!is.na(categorie_vehicule)) |>
        dplyr::count(categorie_vehicule, sort=TRUE) |>
        dplyr::slice_head(n=1) |>
        dplyr::pull(categorie_vehicule)
      if (length(mode_top)==0) return(tags$span("Mode de deplacement"))
      icone <- .mode_icons[mode_top]
      if (is.na(icone)) icone <- "question-circle"
      tagList(bs_icon(icone), " ", mode_top)
    })

    output$kpi_mode_pct <- renderText({
      d <- filtered()
      if (nrow(d) == 0) return("N/A")
      mode_top <- d |>
        dplyr::filter(!is.na(categorie_vehicule)) |>
        dplyr::count(categorie_vehicule, sort=TRUE) |>
        dplyr::slice_head(n=1)
      if (nrow(mode_top) == 0) return("N/A")
      pct <- round(mode_top$n / nrow(d) * 100, 1)
      paste0(pct, " %")
    })

    # ── Onglet recommandé selon le filtre actif ──
    output$kpi_onglet_rec <- renderUI({
      gravite_sel <- input$gravite
      annees_sel  <- input$annees
      regions_sel <- input$regions
      rec <- dplyr::case_when(
        length(gravite_sel) == 1 && gravite_sel == "Mortel" ~
          "Statistiques approfondies — Score risque",
        length(gravite_sel) == 1 && gravite_sel == "Grave"  ~
          "Facteurs de risque",
        length(regions_sel) > 0 && length(regions_sel) <= 3 ~
          "Carte de risque",
        length(annees_sel)  > 0 ~
          "Heatmap — tendances temporelles",
        TRUE ~ "Dashboard — vue globale"
      )
      tags$span(style="font-size:13px; font-weight:600;", rec)
    })

    output$sexe_gravite <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(sexe_dominant), !is.na(gravite_accident)) |>
        mutate(sexe=as.character(sexe_dominant), grav=as.character(gravite_accident)) |>
        group_by(sexe, grav) |> summarise(n=n(),.groups="drop") |>
        group_by(sexe) |> mutate(pct=round(n/sum(n)*100,1)) |> ungroup()

      all_gravites <- c("Mortel","Grave","Léger")
      all_couleurs <- c("#e74c3c","#f39c12","#27ae60")
      gravites <- intersect(all_gravites, unique(d$grav))
      couleurs <- all_couleurs[all_gravites %in% gravites]
      if (length(gravites) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      # Initialiser avec la 1re trace pour éviter le warning "discrete/non-discrete"
      dd1 <- d |> dplyr::filter(grav==gravites[1])
      p <- plot_ly(data=dd1, x=~sexe, y=~pct, type="bar", source="sexe_gravite", name=gravites[1],
                   marker=list(color=couleurs[1]),
                   text=~paste0(pct,"%"), textposition="inside")
      if (length(gravites) > 1) {
        for (i in 2:length(gravites)) {
          dd <- d |> dplyr::filter(grav==gravites[i])
          if (nrow(dd) == 0) next
          p <- p |> add_bars(data=dd, x=~sexe, y=~pct, name=gravites[i],
                             marker=list(color=couleurs[i]),
                             text=~paste0(pct,"%"), textposition="inside")
        }
      }
      p <- plotly::event_register(p, "plotly_click")
      p |> layout(barmode="stack", xaxis=list(title=""),
                  yaxis=list(title="% accidents",range=c(0,100)),
                  legend=list(orientation="h",y=-0.2),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5)) |> plotly::config(displayModeBar=FALSE)
    })

    output$age_bar <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- filtered() |>
        filter(!is.na(tranche_age), as.character(tranche_age)!="Non renseigné") |>
        mutate(age=as.character(tranche_age), grav=as.character(gravite_accident)) |>
        group_by(age,grav) |> summarise(n=n(),.groups="drop")

      all_gravites <- c("Mortel","Grave","Léger")
      all_couleurs <- c("#e74c3c","#f39c12","#27ae60")
      gravites <- intersect(all_gravites, unique(d$grav))
      couleurs <- all_couleurs[all_gravites %in% gravites]
      if (length(gravites) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      dd1 <- d |> dplyr::filter(grav==gravites[1])
      p <- plot_ly(data=dd1, x=~age, y=~n, type="bar", source="age_bar",
                   name=gravites[1], marker=list(color=couleurs[1]))
      if (length(gravites) > 1) {
        for (i in 2:length(gravites)) {
          dd <- d |> dplyr::filter(grav==gravites[i])
          if (nrow(dd) == 0) next
          p <- p |> add_bars(data=dd, x=~age, y=~n,
                             name=gravites[i], marker=list(color=couleurs[i]))
        }
      }
      ordre_age2 <- c("0-17 ans","18-24 ans","25-34 ans","35-44 ans",
                      "45-54 ans","55-64 ans","65-74 ans","65 ans et plus")
      p <- plotly::event_register(p, "plotly_click")
      p |> layout(barmode="stack",
                  xaxis=list(title="", tickangle=-30,
                             categoryorder="array",
                             categoryarray=ordre_age2[ordre_age2 %in% d$age]),
                  yaxis=list(title="Nb accidents"),
                  legend=list(orientation="h",y=-0.35),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5,b=80)) |> plotly::config(displayModeBar=FALSE)
    })

    output$age_mortalite <- renderPlotly({
      req(nrow(filtered_all()) > 0)
      ordre_age <- c("0-17 ans","18-24 ans","25-34 ans","35-44 ans",
                     "45-54 ans","55-64 ans","65-74 ans","65 ans et plus")

      # Deux courbes : Homme vs Femme — taux mortalité par tranche d'âge
      d <- filtered_all() |>
        filter(!is.na(tranche_age), !is.na(sexe_dominant),
               !as.character(tranche_age) %in% c("Non renseigné","Non renseigné","Inconnu","")) |>
        mutate(
          age  = as.character(tranche_age),
          sexe = as.character(sexe_dominant)
        ) |>
        group_by(age, sexe) |>
        summarise(
          n    = n(),
          taux = round(mean(gravite_accident == "Mortel", na.rm=TRUE) * 100, 2),
          .groups = "drop"
        )

      ages_ord <- ordre_age[ordre_age %in% unique(d$age)]
      d_h <- d |> dplyr::filter(sexe == "Masculin") |> dplyr::arrange(match(age, ordre_age))
      d_f <- d |> dplyr::filter(sexe == "Féminin") |> dplyr::arrange(match(age, ordre_age))

      plot_ly(source="age_mortalite") |>
        add_trace(data=d_h, x=~age, y=~taux,
                  type="scatter", mode="lines+markers",
                  name="Masculin",
                  line=list(color="#1b3a6b", width=2.5),
                  marker=list(size=8, color="#1b3a6b"),
                  text=~paste0("Homme - ",age,"<br>",taux,"% mortalite"),
                  hoverinfo="text") |>
        add_trace(data=d_f, x=~age, y=~taux,
                  type="scatter", mode="lines+markers",
                  name="Féminin",
                  line=list(color="#e74c3c", width=2.5),
                  marker=list(size=8, color="#e74c3c"),
                  text=~paste0("Femme - ",age,"<br>",taux,"% mortalite"),
                  hoverinfo="text") |> plotly::event_register("plotly_click") |>
        layout(
          xaxis=list(title="", tickangle=-30,
                     categoryorder="array",
                     categoryarray=ages_ord),
          yaxis=list(title="Taux mortalite (%)"),
          legend=list(orientation="h", y=-0.25),
          paper_bgcolor="transparent", plot_bgcolor="transparent",
          margin=list(t=5, b=80)) |> plotly::config(displayModeBar=FALSE)
    })

    output$catu_pie <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(catu_principal)) |>
        mutate(catu=as.character(catu_principal)) |>
        group_by(catu) |> summarise(n=n(),.groups="drop") |> arrange(desc(n))

      p_catu <- plot_ly(d, labels=~catu, values=~n, type="pie", hole=0.4, source="catu_pie",
        marker=list(colors=c("#1b3a6b","#4a6fa5","#7eb3ff","#b8d4ff")),
        textinfo="label+percent")
      p_catu <- plotly::event_register(p_catu, "plotly_click")
      p_catu |>
        layout(showlegend=TRUE, paper_bgcolor="transparent",
               plot_bgcolor="transparent", margin=list(t=5,b=5)) |> plotly::config(displayModeBar=FALSE)
    })

    output$vehicule_gravite <- renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      d <- d |>
        filter(!is.na(categorie_vehicule),
               as.character(categorie_vehicule)!="Non renseigné") |>
        mutate(veh=as.character(categorie_vehicule), grav=as.character(gravite_accident)) |>
        group_by(veh,grav) |> summarise(n=n(),.groups="drop") |>
        group_by(veh) |> mutate(pct=round(n/sum(n)*100,1), total=sum(n)) |>
        ungroup() |> filter(total>=100)

      ordre_veh <- d |> filter(grav=="Mortel") |> arrange(desc(pct)) |> pull(veh)
      all_gravites <- c("Mortel","Grave","Léger")
      all_couleurs  <- c("#e74c3c","#f39c12","#27ae60")
      gravites <- intersect(all_gravites, unique(d$grav))
      couleurs <- all_couleurs[all_gravites %in% gravites]
      if (length(gravites) == 0) return(plot_ly() |> layout(title="Aucune donnee"))
      dd1 <- d |> dplyr::filter(grav==gravites[1])
      p <- plot_ly(data=dd1, x=~pct, y=~veh, type="bar", orientation="h", source="vehicule_gravite",
                   name=gravites[1], marker=list(color=couleurs[1]),
                   text=~paste0(pct,"%"), textposition="inside")
      if (length(gravites) > 1) {
        for (i in 2:length(gravites)) {
          dd <- d |> dplyr::filter(grav==gravites[i])
          if (nrow(dd) == 0) next
          p <- p |> add_bars(data=dd, x=~pct, y=~veh,
                             name=gravites[i], orientation="h",
                             marker=list(color=couleurs[i]),
                             text=~paste0(pct,"%"), textposition="inside")
        }
      }
      p <- plotly::event_register(p, "plotly_click")
      p |> layout(barmode="stack",
                  xaxis=list(title="% accidents",range=c(0,100)),
                  yaxis=list(title=""),
                  legend=list(orientation="h",y=-0.15),
                  paper_bgcolor="transparent", plot_bgcolor="transparent",
                  margin=list(t=5,l=160)) |> plotly::config(displayModeBar=FALSE)
    })

    output$secu <- renderPlotly({
      req(nrow(filtered()) > 0)
      codes_secu <- c(
        "0"="Non renseigné","1"="Ceinture portee","2"="Casque porte",
        "3"="Dispos. enfant","4"="Gilet refl.",
        "8"="Non determine","9"="Autre",
        "11"="Ceinture porte","12"="Ceinture non portee",
        "13"="Casque porte","21"="Casque non porte",
        "22"="Dispos. enfant porte","23"="Dispos. enfant non porte",
        "91"="Gilet porte","92"="Gilet non porte","93"="Autre porte"
      )
      d <- filtered() |>
        filter(!is.na(secu_principal)) |>
        mutate(sc=as.character(secu_principal),
               secu_label=ifelse(sc %in% names(codes_secu), codes_secu[sc], sc)) |>
        group_by(secu_label) |>
        summarise(n=n(), taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        filter(n>=200) |> arrange(desc(n)) |> slice_head(n=10)

      d <- d |> dplyr::mutate(.col = .taux_to_hex(taux))
      p_secu <- plot_ly(d, x=~n, y=~secu_label, type="bar", orientation="h", source="secu",
        marker=list(color=~.col),
        text=~paste0(round(n/1000,1),"k | ",taux,"%"),
        textposition="outside")
      p_secu <- plotly::event_register(p_secu, "plotly_click")
      p_secu |> layout(xaxis=list(title="Nb accidents"),
               yaxis=list(title=""),
               paper_bgcolor="transparent", plot_bgcolor="transparent",
               margin=list(t=5,r=80,l=160)) |> plotly::config(displayModeBar=FALSE)
    })

    output$insights_box <- renderUI({
      d <- filtered()
      if (nrow(d)==0) return(NULL)

      pct_h     <- round(mean(as.character(d$sexe_dominant)=="Masculin",na.rm=TRUE)*100,1)
      pct_jeune <- round(mean(as.character(d$tranche_age) %in%
                                c("18-24 ans","25-34 ans"),na.rm=TRUE)*100,1)
      age_mort <- d |>
        filter(!is.na(tranche_age), as.character(tranche_age)!="Non renseigné") |>
        mutate(age=as.character(tranche_age)) |>
        group_by(age) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)

      veh_mort <- d |>
        filter(!is.na(categorie_vehicule),
               as.character(categorie_vehicule)!="Non renseigné") |>
        mutate(veh=as.character(categorie_vehicule)) |>
        group_by(veh) |>
        summarise(n=n(), taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        filter(n>=100) |> arrange(desc(taux)) |> slice_head(n=1)

      div(class="insight-box",
        tags$ul(style="margin:0;padding-left:20px;line-height:2;",
          tags$li(tags$b(paste0(pct_h,"%")),
                  " des accidents impliquent un conducteur masculin"),
          tags$li("Les ", tags$b("18-34 ans"), " representent ",
                  tags$b(paste0(pct_jeune,"%")),
                  " des accidents — tranche la plus surrepresentee"),
          tags$li("Les ", tags$b(age_mort$age),
                  " ont le taux de mortalite le plus eleve : ",
                  tags$b(paste0(age_mort$taux,"%"))),
          if (nrow(veh_mort)>0)
            tags$li("Le ", tags$b(veh_mort$veh),
                    " est le vehicule le plus mortel : ",
                    tags$b(paste0(veh_mort$taux,"%")))
        )
      )
    })


    output$dl_csv <- downloadHandler(
      filename = function() {
        anns <- if (length(input$annees)  > 0) paste(input$annees, collapse="-") else "tout"
        regs <- if (length(input$regions) > 0) "filtre" else "france"
        paste0("crashalert_profil_", regs, "_", anns, ".csv")
      },
      content = function(file) {
        utils::write.csv(filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ── Side Panel — clic plotly ───────────────────────────────────────────
    panel_trigger <- shiny::reactiveVal(NULL)

    observeEvent(plotly::event_data("plotly_click", source="age_bar"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="age_bar")
      req(!is.null(ev))
      panel_trigger(list(source="age_bar", key=as.character(ev$x[1])))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    observeEvent(plotly::event_data("plotly_click", source="vehicule_gravite"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="vehicule_gravite")
      req(!is.null(ev))
      panel_trigger(list(source="vehicule_gravite", key=as.character(ev$y[1])))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    observeEvent(plotly::event_data("plotly_click", source="secu"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="secu")
      req(!is.null(ev))
      panel_trigger(list(source="secu", key=as.character(ev$y[1])))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    observeEvent(plotly::event_data("plotly_click", source="sexe_gravite"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="sexe_gravite")
      req(!is.null(ev))
      panel_trigger(list(source="sexe_gravite", key=as.character(ev$x[1])))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    observeEvent(plotly::event_data("plotly_click", source="catu_pie"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="catu_pie")
      req(!is.null(ev))
      catu_levels <- app_data$accidents_dashboard |> dplyr::filter(!is.na(catu_principal)) |> dplyr::count(catu_principal, sort=TRUE) |> dplyr::pull(catu_principal) |> as.character()
      key_val <- catu_levels[ev$pointNumber[1] + 1]; panel_trigger(list(source="catu_pie", key=as.character(key_val)))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    observeEvent(plotly::event_data("plotly_click", source="age_mortalite"), ignoreInit=TRUE, {
      ev <- plotly::event_data("plotly_click", source="age_mortalite")
      req(!is.null(ev))
      panel_trigger(list(source="age_mortalite", key=as.character(ev$x[1]), sexe=as.character(ev$curveNumber)))
      session$sendCustomMessage("openPanel", list(id="profile-side-panel"))
    })

    output$side_panel_content <- renderUI({
      trigger <- panel_trigger()
      req(!is.null(trigger))
      d <- filtered()
      req(nrow(d) > 0)

      if (trigger$source == "age_bar") {
        age_sel <- trigger$key
        d_age <- d |>
          dplyr::filter(as.character(tranche_age) == age_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            graves =sum(gravite_accident=="Grave",  na.rm=TRUE),
            .groups="drop")
        req(nrow(d_age) > 0)
        taux_m      <- round(d_age$mortels / d_age$n * 100, 1)
        taux_g      <- round(d_age$graves  / d_age$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        surrisque   <- round(taux_m / max(taux_global, 0.1), 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "👥 Tranche d’âge"),
          div(class=paste("panel-badge", badge_cls), age_sel),
          div(class="panel-value", format(d_age$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Mortalité : "), sprintf("%.1f%%", taux_m), tags$br(),
            tags$b("Graves : "),       sprintf("%.1f%%", taux_g),  tags$br(),
            tags$b("Moyenne sélection : "), sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(surrisque > 1.5) sprintf("Surrisque de %.1fx vs moyenne.", surrisque)
              else if(surrisque < 0.8) "Sous-représenté dans la mortalité relative."
              else "Risque proche de la moyenne."
            )
          )
        )

      } else if (trigger$source == "vehicule_gravite") {
        veh_sel <- trigger$key
        d_veh <- d |>
          dplyr::filter(as.character(categorie_vehicule) == veh_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            .groups="drop")
        req(nrow(d_veh) > 0)
        taux_m      <- round(d_veh$mortels / d_veh$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        pct_total   <- round(d_veh$n / nrow(d) * 100, 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "🚗 Véhicule"),
          div(class=paste("panel-badge", badge_cls), veh_sel),
          div(class="panel-value", format(d_veh$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Part du total : "),   sprintf("%.1f%%", pct_total),  tags$br(),
            tags$b("Mortalité : "),      sprintf("%.1f%%", taux_m),     tags$br(),
            tags$b("Moyenne : "),          sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(taux_m > taux_global*1.5)
                sprintf("%s : mortalité %.1fx la moyenne — vigilance maximale.", veh_sel, round(taux_m/max(taux_global,0.1),1))
              else
                sprintf("%s : profil de risque proche de la moyenne.", veh_sel)
            )
          )
        )

      } else if (trigger$source == "secu") {
        secu_sel <- trigger$key
        codes_secu_map <- c(
          "0"="Non renseigné","1"="Ceinture portee","2"="Casque porte",
          "3"="Dispos. enfant","4"="Gilet refl.",
          "8"="Non determine","9"="Autre",
          "11"="Ceinture porte","12"="Ceinture non portee",
          "13"="Casque porte","21"="Casque non porte",
          "22"="Dispos. enfant porte","23"="Dispos. enfant non porte",
          "91"="Gilet porte","92"="Gilet non porte","93"="Autre porte")
        d_secu <- d |>
          dplyr::filter(!is.na(secu_principal)) |>
          dplyr::mutate(sc=as.character(secu_principal),
            secu_label=ifelse(sc %in% names(codes_secu_map), codes_secu_map[sc], sc)) |>
          dplyr::filter(secu_label == secu_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            .groups="drop")
        req(nrow(d_secu) > 0)
        taux_m      <- round(d_secu$mortels / d_secu$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "🛡 Équipement"),
          div(class=paste("panel-badge", badge_cls), secu_sel),
          div(class="panel-value", format(d_secu$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Mortalité : "), sprintf("%.1f%%", taux_m),     tags$br(),
            tags$b("Moyenne : "),     sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(grepl("non", tolower(secu_sel)))
                "Sans équipement : risque de décès significativement plus élevé."
              else
                "Port de l’équipement associé à une réduction du risque mortel."
            )
          )
        )

      } else if (trigger$source == "sexe_gravite") {
        sexe_sel <- trigger$key
        d_sx <- d |>
          dplyr::filter(as.character(sexe_dominant) == sexe_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            graves =sum(gravite_accident=="Grave",  na.rm=TRUE),
            .groups="drop")
        req(nrow(d_sx) > 0)
        taux_m      <- round(d_sx$mortels / d_sx$n * 100, 1)
        taux_g      <- round(d_sx$graves  / d_sx$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        pct_total   <- round(d_sx$n / nrow(d) * 100, 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "🚹 Sexe"),
          div(class=paste("panel-badge", badge_cls), sexe_sel),
          div(class="panel-value", format(d_sx$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Part du total : "), sprintf("%.1f%%", pct_total), tags$br(),
            tags$b("Mortalité : "),   sprintf("%.1f%%", taux_m),    tags$br(),
            tags$b("Graves : "),       sprintf("%.1f%%", taux_g),    tags$br(),
            tags$b("Moyenne : "),      sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(isTRUE(taux_m > taux_global*1.2))
                sprintf("%s : surmortalité de %.1f points vs moyenne.", sexe_sel, taux_m-taux_global)
              else
                sprintf("%s : profil de risque proche de la moyenne.", sexe_sel)
            )
          )
        )

      } else if (trigger$source == "catu_pie") {
        catu_sel <- trigger$key
        req(nchar(catu_sel) > 0)
        d_ct <- d |>
          dplyr::filter(as.character(catu_principal) == catu_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            graves =sum(gravite_accident=="Grave",  na.rm=TRUE),
            .groups="drop")
        req(nrow(d_ct) > 0)
        taux_m      <- round(d_ct$mortels / d_ct$n * 100, 1)
        taux_g      <- round(d_ct$graves  / d_ct$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        pct_total   <- round(d_ct$n / nrow(d) * 100, 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "🚶 Rôle"),
          div(class=paste("panel-badge", badge_cls), catu_sel),
          div(class="panel-value", format(d_ct$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Part du total : "), sprintf("%.1f%%", pct_total), tags$br(),
            tags$b("Mortalité : "),   sprintf("%.1f%%", taux_m),    tags$br(),
            tags$b("Graves : "),       sprintf("%.1f%%", taux_g),    tags$br(),
            tags$b("Moyenne : "),      sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(catu_sel == "Piéton")
                "Les piétons sont parmi les usagers les plus vulnérables en cas d’accident."
              else if(catu_sel == "Conducteur")
                "Le conducteur est le premier exposé au risque dans la majorité des configurations d’accident."
              else
                "Les passagers subissent le risque sans maîtrise de la situation."
            )
          )
        )

      } else if (trigger$source == "age_mortalite") {
        age_sel  <- trigger$key
        sexe_num <- as.integer(trigger$sexe)
        sexe_sel <- if(isTRUE(sexe_num == 0)) "Masculin" else "Féminin"
        d_am <- d |>
          dplyr::filter(
            as.character(tranche_age) == age_sel,
            as.character(sexe_dominant) == sexe_sel) |>
          dplyr::summarise(
            n=dplyr::n(),
            mortels=sum(gravite_accident=="Mortel", na.rm=TRUE),
            .groups="drop")
        req(nrow(d_am) > 0)
        taux_m      <- round(d_am$mortels / d_am$n * 100, 1)
        taux_global <- round(mean(d$gravite_accident=="Mortel", na.rm=TRUE)*100, 1)
        badge_cls   <- if(isTRUE(taux_m > taux_global*1.5)) "danger" else if(isTRUE(taux_m > taux_global)) "warning" else "success"
        div(
          div(class="panel-title", "📈 Mortalité par âge"),
          div(class=paste("panel-badge", badge_cls), paste(sexe_sel, "—", age_sel)),
          div(class="panel-value", format(d_am$n, big.mark=" "), " accidents"),
          div(class="panel-content",
            tags$b("Taux mortalité : "), sprintf("%.1f%%", taux_m),     tags$br(),
            tags$b("Moyenne sélection : "), sprintf("%.1f%%", taux_global), tags$br(),
            div(class="panel-hint",
              if(isTRUE(taux_m > taux_global*1.5))
                sprintf("%s %s : taux de mortalité élevé, %.1fx la moyenne.", sexe_sel, age_sel, round(taux_m/max(taux_global,0.1),1))
              else
                sprintf("%s %s : risque dans la moyenne de la sélection.", sexe_sel, age_sel)
            )
          )
        )
      }
    })

  })
}
