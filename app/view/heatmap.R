# app/view/heatmap.R
box::use(
  shiny[NS, moduleServer, tags, tagList, uiOutput, renderUI,
        textOutput, renderText, HTML, div, span, p, selectInput,
        updateSelectInput, observe, reactive, req,
        downloadButton, downloadHandler],
  bslib[layout_columns, card, card_header, card_body, value_box],
  bsicons[bs_icon],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  dplyr[filter, mutate, group_by, summarise, n, arrange, desc,
        slice_head, ungroup, pull, left_join, case_when],
)

.JOURS_ORDER <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
.JOURS_LABEL <- c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim")
.MOIS_LABEL  <- c("Jan","Fév","Mar","Avr","Mai","Jun",
                  "Jul","Aoû","Sep","Oct","Nov","Déc")
.MOMENTS_ORDER <- c("Nuit profonde (0h-6h)","Matin (6h-9h)","Matinée (9h-12h)",
                    "Midi (12h-14h)","Après-midi (14h-17h)","Soirée (17h-20h)",
                    "Nuit (20h-0h)")

#' @export
ui_heatmap <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .hm-topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 20px; background: #fff;
        border-bottom: 1px solid #e9ecef;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        flex-wrap: wrap; gap: 10px;
      }
      .hm-filters { display: flex; align-items: center; gap: 16px; flex-wrap: wrap; }
      .hm-label { font-size: 12px; font-weight: 600; color: #6c757d;
                  margin: 0; white-space: nowrap; }
      .hm-content { padding: 16px; }
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
    ")),

    div(class = "hm-topbar",
      div(class = "hm-filters",
        div(tags$label(class="hm-label", bs_icon("calendar3"), " Annees"),
            selectInput(ns("annees"), label=NULL, choices=NULL,
                        multiple=TRUE, width="160px")),
        div(tags$label(class="hm-label", bs_icon("geo-alt"), " Region"),
            selectInput(ns("regions"), label=NULL, choices=NULL,
                        multiple=TRUE, width="180px")),
        div(tags$label(class="hm-label", bs_icon("signpost-split"), " Département"),
            selectInput(ns("deps"), label=NULL, choices=NULL,
                        multiple=TRUE, width="180px")),
        div(tags$label(class="hm-label", bs_icon("exclamation-triangle"), " Gravite"),
            selectInput(ns("gravite"), label=NULL,
                        choices=c("Mortel","Grave","L\u00e9ger"),
                        selected=NULL, multiple=TRUE, width="160px"))
      ),
      div(style="color:#6c757d;font-size:12px;display:flex;align-items:center;gap:10px;",
          uiOutput(ns("data_summary"), inline=TRUE),
          downloadButton(ns("dl_csv"), label=" CSV",
            icon = shiny::icon("download"),
            class = "btn btn-sm btn-outline-secondary",
            style = "font-size:11px;padding:3px 9px;")
      )
    ),

    div(class = "hm-content",

      # ── KPI temporels ───────────────────────────────────────────────────────
      div(class="section-title", bs_icon("clock-fill"), " Moments cles"),
      layout_columns(
        col_widths=c(3,3,3,3), fill=FALSE,
        value_box("Heure la + mortelle",
          textOutput(ns("kpi_heure"), inline=TRUE),
          showcase=bs_icon("moon-stars-fill"), theme="danger",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_heure_sub"), inline=TRUE))),
        value_box("Jour le + accidente",
          textOutput(ns("kpi_jour"), inline=TRUE),
          showcase=bs_icon("calendar-week"), theme="warning",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_jour_sub"), inline=TRUE))),
        value_box("Mois le + dangereux",
          textOutput(ns("kpi_mois"), inline=TRUE),
          showcase=bs_icon("sun-fill"), theme="warning",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_mois_sub"), inline=TRUE))),
        value_box("Saison la + meurtriere",
          textOutput(ns("kpi_saison"), inline=TRUE),
          showcase=bs_icon("thermometer-half"), theme="primary",
          p(class="mb-0 text-muted", style="font-size:11px;",
            textOutput(ns("kpi_saison_sub"), inline=TRUE)))
      ),

      # ── Heatmap principale heure x jour ────────────────────────────────────
      div(class="section-title", bs_icon("grid-3x3-gap-fill"), " Heatmap Heure x Jour"),
      layout_columns(
        col_widths=c(8,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("grid-3x3-gap-fill"),
                              " Volume accidents — Heure x Jour de la semaine")),
          card_body(plotlyOutput(ns("heatmap_heure_jour"), height="380px"))
        ),
        card(full_screen=TRUE,
          card_header(uiOutput(ns("titre_moment_mortalite"))),
          card_body(plotlyOutput(ns("moment_mortalite"), height="380px"))
        )
      ),

      tags$div(class="mt-3"),

      # ── Heatmap mois x jour ────────────────────────────────────────────────
      div(class="section-title", bs_icon("calendar3"), " Saisonnalite"),
      layout_columns(
        col_widths=c(8,4),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("calendar3"),
                              " Volume accidents — Mois x Jour de la semaine")),
          card_body(plotlyOutput(ns("heatmap_mois_jour"), height="320px"))
        ),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("bar-chart-fill"), " Accidents par saison")),
          card_body(plotlyOutput(ns("saison_bar"), height="320px"))
        )
      ),

      tags$div(class="mt-3"),

      # ── Courbes horaires ───────────────────────────────────────────────────
      div(class="section-title", bs_icon("graph-up"), " Profil horaire"),
      layout_columns(
        col_widths=c(6,6),
        card(full_screen=TRUE,
          card_header(tagList(bs_icon("graph-up"),
                              " Accidents par heure — semaine vs weekend")),
          card_body(plotlyOutput(ns("heure_semaine_weekend"), height="280px"))
        ),
        card(full_screen=TRUE,
          card_header(uiOutput(ns("titre_heure_mortalite"))),
          card_body(plotlyOutput(ns("heure_mortalite"), height="280px"))
        )
      ),

      tags$div(class="mt-3"),

      div(class="section-title", bs_icon("lightbulb-fill"), " Insights cles"),
      uiOutput(ns("insights_box"))
    )
  )
}

#' @export
server_heatmap <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      annees  <- sort(unique(app_data$accidents_dashboard$annee))
      regions <- sort(unique(as.character(app_data$accidents_dashboard$region)))
      regions <- regions[!is.na(regions)]
      deps    <- sort(unique(as.character(app_data$accidents_dashboard$departement)))
      deps    <- deps[!is.na(deps)]
      updateSelectInput(session, "annees",  choices=as.character(annees), selected=character(0))
      updateSelectInput(session, "regions", choices=regions,              selected=character(0))
      updateSelectInput(session, "deps",    choices=deps,                 selected=character(0))
    })

    # Synchronisation région → département
    observe({
      req(length(input$regions) > 0)
      deps_filtered <- app_data$accidents_dashboard |>
        dplyr::filter(as.character(region) %in% input$regions) |>
        dplyr::pull(departement) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "deps", choices=deps_filtered, selected=character(0))
    })

    # Synchronisation département → région
    observe({
      req(length(input$deps) > 0)
      regs_filtered <- app_data$accidents_dashboard |>
        dplyr::filter(as.character(departement) %in% input$deps) |>
        dplyr::pull(region) |> as.character() |> unique() |> sort()
      updateSelectInput(session, "regions", choices=regs_filtered, selected=input$regions)
    })

    filtered <- reactive({
      d <- app_data$accidents_dashboard
      if (length(input$annees)  > 0) d <- d |> filter(annee %in% as.numeric(input$annees))
      if (length(input$regions) > 0) d <- d |> filter(as.character(region) %in% input$regions)
      if (length(input$gravite) > 0) d <- d |> filter(gravite_accident %in% input$gravite)
      if (length(input$deps)    > 0) d <- d |> filter(as.character(departement) %in% input$deps)
      d |> mutate(
        heure_num = as.integer(heure),
        jour      = as.character(jour_semaine),
        mois_num  = as.integer(mois),
        weekend   = jour %in% c("samedi","dimanche")
      )
    })

    # Données sans filtre gravité — pour calculer les taux réels de mortalité/gravité
    filtered_all <- reactive({
      d <- app_data$accidents_dashboard
      if (length(input$annees)  > 0) d <- d |> filter(annee %in% as.numeric(input$annees))
      if (length(input$regions) > 0) d <- d |> filter(as.character(region) %in% input$regions)
      d |> mutate(
        heure_num = as.integer(heure),
        jour      = as.character(jour_semaine),
        mois_num  = as.integer(mois),
        weekend   = jour %in% c("samedi","dimanche"),
        moment_journee = dplyr::case_when(
          heure_num >= 6  & heure_num <  9  ~ "Matin (6h-9h)",
          heure_num >= 9  & heure_num < 12  ~ "Matinee (9h-12h)",
          heure_num >= 12 & heure_num < 14  ~ "Midi (12h-14h)",
          heure_num >= 14 & heure_num < 17  ~ "Apres-midi (14h-17h)",
          heure_num >= 17 & heure_num < 20  ~ "Soiree (17h-20h)",
          heure_num >= 20 | heure_num <  0  ~ "Nuit (20h-0h)",
          heure_num >= 0  & heure_num <  6  ~ "Nuit profonde (0h-6h)",
          TRUE ~ "Inconnu"
        )
      )
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
    output$kpi_heure <- renderText({
      d <- filtered() |>
        group_by(heure) |>
        summarise(taux=mean(gravite_accident=="Mortel",na.rm=TRUE), .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)
      paste0(d$heure, "h00")
    })
    output$kpi_heure_sub <- renderText({
      d <- filtered() |>
        group_by(heure) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)
      paste0("taux mortalite : ", d$taux, "%")
    })

    output$kpi_jour <- renderText({
      d <- filtered() |>
        group_by(jour) |> summarise(n=n(), .groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)
      tools::toTitleCase(d$jour)
    })
    output$kpi_jour_sub <- renderText({
      d <- filtered() |>
        group_by(jour) |> summarise(n=n(), .groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)
      paste0(format(d$n, big.mark="\u00a0"), " accidents")
    })

    output$kpi_mois <- renderText({
      d <- filtered() |>
        group_by(mois_num) |> summarise(n=n(), .groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)
      .MOIS_LABEL[d$mois_num]
    })
    output$kpi_mois_sub <- renderText({
      d <- filtered() |>
        group_by(mois_num) |> summarise(n=n(), .groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)
      paste0(format(d$n, big.mark="\u00a0"), " accidents en moyenne")
    })

    output$kpi_saison <- renderText({
      d <- filtered() |>
        group_by(saison) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)
      as.character(d$saison)
    })
    output$kpi_saison_sub <- renderText({
      d <- filtered() |>
        group_by(saison) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)
      paste0("taux mortalite : ", d$taux, "%")
    })

    # ── Heatmap heure x jour ─────────────────────────────────────────────────
    output$heatmap_heure_jour <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- filtered() |>
        filter(!is.na(heure_num), !is.na(jour)) |>
        group_by(heure_num, jour) |>
        summarise(n=n(), .groups="drop")

      # Matrice heure (0-23) x jour
      heures <- 0:23
      mat <- matrix(0, nrow=length(heures), ncol=length(.JOURS_ORDER),
                    dimnames=list(as.character(heures), .JOURS_ORDER))
      for (i in seq_len(nrow(d))) {
        h <- as.character(d$heure_num[i])
        j <- d$jour[i]
        if (h %in% rownames(mat) && j %in% colnames(mat))
          mat[h, j] <- d$n[i]
      }

      plot_ly(
        x = .JOURS_LABEL,
        y = paste0(heures, "h"),
        z = mat,
        type = "heatmap",
        colorscale = list(c(0,"#f8f9fa"),c(0.3,"#ffd43b"),c(0.7,"#f39c12"),c(1,"#dc3545")),
        hovertemplate = "%{x} %{y}<br>%{z} accidents<extra></extra>"
      ) |>
        layout(
          xaxis = list(title=""),
          yaxis = list(title="Heure", autorange="reversed"),
          paper_bgcolor="transparent",
          margin=list(t=5, l=50)
        )
    })

    # ── Moment journee x mortalite ────────────────────────────────────────────
    # ── Titres dynamiques selon présence de mortels ───────────────────────────
    .titre_metrique <- function(raw, icon_name, label_base) {
      has_mortel <- "Mortel" %in% unique(as.character(raw$gravite_accident))
      label <- if (has_mortel) paste(label_base, "(mortalite)")
               else            paste(label_base, "(gravite)")
      tagList(bs_icon(icon_name), " ", label)
    }

    output$titre_moment_mortalite <- renderUI({
      .titre_metrique(filtered(), "moon-fill", "Taux par moment de journee")
    })

    output$titre_heure_mortalite <- renderUI({
      .titre_metrique(filtered(), "percent", "Taux par heure de la journee")
    })

    output$moment_mortalite <- renderPlotly({
      req(nrow(filtered()) > 0)
      # filtered_all = meme region/annee, TOUTES gravites → denominateur juste
      base <- filtered_all() |> filter(!is.na(moment_journee)) |>
        mutate(moment=as.character(moment_journee))

      grav_sel  <- if (length(input$gravite) > 0) input$gravite
                   else c("Mortel","Grave","Léger")
      has_mortel <- "Mortel" %in% grav_sel

      # Titre et métrique adaptés à la sélection
      if (has_mortel) {
        metric_label <- "Taux mortalite (%)"
        taux_fn <- function(grav) round(mean(grav == "Mortel", na.rm=TRUE) * 100, 2)
      } else if ("Grave" %in% grav_sel) {
        metric_label <- "% accidents graves dans la periode"
        taux_fn <- function(grav) round(mean(grav %in% c("Mortel","Grave"), na.rm=TRUE) * 100, 2)
      } else {
        # Léger seul : % accidents légers = complémentaire de grave+mortel
        metric_label <- "% accidents legers dans la periode"
        taux_fn <- function(grav) round(mean(grav == "Léger", na.rm=TRUE) * 100, 2)
      }

      d <- base |>
        group_by(moment) |>
        summarise(taux=taux_fn(gravite_accident), n=n(), .groups="drop")

      d <- d |> dplyr::mutate(.col = .taux_to_hex(taux))
      plot_ly(d,
        x=~taux, y=~moment,
        type="bar", orientation="h",
        marker=list(color=~.col),
        text=~paste0(taux,"%"), textposition="outside") |>
        layout(
          xaxis=list(title=metric_label),
          yaxis=list(title=""),
          paper_bgcolor="transparent", plot_bgcolor="transparent",
          margin=list(t=5, r=50, l=170))
    })

    # ── Heatmap mois x jour ───────────────────────────────────────────────────
    output$heatmap_mois_jour <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- filtered() |>
        filter(!is.na(mois_num), !is.na(jour)) |>
        group_by(mois_num, jour) |>
        summarise(n=n(), .groups="drop")

      mat <- matrix(0, nrow=12, ncol=length(.JOURS_ORDER),
                    dimnames=list(as.character(1:12), .JOURS_ORDER))
      for (i in seq_len(nrow(d))) {
        m <- as.character(d$mois_num[i])
        j <- d$jour[i]
        if (m %in% rownames(mat) && j %in% colnames(mat))
          mat[m, j] <- d$n[i]
      }

      plot_ly(
        x = .JOURS_LABEL,
        y = .MOIS_LABEL,
        z = mat,
        type = "heatmap",
        colorscale = list(c(0,"#f8f9fa"),c(0.3,"#ffd43b"),c(0.7,"#f39c12"),c(1,"#dc3545")),
        hovertemplate = "%{x} %{y}<br>%{z} accidents<extra></extra>"
      ) |>
        layout(
          xaxis = list(title=""),
          yaxis = list(title="", autorange="reversed"),
          paper_bgcolor="transparent",
          margin=list(t=5, l=50)
        )
    })

    # ── Saison bar ────────────────────────────────────────────────────────────
    output$saison_bar <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- filtered() |>
        filter(!is.na(saison)) |>
        mutate(s=as.character(saison)) |>
        group_by(s) |>
        summarise(
          n=n(),
          mortels=sum(gravite_accident=="Mortel",na.rm=TRUE),
          taux=round(mortels/n*100,2),
          .groups="drop"
        ) |>
        arrange(desc(n))

      couleurs_saison <- c("Été"="#f39c12","Automne"="#e74c3c",
                           "Printemps"="#27ae60","Hiver"="#4a6fa5")

      plot_ly(d, x=~s, y=~n, type="bar",
        marker=list(color=sapply(d$s, function(x)
          ifelse(x %in% names(couleurs_saison), couleurs_saison[x], "#95a5a6"))),
        text=~paste0(format(n,big.mark="\u00a0"),"<br>",taux,"% mort."),
        textposition="outside") |>
        layout(
          xaxis=list(title=""),
          yaxis=list(title="Nb accidents"),
          paper_bgcolor="transparent", plot_bgcolor="transparent",
          margin=list(t=30))
    })

    # ── Courbe heure semaine vs weekend ───────────────────────────────────────
    output$heure_semaine_weekend <- renderPlotly({
      req(nrow(filtered()) > 0)
      d <- filtered() |>
        filter(!is.na(heure_num)) |>
        group_by(heure_num, weekend) |>
        summarise(n=n(), .groups="drop")

      heures_lbl <- paste0(0:23, "h")
      p <- plot_ly()
      for (w in c(FALSE, TRUE)) {
        dd <- d |> filter(weekend==w)
        # Compléter les heures manquantes
        all_h <- data.frame(heure_num=0:23)
        dd <- merge(all_h, dd, by="heure_num", all.x=TRUE)
        dd[is.na(dd$n), "n"] <- 0
        dd <- dd[order(dd$heure_num), ]
        p <- p |> add_trace(
          x=~paste0(heure_num,"h"),
          y=~n, data=dd,
          type="scatter", mode="lines+markers",
          name=ifelse(w,"Weekend","Semaine"),
          line=list(color=ifelse(w,"#e74c3c","#1b3a6b"), width=2.5),
          marker=list(color=ifelse(w,"#e74c3c","#1b3a6b"), size=5)
        )
      }
      p |> layout(
        xaxis=list(title="Heure", tickangle=-45,
                   categoryorder="array", categoryarray=heures_lbl),
        yaxis=list(title="Nb accidents"),
        legend=list(orientation="h", y=-0.3),
        paper_bgcolor="transparent", plot_bgcolor="transparent",
        margin=list(t=5, b=80))
    })

    # ── Taux mortalite par heure ───────────────────────────────────────────────
    output$heure_mortalite <- renderPlotly({
      req(nrow(filtered()) > 0)
      base <- filtered_all() |> filter(!is.na(heure_num))

      grav_sel   <- if (length(input$gravite) > 0) input$gravite
                    else c("Mortel","Grave","Léger")
      has_mortel <- "Mortel" %in% grav_sel

      if (has_mortel) {
        metric_label <- "Taux mortalite (%)"
        taux_fn <- function(grav) round(mean(grav == "Mortel", na.rm=TRUE) * 100, 2)
      } else if ("Grave" %in% grav_sel) {
        metric_label <- "% accidents graves dans la periode"
        taux_fn <- function(grav) round(mean(grav %in% c("Mortel","Grave"), na.rm=TRUE) * 100, 2)
      } else {
        metric_label <- "% accidents legers dans la periode"
        taux_fn <- function(grav) round(mean(grav == "Léger", na.rm=TRUE) * 100, 2)
      }

      d <- base |>
        group_by(heure_num) |>
        summarise(taux=taux_fn(gravite_accident), n=n(), .groups="drop") |>
        arrange(heure_num)

      heures_lbl <- paste0(0:23, "h")

      d <- d |> dplyr::mutate(.col = .taux_to_hex(taux))
      plot_ly(d,
        x=~paste0(heure_num,"h"),
        y=~taux, type="bar",
        marker=list(color=~.col),
        text=~paste0(taux,"%"), textposition="outside") |>
        layout(
          xaxis=list(title="Heure", tickangle=-45,
                     categoryorder="array", categoryarray=heures_lbl),
          yaxis=list(title=metric_label),
          paper_bgcolor="transparent", plot_bgcolor="transparent",
          margin=list(t=30, b=80))
    })

    # ── Insights ──────────────────────────────────────────────────────────────
    output$insights_box <- renderUI({
      d <- filtered()
      if (nrow(d)==0) return(NULL)

      heure_mort <- d |>
        group_by(heure) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  n=n(), .groups="drop") |>
        arrange(desc(taux)) |> slice_head(n=1)

      heure_peak <- d |>
        group_by(heure) |> summarise(n=n(),.groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)

      jour_peak <- d |>
        group_by(jour) |> summarise(n=n(),.groups="drop") |>
        arrange(desc(n)) |> slice_head(n=1)

      nuit_taux <- d |>
        filter(as.character(moment_journee) %in%
                 c("Nuit profonde (0h-6h)","Nuit (20h-0h)")) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1)) |>
        pull(taux)

      jour_taux <- d |>
        filter(as.character(moment_journee) %in%
                 c("Matin (6h-9h)","Matinée (9h-12h)","Après-midi (14h-17h)")) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1)) |>
        pull(taux)

      weekend_taux <- d |>
        group_by(weekend) |>
        summarise(taux=round(mean(gravite_accident=="Mortel",na.rm=TRUE)*100,1),
                  .groups="drop")
      t_we <- weekend_taux[weekend_taux$weekend==TRUE,"taux"][[1]]
      t_sem <- weekend_taux[weekend_taux$weekend==FALSE,"taux"][[1]]

      div(class="insight-box",
        tags$ul(style="margin:0;padding-left:20px;line-height:2;",
          tags$li("L'heure la plus meurtriere est ",
                  tags$b(paste0(heure_mort$heure,"h00")),
                  " avec un taux de mortalite de ",
                  tags$b(paste0(heure_mort$taux,"%"))),
          tags$li("Le pic d'accidents se situe a ",
                  tags$b(paste0(heure_peak$heure,"h00")),
                  " — ", tags$b(format(heure_peak$n,big.mark="\u00a0")),
                  " accidents sur la periode"),
          tags$li(tags$b(tools::toTitleCase(jour_peak$jour)),
                  " est le jour le plus accidente"),
          tags$li("La nuit (20h-6h) est ",
                  tags$b(paste0(round(nuit_taux/max(jour_taux,0.1),1),"x")),
                  " plus meurtriere que la journee (",
                  tags$b(paste0(nuit_taux,"%")), " vs ",
                  tags$b(paste0(jour_taux,"%")), ")"),
          tags$li("Le weekend a un taux mortalite de ",
                  tags$b(paste0(t_we,"%")), " vs ",
                  tags$b(paste0(t_sem,"%")), " en semaine")
        )
      )
    })


    output$dl_csv <- downloadHandler(
      filename = function() {
        anns <- if (length(input$annees)  > 0) paste(input$annees, collapse="-") else "tout"
        regs <- if (length(input$regions) > 0) "filtre" else "france"
        paste0("crashalert_heatmap_", regs, "_", anns, ".csv")
      },
      content = function(file) {
        utils::write.csv(filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

  })
}
