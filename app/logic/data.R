# app/logic/data.R
# Chargement et préparation des données CrashAlert
# Importé via : box::use(app/logic/data)

box::use(
  dplyr[filter, mutate, group_by, summarise, arrange, desc, n, slice_sample,
        case_when, if_else, coalesce, between, pull],
  stringr[str_trim],
  lubridate[year],
)

#' Charge toutes les données depuis data_propre/
#' @export
load_data <- function(path = "data_propre") {
  list(
    accidents_dashboard = readRDS(file.path(path, "accidents_dashboard.rds")),
    accidents_light     = readRDS(file.path(path, "accidents_light.rds")),
    dept_filters = readRDS(file.path(path, "dept_filters.rds")),
    agg_evolution   = readRDS(file.path(path, "agg_evolution.rds")),
    agg_departement = readRDS(file.path(path, "agg_departement.rds")),
    agg_dept_annee  = readRDS(file.path(path, "agg_dept_annee.rds")),
    agg_vehicules   = readRDS(file.path(path, "agg_vehicules.rds")),
    agg_age         = readRDS(file.path(path, "agg_age.rds")),
    agg_heatmap        = readRDS(file.path(path, "agg_heatmap.rds")),
    agg_meteo          = readRDS(file.path(path, "agg_meteo.rds")),
    agg_heatmap2       = readRDS(file.path(path, "agg_heatmap2.rds")),
    agg_heatmap_mois   = readRDS(file.path(path, "agg_heatmap_mois.rds")),
    agg_facteurs       = readRDS(file.path(path, "agg_facteurs.rds")),
    agg_evol_dept      = readRDS(file.path(path, "agg_evol_dept.rds")),
    agg_evol_france    = readRDS(file.path(path, "agg_evol_france.rds"))
  )
}

#' Prépare les listes de filtres
#' @export
prepare_filters <- function(accidents, dept_filters) {
  list(
    annees       = sort(unique(accidents$annee)),
    regions      = sort(unique(dept_filters$region)),
    departements = dept_filters$departement,
    dept_codes   = dept_filters$dep
  )
}

#' Filtre les données selon les sélections utilisateur
#' @export
filter_accidents <- function(data, annees = NULL, regions = NULL,
                              dept_codes = NULL, gravites = NULL) {
  d <- data

  if (!is.null(annees) && length(annees) > 0)
    d <- d |> filter(annee %in% annees)

  if (!is.null(gravites) && length(gravites) > 0)
    d <- d |> filter(gravite_accident %in% gravites)

  if (!is.null(regions) && length(regions) > 0) {
    d <- d |> filter(region %in% regions)
  }

  if (!is.null(dept_codes) && length(dept_codes) > 0)
    d <- d |> filter(dep %in% dept_codes)

  d
}

#' Calcule les KPI principaux
#' @export
compute_kpis <- function(data) {
  list(
    total        = nrow(data),
    morts        = sum(data$gravite_accident == "Mortel", na.rm = TRUE),
    graves       = sum(data$gravite_accident == "Grave",  na.rm = TRUE),
    legers       = sum(data$gravite_accident == "Léger",  na.rm = TRUE),
    pct_mortel   = round(mean(data$gravite_accident == "Mortel", na.rm = TRUE) * 100, 1),
    age_moyen    = round(mean(data[["age_moyen_reel"]] %||%
                              data[["age_moyen"]]      %||%
                              data[["age"]], na.rm = TRUE), 1)
  )
}

# Opérateur null-coalesce (privé)
`%||%` <- function(a, b) if (!is.null(a)) a else b
