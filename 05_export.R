# ==============================================================================
# 05_export.R — Export des fichiers RDS finaux pour CrashAlert
# ==============================================================================
# Dépendances : 00_config.R + 01 + 02 + 03 + 04 sourcés avant.
# Entrée      : accidents enrichi (data frame complet)
# Produits    :
#   data_pipeline/accidents_YYYY.rds          — archive année brute
#   data_propre/accidents_dashboard.rds       — fichier Shiny principal
#   data_propre/accidents_light.rds           — fichier analyses approfondies
#   data_propre/filters.rds                   — filtres UI
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

log_msg("=== 05_export.R — début ===")


# ------------------------------------------------------------------------------
# 1. ARCHIVE ANNÉE — sauvegarde complète de l'année traitée
# ------------------------------------------------------------------------------

log_msg(sprintf("Sauvegarde archive année %d...", ANNEE_CIBLE))

saveRDS(
  accidents,
  file.path(CHEMIN_OUTPUT, paste0("accidents_", ANNEE_CIBLE, ".rds"))
)

log_msg(sprintf("Archive : %d accidents, %d colonnes",
                nrow(accidents), ncol(accidents)))


# ------------------------------------------------------------------------------
# 2. CONCATÉNATION MULTI-ANNÉES
# ------------------------------------------------------------------------------
# On charge toutes les archives disponibles et on les empile.
# Cela permet d'ajouter une nouvelle année sans retraiter les précédentes.

log_msg("Concaténation de toutes les années disponibles...")

archives <- list.files(
  CHEMIN_OUTPUT,
  pattern  = "^accidents_[0-9]{4}\\.rds$",
  full.names = TRUE
)

if (length(archives) == 0) {
  stop("Aucune archive année trouvée dans ", CHEMIN_OUTPUT)
}

log_msg(sprintf("Archives trouvées : %s",
  paste(basename(archives), collapse = ", ")))

accidents_all <- lapply(archives, function(f) {
  d <- readRDS(f)
  annee_f <- as.numeric(gsub(".*accidents_([0-9]{4})\\.rds", "\\1", basename(f)))
  log_msg(sprintf("  Chargement %s : %d lignes", basename(f), nrow(d)))
  d
}) |> bind_rows()

log_msg(sprintf("Base consolidée : %d accidents sur %d années",
                nrow(accidents_all),
                n_distinct(accidents_all$annee, na.rm = TRUE)))


# ------------------------------------------------------------------------------
# 3. ACCIDENTS_DASHBOARD.RDS — fichier principal Shiny
# ------------------------------------------------------------------------------
# Colonnes définies dans COLONNES_DASHBOARD (00_config.R)
# Variables converties en facteurs pour économiser la mémoire

log_msg("Création de accidents_dashboard.rds...")

cols_dashboard <- intersect(COLONNES_DASHBOARD, names(accidents_all))
cols_manquantes <- setdiff(COLONNES_DASHBOARD, names(accidents_all))
if (length(cols_manquantes) > 0) {
  log_msg(sprintf("Colonnes absentes du dashboard : %s",
                  paste(cols_manquantes, collapse = ", ")), "WARN")
}

.FACTEURS_DASHBOARD <- c(
  "gravite_accident", "region", "dep", "departement",
  "lum_label", "atm_label", "col_label", "catr_label", "surf_label",
  "type_route", "moment_journee", "categorie_vehicule"
)

accidents_dashboard <- accidents_all |>
  select(all_of(cols_dashboard)) |>
  mutate(across(
    all_of(intersect(.FACTEURS_DASHBOARD, cols_dashboard)),
    factor
  ))

taille_db <- round(object.size(accidents_dashboard) / 1024^2, 1)
log_msg(sprintf("accidents_dashboard : %d lignes × %d colonnes — %.1f MB RAM",
                nrow(accidents_dashboard), ncol(accidents_dashboard), taille_db))

saveRDS(
  accidents_dashboard,
  file.path(CHEMIN_SHINY, "accidents_dashboard.rds"),
  compress = "xz"
)
log_msg("accidents_dashboard.rds sauvegardé")


# ------------------------------------------------------------------------------
# 4. ACCIDENTS_LIGHT.RDS — fichier analyses approfondies
# ------------------------------------------------------------------------------
# Contient les colonnes dashboard + colonnes supplémentaires (light extra)
# Utilisé par l'onglet Statistiques approfondies (clustering, ACM, profil)

log_msg("Création de accidents_light.rds...")

cols_light <- intersect(
  c(COLONNES_DASHBOARD, COLONNES_LIGHT_EXTRA),
  names(accidents_all)
)
cols_light_manquantes <- setdiff(COLONNES_LIGHT_EXTRA, names(accidents_all))
if (length(cols_light_manquantes) > 0) {
  log_msg(sprintf("Colonnes light absentes : %s",
                  paste(cols_light_manquantes, collapse = ", ")), "WARN")
}

.FACTEURS_LIGHT <- c(.FACTEURS_DASHBOARD,
  "sexe_dominant", "tranche_age", "catu_principal", "secu_principal",
  "jour_semaine", "saison"
)

accidents_light <- accidents_all |>
  select(all_of(cols_light)) |>
  mutate(across(
    all_of(intersect(.FACTEURS_LIGHT, cols_light)),
    factor
  ))

taille_light <- round(object.size(accidents_light) / 1024^2, 1)
log_msg(sprintf("accidents_light : %d lignes × %d colonnes — %.1f MB RAM",
                nrow(accidents_light), ncol(accidents_light), taille_light))

saveRDS(
  accidents_light,
  file.path(CHEMIN_SHINY, "accidents_light.rds"),
  compress = "xz"
)
log_msg("accidents_light.rds sauvegardé")


# ------------------------------------------------------------------------------
# 5. FILTERS.RDS — filtres UI pour Shiny
# ------------------------------------------------------------------------------

log_msg("Création de filters.rds...")

filters <- list(

  # Départements disponibles (pour selectInput)
  departements = accidents_all |>
    filter(!is.na(dep), !is.na(departement), !is.na(region)) |>
    distinct(dep, departement, region) |>
    arrange(region, departement),

  # Années disponibles
  annees = sort(unique(na.omit(accidents_all$annee))),

  # Régions disponibles
  regions = sort(unique(na.omit(as.character(accidents_all$region)))),

  # Gravités
  gravites = c("Mortel", "Blessé hospitalisé", "Blessé léger", "Indemne"),

  # Types de route
  types_route = accidents_all |>
    filter(!is.na(catr_label)) |>
    distinct(catr_label) |>
    arrange(catr_label) |>
    pull(catr_label),

  # Conditions météo
  meteo = accidents_all |>
    filter(!is.na(atm_label)) |>
    distinct(atm_label) |>
    arrange(atm_label) |>
    pull(atm_label)
)

saveRDS(filters, file.path(CHEMIN_SHINY, "filters.rds"))
log_msg(sprintf("filters.rds : %d départements, %d années",
                nrow(filters$departements), length(filters$annees)))


# ------------------------------------------------------------------------------
# 6. RAPPORT FINAL
# ------------------------------------------------------------------------------

log_msg("=== RAPPORT FINAL ===")
log_msg(sprintf("Années traitées     : %s",
                paste(sort(filters$annees), collapse = ", ")))
log_msg(sprintf("Total accidents     : %s",
                format(nrow(accidents_all), big.mark = " ")))
log_msg(sprintf("accidents_dashboard : %.1f MB RAM", taille_db))
log_msg(sprintf("accidents_light     : %.1f MB RAM", taille_light))
log_msg(sprintf("Fichiers dans %s :", CHEMIN_SHINY))
log_msg("  accidents_dashboard.rds")
log_msg("  accidents_light.rds")
log_msg("  filters.rds")

# Alerte OOM si trop lourd pour shinyapps.io
if (taille_db + taille_light > 800) {
  log_msg(sprintf(
    "ALERTE OOM : dashboard (%.0fMB) + light (%.0fMB) = %.0fMB > 800MB",
    taille_db, taille_light, taille_db + taille_light), "WARN")
}

log_msg("=== 05_export.R — terminé ===")
