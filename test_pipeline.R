# ==============================================================================
# test_pipeline.R — Validation du pipeline BAAC sans exécution complète
# ==============================================================================
# Ce script vérifie :
#   1. La présence des fichiers CSV attendus
#   2. La lisibilité de chaque CSV
#   3. La présence des colonnes critiques
#   4. La cohérence des RDS intermédiaires si déjà produits
#   5. La taille mémoire estimée des fichiers finaux
#
# Usage : source("test_pipeline.R")
# Aucune modification de données — lecture seule.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("00_config.R")

.ok  <- function(msg) cat(sprintf("  \u2705 %s\n", msg))
.warn <- function(msg) cat(sprintf("  \u26a0\ufe0f  %s\n", msg))
.err <- function(msg) cat(sprintf("  \u274c %s\n", msg))
.titre <- function(msg) cat(sprintf("\n--- %s ---\n", msg))


# ==============================================================================
# TEST 1 : Présence des fichiers CSV
# ==============================================================================
.titre(sprintf("TEST 1 — Fichiers CSV pour l'année %d", ANNEE_CIBLE))

resultats_csv <- list()

for (type in names(PATTERNS_CSV)) {
  patterns <- PATTERNS_CSV[[type]]
  candidats <- list.files(
    CHEMIN_DONNEES,
    pattern    = paste0("(", paste(patterns, collapse = "|"), ").*",
                        ANNEE_CIBLE, "\\.csv$"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(candidats) == 0) {
    .err(sprintf("%-20s : INTROUVABLE dans %s", type, CHEMIN_DONNEES))
    resultats_csv[[type]] <- NULL
  } else {
    taille_mb <- round(file.size(candidats[1]) / 1024^2, 1)
    .ok(sprintf("%-20s : %s (%.1f MB)", type, basename(candidats[1]), taille_mb))
    resultats_csv[[type]] <- candidats[1]
  }
}

n_csv_ok <- sum(!sapply(resultats_csv, is.null))
if (n_csv_ok < 4) {
  cat(sprintf("\n  %d/4 fichiers trouvés — corriger les chemins avant de lancer le pipeline.\n",
              n_csv_ok))
} else {
  cat("\n  4/4 fichiers présents.\n")
}


# ==============================================================================
# TEST 2 : Lisibilité et colonnes critiques
# ==============================================================================
.titre("TEST 2 — Lisibilité des CSV et colonnes critiques")

.COLONNES_CRITIQUES <- list(
  caracteristiques = c("Num_Acc", "an", "mois", "jour", "hrmn",
                       "lum", "atm", "col", "dep"),
  lieux            = c("Num_Acc", "catr", "surf"),
  usagers          = c("Num_Acc", "grav", "catu", "sexe", "an_nais"),
  vehicules        = c("Num_Acc", "catv", "num_veh")
)

for (type in names(resultats_csv)) {
  fichier <- resultats_csv[[type]]
  if (is.null(fichier)) next

  tryCatch({
    # Lire les 5 premières lignes pour détecter le format
    premiere_ligne <- read_lines(fichier, n_max = 1)
    sep <- if (grepl(";", premiere_ligne)) ";" else ","

    apercu <- read_delim(fichier, delim = sep, n_max = 5,
                         col_types = cols(.default = col_character()),
                         show_col_types = FALSE)

    # Standardiser Num_Acc
    if ("Accident_Id" %in% names(apercu)) {
      apercu <- rename(apercu, Num_Acc = Accident_Id)
    }

    cols_presentes  <- names(apercu)
    cols_critiques  <- .COLONNES_CRITIQUES[[type]]
    cols_manquantes <- setdiff(cols_critiques, cols_presentes)

    if (length(cols_manquantes) == 0) {
      .ok(sprintf("%-20s : %d colonnes lues, toutes les colonnes critiques présentes",
                  type, ncol(apercu)))
    } else {
      .warn(sprintf("%-20s : colonnes critiques manquantes : %s",
                    type, paste(cols_manquantes, collapse = ", ")))
    }

    # Vérifier le séparateur détecté
    if (sep == ";") {
      .ok(sprintf("%-20s : séparateur ';' détecté (format 2019+)", type))
    } else {
      .ok(sprintf("%-20s : séparateur ',' détecté (format 2015-2018)", type))
    }

  }, error = function(e) {
    .err(sprintf("%-20s : erreur de lecture — %s", type, conditionMessage(e)))
  })
}


# ==============================================================================
# TEST 3 : Cohérence des RDS intermédiaires (si déjà produits)
# ==============================================================================
.titre("TEST 3 — RDS intermédiaires existants")

etapes <- c("01","02","03","04")
types_etape <- list(
  "01" = c("caracteristiques","lieux","usagers","vehicules"),
  "02" = c("caracteristiques","lieux","usagers","vehicules"),
  "03" = c("accidents"),
  "04" = c("accidents")
)

for (etape in etapes) {
  for (type in types_etape[[etape]]) {
    chemin <- file.path(CHEMIN_OUTPUT,
                        paste0(etape, "_", type, "_", ANNEE_CIBLE, ".rds"))
    if (file.exists(chemin)) {
      taille <- round(file.size(chemin) / 1024^2, 1)
      d <- readRDS(chemin)
      .ok(sprintf("%-35s : %d lignes × %d colonnes (%.1f MB disque)",
                  basename(chemin), nrow(d), ncol(d), taille))
      rm(d)
    } else {
      cat(sprintf("  \u23f3 %-35s : pas encore produit\n", basename(chemin)))
    }
  }
}


# ==============================================================================
# TEST 4 : Fichiers Shiny finaux (si déjà produits)
# ==============================================================================
.titre("TEST 4 — Fichiers Shiny finaux")

.FICHIERS_SHINY <- c(
  "accidents_dashboard.rds",
  "accidents_light.rds",
  "filters.rds"
)

for (f in .FICHIERS_SHINY) {
  chemin <- file.path(CHEMIN_SHINY, f)
  if (file.exists(chemin)) {
    d <- readRDS(chemin)
    taille_ram <- round(object.size(d) / 1024^2, 1)
    taille_disque <- round(file.size(chemin) / 1024^2, 1)
    if (is.data.frame(d)) {
      .ok(sprintf("%-35s : %d lignes × %d colonnes | RAM %.1f MB | disque %.1f MB",
                  f, nrow(d), ncol(d), taille_ram, taille_disque))

      # Vérifier les colonnes attendues
      if (f == "accidents_dashboard.rds") {
        manquantes <- setdiff(COLONNES_DASHBOARD, names(d))
        if (length(manquantes) > 0) {
          .warn(sprintf("  Colonnes dashboard manquantes : %s",
                        paste(manquantes, collapse = ", ")))
        }
      }
      if (f == "accidents_light.rds") {
        manquantes <- setdiff(COLONNES_LIGHT_EXTRA, names(d))
        if (length(manquantes) > 0) {
          .warn(sprintf("  Colonnes light manquantes : %s",
                        paste(manquantes, collapse = ", ")))
        }
      }
    } else {
      .ok(sprintf("%-35s : liste de %d éléments", f, length(d)))
    }
    rm(d)
  } else {
    cat(sprintf("  \u23f3 %-35s : pas encore produit\n", f))
  }
}


# ==============================================================================
# TEST 5 : Estimation mémoire pour shinyapps.io
# ==============================================================================
.titre("TEST 5 — Estimation mémoire shinyapps.io")

f_db    <- file.path(CHEMIN_SHINY, "accidents_dashboard.rds")
f_light <- file.path(CHEMIN_SHINY, "accidents_light.rds")

if (file.exists(f_db) && file.exists(f_light)) {
  db    <- readRDS(f_db)
  light <- readRDS(f_light)

  ram_db    <- round(object.size(db)    / 1024^2, 1)
  ram_light <- round(object.size(light) / 1024^2, 1)
  ram_total <- ram_db + ram_light

  cat(sprintf("  accidents_dashboard : %.1f MB\n", ram_db))
  cat(sprintf("  accidents_light     : %.1f MB\n", ram_light))
  cat(sprintf("  Total estimé        : %.1f MB\n", ram_total))
  cat(sprintf("  Limite shinyapps.io : 1024 MB\n"))

  if (ram_total < 700) {
    .ok(sprintf("Mémoire OK (%.0f MB < 700 MB)", ram_total))
  } else if (ram_total < 900) {
    .warn(sprintf("Mémoire limite (%.0f MB) — surveiller lors du déploiement", ram_total))
  } else {
    .err(sprintf("Risque OOM (%.0f MB > 900 MB) — réduire les colonnes dans 00_config.R", ram_total))
  }

  rm(db, light)
} else {
  cat("  ⏳ Fichiers finaux pas encore produits — relancer après le pipeline.\n")
}


# ==============================================================================
# RÉSUMÉ
# ==============================================================================
cat(sprintf("\n%s\n", strrep("=", 70)))
cat("  RÉSUMÉ DU TEST\n")
cat(sprintf("%s\n", strrep("=", 70)))
cat(sprintf("  Année cible    : %d\n", ANNEE_CIBLE))
cat(sprintf("  CSV trouvés    : %d/4\n", n_csv_ok))
cat(sprintf("  Log            : %s\n", LOG_FILE))
cat(sprintf("%s\n\n", strrep("=", 70)))
