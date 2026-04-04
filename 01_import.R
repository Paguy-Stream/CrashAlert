# ==============================================================================
# 01_import.R — Importation des fichiers CSV BAAC
# ==============================================================================
# Dépendances : 00_config.R doit être sourcé avant ce fichier.
# Produit      : 4 data frames en mémoire :
#                caracteristiques, lieux, usagers, vehicules
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
})

log_msg("=== 01_import.R — début ===")


# ------------------------------------------------------------------------------
# 1. DÉTECTION DES FICHIERS CSV POUR L'ANNÉE CIBLE
# ------------------------------------------------------------------------------

.detecter_fichier <- function(type) {
  patterns <- PATTERNS_CSV[[type]]
  candidats <- list.files(
    CHEMIN_DONNEES,
    pattern  = paste0("(", paste(patterns, collapse = "|"), ").*", ANNEE_CIBLE, "\\.csv$"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(candidats) == 0) {
    stop(sprintf("Aucun fichier '%s' trouvé pour l'année %d dans %s",
                 type, ANNEE_CIBLE, CHEMIN_DONNEES))
  }
  if (length(candidats) > 1) {
    log_msg(sprintf("Plusieurs fichiers '%s' détectés, premier retenu : %s",
                    type, basename(candidats[1])), "WARN")
  }
  log_msg(sprintf("Fichier '%s' détecté : %s", type, basename(candidats[1])))
  candidats[1]
}

fichiers <- list(
  caracteristiques = .detecter_fichier("caracteristiques"),
  lieux            = .detecter_fichier("lieux"),
  usagers          = .detecter_fichier("usagers"),
  vehicules        = .detecter_fichier("vehicules")
)


# ------------------------------------------------------------------------------
# 2. FONCTION DE LECTURE ADAPTATIVE
# ------------------------------------------------------------------------------
# Gère automatiquement :
#   - Séparateur ; (2019-2024) ou , (2015-2018)
#   - Encodage ISO-8859-1 ou UTF-8
#   - Lecture initiale tout en texte pour éviter les erreurs de type
#   - Standardisation du nom Num_Acc (parfois Accident_Id)
# ------------------------------------------------------------------------------

.lire_csv_baac <- function(fichier) {

  # Détection du séparateur sur la première ligne
  premiere_ligne <- read_lines(fichier, n_max = 1, locale = locale(encoding = "UTF-8"))
  separateur <- if (grepl(";", premiere_ligne)) ";" else ","

  log_msg(sprintf("  Lecture %s (séparateur '%s')", basename(fichier), separateur))

  # Tentative UTF-8
  data <- tryCatch({
    read_delim(
      fichier,
      delim      = separateur,
      col_types  = cols(.default = col_character()),
      locale     = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = ""),
      show_col_types = FALSE
    )
  }, error = function(e) NULL)

  # Fallback ISO-8859-1 si UTF-8 échoue
  if (is.null(data)) {
    log_msg(sprintf("  UTF-8 échoué, fallback ISO-8859-1 : %s", basename(fichier)), "WARN")
    data <- read_delim(
      fichier,
      delim      = separateur,
      col_types  = cols(.default = col_character()),
      locale     = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = ""),
      show_col_types = FALSE
    )
    # Reconvertir en UTF-8
    data <- data |>
      mutate(across(where(is.character), ~ iconv(., from = "ISO-8859-1", to = "UTF-8")))
  }

  # Standardisation du nom de la clé principale
  correspondances <- c("Accident_Id" = "Num_Acc", "Num_Accident" = "Num_Acc")
  for (ancien in names(correspondances)) {
    if (ancien %in% names(data)) {
      data <- rename(data, !!correspondances[[ancien]] := !!ancien)
      log_msg(sprintf("  Colonne '%s' renommée en 'Num_Acc'", ancien))
    }
  }

  # Nettoyage des noms de colonnes (espaces, caractères parasites)
  names(data) <- trimws(names(data))

  # Ajout de l'année source comme vérification
  data <- mutate(data, annee_source = ANNEE_CIBLE)

  log_msg(sprintf("  %s : %d lignes, %d colonnes", basename(fichier), nrow(data), ncol(data)))
  data
}


# ------------------------------------------------------------------------------
# 3. IMPORTATION DES 4 TABLES
# ------------------------------------------------------------------------------

caracteristiques <- .lire_csv_baac(fichiers$caracteristiques)
lieux            <- .lire_csv_baac(fichiers$lieux)
usagers          <- .lire_csv_baac(fichiers$usagers)
vehicules        <- .lire_csv_baac(fichiers$vehicules)


# ------------------------------------------------------------------------------
# 4. VÉRIFICATIONS MINIMALES
# ------------------------------------------------------------------------------

.verifier_table <- function(df, nom) {
  if (!"Num_Acc" %in% names(df)) {
    stop(sprintf("Colonne 'Num_Acc' absente de la table '%s' — vérifier le fichier source.", nom))
  }
  n_acc <- n_distinct(df$Num_Acc, na.rm = TRUE)
  n_na  <- sum(is.na(df$Num_Acc))
  log_msg(sprintf("  %-20s : %d lignes | %d Num_Acc uniques | %d NA sur clé",
                  nom, nrow(df), n_acc, n_na))
  if (n_na > 0) {
    log_msg(sprintf("  %d lignes sans Num_Acc dans '%s' — elles seront perdues à la jointure.",
                    n_na, nom), "WARN")
  }
}

log_msg("--- Vérification des tables importées ---")
.verifier_table(caracteristiques, "caracteristiques")
.verifier_table(lieux,            "lieux")
.verifier_table(usagers,          "usagers")
.verifier_table(vehicules,        "vehicules")


# ------------------------------------------------------------------------------
# 5. SAUVEGARDE INTERMÉDIAIRE (optionnelle, pour reprise sans réimporter)
# ------------------------------------------------------------------------------

saveRDS(caracteristiques, file.path(CHEMIN_OUTPUT, paste0("01_caracteristiques_", ANNEE_CIBLE, ".rds")))
saveRDS(lieux,            file.path(CHEMIN_OUTPUT, paste0("01_lieux_",            ANNEE_CIBLE, ".rds")))
saveRDS(usagers,          file.path(CHEMIN_OUTPUT, paste0("01_usagers_",          ANNEE_CIBLE, ".rds")))
saveRDS(vehicules,        file.path(CHEMIN_OUTPUT, paste0("01_vehicules_",        ANNEE_CIBLE, ".rds")))

log_msg("=== 01_import.R — terminé ===")
