# ==============================================================================
# 02_nettoyage.R — Nettoyage des 4 tables BAAC
# ==============================================================================
# Dépendances : 00_config.R + 01_import.R sourcés avant ce fichier.
# Entrées     : caracteristiques, lieux, usagers, vehicules (en mémoire)
# Produit     : caracteristiques, lieux, usagers, vehicules nettoyés
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

log_msg("=== 02_nettoyage.R — début ===")


# ------------------------------------------------------------------------------
# 1. NETTOYAGE DES CHAÎNES VIDES → NA
# ------------------------------------------------------------------------------

.vider_chaines <- function(df) {
  df |> mutate(across(where(is.character), ~ {
    x <- iconv(., from = "", to = "UTF-8", sub = "")
    na_if(trimws(x), "")
  }))
}

caracteristiques <- .vider_chaines(caracteristiques)
lieux            <- .vider_chaines(lieux)
usagers          <- .vider_chaines(usagers)
vehicules        <- .vider_chaines(vehicules)

log_msg("Chaînes vides converties en NA sur les 4 tables")


# ------------------------------------------------------------------------------
# 2. CONVERSION DES TYPES — CARACTÉRISTIQUES
# ------------------------------------------------------------------------------

# Colonnes numériques attendues dans caracteristiques
.NUM_CAR <- c("Num_Acc", "an", "mois", "jour", "lum", "agg", "int", "atm", "col")
# Colonnes à garder en texte
.TXT_CAR <- c("adr", "gps", "com", "dep", "hrmn")

caracteristiques <- caracteristiques |>
  mutate(across(all_of(intersect(.NUM_CAR, names(caracteristiques))), as.numeric)) |>
  # Correction critique : années sur 2 chiffres (15 → 2015)
  mutate(an = case_when(
    nchar(as.character(an)) == 2 ~ as.numeric(an) + 2000,
    nchar(as.character(an)) == 4 ~ as.numeric(an),
    TRUE ~ NA_real_
  ))

log_msg(sprintf("caracteristiques — années présentes : %s",
                paste(sort(unique(na.omit(caracteristiques$an))), collapse = ", ")))


# ------------------------------------------------------------------------------
# 3. CONVERSION DES TYPES — LIEUX
# ------------------------------------------------------------------------------

.NUM_LIEUX <- c("Num_Acc", "catr", "circ", "vosp", "prof", "plan", "surf", "infra", "situ")

lieux <- lieux |>
  mutate(across(all_of(intersect(.NUM_LIEUX, names(lieux))), as.numeric))

log_msg("Types convertis pour lieux")


# ------------------------------------------------------------------------------
# 4. CONVERSION DES TYPES — USAGERS
# ------------------------------------------------------------------------------

.NUM_USAGERS <- c("Num_Acc", "catu", "grav", "sexe", "an_nais", "trajet", "secu1", "secu2", "secu3", "locp", "actp", "etatp")

usagers <- usagers |>
  mutate(across(all_of(intersect(.NUM_USAGERS, names(usagers))), as.numeric))

log_msg("Types convertis pour usagers")


# ------------------------------------------------------------------------------
# 5. CONVERSION DES TYPES — VÉHICULES
# ------------------------------------------------------------------------------

.NUM_VEHICULES <- c("Num_Acc", "catv", "obs", "obsm", "choc", "manv", "motor", "occutc")

vehicules <- vehicules |>
  mutate(across(all_of(intersect(.NUM_VEHICULES, names(vehicules))), as.numeric))

log_msg("Types convertis pour vehicules")


# ------------------------------------------------------------------------------
# 6. NETTOYAGE DE LA COLONNE HRMN (heure-minute)
# ------------------------------------------------------------------------------
# Format attendu : 4 chiffres HHMM (ex: 0830 = 08h30)
# Certaines années fournissent 1 à 3 chiffres — on complète à gauche

if ("hrmn" %in% names(caracteristiques)) {
  caracteristiques <- caracteristiques |>
    mutate(
      hrmn_char  = gsub("[^0-9]", "", as.character(hrmn)),
      hrmn_clean = case_when(
        nchar(hrmn_char) == 1 ~ paste0("000", hrmn_char),
        nchar(hrmn_char) == 2 ~ paste0("00",  hrmn_char),
        nchar(hrmn_char) == 3 ~ paste0("0",   hrmn_char),
        nchar(hrmn_char) == 4 ~ hrmn_char,
        TRUE ~ NA_character_
      ),
      heure        = as.numeric(substr(hrmn_clean, 1, 2)),
      minute       = as.numeric(substr(hrmn_clean, 3, 4)),
      hrmn_valide  = !is.na(heure) & heure >= 0 & heure <= 23 &
                     !is.na(minute) & minute >= 0 & minute <= 59
    )
  pct_valide <- round(mean(caracteristiques$hrmn_valide, na.rm = TRUE) * 100, 1)
  log_msg(sprintf("hrmn : %s%% de valeurs valides", pct_valide))
} else {
  log_msg("Colonne hrmn absente de caracteristiques", "WARN")
  caracteristiques <- caracteristiques |>
    mutate(heure = NA_real_, minute = NA_real_, hrmn_valide = FALSE)
}


# ------------------------------------------------------------------------------
# 7. NETTOYAGE DES COORDONNÉES GÉOGRAPHIQUES
# ------------------------------------------------------------------------------
# La virgule est utilisée comme séparateur décimal dans certaines années

if (all(c("lat", "long") %in% names(caracteristiques))) {
  caracteristiques <- caracteristiques |>
    mutate(
      lat  = as.numeric(gsub(",", ".", as.character(lat))),
      long = as.numeric(gsub(",", ".", as.character(long))),
      # Validation plage France métropolitaine (définie dans 00_config.R)
      coords_valides = !is.na(lat) & !is.na(long) &
                       lat  >= LAT_MIN & lat  <= LAT_MAX &
                       long >= LON_MIN & long <= LON_MAX
    )
  pct_coords <- round(mean(caracteristiques$coords_valides, na.rm = TRUE) * 100, 1)
  log_msg(sprintf("Coordonnées : %s%% valides (plage France métropolitaine)", pct_coords))
} else {
  log_msg("Colonnes lat/long absentes de caracteristiques", "WARN")
  caracteristiques <- mutate(caracteristiques, coords_valides = FALSE)
}


# ------------------------------------------------------------------------------
# 8. NETTOYAGE DU DÉPARTEMENT
# ------------------------------------------------------------------------------
# Format cible : 2 caractères ("01"…"95", "2A", "2B")

caracteristiques <- caracteristiques |>
  mutate(
    dep = as.character(dep),
    dep = str_sub(dep, 1, 2),
    dep = case_when(
      nchar(dep) == 1              ~ paste0("0", dep),
      dep %in% c("NA", "")        ~ NA_character_,
      TRUE                         ~ dep
    )
  )

log_msg("Département normalisé sur 2 caractères")


# ------------------------------------------------------------------------------
# 9. NETTOYAGE DES DOUBLONS DANS LIEUX
# ------------------------------------------------------------------------------
# La table lieux peut contenir plusieurs lignes par Num_Acc.
# Stratégie : conserver la ligne avec le plus d'informations non nulles.

n_avant <- nrow(lieux)
n_doublons <- lieux |> count(Num_Acc) |> filter(n > 1) |> nrow()

if (n_doublons > 0) {
  log_msg(sprintf("Doublons lieux : %d accidents avec plusieurs lignes", n_doublons))

  lieux <- lieux |>
    group_by(Num_Acc) |>
    mutate(
      score_qualite = case_when(
        !is.na(voie) & !voie %in% c("0", "N/A") & nchar(voie) > 1 ~ 3,
        !is.na(catr) & !is.na(circ)                                ~ 2,
        TRUE                                                        ~ 1
      )
    ) |>
    arrange(Num_Acc, desc(score_qualite)) |>
    slice(1) |>
    select(-score_qualite) |>
    ungroup()

  log_msg(sprintf("Doublons résolus : %d → %d lignes (-%d)",
                  n_avant, nrow(lieux), n_avant - nrow(lieux)))
} else {
  log_msg("Aucun doublon détecté dans lieux")
}


# ------------------------------------------------------------------------------
# 10. NETTOYAGE DES CLÉS USAGERS ET VÉHICULES
# ------------------------------------------------------------------------------

usagers <- usagers |>
  mutate(
    Num_Acc = as.character(Num_Acc),
    num_veh = as.character(num_veh),
    # Les piétons (catu == 3) n'ont pas de véhicule associé
    num_veh = ifelse(!is.na(catu) & catu == 3, NA_character_, num_veh),
    num_veh = na_if(num_veh, ""),
    num_veh = na_if(num_veh, " "),
    num_veh = na_if(num_veh, "NA")
  ) |>
  distinct()

vehicules <- vehicules |>
  mutate(
    Num_Acc = as.character(Num_Acc),
    num_veh = as.character(num_veh)
  ) |>
  distinct(Num_Acc, num_veh, .keep_all = TRUE)

caracteristiques <- caracteristiques |>
  mutate(Num_Acc = as.character(Num_Acc))

lieux <- lieux |>
  mutate(Num_Acc = as.character(Num_Acc))

log_msg("Clés harmonisées en character sur les 4 tables")


# ------------------------------------------------------------------------------
# 11. ANALYSE DES DONNÉES MANQUANTES
# ------------------------------------------------------------------------------

.rapport_na <- function(df, nom) {
  na_pct <- df |>
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) |>
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
    filter(pct_na > SEUIL_NA) |>
    arrange(desc(pct_na))

  if (nrow(na_pct) > 0) {
    log_msg(sprintf("Variables avec > %d%% NA dans '%s' :", SEUIL_NA, nom), "WARN")
    for (i in seq_len(nrow(na_pct))) {
      log_msg(sprintf("  %-30s : %s%%", na_pct$variable[i], na_pct$pct_na[i]), "WARN")
    }
  } else {
    log_msg(sprintf("'%s' : toutes les variables sous le seuil de %d%% NA", nom, SEUIL_NA))
  }
}

.rapport_na(caracteristiques, "caracteristiques")
.rapport_na(lieux,            "lieux")
.rapport_na(usagers,          "usagers")
.rapport_na(vehicules,        "vehicules")


# ------------------------------------------------------------------------------
# 12. SAUVEGARDE INTERMÉDIAIRE
# ------------------------------------------------------------------------------

saveRDS(caracteristiques, file.path(CHEMIN_OUTPUT, paste0("02_caracteristiques_", ANNEE_CIBLE, ".rds")))
saveRDS(lieux,            file.path(CHEMIN_OUTPUT, paste0("02_lieux_",            ANNEE_CIBLE, ".rds")))
saveRDS(usagers,          file.path(CHEMIN_OUTPUT, paste0("02_usagers_",          ANNEE_CIBLE, ".rds")))
saveRDS(vehicules,        file.path(CHEMIN_OUTPUT, paste0("02_vehicules_",        ANNEE_CIBLE, ".rds")))

log_msg("=== 02_nettoyage.R — terminé ===")
