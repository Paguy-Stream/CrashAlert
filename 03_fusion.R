# ==============================================================================
# 03_fusion.R — Fusion des 4 tables BAAC + calcul de la gravité
# ==============================================================================
# Dépendances : 00_config.R + 01_import.R + 02_nettoyage.R sourcés avant.
# Entrées     : caracteristiques, lieux, usagers, vehicules nettoyés
# Produit     : accidents (data frame au niveau accident, une ligne par accident)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

log_msg("=== 03_fusion.R — début ===")


# ------------------------------------------------------------------------------
# 1. AGRÉGATION DES USAGERS AU NIVEAU ACCIDENT
# ------------------------------------------------------------------------------
# La table usagers est au niveau usager (plusieurs lignes par accident).
# On calcule ici les indicateurs de gravité agrégés par accident.
# Référentiel BAAC : grav 1=Indemne, 2=Tué, 3=Blessé hospitalisé, 4=Blessé léger

log_msg("Agrégation des usagers par accident...")

gravite_par_accident <- usagers |>
  mutate(grav = as.numeric(grav)) |>
  group_by(Num_Acc) |>
  summarise(
    nb_usagers              = n(),
    nb_tues                 = sum(grav == 2, na.rm = TRUE),
    nb_blesses_hospitalises = sum(grav == 3, na.rm = TRUE),
    nb_blesses_legers       = sum(grav == 4, na.rm = TRUE),
    nb_indemnes             = sum(grav == 1, na.rm = TRUE),
    gravite_max             = suppressWarnings(max(grav, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    gravite_max   = ifelse(is.infinite(gravite_max), NA_real_, gravite_max),
    nb_victimes   = nb_tues + nb_blesses_hospitalises + nb_blesses_legers,
    # Gravité synthétique (label)
    gravite_accident = case_when(
      nb_tues > 0                 ~ "Mortel",
      nb_blesses_hospitalises > 0 ~ "Grave",
      nb_blesses_legers > 0      ~ "Léger",
      nb_indemnes > 0             ~ "Indemne",
      TRUE                        ~ "Inconnu"
    ),
    # Score de sévérité 1-4 (utile pour tri et modélisation)
    severite_score = case_when(
      nb_tues > 0                 ~ 4,
      nb_blesses_hospitalises > 0 ~ 3,
      nb_blesses_legers > 0      ~ 2,
      nb_indemnes > 0             ~ 1,
      TRUE                        ~ NA_real_
    )
  )

log_msg(sprintf("gravite_par_accident : %d accidents agrégés", nrow(gravite_par_accident)))
log_msg(sprintf("Distribution gravité : %s",
  paste(names(table(gravite_par_accident$gravite_accident)),
        table(gravite_par_accident$gravite_accident), sep = "=", collapse = " | ")))


# ------------------------------------------------------------------------------
# 2. AGRÉGATION DES USAGERS — PROFIL DOMINANT PAR ACCIDENT
# ------------------------------------------------------------------------------
# Pour accidents_light : sexe dominant, tranche d'âge dominante,
# catégorie d'usager principal (conducteur > passager > piéton)

log_msg("Calcul du profil usager dominant par accident...")

.mode_val <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

profil_usager <- usagers |>
  left_join(
    caracteristiques |> select(Num_Acc, an),
    by = "Num_Acc"
  ) |>
  mutate(
    age = suppressWarnings(as.numeric(an) - as.numeric(an_nais)),
    age = ifelse(is.na(age) | age < AGE_MIN | age > AGE_MAX, NA_real_, age),
    tranche_age = case_when(
      age < 18              ~ "Moins de 18 ans",
      age >= 18 & age <= 24 ~ "18-24 ans",
      age >= 25 & age <= 34 ~ "25-34 ans",
      age >= 35 & age <= 49 ~ "35-49 ans",
      age >= 50 & age <= 64 ~ "50-64 ans",
      age >= 65             ~ "65 ans et plus",
      TRUE                  ~ "Non renseigné"
    ),
    sexe_label = case_when(
      as.numeric(sexe) == 1 ~ "Masculin",
      as.numeric(sexe) == 2 ~ "Féminin",
      TRUE                  ~ NA_character_
    ),
    catu_num = as.numeric(catu)
  ) |>
  group_by(Num_Acc) |>
  summarise(
    age_moyen_reel  = round(mean(age, na.rm = TRUE), 1),
    sexe_dominant   = .mode_val(sexe_label),
    tranche_age     = .mode_val(tranche_age),
    # Usager principal : conducteur (1) > passager (2) > piéton (3)
    catu_principal  = case_when(
      any(catu_num == 1, na.rm = TRUE) ~ "Conducteur",
      any(catu_num == 2, na.rm = TRUE) ~ "Passager",
      any(catu_num == 3, na.rm = TRUE) ~ "Piéton",
      TRUE                             ~ "Autre"
    ),
    # Équipement de sécurité principal
    # 2015-2018 : colonne `secu` unique | 2019+ : colonnes `secu1`, `secu2`, `secu3`
    secu_principal = .mode_val(as.character(
      if ("secu1" %in% names(usagers)) as.numeric(secu1)
      else if ("secu" %in% names(usagers)) as.numeric(secu)
      else NA_real_
    )),
    .groups = "drop"
  )

log_msg(sprintf("profil_usager : %d accidents avec profil calculé", nrow(profil_usager)))


# ------------------------------------------------------------------------------
# 3. AGRÉGATION DES VÉHICULES — CATÉGORIE DOMINANTE PAR ACCIDENT
# ------------------------------------------------------------------------------

log_msg("Calcul de la catégorie véhicule dominante par accident...")

.VEHICULE_CATEGORIES <- c(
  "01" = "Vélo",
  "02" = "Cyclomoteur", "30" = "Cyclomoteur",
  "31" = "Deux-roues motorisé", "32" = "Deux-roues motorisé",
  "33" = "Deux-roues motorisé", "34" = "Deux-roues motorisé",
  "41" = "Deux-roues motorisé", "42" = "Deux-roues motorisé", "43" = "Deux-roues motorisé",
  "07" = "Véhicule léger",
  "10" = "Véhicule utilitaire",
  "13" = "Poids lourd", "14" = "Poids lourd", "15" = "Poids lourd",
  "16" = "Poids lourd", "17" = "Poids lourd",
  "37" = "Transport en commun", "38" = "Transport en commun",
  "21" = "Tracteur agricole",
  "50" = "EDP motorisé", "60" = "EDP non motorisé", "80" = "VAE"
)

profil_vehicule <- vehicules |>
  mutate(
    catv_pad = stringr::str_pad(as.character(catv), 2, pad = "0"),
    categorie_vehicule = ifelse(
      catv_pad %in% names(.VEHICULE_CATEGORIES),
      .VEHICULE_CATEGORIES[catv_pad],
      "Autre"
    )
  ) |>
  group_by(Num_Acc) |>
  summarise(
    categorie_vehicule = .mode_val(categorie_vehicule),
    .groups = "drop"
  )

log_msg(sprintf("profil_vehicule : %d accidents avec catégorie calculée", nrow(profil_vehicule)))


# ------------------------------------------------------------------------------
# 4. FUSION PRINCIPALE — NIVEAU ACCIDENT
# ------------------------------------------------------------------------------
# Clé de jointure : Num_Acc (character, harmonisé en 02_nettoyage.R)
# Stratégie : caracteristiques comme base (1 ligne = 1 accident)
#             puis left_join successifs

log_msg("Fusion des 4 tables au niveau accident...")

accidents <- caracteristiques |>
  left_join(lieux,                by = "Num_Acc") |>
  left_join(gravite_par_accident, by = "Num_Acc") |>
  left_join(profil_usager,        by = "Num_Acc") |>
  left_join(profil_vehicule,      by = "Num_Acc")

log_msg(sprintf("Base fusionnée : %d accidents, %d colonnes",
                nrow(accidents), ncol(accidents)))


# ------------------------------------------------------------------------------
# 5. VÉRIFICATIONS POST-FUSION
# ------------------------------------------------------------------------------

n_sans_lieux    <- sum(is.na(accidents$catr))
n_sans_gravite  <- sum(is.na(accidents$gravite_accident))
n_sans_usager   <- sum(is.na(accidents$nb_usagers))
n_sans_vehicule <- sum(is.na(accidents$categorie_vehicule))

log_msg(sprintf("Accidents sans données lieux    : %d (%.1f%%)",
                n_sans_lieux, n_sans_lieux / nrow(accidents) * 100))
log_msg(sprintf("Accidents sans gravité          : %d (%.1f%%)",
                n_sans_gravite, n_sans_gravite / nrow(accidents) * 100))
log_msg(sprintf("Accidents sans usagers          : %d (%.1f%%)",
                n_sans_usager, n_sans_usager / nrow(accidents) * 100))
log_msg(sprintf("Accidents sans catégorie véhic. : %d (%.1f%%)",
                n_sans_vehicule, n_sans_vehicule / nrow(accidents) * 100))

# Alerte si trop d'accidents sans gravité (jointure usagers ratée)
if (n_sans_gravite / nrow(accidents) > 0.05) {
  log_msg(sprintf("ALERTE : plus de 5%% des accidents sans gravité — vérifier la jointure usagers"),
          "WARN")
}


# ------------------------------------------------------------------------------
# 6. SAUVEGARDE INTERMÉDIAIRE
# ------------------------------------------------------------------------------

saveRDS(accidents, file.path(CHEMIN_OUTPUT, paste0("03_accidents_", ANNEE_CIBLE, ".rds")))

log_msg("=== 03_fusion.R — terminé ===")
