# tests/testthat/test_data.R
# ─────────────────────────────────────────────────────────────
# Tests : Chargement et intégrité des données BAAC
# ─────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)

# Chemin relatif depuis la racine du projet
DATA_PATH <- "data_propre/accidents_final_light.rds"

# ── Fixture partagée ──────────────────────────────────────────
acc <- local({
  skip_if_not(file.exists(DATA_PATH), "Fichier RDS absent — tests data ignorés")
  readRDS(DATA_PATH)
})

# ── 1. Fichier lisible ────────────────────────────────────────
test_that("le fichier RDS se charge sans erreur", {
  skip_if_not(file.exists(DATA_PATH))
  expect_no_error(readRDS(DATA_PATH))
})

# ── 2. Structure minimale ─────────────────────────────────────
test_that("le dataframe contient au moins 500 000 lignes", {
  expect_gte(nrow(acc), 500000)
})

test_that("le dataframe contient les colonnes obligatoires", {
  cols_required <- c(
    "num_acc", "annee", "mois", "jour", "heure",
    "dep", "departement", "region",
    "lat", "long",
    "gravite_accident", "grav_label",
    "nb_tues", "nb_victimes",
    "tranche_age", "age_moyen_reel", "sexe_dominant",
    "categorie_vehicule", "lum_label", "atm_label",
    "catr_label", "surf_label", "type_route",
    "accident_mortel", "accident_grave",
    "moment_journee", "saison", "jour_semaine"
  )
  missing <- setdiff(cols_required, names(acc))
  expect_equal(
    length(missing), 0,
    label = paste("Colonnes manquantes :", paste(missing, collapse = ", "))
  )
})

# ── 3. Valeurs canoniques ─────────────────────────────────────
test_that("gravite_accident contient uniquement les valeurs canoniques", {
  valeurs_attendues <- c("Mortel", "Grave", "Léger")
  valeurs_obs <- unique(as.character(acc$gravite_accident))
  inattendues  <- setdiff(valeurs_obs, valeurs_attendues)
  expect_equal(
    length(inattendues), 0,
    label = paste("Valeurs inattendues dans gravite_accident :",
                  paste(inattendues, collapse = ", "))
  )
})

test_that("la colonne annee couvre bien 2015-2024", {
  annees <- sort(unique(acc$annee))
  expect_true(2015 %in% annees)
  expect_true(2024 %in% annees)
  expect_true(all(annees >= 2015 & annees <= 2024))
})

# ── 4. Pas de doublons sur num_acc ────────────────────────────
test_that("num_acc est unique par ligne", {
  n_dup <- acc |> count(num_acc) |> filter(n > 1) |> nrow()
  expect_equal(n_dup, 0, label = paste(n_dup, "num_acc en doublon"))
})

# ── 5. Coordonnées GPS ────────────────────────────────────────
test_that("les latitudes sont dans la plage France métropolitaine", {
  lats_valides <- acc |>
    filter(!is.na(lat), lat != 0) |>
    pull(lat)
  expect_true(all(lats_valides >= 41 & lats_valides <= 52),
              label = "Certaines latitudes hors France métropole")
})

test_that("les longitudes sont dans la plage France métropolitaine", {
  lons_valides <- acc |>
    filter(!is.na(long), long != 0) |>
    pull(long)
  expect_true(all(lons_valides >= -5.5 & lons_valides <= 10),
              label = "Certaines longitudes hors France métropole")
})

# ── 6. Cohérence logique ──────────────────────────────────────
test_that("nb_tues ne dépasse pas nb_victimes", {
  incoherents <- acc |>
    filter(!is.na(nb_tues), !is.na(nb_victimes)) |>
    filter(nb_tues > nb_victimes) |>
    nrow()
  expect_equal(incoherents, 0,
               label = paste(incoherents, "lignes avec nb_tues > nb_victimes"))
})

test_that("accident_mortel est cohérent avec gravite_accident", {
  incoherents <- acc |>
    filter(gravite_accident == "Mortel", accident_mortel == FALSE) |>
    nrow()
  expect_equal(incoherents, 0,
               label = "Accidents 'Mortel' avec accident_mortel == FALSE")
})

# ── 7. Régions et départements ────────────────────────────────
test_that("le nombre de régions est cohérent (13 régions métropole)", {
  n_regions <- n_distinct(acc$region)
  expect_gte(n_regions, 13)
  expect_lte(n_regions, 18) # avec DOM-TOM éventuels
})

test_that("le nombre de départements est cohérent (>= 90)", {
  n_depts <- n_distinct(acc$departement)
  expect_gte(n_depts, 90)
})
