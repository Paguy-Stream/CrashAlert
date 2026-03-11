# tests/testthat/test_filtres.R
# ─────────────────────────────────────────────────────────────
# Tests : Filtres et logique réactive des modules Shiny
# ─────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)

# ── Données mock ──────────────────────────────────────────────
make_mock_full <- function() {
  set.seed(99)
  n <- 2000
  data.frame(
    num_acc          = paste0("ACC", seq_len(n)),
    annee            = sample(2015:2024, n, replace = TRUE),
    mois             = sample(1:12,   n, replace = TRUE),
    departement      = sample(c("Indre-et-Loire","Loiret","Loir-et-Cher",
                                 "Cher","Indre","Eure-et-Loir"), n, replace = TRUE),
    region           = sample(c("Centre-Val de Loire","Bretagne","Normandie"),
                               n, replace = TRUE),
    gravite_accident = sample(c("Mortel","Grave","Léger"),
                               n, replace = TRUE, prob = c(0.06, 0.35, 0.59)),
    categorie_vehicule = sample(c("Voiture","Moto","Vélo","Piéton","Camion"),
                                  n, replace = TRUE),
    atm_label        = sample(c("Normale","Pluie légère","Brouillard","Neige – grêle"),
                               n, replace = TRUE),
    catr_label       = sample(c("Route nationale","Route départementale","Voie communale"),
                               n, replace = TRUE),
    sexe_dominant    = sample(c("Homme","Femme"), n, replace = TRUE),
    tranche_age      = sample(c("18-24 ans","25-34 ans","35-44 ans",
                                 "45-54 ans","55-64 ans","Non renseigné"),
                               n, replace = TRUE),
    heure            = sample(c(paste0("0", 0:9), as.character(10:23)),
                               n, replace = TRUE),
    jour_semaine     = sample(c("Lundi","Mardi","Mercredi","Jeudi",
                                 "Vendredi","Samedi","Dimanche"), n, replace = TRUE),
    saison           = sample(c("Printemps","Été","Automne","Hiver"), n, replace = TRUE),
    accident_mortel  = FALSE,
    stringsAsFactors = FALSE
  ) |>
    mutate(accident_mortel = gravite_accident == "Mortel")
}

mock <- make_mock_full()

# ── Fonctions de filtre (reproduisant les modules) ────────────
filtre_gravite <- function(df, gravites) {
  if (is.null(gravites) || length(gravites) == 0) return(df)
  df |> filter(as.character(gravite_accident) %in% gravites)
}

filtre_annees <- function(df, annee_min, annee_max) {
  df |> filter(annee >= annee_min, annee <= annee_max)
}

filtre_region <- function(df, region) {
  if (is.null(region) || region == "Toutes") return(df)
  df |> filter(as.character(region) == !!region)
}

filtre_vehicule <- function(df, vehicule) {
  if (is.null(vehicule) || vehicule == "Tous") return(df)
  df |> filter(as.character(categorie_vehicule) == vehicule)
}

filtre_non_renseigne_age <- function(df) {
  df |> filter(!as.character(tranche_age) %in%
                 c("Non renseigné", "Non renseigne", "Inconnu", ""))
}

# ── Tests filtre gravité ──────────────────────────────────────
test_that("filtre_gravite sur 'Mortel' retourne uniquement des mortels", {
  res <- filtre_gravite(mock, "Mortel")
  expect_true(all(res$gravite_accident == "Mortel"))
})

test_that("filtre_gravite sur NULL retourne toutes les lignes", {
  res <- filtre_gravite(mock, NULL)
  expect_equal(nrow(res), nrow(mock))
})

test_that("filtre_gravite sur vecteur vide retourne toutes les lignes", {
  res <- filtre_gravite(mock, character(0))
  expect_equal(nrow(res), nrow(mock))
})

test_that("filtre_gravite multi-valeurs fonctionne", {
  res <- filtre_gravite(mock, c("Mortel", "Grave"))
  expect_true(all(res$gravite_accident %in% c("Mortel", "Grave")))
  expect_equal(0, sum(res$gravite_accident == "Léger"))
})

# ── Tests filtre années ───────────────────────────────────────
test_that("filtre_annees respecte les bornes min et max", {
  res <- filtre_annees(mock, 2018, 2021)
  expect_gte(min(res$annee), 2018)
  expect_lte(max(res$annee), 2021)
})

test_that("filtre_annees sur une seule année ne retourne que cette année", {
  res <- filtre_annees(mock, 2020, 2020)
  expect_true(all(res$annee == 2020))
})

test_that("filtre_annees 2015-2024 retourne toutes les lignes", {
  res <- filtre_annees(mock, 2015, 2024)
  expect_equal(nrow(res), nrow(mock))
})

# ── Tests filtre région ───────────────────────────────────────
test_that("filtre_region filtre correctement par région", {
  res <- filtre_region(mock, "Bretagne")
  expect_true(all(res$region == "Bretagne"))
})

test_that("filtre_region sur 'Toutes' retourne tout", {
  res <- filtre_region(mock, "Toutes")
  expect_equal(nrow(res), nrow(mock))
})

test_that("filtre_region sur NULL retourne tout", {
  res <- filtre_region(mock, NULL)
  expect_equal(nrow(res), nrow(mock))
})

# ── Tests filtre véhicule ─────────────────────────────────────
test_that("filtre_vehicule filtre correctement par type", {
  res <- filtre_vehicule(mock, "Moto")
  expect_true(all(res$categorie_vehicule == "Moto"))
})

test_that("filtre_vehicule sur 'Tous' retourne tout", {
  res <- filtre_vehicule(mock, "Tous")
  expect_equal(nrow(res), nrow(mock))
})

# ── Tests filtre Non renseigné âge ────────────────────────────
test_that("filtre_non_renseigne_age supprime les NA et libellés vides", {
  res <- filtre_non_renseigne_age(mock)
  valeurs_restantes <- unique(as.character(res$tranche_age))
  expect_false("Non renseigné" %in% valeurs_restantes)
  expect_false("Non renseigne" %in% valeurs_restantes)
  expect_false("" %in% valeurs_restantes)
})

test_that("filtre_non_renseigne_age conserve les tranches valides", {
  res <- filtre_non_renseigne_age(mock)
  tranches_valides <- c("18-24 ans","25-34 ans","35-44 ans","45-54 ans","55-64 ans")
  expect_true(all(as.character(res$tranche_age) %in% tranches_valides))
})

# ── Tests combinaisons de filtres ─────────────────────────────
test_that("filtres combinés ne retournent pas plus de lignes que l'original", {
  res <- mock |>
    filtre_annees(2019, 2022) |>
    filtre_gravite("Mortel") |>
    filtre_region("Bretagne")
  expect_lte(nrow(res), nrow(mock))
})

test_that("filtres sur données inexistantes retournent 0 lignes sans erreur", {
  res <- filtre_region(mock, "Corse-du-Sud-Inexistante")
  expect_equal(nrow(res), 0)
  expect_s3_class(res, "data.frame")
})
