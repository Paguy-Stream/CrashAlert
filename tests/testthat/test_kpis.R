# tests/testthat/test_kpis.R
# ─────────────────────────────────────────────────────────────
# Tests : Calcul des KPIs — taux mortalité, nb accidents, scores
# ─────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)

# ── Données de test synthétiques (sans dépendance au .rds) ────
make_mock <- function() {
  set.seed(42)
  n <- 1000
  data.frame(
    num_acc          = paste0("ACC", seq_len(n)),
    annee            = sample(2015:2024, n, replace = TRUE),
    departement      = sample(c("Indre-et-Loire", "Loiret", "Loir-et-Cher"), n, replace = TRUE),
    region           = "Centre-Val de Loire",
    gravite_accident = sample(c("Mortel", "Grave", "Léger"),
                               n, replace = TRUE, prob = c(0.06, 0.35, 0.59)),
    nb_tues          = sample(0:3, n, replace = TRUE),
    nb_victimes      = sample(1:5, n, replace = TRUE),
    accident_mortel  = FALSE,
    stringsAsFactors = FALSE
  ) |>
    mutate(
      accident_mortel = gravite_accident == "Mortel",
      nb_tues = ifelse(accident_mortel, pmax(nb_tues, 1L), 0L)
    )
}

mock <- make_mock()

# ── Fonctions KPI (reproduisant la logique des modules) ───────
calc_taux_mortalite <- function(df) {
  round(mean(df$gravite_accident == "Mortel", na.rm = TRUE) * 100, 2)
}

calc_nb_accidents <- function(df) nrow(df)

calc_nb_mortels <- function(df) {
  sum(df$gravite_accident == "Mortel", na.rm = TRUE)
}

calc_kpis_region <- function(df) {
  df |> summarise(
    nb_acc    = n(),
    nb_mort   = sum(gravite_accident == "Mortel", na.rm = TRUE),
    nb_grave  = sum(gravite_accident == "Grave",  na.rm = TRUE),
    nb_leger  = sum(gravite_accident == "Léger",  na.rm = TRUE),
    taux_mort = round(mean(gravite_accident == "Mortel", na.rm = TRUE) * 100, 2)
  )
}

calc_stats_dept <- function(df) {
  df |>
    mutate(dep = as.character(departement)) |>
    group_by(dep) |>
    summarise(
      nb        = n(),
      nb_mort   = sum(gravite_accident == "Mortel", na.rm = TRUE),
      taux_mort = round(mean(gravite_accident == "Mortel", na.rm = TRUE) * 100, 2),
      .groups   = "drop"
    ) |>
    arrange(desc(taux_mort))
}

# ── Tests taux de mortalité ───────────────────────────────────
test_that("taux_mortalite est entre 0 et 100", {
  taux <- calc_taux_mortalite(mock)
  expect_gte(taux, 0)
  expect_lte(taux, 100)
})

test_that("taux_mortalite est numérique scalaire", {
  taux <- calc_taux_mortalite(mock)
  expect_type(taux, "double")
  expect_length(taux, 1)
})

test_that("taux_mortalite est 0 si aucun mortel", {
  df_zero <- mock |> mutate(gravite_accident = "Léger")
  expect_equal(calc_taux_mortalite(df_zero), 0)
})

test_that("taux_mortalite est 100 si tous mortels", {
  df_all <- mock |> mutate(gravite_accident = "Mortel")
  expect_equal(calc_taux_mortalite(df_all), 100)
})

# ── Tests nb accidents ────────────────────────────────────────
test_that("nb_accidents correspond au nrow du dataframe", {
  expect_equal(calc_nb_accidents(mock), nrow(mock))
})

test_that("nb_accidents est positif", {
  expect_gt(calc_nb_accidents(mock), 0)
})

# ── Tests nb mortels ──────────────────────────────────────────
test_that("nb_mortels est inférieur ou égal à nb_accidents", {
  expect_lte(calc_nb_mortels(mock), calc_nb_accidents(mock))
})

test_that("nb_mortels est cohérent avec le compte manuel", {
  attendu <- sum(mock$gravite_accident == "Mortel")
  expect_equal(calc_nb_mortels(mock), attendu)
})

# ── Tests KPIs région ─────────────────────────────────────────
test_that("calc_kpis_region retourne une ligne", {
  kpi <- calc_kpis_region(mock)
  expect_equal(nrow(kpi), 1)
})

test_that("nb_mort + nb_grave + nb_leger == nb_acc", {
  kpi <- calc_kpis_region(mock)
  expect_equal(kpi$nb_mort + kpi$nb_grave + kpi$nb_leger, kpi$nb_acc)
})

test_that("taux_mort dans kpis est cohérent avec nb_mort / nb_acc", {
  kpi <- calc_kpis_region(mock)
  taux_attendu <- round(kpi$nb_mort / kpi$nb_acc * 100, 2)
  expect_equal(kpi$taux_mort, taux_attendu)
})

# ── Tests stats départementales ───────────────────────────────
test_that("stats_dept a autant de lignes que de départements uniques", {
  stats <- calc_stats_dept(mock)
  expect_equal(nrow(stats), n_distinct(mock$departement))
})

test_that("stats_dept est trié par taux_mort décroissant", {
  stats <- calc_stats_dept(mock)
  expect_true(all(diff(stats$taux_mort) <= 0),
              label = "stats_dept pas trié par taux_mort décroissant")
})

test_that("sum(nb) dans stats_dept == nrow(mock)", {
  stats <- calc_stats_dept(mock)
  expect_equal(sum(stats$nb), nrow(mock))
})

# ── Tests sur données réelles si disponibles ──────────────────
test_that("KPIs réels sont dans des plages réalistes", {
  skip_if_not(file.exists("data_propre/accidents_final_light.rds"))
  acc  <- readRDS("data_propre/accidents_final_light.rds")
  kpi  <- calc_kpis_region(acc)
  taux <- calc_taux_mortalite(acc)

  expect_gte(kpi$nb_acc, 500000)
  expect_gte(taux, 3)   # taux mortalité national > 3%
  expect_lte(taux, 15)  # taux mortalité national < 15%
})
