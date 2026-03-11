# tests/testthat/test_exports.R
# ─────────────────────────────────────────────────────────────
# Tests : Exports CSV — structure, contenu, encodage
# ─────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)

# ── Données mock ──────────────────────────────────────────────
make_mock_export <- function() {
  data.frame(
    num_acc          = paste0("ACC", 1:200),
    annee            = sample(2015:2024, 200, replace = TRUE),
    departement      = sample(c("Indre-et-Loire","Loiret","Cher"), 200, replace = TRUE),
    region           = "Centre-Val de Loire",
    gravite_accident = sample(c("Mortel","Grave","Léger"), 200, replace = TRUE,
                               prob = c(0.06, 0.35, 0.59)),
    categorie_vehicule = sample(c("Voiture","Moto","Vélo"), 200, replace = TRUE),
    heure            = sample(0:23, 200, replace = TRUE),
    jour_semaine     = sample(c("Lundi","Mardi","Mercredi"), 200, replace = TRUE),
    nb_tues          = sample(0:2, 200, replace = TRUE),
    nb_victimes      = sample(1:4, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

mock <- make_mock_export()

# ── Fonction d'export reproduisant la logique downloadHandler ─
export_csv <- function(df, filepath, cols = NULL) {
  if (!is.null(cols)) df <- df |> select(any_of(cols))
  write.csv(df, file = filepath, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(filepath)
}

# ── 1. Fichier créé ───────────────────────────────────────────
test_that("export_csv crée bien un fichier", {
  tmp <- tempfile(fileext = ".csv")
  export_csv(mock, tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})

# ── 2. Fichier non vide ───────────────────────────────────────
test_that("export_csv produit un fichier non vide", {
  tmp <- tempfile(fileext = ".csv")
  export_csv(mock, tmp)
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})

# ── 3. Relecture cohérente ────────────────────────────────────
test_that("le CSV relu contient le bon nombre de lignes", {
  tmp <- tempfile(fileext = ".csv")
  export_csv(mock, tmp)
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_equal(nrow(relu), nrow(mock))
  unlink(tmp)
})

test_that("le CSV relu contient les mêmes colonnes que l'original", {
  tmp <- tempfile(fileext = ".csv")
  export_csv(mock, tmp)
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_equal(sort(names(relu)), sort(names(mock)))
  unlink(tmp)
})

# ── 4. Sélection de colonnes ──────────────────────────────────
test_that("export_csv avec sélection de colonnes fonctionne", {
  tmp <- tempfile(fileext = ".csv")
  cols_voulues <- c("num_acc", "annee", "gravite_accident")
  export_csv(mock, tmp, cols = cols_voulues)
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_equal(sort(names(relu)), sort(cols_voulues))
  unlink(tmp)
})

test_that("export_csv ignore les colonnes inexistantes sans erreur", {
  tmp <- tempfile(fileext = ".csv")
  expect_no_error(
    export_csv(mock, tmp, cols = c("num_acc", "colonne_inexistante"))
  )
  unlink(tmp)
})

# ── 5. Données filtrées ───────────────────────────────────────
test_that("export filtré sur 'Mortel' ne contient que des mortels", {
  tmp <- tempfile(fileext = ".csv")
  df_filtre <- mock |> filter(gravite_accident == "Mortel")
  export_csv(df_filtre, tmp)
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_true(all(relu$gravite_accident == "Mortel"))
  unlink(tmp)
})

test_that("export d'un dataframe vide ne plante pas", {
  tmp <- tempfile(fileext = ".csv")
  df_vide <- mock |> filter(gravite_accident == "Inexistant")
  expect_no_error(export_csv(df_vide, tmp))
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_equal(nrow(relu), 0)
  unlink(tmp)
})

# ── 6. Encodage UTF-8 (accents) ───────────────────────────────
test_that("les accents sont préservés dans le CSV", {
  tmp <- tempfile(fileext = ".csv")
  df_accents <- data.frame(
    label = c("Léger", "Île-de-France", "Côte-d'Armor"),
    stringsAsFactors = FALSE
  )
  write.csv(df_accents, file = tmp, row.names = FALSE, fileEncoding = "UTF-8")
  relu <- read.csv(tmp, encoding = "UTF-8")
  expect_equal(relu$label[1], "Léger")
  expect_equal(relu$label[2], "Île-de-France")
  unlink(tmp)
})

# ── 7. Taille fichier raisonnable ─────────────────────────────
test_that("un export de 200 lignes fait moins de 100 Ko", {
  tmp <- tempfile(fileext = ".csv")
  export_csv(mock, tmp)
  taille_ko <- file.size(tmp) / 1024
  expect_lt(taille_ko, 100)
  unlink(tmp)
})
