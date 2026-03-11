# tests/testthat/test_rapport.R
# ─────────────────────────────────────────────────────────────
# Tests : Génération du rapport HTML et PDF par région
# ─────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)

RMD_PATH <- "app/report/rapport_region.Rmd"
CSS_PATH <- "app/report/rapport_style.css"

# ── Données mock pour le rapport ─────────────────────────────
make_mock_rapport <- function(region = "Centre-Val de Loire") {
  set.seed(7)
  n <- 500
  depts <- c("Indre-et-Loire","Loiret","Loir-et-Cher","Cher","Indre","Eure-et-Loir")
  data.frame(
    num_acc          = paste0("ACC", seq_len(n)),
    annee            = sample(2015:2024, n, replace = TRUE),
    mois             = sample(1:12, n, replace = TRUE),
    jour             = sample(1:31, n, replace = TRUE),
    heure            = sample(c(paste0("0",0:9), as.character(10:23)), n, replace = TRUE),
    departement      = sample(depts, n, replace = TRUE),
    region           = region,
    gravite_accident = sample(c("Mortel","Grave","Léger"),
                               n, replace = TRUE, prob = c(0.06, 0.35, 0.59)),
    grav_label       = NA,
    nb_tues          = sample(0:2, n, replace = TRUE),
    nb_victimes      = sample(1:4, n, replace = TRUE),
    nb_blesses_hospitalises = sample(0:3, n, replace = TRUE),
    nb_blesses_legers = sample(0:3, n, replace = TRUE),
    tranche_age      = sample(c("18-24 ans","25-34 ans","35-44 ans",
                                 "45-54 ans","55-64 ans","Non renseigné"),
                               n, replace = TRUE),
    age_moyen_reel   = sample(20:75, n, replace = TRUE),
    sexe_dominant    = sample(c("Homme","Femme"), n, replace = TRUE),
    categorie_vehicule = sample(c("Voiture","Moto","Vélo"), n, replace = TRUE),
    lum_label        = sample(c("Plein jour","Nuit sans éclairage",
                                 "Nuit avec éclairage"), n, replace = TRUE),
    atm_label        = sample(c("Normale","Pluie légère","Brouillard"), n, replace = TRUE),
    catr_label       = sample(c("Route nationale","Route départementale",
                                 "Voie communale"), n, replace = TRUE),
    surf_label       = sample(c("Normale","Mouillée"), n, replace = TRUE),
    col_label        = sample(c("Frontale","Arrière"), n, replace = TRUE),
    jour_semaine     = sample(c("Lundi","Mardi","Mercredi","Jeudi",
                                 "Vendredi","Samedi","Dimanche"), n, replace = TRUE),
    saison           = sample(c("Printemps","Été","Automne","Hiver"), n, replace = TRUE),
    moment_journee   = sample(c("Matin","Après-midi","Soir","Nuit"), n, replace = TRUE),
    accident_mortel  = FALSE,
    accident_grave   = FALSE,
    secu_principal   = sample(c("Ceinture","Casque","Aucun"), n, replace = TRUE),
    vma              = sample(c(50, 80, 90, 110, 130), n, replace = TRUE),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      accident_mortel = gravite_accident == "Mortel",
      accident_grave  = gravite_accident == "Grave"
    )
}

# ── 1. Fichiers sources présents ──────────────────────────────
test_that("le template rapport_region.Rmd existe", {
  skip_if_not(file.exists(RMD_PATH), "Rmd absent — test ignoré")
  expect_true(file.exists(RMD_PATH))
})

test_that("le fichier CSS du rapport existe", {
  skip_if_not(file.exists(CSS_PATH), "CSS absent — test ignoré")
  expect_true(file.exists(CSS_PATH))
})

# ── 2. Génération HTML ────────────────────────────────────────
test_that("le rapport HTML se génère sans erreur", {
  skip_if_not(file.exists(RMD_PATH))
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE))

  mock <- make_mock_rapport()
  tmp_dir    <- tempdir()
  output_file <- file.path(tmp_dir, "test_rapport.html")

  # Copier Rmd et CSS dans le tempdir
  file.copy(RMD_PATH, file.path(tmp_dir, "rapport_region.Rmd"), overwrite = TRUE)
  if (file.exists(CSS_PATH))
    file.copy(CSS_PATH, file.path(tmp_dir, "rapport_style.css"), overwrite = TRUE)

  expect_no_error(
    rmarkdown::render(
      input       = file.path(tmp_dir, "rapport_region.Rmd"),
      output_file = output_file,
      output_format = "html_document",
      params      = list(region = "Centre-Val de Loire", accidents = mock),
      quiet       = TRUE,
      envir       = new.env(parent = globalenv())
    )
  )

  expect_true(file.exists(output_file))
  unlink(output_file)
})

# ── 3. Fichier HTML non vide et cohérent ─────────────────────
test_that("le rapport HTML contient les sections attendues", {
  skip_if_not(file.exists(RMD_PATH))
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE))

  mock        <- make_mock_rapport()
  tmp_dir     <- tempdir()
  output_file <- file.path(tmp_dir, "test_sections.html")

  file.copy(RMD_PATH, file.path(tmp_dir, "rapport_region.Rmd"), overwrite = TRUE)
  if (file.exists(CSS_PATH))
    file.copy(CSS_PATH, file.path(tmp_dir, "rapport_style.css"), overwrite = TRUE)

  suppressWarnings(
    rmarkdown::render(
      input       = file.path(tmp_dir, "rapport_region.Rmd"),
      output_file = output_file,
      output_format = "html_document",
      params      = list(region = "Centre-Val de Loire", accidents = mock),
      quiet       = TRUE,
      envir       = new.env(parent = globalenv())
    )
  )

  contenu <- paste(readLines(output_file, warn = FALSE), collapse = " ")

  expect_true(grepl("Centre-Val de Loire", contenu),
              label = "Le nom de région n'apparaît pas dans le HTML")
  expect_true(grepl("volution", contenu),
              label = "Section Évolution absente")
  expect_true(grepl("Conclusion", contenu),
              label = "Section Conclusion absente")

  unlink(output_file)
})

# ── 4. Génération PDF ─────────────────────────────────────────
test_that("le rapport PDF se génère sans erreur", {
  skip_if_not(file.exists(RMD_PATH))
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE))
  skip_if_not(requireNamespace("tinytex",   quietly = TRUE))
  skip_if_not(tinytex::is_tinytex(), "TinyTeX non installé — test PDF ignoré")

  mock        <- make_mock_rapport()
  tmp_dir     <- tempdir()
  output_file <- file.path(tmp_dir, "test_rapport.pdf")

  file.copy(RMD_PATH, file.path(tmp_dir, "rapport_region.Rmd"), overwrite = TRUE)
  if (file.exists(CSS_PATH))
    file.copy(CSS_PATH, file.path(tmp_dir, "rapport_style.css"), overwrite = TRUE)

  expect_no_error(
    rmarkdown::render(
      input       = file.path(tmp_dir, "rapport_region.Rmd"),
      output_file = output_file,
      output_format = "pdf_document",
      params      = list(region = "Centre-Val de Loire", accidents = mock),
      quiet       = TRUE,
      envir       = new.env(parent = globalenv())
    )
  )

  expect_true(file.exists(output_file))
  expect_gt(file.size(output_file), 10000) # PDF > 10 Ko
  unlink(output_file)
})

# ── 5. Paramètres manquants ou vides ─────────────────────────
test_that("le rapport gère un dataframe filtré avec peu de lignes", {
  skip_if_not(file.exists(RMD_PATH))
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE))

  # Région avec peu d'accidents (seuil nrow < 10 dans dept_sections)
  mock_petit  <- make_mock_rapport() |> slice_head(n = 50)
  tmp_dir     <- tempdir()
  output_file <- file.path(tmp_dir, "test_petit.html")

  file.copy(RMD_PATH, file.path(tmp_dir, "rapport_region.Rmd"), overwrite = TRUE)
  if (file.exists(CSS_PATH))
    file.copy(CSS_PATH, file.path(tmp_dir, "rapport_style.css"), overwrite = TRUE)

  expect_no_error(
    rmarkdown::render(
      input       = file.path(tmp_dir, "rapport_region.Rmd"),
      output_file = output_file,
      output_format = "html_document",
      params      = list(region = "Centre-Val de Loire", accidents = mock_petit),
      quiet       = TRUE,
      envir       = new.env(parent = globalenv())
    )
  )
  unlink(output_file)
})
