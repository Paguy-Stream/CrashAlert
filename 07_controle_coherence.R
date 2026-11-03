# ==============================================================================
# 07_controle_coherence.R : contrôle de cohérence entre onglets et avec l'ONISR
# ==============================================================================
# Usage (depuis la racine du projet, renv actif) :
#   1. Copier onisr_2025_departements.csv dans donnees/
#   2. source("07_controle_coherence.R")
# Ne modifie aucun fichier de l'application. Écrit un CSV de résultats à côté du projet.
# ==============================================================================
suppressPackageStartupMessages(library(dplyr))

d    <- readRDS("data_propre/accidents_dashboard.rds")
ev   <- readRDS("data_propre/agg_evol_dept.rds")
evf  <- readRDS("data_propre/agg_evol_france.rds")
ad   <- readRDS("data_propre/agg_departement.rds")
ada  <- readRDS("data_propre/agg_dept_annee.rds")
ft   <- readRDS("data_propre/features_clustering.rds")
meta <- attr(ft, "meta")

pick <- function(df, ...) { x <- intersect(c(...), names(df)); if (length(x)) x[1] else NA_character_ }
col_dep   <- pick(d, "dep_clean", "dep", "departement_code")
col_tues  <- pick(d, "nb_tues", "tues", "nb_tue", "tues_30j")
col_grav  <- "gravite_accident"
stopifnot(!is.na(col_dep), col_grav %in% names(d), "annee" %in% names(d))
d$.dep <- as.character(d[[col_dep]])
metro_codes <- as.character(ft$dep_clean)
dm <- d[d$.dep %in% metro_codes, ]

cat("\nColonnes détectées : département =", col_dep, "| tués =", col_tues, "\n")
cat("agg_evol_france  :", paste(names(evf), collapse = ", "), "\n")
cat("agg_departement  :", paste(names(ad),  collapse = ", "), "\n")
cat("agg_dept_annee   :", paste(names(ada), collapse = ", "), "\n\n")

res <- list()
chk <- function(nom, a, b, tol = 0) {
  a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
  ok <- length(a) == length(b) && !anyNA(c(a, b)) && all(abs(a - b) <= tol)
  res[[length(res) + 1]] <<- data.frame(controle = nom,
    valeur_a = paste(head(round(a, 2), 3), collapse = " | "),
    valeur_b = paste(head(round(b, 2), 3), collapse = " | "), ok = ok)
  cat(if (ok) "[OK]  " else "[ÉCART]", nom, "\n")
  if (!ok) cat("        a =", paste(head(round(a, 2), 5), collapse = ", "),
               "\n        b =", paste(head(round(b, 2), 5), collapse = ", "), "\n")
}
skip <- function(nom, why) cat("[SAUTÉ]", nom, "-", why, "\n")

# ── A. Cohérence interne : même total partout ────────────────────────────────
cat("== A. Cohérence interne ==\n")
chk("Accidents métropole : dashboard = features_clustering",
    nrow(dm), sum(ft$nb_accidents))
if (!is.null(meta$n_accidents)) chk("Accidents métropole : dashboard = meta (en-tête Statistiques)",
    nrow(dm), meta$n_accidents)
chk("Accidents métropole : dashboard = agg_evol_dept", nrow(dm), sum(ev$n))

c_n_ad <- pick(ad, "nb_accidents", "n")
if (!is.na(c_n_ad)) chk("Accidents (tous territoires) : dashboard = agg_departement",
    nrow(d), sum(ad[[c_n_ad]])) else skip("dashboard = agg_departement", "colonne introuvable")

c_n_ada <- pick(ada, "nb_accidents", "n")
if (!is.na(c_n_ada)) chk("Accidents (tous territoires) : dashboard = agg_dept_annee",
    nrow(d), sum(ada[[c_n_ada]])) else skip("dashboard = agg_dept_annee", "colonne introuvable")

# Accidents mortels
mort_d <- sum(d[[col_grav]] == "Mortel", na.rm = TRUE)
c_m_ad <- pick(ad, "nb_mortels")
if (!is.na(c_m_ad)) chk("Accidents mortels : dashboard = agg_departement", mort_d, sum(ad[[c_m_ad]]))
mort_ev <- sum(round(ev$n * ev$taux_mort / 100))
chk("Accidents mortels métropole : dashboard = agg_evol_dept (arrondis)",
    sum(dm[[col_grav]] == "Mortel", na.rm = TRUE), mort_ev, tol = 60)

# Par année
an_d  <- dm |> count(annee) |> arrange(annee)
an_ev <- ev |> group_by(annee) |> summarise(n = sum(n), .groups = "drop") |> arrange(annee)
chk("Accidents par année (métropole) : dashboard = agg_evol_dept", an_d$n, an_ev$n)
c_n_evf <- pick(evf, "n", "nb_accidents")
if (!is.na(c_n_evf) && nrow(evf) == nrow(an_d))
  chk("Accidents par année : dashboard = agg_evol_france", an_d$n, evf[[c_n_evf]][order(evf$annee)])

# Par département
dep_d <- dm |> count(.dep) |> arrange(.dep)
chk("Accidents par département : dashboard = features_clustering",
    dep_d$n, ft$nb_accidents[match(dep_d$.dep, as.character(ft$dep_clean))])
tx_d <- dm |> group_by(.dep) |> summarise(t = mean(.data[[col_grav]] == "Mortel", na.rm = TRUE) * 100) |> arrange(.dep)
chk("Taux de mortalité par département : dashboard = features_clustering (±0,1 pt)",
    tx_d$t, ft$taux_mortalite[match(tx_d$.dep, as.character(ft$dep_clean))], tol = 0.1)

# Taux national : deux définitions à ne pas confondre
tx_pool <- mean(dm[[col_grav]] == "Mortel", na.rm = TRUE) * 100
tx_moy  <- mean(ft$taux_mortalite)
cat(sprintf("\n[INFO] Taux de mortalité métropole 2015-2025 : %.2f %% (pondéré par les accidents)\n", tx_pool))
cat(sprintf("[INFO] Moyenne simple des 96 taux départementaux : %.2f %% (non pondérée)\n", tx_moy))
cat("       Les deux ne sont pas comparables : à citer avec leur définition (voir note).\n\n")

# ── B. Référence externe ONISR 2025 ──────────────────────────────────────────
cat("== B. Comparaison avec le bilan ONISR 2025 ==\n")
f_ref <- "donnees/onisr_2025_departements.csv"
if (!file.exists(f_ref)) {
  cat("Fichier absent :", f_ref, "\n")
} else {
  ref <- read.csv(f_ref, colClasses = c(dep = "character"), encoding = "UTF-8")
  d25 <- d |> filter(annee == 2025) |> count(dep = .dep, name = "app_acc")
  cmp <- full_join(ref |> select(dep, nom, onisr_acc = nb_accidents, onisr_tues = nb_tues), d25, by = "dep") |>
    mutate(app_acc = coalesce(app_acc, 0L), ecart_acc = app_acc - onisr_acc)
  if (!is.na(col_tues)) {
    t25 <- d |> filter(annee == 2025) |> group_by(dep = .dep) |>
      summarise(app_tues = sum(.data[[col_tues]], na.rm = TRUE), .groups = "drop")
    cmp <- left_join(cmp, t25, by = "dep") |> mutate(app_tues = coalesce(app_tues, 0), ecart_tues = app_tues - onisr_tues)
  }
  me <- cmp$dep %in% metro_codes
  chk("ONISR 2025 : accidents métropole (51 837)", sum(cmp$app_acc[me]), sum(cmp$onisr_acc[me]))
  chk("ONISR 2025 : accidents par département (96 valeurs)", cmp$app_acc[me], cmp$onisr_acc[me])
  if (!is.na(col_tues)) {
    chk("ONISR 2025 : tués métropole (3 263)", sum(cmp$app_tues[me]), sum(cmp$onisr_tues[me]))
    chk("ONISR 2025 : tués outre-mer (252)", sum(cmp$app_tues[!me]), sum(cmp$onisr_tues[!me]))
    chk("ONISR 2025 : tués par département (96 valeurs)", cmp$app_tues[me], cmp$onisr_tues[me])
  } else skip("ONISR tués", "aucune colonne de tués trouvée dans le dashboard")
  chk("ONISR 2025 : accidents outre-mer (3 256)", sum(cmp$app_acc[!me]), sum(cmp$onisr_acc[!me]))
  bad <- cmp$ecart_acc != 0
  if ("ecart_tues" %in% names(cmp)) bad <- bad | coalesce(cmp$ecart_tues, 0) != 0
  ec <- cmp[bad, ]
  if (nrow(ec)) { cat("\nDépartements en écart :\n"); print(as.data.frame(ec), row.names = FALSE) }
}

out <- do.call(rbind, res)
cat(sprintf("\nBilan : %d contrôles, %d OK, %d en écart\n", nrow(out), sum(out$ok), sum(!out$ok)))
write.csv(out, sprintf("../controle_coherence_%s.csv", Sys.Date()), row.names = FALSE, fileEncoding = "UTF-8")
