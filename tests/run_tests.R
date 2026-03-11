# tests/run_tests.R
# ─────────────────────────────────────────────────────────────
#  Lance toute la suite de tests CrashAlert
#  Usage depuis RStudio : source("tests/run_tests.R")
#  Usage depuis terminal : Rscript tests/run_tests.R
# ─────────────────────────────────────────────────────────────

# ── Activer renv pour accéder aux packages du projet ─────────
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}



if (!requireNamespace("testthat", quietly = TRUE))
  renv::install("testthat")

library(testthat)
library(dplyr)

cat("\n")
cat("╔══════════════════════════════════════════════════╗\n")
cat("║        CrashAlert — Suite de tests               ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

# Lancer depuis la racine du projet
# Racine du projet CrashAlert
root <- "C:/Users/emman/Documents/mon_projet_rhino/CrashAlert"
setwd(root)
cat(sprintf("  📁 Racine projet : %s\n\n", root))

# Exclure test-main.R (fichier rhino interne, charge 'box')
fichiers_tests <- list.files(
  "tests/testthat",
  pattern = "^test_.*[.]R$",  # seulement nos fichiers test_*.R
  full.names = TRUE
)

cat(sprintf("  📋 %d fichiers de tests trouvés\n\n", length(fichiers_tests)))

resultats <- lapply(fichiers_tests, function(f) {
  testthat::test_file(f, reporter = "progress")
})

cat("\n─────────────────────────────────────────────────────\n")
cat("  RÉSUMÉ\n")
cat("─────────────────────────────────────────────────────\n")

# Agréger les résultats de chaque fichier
ok       <- sum(sapply(resultats, function(r) sum(as.data.frame(r)$passed,  na.rm=TRUE)))
echecs   <- sum(sapply(resultats, function(r) sum(as.data.frame(r)$failed,  na.rm=TRUE)))
ignores  <- sum(sapply(resultats, function(r) sum(as.data.frame(r)$skipped, na.rm=TRUE)))
warnings <- sum(sapply(resultats, function(r) sum(as.data.frame(r)$warning, na.rm=TRUE)))

cat(sprintf("  ✅ Réussis   : %d\n",  ok))
cat(sprintf("  ❌ Échoués   : %d\n",  echecs))
cat(sprintf("  ⏭  Ignorés   : %d\n",  ignores))
cat(sprintf("  ⚠️  Warnings  : %d\n",  warnings))
cat("─────────────────────────────────────────────────────\n\n")

if (echecs == 0) {
  cat("🎉 Tous les tests passent — prêt pour le push GitHub !\n\n")
} else {
  cat("🚨 Des tests échouent — corriger avant de pusher.\n\n")
  stop("Tests en échec.", call. = FALSE)
}
