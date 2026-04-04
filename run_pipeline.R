# ==============================================================================
# run_pipeline.R — Point d'entrée unique du pipeline BAAC
# ==============================================================================
# Usage :
#   1. Ouvrir 00_config.R et modifier ANNEE_CIBLE
#   2. Déposer les CSV de l'année dans donnees/
#   3. Lancer : source("run_pipeline.R")
#
# Pour rejouer une étape seule (ex: après correction) :
#   source("00_config.R")
#   source("02_nettoyage.R")   # repart des RDS intermédiaires
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. CONFIGURATION
# ------------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("  PIPELINE BAAC — CrashAlert\n")
cat(strrep("=", 70), "\n\n")

source("00_config.R")

cat(sprintf("Année cible    : %d\n", ANNEE_CIBLE))
cat(sprintf("Dossier CSV    : %s\n", CHEMIN_DONNEES))
cat(sprintf("Dossier output : %s\n", CHEMIN_OUTPUT))
cat(sprintf("Dossier Shiny  : %s\n", CHEMIN_SHINY))
cat(sprintf("Log            : %s\n\n", LOG_FILE))


# ------------------------------------------------------------------------------
# 1. FONCTION D'EXÉCUTION SÉCURISÉE
# ------------------------------------------------------------------------------

.chrono <- function(etape, fichier) {
  cat(sprintf("\n[%s] Démarrage...\n", etape))
  t0 <- proc.time()
  tryCatch(
    source(fichier),
    error = function(e) {
      cat(sprintf("\n[%s] ERREUR : %s\n", etape, conditionMessage(e)))
      log_msg(sprintf("ERREUR dans %s : %s", etape, conditionMessage(e)), "ERROR")
      stop(sprintf("Pipeline interrompu à l'étape %s", etape), call. = FALSE)
    }
  )
  duree <- round((proc.time() - t0)[["elapsed"]], 1)
  cat(sprintf("[%s] Terminé en %.1fs\n", etape, duree))
  log_msg(sprintf("%s terminé en %.1fs", etape, duree))
}


# ------------------------------------------------------------------------------
# 2. EXÉCUTION DES ÉTAPES
# ------------------------------------------------------------------------------

.chrono("01_import",        "01_import.R")
.chrono("02_nettoyage",     "02_nettoyage.R")
.chrono("03_fusion",        "03_fusion.R")
.chrono("04_enrichissement","04_enrichissement.R")
.chrono("05_export",        "05_export.R")


# ------------------------------------------------------------------------------
# 3. RÉSUMÉ FINAL
# ------------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("  PIPELINE TERMINÉ\n")
cat(strrep("=", 70), "\n")
cat(sprintf("  Année traitée   : %d\n", ANNEE_CIBLE))
cat(sprintf("  Accidents total : %s\n",
            format(nrow(accidents_all), big.mark = " ")))
cat(sprintf("  Log complet     : %s\n", LOG_FILE))
cat(sprintf("  Fichiers Shiny  : %s\n", CHEMIN_SHINY))
cat("\n  Fichiers produits :\n")
cat("    accidents_dashboard.rds\n")
cat("    accidents_light.rds\n")
cat("    filters.rds\n")
cat(strrep("=", 70), "\n\n")

log_msg(sprintf("Pipeline BAAC terminé — %d accidents consolidés",
                nrow(accidents_all)))
