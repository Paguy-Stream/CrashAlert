# ==============================================================================
# 00_config.R — Configuration du pipeline BAAC
# ==============================================================================
# Usage : modifier uniquement ce fichier pour traiter une nouvelle année.
# Lancer ensuite : source("run_pipeline.R")
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. ANNÉE CIBLE
# ------------------------------------------------------------------------------
# Changer cette valeur pour traiter une nouvelle année (ex: 2025)
ANNEE_CIBLE <- 2024


# ------------------------------------------------------------------------------
# 2. CHEMINS
# ------------------------------------------------------------------------------

# Racine du projet CrashAlert
CHEMIN_PROJET <- "C:/Users/emman/Documents/mon_projet_rhino/CrashAlert"

# Dossier contenant les CSV bruts BAAC
# Structure attendue : donnees/caracteristiques_2024.csv, lieux_2024.csv, etc.
CHEMIN_DONNEES <- file.path(CHEMIN_PROJET, "donnees")

# Dossier de sortie des fichiers .rds produits par le pipeline
CHEMIN_OUTPUT  <- file.path(CHEMIN_PROJET, "data_pipeline")

# Dossier final utilisé par l'application Shiny
CHEMIN_SHINY   <- file.path(CHEMIN_PROJET, "data_propre")

# Dossier des logs
CHEMIN_LOGS    <- file.path(CHEMIN_PROJET, "data_pipeline", "logs")


# ------------------------------------------------------------------------------
# 3. NOMS DES FICHIERS CSV ATTENDUS PAR ANNÉE
# ------------------------------------------------------------------------------
# Le pipeline détecte automatiquement le bon fichier pour chaque type.
# Patterns acceptés (les deux formes existent selon les années BAAC) :
#   caracteristiques_YYYY.csv  ou  carcteristiques_YYYY.csv
#   lieux_YYYY.csv
#   usagers_YYYY.csv
#   vehicules_YYYY.csv

PATTERNS_CSV <- list(
  caracteristiques = c("caracteristiques", "carcteristiques"),
  lieux            = c("lieux"),
  usagers          = c("usagers"),
  vehicules        = c("vehicules")
)


# ------------------------------------------------------------------------------
# 4. PARAMÈTRES DE NETTOYAGE
# ------------------------------------------------------------------------------

# Seuil de données manquantes au-delà duquel une variable est signalée (%)
SEUIL_NA <- 10

# Plage d'âge valide (valeurs hors plage → NA)
AGE_MIN <- 0
AGE_MAX <- 120

# Départements DOM-TOM à exclure de l'analyse métropolitaine
DOMTOM <- c("971","972","973","974","975","976","977","978",
            "984","985","986","987","988","989")

# Coordonnées géographiques valides pour la France métropolitaine
LAT_MIN <-  41.0
LAT_MAX <-  51.5
LON_MIN <-  -5.5
LON_MAX <-   9.8


# ------------------------------------------------------------------------------
# 5. PARAMÈTRES D'EXPORT
# ------------------------------------------------------------------------------

# Colonnes conservées dans accidents_dashboard.rds (usage Shiny principal)
COLONNES_DASHBOARD <- c(
  "Num_Acc", "annee", "mois", "jour", "heure", "minute",
  "dep", "departement", "region",
  "lat", "long", "coords_valides",
  "lum_label", "atm_label", "col_label", "catr_label", "surf_label",
  "gravite_accident", "nb_victimes", "nb_tues", "nb_blesses_hospitalises",
  "age_moyen_reel", "moment_journee", "categorie_vehicule",
  "sexe_dominant", "tranche_age", "jour_semaine", "saison",
  "catu_principal", "secu_principal"
)
COLONNES_LIGHT_EXTRA <- c(
  "sexe_dominant", "tranche_age", "jour_semaine", "saison",
  "catu_principal", "secu_principal"
)


# ------------------------------------------------------------------------------
# 6. LOGGING
# ------------------------------------------------------------------------------

# Créer les dossiers si absents
if (!dir.exists(CHEMIN_OUTPUT)) dir.create(CHEMIN_OUTPUT, recursive = TRUE)
if (!dir.exists(CHEMIN_LOGS))   dir.create(CHEMIN_LOGS,   recursive = TRUE)

# Fichier de log horodaté pour cette exécution
LOG_FILE <- file.path(
  CHEMIN_LOGS,
  paste0("pipeline_", ANNEE_CIBLE, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# Fonction de log utilisée par tous les modules
log_msg <- function(msg, niveau = "INFO") {
  horodatage <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ligne <- sprintf("[%s] [%s] %s", horodatage, niveau, msg)
  message(ligne)
  cat(ligne, "\n", file = LOG_FILE, append = TRUE)
}

log_msg(paste("Pipeline BAAC démarré — année cible :", ANNEE_CIBLE))
log_msg(paste("Dossier données :", CHEMIN_DONNEES))
log_msg(paste("Dossier output  :", CHEMIN_OUTPUT))
