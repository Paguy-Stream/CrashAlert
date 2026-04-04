# ==============================================================================
# 04_enrichissement.R — Variables dérivées, labels et référentiel géographique
# ==============================================================================
# Dépendances : 00_config.R + 01 + 02 + 03 sourcés avant.
# Entrée      : accidents (data frame au niveau accident)
# Produit     : accidents enrichi avec labels, région, département,
#               variables temporelles et catégorielles
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

log_msg("=== 04_enrichissement.R — début ===")


# ------------------------------------------------------------------------------
# 1. VARIABLES TEMPORELLES
# ------------------------------------------------------------------------------

log_msg("Création des variables temporelles...")

accidents <- accidents |>
  mutate(
    # Année consolidée depuis `an`
    annee = as.numeric(an),

    # Saison
    mois_num = as.numeric(mois),
    saison = case_when(
      mois_num %in% c(12, 1, 2) ~ "Hiver",
      mois_num %in% c(3, 4, 5)  ~ "Printemps",
      mois_num %in% c(6, 7, 8)  ~ "Été",
      mois_num %in% c(9, 10, 11) ~ "Automne",
      TRUE                       ~ "Non renseigné"
    ),

    # Jour de la semaine (via date reconstituée)
    date_accident = suppressWarnings(
      as.Date(paste(annee, str_pad(mois, 2, pad = "0"),
                    str_pad(as.numeric(jour), 2, pad = "0"), sep = "-"))
    ),
    jour_semaine = ifelse(
      !is.na(date_accident),
      tolower(weekdays(date_accident)),
      NA_character_
    ),

    # Moment de la journée (depuis heure extraite en 02_nettoyage.R)
    heure_num = as.numeric(heure),
    moment_journee = case_when(
      heure_num >= 0  & heure_num < 6  ~ "Nuit (0h-6h)",
      heure_num >= 6  & heure_num < 9  ~ "Matin (6h-9h)",
      heure_num >= 9  & heure_num < 12 ~ "Matinée (9h-12h)",
      heure_num >= 12 & heure_num < 14 ~ "Midi (12h-14h)",
      heure_num >= 14 & heure_num < 17 ~ "Après-midi (14h-17h)",
      heure_num >= 17 & heure_num < 20 ~ "Soirée (17h-20h)",
      heure_num >= 20                  ~ "Nuit (20h-0h)",
      TRUE                             ~ "Non renseigné"
    )
  ) |>
  select(-mois_num, -heure_num, -date_accident)

log_msg("Variables temporelles créées")


# ------------------------------------------------------------------------------
# 2. LABELS BAAC (codes numériques → libellés)
# ------------------------------------------------------------------------------
# Source : documentation officielle BAAC (ONISR)

log_msg("Ajout des labels BAAC...")

.ajouter_label <- function(df, col, codes, labels) {
  if (!col %in% names(df)) return(df)
  ref <- tibble(
    !!col         := as.character(codes),
    !!paste0(col, "_label") := labels
  )
  df |>
    mutate(!!col := as.character(.data[[col]])) |>
    left_join(ref, by = col) |>
    mutate(!!paste0(col, "_label") := ifelse(
      is.na(.data[[paste0(col, "_label")]]),
      paste0("Code ", .data[[col]]),
      .data[[paste0(col, "_label")]]
    ))
}

# Luminosité
accidents <- .ajouter_label(accidents, "lum",
  codes  = c("-1","1","2","3","4","5"),
  labels = c("Non renseigné","Plein jour","Crépuscule ou aube",
             "Nuit sans éclairage public","Nuit avec éclairage public éteint",
             "Nuit avec éclairage public allumé")
)

# Conditions atmosphériques
accidents <- .ajouter_label(accidents, "atm",
  codes  = c("-1","1","2","3","4","5","6","7","8","9"),
  labels = c("Non renseigné","Normale","Pluie légère","Pluie forte",
             "Neige - grêle","Brouillard - fumée","Vent fort - tempête",
             "Temps éblouissant","Temps couvert","Autre")
)

# Type de collision
accidents <- .ajouter_label(accidents, "col",
  codes  = c("-1","1","2","3","4","5","6","7"),
  labels = c("Non renseigné","Deux véhicules - frontale",
             "Deux véhicules - par l'arrière","Deux véhicules - par le côté",
             "Trois véhicules et plus - en chaîne",
             "Trois véhicules et plus - collisions multiples",
             "Autre collision","Sans collision")
)

# Catégorie de route
accidents <- .ajouter_label(accidents, "catr",
  codes  = c("-1","1","2","3","4","5","6","7","9"),
  labels = c("Non renseigné","Autoroute","Route nationale","Route départementale",
             "Voie communale","Hors réseau public","Parc de stationnement",
             "Routes de métropole urbaine","Autre")
)

# État de la surface
accidents <- .ajouter_label(accidents, "surf",
  codes  = c("-1","0","1","2","3","4","5","6","7","8","9"),
  labels = c("Non renseigné","Sans objet","Normale","Mouillée","Flaques",
             "Inondée","Enneigée","Boue","Verglacée","Corps gras / huile","Autre")
)

log_msg(sprintf("Labels ajoutés : %s",
  paste(grep("_label$", names(accidents), value = TRUE), collapse = ", ")))


# ------------------------------------------------------------------------------
# 3. RÉFÉRENTIEL GÉOGRAPHIQUE — DÉPARTEMENT ET RÉGION
# ------------------------------------------------------------------------------

log_msg("Ajout des noms de département et région...")

.REF_GEO <- tibble(
  code_dep = c(
    "01","02","03","04","05","06","07","08","09","10",
    "11","12","13","14","15","16","17","18","19","21",
    "22","23","24","25","26","27","28","29","2A","2B",
    "30","31","32","33","34","35","36","37","38","39",
    "40","41","42","43","44","45","46","47","48","49",
    "50","51","52","53","54","55","56","57","58","59",
    "60","61","62","63","64","65","66","67","68","69",
    "70","71","72","73","74","75","76","77","78","79",
    "80","81","82","83","84","85","86","87","88","89",
    "90","91","92","93","94","95",
    "971","972","973","974","976"
  ),
  departement = c(
    "Ain","Aisne","Allier","Alpes-de-Haute-Provence","Hautes-Alpes",
    "Alpes-Maritimes","Ardèche","Ardennes","Ariège","Aube",
    "Aude","Aveyron","Bouches-du-Rhône","Calvados","Cantal",
    "Charente","Charente-Maritime","Cher","Corrèze","Côte-d'Or",
    "Côtes-d'Armor","Creuse","Dordogne","Doubs","Drôme",
    "Eure","Eure-et-Loir","Finistère","Corse-du-Sud","Haute-Corse",
    "Gard","Haute-Garonne","Gers","Gironde","Hérault",
    "Ille-et-Vilaine","Indre","Indre-et-Loire","Isère","Jura",
    "Landes","Loir-et-Cher","Loire","Haute-Loire","Loire-Atlantique",
    "Loiret","Lot","Lot-et-Garonne","Lozère","Maine-et-Loire",
    "Manche","Marne","Haute-Marne","Mayenne","Meurthe-et-Moselle",
    "Meuse","Morbihan","Moselle","Nièvre","Nord",
    "Oise","Orne","Pas-de-Calais","Puy-de-Dôme","Pyrénées-Atlantiques",
    "Hautes-Pyrénées","Pyrénées-Orientales","Bas-Rhin","Haut-Rhin","Rhône",
    "Haute-Saône","Saône-et-Loire","Sarthe","Savoie","Haute-Savoie",
    "Paris","Seine-Maritime","Seine-et-Marne","Yvelines","Deux-Sèvres",
    "Somme","Tarn","Tarn-et-Garonne","Var","Vaucluse",
    "Vendée","Vienne","Haute-Vienne","Vosges","Yonne",
    "Territoire de Belfort","Essonne","Hauts-de-Seine","Seine-Saint-Denis",
    "Val-de-Marne","Val-d'Oise",
    "Guadeloupe","Martinique","Guyane","La Réunion","Mayotte"
  ),
  region = c(
    "Auvergne-Rhône-Alpes","Hauts-de-France","Auvergne-Rhône-Alpes",
    "Provence-Alpes-Côte d'Azur","Provence-Alpes-Côte d'Azur",
    "Provence-Alpes-Côte d'Azur","Auvergne-Rhône-Alpes","Grand Est",
    "Occitanie","Grand Est",
    "Occitanie","Occitanie","Provence-Alpes-Côte d'Azur","Normandie",
    "Auvergne-Rhône-Alpes",
    "Nouvelle-Aquitaine","Nouvelle-Aquitaine","Centre-Val de Loire",
    "Nouvelle-Aquitaine","Bourgogne-Franche-Comté",
    "Bretagne","Nouvelle-Aquitaine","Nouvelle-Aquitaine",
    "Bourgogne-Franche-Comté","Auvergne-Rhône-Alpes",
    "Normandie","Centre-Val de Loire","Bretagne","Corse","Corse",
    "Occitanie","Occitanie","Occitanie","Nouvelle-Aquitaine","Occitanie",
    "Bretagne","Centre-Val de Loire","Centre-Val de Loire",
    "Auvergne-Rhône-Alpes","Bourgogne-Franche-Comté",
    "Nouvelle-Aquitaine","Centre-Val de Loire","Auvergne-Rhône-Alpes",
    "Auvergne-Rhône-Alpes","Pays de la Loire",
    "Centre-Val de Loire","Occitanie","Nouvelle-Aquitaine","Occitanie",
    "Pays de la Loire",
    "Normandie","Grand Est","Grand Est","Pays de la Loire","Grand Est",
    "Grand Est","Bretagne","Grand Est","Bourgogne-Franche-Comté",
    "Hauts-de-France",
    "Hauts-de-France","Normandie","Hauts-de-France",
    "Auvergne-Rhône-Alpes","Nouvelle-Aquitaine",
    "Occitanie","Occitanie","Grand Est","Grand Est","Auvergne-Rhône-Alpes",
    "Bourgogne-Franche-Comté","Bourgogne-Franche-Comté","Pays de la Loire",
    "Auvergne-Rhône-Alpes","Auvergne-Rhône-Alpes",
    "Île-de-France","Normandie","Île-de-France","Île-de-France",
    "Nouvelle-Aquitaine",
    "Hauts-de-France","Occitanie","Occitanie",
    "Provence-Alpes-Côte d'Azur","Provence-Alpes-Côte d'Azur",
    "Pays de la Loire","Nouvelle-Aquitaine","Nouvelle-Aquitaine",
    "Grand Est","Bourgogne-Franche-Comté",
    "Bourgogne-Franche-Comté","Île-de-France","Île-de-France",
    "Île-de-France","Île-de-France","Île-de-France",
    "Outre-mer","Outre-mer","Outre-mer","Outre-mer","Outre-mer"
  )
)

# Normalisation du code département avant jointure
accidents <- accidents |>
  mutate(
    dep_join = as.character(dep),
    dep_join = str_trim(dep_join),
    # Corse : 201/200 → 2A, 202/206 → 2B
    dep_join = case_when(
      dep_join %in% c("201","200") ~ "2A",
      dep_join %in% c("202","206") ~ "2B",
      dep_join == "20"             ~ "2B",
      nchar(dep_join) == 1        ~ paste0("0", dep_join),
      TRUE                        ~ dep_join
    )
  ) |>
  left_join(.REF_GEO, by = c("dep_join" = "code_dep")) |>
  mutate(
    departement = coalesce(departement, paste0("Département ", dep)),
    region      = coalesce(region, "Non renseigné")
  ) |>
  select(-dep_join)

n_sans_region <- sum(accidents$region == "Non renseigné")
log_msg(sprintf("Région : %d accidents sans région (%.1f%%)",
                n_sans_region, n_sans_region / nrow(accidents) * 100))
log_msg(sprintf("Régions distinctes : %d", n_distinct(accidents$region)))


# ------------------------------------------------------------------------------
# 4. FILTRAGE FINAL
# ------------------------------------------------------------------------------
# On ne filtre que ce qui rendrait une ligne inutilisable dans Shiny

log_msg("Filtrage final...")
n_avant <- nrow(accidents)

accidents <- accidents |>
  filter(
    !is.na(annee) & annee >= 2015 & annee <= 2030,
    !is.na(dep)   & nchar(as.character(dep)) >= 2,
    !is.na(gravite_accident)
  )

log_msg(sprintf("Filtrage : %d → %d accidents (-%d supprimés)",
                n_avant, nrow(accidents), n_avant - nrow(accidents)))


# ------------------------------------------------------------------------------
# 5. SAUVEGARDE INTERMÉDIAIRE
# ------------------------------------------------------------------------------

saveRDS(accidents, file.path(CHEMIN_OUTPUT, paste0("04_accidents_", ANNEE_CIBLE, ".rds")))

log_msg("=== 04_enrichissement.R — terminé ===")
