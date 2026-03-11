# CrashAlert — Analyse des accidents routiers en France (2015–2024)

![R](https://img.shields.io/badge/R-4.x-276DC3?style=flat-square&logo=r)
![Shiny](https://img.shields.io/badge/Shiny-rhino-blue?style=flat-square)
![Data](https://img.shields.io/badge/données-BAAC%202015--2024-green?style=flat-square)
![Lignes](https://img.shields.io/badge/accidents-564%20198-red?style=flat-square)
![Licence](https://img.shields.io/badge/licence-MIT-lightgrey?style=flat-square)

> Application interactive d'analyse des accidents corporels de la circulation en France,
> construite avec R/Shiny (architecture rhino + bslib) sur les données officielles BAAC.

🚀 **[Accéder à l'application](https://paguystream.shinyapps.io/CrashCirculation/)**

---

## Aperçu

CrashAlert permet d'explorer **564 198 accidents** survenus entre 2015 et 2024 à travers
7 modules analytiques interactifs. L'objectif est de rendre les données BAAC accessibles
et interprétables — sans nécessiter de compétences en data science.

---

## Fonctionnalités

| Module | Description |
|--------|-------------|
| **Tableau de bord** | KPIs nationaux, évolution annuelle, comparaison interdépartementale |
| **Carte de risque** | 564k accidents géolocalisés, score de risque par département, filtres dynamiques |
| **Profil accidentologique** | Âge, genre, mode de déplacement, taux mortalité H/F par tranche d'âge |
| **Temporalité** | Heatmap heure × jour, taux circulaires, saisonnalité |
| **Facteurs de risque** | Météo, type de route, équipements, clustering k-means |
| **Analyse avancée** | Scoring composite, modélisation multivariée |
| **Rapport régional** | Génération automatique de rapports HTML/PDF par région (rmarkdown) |

---

## Structure du projet

```
CrashAlert/
├── app/
│   ├── main.R                  # Point d'entrée principal
│   ├── report/
│   │   ├── rapport_region.Rmd  # Template rapport régional
│   │   └── rapport_style.css   # Styles du rapport
│   └── view/
│       ├── dashboard.R         # Module tableau de bord
│       ├── risk_map.R          # Module carte de risque
│       ├── profile.R           # Module profil accidentologique
│       ├── heatmap.R           # Module temporalité
│       ├── facteurs.R          # Module facteurs de risque
│       ├── analyse.R           # Module analyse avancée
│       └── about.R             # Module à propos
├── data_propre/
│   ├── accidents_final_light.rds   # Données consolidées (564 198 lignes)
│   └── geo/
│       └── departements.geojson    # Fonds de carte départements
├── app.R                       # Lanceur Shiny
├── rhino.yml                   # Configuration rhino
└── README.md
```

---

## Données

**Source :** [Base de données BAAC](https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-baac/)
publiée par l'ONISR sur data.gouv.fr — Licence Ouverte v2.0.

**Période :** 2015 – 2024  
**Volume :** 564 198 accidents après nettoyage et consolidation  
**Structure originale :** 4 fichiers par année (Caractéristiques, Lieux, Véhicules, Usagers)

### Colonnes clés

```
num_acc, an, annee, mois, jour, heure, moment_journee, saison, jour_semaine,
lat, long, dep, departement, region, gravite_accident, grav_label,
nb_tues, nb_blesses_hospitalises, nb_victimes,
tranche_age, age_moyen_reel, sexe_dominant, categorie_vehicule,
lum_label, atm_label, col_label, catr_label, surf_label,
type_route, vma, secu_principal, accident_mortel, accident_grave
```

---

## Stack technique

- **Framework :** R 4.x · Shiny · [rhino](https://appsilon.github.io/rhino/) · bslib
- **Visualisation :** Leaflet · Plotly · ggplot2
- **Données :** dplyr · tidyverse · sf
- **Rapports :** rmarkdown · kableExtra · tinytex (PDF)
- **Déploiement :** shinyapps.io

---

## Installation locale

### Prérequis

- R ≥ 4.1
- RStudio (recommandé)

### Étapes

```r
# 1. Cloner le repo
# git clone https://github.com/Paguy-Stream/CrashAlert.git

# 2. Installer les dépendances
install.packages(c(
  "shiny", "rhino", "bslib", "dplyr", "tidyverse",
  "leaflet", "plotly", "ggplot2", "sf",
  "rmarkdown", "kableExtra", "scales"
))

# Pour la génération PDF (optionnel)
install.packages("tinytex")
tinytex::install_tinytex()

# 3. Lancer l'application
shiny::runApp()
```

> **Note :** Le fichier `data_propre/accidents_final_light.rds` (données consolidées)
> doit être présent. En raison de sa taille, il n'est pas inclus dans le repo.
> Voir la section Données pour le reconstituer depuis les sources BAAC.

---

## Rapport régional

Le module de rapport génère automatiquement un document complet par région :

```r
rmarkdown::render(
  "app/report/rapport_region.Rmd",
  params = list(region = "Bretagne", accidents = accidents_df),
  output_format = "html_document"  # ou "pdf_document"
)
```

**Sections générées :** Synthèse KPIs · Évolution annuelle · Répartition gravité ·
Profil temporel · Comparatif départemental · Facteurs de risque · Profil accidentés · Conclusion

---

## Auteur

**Emmanuel Bouendo** — Université de Tours  
📧 [emmanuelpaguiel@gmail.com](mailto:emmanuelpaguiel@gmail.com)  
🔗 [Portfolio](https://paguy-stream.github.io) · [GitHub](https://github.com/Paguy-Stream)

---

## Licence

Ce projet est sous licence [MIT](LICENSE).  
Les données BAAC sont sous [Licence Ouverte v2.0](https://www.etalab.gouv.fr/licence-ouverte-open-licence) — © ONISR / data.gouv.fr.
