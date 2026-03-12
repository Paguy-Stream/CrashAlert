# CrashAlert v4.0 — Documentation Onglet 1 : Tableau de bord principal

## Objectif
Donner une vue d'ensemble immédiate de l'accidentalité routière française 2015–2024.
Public cible : analystes, compagnies d'assurance, décideurs publics.

---

## Architecture technique
- **Framework** : rhino + bslib (Bootstrap 5)
- **Modules** : `app/view/dashboard.R` (UI + Server)
- **Données** : `app/logic/data.R` → `filter_accidents()`, `compute_kpis()`
- **Fichier données** : `data_propre/accidents_final_light.rds` (564 198 lignes)

---

## Composants UI

### Système de filtres (Offcanvas Bootstrap)
- **Déclencheur** : bouton "Filtres" en topbar
- **Panneau** : offcanvas latéral gauche, 320px
- **Filtres disponibles** : Années (2015–2024), Gravité (Mortel/Grave/Léger), Régions
- **État initial** : aucune sélection = toutes les données chargées
- **Tags actifs** : affichés dans la topbar pour visibilité immédiate
- **Boutons** : Appliquer (ferme le panneau) + Réinitialiser

### KPI Ligne 1 — Volume
| KPI | Colonne source | Couleur |
|-----|---------------|---------|
| Total accidents | `nrow(filtered())` | Navy (#1b3a6b) |
| Décès | `gravite_accident == "Mortel"` | Rouge (#e74c3c) |
| Blessés graves | `gravite_accident == "Grave"` | Orange (#f39c12) |
| Blessés légers | `gravite_accident == "Léger"` | Vert (#27ae60) |

### KPI Ligne 2 — Taux & Profil
| KPI | Calcul | Note |
|-----|--------|------|
| Taux mortalité | `mean(gravite == "Mortel") * 100` | % |
| Accidents de nuit | `moment_journee %in% c("Nuit (20h-0h)", "Nuit profonde (0h-6h)")` | % |
| Route mouillée | `surf_label %in% c("Mouillée","Flaques","Inondée")` | % |
| Âge moyen | `mean(age_moyen_reel)` | ans |

### Carte des accidents
- **Fonds de carte** : CartoDB.Positron (neutre, professionnel)
- **Marqueurs** : cercles colorés par gravité, clustering automatique (rayon 40px)
- **Échantillon** : max 5 000 points pour performance
- **Coordonnées filtrées** : lat [41–51], lon [-5–10], `coords_valides == TRUE`
- **Couleurs** : Mortel=#e74c3c, Grave=#f39c12, Léger=#27ae60
- **Popup** : date/heure, département, véhicule, collision, luminosité, météo, route, surface, victimes

### Répartition gravité (Donut)
- **Type** : pie avec hole=0.45
- **Dynamique** : oui, réagit aux filtres
- **Couleurs** : cohérentes avec carte et KPI

### Évolution annuelle (Lignes)
- **Séries** : Accidents totaux (navy), Mortels (rouge), Graves (orange)
- **Dynamique** : oui — agrégation depuis `filtered()` par `annee`
- **Note** : la baisse 2020 correspond au confinement COVID

### Véhicules impliqués (Barres)
- **Source** : `filtered()` groupé par `categorie_vehicule`
- **Exclus** : "Non renseigné" (~53% des lignes)
- **Annotation** : taux mortalité % affiché sur chaque barre
- **Top 8** catégories par volume

### Type de collision (Pie)
- **Source** : `filtered()` groupé par `col_label`
- **Exclus** : "Non renseigné"
- **Top 6** types
- **Palette** : dégradé navy

### Conditions météo (Barres horizontales)
- **Métrique** : taux de mortalité (%), pas le volume
- **Source** : `filtered()` groupé par `atm_label`
- **Exclus** : "Normale" (trop dominant, masque les insights)
- **Dynamique** : oui
- **Insight clé** : Vent fort > Brouillard > Pluie forte en mortalité

### Type de route (Barres superposées)
- **Séries** : Total (navy) + Mortels (rouge) en overlay
- **Source** : `filtered()` groupé par `type_route`
- **Insight clé** : Routes départementales = plus d'accidents mortels en absolu

### Tableau Top 15 départements
- **Source** : `app_data$agg_departement` (statique, pré-agrégé)
- **Correction doublons** : fusion codes dep `75`/`750`, `13`/`130` etc.
  via `gsub("0$", "", dep)` + re-agrégation
- **Coloration** : taux mortalité avec `styleInterval(c(5,8,12))`
  - Vert < 5% | Jaune 5–8% | Orange 8–12% | Rouge > 12%

---

## Décisions de design

### Pourquoi rhino + bslib plutôt que bs4Dash ?
- bs4Dash : erreurs `status="light"` non supporté, modules mal sourcés
- bslib Bootstrap 5 : plus moderne, mieux maintenu, meilleure compatibilité

### Pourquoi offcanvas plutôt que sidebar fixe ?
- Sidebar fixe : occupait 280px en permanence, masquait les graphiques
- Offcanvas : espace plein écran pour les visualisations, pattern UX moderne

### Palette de couleurs
```
Navy principal  : #1b3a6b
Navy secondaire : #4a6fa5
Rouge mortel    : #e74c3c
Orange grave    : #f39c12
Vert léger      : #27ae60
Gris neutre     : #6c757d
```

### Dynamisme des graphiques
- Tous les graphiques sont dynamiques (réagissent aux filtres)
- Performance validée : 0.19s sur 564k lignes pour une agrégation météo
- Seul le tableau Top 15 reste sur `agg_departement` pré-agrégé
  (car nécessite la colonne `classe_gravite` calculée en amont)

---

## Bugs corrigés

| Bug | Cause | Correction |
|-----|-------|------------|
| `status="light"` invalide | bs4Dash | Migration vers bslib |
| `filtersServer` introuvable | Source manquant server.R | Ajout source dans server.R |
| `objet 'adr' introuvable` | Column detection | Fonction `col_safe()` |
| Légende carte grise | `colorFactor` avec domain incorrect | `intersect()` + couleurs nommées |
| Facteurs → indices numériques popup | Facteurs R dans leaflet | `as.character()` avant popup |
| Doublons Top 15 (dep 75/750) | Codes dep avec/sans zéro | `gsub("0$","",dep)` + re-agrégation |
| `if_else` introuvable | dplyr dans contexte leaflet | Remplacé par `ifelse` base R |
| `reorder` introuvable | stats base R non importé | Pré-tri dans les données |

---

## TODO / Idées futures

- [ ] Textes d'insights dynamiques sous chaque graphique (via API Claude)
- [ ] Onglet 2 : Carte choroplèthe risques par département
- [ ] Onglet 3 : Profil accidentologie (véhicule, âge, sexe)
- [ ] Onglet 4 : Facteurs de risque détaillés
- [ ] Onglet 5 : Heatmap temporelle (heure × jour)
- [ ] Export PDF du rapport filtré
- [ ] Comparaison année N vs N-1

---

*Dernière mise à jour : 09/03/2026 — Session rhino migration*
