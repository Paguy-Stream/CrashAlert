# CrashAlert v4.0 — Documentation Onglet 2 : Carte des risques

## Objectif
Visualisation géographique des risques routiers par département français.
Permet d'identifier les zones à risque, comparer les départements et suivre
l'évolution année par année.
Public cible : compagnies d'assurance (tarification géographique), collectivités,
décideurs publics.

---

## Architecture technique
- **Module** : `app/view/risk_map.R` (UI + Server)
- **Données brutes** : `app_data$accidents` — agrégation dynamique par département
- **Géographie** : `data_propre/geo/departements.geojson` (96 départements métropole)
  Source : github.com/gregoiredavid/france-geojson (version simplifiée)
- **Rendu carte** : `sf` + `leaflet::addPolygons` (choroplèthe)

---

## Composants UI

### Topbar
- **Filtre années** : selectInput multiple, indépendant du filtre onglet 1
  - Vide = toutes les années 2015–2024
  - Sélection 1 année = active la comparaison vs année précédente
- **Boutons métriques** : Taux mortalité / Volume accidents / Nb décès
  - Changent la coloration de la carte ET le Top 5 ET les labels hover
- **Résumé** : nb accidents + années sélectionnées (droite topbar)

### Carte choroplèthe
- **Technologie** : `sf::st_read()` + `leaflet::addPolygons()`
- **Palette** : dégradé vert→jaune→orange→rouge (#d4edda → #dc3545)
- **Hover** : label département + valeur métrique
- **Highlight** : contour navy (#1b3a6b) au survol
- **Clic** : met à jour le panneau "Département sélectionné"
- **DOM-TOM exclus** de la carte (affichés dans tableau séparé)

### Panneau département sélectionné
- Nom, région, nb accidents, nb décès, taux mortalité, blessés graves
- **Variation vs année précédente** : affiché uniquement si 1 seule année
  sélectionnée — flèche rouge ↗ ou verte ↘ + % delta

### Top 5
- **Dynamique** : suit la métrique sélectionnée (taux / volume / décès)
- **Exclu** : DOM-TOM (faussent le classement métropole)
- Coloré rouge→jaune selon rang

### Graphique taux mortalité par région
- Barres horizontales colorées (vert→rouge selon valeur)
- Source : `dept_data()` filtré, groupé par région, moyenne des taux
- DOM-TOM exclus

### Tableau classement métropole
- Trié par taux mortalité décroissant
- Coloration conditionnelle : vert <5% | jaune 5-8% | orange 8-12% | rouge >12%
- Pagination 10 lignes, recherche, tri cliquable

### Tableau Outre-mer
- Même structure, territoires DOM-TOM uniquement

---

## Logique de nettoyage des codes département

Problème : les codes dep dans les données ont des formats incohérents.

```r
# Règle de nettoyage appliquée dans .compute_dept()
dep_clean = case_when(
  dep_raw == "201"    ~ "2A",   # Corse-du-Sud
  dep_raw == "202"    ~ "2B",   # Haute-Corse
  # codes 3 chiffres se terminant par 0 (ex: 100->10, 300->30)
  # SAUF DOM-TOM (971, 972...)
  nchar(dep_raw)==3 & grepl("0$",dep_raw) & !dep_raw %in% DOMTOM ~ sub("0$","",dep_raw),
  nchar(dep_raw)==1   ~ paste0("0", dep_raw),  # 1->01, 2->02...
  TRUE                ~ dep_raw
)
```

**Cas problématiques identifiés :**
- `gsub("0$","",dep)` naïf transformait `10`→`1`→`01` (8 départements affectés)
- Départements concernés : 10, 30, 40, 50, 60, 70, 80, 90
- Corrigé en utilisant `nchar(dep_raw)==3` comme condition préalable

---

## Performance

Calcul dynamique depuis 564k lignes :
- Filtre + group_by + summarise : ~0.3s (acceptable)
- Le GeoJSON est chargé une fois via `reactive()` (mis en cache)
- La jointure sf se fait côté R avant le rendu leaflet

---

## Décisions de design

### Pourquoi filtre année indépendant ?
- La carte des risques sert à des analyses géographiques spécifiques
- Une compagnie d'assurance veut comparer 2020 vs 2019 (COVID)
- Le filtre onglet 1 sert la vue d'ensemble, celui-ci sert l'analyse fine

### Pourquoi sf + addPolygons plutôt que addGeoJSON ?
- `addGeoJSON` ne supporte pas `fillColor` dynamique par feature
- `addPolygons` avec objet sf permet colorisation par colonne directement
- Plus performant et plus flexible pour les highlights

### Pourquoi exclure DOM-TOM de la carte ?
- Coordonnées hors bbox France métropolitaine
- Taux mortalité très élevés (Wallis-et-Futuna 19%) fausseraient l'échelle
- Affichés dans tableau dédié pour transparence

---

## Bugs corrigés

| Bug | Cause | Correction |
|-----|-------|------------|
| Carte entièrement noire | `addGeoJSON` sans support fillColor | Migration vers `sf` + `addPolygons` |
| 3 onglets dupliqués | gsub appliqué 3x sur main.R | Réécriture complète main.R |
| Labels N/A sur 8 depts | `gsub("0$","",dep)` trop agressif | Condition `nchar==3` préalable |
| `n()` introuvable | Manquant dans box::use dplyr | Ajout explicite |
| Top 5 toujours taux mortalité | Hardcodé | Rendu dynamique via `metric()` |

---

## TODO / Idées futures

- [ ] Bouton "comparer deux années" (carte différentielle)
- [ ] Export carte en PNG
- [ ] Filtre par classe de gravité sur cet onglet
- [ ] Carte par commune (données plus granulaires si disponibles)
- [ ] Indice de risque composite (accidents + mortalité + gravité)

---

*Dernière mise à jour : 09/03/2026 — Session rhino migration*
