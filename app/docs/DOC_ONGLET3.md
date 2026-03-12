# CrashAlert v4.0 — Documentation Onglet 3 : Profil accidentologique

## Objectif
Analyse du profil des personnes impliquées dans les accidents : genre, âge,
rôle, véhicule, équipement de sécurité. Permet d'identifier les populations
à risque et les facteurs comportementaux.
Public cible : assureurs (segmentation client), prévention routière, pouvoirs publics.

---

## Architecture technique
- **Module** : `app/view/profile.R` (UI + Server)
- **Données** : `app_data$accidents` — calculs dynamiques sur 564 198 lignes
- **Filtres** : Années, Région, Gravité — indépendants des autres onglets

---

## Composants UI

### Topbar filtres
- **Années** : multi-select, vide = 2015-2024
- **Région** : multi-select sur 13 régions métropolitaines + outre-mer
- **Gravité** : Mortel / Grave / Léger (filtre le dataset entier)
- **Compteur** : nb accidents correspondant aux filtres (droite)

### KPI (4 value_box)
| KPI | Calcul |
|-----|--------|
| Conducteurs | % accidents où catu_principal == "Conducteur" |
| Hommes | % accidents où sexe_dominant == "Homme" |
| 18-34 ans | % accidents tranche_age %in% c("18-24 ans","25-34 ans") |
| 75+ ans mortalité | Taux mortalité de la tranche 75+ ans |

### Section Genre & Age (3 graphiques côte à côte)

**1. Sexe & gravité (volume empilé)**
- Barres 100% empilées : Femme vs Homme
- 3 niveaux : Léger (vert) / Grave (orange) / Mortel (rouge)
- Montre la répartition relative de gravité par sexe

**2. Taux mortalité Homme vs Femme**
- Deux courbes superposées par tranche d'âge
- Bleu navy = Homme, Rouge = Femme
- Révèle l'écart H/F qui se creuse avec l'âge

**3. Accidents par tranche d'âge**
- Barres empilées absolues (volume)
- Ordre fixe : 0-17 → 75+ ans
- Montre le pic 18-34 ans clairement

### Taux mortalité par âge + Rôle
**Taux mortalité par âge** : courbe continue, points colorés vert→rouge selon valeur
**Rôle dans l'accident** : donut — Conducteur / Passager / Piéton
- "Piéton en roller ou trottinette" fusionné avec "Piéton" (trop faible volume)

### Section Véhicule & Sécurité

**Véhicule x gravité**
- Barres horizontales 100% empilées, triées par taux mortalité décroissant
- Tracteur agricole et "Autre" en tête (milieu rural)
- Cyclomoteur et véhicule utilitaire : faible mortalité mais fort volume grave

**Équipement sécurité**
- Top 10 équipements par volume
- Coloré par taux mortalité (vert→rouge)
- Révèle que "Ceinture non portée" a un taux mortalité 3x supérieur à "portée"

### Insights clés
Box dynamique calculée à chaque changement de filtre :
- % conducteurs masculins
- Taux mortalité H vs F + ratio
- % 18-34 ans
- Tranche d'âge la plus mortelle
- Véhicule le plus mortel (parmi ceux avec n≥100)

---

## Palette couleurs gravité
```
Léger  : #27ae60 (vert)
Grave  : #f39c12 (orange)
Mortel : #e74c3c (rouge)
```
Constante `.COULEURS` définie en haut du module pour cohérence.

---

## Ordre des tranches d'âge
```r
.ORDRE_AGE <- c("0-17 ans","18-24 ans","25-34 ans","35-44 ans",
                "45-54 ans","55-64 ans","65-74 ans","75+ ans")
```
Appliqué via `categoryorder="array"` + `categoryarray=.ORDRE_AGE` dans plotly
pour éviter le tri alphabétique ou les warnings "discrete/non-discrete".

---

## Bugs corrigés

| Bug | Cause | Correction |
|-----|-------|------------|
| Léger absent des graphiques | Valeur "Léger" avec accent non incluse | Ajout explicite avec `\u00e9` |
| Warnings "discrete & non-discrete" | Mix factor/character sur axe X | `categoryorder` + `categoryarray` dans layout |
| Warning "Ignoring 1 observations" | "Piéton en roller ou trottinette" hors palette | Fusion avec "Piéton" via `grepl` |
| `p()` introuvable | Non importé dans box::use | Ajout dans imports shiny |

---

## Insights clés (données 2015-2024 toutes années)
- **59%** des accidents impliquent un conducteur masculin
- Les hommes ont un taux de mortalité ~1.8x supérieur aux femmes
- Les **18-34 ans** représentent ~50% des accidents
- Les **75+ ans** ont le taux de mortalité le plus élevé (~18%)
- Le **tracteur agricole** est le véhicule le plus mortel (~17%)
- La **ceinture non portée** multiplie le taux mortalité par 3+

---

## TODO / Idées futures
- [ ] Ajouter évolution temporelle du profil (ex: part des jeunes 2015→2024)
- [ ] Croiser âge × véhicule × gravité (treemap ou sunburst)
- [ ] Ajouter filtre département (actuellement seulement région)
- [ ] Export du profil en PDF one-pager

---

*Dernière mise à jour : 09/03/2026 — Session rhino migration*
