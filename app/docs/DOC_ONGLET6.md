# CrashAlert v4.0 — Documentation Onglet 6 : Analyse Avancée

## Objectif
Onglet signature data scientist : segmentation territoriale, score de risque
composite et analyse multivariée des profils d'accidents. Dépasse les analyses
descriptives des onglets précédents pour révéler des structures dans les données.

Public cible : chercheurs, assureurs (tarification territoriale), décideurs
publics (allocation budgétaire sécurité routière), journalistes de données.

---

## Architecture technique
- **Module** : `app/view/analyse.R` (UI + Server)
- **Données** : `app_data$accidents` — calculs au chargement (reactive cachée)
- **Packages** : `FactoMineR` (ACM), `cluster` (silhouette), `stats` (kmeans),
  `tibble` (column_to_rownames)
- **Périmètre** : France métropolitaine uniquement (96 départements, DOM-TOM exclus)
- **Filtrage DOM-TOM** : codes exclus = 971–978, 984–989 (liste exhaustive dans `.DOMTOM`)
- **Pas de filtres** : analyse globale 2015-2024 — robustesse statistique maximale

---

## Fonctions internes

### `.prepare_features(accidents)`
Calcule la matrice départementale de 7 indicateurs :

| Variable | Calcul | Rôle | R² avec mortalité |
|----------|--------|------|-------------------|
| `taux_mortalite` | % accidents mortels | Gravité | variable cible |
| `taux_nuit` | % accidents de nuit | Dangerosité temporelle | 1.6% |
| `taux_route_dept` | % sur route départementale | Contexte infrastructure | **26.4%** |
| `taux_jeunes` | % conducteurs 18-34 ans | Profil conducteur | **25.3%** |
| `taux_sans_collision` | % accidents sans tiers | Isolement routier | **19.5%** |
| `taux_weekend` | % samedi-dimanche | Rythme hebdomadaire | non testé |
| `nb_accidents` | Volume total | Poids statistique | non testé |

**Météo supprimée** : `taux_mauvais_tps` avait un R²=0.3% avec la mortalité
départementale, non discriminant. Remplacée par `taux_sans_collision` (accidents
sans tiers, terminologie ONISR), qui mesure l'isolement routier et les sorties
de route. R²=19.5%.

Nettoyage codes département identique à `risk_map.R` (2A/2B, zéro-padding).
Seuil minimum : 100 accidents par département.

### `.run_clustering(features)`
1. Normalisation `base::scale()` des 6 variables (hors `nb_accidents`)
2. K-means k=4, nstart=25, seed=42
3. Calcul score de risque composite 0-100
4. Attribution classe de risque (Faible / Modéré / Élevé / Très élevé)

**Note** : utiliser `base::scale()` explicitement. `stats::scale()` n'est pas
exporté dans l'environnement `box::use`.

---

## Section 1 — Score de risque composite

### Formule (v2, fondée sur les corrélations empiriques)
```
Score = taux_mortalite      / max * 40   # résultat principal
      + taux_route_dept     / max * 25   # R²=26%, infrastructure
      + taux_sans_collision / max * 20   # R²=20%, isolement routier
      + (100 - taux_jeunes) / 100 * 10  # R²=25%, profil senior
      + taux_nuit           / max *  5  # accessibilité secours
```

### Justification des pondérations
Les pondérations reflètent les corrélations empiriques mesurées sur les
96 départements métropolitains :

| Variable | Corrélation | R² | Poids score |
|----------|-------------|-----|-------------|
| taux_route_dept | +0.514 | 26.4% | 25% |
| taux_jeunes | -0.503 | 25.3% | 10% (signe inversé) |
| taux_sans_collision | +0.441 | 19.5% | 20% |
| taux_nuit | +0.125 | 1.6% | 5% |
| taux_mauvais_tps | +0.059 | 0.3% | supprimé |

**Limite connue** : absence de données de trafic (véhicule-km). Un département
rural avec peu de circulation peut afficher un taux de mortalité élevé
mécaniquement. L'ONISR utilise les distances parcourues pour neutraliser cet
effet — ces données ne sont pas disponibles dans la base BAAC.

### Classes de risque
| Classe | Score | Couleur carte |
|--------|-------|--------------|
| Très élevé | ≥ 65 | Rouge foncé |
| Élevé | 45-64 | Rouge clair |
| Modéré | 30-44 | Jaune-orange |
| Faible | < 30 | Vert |

### Résultats (2015-2024, score v2)
Les scores sont à lire comme des ordres de grandeur relatifs, pas des
indicateurs absolus — ils varient selon la fenêtre temporelle analysée.

Régions surreprésentées dans le Top 15 : Grand Est (Meuse en tête),
Nouvelle-Aquitaine (Dordogne, Landes), Bourgogne-Franche-Comté
(Saône-et-Loire, Jura), Massif Central.

Paris (75) affiche le score le plus faible de la base.

---

## Section 2 — Clustering k-means

### Validation du nombre de clusters
Scores silhouette testés de k=2 à k=8 :
- k=2 : 0.34 (optimal mathématiquement)
- **k=4 retenu** : richesse interprétative supérieure, profils opérationnels pertinents

### Les 3 clusters principaux (après correction DOM-TOM)

Les codes 978, 987, 988 n'étaient pas filtrés dans la version précédente et
formaient un faux cluster de 3 observations (mortalité 16.5%, nuit 39.7%).
Corrigé : liste `.DOMTOM` étendue à tous les codes outre-mer.

| Cluster | Nom | N | Mortalité moy. | Nuit moy. | Route dept% |
|---------|-----|---|----------------|-----------|-------------|
| 1 | Rural à haut risque | 40 | 11.6% | 23.0% | 62.7% |
| 3 | Rural intermédiaire | 41 | 7.7% | 24.8% | 45.3% |
| 4 | Urbain métropolitain | 15 | 3.8% | 27.5% | 27.8% |

**Rural à haut risque** (40 depts, France rurale profonde) : le profil le plus
préoccupant. Mortalité 11.6%, route départementale 62.7%. Couvre Massif Central,
Sud-Ouest, Est.

**Rural intermédiaire** (41 depts) : mortalité 7.7%, mélange de départements
périurbains et ruraux moins isolés. Couvre la majorité de la France de province.

**Urbain métropolitain** (15 depts : Paris, IDF, Lyon, Bordeaux, Strasbourg...) :
mortalité 3.8%, le plus bas. Paris (75) affiche 0.7%, soit 16 fois moins que
les départements ruraux les plus exposés.

**Note** : le score composite et le clustering captent des dimensions
complémentaires. Le score mesure un risque cumulé pondéré ; le clustering mesure
la similarité multidimensionnelle. Ces deux lectures se complètent.

### Visualisations
- **Carte choroplèthe** colorée par cluster (3 couleurs distinctes)
- **Radar chart** : profil moyen sur 6 axes par cluster. L'axe `Sans collision`
  remplace `Mauvais temps` par rapport à la version précédente.
- **Tableau interactif** : colonnes Cluster, Département, Région, Score, Mort.%,
  Nuit%, Route dept%, Sans collision%, Nb acc.

---

## Section 3 — ACM (Analyse des Correspondances Multiples)

### Paramètres
- **Échantillon** : 20 000 accidents stratifiés (seed=123)
- **Variables actives** : tranche_age, sexe, categorie_vehicule,
  meteo (binaire : Normale/Perturbée), lumin (3 modalités), route (5 modalités)
- **Dimensions retenues** : 2 (biplot) sur 8 calculées

### Variance expliquée
- Dim 1 : 6.9%
- Dim 2 : 5.9%
- Total Dim1+Dim2 : 12.8% — normal en ACM (variance diffuse sur nombreuses dimensions)

### Coordonnées et contributions (issues du calcul, seed=123)

| Modalité | Dim1 | Dim2 | Contrib Dim1 | Contrib Dim2 |
|----------|------|------|--------------|--------------|
| Tracteur agricole | 2.34 | 0.54 | 11.4% | 0.7% |
| Nuit sans éclairage | 1.73 | -0.92 | 18.5% | 6.1% |
| Autoroute | 1.56 | 0.22 | 18.1% | 0.4% |
| Nationale | 0.93 | 0.12 | 4.1% | 0.1% |
| Poids lourd | 0.93 | 1.16 | 2.4% | 4.4% |
| Cyclomoteur | -1.22 | -1.19 | 5.8% | 6.4% |
| Vélo | -1.43 | 1.85 | 2.6% | 5.1% |
| Urbaine | -0.62 | 0.01 | 12.3% | 0.0% |
| 0-17 ans | -0.95 | -0.12 | 8.1% | 0.2% |
| 18-24 ans | 0.02 | -0.81 | 0.0% | 13.5% |
| Nuit avec éclairage | -0.29 | -1.07 | 1.3% | 20.5% |
| Perturbée | 0.32 | -0.81 | 1.4% | 10.4% |
| Jour | -0.13 | 0.47 | 0.8% | 12.2% |
| Véhicule léger | -0.10 | -0.14 | 0.4% | 1.0% |

### Interprétation des axes

**Axe 1** est structuré par Nuit sans éclairage (18.5%), Autoroute (18.1%),
Urbaine (12.3%), Tracteur agricole (11.4%), 0-17 ans (8.1%).
Opposition : milieu rural rapide (droite) vs milieu urbain (gauche).

**Axe 2** est structuré par Nuit avec éclairage (20.5%), 18-24 ans (13.5%),
Jour (12.1%), Perturbée (10.4%).
Opposition : nuit périurbaine/jeunes (bas) vs jour/seniors (haut).

### Points notables et corrections d'interprétation

**Tracteur agricole** (Dim1=2.34) : modalité la plus atypique sur l'axe 1.
Il partage avec Nuit sans éclairage (Dim1=1.73) une position à droite sur cet
axe, mais ils sont opposés sur l'axe 2 (Tracteur : +0.54, Nuit sans éclairage :
-0.92). Leur association n'est que partielle. L'ACM ne renseigne pas sur la
gravité — pour la mortalité par type de véhicule, voir l'onglet Facteurs de risque.

**Vélo et Cyclomoteur** sont opposés sur l'axe 2 (Vélo : +1.85, Cyclomoteur :
-1.19). Le vélo est associé aux seniors et au jour ; le cyclomoteur aux jeunes
et à la nuit. Ils ne forment pas un groupe commun.

**Véhicule léger, Route départementale, Homme** : proches du centre, profil
de l'accident le plus fréquent.

**Autoroute et Nationale** ne sont pas regroupés avec Tracteur agricole malgré
leur position à droite : leurs coordonnées sur Dim2 diffèrent significativement
(Autoroute : +0.22, Tracteur : +0.54, Nuit sans éclairage : -0.92).

**Principe général** : l'ACM décrit des associations entre profils de contexte,
pas des niveaux de gravité. Les interprétations sur la mortalité sont à croiser
avec les onglets Profil accidentologique et Facteurs de risque.

---

## Décisions de design

### Pourquoi pas de filtres sur cet onglet ?
L'analyse avancée requiert un maximum de données pour la robustesse statistique.
Filtrer par année ou région fragmenterait les clusters et rendrait l'ACM
non significative. L'onglet est conçu comme une vue macro permanente.

### Pourquoi k=4 et pas k=2 ?
k=2 sépare uniquement urbain/rural — trop grossier pour un usage opérationnel.
k=4 produit 3 profils interprétables (après exclusion du cluster DOM-TOM résiduel).
k=2 reste optimal mathématiquement (silhouette=0.34) mais insuffisant pour l'analyse.

### Pourquoi ACM et pas ACP ?
Les variables sont toutes catégorielles. L'ACP est réservée aux variables continues.

### Pourquoi supprimer la météo du score ?
R²=0.3% avec la mortalité départementale, non discriminant empiriquement.
L'ONISR note que les conditions météo défavorables peuvent réduire la vitesse
et ne sont pas systématiquement les plus accidentogènes. Les accidents par beau
temps sont fréquents. La météo agit davantage sur la fréquence que sur la gravité
à l'échelle départementale.

---

## Bugs corrigés (session 10/03/2026)

| Bug | Cause | Correction |
|-----|-------|------------|
| DOM-TOM dans cluster 2 | 978, 987, 988 absents de `.DOMTOM` | Liste `.DOMTOM` étendue (984–989) |
| `taux_mauvais_tps` introuvable au runtime | Variable supprimée de `.prepare_features` mais encore dans le radar et le tableau | Remplacé par `taux_sans_collision` dans tous les outputs |
| `styleInterval` erreur | Cuts non triés croissants | Remplacé par `styleEqual` |
| `styleEqual` introuvable | Absent des imports `box::use DT[...]` | Ajouté aux imports DT |

---

## TODO / Idées futures
- [ ] Clustering hiérarchique (dendrogramme) en complément du k-means
- [ ] Évolution temporelle des clusters (le cluster Rural à haut risque se réduit-il ?)
- [ ] Intégration données socio-économiques (PIB/hab, densité médicale)
- [ ] Export rapport PDF par département
- [ ] Score de risque normalisé par population (accidents/100k habitants)
- [ ] Insights statiques pré-calculés au chargement (alternative sans API)

---

*Dernière mise à jour : 10/03/2026 — Sections 4 et 5 ajoutées (évolution temporelle + corrélation score × évolution)*

---

## Section 4 — Évolution temporelle (ajout session 10/03/2026)

### Objectif
Visualiser l'évolution du taux de mortalité annuel par département sur 2015-2024,
détecter les tendances significatives et les comparer à la moyenne nationale.

### Données
- `evol_data` : taux mortalité annuel par département, seuil ≥ 50 accidents/an
- `evol_france` : taux national métropolitain par année
- `tendances` : pente + R² par département (régression linéaire 2015-2024,
  filtre ≥ 8 années disponibles)

### Tendance nationale
Non linéaire (R²=0.15) : baisse 2015-2021, remontée 2022-2024.
Valeurs : 5.58% (2015) → 5.18% (2021) → 5.90% (2024).
**2020 = année COVID** : baisse mécanique du trafic, non comparable.

### Seuil de significativité des tendances
R² ≥ 0.4 requis pour qualifier une tendance. En dessous : "évolution irrégulière".
Seuil pente : |pente| ≥ 0.3 pt/an pour distinguer amélioration/dégradation/stable.

### 4 catégories de tendance

| Catégorie | Condition | Couleur badge |
|-----------|-----------|--------------|
| Amélioration | R²≥0.4 et pente ≤ -0.3 | Vert |
| Dégradation | R²≥0.4 et pente ≥ +0.3 | Rouge |
| Stable | R²≥0.4 et -0.3 < pente < +0.3 | Jaune |
| Instable | R²<0.4 | Gris |

### Exemples notables (calculés sur 2015-2024)
- Tarn (81) : -1.06pt/an, R²=0.752 → Amélioration fiable (21% → 11%)
- Alpes-de-Haute-Provence (04) : -1.02pt/an, R²=0.774 → Amélioration fiable
- Vienne (86) : +0.86pt/an, R²=0.721 → Dégradation fiable (6% → 11%)
- Dordogne (24) : R²=0.013 → Instable malgré score composite élevé

### Composants UI
- `selectInput` dept_evol : sélection 3 départements max
- `checkboxInput` show_france : ligne France pointillée gris
- `checkboxInput` show_tendance : lignes de régression par département
- `output$badges_tendance` : badges latéraux avec pente, R², évolution
- `output$evol_temporelle` : graphique plotly, annotation COVID 2020
- `output$evol_resultat_box` : boîte verte dynamique, mise à jour à chaque sélection

### Limite documentée
Absence de données de trafic (véhicule-km) : impossible de distinguer
une baisse d'accidents d'une baisse du trafic.

---

## Section 5 — Corrélation score × évolution temporelle (ajout session 10/03/2026)

### Objectif
Mettre en relation le score composite de risque (valeur statique sur 2015-2024)
avec la trajectoire d'évolution du taux de mortalité sur la même période.
Répondre à la question : les départements les plus risqués s'améliorent-ils davantage ?

### Données
- `corr_data` reactive : jointure `features()` × `tendances()` × amplitude evol_data
  - `score_risque` : score composite 0-100 (calculé sur toute la période)
  - `pente` : coefficient de régression linéaire taux_mort ~ annee (2015-2024)
  - `r2` : R² de cette régression
  - `mort_debut` / `mort_fin` : moyenne taux 2015-2016 / 2023-2024
  - `amplitude` : max(taux_mort) - min(taux_mort) sur 10 ans
  - `quartile` : ntile(score_risque, 4)

### Corrélations empiriques mesurées (96 depts métropolitains)

| Paire | R | Interprétation |
|-------|---|----------------|
| score × mort_debut | +0.866 | Le score capture bien le niveau de départ |
| score × mort_fin | +0.836 | Les forts scores restent risqués — pas de rattrapage général |
| score × amplitude | +0.756 | Les forts scores ont des courbes plus volatiles |
| mort_debut × pente | -0.576 | Les depts qui partaient haut ont davantage baissé |
| score × pente | -0.241 | Relation non linéaire, pas directe |
| score × R² | -0.025 | Aucun lien entre score et régularité de la trajectoire |

### Résultat central : relation non linéaire par quartile

| Quartile | Score moyen | Pente médiane | n_amelio | n_degrad | n_instable |
|----------|-------------|---------------|----------|----------|------------|
| Q1 | ~35 | +0.015 pt/an | 0 | 3 | 20 |
| Q2 | ~51 | -0.197 pt/an | 6 | 1 | 16 |
| Q3 | ~62 | -0.260 pt/an | 5 | 2 | 16 |
| Q4 | ~74 | -0.118 pt/an | 3 | 0 | 20 |

**Lecture :**
- Q1 (urbains, score faible) : stagnation/légère dégradation. 0 amélioration significative.
  Dégradations fiables : Haute-Garonne (+0.53, R²=0.85), Alpes-Maritimes (+0.36, R²=0.94), Loire (+0.42, R²=0.77)
- Q2-Q3 (péri-urbain/rural intermédiaire) : moteur de la baisse nationale
- Q4 (rural profond, score fort) : s'améliore quand la tendance est fiable (Tarn, AHP),
  mais 20/23 depts sont instables (petits volumes, évolution erratique)

### Outliers notables
- **Tarn (81)** : score=73.8, pente=-1.06, R²=0.752 — meilleure amélioration fiable
- **Alpes-de-Haute-Provence (04)** : score=67.6, pente=-1.02, R²=0.774
- **Sarthe (72)** : score~50 (Q2), pente~-1.0 — forte amélioration hors rural profond
- **Vienne (86)** : score=77.4, pente=+0.86, R²=0.721 — dégradation la plus fiable
- **Indre (36)** : pente=+0.83, R²=0.481 — 2e dégradation significative

### Interprétation
La relation n'est pas "plus risqué = plus d'amélioration". Elle est en U inversé :
- Les urbains (Q1) se dégradent : pression de trafic croissante, mortalité relative en hausse
- Les ruraux intermédiaires (Q2-Q3) s'améliorent le plus : masse critique d'accidents
  suffisante pour que les politiques de sécurité aient un effet mesurable
- Les ruraux profonds (Q4) sont instables : petits volumes = variance élevée,
  quelques accidents mortels de plus ou de moins font basculer le taux

### Limite documentée
L'effet de régression vers la moyenne est attendu statistiquement
(les depts qui partaient haut redescendent mécaniquement).
Il ne prouve pas l'efficacité de politiques de sécurité routière spécifiques.

### Composants UI
- `output$scatter_score_evol` : scatter plotly, couleur par tendance,
  taille par amplitude, courbe loess (span=0.6), ligne y=0, annotations outliers
- `output$corr_resultat_box` : boîte verte dynamique avec 3 corrélations,
  tableau par quartile, top améliorations et dégradations
