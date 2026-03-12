# CrashAlert v4.0 — Documentation Onglet 5 : Facteurs de risque

## Objectif
Analyse des facteurs environnementaux et infrastructurels qui influencent
la gravité des accidents : météo, luminosité, état de la chaussée, type de
route, type de collision, et leurs combinaisons.
Public cible : assureurs (tarification contextuelle), collectivités
(investissements sécurité), forces de l'ordre (déploiement ciblé).

---

## Architecture technique
- **Module** : `app/view/facteurs.R` (UI + Server)
- **Données** : `app_data$accidents` — calculs dynamiques
- **Colonnes utilisées** : `atm_label`, `lum_label`, `surf_label`,
  `catr_label`, `col_label`, `gravite_accident`, `annee`, `region`
- **Filtres** : Années, Région, Type de route

---

## Helper `.taux_var()`

Fonction interne réutilisable calculant taux mortalité + volume par modalité :

```r
.taux_var <- function(d, col, min_n = 200) {
  d |>
    filter(!is.na(.data[[col]]), as.character(.data[[col]]) != "Non renseigné") |>
    mutate(val = as.character(.data[[col]])) |>
    group_by(val) |>
    summarise(n=n(), mortels=sum(gravite_accident=="Mortel"), taux=mortels/n*100) |>
    filter(n >= min_n) |>
    arrange(desc(taux))
}
```

Utilisée pour KPI, graphiques barres, et insights.

---

## Composants UI

### KPI (4 value_box)
| KPI | Variable | Thème |
|-----|----------|-------|
| Météo la + meurtrière | `atm_label` | danger |
| Route la + meurtrière | `catr_label` | danger |
| Luminosité la + risquée | `lum_label` | warning |
| Collision la + meurtrière | `col_label` | warning |

### Section Conditions environnementales (3 graphiques)

**Météo — Taux mortalité**
- Barres horizontales colorées vert→rouge, triées par taux décroissant
- Brouillard-fumée systématiquement en tête (~13%)
- Pluie légère paradoxalement faible (conducteurs prudents)

**Luminosité — Taux mortalité**
- Nuit sans éclairage >> Nuit avec éclairage >> Plein jour
- Éclairage public réduit le risque de ~50%

**État chaussée — Taux mortalité**
- Verglacée > Enneigée > Mouillée > Normale
- Corps gras/huile : rare mais très meurtrier

### Section Infrastructure & Collision

**Type de route × gravité (barres 100% empilées)**
- Triées par taux mortalité décroissant
- Route départementale : plus meurtrière que nationale (vitesses élevées, pas de séparateur)
- Autoroute : faible mortalité malgré vitesses (conception sécurisée)
- Voie communale : mortalité faible (zones urbaines, vitesse limitée)

**Type de collision × gravité (barres 100% empilées)**
- Frontale : collision la plus meurtrière (~11-12%)
- Par l'arrière : la moins meurtrière (même sens)
- Sans collision : chutes/sorties de route, taux intermédiaire

### Section Combinaisons à haut risque

**Nuit × État chaussée — Taux mortalité**
- Barres groupées : Jour (orange) vs Nuit (navy)
- Verglas de nuit = combo le plus dangereux
- Nuit multiplie le risque sur toutes les surfaces

**Type route × Météo — Taux mortalité**
- Courbes multi-séries : une ligne par condition météo
- Révèle les interactions : pluie forte sur route départementale = pic
- Temps éblouissant dangereux sur autoroute (vitesse + soleil rasant)

### Insights clés
Box dynamique recalculée à chaque filtre :
- Condition météo la plus meurtrière + taux
- Comparaison nuit sans éclairage vs plein jour
- Chaussée la plus meurtrière
- Route la plus mortelle + comparaison nationale vs communale
- Collision la plus meurtrière

---

## Faits clés (données 2015-2024)

| Facteur | Modalité la + meurtrière | Taux mortalité |
|---------|--------------------------|----------------|
| Météo | Brouillard - fumée | ~13-14% |
| Luminosité | Nuit sans éclairage | ~13-14% |
| Chaussée | Verglacée | ~9% |
| Type route | Route départementale | ~10% |
| Collision | Frontale | ~11-12% |

**Facteur multiplicateur nuit vs jour** : ~1.5x à 2x selon les conditions

---

## Décisions de design

### Pourquoi filtre "Type route" plutôt que "Gravité" ?
- La gravité est la variable dépendante analysée — la filtrer biaiserait les taux
- Le type de route est un facteur contextuel pertinent pour segmenter l'analyse

### Pourquoi min_n = 200 dans `.taux_var()` ?
- Évite les taux extrêmes sur petits échantillons (ex: 2 accidents = 50%)
- Abaissé à 100 pour certains graphiques où les modalités rares sont pertinentes

### Raccourcissement labels
- `"avec éclairage public "` → `"écl. "` dans luminosité
- `"Deux véhicules - "` → `"2 veh. - "` dans collision
- Nécessaire pour lisibilité des axes

---

## Bugs corrigés

Aucun bug à la première exécution — onglet fonctionnel du premier coup.

---

## TODO / Idées futures
- [ ] Ajouter `infra` (infrastructure : carrefour, tunnel, voie ferrée...)
- [ ] Ajouter `vma` (vitesse maximale autorisée) × gravité
- [ ] Carte de chaleur météo × luminosité (matrice 2D taux mortalité)
- [ ] Comparaison rural vs urbain (proxy via type route)
- [ ] Export tableau des combinaisons à haut risque en CSV

---

*Dernière mise à jour : 09/03/2026 — Session rhino migration*
