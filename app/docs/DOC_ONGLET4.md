# CrashAlert v4.0 — Documentation Onglet 4 : Temporalité

## Objectif
Analyse des patterns temporels des accidents routiers : à quelle heure, quel
jour, quel mois et quelle saison surviennent les accidents et les décès.
Permet d'identifier les créneaux à haut risque pour cibler les actions de
prévention et adapter la tarification assurance.
Public cible : forces de l'ordre (déploiement), assureurs (tarification horaire),
collectivités (signalisation nocturne), prévention routière.

---

## Architecture technique
- **Module** : `app/view/heatmap.R` (UI + Server)
- **Données** : `app_data$accidents` — calculs dynamiques
- **Colonnes utilisées** : `heure`, `jour_semaine`, `mois`, `saison`,
  `moment_journee`, `gravite_accident`, `annee`, `region`
- **Filtres** : Années, Région, Gravité — indépendants des autres onglets

---

## Format des colonnes temporelles

| Colonne | Type | Format | Exemple |
|---------|------|---------|---------|
| `heure` | character | "00"-"23" | "17" |
| `jour_semaine` | character | nom complet minuscule | "vendredi" |
| `mois` | character | "01"-"12" (zéro-padded) | "06" |
| `saison` | character | nom capitalisé | "Été" |
| `moment_journee` | character | libellé complet | "Soirée (17h-20h)" |

**Transformation dans `filtered()`** :
```r
mutate(
  heure_num = as.integer(heure),    # 0-23
  jour      = as.character(jour_semaine),
  mois_num  = as.integer(mois),     # 1-12
  weekend   = jour %in% c("samedi","dimanche")
)
```

---

## Composants UI

### KPI (4 value_box)
| KPI | Calcul | Thème |
|-----|--------|-------|
| Heure la + mortelle | argmax taux mortalité par heure | danger (rouge) |
| Jour le + accidenté | argmax volume par jour | warning (jaune) |
| Mois le + dangereux | argmax volume par mois | warning (jaune) |
| Saison la + meurtrière | argmax taux mortalité par saison | primary (navy) |

### Section Heatmap Heure × Jour

**Heatmap principale (heure × jour de la semaine)**
- Matrice 24 × 7 construite manuellement via boucle
- Axe Y : heures 0h-23h (inversé — 0h en haut)
- Axe X : Lun → Dim
- Palette : blanc→jaune→orange→rouge
- Révèle clairement les pics : vendredi 17h-18h, samedi nuit

**Taux mortalité par moment de journée**
- Barres horizontales colorées vert→rouge
- Ordre fixe : Nuit profonde → Nuit (via `.MOMENTS_ORDER`)
- Révèle que la nuit profonde (0h-6h) est 2-3x plus meurtrière

### Section Saisonnalité

**Heatmap mois × jour de la semaine**
- Matrice 12 × 7
- Révèle les pics estivaux (juin-juillet samedi)
- Février = mois le plus bas (moins de jours)

**Accidents par saison**
- Barres colorées par saison (rouge=automne, orange=été, bleu=hiver, vert=printemps)
- Affiche nb accidents + taux mortalité en annotation

### Section Profil horaire

**Accidents par heure — semaine vs weekend**
- Deux courbes superposées (navy=semaine, rouge=weekend)
- Semaine : double pic 8h et 17h (trajets domicile-travail)
- Weekend : pic décalé 14h-18h, volume global inférieur

**Taux mortalité par heure**
- Barres colorées vert→rouge
- Pic mortalité 1h-3h (nuit profonde, alcool)
- Creux 7h-9h (malgré fort volume, mortalité faible)

### Insights clés
Box dynamique calculée à chaque changement de filtre :
- Heure la plus meurtrière + taux
- Heure pic accidents + volume
- Jour le plus accidenté
- Ratio nuit/jour mortalité
- Comparaison weekend vs semaine taux mortalité

---

## Constantes de tri

```r
.JOURS_ORDER   <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
.JOURS_LABEL   <- c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim")
.MOIS_LABEL    <- c("Jan","Fév","Mar","Avr","Mai","Jun","Jul","Aoû","Sep","Oct","Nov","Déc")
.MOMENTS_ORDER <- c("Nuit profonde (0h-6h)","Matin (6h-9h)","Matinée (9h-12h)",
                    "Midi (12h-14h)","Après-midi (14h-17h)","Soirée (17h-20h)","Nuit (20h-0h)")
```

---

## Faits clés (données 2015-2024 toutes régions)

- **2h00** est l'heure la plus meurtrière (~22% taux mortalité)
- **17h00** est le pic d'accidents en volume (retours travail)
- **Vendredi** est le jour le plus accidenté
- La **nuit profonde** est ~2x plus meurtrière que la journée
- Le **weekend** a un taux mortalité supérieur à la semaine (~+1.5pt)
- **Juin-Juillet** affichent les pics absolus (vacances, chaleur)
- L'**hiver** a le taux mortalité le plus élevé malgré moins d'accidents

---

## Bugs corrigés

| Bug | Cause | Correction |
|-----|-------|------------|
| Warning "Ignoring 1 observations" | NA/vide dans colonne saison | `filter(!is.na(saison), saison != "")` |
| Heatmap vide | `addGeoJSON` incompatible | Architecture matrice manuelle |

---

## TODO / Idées futures
- [ ] Heatmap taux mortalité (pas seulement volume)
- [ ] Filtre par type de route (urbain vs rural — patterns très différents)
- [ ] Animation temporelle : évolution du pattern 2015→2024
- [ ] Comparaison avant/après COVID (2019 vs 2020)
- [ ] Export heatmap en PNG

---

*Dernière mise à jour : 09/03/2026 — Session rhino migration*
