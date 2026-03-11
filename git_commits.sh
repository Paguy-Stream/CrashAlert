#!/bin/bash
# =============================================================
#  CrashAlert — Séquence de commits Git
#  À exécuter depuis le dossier racine du projet :
#  C:\Users\emman\Documents\mon_projet_rhino\CrashAlert\
# =============================================================
#
#  AVANT DE COMMENCER :
#  1. Créer le repo sur GitHub (sans README, sans .gitignore)
#  2. Copier l'URL du repo (ex: https://github.com/Paguy-Stream/CrashAlert.git)
#  3. Ouvrir un terminal dans le dossier CrashAlert/
#  4. Remplacer VOTRE_URL_REPO ci-dessous par l'URL réelle
#
# =============================================================

REPO_URL="https://github.com/Paguy-Stream/CrashAlert.git"

# ── INIT ──────────────────────────────────────────────────────
git init
git branch -M main
git remote add origin $REPO_URL


# ── .gitignore ────────────────────────────────────────────────
cat > .gitignore << 'EOF'
# Données lourdes (à ne pas versionner)
data_propre/accidents_final_light.rds
data_propre/*.rds

# Dossiers R
.Rproj.user/
.Rhistory
.RData
*.Rproj

# Outputs temporaires
app/report/*.html
app/report/*.pdf

# OS
.DS_Store
Thumbs.db
EOF

git add .gitignore
git commit -m "chore(init): initialisation projet rhino + .gitignore"


# ── STRUCTURE ─────────────────────────────────────────────────
# (à ce stade : rhino.yml, app.R, app/main.R vides ou squelette)
git add rhino.yml app.R app/main.R
git commit -m "chore(init): structure des dossiers rhino (app/, view/, report/)"


# ── DONNÉES ───────────────────────────────────────────────────
# Ajouter uniquement les scripts de traitement, PAS les .rds
git add data_propre/geo/
git commit -m "data(baac): ajout du fond de carte départements GeoJSON"

# Si tu as un script de nettoyage/consolidation des données :
# git add scripts/nettoyage_baac.R
# git commit -m "data(baac): script de consolidation BAAC 2015-2024 (564k lignes)"


# ── MODULES VUE ───────────────────────────────────────────────
git add app/view/dashboard.R
git commit -m "feat(dashboard): tableau de bord — KPIs nationaux et évolution annuelle"

git add app/view/risk_map.R
git commit -m "feat(map): carte de risque interactive — Leaflet + score départemental"

git add app/view/profile.R
git commit -m "feat(profile): profil accidentologique — âge, genre, mode de déplacement"

git add app/view/heatmap.R
git commit -m "feat(heatmap): analyse temporelle — heatmap heure×jour et taux circulaires"

git add app/view/facteurs.R
git commit -m "feat(facteurs): facteurs de risque — météo, route, clustering k-means"

git add app/view/analyse.R
git commit -m "feat(analyse): analyse avancée — scoring composite et modélisation"

git add app/view/about.R
git commit -m "feat(ui): onglet À propos — description du projet et méthodologie"


# ── UI / NAVIGATION ───────────────────────────────────────────
git add app/main.R
git commit -m "feat(ui): navigation principale rhino/bslib — topbar et thème CrashAlert"


# ── EXPORTS ───────────────────────────────────────────────────
# (après avoir ajouté les downloadHandler dans les modules)
git add app/view/dashboard.R app/view/profile.R app/view/heatmap.R app/view/risk_map.R
git commit -m "feat(export): export CSV par onglet — dashboard, profile, heatmap, map"


# ── RAPPORT ───────────────────────────────────────────────────
git add app/report/
git commit -m "feat(report): rapport HTML/PDF automatique par région (rmarkdown + kableExtra)"


# ── CORRECTIONS ───────────────────────────────────────────────
git add app/view/profile.R
git commit -m "fix(profile): restauration graphique taux mortalité Homme vs Femme"

git add app/view/profile.R
git commit -m "fix(profile): correction filtre 'Non renseigné' sur axe X tranche d'âge"

git add app/report/rapport_region.Rmd
git commit -m "fix(report): correction rendu PDF — booktabs LaTeX, HOLD_position, always_allow_html"


# ── DOCUMENTATION ─────────────────────────────────────────────
# Copier le README.md généré à la racine du projet avant ce commit
git add README.md
git commit -m "docs(readme): README complet — présentation, installation, structure, stack"


# ── PUSH ──────────────────────────────────────────────────────
git push -u origin main

echo ""
echo "✅ Tous les commits ont été poussés sur GitHub !"
echo "🔗 Vérifiez votre repo : $REPO_URL"
