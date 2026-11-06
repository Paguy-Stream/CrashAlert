# 06_finalisation.R — à lancer APRÈS run_pipeline.R (qui réécrit accidents_dashboard.rds)
suppressPackageStartupMessages(library(dplyr))
dir_arch <- "data_pipeline"; dir_out <- "data_propre"

# ── 0. Référence ONISR (personnes tuées) : à compléter avec les autres années ──
ref <- data.frame(annee = 2025, metropole = 3263, outre_mer = 252)

# ── 1. Lecture des archives annuelles ─────────────────────────────────────────
cols <- c("annee","mois","dep","departement","region","gravite_accident","nb_tues",
          "nb_blesses_hospitalises","nb_blesses_legers","nb_indemnes","agg","lum_label",
          "col_label","catr_label","tranche_age","categorie_vehicule","categories_impliquees","jour_semaine")
fich <- list.files(dir_arch, "^accidents_\\d{4}\\.rds$", full.names = TRUE)
base <- bind_rows(lapply(fich, function(f) {
  an <- as.integer(sub(".*accidents_(\\d{4})\\.rds$", "\\1", f))
  x <- readRDS(f)[, cols] |>
    mutate(across(c(dep, departement, region, gravite_accident, lum_label, col_label,
                    catr_label, tranche_age, categorie_vehicule, categories_impliquees, jour_semaine), as.character),
           annee = as.numeric(annee), mois = as.integer(mois), agg = as.integer(agg))
  stopifnot(all(x$annee == an)); x
}))
cat("Archives :", length(fich), "| accidents :", nrow(base), "| années :", paste(range(base$annee), collapse = "-"), "\n")
stopifnot(!any(grepl("^Département", base$departement)), !anyNA(base$region))

# ── 2. Test ONISR : on s'arrête avant d'écrire quoi que ce soit ───────────────
tues <- base |> group_by(annee) |>
  summarise(metropole = sum(nb_tues[region != "Outre-mer"]),
            outre_mer = sum(nb_tues[region == "Outre-mer"]), .groups = "drop")
chk <- inner_join(ref, tues, by = "annee", suffix = c("_ref", "_obs"))
print(chk)
if (any(chk$metropole_ref != chk$metropole_obs | chk$outre_mer_ref != chk$outre_mer_obs))
  stop("Écart avec le bilan ONISR : rien n'a été écrit.")
manq <- setdiff(unique(base$annee), ref$annee)
if (length(manq)) message("Années sans référence ONISR (non testées) : ", paste(manq, collapse = ", "))

# ── 3. Dashboard produit par 05_export.R : cohérence avec les archives ────────
db <- readRDS(file.path(dir_out, "accidents_dashboard.rds"))
stopifnot(nrow(db) == nrow(base))

# ── 4. Sauvegarde ─────────────────────────────────────────────────────────────
sauv <- file.path("..", "agg_backup", paste0("avant_finalisation_", format(Sys.Date(), "%Y%m%d")))
dir.create(sauv, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(dir_out, "\\.rds$", full.names = TRUE), sauv, overwrite = FALSE)

# ── 5. dept_filters ───────────────────────────────────────────────────────────
old_df <- readRDS(file.path(dir_out, "dept_filters.rds"))
new_df <- db |> distinct(dep, region, departement) |>
  mutate(across(everything(), as.character)) |> arrange(departement)
stopifnot(!anyDuplicated(new_df$dep), identical(names(new_df), names(old_df)))

# ── 6. Agrégats (« Grave » = gravité maximale Grave, comme partout dans l'app) ─
old_ev <- readRDS(file.path(dir_out, "agg_evolution.rds"))
stopifnot(is.factor(old_ev$mois_nom))
lib_mois <- levels(old_ev$mois_nom)
saison_map <- setNames(old_ev$saison[match(sprintf("%02d", 1:12), old_ev$mois)], sprintf("%02d", 1:12))

nat <- base |> group_by(annee) |> summarise(taux_nat = mean(gravite_accident == "Mortel") * 100, .groups = "drop")
new_da <- base |> group_by(annee, dep_clean = dep) |>
  summarise(departement = first(departement), region = first(region), nb_accidents = n(),
            nb_mortels = sum(gravite_accident == "Mortel"), nb_graves = sum(gravite_accident == "Grave"),
            nb_tues = sum(nb_tues, na.rm = TRUE), .groups = "drop") |>
  mutate(taux_mortalite = round(nb_mortels / nb_accidents * 100, 2)) |>
  left_join(nat, by = "annee") |>
  mutate(indice_surmortalite = round(taux_mortalite / taux_nat * 100, 1)) |> select(-taux_nat)

new_ev <- base |> group_by(annee, mois = sprintf("%02d", mois)) |>
  summarise(nb_accidents = n(), nb_mortels = sum(gravite_accident == "Mortel"),
            nb_graves = sum(gravite_accident == "Grave"), nb_tues = sum(nb_tues, na.rm = TRUE),
            nb_blesses_hospitalises = sum(nb_blesses_hospitalises, na.rm = TRUE),
            nb_blesses_legers = sum(nb_blesses_legers, na.rm = TRUE),
            nb_indemnes = sum(nb_indemnes, na.rm = TRUE), .groups = "drop") |>
  mutate(taux_mortalite = round(nb_mortels / nb_accidents * 100, 2),
         taux_gravite = round((nb_mortels + nb_graves) / nb_accidents * 100, 1),
         mois_nom = factor(lib_mois[as.integer(mois)], levels = lib_mois),
         trimestre = paste0("T", ceiling(as.integer(mois) / 3)), saison = unname(saison_map[mois]))

new_dep <- base |> group_by(dep) |>
  summarise(region = first(region), departement = first(departement), nb_accidents = n(),
            nb_mortels = sum(gravite_accident == "Mortel"), nb_graves = sum(gravite_accident == "Grave"),
            nb_tues = sum(nb_tues, na.rm = TRUE),
            nb_blesses_hospitalises = sum(nb_blesses_hospitalises, na.rm = TRUE),
            n_annees = n_distinct(annee), .groups = "drop") |>
  mutate(nb_accidents_moyen_annuel = round(nb_accidents / n_annees),
         taux_mortalite = round(nb_mortels / nb_accidents * 100, 2),
         taux_gravite = round((nb_mortels + nb_graves) / nb_accidents * 100, 1),
         densite_accidents = nb_accidents_moyen_annuel,
         classe_gravite = case_when(taux_mortalite >= 10 ~ "Très élevée", taux_mortalite >= 5 ~ "Élevée",
                                    taux_mortalite >= 2 ~ "Moyenne", TRUE ~ "Faible")) |>
  select(dep, region, departement, nb_accidents, nb_accidents_moyen_annuel, nb_mortels, nb_graves,
         taux_mortalite, taux_gravite, nb_tues, nb_blesses_hospitalises, densite_accidents, classe_gravite) |>
  arrange(desc(taux_mortalite))
stopifnot(sum(new_ev$nb_accidents) == nrow(base), sum(new_da$nb_accidents) == nrow(base),
          sum(new_dep$nb_accidents) == nrow(base), setequal(new_df$dep, new_dep$dep))

# ── 7. Évolution métropole, score structurel (sans mortalité) et clustering ────
metro <- c(setdiff(sprintf("%02d", 1:95), "20"), "2A", "2B")
bm <- filter(base, dep %in% metro)
new_evd <- bm |> group_by(annee, dep_clean = dep) |>
  summarise(departement = first(departement), taux_mort = round(mean(gravite_accident == "Mortel") * 100, 2),
            n = n(), .groups = "drop")
new_evf <- bm |> group_by(annee) |>
  summarise(taux_mort = round(mean(gravite_accident == "Mortel") * 100, 3), .groups = "drop")

nr <- c("Non renseigné", "Code NA")
part <- function(x, vrai, inval = nr) { v <- !(x %in% inval) & !is.na(x); if (!any(v)) return(NA_real_); mean(vrai[v]) * 100 }
vars <- c("part_route_dept","part_sans_collision","part_nuit_sans_eclairage","part_hors_agglo")
feat <- bm |> group_by(dep_clean = dep) |>
  summarise(departement = first(departement), region = first(region), nb_accidents = n(),
    part_route_dept          = part(catr_label, catr_label == "Route départementale"),
    part_sans_collision      = part(col_label,  col_label == "Sans collision"),
    part_nuit_sans_eclairage = part(lum_label,  lum_label %in% c("Nuit sans éclairage public","Nuit avec éclairage public éteint")),
    part_hors_agglo          = mean(agg == 1, na.rm = TRUE) * 100,
    part_accidents_18_24     = mean(tranche_age == "18-24 ans", na.rm = TRUE) * 100,
    part_accidents_2rm       = mean((grepl("|Cyclomoteur|", categories_impliquees, fixed = TRUE) |
                                         grepl("|Deux-roues motorisé|", categories_impliquees, fixed = TRUE))[!is.na(categories_impliquees)]) * 100,
    part_accidents_weekend   = mean(jour_semaine %in% c("samedi","dimanche"), na.rm = TRUE) * 100,
    taux_mortalite = mean(gravite_accident == "Mortel") * 100,   # validation uniquement
    .groups = "drop")
stopifnot(nrow(feat) == 96, !anyNA(feat[, vars]))
z <- scale(as.matrix(feat[, vars])); zs <- as.data.frame(z); names(zs) <- paste0(vars, "_z")
feat$score_brut   <- rowMeans(z)
feat$score_risque <- round((feat$score_brut - min(feat$score_brut)) / diff(range(feat$score_brut)) * 100, 1)
q <- quantile(feat$score_risque, 0:4/4); if (anyDuplicated(q)) stop("Quartiles non distincts")
feat$classe_risque <- as.character(cut(feat$score_risque, breaks = q, include.lowest = TRUE,
                                       labels = c("Faible","Modéré","Élevé","Très élevé")))
feat <- bind_cols(feat, zs)
set.seed(42)
sil <- sapply(2:4, function(k) { km <- kmeans(z, centers = k, nstart = 25, iter.max = 100)
  mean(cluster::silhouette(km$cluster, dist(z))[, 3]) })
km <- kmeans(z, centers = 3, nstart = 25, iter.max = 100)
ordre <- order(tapply(feat$score_brut, km$cluster, mean), decreasing = TRUE)
feat$cluster <- as.character(match(km$cluster, ordre))
feat$cluster_nom <- unname(c("1" = "Réseau rural dominant", "2" = "Réseau mixte", "3" = "Réseau urbain dominant")[feat$cluster])
r <- cor(feat$score_risque, feat$taux_mortalite); p <- prcomp(z)
attr(feat, "meta") <- list(periode = paste(range(base$annee), collapse = "-"), n_accidents = nrow(bm), vars = vars,
  silhouette = setNames(round(sil, 3), paste0("k", 2:4)), pearson = round(r, 2), r2 = round(r^2, 2),
  spearman = round(cor(feat$score_risque, feat$taux_mortalite, method = "spearman"), 2),
  pc1_var = round(100 * p$sdev[1]^2 / sum(p$sdev^2), 1))

# ── 8. Dashboard optimisé (idempotent) ────────────────────────────────────────
db$Num_Acc <- NULL
db <- db |> mutate(
  across(c(annee, mois, jour, heure, minute, nb_victimes, nb_tues, nb_blesses_hospitalises), as.integer),
  across(c(sexe_dominant, tranche_age, jour_semaine, saison, catu_principal, secu_principal), as.factor),
  across(c(dep, departement, region), as.character))

# ── 9. Écriture (tous les contrôles ont réussi) ──────────────────────────────
saveRDS(new_df,  file.path(dir_out, "dept_filters.rds"))
saveRDS(new_da,  file.path(dir_out, "agg_dept_annee.rds"))
saveRDS(new_ev,  file.path(dir_out, "agg_evolution.rds"))
saveRDS(new_dep, file.path(dir_out, "agg_departement.rds"))
saveRDS(new_evd, file.path(dir_out, "agg_evol_dept.rds"))
saveRDS(new_evf, file.path(dir_out, "agg_evol_france.rds"))
saveRDS(feat,    file.path(dir_out, "features_clustering.rds"))
saveRDS(db,      file.path(dir_out, "accidents_dashboard.rds"), compress = "xz")
if (file.exists(file.path(dir_out, "accidents_light.rds"))) file.remove(file.path(dir_out, "accidents_light.rds"))

cat("\nTerminé | dashboard :", round(object.size(db) / 1024^2, 1), "Mo | départements :", nrow(feat),
    "| r =", attr(feat, "meta")$pearson, "| R² =", attr(feat, "meta")$r2, "\n")
print(table(feat$cluster_nom))
cat("À refaire à part : acm_result.rds (non régénéré ici).\n")
