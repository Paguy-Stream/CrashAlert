# 08_acm.R : ACM reproductible depuis accidents_dashboard.rds (une ligne = un accident)
# Recodages (documentés, à revoir si la méthode change) :
#  - lignes écartées : âge, véhicule "Autre", météo/éclairage/route non renseignés, crépuscule ou aube
#  - meteo : Normale vs Perturbee (toutes les autres modalités renseignées)
#  - lumin : Jour / Nuit avec eclairage (allumé) / Nuit sans eclairage (absent ou éteint)
#  - route : voie communale + routes de métropole urbaine = Urbaine ; stationnement, hors réseau = Autre
suppressPackageStartupMessages({ library(dplyr); library(FactoMineR) })
dir_out <- "data_propre"; N <- 20000; graine <- 123
db <- readRDS(file.path(dir_out, "accidents_dashboard.rds"))

d <- db |> transmute(
  annee,
  age   = as.character(tranche_age),
  sexe  = as.character(sexe_dominant),
  vehic = as.character(categorie_vehicule),
  meteo = case_when(atm_label == "Normale" ~ "Normale",
                    atm_label %in% c("Autre", "Non renseign\u00e9", "Code NA") ~ NA_character_,
                    TRUE ~ "Perturbee"),
  lumin = case_when(lum_label == "Plein jour" ~ "Jour",
                    lum_label == "Nuit avec \u00e9clairage public allum\u00e9" ~ "Nuit avec eclairage",
                    lum_label %in% c("Nuit sans \u00e9clairage public", "Nuit avec \u00e9clairage public \u00e9teint") ~ "Nuit sans eclairage",
                    TRUE ~ NA_character_),
  route = case_when(catr_label == "Autoroute" ~ "Autoroute",
                    catr_label == "Route nationale" ~ "Nationale",
                    catr_label == "Route d\u00e9partementale" ~ "D\u00e9partementale",
                    catr_label %in% c("Voie communale", "Routes de m\u00e9tropole urbaine") ~ "Urbaine",
                    catr_label %in% c("Autre", "Parc de stationnement", "Hors r\u00e9seau public") ~ "Autre",
                    TRUE ~ NA_character_)
) |> filter(age != "Non renseign\u00e9", vehic != "Autre") |> filter(if_all(c(meteo, lumin, route), ~ !is.na(.x)))
cat("Lignes retenues :", format(nrow(d), big.mark = " "), "sur", format(nrow(db), big.mark = " "), "\n")

# Échantillon stratifié par année (allocation proportionnelle), taille exacte N
set.seed(graine)
idx <- unlist(lapply(split(seq_len(nrow(d)), d$annee), function(i) sample(i, round(N * length(i) / nrow(d)))), use.names = FALSE)
if (length(idx) > N) idx <- sample(idx, N)
if (length(idx) < N) idx <- c(idx, sample(setdiff(seq_len(nrow(d)), idx), N - length(idx)))
d_sample <- as_tibble(d[idx, c("age", "sexe", "vehic", "meteo", "lumin", "route")])

res <- MCA(as.data.frame(lapply(d_sample, factor)), ncp = 5, graph = FALSE)

old <- file.path(dir_out, "acm_result.rds")
if (file.exists(old)) file.copy(old, "../acm_result_avant.rds", overwrite = TRUE)
saveRDS(list(res = res, d_sample = d_sample), old)

e <- res$eig
cat("Axes calcul\u00e9s :", nrow(e), "| variance axes 1+2 :", round(sum(e[1:2, 2]), 1), "%\n")
cat("Dernier recodage mis en forme pour about.R : 2 axes affich\u00e9s sur", nrow(e), "calcul\u00e9s (",
    format(round(sum(e[1:2, 2]), 1), decimal.mark = ","), "% )\n")
print(round(prop.table(table(d_sample$route)) * 100, 1))