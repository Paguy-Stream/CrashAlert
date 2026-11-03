# deploy.R : deploiement reproductible (n'embarque que ce dont l'appli a besoin)
tous  <- list.files(all.files = TRUE, recursive = TRUE, include.dirs = FALSE)
exclu <- paste0("^(\\.git/|\\.Rproj\\.user/|renv/library|renv/staging|renv/local|rsconnect/|donnees/|",
                "data_pipeline/|tests/|\\.github/|app/docs/DOC_ONGLET|\\.RData$|\\.Rhistory$|",
                "0[0-9]_[^/]*\\.R$|run_pipeline\\.R$|test_pipeline\\.R$|deploy\\.R$)")
files <- tous[!grepl(exclu, tous)]
stopifnot(all(c("app.R", "renv.lock", "config.yml", "rhino.yml",
                "data_propre/accidents_dashboard.rds", "app/logic/acm_texte.R") %in% files))
cat("Fichiers :", length(files), "| taille :", round(sum(file.size(files)) / 1024^2, 1), "Mo\n")
rsconnect::deployApp(appName = "CrashCirculation", appFiles = files, forceUpdate = TRUE)

