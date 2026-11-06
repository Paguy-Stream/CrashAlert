
.CATEGORIES <- c("EDP non motoris\u00e9", "V\u00e9lo", "VAE", "EDP motoris\u00e9", "Cyclomoteur",
                 "Deux-roues motoris\u00e9", "Voiturette", "V\u00e9hicule l\u00e9ger",
                 "V\u00e9hicule utilitaire", "Tracteur agricole", "Transport en commun",
                 "Poids lourd", "Autre")

#' Accidents par categorie de vehicule impliquee et par gravite.
#' Un accident est compte dans chaque categorie impliquee (definition ONISR).
#' Retourne : categorie_vehicule, grav, nb.
#' @export
par_categorie <- function(d) {
  ci <- d$categories_impliquees
  g  <- as.character(d$gravite_accident)
  res <- lapply(.CATEGORIES, function(k) {
    i <- grepl(paste0("|", k, "|"), ci, fixed = TRUE)
    if (!any(i)) return(NULL)
    t <- table(g[i])
    if (length(t) == 0) return(NULL)
    data.frame(categorie_vehicule = k, grav = names(t), nb = as.integer(t),
               stringsAsFactors = FALSE)
  })
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res) == 0)
    return(data.frame(categorie_vehicule = character(), grav = character(), nb = integer()))
  do.call(rbind, res)
}

