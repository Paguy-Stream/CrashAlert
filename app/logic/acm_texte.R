# app/logic/acm_texte.R : texte d'interprétation de l'ACM calculé depuis l'objet MCA
box::use(
  htmltools[tags, tagList],
  bsicons[bs_icon],
  stats[setNames],
  utils[head],
)

.libelle <- c("Nuit sans \u00e9clairage" = "Nuit sans \u00e9clairage",
              "Nuit avec \u00e9clairage" = "Nuit avec \u00e9clairage",
              "Perturb\u00e9e"           = "Perturb\u00e9e")
.nom  <- function(x) ifelse(x %in% names(.libelle), unname(.libelle[x]), x)
.pct  <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "\u00a0%")
.num  <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
.de   <- function(n) if (grepl("^[AEIOUYH\u00c9]", n)) paste0("d\u2019", .nom(n)) else paste0("de ", .nom(n))
.joindre <- function(v) if (length(v) < 2) v else paste(paste(v[-length(v)], collapse = ", "), "et", v[length(v)])

#' @export
acm_texte <- function(res, ancre1 = "Nuit sans \u00e9clairage", ancre2 = "65 ans et plus") {
  co <- res$var$coord[, 1:2, drop = FALSE]; ct <- res$var$contrib[, 1:2, drop = FALSE]
  if (!all(c(ancre1, ancre2) %in% rownames(co))) return(NULL)
  x  <- setNames(co[, 1], rownames(co)); y  <- setNames(co[, 2], rownames(co))
  c1 <- setNames(ct[, 1], rownames(ct)); c2 <- setNames(ct[, 2], rownames(ct))
  s1 <- if (x[[ancre1]] >= 0) 1 else -1
  s2 <- if (y[[ancre2]] >= 0) 1 else -1
  h  <- if (s1 == 1) c("\u00e0 droite", "\u00e0 gauche") else c("\u00e0 gauche", "\u00e0 droite")
  v  <- if (s2 == 1) c("en haut", "en bas") else c("en bas", "en haut")

  cote <- function(coord, sens, k, contr) { nm <- names(coord)[sign(coord) == sens]; head(nm[order(-contr[nm])], k) }
  gr   <- function(nm, contr) tags$b(.joindre(paste0(.nom(nm), " (", .pct(contr[nm]), ")")))
  a1 <- cote(x, s1, 3, c1); b1 <- cote(x, -s1, 2, c1)
  a2 <- cote(y, s2, 2, c2); b2 <- cote(y, -s2, 4, c2)

  ok1 <- all(c("Autoroute", "Urbaine") %in% names(x)) && isTRUE(x[["Autoroute"]] * s1 > 0) && isTRUE(x[["Urbaine"]] * s1 < 0)
  ok2 <- all(c("F\u00e9minin", "Masculin") %in% names(y)) && isTRUE(y[["F\u00e9minin"]] * s2 > 0) && isTRUE(y[["Masculin"]] * s2 < 0)

  notes <- list()
  ord <- names(sort(x * s1, decreasing = TRUE))
  notes[[1]] <- tagList("\u2022 ", tags$b(.nom(ord[1])),
    paste0(" est la modalit\u00e9 la plus ", h[1], " (Dim1 = ", .num(x[[ord[1]]]), "), proche ", .de(ord[2]), " et ", .de(ord[3]),
           " : ces contextes sont souvent associ\u00e9s dans l\u2019\u00e9chantillon. "),
    tags$em("L\u2019ACM ne dit rien de leur mortalit\u00e9 ; voir l\u2019onglet Facteurs de risque."), tags$br())
  if (all(c("V\u00e9lo", "Cyclomoteur") %in% names(y)) && y[["V\u00e9lo"]] * s2 > 0 && y[["Cyclomoteur"]] * s2 < 0)
    notes[[length(notes) + 1]] <- tagList("\u2022 ", tags$b("V\u00e9lo et Cyclomoteur sont oppos\u00e9s sur l\u2019axe vertical"),
      paste0(" : le v\u00e9lo (Dim2 = ", .num(y[["V\u00e9lo"]]), ") est associ\u00e9 aux seniors, le cyclomoteur (Dim2 = ",
             .num(y[["Cyclomoteur"]]), ") aux plus jeunes."), tags$br())
  rg <- rank(x * s1)
  if (all(c("EDP motoris\u00e9", "VAE") %in% names(x)) && rg[["EDP motoris\u00e9"]] <= 5 && rg[["VAE"]] <= 5)
    notes[[length(notes) + 1]] <- tagList("\u2022 ", tags$b("Les EDP motoris\u00e9s"),
      paste0(" (Dim1 = ", .num(x[["EDP motoris\u00e9"]]), ") et le v\u00e9lo \u00e0 assistance \u00e9lectrique (Dim1 = ", .num(x[["VAE"]]),
             ") sont parmi les profils les plus ", h[2], if (ok1) ", c\u2019est-\u00e0-dire les plus urbains." else "."))

  tags$div(class = "resultat-box",
    tags$b(bs_icon("graph-up"), " Ce que les donn\u00e9es nous disent :"), tags$br(),
    tags$b(if (ok1) "Axe 1 (horizontal) : hors agglom\u00e9ration rapide vs milieu urbain." else "Axe 1 (horizontal)."),
    " Les modalit\u00e9s qui contribuent le plus \u00e0 cet axe sont ", gr(a1, c1), " ", h[1], ", oppos\u00e9es \u00e0 ", gr(b1, c1), " ", h[2], ". ",
    if (ok1) "Ce sont des contextes structurellement diff\u00e9rents : routes rapides et non \u00e9clair\u00e9es d\u2019un c\u00f4t\u00e9, voirie urbaine et deux-roues l\u00e9gers de l\u2019autre.",
    tags$br(), tags$br(),
    tags$b(if (ok2) "Axe 2 (vertical) : seniors et femmes vs jeunes et hommes." else "Axe 2 (vertical)."),
    " Cet axe est structur\u00e9 par ", gr(a2, c2), " ", v[1], ", oppos\u00e9s \u00e0 ", gr(b2, c2), " ", v[2], ".",
    tags$br(), tags$br(), tags$b("Points notables :"), tags$br(), do.call(tagList, notes), tags$br(), tags$br(),
    tags$em(style = "color:#7f8c8d;font-size:12px;", bs_icon("info-circle"),
      " Note : l\u2019ACM identifie des associations entre contextes d\u2019accidents. Pour la mortalit\u00e9 par profil, croiser avec les onglets ",
      tags$b("Profil accidentologique"), " et ", tags$b("Facteurs de risque"), "."))
}
