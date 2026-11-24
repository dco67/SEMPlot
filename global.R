# ═══════════════════════════════════════════════════════════════════════════
#  SEMPlot — atelier des arguments de semPaths()          global.R (moteur)
#  Modèles d'exemple, dictionnaire des arguments, construction de l'appel
#  et du code R reproductible.
# ═══════════════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(lavaan)
  library(semPlot)
})

# ── 1. Modèles d'exemple ──────────────────────────────────────────────────
# Chaque modèle : titre, description, code R qui le produit (affiché tel quel
# dans le code reproductible) et objet lavaan ajusté. Les estimations ne sont
# pas l'objet de l'application : elles ne servent qu'à remplir le diagramme.

HS <- lavaan::HolzingerSwineford1939
HS_ord <- HS
for (v in paste0("x", 1:9)) HS_ord[[v]] <- cut(HS[[v]], quantile(HS[[v]], c(0, 1/3, 2/3, 1)),
                                              labels = FALSE, include.lowest = TRUE)

MODELES <- list(
  chemins = list(
    titre = "Chemins (variables observées)",
    description = "Modèle de chemins sur cinq variables observées : aucune variable latente, seulement des rectangles, des flèches causales, des résidus et une covariance entre exogènes.",
    code = c(
      "# Données simulées (graine fixe)",
      "pop <- '",
      "  Revenu  ~ 0.5*Education",
      "  Sante   ~ 0.3*Revenu + 0.2*Education + -0.3*Age",
      "  Bonheur ~ 0.4*Sante + 0.2*Revenu",
      "  Education ~~ -0.2*Age'",
      "set.seed(2026)",
      "donnees <- simulateData(pop, sample.nobs = 400)",
      "",
      "modele <- '",
      "  Revenu  ~ Education",
      "  Sante   ~ Revenu + Education + Age",
      "  Bonheur ~ Sante + Revenu'",
      "ajustement <- sem(modele, data = donnees)")),
  afc = list(
    titre = "Analyse factorielle confirmatoire",
    description = "Holzinger et Swineford (1939) : trois facteurs latents corrélés (visuel, verbal, vitesse), mesurés chacun par trois tests. La première saturation de chaque facteur est fixée à 1 (flèche en tirets).",
    code = c(
      "modele <- '",
      "  visuel  =~ x1 + x2 + x3",
      "  verbal  =~ x4 + x5 + x6",
      "  vitesse =~ x7 + x8 + x9'",
      "ajustement <- cfa(modele, data = HolzingerSwineford1939)")),
  sem = list(
    titre = "Équations structurelles (Bollen)",
    description = "Démocratisation politique (Bollen, 1989) : l'industrialisation de 1960 influence la démocratie de 1960 et de 1965. Modèle de mesure + modèle structurel, avec des erreurs corrélées entre les mêmes indicateurs mesurés à deux dates.",
    code = c(
      "modele <- '",
      "  # mesure",
      "  ind60 =~ x1 + x2 + x3",
      "  dem60 =~ y1 + y2 + y3 + y4",
      "  dem65 =~ y5 + y6 + y7 + y8",
      "  # structure",
      "  dem60 ~ ind60",
      "  dem65 ~ ind60 + dem60",
      "  # erreurs corrélées",
      "  y1 ~~ y5",
      "  y2 ~~ y4 + y6",
      "  y3 ~~ y7",
      "  y4 ~~ y8",
      "  y6 ~~ y8'",
      "ajustement <- sem(modele, data = PoliticalDemocracy)")),
  multigroupe = list(
    titre = "Multigroupe (deux écoles)",
    description = "Le modèle de Holzinger et Swineford ajusté séparément dans deux écoles (Pasteur et Grant-White). semPaths dessine un diagramme par groupe ; panelGroups et combineGroups changent la façon de les présenter.",
    code = c(
      "modele <- '",
      "  visuel  =~ x1 + x2 + x3",
      "  verbal  =~ x4 + x5 + x6",
      "  vitesse =~ x7 + x8 + x9'",
      "ajustement <- cfa(modele, data = HolzingerSwineford1939, group = \"school\")")),
  croissance = list(
    titre = "Croissance latente",
    description = "Quatre mesures répétées (t1 à t4) : un niveau initial (i) et une pente (s), prédits par deux covariables. Toutes les saturations sont fixées (tirets) ; le modèle comprend des moyennes, d'où les triangles des ordonnées à l'origine.",
    code = c(
      "modele <- '",
      "  i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4",
      "  s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4",
      "  i ~ x1 + x2",
      "  s ~ x1 + x2'",
      "ajustement <- growth(modele, data = Demo.growth)")),
  ordinal = list(
    titre = "Indicateurs ordinaux (seuils)",
    description = "Le modèle de Holzinger et Swineford avec des indicateurs ramenés à trois catégories, traités comme ordinaux : chaque indicateur reçoit deux seuils, que semPaths dessine en traits dans les rectangles.",
    code = c(
      "# Chaque test ramené à trois catégories (terciles)",
      "hs3 <- HolzingerSwineford1939",
      "for (v in paste0(\"x\", 1:9))",
      "  hs3[[v]] <- cut(hs3[[v]], quantile(hs3[[v]], c(0, 1/3, 2/3, 1)),",
      "                  labels = FALSE, include.lowest = TRUE)",
      "modele <- '",
      "  visuel  =~ x1 + x2 + x3",
      "  verbal  =~ x4 + x5 + x6",
      "  vitesse =~ x7 + x8 + x9'",
      "ajustement <- cfa(modele, data = hs3, ordered = paste0(\"x\", 1:9))")),
  bifacteur = list(
    titre = "Bifacteur",
    description = "Un facteur général (g) mesuré par les neuf tests, et trois facteurs spécifiques orthogonaux. L'argument bifactor place le facteur général de l'autre côté des indicateurs.",
    code = c(
      "modele <- '",
      "  g       =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9",
      "  visuel  =~ x1 + x2 + x3",
      "  verbal  =~ x4 + x5 + x6",
      "  vitesse =~ x7 + x8 + x9'",
      "ajustement <- cfa(modele, data = HolzingerSwineford1939, orthogonal = TRUE, std.lv = TRUE)"))
)

ajuster_modele <- function(cle) {
  env <- new.env(parent = globalenv())
  env$HolzingerSwineford1939 <- HS; env$PoliticalDemocracy <- lavaan::PoliticalDemocracy
  env$Demo.growth <- lavaan::Demo.growth
  suppressWarnings(eval(parse(text = MODELES[[cle]]$code), envir = env))
  env$ajustement
}
AJUSTEMENTS <- lapply(setNames(names(MODELES), names(MODELES)), ajuster_modele)
CHOIX_MODELES <- setNames(names(MODELES), vapply(MODELES, `[[`, "", "titre"))

# ── 2. Dictionnaire des arguments ─────────────────────────────────────────
# type : select (valeurs en texte), check (TRUE/FALSE), slider (nombre),
#        color (couleur), text.
# defaut : valeur par défaut de semPaths ; "auto" = argument omis (semPaths
#          choisit selon le modèle).  num = TRUE : la valeur est un nombre.
#  modeles : NULL = utile partout ; sinon, modèles où l'argument a un effet.
A <- function(arg, groupe, type, libelle, defaut, aide, choix = NULL, min = NULL, max = NULL,
              pas = NULL, num = FALSE, qgraph = FALSE, modeles = NULL, lie = NULL)
  list(arg = arg, groupe = groupe, type = type, libelle = libelle, defaut = defaut, aide = aide,
       choix = choix, min = min, max = max, pas = pas, num = num, qgraph = qgraph, modeles = modeles,
       lie = lie)

AUTO <- c("Automatique (omis)" = "auto")
COULEURS <- c("Automatique (omis)" = "auto", "Noir" = "#000000", "Blanc" = "#FFFFFF", "Gris clair" = "#E6E6E6",
  "Gris foncé" = "#555555", "Bleu pâle" = "#DCEBFA", "Vert pâle" = "#DFF2E1", "Jaune pâle" = "#FFF6CC",
  "Bleu foncé" = "#1F4E79", "Bordeaux" = "#8E2C2C")
OUI_NON_AUTO <- c(AUTO, "Oui (TRUE)" = "TRUE", "Non (FALSE)" = "FALSE")

ARGS <- list(
  # ── Contenu ──
  A("what", "Contenu", "select", "Ce que montrent les flèches", "paths",
    "Ce que représentent l'épaisseur et la couleur des flèches. « paths » dessine seulement la structure, toutes les flèches identiques ; « est » et « std » rendent chaque flèche d'autant plus épaisse que l'estimation (brute ou standardisée) est forte, verte si positive, rouge si négative ; « eq » colore de la même façon les paramètres contraints égaux ; « col » colore les flèches selon les groupes de variables.",
    c("Structure seulement (paths)" = "paths", "Estimations (est)" = "est", "Estimations standardisées (std)" = "std",
      "Contraintes d'égalité (eq)" = "eq", "Couleurs des groupes (col)" = "col")),
  A("whatLabels", "Contenu", "select", "Étiquettes des flèches", "auto",
    "Le texte écrit sur les flèches. Par défaut, il suit « what » (aucune étiquette avec « paths »). « name » affiche le nom du paramètre, « est » l'estimation brute, « std » l'estimation standardisée, « hide » n'affiche rien.",
    c(AUTO, "Aucune (hide)" = "hide", "Noms des paramètres (name)" = "name", "Estimations (est)" = "est",
      "Estimations standardisées (std)" = "std", "Contraintes d'égalité (eq)" = "eq")),
  A("nDigits", "Contenu", "slider", "Décimales des étiquettes", 2,
    "Nombre de décimales des estimations écrites sur les flèches (avec whatLabels = « est » ou « std »).",
    min = 0, max = 4, pas = 1, num = TRUE),
  A("residuals", "Contenu", "check", "Afficher les résidus", TRUE,
    "Dessine les variances résiduelles (erreurs) des variables endogènes et les variances des exogènes. Leur forme dépend de « style » : petite boucle (ram) ou flèche entrante (lisrel)."),
  A("intercepts", "Contenu", "check", "Afficher les ordonnées à l'origine", TRUE,
    "Dessine les moyennes et ordonnées à l'origine sous forme de triangles. N'a d'effet que si le modèle estime des moyennes : ici, les modèles de croissance, multigroupe et ordinal.",
    modeles = c("croissance", "ordinal", "multigroupe")),
  A("thresholds", "Contenu", "check", "Afficher les seuils", TRUE,
    "Dessine les seuils des indicateurs ordinaux en petits traits dans les rectangles. N'a d'effet qu'avec des indicateurs ordinaux.",
    modeles = "ordinal"),
  A("exoVar", "Contenu", "select", "Variances des exogènes", "auto",
    "Affiche (TRUE) ou masque (FALSE) les variances des variables exogènes.", OUI_NON_AUTO),
  A("exoCov", "Contenu", "check", "Covariances entre exogènes", TRUE,
    "Affiche les covariances (doubles flèches courbes) entre variables exogènes. Les masquer allège le diagramme, mais il ne représente plus tout le modèle.",
    modeles = c("chemins", "afc", "multigroupe", "ordinal", "croissance")),
  A("structural", "Contenu", "check", "Partie structurelle seulement", FALSE,
    "Ne dessine que les variables latentes et les relations entre elles : les indicateurs disparaissent. Utile pour présenter le modèle structurel d'un grand SEM.",
    modeles = c("afc", "sem", "multigroupe", "ordinal", "croissance", "bifacteur")),
  A("nCharNodes", "Contenu", "slider", "Caractères des noms de variables", 3,
    "Les noms des variables sont abrégés à ce nombre de caractères. 0 = noms complets. Avec la valeur par défaut (3), « Education » devient « Edc ».",
    min = 0, max = 10, pas = 1, num = TRUE),
  # ── Disposition ──
  A("layout", "Disposition", "select", "Disposition générale", "tree",
    "Algorithme qui place les variables. « tree » les range en niveaux (exogènes d'un côté, endogènes de l'autre) ; « tree2 » et « tree3 » en sont des variantes qui gèrent mieux les modèles chargés ; « circle » les dispose en cercle ; « spring » place les variables reliées près les unes des autres.",
    c("Arbre (tree)" = "tree", "Arbre 2 (tree2)" = "tree2", "Arbre 3 (tree3)" = "tree3",
      "Cercle (circle)" = "circle", "Cercle 2 (circle2)" = "circle2", "Cercle 3 (circle3)" = "circle3",
      "Ressorts (spring)" = "spring")),
  A("rotation", "Disposition", "select", "Orientation", "1",
    "Côté où se trouvent les variables exogènes : 1 = en haut, 2 = à gauche, 3 = en bas, 4 = à droite. Ne s'applique qu'aux dispositions en arbre.",
    c("1 : exogènes en haut" = "1", "2 : exogènes à gauche" = "2", "3 : exogènes en bas" = "3", "4 : exogènes à droite" = "4"),
    num = TRUE),
  A("layoutSplit", "Disposition", "check", "Disposer mesure et structure séparément", FALSE,
    "Place d'abord les variables latentes (partie structurelle), puis chaque modèle de mesure autour d'elles, selon « measurementLayout ». Utile pour les SEM complexes.",
    modeles = c("sem", "afc", "multigroupe", "bifacteur")),
  A("measurementLayout", "Disposition", "select", "Disposition des modèles de mesure", "tree",
    "Avec layoutSplit = TRUE : disposition des indicateurs autour de chaque variable latente.",
    c("Arbre (tree)" = "tree", "Cercle (circle)" = "circle"), modeles = c("sem", "afc", "multigroupe", "bifacteur")),
  A("bifactor", "Disposition", "select", "Facteur général (bifactor)", "auto",
    "Nom du facteur général d'un modèle bifacteur : il est placé de l'autre côté des indicateurs, face aux facteurs spécifiques.",
    c("Aucun (omis)" = "auto", "g" = "g"), modeles = "bifacteur"),
  A("centerLevels", "Disposition", "check", "Centrer les niveaux", TRUE,
    "Centre les variables de chaque niveau de l'arbre. Désactivé, elles s'alignent sur le bord gauche."),
  A("optimizeLatRes", "Disposition", "check", "Placer au mieux les résidus des latentes", FALSE,
    "Oriente les résidus des variables latentes vers l'angle le moins encombré, plutôt que toujours dans la même direction.",
    modeles = c("sem", "croissance", "afc", "multigroupe", "ordinal", "bifacteur")),
  A("intAtSide", "Disposition", "select", "Ordonnées à l'origine sur le côté", "auto",
    "Place les triangles des ordonnées à l'origine sur le côté des variables plutôt qu'au-dessous.",
    OUI_NON_AUTO, modeles = c("croissance", "multigroupe")),
  A("ThreshAtSide", "Disposition", "check", "Seuils sur le côté", FALSE,
    "Dessine les seuils sur le côté des rectangles plutôt qu'à l'intérieur.", modeles = "ordinal"),
  # ── Flèches ──
  A("style", "Flèches", "select", "Style des résidus", "auto",
    "Convention de dessin des variances résiduelles. « ram » : boucle à double tête sur la variable ; « lisrel » : flèche simple venant d'un petit cercle d'erreur, comme dans la plupart des manuels ; « mx » : comme ram, à la manière d'OpenMx.",
    c(AUTO, "RAM (boucles)" = "ram", "LISREL (flèches d'erreur)" = "lisrel", "OpenMx (mx)" = "mx")),
  A("residScale", "Flèches", "slider", "Taille des résidus", 5,
    "Taille des boucles ou des cercles d'erreur des résidus. Par défaut, elle suit la largeur des variables observées (sizeMan).",
    min = 2, max = 20, pas = 0.5, num = TRUE, lie = "sizeMan"),
  A("curve", "Flèches", "slider", "Courbure des covariances", 1,
    "Courbure des doubles flèches entre variables qui ne sont pas voisines.", min = 0, max = 4, pas = 0.25, num = TRUE),
  A("curvature", "Flèches", "slider", "Courbure entre voisines", 1,
    "Courbure des doubles flèches entre variables voisines du même niveau (par exemple les covariances entre facteurs).",
    min = 0, max = 4, pas = 0.25, num = TRUE),
  A("covAtResiduals", "Flèches", "check", "Covariances attachées aux résidus", TRUE,
    "Avec le style lisrel, les covariances entre erreurs partent des cercles d'erreur plutôt que des variables elles-mêmes.",
    modeles = c("sem", "chemins")),
  A("fixedStyle", "Flèches", "select", "Trait des paramètres fixés", "2",
    "Type de trait des paramètres fixés (par exemple la première saturation fixée à 1) : 2 = tirets, 1 = trait plein, 3 = pointillés. C'est ce qui distingue, sur le diagramme, ce qui est estimé de ce qui est imposé.",
    c("Tirets (2)" = "2", "Trait plein (1)" = "1", "Pointillés (3)" = "3"), num = TRUE),
  A("edge.label.cex", "Flèches", "slider", "Taille des étiquettes des flèches", 0.6,
    "Taille du texte écrit sur les flèches.", min = 0.3, max = 2, pas = 0.1, num = TRUE),
  A("edge.label.position", "Flèches", "slider", "Position des étiquettes", 0.5,
    "Position de l'étiquette le long de la flèche : 0 = au départ, 1 = à l'arrivée. Déplacer les étiquettes évite qu'elles se chevauchent.",
    min = 0.1, max = 0.9, pas = 0.05, num = TRUE, qgraph = TRUE),
  A("edge.width", "Flèches", "slider", "Épaisseur des flèches", 1,
    "Multiplie l'épaisseur de toutes les flèches.", min = 0.3, max = 3, pas = 0.1, num = TRUE, qgraph = TRUE),
  A("asize", "Flèches", "slider", "Taille des pointes", 2,
    "Taille des pointes de flèches.", min = 0.5, max = 6, pas = 0.5, num = TRUE, qgraph = TRUE),
  A("fade", "Flèches", "check", "Estomper les flèches faibles", TRUE,
    "Avec what = « est » ou « std », les flèches faibles deviennent plus pâles. Désactivé, toutes ont la même intensité.",
    qgraph = TRUE),
  # ── Nœuds ──
  A("sizeMan", "Nœuds", "slider", "Largeur des variables observées", 5,
    "Taille des rectangles (variables observées).", min = 2, max = 15, pas = 0.5, num = TRUE),
  A("sizeMan2", "Nœuds", "slider", "Hauteur des variables observées", 5,
    "Hauteur des rectangles. Par défaut, elle égale la largeur (sizeMan) : les variables observées sont des carrés. La réduire donne des rectangles plus larges que hauts.",
    min = 1, max = 15, pas = 0.5, num = TRUE, lie = "sizeMan"),
  A("sizeLat", "Nœuds", "slider", "Taille des variables latentes", 8,
    "Taille des cercles (variables latentes).", min = 3, max = 20, pas = 0.5, num = TRUE),
  A("sizeInt", "Nœuds", "slider", "Taille des triangles", 2,
    "Taille des triangles des ordonnées à l'origine.", min = 1, max = 10, pas = 0.5, num = TRUE, modeles = c("croissance", "ordinal", "multigroupe")),
  A("shapeMan", "Nœuds", "select", "Forme des variables observées", "auto",
    "Forme des variables observées ; par convention, des rectangles ou des carrés.",
    c(AUTO, "Rectangle" = "rectangle", "Carré" = "square", "Cercle" = "circle", "Ellipse" = "ellipse", "Losange" = "diamond")),
  A("shapeLat", "Nœuds", "select", "Forme des variables latentes", "auto",
    "Forme des variables latentes ; par convention, des cercles ou des ellipses.",
    c(AUTO, "Cercle" = "circle", "Ellipse" = "ellipse", "Carré" = "square", "Rectangle" = "rectangle")),
  A("label.cex", "Nœuds", "slider", "Taille des noms de variables", 1,
    "Taille du texte dans les nœuds.", min = 0.5, max = 2.5, pas = 0.1, num = TRUE, qgraph = TRUE),
  A("label.scale", "Nœuds", "check", "Ajuster le texte à la taille du nœud", TRUE,
    "Réduit automatiquement le texte pour qu'il tienne dans le nœud. Désactivé, label.cex s'applique tel quel.",
    qgraph = TRUE),
  A("equalizeManifests", "Nœuds", "check", "Uniformiser les variables observées", FALSE,
    "Donne la même taille à toutes les variables observées, quelle que soit la longueur de leur nom."),
  # ── Couleurs ──
  A("groups", "Couleurs", "select", "Colorer par groupe de variables", "auto",
    "Donne une couleur à chaque variable latente et à ses indicateurs (« latents »), ou à chaque variable observée (« manifests »).",
    c("Non (omis)" = "auto", "Par facteur (latents)" = "latents", "Par variable observée (manifests)" = "manifests"),
    modeles = c("afc", "sem", "multigroupe", "croissance", "ordinal", "bifacteur")),
  A("pastel", "Couleurs", "check", "Couleurs pastel", FALSE,
    "Avec groups : des teintes pastel, plus lisibles à l'impression."),
  A("theme", "Couleurs", "select", "Thème de couleurs (qgraph)", "auto",
    "Palette des flèches positives et négatives. « colorblind » convient aux personnes daltoniennes ; « gray » donne un diagramme en niveaux de gris.",
    c("Classique (omis)" = "auto", "Daltonisme (colorblind)" = "colorblind", "Gris (gray)" = "gray"), qgraph = TRUE),
  A("edge.color", "Couleurs", "select", "Couleur unique des flèches", "auto",
    "Impose une seule couleur à toutes les flèches (par exemple du noir pour une publication). Elle remplace les couleurs selon le signe.", COULEURS),
  A("color.lat", "Couleurs", "select", "Couleur des variables latentes", "auto",
    "Couleur de remplissage des variables latentes. Avec color.man, forme l'argument color = list(lat = …, man = …).", COULEURS),
  A("color.man", "Couleurs", "select", "Couleur des variables observées", "auto",
    "Couleur de remplissage des variables observées.", COULEURS),
  A("title", "Multigroupe", "select", "Afficher le nom du groupe", "auto",
    "Écrit le nom de chaque groupe en haut à gauche de son diagramme (title = TRUE).",
    OUI_NON_AUTO, modeles = "multigroupe"),
  # ── Multigroupe ──
  A("panelGroups", "Multigroupe", "check", "Un panneau par groupe", FALSE,
    "Dessine les diagrammes des groupes côte à côte dans une même figure. Sinon, semPaths en produit un par groupe, l'un après l'autre.",
    modeles = "multigroupe"),
  A("combineGroups", "Multigroupe", "check", "Combiner les groupes", FALSE,
    "Superpose les groupes en un seul diagramme : utile pour montrer la structure commune, mais les estimations propres à chaque groupe se confondent.",
    modeles = "multigroupe")
)
names(ARGS) <- vapply(ARGS, `[[`, "", "arg")
GROUPES <- unique(vapply(ARGS, `[[`, "", "groupe"))
id_arg <- function(a) paste0("a_", gsub(".", "_", a, fixed = TRUE))

# Valeurs par défaut sous forme d'entrées Shiny
defaut_input <- function(a) ARGS[[a]]$defaut
DEFAUTS <- setNames(lapply(names(ARGS), defaut_input), names(ARGS))

# ── 3. Préréglages ────────────────────────────────────────────────────────
PRESETS <- list(
  defaut = list(titre = "Défauts de semPaths", val = list()),
  pedago = list(titre = "Lecture pédagogique",
    val = list(what = "std", whatLabels = "std", style = "lisrel", nCharNodes = 0, edge.label.cex = 1,
               sizeMan = 7, sizeLat = 10, residScale = 10, fade = FALSE, rotation = "2", label.cex = 1.2)),
  publication = list(titre = "Publication (noir et blanc)",
    val = list(what = "paths", whatLabels = "std", style = "lisrel", nCharNodes = 0, edge.color = "#000000",
               color.lat = "#FFFFFF", color.man = "#FFFFFF", edge.label.cex = 0.9, sizeMan = 7, sizeLat = 10,
               fade = FALSE, residScale = 10)),
  compact = list(titre = "Compact",
    val = list(residuals = FALSE, intercepts = FALSE, thresholds = FALSE, whatLabels = "hide", layout = "tree2",
               sizeMan = 4, sizeLat = 6, exoCov = FALSE))
)

# ── 4. Des entrées à l'appel ──────────────────────────────────────────────
# valeurs : liste nommée (nom d'argument -> valeur d'entrée Shiny).
# Retourne la liste des arguments NON par défaut, convertis pour semPaths.
convertir <- function(a, v) {
  x <- ARGS[[a]]
  if (is.null(v)) return(NULL)
  if (x$type == "check") return(isTRUE(v))
  if (x$type %in% c("slider") || isTRUE(x$num)) return(as.numeric(v))
  if (identical(v, "TRUE")) return(TRUE)
  if (identical(v, "FALSE")) return(FALSE)
  v
}
est_defaut <- function(a, v, valeurs = list()) {
  x <- ARGS[[a]]
  if (is.null(v)) return(TRUE)
  if (!is.null(x$lie)) {   # défaut qui suit un autre argument
    ref <- valeurs[[x$lie]]; if (is.null(ref)) ref <- ARGS[[x$lie]]$defaut
    return(isTRUE(all.equal(as.numeric(v), as.numeric(ref))))
  }
  if (identical(v, "auto")) return(TRUE)
  if (x$type == "check") return(isTRUE(v) == isTRUE(x$defaut))
  if (isTRUE(x$num) || x$type == "slider") return(isTRUE(all.equal(as.numeric(v), as.numeric(x$defaut))))
  identical(as.character(v), as.character(x$defaut))
}
args_modifies <- function(valeurs) {
  out <- list()
  for (a in names(ARGS)) {
    v <- valeurs[[a]]
    if (!est_defaut(a, v, valeurs)) out[[a]] <- convertir(a, v)
  }
  # couleurs des nœuds : un seul argument color = list(...)
  cols <- list()
  if (!is.null(out$color.lat)) { cols$lat <- out$color.lat; out$color.lat <- NULL }
  if (!is.null(out$color.man)) { cols$man <- out$color.man; out$color.man <- NULL }
  if (length(cols)) {   # semPaths attend les deux couleurs ; blanc = son défaut
    out$color <- list(lat = if (is.null(cols$lat)) "#FFFFFF" else cols$lat,
                      man = if (is.null(cols$man)) "#FFFFFF" else cols$man)
  }
  # measurementLayout n'a de sens qu'avec layoutSplit
  if (!isTRUE(out$layoutSplit)) out$measurementLayout <- NULL
  out
}

# ── 5. Dessin ─────────────────────────────────────────────────────────────
dessiner <- function(cle, args) {
  fit <- AJUSTEMENTS[[cle]]
  ng <- lavaan::lavInspect(fit, "ngroups")
  if (ng > 1 && !isTRUE(args$panelGroups) && !isTRUE(args$combineGroups)) {
    op <- par(mfrow = c(1, ng)); on.exit(par(op))
  }
  do.call(semPlot::semPaths, c(list(fit), args, list(ask = FALSE)))
  invisible(NULL)
}

# ── 6. Code R reproductible ───────────────────────────────────────────────
deparse_val <- function(v) {
  if (is.list(v)) return(sprintf("list(%s)", paste(sprintf("%s = %s", names(v), vapply(v, deparse_val, "")), collapse = ", ")))
  if (is.character(v)) return(sprintf("\"%s\"", v))
  if (is.logical(v)) return(if (v) "TRUE" else "FALSE")
  format(v, trim = TRUE)
}
code_r <- function(cle, args, dernier = NULL) {
  m <- MODELES[[cle]]
  ng <- lavaan::lavInspect(AJUSTEMENTS[[cle]], "ngroups")
  lignes <- if (length(args)) {
    noms <- names(args)
    vapply(seq_along(args), function(i) {
      n <- noms[i]
      s <- sprintf("  %s = %s", n, deparse_val(args[[i]]))
      if (i < length(args)) s <- paste0(s, ",")
      marque <- !is.null(dernier) && (n == dernier || (n == "color" && dernier %in% c("color.lat", "color.man")))
      if (marque) s <- paste0(s, "   # <- dernier argument modifié")
      s
    }, "")
  } else character(0)
  appel <- if (length(lignes)) c("semPaths(ajustement,", lignes, ")") else "semPaths(ajustement)"
  if (ng > 1 && !isTRUE(args$panelGroups) && !isTRUE(args$combineGroups))
    appel <- c(sprintf("par(mfrow = c(1, %d))   # un diagramme par groupe, côte à côte", ng), appel)
  c("library(lavaan)", "library(semPlot)", "", m$code, "", appel)
}
