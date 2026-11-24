# ═══════════════════════════════════════════════════════════════════════════
#  SEMPlot — l'atelier des arguments de semPaths()                    app.R
#  On manipule un argument, le diagramme et le code R suivent.
# ═══════════════════════════════════════════════════════════════════════════
library(shiny)
library(bslib)
source("global.R", local = FALSE, encoding = "UTF-8")

# ── Contrôles générés à partir du dictionnaire ───────────────────────────
etiquette <- function(x) tags$span(tags$code(class = "argnom", x$arg), " ", x$libelle)
controle <- function(x) {
  id <- id_arg(x$arg); lab <- etiquette(x)
  w <- switch(x$type,
    select = selectInput(id, lab, choices = x$choix, selected = x$defaut, width = "100%"),
    check  = checkboxInput(id, lab, value = isTRUE(x$defaut), width = "100%"),
    slider = sliderInput(id, lab, min = x$min, max = x$max, value = x$defaut, step = x$pas, width = "100%"))
  div(class = "ctrl", `data-arg` = x$arg, w)
}
panneau_groupe <- function(g) {
  xs <- Filter(function(x) x$groupe == g, ARGS)
  accordion_panel(g, value = g, icon = icon(switch(g, Contenu = "eye", Disposition = "sitemap", `Flèches` = "arrow-right",
                                                   `Nœuds` = "shapes", Couleurs = "palette", Multigroupe = "users")),
                  lapply(xs, controle))
}

css <- "
body { font-size: 1.08rem; }
.argnom { font-size: .92em; color: #1F4E79; background: #eef4fa; padding: 0 .25em; border-radius: 3px; }
.ctrl.inapplicable { opacity: .45; }
.ctrl.inapplicable .argnom::after { content: ' — sans effet ici'; font-family: inherit; color: #888; font-size: .85em; }
.ctrl.modifie .argnom { background: #fff3cd; color: #7a4b00; font-weight: 600; }
#code_r { font-size: .95rem; background: #f7f7f9; border: 1px solid #e3e3e8; border-radius: 6px; padding: .8rem;
          white-space: pre; overflow-x: auto; margin: 0; }
#code_r .marque { background: #fff3cd; display: inline-block; width: 100%; }
.explic h5 code { font-size: 1.05em; }
.valeurs { font-family: monospace; background: #f7f7f9; padding: .3rem .6rem; border-radius: 5px; display: inline-block; }
.preset-btn { margin: 0 .3rem .4rem 0; }
.table-aide td, .table-aide th { vertical-align: top; }
.accordion-button { font-size: 1.05rem; font-weight: 600; }
/* barre latérale : défile seule et reste à l'écran, quelle que soit la longueur des familles ouvertes */
.bslib-sidebar-layout > .sidebar { position: sticky; top: 4.5rem; align-self: start;
  max-height: calc(100vh - 5.5rem); overflow-y: auto; }
.bslib-sidebar-layout > .main { align-self: start; }
"

js <- "
Shiny.addCustomMessageHandler('applicable', function(m) {
  document.querySelectorAll('.ctrl').forEach(function(el) {
    el.classList.toggle('inapplicable', m.inapplicables.indexOf(el.dataset.arg) >= 0);
    el.classList.toggle('modifie', m.modifies.indexOf(el.dataset.arg) >= 0);
  });
});
function copierCode() {
  var t = document.getElementById('code_r').innerText;
  navigator.clipboard.writeText(t).then(function() {
    var b = document.getElementById('copier'); var o = b.innerHTML; b.innerHTML = '✓ Copié';
    setTimeout(function() { b.innerHTML = o; }, 1500); });
}
"

ui <- page_navbar(
  title = "semPlot — l'atelier des arguments",
  id = "nav", fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font_size = "1.05rem",
                   primary = "#1F4E79"),
  header = tags$head(tags$style(HTML(css)), tags$script(HTML(js))),

  nav_panel("Introduction", value = "intro",
    div(style = "max-width: 1000px; margin: 1rem auto;",
      h3("Dessiner un modèle avec semPaths()"),
      p("La fonction ", code("semPaths()"), " du paquet ", strong("semPlot"), " dessine le diagramme d'un modèle
         estimé avec lavaan. Elle compte plus de 70 arguments, et les meilleurs diagrammes demandent d'en régler
         une dizaine. Cette application les montre à l'œuvre : ", strong("vous changez un argument, le diagramme et
         le code R se mettent à jour"), ", et un encadré explique ce que l'argument vient de faire."),
      h4("Comment travailler"),
      tags$ol(
        tags$li("Dans l'onglet ", strong("Atelier"), ", choisissez un modèle : chemins sur variables observées,
                 analyse factorielle confirmatoire, équations structurelles, multigroupe, croissance, indicateurs
                 ordinaux ou bifacteur."),
        tags$li("Ouvrez une famille d'arguments dans la barre latérale (Contenu, Disposition, Flèches, Nœuds,
                 Couleurs, Multigroupe) et modifiez-en un. Son nom R est écrit en bleu à côté de son libellé."),
        tags$li("Le diagramme change aussitôt. Cochez ", strong("Comparer avant / après"), " pour voir côte à côte
                 le diagramme précédent et le nouveau."),
        tags$li("Le ", strong("code R"), " sous le diagramme est complet et reproductible : chargement des paquets,
                 modèle, estimation, puis l'appel à semPaths() avec ", em("seulement"), " les arguments que vous
                 avez changés. Le dernier argument modifié y est surligné. Copiez-le ou téléchargez-le."),
        tags$li("Les ", strong("préréglages"), " appliquent d'un coup une combinaison utile (lecture pédagogique,
                 publication en noir et blanc, version compacte). ", strong("Réinitialiser"), " revient aux défauts
                 de semPaths()."),
        tags$li("L'onglet ", strong("Aide-mémoire"), " résume tous les arguments de l'atelier, avec leur valeur par
                 défaut.")),
      h4("Lire un diagramme d'équations structurelles"),
      tags$table(class = "table table-sm", style = "max-width: 820px",
        tags$thead(tags$tr(tags$th("Symbole"), tags$th("Signification"))),
        tags$tbody(
          tags$tr(tags$td("Rectangle ou carré"), tags$td("Variable observée (mesurée)")),
          tags$tr(tags$td("Cercle ou ellipse"), tags$td("Variable latente (facteur non observé)")),
          tags$tr(tags$td("Triangle"), tags$td("Constante : moyenne ou ordonnée à l'origine")),
          tags$tr(tags$td("Flèche simple"), tags$td("Effet supposé causal, ou saturation d'un indicateur sur son facteur")),
          tags$tr(tags$td("Flèche double courbe"), tags$td("Covariance (ou corrélation) non expliquée par le modèle")),
          tags$tr(tags$td("Boucle sur une variable, ou petite flèche entrante"), tags$td("Variance résiduelle (erreur) ou variance d'une exogène")),
          tags$tr(tags$td("Trait en tirets"), tags$td("Paramètre fixé (par exemple une saturation fixée à 1), et non estimé")),
          tags$tr(tags$td("Épaisseur et couleur (avec what = « est » ou « std »)"), tags$td("Force et signe de l'estimation : vert positif, rouge négatif")))),
      h4("Documentation"),
      tags$ul(
        tags$li(a(href = "https://cran.r-project.org/package=semPlot", target = "_blank", "Page CRAN de semPlot (manuel de référence)")),
        tags$li(a(href = "https://github.com/SachaEpskamp/semPlot", target = "_blank", "Dépôt GitHub de semPlot")),
        tags$li(a(href = "https://cran.r-project.org/package=qgraph", target = "_blank", "Paquet qgraph"), " : semPaths() lui transmet
                 les arguments qu'il ne connaît pas (", code("label.cex"), ", ", code("edge.width"), ", ", code("fade"), "…)."),
        tags$li(a(href = "https://lavaan.ugent.be/", target = "_blank", "Site de lavaan"))))),

  nav_panel("Atelier", value = "atelier",
    layout_sidebar(
      sidebar = sidebar(width = 430, open = "always",
        selectInput("modele", "Modèle", choices = CHOIX_MODELES, selected = "afc", width = "100%"),
        uiOutput("desc_modele"),
        div(strong("Préréglages")),
        div(lapply(names(PRESETS), function(k)
          actionButton(paste0("preset_", k), PRESETS[[k]]$titre, class = "btn-sm btn-outline-primary preset-btn"))),
        actionButton("reinit", "Réinitialiser (défauts de semPaths)", icon = icon("rotate-left"),
                     class = "btn-sm btn-outline-secondary w-100"),
        hr(style = "margin:.4rem 0"),
        accordion(id = "acc", open = "Contenu", multiple = FALSE, !!!lapply(GROUPES, panneau_groupe))),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("Diagramme"),
          div(class = "d-flex align-items-center gap-3 flex-wrap",
            div(style = "margin-bottom:-1rem", checkboxInput("comparer", "Comparer avant / après", FALSE)),
            downloadButton("dl_png", "PNG", class = "btn-sm btn-outline-primary"),
            downloadButton("dl_pdf", "PDF", class = "btn-sm btn-outline-primary"),
            downloadButton("dl_svg", "SVG", class = "btn-sm btn-outline-primary"))),
        conditionalPanel("!input.comparer", plotOutput("diagramme", height = "640px")),
        conditionalPanel("input.comparer",
          layout_columns(col_widths = c(6, 6),
            div(h6(class = "text-center text-muted", "Avant"), plotOutput("avant", height = "560px")),
            div(h6(class = "text-center", strong("Après")), plotOutput("apres", height = "560px"))))),
      layout_columns(col_widths = c(7, 5),
        card(
          card_header(class = "d-flex justify-content-between align-items-center",
            span("Code R reproductible"),
            div(class = "d-flex gap-2",
              tags$button(id = "copier", class = "btn btn-sm btn-outline-primary", onclick = "copierCode()",
                          icon("copy"), " Copier"),
              downloadButton("dl_code", ".R", class = "btn-sm btn-outline-primary"))),
          uiOutput("code")),
        card(card_header("Ce que vous venez de modifier"), uiOutput("explication"))))),

  nav_panel("Aide-mémoire", value = "aide",
    div(style = "max-width: 1200px; margin: 1rem auto;",
      h3("Les arguments de l'atelier"),
      p("Défaut = valeur utilisée par semPaths() quand l'argument est omis. « automatique » : semPaths() choisit
         selon le modèle. Les arguments marqués ", em("qgraph"), " ne sont pas propres à semPaths() : il les
         transmet à la fonction qgraph() qui dessine le graphe."),
      uiOutput("aide"))),
  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "dcstats.org"))
)

server <- function(input, output, session) {

  # ── Valeurs courantes des arguments ──
  valeurs <- reactive(setNames(lapply(names(ARGS), function(a) input[[id_arg(a)]]), names(ARGS)))
  args_cour <- reactive(args_modifies(valeurs()))

  etat <- reactiveVal(DEFAUTS)        # dernières valeurs connues
  avant <- reactiveVal(list())        # arguments de l'appel précédent
  dernier <- reactiveVal(NULL)        # nom du dernier argument modifié
  ancien_val <- reactiveVal(NULL)     # sa valeur précédente
  en_lot <- reactiveVal(NULL)         # titre d'un préréglage en cours d'application
  lot_affiche <- reactiveVal(NULL)    # titre du dernier préréglage appliqué

  observeEvent(valeurs(), {
    v <- valeurs(); old <- etat()
    if (any(vapply(v, is.null, TRUE))) return()
    chg <- names(v)[!mapply(function(a, b) identical(as.character(a), as.character(b)), v, old[names(v)])]
    if (!length(chg)) return()
    lot <- en_lot()
    # changements qui ne font que suivre sizeMan : on garde l'explication de sizeMan
    if (is.null(lot) && identical(dernier(), "sizeMan") && all(chg %in% c("sizeMan2", "residScale"))) { etat(v); return() }
    avant(args_modifies(old))
    if (!is.null(lot)) { lot_affiche(lot); dernier(NULL); en_lot(NULL) }
    else if (length(chg) == 1) { dernier(chg); ancien_val(old[[chg]]); lot_affiche(NULL) }
    else dernier(NULL)
    # dimensions liées : sizeMan2 et residScale suivent sizeMan s'ils étaient à leur défaut
    if ("sizeMan" %in% chg && is.null(lot)) {
      for (b in c("sizeMan2", "residScale"))
        if (isTRUE(all.equal(as.numeric(old[[b]]), as.numeric(old$sizeMan))))
          updateSliderInput(session, id_arg(b), value = v$sizeMan)
    }
    etat(v)
  }, ignoreInit = TRUE)

  # ── Applicabilité et surlignage dans la barre latérale ──
  observe({
    m <- input$modele; mods <- names(args_modifies(valeurs()))
    if ("color" %in% mods) mods <- c(mods, "color.lat", "color.man")
    inap <- names(ARGS)[vapply(ARGS, function(x) !is.null(x$modeles) && !(m %in% x$modeles), TRUE)]
    session$sendCustomMessage("applicable", list(inapplicables = as.list(inap), modifies = as.list(mods)))
  })

  output$desc_modele <- renderUI(p(class = "small text-muted", MODELES[[input$modele]]$description))

  # ── Préréglages et réinitialisation ──
  maj <- function(a, v) {
    x <- ARGS[[a]]; id <- id_arg(a)
    switch(x$type,
      select = updateSelectInput(session, id, selected = v),
      check  = updateCheckboxInput(session, id, value = isTRUE(v)),
      slider = updateSliderInput(session, id, value = v))
  }
  appliquer <- function(vals, titre) {
    for (a in names(ARGS)) if (!is.null(ARGS[[a]]$lie) && is.null(vals[[a]]))   # dimensions liées
      vals[[a]] <- if (!is.null(vals[[ARGS[[a]]$lie]])) vals[[ARGS[[a]]$lie]] else DEFAUTS[[ARGS[[a]]$lie]]
    a_changer <- Filter(function(a) {
      v <- if (!is.null(vals[[a]])) vals[[a]] else DEFAUTS[[a]]
      !identical(as.character(input[[id_arg(a)]]), as.character(v)) }, names(ARGS))
    if (!length(a_changer)) return()
    en_lot(titre)
    for (a in a_changer) maj(a, if (!is.null(vals[[a]])) vals[[a]] else DEFAUTS[[a]])
  }
  lapply(names(PRESETS), function(k) observeEvent(input[[paste0("preset_", k)]],
    appliquer(PRESETS[[k]]$val, PRESETS[[k]]$titre)))
  observeEvent(input$reinit, appliquer(list(), "Réinitialisation"))

  # ── Dessin ──
  trace <- function(args) {
    r <- tryCatch({ dessiner(input$modele, args); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(r)) {
      plot.new()
      text(.5, .6, "semPaths() ne peut pas dessiner cette combinaison d'arguments\npour ce modèle.", cex = 1.3, col = "#8E2C2C")
      text(.5, .4, paste("Message de semPlot :", r), cex = .95, col = "#555555")
      text(.5, .28, "C'est une limite du paquet : revenez à la valeur précédente.", cex = .95, col = "#555555")
    }
  }
  output$diagramme <- renderPlot(trace(args_cour()), res = 96)
  output$apres <- renderPlot(trace(args_cour()), res = 96)
  output$avant <- renderPlot(trace(avant()), res = 96)

  # ── Code R ──
  code_txt <- reactive(code_r(input$modele, args_cour(), dernier()))
  output$code <- renderUI({
    l <- htmltools::htmlEscape(code_txt())
    l <- ifelse(grepl("dernier argument modifié", l), sprintf("<span class='marque'>%s</span>", l), l)
    tags$pre(id = "code_r", HTML(paste(l, collapse = "\n")))
  })

  # ── Explication du dernier argument ──
  fmt_val <- function(a, v) {
    x <- ARGS[[a]]
    if (is.null(v)) return("—")
    if (x$type == "check") return(if (isTRUE(v)) "TRUE" else "FALSE")
    if (x$type == "select") { n <- names(x$choix)[match(as.character(v), x$choix)]; if (!is.na(n)) return(n) }
    as.character(v)
  }
  output$explication <- renderUI({
    a <- dernier()
    if (is.null(a)) {
      if (identical(lot_affiche(), "Réinitialisation")) return(div(class = "explic",
        h5("Réinitialisation"), p("Tous les arguments sont revenus aux valeurs par défaut de semPaths() : l'appel ne
           contient plus que l'objet ajusté, semPaths(ajustement).")))
      if (!is.null(lot_affiche())) return(div(class = "explic",
        h5(paste("Préréglage :", lot_affiche())), p("Plusieurs arguments ont changé d'un coup : ils sont surlignés en jaune dans la barre
           latérale et figurent tous dans le code R. Modifiez-en un pour voir son effet propre.")))
      return(div(class = "explic", p(class = "text-muted", "Modifiez un argument dans la barre latérale : son rôle
        et son effet s'afficheront ici.")))
    }
    x <- ARGS[[a]]; nv <- valeurs()[[a]]
    nom_r <- if (a %in% c("color.lat", "color.man")) sprintf("color = list(%s = …)", sub("color.", "", a, fixed = TRUE)) else a
    note <- if (!is.null(x$modeles) && !(input$modele %in% x$modeles))
      div(class = "alert alert-warning small", "Cet argument n'a pas d'effet visible sur ce modèle. Essayez-le avec : ",
          paste(names(CHOIX_MODELES)[CHOIX_MODELES %in% x$modeles], collapse = " ; "), ".")
    defaut <- if (est_defaut(a, nv, valeurs())) p(class = "small text-muted", "C'est la valeur par défaut : l'argument
        disparaît du code R, semPaths() l'applique de lui-même.")
    div(class = "explic",
      h5(code(nom_r), " — ", x$libelle),
      div(class = "valeurs", fmt_val(a, ancien_val()), " → ", strong(fmt_val(a, nv))),
      p(style = "margin-top:.6rem", x$aide),
      if (isTRUE(x$qgraph)) p(class = "small text-muted", "Argument de qgraph(), transmis par semPaths()."),
      defaut, note)
  })

  # ── Téléchargements ──
  nom_f <- function(ext) function() sprintf("diagramme_%s.%s", input$modele, ext)
  output$dl_png <- downloadHandler(nom_f("png"), function(f) {
    png(f, width = 2400, height = 1600, res = 220); on.exit(dev.off()); trace(args_cour()) })
  output$dl_pdf <- downloadHandler(nom_f("pdf"), function(f) {
    pdf(f, width = 11, height = 7.5); on.exit(dev.off()); trace(args_cour()) })
  output$dl_svg <- downloadHandler(nom_f("svg"), function(f) {
    svg(f, width = 11, height = 7.5); on.exit(dev.off()); trace(args_cour()) })
  output$dl_code <- downloadHandler(function() sprintf("semPaths_%s.R", input$modele),
    function(f) writeLines(sub("   # <- dernier argument modifié", "", code_txt(), fixed = TRUE), f, useBytes = TRUE))

  # ── Aide-mémoire ──
  output$aide <- renderUI({
    lapply(GROUPES, function(g) {
      xs <- Filter(function(x) x$groupe == g, ARGS)
      tagList(h4(g), tags$table(class = "table table-sm table-aide",
        tags$thead(tags$tr(tags$th("Argument"), tags$th("Rôle"), tags$th("Défaut"), tags$th("Valeurs dans l'atelier"))),
        tags$tbody(lapply(xs, function(x) {
          dft <- if (identical(x$defaut, "auto")) "automatique" else if (x$type == "check") (if (isTRUE(x$defaut)) "TRUE" else "FALSE") else as.character(x$defaut)
          if (!is.null(x$lie)) dft <- sprintf("= %s", x$lie)
          vals <- switch(x$type, check = "TRUE, FALSE",
            slider = sprintf("%s à %s", format(x$min), format(x$max)),
            select = paste(setdiff(unname(x$choix), "auto"), collapse = ", "))
          tags$tr(tags$td(code(if (x$arg %in% c("color.lat", "color.man")) sprintf("color$%s", sub("color.", "", x$arg, fixed = TRUE)) else x$arg),
                          if (isTRUE(x$qgraph)) tags$div(class = "small text-muted", "qgraph")),
                  tags$td(strong(x$libelle), br(), span(class = "small", x$aide)),
                  tags$td(code(dft)), tags$td(class = "small", vals))
        }))))
    })
  })
}

shinyApp(ui, server)
