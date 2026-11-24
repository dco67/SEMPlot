# SEMPlot — l'atelier des arguments de semPaths()

**SEMPlot** est une application Shiny pédagogique, en français, pour apprendre à dessiner des modèles d'équations structurelles avec la fonction `semPaths()` du paquet R **semPlot**.

- **Application en ligne :** <https://shiny.dcstats.org/SEMPlot/>

## Le principe

`semPaths()` compte plus de 70 arguments, et un bon diagramme demande d'en régler une dizaine. Dans l'atelier, **on change un argument, le diagramme et le code R se mettent à jour**, et un encadré explique ce que l'argument vient de faire.

## Contenu

- **Sept modèles d'exemple**, sur de vrais jeux de données :
  - chemins sur variables observées;
  - analyse factorielle confirmatoire (Holzinger et Swineford);
  - équations structurelles (démocratisation politique de Bollen);
  - multigroupe;
  - croissance latente;
  - indicateurs ordinaux (seuils);
  - bifacteur.
- **48 arguments** de `semPaths()` et de qgraph, en six familles : Contenu, Disposition, Flèches, Nœuds, Couleurs, Multigroupe.
  - Chacun a un libellé et des valeurs expliqués en français.
  - Les arguments sans effet sur le modèle choisi sont grisés.
- **Code R complet et reproductible** : paquets, modèle, estimation, puis `semPaths()` avec seulement les arguments modifiés. Le dernier argument changé est surligné. On peut copier le code ou le télécharger en `.R`.
- **Explication de l'argument modifié** : l'ancienne et la nouvelle valeur, son rôle, et la mention d'un argument transmis à qgraph.
- **Comparaison avant / après** : les deux diagrammes côte à côte.
- **Préréglages** : défauts de semPaths, lecture pédagogique, publication en noir et blanc, compact. Un bouton permet de réinitialiser.
- **Téléchargement du diagramme** en PNG, PDF ou SVG.
- **Aide-mémoire** de tous les arguments de l'atelier, avec leur valeur par défaut, et une introduction à la lecture des diagrammes SEM.

## Exécuter l'application localement

```r
install.packages(c("shiny", "bslib", "lavaan", "semPlot"))
shiny::runApp("SEMPlot")
```

Le dossier contient `app.R` (interface et serveur) et `global.R` (modèles d'exemple, dictionnaire des arguments, construction de l'appel et du code R).

## Documentation

- [semPlot sur CRAN](https://cran.r-project.org/package=semPlot) · [dépôt GitHub de semPlot](https://github.com/SachaEpskamp/semPlot)
- [qgraph sur CRAN](https://cran.r-project.org/package=qgraph)
- [lavaan](https://lavaan.ugent.be/)

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
