ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),

  # Application title
  titlePanel("", "metanetwork"),

  #
  sidebarLayout(
    sidebarPanel(
      # Title
      h2("Diagramme à cordes"),
      # Panels
      tabsetPanel(
        id = "params",
        # INPUT UIs
        tabPanel("Configuration",
          icon = icon("gear"),
          hr(),
          h4("Fichiers sources"),
          textInput(
            inputId = "googledrive_link",
            label = "Lien vers le classeur Google Drive",
            value = "https://docs.google.com/spreadsheets/d/15xejjdLDYrjSbuO4Ufb83x38vBASi7bqTDF0svO22Uc/edit?usp=sharing"
          ),
          helpText("Remarque : la feuille de calcul
          Google Drive doit être visible
          par toute personne disposant du lien ;
          pour avoir une feuille de calcul bien structurée,
          le plus simple est de copier le document exemple:
           'Fichier' -> 'Créer une copie'"),
          actionButton("reloadDataButton",
            icon = icon("refresh"),
            "Recharger les données"
          ),
          actionButton("hyperlink", "Se rendre sur la feuille Google",
            icon = icon("th")
          ),
          hr(),
          h4("Paramètres généraux"),
          numericInput("textSize", "Taille de la police",
            value = 1, step = 0.05
          ),
          checkboxInput("shadowNode", "Ombragement des noeuds", value = TRUE),
          hr(),
          h4("Largeur des barres de sous-réseaux"),
          numericInput("rad1", "Limite inférieure", value = 0.95, step = 0.05),
          numericInput("rad2", "Limite supérieur", value = 1, step = 0.05),
          hr(),
          h4("Exporter la figure"),
          textInput("filename", "Nom du fichier", "metanetwork.png"),
          numericInput("res", "Resolution", value = 300, step = 10),
          numericInput("img_size", "Taille de l'image", value = 200, step = 10),
          checkboxInput("legend", "Afficher la légende", value = TRUE),
          downloadButton("downloadFig", "Télécharger la figure")
        ),
        tabPanel("Noeuds",
          icon = icon("gear"),
          hr(),
          numericInput("nodeSize", "Taille des noeuds", value = 1, step = 0.05),
          selectInput("colNode", "Palette de couleurs", nodeColPal)
        ),
        tabPanel("Liens",
          icon = icon("gear"),
          hr(),
          # colourpicker::colourInput("colLink", "Couleur des liens", "#876b40"),
          numericInput("linkWidth", "Taille des liens", value = 1, step = 0.05),
          selectInput("focus",
            "Groupe de liens mis en évidence",
            choices = c("All")
          )
        )
      ),
      hr(),
      # FOOTER
      HTML(
        paste0(
          "<div id='footer_left'><a href='https://insilecoinc.github.io/' target='_blank'><img src='img/insileco_logo256.png' alt='inSileco logo' width = '100%'/></a></div><div id='footer_right'><h5>This shiny app was built by <a href='https://insilecoinc.github.io/' target='_blank'>inSileco</a> with the ",
          a("R package shiny",
            href = "https://shiny.rstudio.com/",
            target = "_blank"
          ), ", the source code is available on "
        )
      ),
      a(icon("github"),
        href = "https://github.com/inSilecoInc/metanetwork", target = "_blank"
      ),
      HTML(".</h5></div>")
    ),
    # RIGHT PANEL
    mainPanel(
      shinycssloaders::withSpinner(
        imageOutput("metanetwork")
      )
    )
  )
)
