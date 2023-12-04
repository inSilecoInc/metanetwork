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
      h3(div("Diagramme d'accord")),
      # Panels
      tabsetPanel(
        id = "params",
        # INPUT UIs
        tabPanel("Configuration",
          icon = icon("gear"),
          hr(),
          h4("Fichiers sources"),
          fileInput("nodes", "Noeuds", accept = ".csv"),
          fileInput("links", "Liens", accept = ".csv"),
          h4("Paramètres généraux"),
          numericInput("textSize", "Taille de la police", value = 1, step = 0.05),
          checkboxInput("shadowNode", "Ombragement des noeuds", value = TRUE),
          hr(),
          h4("Largeur des bars de sous-réseaux"),
          numericInput("rad1", "Limite inférieure", value = 0.95, step = 0.05),
          numericInput("rad2", "Limite supérieur", value = 1, step = 0.05),
          hr(),
          h4("Exporter la figure"),
          textInput("filename", "Nom du fichier", "metanetwork.png"),
          numericInput("res", "Resolution", value = 300, step = 10),
          numericInput("img_size", "Taille de l'image", value = 200, step = 10),
          checkboxInput("legend", "Afficher la légende", value = TRUE),
          downloadButton('downloadFig', 'Télécharger la figure')
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
          #colourpicker::colourInput("colLink", "Couleur des liens", "#876b40"),
          numericInput("linkWidth", "Taille des liens", value = 1, step = 0.05),
          selectInput("focus", 
            "Groupe de liens mis en évidence", 
            choices = grp_foc
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
        color = "#37abc8",
        imageOutput("metanetwork")
      )
    )
  )
)