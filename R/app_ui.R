#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd

app_ui <- function(request) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
    ),

    # Application title
    titlePanel("", "metanetwork"),

    #
    sidebarLayout(
      sidebarPanel(
        # Title
        h2("Diagramme \u00e0 cordes"),
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
            actionButton("reloadDataButton",
              icon = icon("refresh"),
              "Recharger les donn\u00e9es"
            ),
            uiOutput("gsLink"),
            helpText("Remarque\u00a0: la feuille de calcul
          Google Drive doit \u00eatre visible
          par toute personne disposant du lien\u00a0;
          pour avoir une feuille de calcul bien structur\u00e9e,
          le plus simple est de copier le document exemple:
           'Fichier' -> 'Cr\u00e9er une copie'"),
            hr(),
            h4("Param\u00e8tres g\u00e9n\u00e9raux"),
            numericInput("textSize", "Taille de la police",
              value = 1, step = 0.05
            ),
            checkboxInput("shadowNode", "Ombragement des noeuds", value = TRUE),
            hr(),
            h4("Largeur des barres de sous-r\u00e9seaux"),
            numericInput("rad1", "Limite inf\u00e9rieure", value = 0.95, step = 0.05),
            numericInput("rad2", "Limite sup\u00e9rieur", value = 1, step = 0.05),
            hr(),
            h4("Exporter la figure"),
            textInput("filename", "Nom du fichier", "metanetwork.png"),
            numericInput("res", "Resolution", value = 300, step = 10),
            numericInput("img_size", "Taille de l'image", value = 200, step = 10),
            checkboxInput("legend", "Afficher la l\u00e9gende", value = TRUE),
            downloadButton("downloadFig", "T\u00e9l\u00e9charger la figure")
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
              "Groupe de liens mis en \u00e9vidence",
              choices = c("All")
            )
          )
        ),
        hr(),
        # FOOTER
        HTML(
          paste0(
            "<div id='footer_left'><a href='https://insilecoinc.github.io/' target='_blank'><img src='www/img/insileco_logo256.png' alt='inSileco logo' width = '100%'/></a></div><div id='footer_right'><h5>This shiny app was built by <a href='https://insilecoinc.github.io/' target='_blank'>inSileco</a> with the ",
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

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Metanetwork"
    )
  )
}
