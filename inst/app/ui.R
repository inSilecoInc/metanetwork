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
      h3(div("inSileco Metanetwork App")),

      # Panels
      tabsetPanel(
        id = "slc_mode",
        tabsetPanel(
          id = "params",
          # INPUT UIs
          tabPanel("params",
            icon = icon("user-check"),
            numericInput("line_witdh", "Line width", value = 1, step = 0.05)
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
      tabsetPanel(
        id = "view_tabs",
        tabPanel(
          "Figure",
          icon = icon("chart-bar")
        )
      )
    )
  )
)