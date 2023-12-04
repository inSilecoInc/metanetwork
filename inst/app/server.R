server <- function(input, output, session) {
  # INPUTS
  nodes <- system.file("extdata", "nodes_wo_col.csv", package = "metanetwork") |>
    read.csv()
  links <- system.file("extdata", "links.csv", package = "metanetwork") |>
    read.csv()

  output$downloadFig <- shiny::downloadHandler(
    filename = function() input$filename,
    contentType = "image/png",
    content = function(file) file.copy("www/img/export_fig.png", file)
  )

  ## Enable option based on columns names source file
  shiny::observeEvent(nodes, {
    if ("col" %in% names(nodes)) {
        shinyjs::disable("colNode")
    } else {
        shinyjs::enable("colNode")
    }
    if ("size" %in% names(nodes)) {
        shinyjs::disable("nodeSize")
    } else {
        shinyjs::enable("nodeSize")
    }
  })

  output$metanetwork <- shiny::renderImage({
      # Generate the PNG
      metanetwork(
        nodes,
        links,
        colNode = choose_pal(input$colNode),
        nodeSize = input$nodeSize,
        colLink = "#876b40",
        linkWidth = input$linkWidth,
        textSize = input$textSize,
        rad1 = input$rad1,
        rad2 = input$rad2,
        focus = NULL,
        shadowNode = input$shadowNode,
        shadowLink = "#f4f4f4",
        export = TRUE,
        filename = "www/img/export_fig.png",
        res = input$res,
        img_size = input$img_size,
        legend = input$legend
      )

      # Return a list containing the filename
      list(src = "www/img/export_fig.png",
          contentType = 'image/png',
          width = "60%",
          style = "display: block; margin-left: auto; margin-right: auto;",
          alt = "This is alternate text")
    }, deleteFile = FALSE)

}
