server <- function(input, output, session) {
  
  # INPUTS
  r <- reactiveValues()

  observeEvent(input$googledrive_link,{
    r$nodes <- googlesheets4::read_sheet(ss = input$googledrive_link,
                            sheet = "noeuds",
                            col_names = TRUE,
                            col_types = "ccccd")
    r$links <- googlesheets4::read_sheet(ss = input$googledrive_link,
                            sheet = "liens",
                            col_names = TRUE,
                            col_types = "cccd")

    grp_foc <- c(list(All = "all"), as.list(unique(r$nodes$subnetwork)))
    names(grp_foc)[-1] <- unlist(grp_foc[-1])
    updateSelectInput(session, "focus", choices = grp_foc)
  })

  # OUTPUTS
  output$downloadFig <- shiny::downloadHandler(
    filename = function() input$filename,
    contentType = "image/png",
    content = function(file) file.copy("www/img/export_fig.png", file)
  )

  output$metanetwork <- shiny::renderImage(
    {
      # Generate the PNG
      metanetwork(
        r$nodes,
        r$links,
        colNode = choose_pal(input$colNode),
        nodeSize = input$nodeSize,
        colLink = "#876b40",#input$colLink,
        linkWidth = input$linkWidth,
        textSize = input$textSize,
        rad1 = input$rad1,
        rad2 = input$rad2,
        focus = input$focus,
        shadowNode = input$shadowNode,
        export = TRUE,
        filename = "www/img/export_fig.png",
        res = input$res,
        img_size = input$img_size,
        legend = input$legend
      )

      # Return a list containing the filename
      list(
        src = "www/img/export_fig.png",
        contentType = "image/png",
        width = "60%",
        style = "display: block; margin-left: auto; margin-right: auto;",
        alt = "This is alternate text"
      )
    },
    deleteFile = FALSE
  )
}
