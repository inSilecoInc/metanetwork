#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
    # INPUTS
    r <- shiny::reactiveValues()

    # Reload data function
    reloadData <- function() {
        r$nodes <- googlesheets4::read_sheet(
            ss = input$googledrive_link,
            sheet = "noeuds",
            col_names = TRUE,
            col_types = "ccccd"
        )
        r$links <- googlesheets4::read_sheet(
            ss = input$googledrive_link,
            sheet = "liens",
            col_names = TRUE,
            col_types = "cccd"
        )

        grp_foc <- c(list(All = "all"), as.list(unique(r$nodes$subnetwork)))
        names(grp_foc)[-1] <- unlist(grp_foc[-1])

        shiny::updateSelectInput(session, "focus",
            choices = grp_foc, selected = "All"
        )
    }

    shiny::observeEvent(
        {
            input$googledrive_link
        },
        {
            reloadData()
        }
    )

    shiny::observeEvent(
        {
            input$reloadDataButton
        },
        {
            reloadData()
        },
        ignoreInit = TRUE
    )

    # OUTPUTS
    output$downloadFig <- shiny::downloadHandler(
        filename = function() input$filename,
        contentType = "image/png",
        content = function(file) file.copy(app_sys("app/www/img/export_fig.png", file))
    )

    output$gsLink <- renderUI({
        shiny::actionButton("hyperlink", "Se rendre sur la feuille Google",
            icon = icon("th"),
            onclick = paste0("window.open('", input$googledrive_link, "', '_blank')")
        )
    })

    output$metanetwork <- shiny::renderImage(
        {
            # Generate the PNG
            metanetwork(
                r$nodes,
                r$links,
                colNode = choose_pal(input$colNode),
                nodeSize = input$nodeSize,
                colLink = "#876b40", # input$colLink,
                linkWidth = input$linkWidth,
                textSize = input$textSize,
                rad1 = input$rad1,
                rad2 = input$rad2,
                focus = input$focus,
                shadowNode = input$shadowNode,
                export = TRUE,
                filename = file.path(app_sys(), "app/www/img/export_fig.png"),
                res = input$res,
                img_size = input$img_size,
                legend = input$legend
            )

            # Return a list containing the filename
            list(
                src = app_sys("app/www/img/export_fig.png"),
                contentType = "image/png",
                width = "60%",
                style = "display: block; margin-left: auto; margin-right: auto;"
            )
        },
        deleteFile = FALSE
    )
}
