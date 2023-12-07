#' Function to run the shiny app to generate the figure
#'
#' @export
run_metanetwork_app <- function() {
  shiny::runApp(fs::path_package("metanetwork", "app"))
}
