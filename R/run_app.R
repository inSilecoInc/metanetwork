#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @import shiny
#' @importFrom golem with_golem_options
#'
#' @export

run_app <- function(
    onStart = fct_start,
    options = list(),
    enableBookmarking = "url",
    uiPattern = "/",
    ...) {
    with_golem_options(
        app = shinyApp(
            ui = app_ui,
            server = app_server,
            onStart = onStart,
            options = options,
            enableBookmarking = enableBookmarking,
            uiPattern = uiPattern
        ),
        golem_opts = list(...)
    )
}
