#' Function to plot metanetworks using hierarchical edge bundling
#'
#' @param nodes data.frame or path to csv of nodes with columns network (name of network), subnetwork (groups within networks), category (categories within groups), col (hexadecimal color of nodes, optional), size (size of nodes, optional).
#' @param links data.frame or path to csv of links between categories of nodes with columns from (category intiating link), to (category receiving link), col (hexadecimal color of links, optional), width (width of links, optional)
#' @param colNode hexadecimal for node color of length = 1 or length = `nrow(nodes), or color palette used to for networks, if not provided in `nodes`. All nodes within a subnetwork must have the same color.
#' @param nodeSize numeric, size of nodes, if not provided in `nodes`. Can be a single numeric value or a numeric vector of length = `nrow(nodes)`. If using frequencies or other numeric values ranging from 0 to infinity, you should normalize the values so that the maximum value is at least no greater than 5 depending on the size of the graph.
#' @param colLink color of links, if not provided in `links`. Can be a single numeric value or a numeric vector of length = `nrow(links)`.
#' @param linkWidth numeric, width of links, if not provided in `links`. Can be a single numeric value or a numeric vector of length = `nrow(links)`.
#' @param textSize numeric, size of text displayed on figure. All text displayed is a proportion of this parameter.
#' @param rad1,rad2 lower and upper boundary for placement of individual network titles.
#' @param focus character, name of network(s) to focus on; if length(focus) == 1, all links towards a single network; if length(focus) > 1, links focused on identified networks.
#' @param shadowNode logical, add a shadow around nodes.
#' @param shadowLink color of links that are not focused on.
#' @param export logical, export figure as png image.
#' @param filename filename of the (ignored if `export` is FALSE).
#' @param res resolution of png image.
#' @param img_size height and width of png image.
#' @param legend logical, add a legend to png image.
#'
#' @examples
#' # Data 1
#' \dontrun{
#' nodes <- metanetwork:::nodes
#' links <- metanetwork:::links
#'
#' # Figure
#' metanetwork(metanetwork:::nodes, metanetwork:::links)
#' metanetwork(metanetwork:::nodes, metanetwork:::links, focus = "Species")
#' metanetwork(metanetwork:::nodes, metanetwork:::links, focus = c("Species", "Drivers", "Managers"))
#'
#' # Data 2
#' nodes <- system.file("extdata", "nodes.csv", package = "metanetwork") |> read.csv()
#' links <- system.file("extdata", "links.csv", package = "metanetwork") |> read.csv()
#' }
#' @export
metanetwork <- function(
    nodes,
    links,
    colNode = NULL,
    nodeSize = 0.5,
    colLink = "#876b40",
    linkWidth = 1,
    textSize = 1,
    rad1 = 0.925,
    rad2 = 1,
    focus = NULL,
    shadowNode = TRUE,
    shadowLink = "#f4f4f4",
    export = TRUE,
    filename = "metanetwork.png",
    res = 300,
    img_size = 200,
    legend = TRUE) {
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # DATA PREPARATION
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # --------------------
  # LEGEND
  # --------------------
  if (legend & !export) stop("Legends are only available when a figure is exported. Set `export = TRUE` if you wish to produce a legend.")

  # --------------------
  # NODES
  # --------------------
  # Check if data must be imported
  if (is.character(nodes)) {
    # WARNING: security issue here, modify code to check structure before loading data
    nodes <- vroom::vroom(nodes)
  }

  # Check if data has the proper columns
  chk <- all(c("network", "subnetwork", "category") %in% colnames(nodes))
  if (!chk) stop("Columns `network`, `subnetwork` and `category` must be present in `nodes`.")

  # Check if colors are included in the data
  cols <- as.factor(nodes$subnetwork) |> 
    as.numeric()
  if (is.null(colNode)) {
    if ("col" %in% colnames(nodes)) {
      cli::cli_alert_info("`col` found in column names")
    } else {
      cli::cli_alert_warning(
        "`col` not found in column names, defaulting to viridis"
      )
      nodes$col <- viridis::viridis(max(cols))[cols]
    }
  } else {
     # Add colors
     if (is.function(colNode)) {
       nodes$col <- colNode(max(cols))[cols]
     } else if (is.character(colNode)) {
       nodes$col <- colNode
     } else {
       cli::cli_alert_warning(
         "wrong color type, defaulting to viridis"
       )
       nodes$col <- viridis::viridis(max(cols))[cols]
     }
  }

  # Check if node sizes are included in the data
  chk <- "size" %in% colnames(nodes)
  if (!chk) {
    # Add node size
    nodes$size <- nodeSize
  }

  # --------------------
  # LINKS
  # --------------------
  # Check if data must be imported
  if (is.character(links)) {
    # WARNING: security issue here, modify code to check structure before loading data
    links <- vroom::vroom(links)
  }

  # Check if data has the proper columns
  chk <- all(c("from", "to") %in% colnames(links))
  if (!chk) stop("Columns `from` and `to` must be present in `links`.")

  # Check if colors are included in the data
  chk <- "col" %in% colnames(links)
  if (!chk) {
    # Add colors
    links$col <- colLink
  }

  # Check if link widths are included in the data
  chk <- "width" %in% colnames(links)
  if (!chk) {
    # Add link width
    links$width <- linkWidth
  }

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # GRAPH ELEMENTS
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Boundaries of networks
  tmp <- nodes
  tmp$subnetwork <- tmp$network
  networks <- bound(tmp)
  networks <- nodePos(tmp, networks, edgeRad = .85, groupRad = .6)$networkGroup

  # Boundaries of subnetworks
  networkGroup <- bound(nodes)

  # Colors of subnetworks
  networkGroup <- dplyr::left_join(
    networkGroup,
    nodes[, c("subnetwork", "col")],
    by = c("Var1" = "subnetwork")
  ) |>
    dplyr::distinct()

  # Node and subnetwork coordinates
  meta <- nodePos(nodes, networkGroup, edgeRad = .85, groupRad = .6)
  nodes <- meta$nodes
  networkGroup <- meta$networkGroup

  # Position of labels if `legend = TRUE`
  if (legend) {
    labs <- nodePos(nodes, networkGroup, edgeRad = .9, groupRad = .6)
    labs <- labs$nodes |>
      dplyr::filter(size > 0) |>
      dplyr::group_by(network, subnetwork) |>
      dplyr::mutate(lab = 1:dplyr::n()) |>
      dplyr::ungroup()
  }

  # Link colors
  meta <- linkCol(
    links,
    networkGroup,
    focus = focus,
    colLink = colLink,
    shadowLink = shadowLink
  )
  links <- meta$links
  networkGroup <- meta$networkGroup

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # PLOT
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  if (export) {
    grDevices::png(
      filename,
      res = res,
      width = img_size,
      height = img_size,
      units = "mm"
    )
  }

  graphics::par(mar = c(0, 0, 0, 0), family = "serif", bg = "#ffffff00")
  plot0(x = c(-1.1, 1.1), asp = 1)

  # Networks
  boxGroup(
    links,
    networks,
    rad1 = 1.03, rad2 = 1.13,
    colBox = "#00000000", colNames = "#000000",
    border = "#000000",
    cexNetwork = textSize * 2,
    type = "line"
  )

  # Subnetworks
  boxGroup(
    links,
    networkGroup,
    rad1 = rad1,
    rad2 = rad2,
    colBox = networkGroup$colBox,
    colNames = networkGroup$colNames,
    border = "transparent",
    cexNetwork = textSize
  )

  # Links
  plotLinks(nodes, links, networkGroup, lwd = links$width)

  # Shadow under nodes
  if (shadowNode) {
    graphics::points(
      nodes$x,
      nodes$y,
      pch = 20,
      cex = (nodes$size * 5),
      col = "#d7d7d7"
    )
  }

  # Nodes
  graphics::points(
    nodes$x,
    nodes$y,
    pch = 20,
    cex = (nodes$size * 3),
    col = nodes$col
  )

  # Add labels if legend
  if (legend) graphics::text(x = labs$x, y = labs$y, labels = labs$lab, cex = textSize * .6)

  if (export) grDevices::dev.off()

  # Legend
  if (legend) {
    metanetwork_legend(
      labs = labs,
      res = res,
      textSize = textSize,
      filename
    )
  }

  invisible(filename)
}
