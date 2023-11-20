#' Function to plot metanetworks using hierarchical edge bundling
#'
#' @param nodes data.frame or path to csv of nodes with columns network (name of network), subnetwork (groups within networks), category (categories within groups), col (hexadecimal color of nodes, optional), size (size of nodes, optional).
#' @param links data.frame or path to csv of links between categories of nodes with columns from (category intiating link), to (category receiving link), col (hexadecimal color of links, optional), width (width of links, optional)
#' @param colNode hexadecimal for node color of length = 1 or length = `nrow(nodes), or color palette used to for networks, if not provided in `nodes`
#' @param nodeSize numeric, size of nodes, if not provided in `nodes`. Can be a single numeric value or a numeric vector of length = `nrow(nodes)`.
#' @param colLink color of links, if not provided in `links`. Can be a single numeric value or a numeric vector of length = `nrow(links)`.
#' @param linkWidth numeric, width of links, if not provided in `links`. Can be a single numeric value or a numeric vector of length = `nrow(links)`.
#' @param rad1 lower boundary for placement of individual network titles
#' @param rad2 upper boundary for placement of individual network titles
#' @param focus character, name of network(s) to focus on; if length(focus) == 1, all links towards a single network; if length(focus) > 1, links focused on identified networks
#' @param shadowNode logical, add a shadow around nodes
#' @param shadowLink color of links that are not focused on
#'
#' @examples
#' # Data 1
#' nodes <- metanetwork:::nodes
#' links <- metanetwork:::links
#'
#' # Figure
#' plotMetanetwork(metanetwork)
#' plotMetanetwork(metanetwork, focus = "Species")
#' plotMetanetwork(metanetwork, focus = c("Species", "Drivers", "Managers"))
#'
#' # Data 2
#' nodes <- system.file("extdata", "nodes.csv", package = "metanetwork") |> read.csv()
#' links <- system.file("extdata", "links.csv", package = "metanetwork") |> read.csv()
#'
#' @export
plotMetanetwork <- function(
    nodes,
    links,
    colNode = viridis::viridis,
    nodeSize = 0.5,
    colLink = "#876b40",
    linkWidth = 1,
    rad1 = .925,
    rad2 = 1,
    focus = NULL,
    shadowNode = TRUE,
    shadowLink = "#f4f4f4") {
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # DATA PREPARATION
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
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
  if (!chk) stop("Columns `network`, `subnetwork` and `category` must be present in the data")

  # Check if colors are included in the data
  chk <- "col" %in% colnames(nodes)
  if (!chk) {
    # Add colors
    if (is.character(colNode)) {
      nodes$col <- colNode
    } else {
      cols <- as.factor(nodes$network) |> as.numeric()
      nodes$col <- colNode(max(cols))[cols]
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
  if (chk) stop("Columns `from` and `to` must be present in the data")

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
  # Boundaries of individual networks
  networkGroup <- bound(nodes)

  # Node coordinates
  metanetwork <- nodePos(nodes, networkGroup, edgeRad = .875, groupRad = .5)
  nodes <- metanetwork$nodes
  networkGroup <- metanetwork$networkGroup
}
