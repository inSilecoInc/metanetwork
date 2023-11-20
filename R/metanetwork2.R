#' Function to plot metanetworks using hierarchical edge bundling 
#'
#' @param metanetwork list composed of a data.frame of nodes and links
#' @param rad1 lower boundary for placement of individual network titles
#' @param rad2 upper boundary for placement of individual network titles
#' @param sizeEdge
#' @param colPal color palette used to differentiate individual networks 
#' @param type type of colors: 'all' = all links with single color given by argument `colLinks`; 'focus' = focus on the links of identified network
#' @param focus character, name of network(s) to focus on; if length(focus) == 1, all links towards a single network; if length(focus) > 1, links focused on identified networks
#' @param colLinks color of links of `type` == 'all'
#' @param colShadow color of links that we are not focused on
#' @param shadowEdge logical, add a shadow around nodes
#' @param cexSize = numeric, size of nodes
#'
#' @examples
#' # Data
#' metanetwork <- metanetwork:::metanetwork
#' 
#' # Figure 
#' plotMetanetwork(metanetwork)
#' plotMetanetwork(metanetwork, type = "focus", focus = "Species")
#' plotMetanetwork(metanetwork, type = 'focus', focus = c('Species', 'Drivers', 'Managers'))
#'
#' @export
plotMetanetwork <- function(metanetwork, rad1 = .925, rad2 = 1, sizeEdge = TRUE, colPal = viridis::viridis, type = 'all', focus = NULL, colLinks = '#876b40', colShadow = '#f4f4f4', shadowEdge = TRUE, cexSize = 0.5) {
                          
  # Boundaries of individual networks
  metanetwork$networkGroup <- bound(metanetwork)

  # Node coordinates
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)

  # Colors
  metanetwork <- colGroups(metanetwork, colPal = colPal(nrow(metanetwork$networkGroup)))

  # Node size
  # metanetwork <- nodeSize(metanetwork, freq = sizeEdge)
  metanetwork$nodes$cex <- cexSize

  # Link col
  metanetwork <- linkCol(
    metanetwork, 
    type = type, 
    focus = focus, 
    colLinks = colLinks, 
    colShadow = colShadow
  )

  # Plot
  par(mar = c(0,0,0,0))
  graphicsutils::plot0()
  boxGroup(
    metanetwork,
    rad1 = rad1,
    colBox = metanetwork$networkGroup$cols,
    colNames = metanetwork$networkGroup$colNames,
    border = 'transparent'
  )
  
  plotLinks(
    metanetwork,
    col = metanetwork$links$cols
  )
  
  if (shadowEdge) {
    points(
      metanetwork$nodes$x,
      metanetwork$nodes$y,
      pch = 20,
      cex = (metanetwork$nodes$cex * 5),
      col = '#d7d7d7'
    )
  }

  points(
    metanetwork$nodes$x,
    metanetwork$nodes$y,
    pch = 20,
    cex = (metanetwork$nodes$cex * 3),
    col = metanetwork$nodes$cols
  )
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

