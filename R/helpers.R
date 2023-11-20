# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#' Let's begin by creating a function that will give us the x and y coordinates
#' of the outside of a circle given a certain radius
coordCircle <- function(theta, radius = 1) {
  data.frame(
    x = radius * cos(theta),
    y = radius * sin(theta)
  )
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
bound <- function(nodes, order = NULL, gap = .05, addGap = T) {
  # Metanetwork list composed of "nodes" and "links"
  # Size of gap between groups on the graph
  # addGap logical whether to add gap or not
  nGroup <- as.data.frame(table(nodes$subnetwork))
  if (!is.null(order)) nGroup <- nGroup[match(order, nGroup$Var1), ]
  nGroup$Prop <- nGroup$Freq / sum(nGroup$Freq)
  nGroup$spanDeg <- 2 * pi * nGroup$Prop
  nGroup$upper <- nGroup$lower <- 0
  for (i in 2:nrow(nGroup)) nGroup$lower[i] <- nGroup$lower[i - 1] + nGroup$spanDeg[i - 1]
  nGroup$upper <- nGroup$lower + nGroup$spanDeg

  if (addGap) {
    nGroup$lower <- nGroup$lower + gap / 2
    nGroup$upper <- nGroup$upper - gap / 2
  }

  nGroup
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
nodePos <- function(nodes, networkGroup, edgeRad = 0.975, groupRad = 0.5, gapEdge = 0.1, addGap = T) {
  # Add x and y columns to nodes and networkGroup data
  nodes$y <- nodes$x <- 0
  networkGroup$y <- networkGroup$x <- 0

  # Get coordinates for all networks
  for (i in 1:nrow(networkGroup)) {
    # Distribute points within each network space
    edgeDeg <- seq((networkGroup$lower[i] + (gapEdge / 2)),
      (networkGroup$upper[i] - (gapEdge / 2)),
      length = networkGroup$Freq[i]
    )

    # Get position for each edge
    nodePos <- coordCircle(theta = edgeDeg, radius = edgeRad)

    # Add to nodes data
    nodes$x[nodes$subnetwork == networkGroup$Var1[i]] <- nodePos$x
    nodes$y[nodes$subnetwork == networkGroup$Var1[i]] <- nodePos$y

    # Distribute network groups in space
    groupDeg <- mean(c(networkGroup$lower[i], networkGroup$upper[i]))

    # Get position for each group
    groupPos <- coordCircle(theta = groupDeg, radius = groupRad)

    # Add to group data
    networkGroup$x[i] <- groupPos$x
    networkGroup$y[i] <- groupPos$y
  }

  metanetwork <- list()
  metanetwork$nodes <- nodes
  metanetwork$networkGroup <- networkGroup
  metanetwork
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
boxGroup <- function(metanetwork, rad1 = .95, rad2 = 1, colBox = NULL, names = NULL, colNames = NULL, addNames = T, cexNetwork = 1, ...) {
  # metanetwork = data list composed of 'nodes', 'links' & 'networkGroup'
  # rad1 = lower boundary for polygons
  # rad2 = upper boundary for polygons
  # colBox = color of boxes
  # names = names of individual networks
  # colNames = color of names
  # addNames = logical, add names of networks to graph
  if (!is.null(colNames) & length(colNames) == 1) {
    colNames <- rep(colNames, nrow(metanetwork$links))
  }

  if (!is.null(colBox) & length(colBox) == 1) {
    colBox <- rep(colBox, nrow(metanetwork$links))
  }

  for (i in 1:nrow(metanetwork$networkGroup)) {
    a <- coordCircle(
      theta = seq(metanetwork$networkGroup$lower[i],
        metanetwork$networkGroup$upper[i],
        length = 200
      ),
      radius = rad1
    )

    b <- coordCircle(
      theta = seq(metanetwork$networkGroup$upper[i],
        metanetwork$networkGroup$lower[i],
        length = 200
      ),
      radius = rad2
    )

    polygon(rbind(a, b, a[1L, ]), col = colBox[i], ...)

    if (addNames) {
      middle <- mean(c(
        metanetwork$networkGroup$lower[i],
        metanetwork$networkGroup$upper[i]
      ))
      clockwise <- if (middle > pi) F else T
      plotrix::arctext(
        x = as.character(metanetwork$networkGroup$Var1[i]),
        radius = mean(c(rad1, rad2)),
        middle = middle,
        col = colNames[i],
        clockwise = clockwise,
        font = 2,
        cex = cexNetwork
      )
    }
  }
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
boxGroup2 <- function(metanetwork, rad1 = .95, rad2 = 1, colBox = NULL, names = NULL, colNames = NULL, addNames = T, cexNetwork = 1, ...) {
  # metanetwork = data list composed of 'nodes', 'links' & 'networkGroup'
  # rad1 = lower boundary for polygons
  # rad2 = upper boundary for polygons
  # colBox = color of boxes
  # names = names of individual networks
  # colNames = color of names
  # addNames = logical, add names of networks to graph
  if (!is.null(colNames) & length(colNames) == 1) {
    colNames <- rep(colNames, nrow(metanetwork$links))
  }

  if (!is.null(colBox) & length(colBox) == 1) {
    colBox <- rep(colBox, nrow(metanetwork$links))
  }

  for (i in 1:nrow(metanetwork$networkGroup)) {
    a <- coordCircle(
      theta = seq(metanetwork$networkGroup$lower[i],
        metanetwork$networkGroup$upper[i],
        length = 200
      ),
      radius = rad1
    )

    b <- coordCircle(
      theta = seq(metanetwork$networkGroup$upper[i],
        metanetwork$networkGroup$lower[i],
        length = 200
      ),
      radius = rad2
    )

    # polygon(rbind(a, b, a[1L,]), col = colBox[i], ...)
    # polygon(rbind(a, b, a[1L,]), col = '#00000000', ...)
    lines(a, col = "#000000", lwd = 3)

    if (addNames) {
      middle <- mean(c(
        metanetwork$networkGroup$lower[i],
        metanetwork$networkGroup$upper[i]
      ))
      clockwise <- if (middle > pi) F else T
      plotrix::arctext(
        x = as.character(metanetwork$networkGroup$Var1[i]),
        radius = mean(c(rad1, rad2)),
        middle = middle,
        col = colNames[i],
        clockwise = clockwise,
        font = 2,
        cex = cexNetwork
      )
    }
  }
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
plotLinks <- function(metanetwork, cols = NULL, ...) {
  if (!is.null(cols) & length(cols) == 1) {
    cols <- rep(cols, nrow(metanetwork$links))
  }

  for (i in 1:nrow(metanetwork$links)) {
    link <- metanetwork$links[i, ]
    edgeFromID <- which(metanetwork$nodes$name == link$from)
    edgeToID <- which(metanetwork$nodes$name == link$to)
    groupFromID <- which(metanetwork$networkGroup$Var1 == metanetwork$nodes$network[edgeFromID])
    groupToID <- which(metanetwork$networkGroup$Var1 == metanetwork$nodes$network[edgeToID])

    if (metanetwork$nodes$network[edgeFromID] != metanetwork$nodes$network[edgeToID]) {
      linkPath <- rbind(
        metanetwork$nodes[edgeFromID, c("x", "y")],
        metanetwork$networkGroup[groupFromID, c("x", "y")],
        metanetwork$networkGroup[groupToID, c("x", "y")],
        metanetwork$nodes[edgeToID, c("x", "y")]
      )
    } else {
      linkPath <- rbind(
        metanetwork$nodes[edgeFromID, c("x", "y")],
        metanetwork$networkGroup[groupFromID, c("x", "y")],
        metanetwork$nodes[edgeToID, c("x", "y")]
      )
    }

    lines(xspline(linkPath$x, linkPath$y, shape = 1, draw = FALSE), col = cols[i], ...)
  }
}

# # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# colGroups <- function(metanetwork, colPal = pal_insileco) {
#   # Group colors
#   metanetwork$networkGroup$cols <- colPal[1:nrow(metanetwork$networkGroup)]

#   # Node colors
#   metanetwork$nodes$cols <- NA
#   for (i in 1:nrow(metanetwork$networkGroup)) {
#     metanetwork$nodes$cols[metanetwork$nodes$network == metanetwork$networkGroup$Var1[i]] <- metanetwork$networkGroup$cols[i]
#   }

#   metanetwork
# }
# # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
nodeSize <- function(metanetwork, freq = T) {
  if (isTRUE(freq)) {
    nLink <- as.data.frame(table(c(metanetwork$links$from, metanetwork$links$to)), stringsAsFactors = F)
    colnames(nLink)[1L] <- "name"
    metanetwork$nodes <- dplyr::left_join(metanetwork$nodes, nLink, by = "name")
    metanetwork$nodes$cex <- (metanetwork$nodes$Freq / max(metanetwork$nodes$Freq))
  } else {
    metanetwork$nodes$cex <- .33
  }

  return(metanetwork)
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
linkCol <- function(metanetwork, type = "all", focus = NULL, colLinks = "#876b40", colShadow = "#f4f4f4") {
  # metanetwork = list composed of 'nodes', 'links' and 'networkGroup'
  # type        = type of colors:
  #                 'all' = all links with single color = `colLinks`
  #                 'focus' = focus on the links of identified network
  # focus       = character, name of network(s) to focus on;
  #                 if length(focus) == 1, all links towards a single network
  #                 if length(focus) > 1, links focused on identified networks
  # colLinks    = color of links of `type` == 'all'
  # colShadow   = color of links that we are not focused on

  # Function
  if (type == "all") {
    metanetwork$links$cols <- colLinks
  }

  if (type == "focus" & length(focus) == 1) {
    # Box colors
    focusID <- metanetwork$networkGroup$Var1 %in% focus
    colBox <- metanetwork$networkGroup$cols
    metanetwork$networkGroup$cols[!focusID] <- colShadow
    metanetwork$networkGroup$colNames <- colBox
    metanetwork$networkGroup$colNames[focusID] <- colShadow

    # Link colors
    # metanetwork$links$cols <- paste0(colShadow, 88)
    metanetwork$links$cols <- colShadow
    linkCol <- data.frame(
      from = metanetwork$nodes$network[match(
        metanetwork$links$from,
        metanetwork$nodes$name
      )],
      to = metanetwork$nodes$network[match(
        metanetwork$links$to,
        metanetwork$nodes$name
      )],
      stringsAsFactors = F
    )

    linkID <- linkCol$from %in% focus & linkCol$to %in% focus
    metanetwork$links$cols[linkID] <- metanetwork$networkGroup$cols[focusID] # "cannibalism"

    linkID <- (linkCol$from %in% focus | linkCol$to %in% focus) & !linkID
    cols <- paste0(linkCol$from[linkID], linkCol$to[linkID])
    cols <- gsub(focus, "", cols)
    cols <- match(cols, metanetwork$networkGroup$Var1)
    cols <- metanetwork$networkGroup$colNames[cols]
    metanetwork$links$cols[linkID] <- cols
  }

  if (type == "focus" & length(focus) > 1) {
    # Box colors
    focusID <- metanetwork$networkGroup$Var1 %in% focus
    colBox <- metanetwork$networkGroup$cols
    metanetwork$networkGroup$cols[!focusID] <- colShadow
    metanetwork$networkGroup$colNames <- colBox
    metanetwork$networkGroup$colNames[focusID] <- colShadow

    # Link colors
    metanetwork$links$cols <- colShadow
    linkCol <- data.frame(
      from = metanetwork$nodes$network[match(
        metanetwork$links$from,
        metanetwork$nodes$name
      )],
      to = metanetwork$nodes$network[match(
        metanetwork$links$to,
        metanetwork$nodes$name
      )],
      stringsAsFactors = F
    )

    linkID <- linkCol$from %in% focus & linkCol$to %in% focus
    metanetwork$links$cols[linkID] <- colLinks
  }

  # Add transparency
  # metanetwork$links$cols <- paste0(metanetwork$links$cols, '22')

  metanetwork
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
