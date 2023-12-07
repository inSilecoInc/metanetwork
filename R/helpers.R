# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#' Let's begin by creating a function that will give us the x and y coordinates
#' of the outside of a circle given a certain radius
#' @noRd
coordCircle <- function(theta, radius = 1) {
    data.frame(x = radius * cos(theta), y = radius * sin(theta))
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
bound <- function(nodes, gap = 0.05, addGap = TRUE) {
    # Metanetwork list composed of 'nodes' and 'links' Size
    # of gap between groups on the graph addGap logical
    # whether to add gap or not
    nGroup <- table(nodes$subnetwork) |>
        as.data.frame() |>
        dplyr::left_join(nodes[, c("network", "subnetwork")],
            by = c(Var1 = "subnetwork")) |>
        dplyr::distinct() |>
        dplyr::arrange(network)

    nGroup$Prop <- nGroup$Freq/sum(nGroup$Freq)
    nGroup$spanDeg <- 2 * pi * nGroup$Prop
    nGroup$upper <- nGroup$lower <- 0
    for (i in 2:nrow(nGroup)) {
        nGroup$lower[i] <- nGroup$lower[i - 1] + nGroup$spanDeg[i -
            1]
    }
    nGroup$upper <- nGroup$lower + nGroup$spanDeg

    if (addGap) {
        nGroup$lower <- nGroup$lower + gap / 2
        nGroup$upper <- nGroup$upper - gap/2
    }

    nGroup
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
nodePos <- function(nodes, networkGroup, edgeRad = 0.975, groupRad = 0.5,
    gapEdge = 0.1, addGap = TRUE) {
    # Add x and y columns to nodes and networkGroup data
    nodes$y <- nodes$x <- 0
    networkGroup$y <- networkGroup$x <- 0

    # Get coordinates for all networks
    for (i in 1:nrow(networkGroup)) {
        # Distribute points within each network space
        edgeDeg <- seq((networkGroup$lower[i] + (gapEdge/2)),
            (networkGroup$upper[i] - (gapEdge/2)), length = networkGroup$Freq[i])

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

    metanetwork <- list(nodes = nodes, networkGroup = networkGroup)
    metanetwork
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
linkCol <- function(links, networkGroup, focus = NULL, colLink = "#876b40",
    shadowLink = "#f4f4f4") {
    # focus = character, name of network(s) to focus on; if
    # length(focus) == 1, all links towards a single
    # network if length(focus) > 1, links focused on
    # identified networks colLink = color of links when all
    # colors are the same shadowLink = color of links that
    # we are not focused on

    # Function
    if (is.null(focus)) {
        networkGroup$colNames <- shadowLink
        networkGroup$colBox <- networkGroup$col
    } else {
        # Box colors for subnetworks
        uid <- networkGroup$Var1 %in% focus
        networkGroup$colNames <- networkGroup$col
        networkGroup$colNames[uid] <- shadowLink
        networkGroup$colBox <- networkGroup$col
        networkGroup$colBox[!uid] <- shadowLink

        # Identify with which subnetwork each interaction
        # is associated
        links$col <- shadowLink
        linkCol <- data.frame(from = nodes$subnetwork[match(links$from,
            nodes$category)], to = nodes$subnetwork[match(links$to,
            nodes$category)])

        if (length(focus) == 1) {
            # Intra-network interactions
            linkID <- linkCol$from %in% focus & linkCol$to %in%
                focus
            links$col[linkID] <- networkGroup$col[uid]

            # Inter-network interactions
            linkID <- (linkCol$from %in% focus | linkCol$to %in%
                focus) & !linkID
            cols <- paste0(linkCol$from[linkID], linkCol$to[linkID])
            cols <- gsub(focus, "", cols)
            cols <- match(cols, networkGroup$Var1)
            cols <- networkGroup$col[cols]
            links$col[linkID] <- cols
        }

        if (length(focus) > 1) {
            linkID <- linkCol$from %in% focus & linkCol$to %in%
                focus
            links$col[linkID] <- colLink
        }
    }

    list(links = links, networkGroup = networkGroup)
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
boxGroup <- function(links, networkGroup, rad1 = 0.95, rad2 = 1,
    colBox = NULL, names = NULL, colNames = NULL, addNames = T,
    cexNetwork = 1, type = "box", ...) {
    # metanetwork = data list composed of 'nodes', 'links'
    # & 'networkGroup' rad1 = lower boundary for polygons
    # rad2 = upper boundary for polygons colBox = color of
    # boxes names = names of individual networks colNames =
    # color of names addNames = logical, add names of
    # networks to graph
    if (!is.null(colNames) & length(colNames) == 1) {
        colNames <- rep(colNames, nrow(links))
    }

    if (!is.null(colBox) & length(colBox) == 1) {
        colBox <- rep(colBox, nrow(links))
    }

    for (i in seq_len(nrow(networkGroup))) {
        a <- coordCircle(theta = seq(networkGroup$lower[i], networkGroup$upper[i],
            length = 200), radius = rad1)

        b <- coordCircle(theta = seq(networkGroup$upper[i], networkGroup$lower[i],
            length = 200), radius = rad2)

        if (type == "box") {
            graphics::polygon(rbind(a, b, a[1L, ]), col = colBox[i],
                ...)
        }
        if (type == "line") {
            graphics::lines(a, col = "#000000", lwd = 3)
        }

        if (addNames) {
            middle <- mean(c(networkGroup$lower[i], networkGroup$upper[i]))
            clockwise <- middle <= pi
            plotrix::arctext(x = as.character(networkGroup$Var1[i]),
                radius = mean(c(rad1, rad2)), middle = middle,
                col = colNames[i], clockwise = clockwise, font = 2,
                cex = cexNetwork)
        }
    }
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
plotLinks <- function(nodes, links, networkGroup, ...) {
    for (i in seq_len(nrow(links))) {
        link <- links[i, ]
        edgeFromID <- which(nodes$category == link$from)
        edgeToID <- which(nodes$category == link$to)
        groupFromID <- which(networkGroup$Var1 == nodes$subnetwork[edgeFromID])
        groupToID <- which(networkGroup$Var1 == nodes$subnetwork[edgeToID])

        if (nodes$subnetwork[edgeFromID] != nodes$subnetwork[edgeToID]) {
            linkPath <- rbind(nodes[edgeFromID, c("x", "y")],
                networkGroup[groupFromID, c("x", "y")], networkGroup[groupToID,
                  c("x", "y")], nodes[edgeToID, c("x", "y")])
        } else {
            linkPath <- rbind(nodes[edgeFromID, c("x", "y")],
                networkGroup[groupFromID, c("x", "y")], nodes[edgeToID,
                  c("x", "y")])
        }

        graphics::lines(graphics::xspline(linkPath$x, linkPath$y,
            shape = 1, draw = FALSE), col = links$col[i], ...)
    }
}
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
metanetwork_legend <- function(labs, res = 300, textSize = 1,
    filename = NULL) {
    # Height of figure should depend on the maximum number
    # of elements in a subnetwork Width given to each
    # subnetwork should depend on the number of
    # subnetworks, but have a maximum width parameter

    nCol <- length(unique(labs$subnetwork))
    nRow <- dplyr::group_by(labs, subnetwork) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::summarise(n = max(n)) |>
        as.numeric()

    # Categories
    cat <- labs |>
        dplyr::group_by(network, subnetwork) |>
        dplyr::mutate(id = dplyr::cur_group_id()) |>
        dplyr::ungroup() |>
        dplyr::mutate(x = 0 + id, y = 0 + lab, labels = glue::glue("{lab}: {category}"))

    # Subnetworks
    sub <- cat |>
        dplyr::select(network, subnetwork, col, id) |>
        dplyr::distinct() |>
        dplyr::mutate(x1 = id, x2 = id + 0.9, y1 = 0, y2 = 1)

    # Networks
    net <- sub |>
        dplyr::group_by(network) |>
        dplyr::summarise(x1 = min(x1), x2 = max(x2))

    grDevices::png("metanetwork_legend.png", res = res, width = 50 *
        nCol, height = 5 * nRow, units = "mm")

    # Plot
    graphics::par(mar = c(0.5, 0.5, 0.5, 0.5), bg = "#ffffff00")
    plot0(x = c(1, nCol + 1), y = c(3, -nRow))

    # Graphical elements
    xG <- 0.05

    # Networks
    for (i in seq_len(nrow(net))) {
        graphics::lines(x = c(net$x1[i], net$x2[i]), y = c(1.25,
            1.25), lwd = 2)
    }

    graphics::text(x = net$x1, y = 2, labels = net$network, cex = textSize,
        font = 2, adj = c(0, 0.5))

    # Subnetworks
    graphics::rect(sub$x1, sub$y1, sub$x2, sub$y2, col = sub$col,
        border = sub$col)
    graphics::text(x = sub$x1 + xG, y = 0.5, labels = sub$subnetwork,
        cex = textSize * 0.9, font = 2, adj = c(0, 0.5))

    # Categories
    graphics::text(x = cat$x + xG, y = -cat$y, labels = cat$labels,
        adj = c(0, 0.5), cex = textSize * 0.85)

    grDevices::dev.off()
    # ------------------------------------------------------------------------------------------
    # Figures of interest
    i1 <- magick::image_read(filename)
    i2 <- magick::image_read("metanetwork_legend.png")

    # Size of images
    i1s <- magick::image_info(i1)$width
    i2s <- magick::image_info(i2)$width

    # Resize legend if size is greater than size of
    # metanetwork
    if (i2s > i1s)
        i2 <- magick::image_scale(i2, i1s * 0.9)

    # Add left/right border to center
    w <- magick::image_info(i2)$width
    b <- (i1s - w)/2
    i2 <- magick::image_border(i2, "#ffffff00", b)

    # Combine figures together
    img <- magick::image_append(c(i1, i2), stack = TRUE)

    magick::image_write(img, path = filename, format = "png")

    file.remove("metanetwork_legend.png")
}



# borrowed from
# https://github.com/inSileco/graphicsutils/blob/master/R/plot0.R
plot0 <- function(x = c(-1, 1), y = NULL, fill = NULL, text = NULL,
    grid.col = NULL, grid.lwd = 1, grid.lty = 1, ...) {
    ##
    args <- list(...)
    args_txt <- args[names(args) %in% methods::formalArgs(graphics::text.default)]
    deft <- list(ann = FALSE, axes = FALSE, type = "n")
    # default behavior for matrix and vectors
    if (NCOL(as.matrix(x)) > 1 && is.null(y)) {
        if (is.null(y)) {
            y <- x[, 2L]
        }
        x <- x[, 1L]
    } else {
        if (is.null(y)) {
            y <- x
        } else {
            stopifnot(length(x) == length(y))
        }
    }
    #
    coor <- list(x = x, y = y)

    ##
    if (length(args) > 0) {
        id <- which(names(deft) %in% names(args))
        if (length(id) > 0) {
            deft <- deft[-id]
        }
        do.call(graphics::plot.default, args = as.list(c(coor,
            args, deft)))
    } else {
        graphics::plot.default(x = x, y = y, ann = FALSE, axes = FALSE,
            type = "n")
    }

    ##
    if (!is.null(fill)) {
        plotAreaColor(color = fill)
    }
    #
    if (!is.null(grid.col)) {
        graphics::grid(col = grid.col, lty = grid.lty, lwd = grid.lwd)
    }

    ##
    if (!is.null(text)) {
        do.call(graphics::text.default, args = as.list(c(x = mean(x),
            y = mean(y), labels = as.character(text), args_txt)))
    }
    ##
    invisible(NULL)
}

# borrowed from
# https://github.com/inSileco/graphicsutils/blob/master/R/plotAreaColor.R
plotAreaColor <- function(color = "grey80", border = NA, ...) {
    args <- list(...)
    lp <- graphics::par()$usr
    coor <- list(xleft = lp[1L], ybottom = lp[3L], xright = lp[2L],
        ytop = lp[4L])
    if (!is.null(names(color))) {
        names(color) <- NULL
    }
    ##
    if (length(args) > 0) {
        do.call(graphics::rect, args = as.list(c(coor, border = border,
            col = color, args)))
    } else {
        do.call(graphics::rect, args = as.list(c(coor, border = border,
            col = color)))
    }
    ##
    invisible(NULL)
}

## Choose palette colors
choose_pal <- function(name) {
    pal <- switch(name, viridis = viridis::viridis, magma = viridis::magma,
        inferno = viridis::inferno, plasma = viridis::plasma,
        cividis = viridis::cividis, rocket = viridis::rocket,
        mako = viridis::mako, turbo = viridis::turbo, NULL)
    return(pal)
}
