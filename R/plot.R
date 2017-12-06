#' @title Visualize graph.
#'
#' @description \code{plot.grapherator} generates a scatterplot of the nodes in the
#' Euclidean plane. Additionally, the edge weights are visualized. In case of one
#' weight per edge either a histogram or an empirical distribution function is drawn.
#' For graphs with two weights per edge a scatterplot is used.
#'
#' @param show.cluster.centers [\code{logical(1)}]\cr
#'   Display cluster centers?
#'   Default is \code{TRUE}. This option is ignored silently if the instance is not clustered.
#' @param highlight.clusters [\code{logical(1)}]\cr
#'   Shall nodes be coloured by cluster membership?
#'   Default is \code{FALSE}.
#' @param show.edges [\code{logical(1)}]\cr
#'   Display edges? Keep in mind, that the number of edges is \eqn{O(n^2)}
#'   where \eqn{n} is the number of nodes.
#'   Default is \code{TRUE}.
#' @param weight.plot.type [\code{character(1)}]\cr
#'   Type of visualization which should be used for weights in case \code{x} has only
#'   as single weight attached to each edge. Either \dQuote{histogram} or \dQuote{ecdf}
#'   (empirical distribution function) are possible choices.
#'   Default is \code{histogram}.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @param x [\code{grapherator}]\cr
#'   Graph.
#' @param y Not used at the moment.
#' @return [\code{list}] A list of \code{\link[ggplot2]{ggplot}} objects with components
#' \code{pl.weights} (scatterplot of edge weights) and eventually \code{pl.coords} (scatterplot of
#' nodes). The latter is \code{NULL}, if \code{graph} has no associated coordinates.
#' @examples
#' g = graph(0, 100)
#' g = addNodes(g, n = 25, generator = addNodesGrid)
#' g = addEdges(g, generator = addEdgesDelauney)
#' g = addWeights(g, generator = addWeightsDistance, method = "manhattan")
#' \dontrun{
#' pls = plot(g, weight.plot.type = "ecdf")
#' }
#'
#' g = addWeights(g, generator = addWeightsRandom,
#'   method = rpois, lambda = 0.1)
#' \dontrun{
#' pls = plot(g, show.edges = FALSE)
#' }
#'
#' g = graph(0, 100)
#' g = addNodes(g, n = 25, generator = addNodesGrid)
#' g = addNodes(g, n = 9, by.centers = TRUE, generator = addNodesGrid,
#'   lower = c(0, 0), upper = c(7, 7))
#' g = addEdges(g, generator = addEdgesDelauney)
#' g = addWeights(g, generator = addWeightsCorrelated, rho = -0.8)
#' \dontrun{
#' do.call(gridExtra::grid.arrange, plot(g, show.edges = FALSE))
#' do.call(gridExtra::grid.arrange, plot(g, show.edges = TRUE,
#'   show.cluster.centers = FALSE))
#' }
#' @export
plot.grapherator = function(x, y = NULL,
  show.cluster.centers = TRUE, highlight.clusters = FALSE, show.edges = TRUE,
  weight.plot.type = "histogram",
  ...) {
  assertFlag(show.cluster.centers)
  assertFlag(highlight.clusters)
  assertFlag(show.edges)
  assertChoice(weight.plot.type, choices = c("histogram", "ecdf"))

  # extract data
  n.nodes = x$n.nodes
  n.edges = x$n.edges
  n.clusters = x$n.clusters
  n.weights = x$n.weights

  pl.coords = NULL

  if (n.weights > 2L)
    stopf("plot.grapherator: More than 2 weights are currently not supported.")

  # draw coordinates if possible
  if (!is.null(x$coordinates)) {
    dd = as.data.frame(x$coordinates)
    names(dd) = c("x1", "x2")
    # add clusters if available
    if (!is.null(x$membership))
      dd$Cluster = as.factor(x$membership)
    pl.coords = ggplot2::ggplot(dd, aes_string(x = "x1", y = "x2"))

    # add edges if desired
    if (show.edges) {
      adj.mat = if (!is.null(x$adj.mat))
          x$adj.mat
        else
          matrix(TRUE, nrow = x$n.nodes, ncol = x$n.nodes)

      edges = data.frame()
      n = nrow(adj.mat)
      for (i in 1:n) {
        for (j in 1:n) {
          if (!adj.mat[i, j])
            next
          coords = x$coordinates[c(i, j), ]
          edges = rbind(edges, data.frame(
            x1 = coords[1L, 1L], y1 = coords[1L, 2L],
            x2 = coords[2L, 1L], y2 = coords[2L, 2L]))
        }
      }
      pl.coords = pl.coords + geom_segment(data = edges, aes_string(x = "x1", y = "y1", xend = "x2", yend = "y2"))
    }

    pl.coords = pl.coords + ggplot2::geom_point()
    pl.coords = if (n.clusters > 0L & highlight.clusters) pl.coords +
      ggplot2::geom_point(aes_string(colour = "Cluster")) else pl.coords + ggplot2::geom_point()
    if (n.clusters > 0L & show.cluster.centers) {
      ddc = as.data.frame(x$center.coordinates)
      names(ddc) = c("x1", "x2")
      pl.coords = pl.coords +
        ggplot2::geom_point(data = ddc, colour = "black", size = 3)
      pl.coords = pl.coords +
        ggplot2::geom_point(data = ddc, colour = "white", size = 2.2)
    }

    pl.coords = pl.coords +
      ggplot2::ggtitle("Network topology", subtitle = sprintf("#nodes: %i, #edges: %i, #clusters: %i\nNode type(s): %s, edge type(s): %s", n.nodes, n.edges, n.clusters, collapse(x$node.types, ", "), collapse(x$edge.types, ", ")))
    pl.coords = pl.coords +
      ggplot2::xlab(expression(x[1])) +
      ggplot2::ylab(expression(x[2]))
    pl.coords = pl.coords + theme(legend.position = "none")
  }

  pl.weights = NULL
  if (x$n.weights == 1L) {
    weights1 = x$weights[[1L]]

    # do not show high values
    if (!is.null(x$adj.mat)) {
      weights1[x$adj.mat == 0] = NA
    }

    weights1 = as.numeric(weights1[upper.tri(weights1)])

    dd = data.frame(w1 = weights1)
    if (weight.plot.type == "histogram")
      pl.weights = ggplot2::ggplot(dd, aes_string(x = "w1", y = "..density..")) + ggplot2::geom_histogram()
    else
      pl.weights = ggplot2::ggplot(dd, aes_string(x = "w1")) + ggplot2::stat_ecdf()
    pl.weights = pl.weights + ggplot2::ggtitle("Edge weights", subtitle = collapse(paste(names(dd), x$weight.types, sep = " : "), sep = ", "))
    pl.weights = pl.weights + xlab(expression(w[1])) + ylab("Freq.")
  } else if (x$n.weights == 2L) {
    weights1 = x$weights[[1L]]
    weights2 = x$weights[[2L]]

    # do not show high values
    if (!is.null(x$adj.mat)) {
      weights1[x$adj.mat == 0] = NA
      weights2[x$adj.mat == 0] = NA
    }

    weights1 = as.numeric(weights1[upper.tri(weights1)])
    weights2 = as.numeric(weights2[upper.tri(weights2)])

    dd = data.frame(w1 = weights1, w2 = weights2)
    pl.weights = ggplot2::ggplot(dd, aes_string(x = "w1", y = "w2")) + ggplot2::geom_point()
    pl.weights = pl.weights +
      ggplot2::ggtitle("Edge weights", subtitle = collapse(paste(names(dd), x$weight.types, sep = " : "), sep = ", "))
    pl.weights = pl.weights + xlab(expression(w[1])) + ylab(expression(w[2]))
  }

  return(list(pl.coords = pl.coords, pl.weights = pl.weights))
}

# experimental
addEdgesToPlot = function(x, g, edge.list) {  # nocov start
  assertClass(x, "ggplot")
  assertMatrix(edge.list, nrows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)

  adj.mat = if (!is.null(g$adj.mat))
    g$adj.mat
  else
    matrix(TRUE, nrow = g$n.nodes, ncol = g$n.nodes)

  edges = data.frame()
  for (i in 1:ncol(edge.list)) {
    edge = edge.list[, i]
    if (!adj.mat[edge[1L], edge[2L]])
      next
    coords = g$coordinates[edge, ]
    edges = rbind(edges, data.frame(
      x1 = coords[1L, 1L], y1 = coords[1L, 2L],
      x2 = coords[2L, 1L], y2 = coords[2L, 2L]))
  }

  x + geom_segment(data = edges, aes_string(x = "x1", y = "y1", xend = "x2", yend = "y2"), colour = "tomato")
}  # nocov end
