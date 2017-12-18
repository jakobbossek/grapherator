#' @title Graph string representation.
#'
#' @description Given a \code{\link{grapherator}} object the function returns
#' a string representation. Basically this is a concatenation of meta data, node,
#' edge and weight generator types of the following format:
#' N<n.nodes>-E<n.edges>-C<n.clusters>-W<n.weights>---<node-types>---<edge-types>---<weight-types>
#' where n.x is the number of x of the graph.
#'
#' @param x [\code{\link{grapherator}}]\cr
#'   Graph.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{character(1)}]
#' @examples
#' g = graph(lower = c(0, 0), upper = c(100, 100))
#' g = addNodes(g, n = 3, generator = addNodesUniform)
#' g = addNodes(g, n = 14, by.centers = TRUE, generator = addNodesUniform,
#'   lower = c(0, 0), upper = c(10, 10))
#' g = addEdges(g, generator = addEdgesWaxman, alpha = 0.2,
#'   beta = 0.2, type = "intracluster")
#' g = addEdges(g, generator = addEdgesDelauney, type = "intercenter")
#' g = addWeights(g, generator = addWeightsCorrelated, rho = -0.9)
#' g = addWeights(g, generator = addWeightsDistance, method = "euclidean")
#' as.character(g)
#' @export
as.character.grapherator = function(x, ...) {
  meta = sprintf("N%i-E%i-C%i-W%i", getNumberOfNodes(x), getNumberOfEdges(x),
    getNumberOfClusters(x), getNumberOfWeights(x))
  types = list(x$node.types, x$edge.types, x$weight.types)
  types = sapply(types, collapse, "-")
  collapse(c(meta, types), sep = "---")
}
