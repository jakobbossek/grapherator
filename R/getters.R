#' @title Getter functions.
#'
#' @description Functions to extract meta information of \code{grapherator} object.
#'
#' @template arg_grapherator
#' @param cluster.centers [\code{logical(1)}]\cr
#'   Return coordinates of cluster centers only?
#'   Default is \code{FALSE}.
#' @param objective [\code{integer(1)}]\cr
#'   Number of weight matrix to return.
#' @export
#' @examples
#' g = graph(0, 100)
#' g = addNodes(g, n = 25, generator = addNodesGrid)
#' g = addEdges(g, generator = addEdgesGrid)
#' g = addWeights(g, generator = addWeightsRandom, method = runif, min = 5, max = 100, to.int = TRUE)
#' g = addWeights(g, generator = addWeightsDistance, method = "euclidean")
#'
#' getNumberOfNodes(g)
#' getNumberOfEdges(g)
#' getNumberOfClusters(g)
#' getNumberOfWeights(g)
#' getNodeCoordinates(g)
#' getWeightMatrix(g, 2)
#' getAdjacencyMatrix(g)
#' getNodeDegrees(g)
#' @rdname getter
#' @name getter
getNumberOfNodes = function(graph) {
  return(graph$n.nodes)
}

#' @export
#' @rdname getter
getNumberOfEdges = function(graph) {
  return(graph$n.edges)
}

#' @export
#' @rdname getter
getNumberOfClusters = function(graph) {
  return(graph$n.clusters)
}

#' @export
#' @rdname getter
getNumberOfWeights = function(graph) {
  return(graph$n.weights)
}

#' @export
#' @rdname getter
getNodeCoordinates = function(graph, cluster.centers = FALSE) {
  if (!cluster.centers)
    return(graph$coordinates)
  return(graph$center.coordinates)
}

#' @export
#' @rdname getter
getWeightMatrix = function(graph, objective) {
  if (objective > getNumberOfWeights(graph))
    stopf("getWeightMatrix: requested %i-th weight matrix,
      but graph has only %i weight matrizes.", objective, getNumberOfWeights(graph))
  return(graph$weights[[objective]])
}

#' @export
#' @rdname getter
getAdjacencyMatrix = function(graph) {
  return(graph$adj.mat)
}

#' @export
#' @rdname getter
getNodeDegrees = function(graph) {
  return(rowSums(getAdjacencyMatrix(graph)))
}
