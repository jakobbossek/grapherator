#' @title Generate a bare graph.
#'
#' @description This function generates a bare graph. The generated
#' object does not contain nodes, edges or edge weights. It serves as a starting
#' point for a three step approach of graph problem construction:
#' 1) Add nodes respectively coordinates via \code{\link{addNodes}}, add edges
#' via \code{\link{addEdges}} and finally add edge weights with the function
#' \code{\link{addWeights}}.
#'
#' @param lower [\code{integer(1)}]\cr
#'   Lower bounds for node coordinates in the euclidean plane.
#' @param upper [\code{integer(1)}]\cr
#'   Upper bounds for node coordinates in the euclidean plane.
#' @template ret_grapherator
#' @family graph generators
#' @export
graph = function(lower, upper) {
  if (length(lower) == 1L)
    lower = rep(lower, 2L)
  if (length(upper) == 1L)
    upper = rep(upper, 2L)
  assertNumeric(lower, len = 2L, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(upper, len = 2L, any.missing = FALSE, all.missing = FALSE)
  if (any(lower >= upper))
    stopf("graph: all elements of lower need to be stricly lower than the corresponding
      value in upper!")
  BBmisc::makeS3Obj(
    lower = lower,
    upper = upper,
    n.clusters = 0L,
    n.nodes = 0L,
    n.weights = 0L,
    node.types = character(0L),
    edge.types = character(0L),
    weight.types = character(0L),
    weights = list(),
    membership = NULL,
    coordinates = NULL,
    classes = "grapherator")
}

#' @export
print.grapherator = function(x, ...) {
  catf("GRAPH")
  catf("Number of nodes: %i", x$n.nodes)
  if (x$n.clusters > 0L)
    catf("Number of clusters: %i", x$n.clusters)
  n.weights = length(x$weights)
  catf("Weights per edge: %i (%s)", n.weights, BBmisc::collapse(x$weight.types))
}
