#' @title Add weights to graph.
#'
#' @description \code{addWeights} allows to add edge weights to a graph. This is
#' the last step of the graph generation process. Note that adding edges is not
#' possible once \code{addWeights} was called once.
#'
#' @template arg_grapherator
#' @param generator [\code{function(graph, ...)}]\cr
#'   Function used to generate weights. The functions needs to expect the graph
#'   as the first argument \code{graph}. Additional control argument are
#'   possible.
#' @param weights [\code{matrix}]\cr
#'   Square matrix of weights.
#'   If some weights are already assigned, pay attention to the correct dimensions.
#'   If this is passed all other arguments are ignored.
#'   Default is \code{NULL}.
#' @param symmetric [\code{logical(1)}]\cr
#'   Should the weights be symmetric, i.e., \eqn{w(i, j) = w(j, i)} for each pair \eqn{i, j} of nodes?
#'   Default is \code{TRUE}.
#' @param to.int [\code{logical(1)}]\cr
#'   Should weights be rounded to integer?
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Additional arguments passed down to \code{generator}.
#' @template ret_grapherator
#' @family graph generators
#' @export
addWeights = function(graph, generator = NULL, weights = NULL, symmetric = TRUE, to.int = FALSE, ...) {
  assertClass(graph, "grapherator")
  assertFunction(generator, null.ok = TRUE)
  assertFlag(to.int)
  assertFlag(symmetric)

  makeSymmetricWeights = function(ww) {
    ww[lower.tri(ww)] = t(ww)[lower.tri(t(ww))]
    return(ww)
  }

  n.nodes = graph$n.nodes
  if (n.nodes == 0)
    stopf("addWeights: first place nodes/coordinates and add edges.")

  if (is.null(graph$adj.mat))
    graph = addEdges(graph, generator = addEdgesComplete)

  if (!is.null(weights))
    assertMatrix(weights, nrows = n.nodes, ncols = n.nodes, mode = "numeric")

  # add manually generated weights
  if (!is.null(weights)) {
    if (symmetric)
      weights = makeSymmetricWeights(weights)
    graph$weights = c(graph$weights, list(weights))
    graph$n.weights = graph$n.weights + 1L
    graph$weight.types = c(graph$weight.types, "manual")
  # generate weights
  } else {
    generator.res = do.call(generator, c(list(graph = graph), list(...)))
    weights = generator.res$weights
    if (symmetric) {
      weights = lapply(weights, makeSymmetricWeights)
    }
    graph$weights = c(graph$weights, weights)
    graph$n.weights = graph$n.weights + length(weights)
    graph$weight.types = c(graph$weight.types, generator.res$generator)
  }

  return(graph)
}
