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
#' @examples
#' # first we define a simple graph
#' g = graph(0, 100)
#' g = addNodes(g, n = 5, generator = addNodesLHS)
#' g = addNodes(g, n = c(3, 10, 20, 10, 40), by.centers = TRUE, generator = addNodesUniform,
#'   lower = c(0, 0), upper = c(15, 15))
#' g = addEdges(g, generator = addEdgesDelauney)
#'
#' # first graph contains two integer random weights per edge
#' g1 = addWeights(g, generator = addWeightsRandom, method = runif, min = 10, max = 20, to.int = TRUE)
#' g1 = addWeights(g, generator = addWeightsRandom, method = runif, min = 10, max = 30, to.int = TRUE)
#' \dontrun{
#' plot(g1)$pl.weights
#' }
#'
#' # next one contains correlated weights. The first weight corresponds to the
#' # Euclidean distance of the points, the second is generated in a way, that
#' # a given correlation rho is achieved.
#' g2 = addWeights(g, generator = addWeightsCorrelated, rho = -0.7)
#' \dontrun{
#' plot(g2)$pl.weights
#' }
#'
#' # Last example contains two weights per edge: the first one is the Manhattan
#' # block distance between the nodes in the plane. The second one is the Euclidean
#' # distance plus a normally distributed jitter. Here we write a custom weight
#' # generator which returns two weight matrizes.
#' myWeightGenerator = function(graph, ...) {
#'   n = getNumberOfNodes(graph)
#'   adj.mat = getAdjacencyMatrix(graph)
#'   coords = getNodeCoordinates(graph)
#'
#'   man.dist = as.matrix(dist(coords), method = "manhattan")
#'   euc.dist = as.matrix(dist(coords)) + abs(rnorm(n * n, ...))
#'
#'   # keep in mind non-existent edges
#'   euc.dist[!adj.mat] = man.dist[!adj.mat] = Inf
#'
#'   # return the necessary format
#'   return(list(weights = list(man.dist, euc.dist), generator = "MyWG"))
#' }
#'
#' g3 = addWeights(g, generator = myWeightGenerator, mean = 30, sd = 5)
#' \dontrun{
#' plot(g3)$pl.weights
#' }
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

  getNextInteger = function(x) {
    ceiling(x + 0.5)
  }

  makeIntegerWeights = function(ww, adj.mat) {
    ww[adj.mat] = getNextInteger(ww[adj.mat])
    return(ww)
  }

  n.nodes = graph$n.nodes
  if (n.nodes == 0)
    stopf("addWeights: first place nodes/coordinates and add edges.")

  if (is.null(weights) & is.null(generator))
    stopf("grapherator::addWeights: one of weights or generator must be provided.")

  if (is.null(graph$adj.mat))
    graph = addEdges(graph, generator = addEdgesComplete)

  if (!is.null(weights))
    assertMatrix(weights, nrows = n.nodes, ncols = n.nodes, mode = "numeric")

  # add manually generated weights
  if (!is.null(weights)) {
    if (symmetric)
      weights = makeSymmetricWeights(weights)
    if (to.int)
      weights = makeIntegerWeights(weights, graph$adj.mat)
    graph$weights = c(graph$weights, list(weights))
    graph$n.weights = graph$n.weights + 1L
    graph$weight.types = c(graph$weight.types, "MAN")
  # generate weights
  } else {
    generator.res = do.call(generator, c(list(graph = graph), list(...)))
    weights = generator.res$weights
    if (symmetric)
      weights = lapply(weights, makeSymmetricWeights)
    if (to.int)
      weights = lapply(weights, makeIntegerWeights, graph$adj.mat)
    graph$weights = c(graph$weights, weights)
    graph$n.weights = graph$n.weights + length(weights)
    graph$weight.types = c(graph$weight.types, generator.res$generator)
  }

  return(graph)
}
