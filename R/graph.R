#' @title Generate a bare graph.
#'
#' @description This function generates a bare graph object of type \code{\link{grapherator}}.
#' The generated object does not contain nodes, edges or edge weights. It serves as a starting
#' point for a three step approach of grapherator graph construction:
#' 1) Add nodes respectively coordinates via \code{\link{addNodes}}, 2) add edges
#' via \code{\link{addEdges}} and finally 3) add edge weights with the function
#' \code{\link{addWeights}}.
#'
#' @param lower [\code{integer(1)}]\cr
#'   Lower bounds for node coordinates in the Euclidean plane.
#' @param upper [\code{integer(1)}]\cr
#'   Upper bounds for node coordinates in the Euclidean plane.
#' @template ret_grapherator
#' @family graph generators
#' @examples
#' # complete graph with one U(10, 20) sampled weight per edge
#' g = graph(0, 10)
#' g = addNodes(g, n = 10, generator = addNodesUniform)
#' g = addEdges(g, generator = addEdgesComplete)
#' g = addWeights(g, generator = addWeightsRandom, method = runif, min = 10, max = 20)
#' \dontrun{
#' do.call(gridExtra::grid.arrange, plot(g, show.edges = FALSE))
#' }
#'
#' # we extend the graph by adding another weight which is based
#' # on the Euclidean distance between the node coordinates
#' g = addWeights(g, generator = addWeightsDistance, method = "euclidean")
#' \dontrun{
#' do.call(gridExtra::grid.arrange, plot(g, show.edges = FALSE))
#' }
#'
#' # next we generate a graph with each two weights per edge which resembles
#' # a street network. The edge weights have a positive correlation.
#' g = graph(0, 100)
#' g = addNodes(g, n = 5, generator = addNodesLHS)
#' g = addNodes(g, n = c(10, 10, 15, 20, 50), by.centers = TRUE,
#'   generator = addNodesUniform, lower = c(0, 0), upper = c(10, 10))
#' g = addEdges(g, generator = addEdgesDelauney, type = "intracluster")
#' g = addEdges(g, generator = addEdgesDelauney, type = "intercluster", k = 4L)
#' g = addWeights(g, generator = addWeightsCorrelated, rho = 0.6)
#' \dontrun{
#' print(g)
#' do.call(gridExtra::grid.arrange, plot(g, show.edges = FALSE))
#' }
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
    n.edges = 0L,
    n.weights = 0L,
    node.types = character(0L),
    edge.types = character(0L),
    weight.types = character(0L),
    degrees = NULL,
    weights = list(),
    membership = NULL,
    coordinates = NULL,
    classes = "grapherator")
}

#' @export
print.grapherator = function(x, ...) {
  catf("GRAPHERATOR GRAPH")
  catf("#nodes           : %i (%s)", getNumberOfNodes(x), BBmisc::collapse(x$node.types))
  catf("#edges           : %i (%s)", getNumberOfEdges(x), BBmisc::collapse(x$edge.types))
  n.clusters = getNumberOfClusters(x)
  if (n.clusters > 0L)
    catf("#clusters:         %i", n.clusters)
  n.weights = length(x$weights)
  catf("#weights per edge: %i (%s)", getNumberOfWeights(x), BBmisc::collapse(x$weight.types))
}
