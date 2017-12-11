#' @title Add nodes to graph.
#'
#' @description Places node coordinates in the two-dimensional Euclidean plane.
#'
#' @template arg_grapherator
#' @param n [\code{integer}]\cr
#'   Number of nodes to place. If \code{by.centers} is \code{FALSE} a single
#'   integer value is expected. Otherwise, a vector \eqn{v} may be passed. In this case
#'   \eqn{v[i]} coordinates are generated for each cluster. However, if a single value is
#'   passed and \code{by.center == TRUE}, each cluster is assigned the same number of
#'   nodes.
#' @param coordinates [\code{matrix(n, 2)}]\cr
#'   Matrix of coordinates (each row is one node/point).
#'   Default is \code{NULL}. If this is set, setting of \code{generator}, \code{by.centers},
#'   and \code{par.fun} are ignored. This parameter is handy, if one wants to add
#'   coordinates by hand.
#'   Default is \code{NULL}.
#' @param generator [\code{function(graph, ...)}]\cr
#'   Function used to generate nodes. The functions needs to expect the number
#'   of points to generate as the first argument \code{n}. Additional control argument are
#'   possible.
#' @param by.centers [\code{logical(1)}]\cr
#'   Should coordinates be placed for each cluster center seperately? This enables
#'   generation of clustered graphs.
#'   Default is \code{FALSE}.
#' @param skip.centers [\code{integer}]\cr
#'   Optional IDs of cluster centers not to consider in clustered node generation, i.e.,
#'   if \code{by.centers = TRUE}.
#'   Default is not to skip any cluster.
#' @param par.fun [\code{function(cc) | NULL}]\cr
#'   Optional function which is applied to each cluster center before the generation
#'   of coordinates in case \code{by.centers} is \code{TRUE}. This enables to specifically
#'   determine additional parameters for the \code{generator} for each cluster.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{generator}.
#' @template ret_grapherator
#' @family graph generators
#' @export
addNodes = function(graph, n, generator, coordinates = NULL, by.centers = FALSE, skip.centers = integer(0L), par.fun = NULL, ...) {
  assertClass(graph, "grapherator")

  if (graph$n.edges > 0L)
    stopf("grapherator::addNodes: add nodes before adding edges.")

  if (!is.null(coordinates)) {
    assertMatrix(coordinates, mode = "numeric", min.rows = 1L, ncols = 2L, any.missing = FALSE, all.missing = FALSE)
    n = nrow(coordinates)
  }
  assertIntegerish(n, lower = 1L, min.len = 1L)
  if (length(n) > 1L) {
    if (length(n) != graph$n.nodes)
      stopf("grapherator::addNodes: n should be a single integer or a vector of graph$n.nodes integers.")
  }
  assertFlag(by.centers)
  assertFunction(par.fun, null.ok = TRUE)

  membership = NULL
  node.type = NULL

  # Helper function which aligns points with lower left point in [0,0].
  #
  # @param cluster.centers [matrix(2, n)]
  #   Matrix of city coordinates.
  # @return [matrix(2, n)]
  moveToOrigin = function(cluster.centers) {
    offset = abs(apply(cluster.centers, 2L, min))
    t(t(cluster.centers) + offset)
  }

  # if coordinates are passed, ignore the rest and add them
  if (!is.null(coordinates)) {
    coords = coordinates
    membership = if (graph$n.clusters == 0) rep(0, n) else rep(max(graph$membership) + 1L, n)
    node.type = "MANNG"
  }
  # if no two-phase approach simply delegate to coordinate generator
  else if (!by.centers) {
    res = generator(n, lower = graph$lower, upper = graph$upper, ...)
    coords = res$coords
    node.type = res$generator
    membership = if (graph$n.clusters == 0) rep(0, n) else rep(max(graph$membership) + 1L, n)
  # otherwise use existing coordinates as cluster centers and place around them
  } else {
    # use current nodes as center coordinates
    center.coordinates = graph$coordinates
    if (graph$n.clusters > 0L)
      stopf("Currently one can add clusters only once!")
    nc = graph$n.nodes
    graph$n.clusters = nc
    graph$center.coordinates = center.coordinates
    graph$center.ids = 1:nc
    # currently we allow only one "level" of clustering
    # thus we can set the membership here already
    graph$membership = 1:nc

    # sanity check skip.centers
    if (length(skip.centers) > 0L) {
      infeasible.centers = setdiff(skip.centers, 1:graph$n.clusters)
      if (length(infeasible.centers) > 0L)
        stopf("addNodes: cluster centers %s shall be skipped, but there are only %i clusters.", collapse(infeasible.centers), graph$n.clusters)
    }

    n = if (length(n) == 1L) rep(n, nc) else n
    node.type = NULL
    coords = vector(mode = "list", length = nc)
    membership = integer()
    for (i in seq_len(nc)) {
      if (i %in% skip.centers)
        next
      gen.args = list(n = n[i])
      # generate coordinates in origin
      if (!is.null(par.fun))
        gen.args = c(gen.args, par.fun(center.coordinates[i, ]))
      gen.args = c(gen.args, list(...))
      res.cluster = do.call(generator, gen.args)
      coords.cluster = res.cluster$coords
      node.type = res.cluster$generator

      #coords.cluster = moveToOrigin(coords.cluster)
      rects = apply(apply(coords.cluster, 2L, range), 2L, diff)
      cl.center = center.coordinates[i, ]
      # now move the way that centers are in fact centers
      #FIXME: ugly as hell
      coords.cluster = t(t(coords.cluster) + cl.center - rects / 2)
      coords[[i]] = coords.cluster
      membership = c(membership, rep(i, nrow(coords.cluster)))
    }
    # concatenate coordinates
    coords = do.call(rbind, coords)
    # assign membership (we know which cluster belongs to which center)

    #membership = rep(1:nc, n)
  }
  # update meta data of graph
  graph$n.nodes = if (!is.null(graph$n.nodes)) graph$n.nodes + sum(n) else sum(n)
  graph$coordinates = if (!is.null(graph$coordinates)) rbind(graph$coordinates, coords) else coords
  graph$membership = if (!is.null(graph$membership)) c(graph$membership, membership)
  graph$node.types = c(graph$node.types, node.type)
  return(graph)
}
