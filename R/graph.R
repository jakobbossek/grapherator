#' @title Generate a bare multi-objective graph.
#'
#' @description This function generates a bare multi-objective graph. The generated
#' object does not contain nodes, edges or edge weights. It serves as a starting
#' point for a three step-approach of multi-objective graph problem construction:
#' 1) Add nodes respectively coordinates via \code{\link{addCoordinates}}, add edges
#' via \code{\link{addEdges}} and finally add edge weights with the function
#' \code{\link{addWeights}}.
#'
#' @param lower [\code{integer(1)}]\cr
#'   Lower bounds for coordinates.
#' @param upper [\code{integer(1)}]\cr
#'   Upper bounds for coordinates.
#' @template ret_grapherator
#' @family graph generators
#' @export
graph = function(lower, upper) {
  #n = asInt(n, lower = 2L)
  if (length(lower) == 1L)
    lower = rep(lower, 2L)
  if (length(upper) == 1L)
    upper = rep(upper, 2L)
  assertNumeric(lower, len = 2L, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(upper, len = 2L, any.missing = FALSE, all.missing = FALSE)
  if (any(lower >= upper))
    stopf("grapherator: all elements of lower need to be stricly lower than the corresponding
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
  catf("MULTI-OBJECTIVE GRAPH PROBLEM")
  catf("Number of nodes: %i", x$n.nodes)
  if (x$n.clusters > 0L)
    catf("Number of clusters: %i", x$n.clusters)
  n.weights = length(x$weights)
  catf("Weights per edge: %i (%s)", n.weights, BBmisc::collapse(x$weight.types))
}

#' @title Coordinate generators.
#'
#' @description Functions for the placement of node coordinates in the
#' euclidean plane. Function \code{addNodesLHS} generates a space-filling
#' latin hypercube sample, \code{addNodesUniform} samples points from a
#' bivariate uniform distribution, \code{addNodesGrid} generates a regular
#' grid of points, \code{addNodesTriangular} generates a regular triangular
#' grid and \code{addNodesNormal} generates nodes on basis of a normal
#' distribution.
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of points to generate.
#' @param lower [\code{numeric(2)}]\cr
#'   Minimal values for the first and second coordinates respectively.
#'   Default is 0.
#' @param upper [\code{numeric(2)}]\cr
#'   Maximal values for the first and second coordinates respectively.
#'   Default is 1.
#' @param method [\code{function}]\cr
#'   Function from package \pkg{lhs}.
#'   Default is \code{\link[lhs]{maximinLHS}}.
#' @param x.mean [\code{numeric}]\cr
#'   Mean value of normal distribution for x-value generation.
#'   Only relevant for \code{\link{addNodesNormal}}.
#' @param x.sd [\code{numeric}]\cr
#'   Standard deviation of normal distribution for x-value generation.
#'   Only relevant for \code{\link{addNodesNormal}}.
#' @param y.mean [\code{numeric}]\cr
#'   Mean value of normal distribution for y-value generation.
#'   Only relevant for \code{\link{addNodesNormal}}.
#' @param y.sd [\code{numeric}]\cr
#'   Standard deviation of normal distribution for y-value generation.
#'   Only relevant for \code{\link{addNodesNormal}}.
#' @return [\code{matrix(n, 2)}] Matrix of node coordinates.
#' @rdname coordGenerators
#' @name coordGenerators
#' @export
addNodesLHS = function(n, lower = 0, upper = 1, method = NULL) {
  if (is.null(method)) {
    requirePackages("lhs", why = "mcMST::addNodesLHS", default.method = "load")
    method = lhs::maximinLHS
  }

  coords = method(n, 2L)
  # stretch
  coords = lower + (upper - lower) * coords
  return(list(coords = coords, generator = "LHSNG"))
}

#' @export
#' @rdname coordGenerators
addNodesUniform = function(n, lower, upper) {
  coords = lapply(seq_len(2L), function(i) {
    runif(n, min = lower[i], max = upper[i])
  })
  coords = do.call(cbind, coords)
  return(list(coords = coords, generator = "UNG"))
}

#' @export
#' @rdname coordGenerators
addNodesTriangular = function(n, lower, upper) {
  m = sqrt(n)
  # determine offset of each second line
  d = (upper[1] - lower[1]) / (m - 1)
  d = d / 2
  print(d)
  print(m)
  offset = rep(c(rep(0, m), rep(d, m)), m)[1:n]
  print(offset)
  coords = addNodesGrid(n, lower, upper)
  coords[, 1L] = coords[, 1L] + offset
  return(list(coords = coords, generator = "TNG"))
}

#' @export
#' @rdname coordGenerators
addNodesGrid = function(n, lower, upper) {
  m = sqrt(n)
  x1 = seq(lower[1], upper[1], length.out = m)
  x2 = seq(lower[2], upper[2], length.out = m)
  coords = expand.grid(x1, x2)
  names(coords) = NULL
  coords = as.matrix(coords)
  return(list(coords = coords, generator = "GNG"))
}

#' @export
#' @rdname coordGenerators
addNodesNormal = function(n, lower, upper, x.mean, x.sd, y.mean, y.sd) {
  x1 = rnorm(n, x.mean, x.sd)
  x2 = rnorm(n, y.mean, y.sd)

  x1 = pmin(pmax(x1, lower[1L]), upper[1L])
  x2 = pmin(pmax(x2, lower[2L]), upper[2L])

  coords = cbind(x1, x2)
  return(list(coords = coords, generator = "NNG"))
}

#' @title Add node coordinates to graph.
#'
#' @description Places node coordinates in the two-dimensional euclidean plane.
#'
#' @template arg_grapherator
#' @param n [\code{integer}]\cr
#'   Number of coordinates to place. If \code{by.centers} is \code{FALSE} a single
#'   integer value is expected. Otherwise, a vector v may be passed. In this case
#'   v[i] coordinates are generated for each cluster. However, if a single value is
#'   passed and \code{by.center == TRUE}, each cluster is assigned the same number of
#'   coordinates.
#' @param coordinates [\code{matrix(n, 2)}]\cr
#'   Matrix of coordinates (each row is one point).
#'   Default is \code{NULL}. If this is set, setting of \code{generator}, \code{by.centers},
#'   and \code{par.fun} are ignored. This parameter is handy, if one wants to add
#'   coordinates by hand.
#'   Default is \code{NULL}.
#' @param generator [\code{function(n, ...)}]\cr
#'   Function used to generate coordinates. The generator needs to expect the number
#'   of points to generate as the first argument \code{n}. Additional control argument are
#'   possible.
#' @param by.centers [\code{logical(1)}]\cr
#'   Should coordinates be placed for each cluster center seperately? This enables
#'   geneation of clustered coordinates.
#'   Default is \code{FALSE}.
#' @param par.fun [\code{function(cc) | NULL}]\cr
#'   Optional function which is applied to each cluster center \code{cc} before the generation
#'   of coordinates in case \code{by.centers} is \code{TRUE}. This enables to specifically
#'   determine additional parameters for the \code{generator} for each cluster.
#' @param ... [any]\cr
#'   Furhter arguments passed down to \code{generator}.
#' @template ret_grapherator
#' @family graph generators
#' @export
addCoordinates = function(graph, n, generator, coordinates = NULL, by.centers = FALSE, par.fun = NULL, ...) {
  assertClass(graph, "grapherator")
  if (!is.null(coordinates)) {
    assertMatrix(coordinates, mode = "numeric", min.rows = 1L, ncols = 2L, any.missing = FALSE, all.missing = FALSE)
    n = nrow(coordinates)
  }
  n = asInt(n, lower = 1L)
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
    n = rep(n, nc)
    node.type = NULL
    coords = lapply(seq_len(nc), function(i) {
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
      return(coords.cluster)
    })
    # concatenate coordinates
    coords = do.call(rbind, coords)
    # assign membership (we know which cluster belongs to which center)
    membership = rep(1:nc, each = n[1L])
  }
  # update meta data of graph
  graph$n.nodes = if (!is.null(graph$n.nodes)) graph$n.nodes + sum(n) else sum(n)
  graph$coordinates = if (!is.null(graph$coordinates)) rbind(graph$coordinates, coords) else coords
  graph$membership = if (!is.null(graph$membership)) c(graph$membership, membership)
  graph$node.types = c(graph$node.types, node.type)
  return(graph)
}

#' @title Define edges in multi-objective graph.
#'
#' @description By default \code{\link{addWeights}} generates n(n-1)/2 weights, i.e.,
#' the graph is assumed to be complete. This method allows to defne an adjacency
#' matrix to make the graph more sparse. The method can be applied multiple times
#' with different parameterizations.
#'
#' @template arg_grapherator
#' @param method [\code{function(...)}]\cr
#'   Method applied to \code{graph} in order to determine which edges to keep.
#'   Possible values are \dQuote{onion}, \dQuote{delauney}, \dQuote{wayman} or \dQuote{grid}.
#' @param type [\code{character(1)}]\cr
#'   Value \dQuote{all} applies \code{method} to all nodes. Value \dQuote{intreacluster}
#'   instead applies the method for each cluster separately. Lastly, value \dQuote{intercluster}
#'   applies \code{method} to the cluster centers.
#' @param ... [any]\cr
#'   Passed down to edge constructor.
#' @family graph generators
#' @template ret_grapherator
addEdges = function(graph, method, type = "all", ...) { # nocov start
  assertClass(graph, "grapherator")
  assertFunction(method)
  assertChoice(type, choices = c("all", "intercluster", "intracluster"))

  if (graph$n.weights > 0L)
    stopf("addEdges: add edges before adding weights.")

  adj.mat = NULL
  edge.type = NULL

  if (is.null(graph$adj.mat)) {
    adj.mat = matrix(FALSE, ncol = graph$n.nodes, nrow = graph$n.nodes)
    graph$adj.mat = adj.mat
  } else {
    adj.mat = graph$adj.mat
  }

  if (graph$n.clusters == 0L & type %in% c("intracluster", "intercluster"))
    stopf("addEdges: type {intra,inter}cluster only possible if nodes are clustered.")

  if (type == "intracluster") {
    # apply stuff clusterwise
    clusters = seq_len(graph$n.clusters)
    # for each cluster ...
    for (cluster in clusters) {
      # get all points in that cluster
      idx.cluster = which(graph$membership == cluster)
      # apply addEdge method to subset of cluster points
      res.adj.mat = do.call(method, c(list(n = length(idx.cluster), coordinates = graph$coordinates[idx.cluster, , drop = FALSE]), list(...)))
      cl.adj.mat = res.adj.mat$adj.mat
      edge.type = res.adj.mat$generator
      # update adjacency matrix
      adj.mat[idx.cluster, idx.cluster] = adj.mat[idx.cluster, idx.cluster] | cl.adj.mat
    }
    graph$adj.mat = adj.mat
    edge.type = sprintf("CL%s", edge.type)
  } else if (type == "intercluster") {
    idx.centers = graph$center.ids
    res.centers = do.call(method, c(list(n = length(idx.centers), coordinates = graph$center.coordinates), list(...)))
    centers.adj.mat = res.centers$adj.mat
    edge.type = res.centers$generator
    adj.mat[idx.centers, idx.centers] = adj.mat[idx.centers, idx.centers] | centers.adj.mat
    graph$adj.mat = adj.mat
  } else {
    graph$adj.mat = adj.mat | do.call(method, c(list(n = graph$n.nodes, coordinates = graph$coordinates), list(...)))
  }
  graph$edge.types = c(graph$edge.types, edge.type)
  return(graph)
  # # k-konvex hull approach
  # #FIXME: make this flexible
  # #FIXME: we want to be able to apply this only to edges between cluster centers
  # # and within clusters
  # the.adj.mat
  # if (method == "onion") {
  #   the.adj.mat = addEdgesOnion(graph, ...)
  # # Delauney triangulation
  # } else if (method == "delauney") {
  #   the.adj.mat = addEdgesDelauney(graph, ...)
  # # waxman model
  # } else if (method == "waxman") {
  #   the.adj.mat = addEdgesWaxman(graph, ...)
  # } else if (method == "grid") {
  #   the.adj.mat = addEdgesGrid(graph, ...)
  # }

  # # can add edges multiple times
  # graph$adj.mat = adj.mat | the.adj.mat
  # return(graph)
} # nocov end

# complete edge generator
addEdgesComplete = function(n, coordinates, ...) {
  adj.mat = matrix(TRUE, nrow = n, ncol = n)
  diag(adj.mat) = FALSE
  return(list(adj.mat = adj.mat, generator = "CEG"))
}

# Edge generator for grid layout
addEdgesGrid = function(n, coordinates, ...) {
  # get euclidean coordinates
  euc.dists = as.matrix(dist(coordinates))

  # assure min.dist is not zero
  diag(euc.dists) = Inf
  min.dist = min(euc.dists)

  adj.mat = matrix(FALSE, nrow = n, ncol = n)
  # we need to add a small constant here
  adj.mat[euc.dists <= min.dist + 1e-10] = TRUE
  return(list(adj.mat = adj.mat, generator = "GEG"))
}

# Edge generator by repeated convex hull computation.
addEdgesOnion = function(n, coordinates, ...) {
  adj.mat = matrix(FALSE, nrow = n, ncol = n)

  coordinates2 = coordinates
  # which coordinates are already done?
  coords.done = rep(FALSE, n)
  # indizes of coordinates not yet "onioned"
  # Needed as mapping for indices of coordinate matrix
  idx = which(!coords.done)
  n.edges = 0L
  while (TRUE) {
    # compute hull of remaining points
    ch = chull(coordinates2[!coords.done, , drop = FALSE])
    n.edges = n.edges + length(ch) - 1L
    # close hull
    ch = c(ch, ch[1L])
    # set edges
    for (i in (seq_along(ch) - 1L)) {
      adj.mat[idx[ch[i]], idx[ch[i + 1L]]] = TRUE
      adj.mat[idx[ch[i + 1L]], idx[ch[i]]] = TRUE
    }
    # update managing stuff
    coords.done[idx[ch]] = TRUE
    idx = which(!coords.done)
    if (sum(coords.done) == n)
      break
  }
  print(n.edges)
  return(list(adj.mat = adj.mat, generator = "OEG"))
}

# Edge generator based on Delauney triangulation.
addEdgesDelauney = function(n, coordinates, ...) {
  adj.mat = matrix(FALSE, nrow = n, ncol = n)
  requirePackages("deldir", why = "mcMST:addEdges")
  # compute triangulation
  # The 5, 6 colums contains the indizes of the points
  dt = deldir(as.data.frame(coordinates))$delsgs[, 5:6]
  for (i in seq_row(dt)) {
    adj.mat[dt[i, 1L], dt[i, 2L]] = TRUE
    adj.mat[dt[i, 2L], dt[i, 1L]] = TRUE
  }
  return(list(adj.mat = adj.mat, generator = "DEG"))
}

# Waxman's probablistic edge generator.
#
# @param alpha [\code{numeric(1)}]\cr
#   Average degree of nodes.
# @param beta [\code{numeric(1)}]\cr
#   Scale between short and long edges.
addEdgesWaxman = function(n, coordinates, alpha = 0.5, beta = 0.5, ...) {
  # get euclidean distances (only necessary for probability computation)
  euc.dists = as.matrix(dist(coordinates))
  # maximal euclidean distance
  max.dist = max(euc.dists)
  # sanity checks on parameters
  alpha = assertNumber(alpha, lower = 0.01, upper = 1)
  beta = assertNumber(beta, lower = 0.01, upper = 1)

  probs = alpha * exp(-euc.dists / (beta * max.dist))
  adj.mat = matrix(runif(n * n), nrow = n) < probs
  # asure symmetry and loop-free property
  diag(adj.mat) = FALSE
  adj.mat[lower.tri(adj.mat)] = t(adj.mat)[lower.tri(adj.mat)]

  return(list(adj.mat = adj.mat, generator = "WEG"))
}

addEdgesSpanningTree = function(n, coordinates, runs = 1L, ...) {
  # compute a spanning tree
  dist.mat = as.matrix(dist(coordinates))

  # init empty adjacency matrix
  adj.mat = matrix(FALSE, nrow = n, ncol = n)

  for (run in seq_len(runs)) {
    stree = vegan::spantree(dist.mat)

    # build edges
    edges = cbind(2:n, stree$kid)

    # set edges
    for (i in 1:(n - 1L)) {
      adj.mat[edges[i, 1L], edges[i, 2L]] = adj.mat[edges[i, 2L], edges[i, 1L]] = TRUE
      dist.mat[edges[i, 1L], edges[i, 2L]] = dist.mat[edges[i, 2L], edges[i, 1L]] = NA
    }
  }

  return(list(adj.mat = adj.mat, generator = "STEG"))
}

#' @title Add weights to a multi-objective graph.
#'
#' @description \code{addWeights} allows to generate edge weights for a multi-objective
#' graph instance. The weights can be generated on basis of the node coordinates (in this
#' case \code{\link[stats]{dist}} is applied with the cooresponding \code{method}).
#' Alternatively, all kinds of random weights can be generated.
#'
#' @template arg_grapherator
#' @param method [\code{character(1)}]\cr
#'   Method used to generate weights. Possible values are \dQuote{euclidean}, \dQuote{maximum},
#'   \dQuote{manhatten}, \dQuote{canberra}, \dQuote{binary}, \code{minkowski} or \code{random}.
#'   The latter generates (random) weights utilizing \code{weight.fun}. The remaining
#'   options are passed down to \code{\link[stats]{dist}}, i.e., weights are generated
#'   as distances between the node coordinates.
#' @param weights [\code{matrix}]\cr
#'   Square matrix of weights.
#'   If some weights are already assigned, pay attention to the correct dimensions.
#'   If this is passed all other arguments are ignored.
#'   Default is \code{NULL}.
#' @param weight.fun [\code{function(m, ...) | NULL}]\cr
#'   Function used to generate weights. The first arument needs to be number of weights
#'   to generate.
#' @param symmetric [\code{logical(1)}]\cr
#'   Should the weights be symmetric, i.e., w(i, j) = w(j, i) for each pair i, j of nodes?
#'   Default is \code{TRUE}.
#' @param to.int [\code{logical(1)}]\cr
#'   Should weights be rounded to integer?
#'   Default is \code{FALSE}.
#' @param rho [\code{numeric(1)}]\cr
#'   Correlation of edges weights for \code{method} \dQuote{correlated}.
#'   Default is \code{0.5}.
#' @param ... [any]\cr
#'   Additional arguments passed down to \code{weight.fun} or \code{\link[stats]{dist}}. See
#'   documentation of argument \code{method} for details.
#' @template ret_grapherator
#' @family graph generators
#' @export
addWeights = function(graph, method = "euclidean", weights = NULL, weight.fun = NULL, symmetric = TRUE, to.int = FALSE, rho = 0.5, ...) {
  assertClass(graph, "grapherator")
  assertChoice(method, choices = c("correlated", "concave", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "random"))
  assertFlag(to.int)
  assertNumber(rho, lower = -1, upper = 1)

  n.nodes = graph$n.nodes
  if (n.nodes == 0)
    stopf("addWeights: first place nodes/coordinates.")

  if (!is.null(weights))
    assertMatrix(weights, nrows = n.nodes, ncols = n.nodes, mode = "numeric")

  ws = graph$weights
  n.weights = if (is.null(ws)) 0L else length(ws)

  if (!is.null(weights)) {
    graph$weights[[n.weights + 1L]] = weights
    graph$n.weights = graph$n.weights + 1L
    graph$weight.types = c(graph$weight.types, "unknown")
  } else if (method == "correlated") {
    # get euclidean coordinates
    ww.euc = as.matrix(dist(graph$coordinates, method = "euclidean", ...))
    ww.euc.num = as.numeric(ww.euc)
    m = length(ww.euc.num)
    W = matrix(
      c(
        rep(1, m),
        ww.euc.num,
        runif(m, -1, 1)
      ),
    byrow = FALSE,
    ncol = 3L)

    # QR-decomposition
    Q = qr.Q(qr(W))

    T = matrix(c(1, rho, sqrt(1 - rho^2)), ncol = 3L)
    Y = T %*% t(Q)

    # normalize Y
    Y = (Y * graph$upper[1L])
    Y = Y + abs(min(Y)) + 10
    Y = matrix(Y, ncol = nrow(ww.euc))
    diag(Y) = 0

    if (symmetric)
      Y[lower.tri(Y)] = t(Y)[lower.tri(t(Y))]

    if (!is.null(graph$adj.mat)) {
      ww.euc[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value
      Y[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value
    }

    graph$weights[[n.weights + 1L]] = ww.euc
    graph$weights[[n.weights + 2L]] = Y
    graph$n.weights = graph$n.weights + 2L
    graph$weight.types = c(graph$weight.types, c("euclidean", sprintf("%.2f-correlated", rho)))
  } else if (method == "concave") {
    n = graph$n.nodes
    xhi = 10
    nu = 20
    M = 100

    degrees = rowSums(graph$adj.mat)
    # first node is the one with highest degree
    n1 = which.max(degrees)

    # choose second node as the one with highest degree among all
    # nodes adjacent to the first node
    adj.to.n1 = which(graph$adj.mat[n1, ])
    idx.n2 = which.max(degrees[adj.to.n1])
    n2 = adj.to.n1[idx.n2]

    # select third node as the one
    adj.to.n2 = which(graph$adj.mat[n2, ])

    adj.to.n1 = setdiff(adj.to.n1, n2)
    adj.to.n2 = setdiff(adj.to.n2, n1)

    # nodes adjacent to both n1 and n2
    adj.to.n1n2 = intersect(adj.to.n1, adj.to.n2)
    if (length(adj.to.n1n2) > 0) {
      idx.n3 = which.max(degrees[adj.to.n1n2])
    } else {
      adj.to.n1n2 = unique(c(adj.to.n1, adj.to.n2))
      idx.n3 = which.max(degrees[adj.to.n1n2])
    }
    n3 = adj.to.n1n2[idx.n3]

    ns = c(n1, n2, n3)

    ww1 = matrix(1e6, nrow = n, ncol = n)
    ww2 = matrix(1e6, nrow = n, ncol = n)

    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j)
          next
        if (!(i %in% ns) & !(j %in% ns)) {
          ww1[i, j] = sample(xhi:nu, 1L)
          ww2[i, j] = sample(xhi:nu, 1L)
        } else if (xor(i %in% ns, j %in% ns)) {
          ww1[i, j] = sample((M - xhi):M, 1L)
          ww2[i, j] = sample((M - xhi):M, 1L)
        } else if (i == n1 & j == n2) {
          ww1[i, j] = ww2[i, j] = xhi
        } else if (i == n1 & j == n3) {
          ww1[i, j] = 1
          ww2[i, j] = M - xhi
        } else if (i == n2 & j == n3) {
          ww1[i, j] = M - xhi
          ww2[i, j] = 1
        }
      }
    }

    if (symmetric) {
      ww1[lower.tri(ww1)] = t(ww1)[lower.tri(t(ww1))]
      ww2[lower.tri(ww2)] = t(ww2)[lower.tri(t(ww2))]
    }

    if (!is.null(graph$adj.mat)) {
      ww1[!graph$adj.mat] = 1e6
      ww2[!graph$adj.mat] = 1e6
    }

    graph$weights[[n.weights + 1L]] = ww1
    graph$weights[[n.weights + 2L]] = ww2

    graph$n.weights = graph$n.weights + 2L
    graph$weight.types = c(graph$weight.types, "concave")
  } else if (method != "random") {
    if (is.null(graph$coordinates))
      stopf("Method '%s' needs coordinates.", method)
    ww = as.matrix(dist(graph$coordinates, method = method, ...))

    if (!is.null(graph$adj.mat))
      ww[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value

    graph$weights[[n.weights + 1L]] = ww
    graph$n.weights = graph$n.weights + 1L
    graph$weight.types = c(graph$weight.types, "distance")
  } else {
    if (is.null(weight.fun))
      stopf("You need to pass a weight fun.")

    # always generate n^2 weights
    m = n.nodes * n.nodes

    ww = weight.fun(m, ...)

    #if (!is.null(adj.mat)) {
    ww = matrix(ww, ncol = n.nodes, nrow = n.nodes)
    diag(ww) = .0
    if (symmetric) {
      ww[lower.tri(ww)] = t(ww)[lower.tri(t(ww))]
    }

    if (!is.null(graph$adj.mat))
      ww[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value
    graph$weights[[n.weights + 1L]] = ww
    graph$n.weights = graph$n.weights + 1L
    graph$weight.types = c(graph$weight.types, "random")
  }

  return(graph)
}
