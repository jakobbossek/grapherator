#' Edge generators.
#'
#' @description Function to add edges into a graph. The following methods
#' are implemented so far:
#' \describe{
#'   \item{\code{addEdgesComplete}}{Generates a simple complete graph. I.e., an edge
#'   exists between each two nodes. However, no self-loops or multi-edges are included.}
#'   \item{\code{addEdgesGrid}}{Only usefull if nodes are generated via \code{\link{addNodesGrid}}.
#'   This method generates a Manhattan-like street network.}
#'   \item{\code{addEdgesOnion}}{This method determines the nodes on the convex hull
#'   of the node cloud in the euclidean plane and adds edges between neighbour nodes.
#'   Ignoring all nodes on the hull, this process is repeated iteratively resulting in an
#'   onion like peeling topololgy. Note that the graph is not connected! In order to
#'   ensure connectivity, another edge generator must be applied in addition, e.g.,
#'   \code{addEdgesSpanningTree}.}
#'   \item{\code{addEdgesDelauney}}{Edges are determined by means of a Delauney triangulation
#'   of the node coordinates in the Euclidean plane.}
#'   \item{\code{addEdgesWaxman}}{Edges are generated using the Waxman-model, i.e., the
#'   probability \eqn{p_{ij}} for the edge \eqn{(i, j)} is given by
#'   \deqn{p_{ij} = \alpha e^{-\beta d_{ij}}},
#'   where \eqn{\alpha, \beta \geq 0} are control parameters and \eqn{d_{ij}} is the
#'   Euclidean distance of the nodes \eqn{i} and \eqn{j}.}
#'   \item{\code{addEdgesSpanningTree}}{A minimum spanning tree is computed based on
#'   a complete random weight matrix. All edges of the spanning tree are added. If \code{runs}
#'   is greater 1, the process is repeated for \code{runs}. However, already added edges are
#'   ignored in subsequent runs.
#'   This method is particularly useful to assist probablistic methods, e.g., Waxman model,
#'   in order to generate connected graphs.}
#'   \item{\code{addEdgesGilbert}}{Use Gilbert-model to generate edges. I.e., each edge is
#'   added with probability \eqn{p \in [0, 1]}.}
#'   \item{\code{addEdgesErdosRenyi}}{In total \eqn{m \leq n(n-1)/2} edges are added at random.}
#' }
#'
#' @note These functions are not meant to be called directly. Instead, they need
#' to be assigned to the \code{generator} argument of \code{\link{addEdges}}.
#'
#' @details Currently all edge generators create symmetric edges only.
#'
#' @template arg_grapherator
#' @param alpha [\code{numeric(1)}]\cr
#'   Positive number indicating the average degree of nodes in the Waxman model.
#'   Default is 0.5.
#' @param beta [\code{numeric(1)}]\cr
#'   Positive number indicating the scale between short and long edges in the Waxman model.
#'   Default is 0.5.
#' @param runs [\code{integer(1)}]\cr
#'   Number of runs to perform by \code{\link{addEdgesSpanningTree}}.
#'   Default is \code{1}.
#' @param m [\code{integer(1)}]\cr
#'   Number of edges to sample for Erdos-Renyi graphs.
#'   Must be at most \eqn{n(n-1)/2} where \eqn{n} is the number of nodes of \code{graph}.
#' @param p [\code{numeric(1)}]\cr
#'   Probability for each edge \eqn{(v_i, v_j), i, j = 1, \ldots, n} to be added
#'   for Gilbert graphs.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{list}] List with components:
#' \describe{
#'   \item{adj.mat \code{matrix}}{Adjacency matrix.}
#'   \item{generator [\code{character(1)}]}{String description of the generator used.}
#' }
#' @export
#' @rdname edgeGenerators
#' @name edgeGenerators
addEdgesComplete = function(graph, ...) {
  assertClass(graph, "grapherator")
  n = graph$n.nodes
  adj.mat = matrix(TRUE, nrow = n, ncol = n)
  diag(adj.mat) = FALSE
  return(list(adj.mat = adj.mat, generator = "CEG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesGrid = function(graph, ...) {
  assertClass(graph, "grapherator")
  coordinates = graph$coordinates
  n = graph$n.nodes

  # get euclidean coordinates
  euc.dists = as.matrix(dist(coordinates))

  # assure min.dist is not zero
  diag(euc.dists) = Inf
  min.dist = min(euc.dists)

  adj.mat = matrix(FALSE, nrow = n, ncol = n)
  # we need to add a small constant here
  adj.mat[euc.dists <= min.dist + 1e-10] = TRUE
  diag(adj.mat) = FALSE
  return(list(adj.mat = adj.mat, generator = "GEG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesOnion = function(graph, ...) {
  assertClass(graph, "grapherator")
  n = graph$n.nodes
  coordinates = graph$coordinates
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
    ch = grDevices::chull(coordinates2[!coords.done, , drop = FALSE])
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
  return(list(adj.mat = adj.mat, generator = "OEG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesDelauney = function(graph, ...) {
  assertClass(graph, "grapherator")
  n = graph$n.nodes
  coordinates = graph$coordinates
  adj.mat = matrix(FALSE, nrow = n, ncol = n)
  # compute triangulation
  # The 5, 6 colums contains the indizes of the points
  df.deldir = as.data.frame(coordinates)
  colnames(df.deldir) = c("x", "y")
  dt = deldir::deldir(df.deldir)$delsgs[, 5:6]
  for (i in seq_row(dt)) {
    adj.mat[dt[i, 1L], dt[i, 2L]] = TRUE
    adj.mat[dt[i, 2L], dt[i, 1L]] = TRUE
  }
  return(list(adj.mat = adj.mat, generator = "DEG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesWaxman = function(graph, alpha = 0.5, beta = 0.5, ...) {
  assertClass(graph, "grapherator")
  assertNumber(alpha, lower = 0)
  assertNumber(beta, lower = 0)

  coordinates = graph$coordinates
  n = graph$n.nodes

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

#' @export
#' @rdname edgeGenerators
addEdgesGilbert = function(graph, p, ...) {
  assertClass(graph, "grapherator")
  assertNumber(p, lower = 0, upper = 1)

  n = getNumberOfNodes(graph)
  adj.mat = matrix(runif(n * n), nrow = n) < p
  diag(adj.mat) = FALSE

  # assure symmetric edges
  adj.mat[lower.tri(adj.mat)] = t(adj.mat)[lower.tri(t(adj.mat))]

  return(list(adj.mat = adj.mat, generator = "GilEG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesErdosRenyi = function(graph, m, ...) {
  assertClass(graph, "grapherator")
  n = getNumberOfNodes(graph)
  m = asInt(m, upper = as.integer(n * (n - 1) / 2))

  # possible edges are all n(n-1)/2 edges
  # Thus, get indizes (i, j) of all upper triangular matrix indizes
  edges.possible = which(upper.tri(matrix(, n, n)) == TRUE, arr.ind = TRUE)

  # sample m edges without replacement
  edges.sampled = sample(1:nrow(edges.possible), size = m)
  edges.sampled = edges.possible[edges.sampled, , drop = FALSE]

  # init empty adjacency matrix
  adj.mat = matrix(FALSE, ncol = n, nrow = n)

  # add edges
  adj.mat[edges.sampled] = TRUE

  # add backward edges (we always create symmetric graphs)
  edges.sampled = cbind(edges.sampled[, 2L], edges.sampled[, 1L])
  adj.mat[edges.sampled] = TRUE

  return(list(adj.mat = adj.mat, generator = "EREG"))
}

#' @export
#' @rdname edgeGenerators
addEdgesSpanningTree = function(graph, runs = 1L, ...) {
  assertClass(graph, "grapherator")
  runs = asInt(runs, lower = 1L)

  coordinates = graph$coordinates
  n = graph$n.nodes

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
      dist.mat[edges[i, 1L], edges[i, 2L]] = dist.mat[edges[i, 2L], edges[i, 1L]] = Inf
    }
  }

  return(list(adj.mat = adj.mat, generator = "STEG"))
}
