#' @title Weight generators.
#'
#' @description Function for adding weight(s) to edges. The following functions
#' are implemented and may be passed as argument \code{generator} to \code{\link{addWeights}}:
#' \describe{
#'  \item{\code{addWeightsRandom}}{Add purely random weights. Calls the passed \code{method}, e.g., \code{method = runif} to generate weights.}
#'  \item{\code{addWeightsDistance}}{Weights correspond to a distance metric based on the node coordinates
#'  in the Euclidean plane. Internally function \code{\link[stats]{dist}} is called.}
#'  \item{\code{addWeightsCorrelated}}{This method generates two weight matrices with correlated weights. The
#'  correlation may be adjusted by the \code{rho} argument. Here, the first weight of an
#'  edge is the Euclidean distance between the nodes in the plane and the second one
#'  is generated in a way, that the correlation is close to \code{rho}.}
#'  \item{\code{addWeightsCocave}}{This method is interesting for generating bi-objective graphs to
#'  benchmark algorithms for the multi-criteria spanning tree problem. Graphs generated this way expose
#'  a concave Pareto-front.}
#' }
#'
#' @note These functions are not meant to be called directly. Instead, they need
#' to be assigned to the \code{generator} argument of \code{\link{addWeights}}.
#'
#' @template arg_grapherator
#' @param xhi [\code{integer(1)}]\cr
#'   Positive integer for \code{addWeightsConcave}.
#'   Default is 10.
#' @param nu [\code{integer(1)}]\cr
#'   Positive integer for \code{addWeightsConcave}.
#'   Default is 20.
#' @param M [\code{integer(1)}]\cr
#'   Maximum weight for weights generated via \code{addWeightsConcave}.
#'   Note that \code{M} minus \code{xhi} needs to be much bigger than \code{nu}.
#'   Default is 100.
#' @param rho [\code{numeric(1)}]\cr
#'   Desired correlation, i.e., value between -1 and 1, of edge weights for
#'   \code{addWeightsCorrelated}.
#' @param method [\code{character(1)} | \code{function(n, ...)}]\cr
#'   String representing the distance measure to use for \code{addWeightsDistance}
#'   (see \code{method} argument of \code{\link[stats]{dist}}) or \code{function(n, ...)}
#'   used to generate random weights in case of \code{addWeightsRandom}.
#' @param ... [any]
#'   Further arguments. Not used at the moment.
#'   This may be useful for user-written weight generators.
#' @return [\code{list}] A list with components
#' \describe{
#'   \item{weights [\code{list}]}{List of weight matrices. Even in the case of one weight matrix
#'   it is wrapped in a list of length one.}
#'   \item{generator [\code{character(1)}]}{String description of the generator used.}
#' }
#' @export
#' @rdname weightGenerators
addWeightsConcave = function(graph, xhi = 10, nu = 20, M = 100, ...) {
  assertClass(graph, "grapherator")
  xhi = asInt(xhi, lower = 1)
  nu = asInt(nu, lower = 1)
  M = asInt(M, lower = 1)

  if (nu <= xhi)
    stopf("addWeightsConcave: nu > xhi is required.")

  if (M - xhi <= nu)
    stopf("addWeightsConcave: M - xhi >> nu is required.")

  n = graph$n.nodes
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

  if (!is.null(graph$adj.mat)) {
    ww1[!graph$adj.mat] = 1e6
    ww2[!graph$adj.mat] = 1e6
  }

  return(list(weights = list(ww1, ww2), generator = "CONC"))
}

#' @export
#' @rdname weightGenerators
addWeightsCorrelated = function(graph, rho, ...) {
  assertClass(graph, "grapherator")
  assertNumber(rho, lower = -1, upper = 1)

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

  if (!is.null(graph$adj.mat)) {
    ww.euc[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value
    Y[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value
  }

  return(list(weights = list(ww.euc, Y), generator = sprintf("%.2f-COR", rho)))
}

#' @export
#' @rdname weightGenerators
addWeightsDistance = function(graph, method, ...) {
  assertClass(graph, "grapherator")
  assertChoice(method, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "random"))

  if (is.null(graph$coordinates))
    stopf("Method '%s' needs coordinates.", method)

  ww = as.matrix(dist(graph$coordinates, method = method, ...))

  if (!is.null(graph$adj.mat))
    ww[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value

  return(list(weights = list(ww), generator = sprintf("DIST", method)))
}

#' @export
#' @rdname weightGenerators
addWeightsRandom = function(graph, method, ...) {
  n = graph$n.nodes
  if (missing(method))
    stopf("addWeightsRandom: You need to pass the method argument.")

  # always generate n^2 weights
  m = n * n

  ww = method(m, ...)

  ww = matrix(ww, ncol = n, nrow = n)
  diag(ww) = .0

  if (!is.null(graph$adj.mat))
    ww[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value

  return(list(weights = list(ww), generator = "RND"))
}
