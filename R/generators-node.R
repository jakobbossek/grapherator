#' @title Node generators.
#'
#' @description Functions for the placement of nodes / node coordinates in the
#' Euclidean plane. Function \code{addNodesLHS} generates a space-filling
#' Latin-Hypercube-Sample (LHS), function \code{addNodesUniform} samples points from a
#' bivariate uniform distribution, \code{addNodesGrid} generates a regular
#' grid/lattice of points, \code{addNodesTriangular} generates a regular triangular
#' grid/lattice and \code{addNodesNormal} generates nodes on basis of a normal
#' distribution.
#'
#' @note These functions are not meant to be called directly. Instead, they need
#' to be assigned to the \code{generator} argument of \code{\link{addNodes}}.
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of nodes to generate.
#' @param lower [\code{numeric(2)}]\cr
#'   Minimal values for the first and second node coordinates respectively.
#'   Default is 0 for both dimensions.
#' @param upper [\code{numeric(2)}]\cr
#'   Maximal values for the first and second node coordinates respectively.
#'   Default is 1 for both dimensions.
#' @param method [\code{function}]\cr
#'   Function from package \pkg{lhs}.
#'   Default is \code{\link[lhs]{maximinLHS}}.
#'   Only relevant for \code{\link{addNodesLHS}}.
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
#' @return [\code{list}] List with components:
#' \describe{
#'   \item{coords [\code{matrix(n, 2)}]}{Matrix of node coordinates.}
#'   \item{generator [\code{character(1)}]}{String description of the generator used.}
#' }
#' @rdname nodeGenerators
#' @name nodeGenerators
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
#' @rdname nodeGenerators
addNodesUniform = function(n, lower, upper) {
  coords = lapply(seq_len(2L), function(i) {
    runif(n, min = lower[i], max = upper[i])
  })
  coords = do.call(cbind, coords)
  return(list(coords = coords, generator = "UNG"))
}

#' @export
#' @rdname nodeGenerators
addNodesTriangular = function(n, lower, upper) {
  m = sqrt(n)
  # determine offset of each second line
  d = (upper[1] - lower[1]) / (m - 1)
  d = d / 2
  offset = rep(c(rep(0, m), rep(d, m)), m)[1:n]
  coords = addNodesGrid(n, lower, upper)$coords
  coords[, 1L] = coords[, 1L] + offset
  return(list(coords = coords, generator = "TNG"))
}

#' @export
#' @rdname nodeGenerators
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
#' @rdname nodeGenerators
addNodesNormal = function(n, lower, upper, x.mean, x.sd, y.mean, y.sd) {
  x1 = rnorm(n, x.mean, x.sd)
  x2 = rnorm(n, y.mean, y.sd)

  x1 = pmin(pmax(x1, lower[1L]), upper[1L])
  x2 = pmin(pmax(x2, lower[2L]), upper[2L])

  coords = cbind(x1, x2)
  return(list(coords = coords, generator = "NNG"))
}
