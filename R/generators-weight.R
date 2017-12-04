addWeightsConcave = function(graph, ...) {
  assertClass(graph, "grapherator")
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

  if (!is.null(graph$adj.mat)) {
    ww1[!graph$adj.mat] = 1e6
    ww2[!graph$adj.mat] = 1e6
  }

  return(list(weights = list(ww1, ww2), generator = "concave"))
}

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

  return(list(weights = list(ww.euc, Y), generator = sprintf("%.2f-correlated", rho)))
}

addWeightsDistance = function(graph, method, ...) {
  assertClass(graph, "grapherator")
  assertChoice(method, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "random"))

  if (is.null(graph$coordinates))
    stopf("Method '%s' needs coordinates.", method)

  ww = as.matrix(dist(graph$coordinates, method = method, ...))

  if (!is.null(graph$adj.mat))
    ww[!graph$adj.mat] = 1e6 #FIXME: numeric infinity value

  return(list(weights = list(ww), generator = sprintf("distance-%s", method)))
}

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

  return(list(weights = list(ww), generator = "random"))
}
