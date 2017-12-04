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