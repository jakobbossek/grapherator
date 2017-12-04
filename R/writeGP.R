#' @title Export/import multi-criteria graph instance.
#'
#' @description Given a \code{mcGP} graph instance function \code{\link{writeGP}}
#' saves the graph to a file. Function \code{\link{readGP}} imports a graph
#' given a filename.
#'
#' @details Instances are stored in a format similar to the one used
#' by Cardoso et al. in their MOST project. Note that all values in each line are separated by comma.
#' First line contains four integer values: number of nodes n, number of edges m,
#' number of clusters cl and number of weights p per edge.
#' The second line contains the weight types.
#' The third line contains the node types.
#' The next n lines contain the node coordinates.
#' In case of a clustered instance the next line contains the node to cluster
#' membership mapping.
#' The last m lines contain the following information each:
#' i,j,w1(i,j),...,wp(i,j)
#' I.e., each two node numbers i and j followed by the p weights of the edge
#' (i, j).
#'
#' @template arg_mcGP
#' @param file [\code{character(1)}]\cr
#'   Path to file where the graph problem shall be stored (for \code{\link{writeGP}})
#'   or which contains the graph problem to be imported (for \code{link{readGP}}).
#' @return Function \code{\link{writeGP}} silently returns the passed filename
#' \code{file} whereas \code{\link{writeGP}} returns a \code{mcGP} object.
#' @rdname writeGP
#' @name writeGP
#' @export
writeGP = function(graph, file) {
  assertClass(graph, "mcGP")
  con = file(path.expand(file), "w")
  on.exit(close(con))

  n = graph$n.nodes
  m = if (!is.null(graph$adj.mat)) sum(graph$adj.mat) else (n * n)
  p = graph$n.weights
  cl = graph$n.clusters

  meta = sprintf("%i,%i,%i,%i", n, m, cl, p)
  writeLines(meta, con)

  weight.types = collapse(graph$weight.types, ",")
  cat(weight.types, file = con, sep = "\n")

  node.types = collapse(graph$node.types, ",")
  cat(node.types, file = con, sep = "\n")

  write.table(graph$coordinates, file = con, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)

  # store membership in case the object is clustered
  if (cl > 0) {
    membership = collapse(graph$membership, ",")
    writeLines(membership, con)
  }

  weights = matrix(NA, nrow = m, ncol = 2L + p)

  k = 1L
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (!is.null(graph$adj.mat)) {
        if (!graph$adj.mat[i, j])
          next
      }
      weights[k, ] = c(i, j, sapply(graph$weights, function(weight.mat) weight.mat[i, j]))
      k = k + 1L
    }
  }
  write.table(weights, file = con, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  assert(k == m + 1)
  return(invisible(file))
}

#' @rdname writeGP
#' @export
readGP = function(file) {
  assertFile(file, access = "r")
  con = file(file, "r")
  on.exit(close(con))

  # generate bare graph
  g = mcGP(lower = 0, upper = 100)

  # import meta data
  meta = scan(file = con, what = integer(), n = 4L, sep = ",", quiet = TRUE)
  g$n.nodes    = n  = meta[1L]
  g$n.edges    = m  = meta[2L]
  g$n.clusters = cl = meta[3L]
  g$n.weights  = p  = meta[4L]

  # import weight and node types
  g$weight.types = scan(con, what = character(), n = p, sep = ",", quiet = TRUE)
  g$node.types = strsplit(scan(con, what = character(), n = 1L, quiet = TRUE), ",")[[1L]]

  # import coordinates
  g$coordinates = as.matrix(read.table(con, sep = ",", nrows = n, header = FALSE, stringsAsFactors = FALSE))

  # import cluster membership (if applicable)
  if (cl > 0L) {
    g$membership = scan(file = con, what = integer(), n = n, sep = ",", quiet = TRUE)
    # since cluster centers are allowed only in the first step(s), i.e.,
    # no clusters can be added after clusters were added already we simply
    # can take the first cl coordinates as cluster center coordinates
    g$center.coordinates = g$coordinates[seq_len(cl), , drop = FALSE]
  }

  # import edge->weight mapping
  ww = read.table(con, sep = ",", nrows = m, header = FALSE, stringsAsFactors = FALSE)

  # recreate weight matrices and adjacency matrix
  adj.mat = matrix(FALSE, nrow = n, ncol = n)
  weights = vector(mode = "list", length = p)
  for (i in seq_len(p)) {
    weights[[i]] = matrix(10000, nrow = n, ncol = n)
  }

  for (k in seq_len(m)) {
    i = ww[k, 1L]
    j = ww[k, 2L]
    cur.weight = ww[k, 3:(2 + p)]
    for (l in seq_len(p)) {
      weights[[l]][i, j] = as.numeric(cur.weight[l])
    }
    if (i != j)
      adj.mat[i, j] = TRUE
  }
  g$adj.mat = adj.mat
  g$weights = weights
  return(g)
}
