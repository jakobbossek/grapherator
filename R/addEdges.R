#' @title Add edges to graph.
#'
#' @description This method allows to add edges to a \code{grapherator} graph.
#' The method can be applied multiple times with different parameterizations. E.g.,
#' add edges in clusters first and add edges between clusters in a second step.
#'
#' @references
#' Erdos, P., and A. Renyi. 1959. "On random graphs, I." Publicationes Mathematicae
#' (Debrecen) 6: 290-97.
#'
#' Waxman, B. M. 1988. "Routing of Multipoint Connections."" IEEE Journal on Selected
#' Areas in Communications 6 (9): 1617-22. doi:10.1109/49.12889.
#'
#' Knowles, J. D., and D. W. Corne. 2001. "Benchmark Problem Generators and Results
#' for the Multiobjective Degree-Constrained Minimum Spanning Tree Problem."
#' In Proceedings of the 3rd Annual Conference on Genetic and Evolutionary
#' Computation, 424-31. GECCO'01. San Francisco, CA, USA: Morgan Kaufmann Publishers Inc.
#'
#' @template arg_grapherator
#' @param generator [\code{function(graph, ...)}]\cr
#'   Method applied to \code{graph} in order to determine which edges to add.
#' @param type [\code{character(1)}]\cr
#'   Value \dQuote{all} applies \code{generator} to all nodes. Value \dQuote{intracluster}
#'   instead applies the method for each cluster separately. Value \dQuote{intercluster}
#'   selects each \code{k} nodes from each cluster and applies \code{generator} to the union.
#'   Lastly, value \dQuote{intercenter} selects the cluster centers exclusively.
#'   Default is \dQuote{all}.
#' @param k [\code{integer} | \code{NULL}]\cr
#'   Integer vector specifying the number of nodes selected randomly from each cluster
#'   to be selected for edge construction. May be a scalar value or a vector of length
#'   \code{graph$n.clusters}. NAs are allowed and indicate clusters to be ignored.
#' @param ... [any]\cr
#'   Further arguments passed down to edge generator \code{generator}.
#' @family graph generators
#' @template ret_grapherator
#' @export
addEdges = function(graph, generator, type = "all", k = NULL, ...) {
  assertClass(graph, "grapherator")
  assertFunction(generator)
  assertChoice(type, choices = c("all", "intercluster", "intracluster", "intercenter"))

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

  if (type == "intercluster") {
    if (is.null(k))
      stopf("addEdges: for type 'intercluster' parameter k must be set.")
    if (length(k) == 1L)
      k = rep(k, graph$n.clusters)
    assertInteger(k, lower = 1L, any.missing = TRUE, all.missing = FALSE)

    # apply stuff clusterwise
    clusters = seq_len(graph$n.clusters)
    # gather ids of nodes to consider
    used.nodes = integer(0L)
    for (cluster in clusters) {
      # get all points in that cluster
      idx.cluster = which(graph$membership == cluster)
      # NAs indicate cluster skipping
      if (is.na(k[cluster]))
        next
      if (k[cluster] > length(idx.cluster))
        stopf("addEdges: k[%i] is larger than the %i-th cluster.", cluster, cluster)
      idx.cluster = sample(idx.cluster, size = k[cluster], replace = FALSE)
      used.nodes = c(used.nodes, idx.cluster)
    }
    # generate temporary graph
    graph2 = graph
    graph2$coordinates = graph$coordinates[used.nodes, , drop = FALSE]
    graph2$n.nodes = length(used.nodes)
    # apply addEdge method to subset of cluster points
    res.adj.mat = do.call(generator, c(list(graph = graph2), list(...)))
    cl.adj.mat = res.adj.mat$adj.mat
    edge.type = res.adj.mat$generator
    # update adjacency matrix
    adj.mat[used.nodes, used.nodes] = adj.mat[used.nodes, used.nodes] | cl.adj.mat
    graph$adj.mat = adj.mat
    edge.type = sprintf("CL%s", edge.type)
  } else if (type == "intracluster") {

    # apply stuff clusterwise
    clusters = seq_len(graph$n.clusters)
    # for each cluster ...
    for (cluster in clusters) {
      # get all points in that cluster
      idx.cluster = which(graph$membership == cluster)
      if (!is.null(k) & type == "intercluster") {
        # NAs indicate cluster skipping
        if (is.na(k[cluster]))
          next
        idx.cluster = sample(idx.cluster, size = k, replace = FALSE)
      }

      # generate temporary graph
      graph2 = graph
      graph2$coordinates = graph$coordinates[idx.cluster, , drop = FALSE]
      graph2$n.nodes = length(idx.cluster)
      # apply addEdge method to subset of cluster points
      res.adj.mat = do.call(generator, c(list(graph = graph2), list(...)))
      cl.adj.mat = res.adj.mat$adj.mat
      edge.type = res.adj.mat$generator
      # update adjacency matrix
      adj.mat[idx.cluster, idx.cluster] = adj.mat[idx.cluster, idx.cluster] | cl.adj.mat
    }
    graph$adj.mat = adj.mat
    edge.type = sprintf("CL%s", edge.type)
  } else if (type == "intercenter") {
    idx.centers = graph$center.ids

    # generate temporary graph
    graph2 = graph
    graph2$coordinates = graph$center.coordinates
    graph2$n.nodes = length(idx.centers)

    res.centers = do.call(generator, c(list(graph = graph2), list(...)))
    centers.adj.mat = res.centers$adj.mat
    edge.type = res.centers$generator
    adj.mat[idx.centers, idx.centers] = adj.mat[idx.centers, idx.centers] | centers.adj.mat
    graph$adj.mat = adj.mat
  } else {
    res = do.call(generator, c(list(graph = graph), list(...)))
    edge.type = res$generator
    graph$adj.mat = adj.mat | res$adj.mat
  }
  graph$n.edges = sum(graph$adj.mat[upper.tri(graph$adj.mat)])
  graph$edge.types = c(graph$edge.types, edge.type)
  graph$degree = rowSums(graph$adj.mat, na.rm = TRUE)
  return(graph)
}
