library(methods)
library(devtools)
library(testthat)
library(smoof)
library(ggplot2)
library(gridExtra)
library(microbenchmark)
library(magrittr)
library(threejs)
library(checkmate)

load_all(".")

set.seed(1)

  g = graph(0, 100)
  n = 25L
  g = addNodes(g, n = n, generator = addNodesUniform)

  edge.setups = list(
    list(generator = addEdgesDelauney, pars = list()),
    list(generator = addEdgesWaxman, pars = list(alpha = 0.3, beta = 0.1)),
    list(generator = addEdgesGilbert, pars = list(p = 0.3)),
    list(generator = addEdgesErdosRenyi, pars = list(m = floor(n / 2 - 4))),
    list(generator = addEdgesSpanningTree, pars = list(runs = 3L)),
    list(generator = addEdgesComplete, pars = list()),
    #list(generator = addEdgesOnion, pars = list()),
    list(generator = addEdgesGrid, pars = list())
  )

  i = 1
  for (edge.setup in edge.setups) {
    edge.generator = edge.setup$generator
    catf("edge generator %i", i)
    i = i + 1
    pars = c(list(graph = g, generator = edge.generator), edge.setup$pars)
    g1 = do.call(addEdges, pars)
    adj.mat = getAdjacencyMatrix(g1)
    expect_true(!is.null(adj.mat))
    expect_true(isSymmetricMatrix(adj.mat))
    expect_true(all(diag(adj.mat) == 0))
  }

stop()

g7 = graph(lower = c(0, 0), upper = c(100, 100))
g7 = addNodes(g7, n = 25, generator = addNodesGrid)
g7 = addEdges(g7, generator = addEdgesDelauney)
#g7 = addEdges(g7, generator = addEdgesGilbert, p = 0.04)
g7 = addEdges(g7, generator = addEdgesErdosRenyi, m = 10)
g7 = addWeights(g7, generator = addWeightsRandom, method = runif, min = 10, max = 100)
pls = plot(g7, weight.plot.type = "eadf")
do.call(grid.arrange, pls)
stop()
#g7 = addWeights(g7, generator = addWeightsRandom, method = runif, min = 10, max = 100)
#g7 = addWeights(g7, generator = addWeightsConcave)

pls = plot(g7)
do.call(grid.arrange, pls)

stop(37578)

g = graph(lower = c(0, 0), upper = c(100, 100))
g = addNodes(g, n = 10, generator = addNodesUniform)
g = addNodes(g, n = 29, by.centers = TRUE, generator = addNodesUniform, lower = c(0, 0), upper = c(10, 10))
#g = addEdges(g, method = addEdgesWaxman, alpha = 0.2, beta = 0.2, type = "intracluster")
#g = addEdges(g, method = addEdgesDelauney, type = "intracluster")
g = addEdges(g, method = addEdgesWaxman, type = "intercluster", alpha = 1, beta = 1)

g = addEdges(g, method = addEdgesDelauney, type = "intracluster")
#g = addEdges(g, method = addEdgesSpanningTree, runs = 2L, type = "intercluster")
#g = addEdges(g, method = addEdgesDelauney, type = "intercluster")
#g = addEdges(g, method = delauney)
#g = addEdges(g, method = "grid")

#FIXME: correlated weight are not symmetric!!!
#g = addWeights(g, method = "correlated", rho = -0.9)
#g = addWeights(g, method = "euclidean")
g = addWeights(g, method = "random", weight.fun = runif, min = 5, max = 100)
g = addWeights(g, method = "random", weight.fun = runif, min = 20, max = 100)

#g = genRandomMCGP(30)

# check for symmetry
all(g$weights[[1L]] == t(g$weights[[1L]]))
all(g$weights[[2L]] == t(g$weights[[2L]]))
