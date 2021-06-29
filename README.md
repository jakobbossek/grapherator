
# grapherator: Modular graph generation

<!-- badges: start -->

[![DOI](http://joss.theoj.org/papers/10.21105/joss.00528/status.svg)](https://doi.org/10.21105/joss.00528)
[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/grapherator)](http://cran.r-project.org/web/packages/grapherator)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grapherator)](http://cran.rstudio.com/web/packages/grapherator/index.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/grapherator?color=orange)](http://cran.rstudio.com/web/packages/grapherator/index.html)
[![Build
Status](https://travis-ci.org/jakobbossek/grapherator.svg?branch=master)](https://travis-ci.org/jakobbossek/grapherator)
[![Build
status](https://ci.appveyor.com/api/projects/status/n5iheb2q282lt9y3/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/grapherator/branch/master)
[![Codecov test
coverage](https://codecov.io/gh/jakobbossek/grapherator/branch/master/graph/badge.svg)](https://codecov.io/gh/jakobbossek/grapherator?branch=master)
<!-- badges: end -->

## Introduction

Due to lack of real world data comparisson of algorithms for graph
problems is most often carried out on artificially generated graphs. The
R package **grapherator** implements a modular approach to benachmark
graph generation focusing on undirected, weighted graphs. The graph
generation process follows a three-step procedure:

1.  Node generation,
2.  edge generation and finally
3.  edge weight generation.

Each step may be repeated multiple times with different generators
before the transition to the next step is conducted.

## Example

Here, we first generate a bi-criteria complete graph with n = 25 nodes
and each two conflicting weights per edge. The first weight is the
Euclidean distance of node coordinates in the Euclidean plane \[0, 10\]
x \[0, 10\]. The second objective follows a N(5, 1.5)-distribution,
i.e., a normal distribution with mean 5 and standard deviation 1.5.

``` r
set.seed(1)
g1 = graph(lower = 0, upper = 10)
g1 = addNodes(g1, n = 25, generator = addNodesUniform)
g1 = addWeights(g1, generator = addWeightsDistance, method = "euclidean")
g1 = addWeights(g1, generator = addWeightsRandom, method = rnorm, mean = 5, sd = 1.5)
print(g1)
do.call(gridExtra::grid.arrange, plot(g1))
```

The next example demonstrates multiple iterations of each step to create
a complex, clustered, connected network with different edge generators
applied for establishing links within clusters and between cluster
centers respectively. Detailed description: First, 10 cluster centers
are placed via Latin-Hypercube-Sampling (LHS). Next, each cluster is
crowded with 29 additional nodes by uniform sampling around the cluster
center. Edge generation is done by randomly adding an edge between each
two nodes in a cluster with small probability p = 0.2. To ensure
connectivity, the spanning tree edge generator is applied. The last step
of edge generation connects the clusters via Delauney triangulation. In
a final step, two negatively correlated weights are added.

``` r
set.seed(1)
g2 = graph(lower = 0, upper = 100)
g2 = addNodes(g2, n = 10, generator = addNodesLHS)
g2 = addNodes(g2, n = 29, by.centers = TRUE, generator = addNodesUniform, lower = c(0, 0), upper = c(5, 5))
g2 = addEdges(g2, type = "intracluster", generator = addEdgesGilbert, p = 0.2)
g2 = addEdges(g2, type = "intracluster", generator = addEdgesSpanningTree)
g2 = addEdges(g2, type = "intercenter", generator = addEdgesDelauney)
g2 = addWeights(g2, generator = addWeightsCorrelated, rho = -0.7)
print(g2)
do.call(gridExtra::grid.arrange, plot(g2))
```

The following image shows both example graphs `g1` and `g2`. The
topology is shown in the left column, while a scatterplot of the weights
is visualized in the right column.

![Example
graphs](https://raw.githubusercontent.com/jakobbossek/grapherator/master/images/README_graphs.png)

See the package vignettes for more examples and thorough description.

## Installation Instructions

Install the [CRAN](http://cran.r-project.org) release version via:

``` r
install.packages("grapherator")
```

If you are interested in trying out and playing around with the current
development version use the
[devtools](https://github.com/hadley/devtools) package and install
directly from GitHub:

``` r
install.packages("devtools", dependencies = TRUE)
devtools::install_github("jakobbossek/grapherator")
```

## Contributing to grapherator

If you encounter problems using this software, e.g., bugs or
insufficient/misleading documentation, or you simply have a question,
feel free to open an issue in the [issue
tracker](https://github.com/jakobbossek/grapherator/issues). In order to
reproduce potential problems, please provide a minimal and reproducible
code example.

Contributions to this software package are welcome via [pull
requests](https://help.github.com/articles/about-pull-requests/) and
will be merged at the sole discretion of the author.

## Related work

The following R packages provide some methods to generate random graphs:

-   [igraph: Network Analysis and
    Visualization](https://cran.r-project.org/package=igraph) Includes
    some methods to generate classical Erdos-Renyi random graphs as well
    as more recent models, e.g., small-world graphs.
-   [netgen: Network Generator for Combinatorial Graph
    Problems](https://cran.r-project.org/package=netgen) Contains some
    methods to generate complete graphs especially for benchmarking
    Travelling-Salesperson-Problem solvers.
-   [bnlearn: Bayesian Network Structure Learning, Parameter Learning
    and
    Inference](https://cran.r-project.org/web/packages/bnlearn/index.html)
    Function `bnlearn::random.graph` implements some algorithms to
    create graphs.

More interesting libraries:

-   [Graphcuisine](http://www.aviz.fr/Research/Graphcuisine) Provides an
    interactive Evolutionary Algorithm to create random graphs with
    certain user-guided characteristics or maximal similarity to a given
    baseline graph.
-   [NetworkX](https://networkx.github.io) Powerful python package which
    provides many methods to generate classic and random networks.
