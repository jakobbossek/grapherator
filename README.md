# grapherator: Modular graph generation

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/grapherator)](http://cran.r-project.org/web/packages/grapherator)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grapherator)](http://cran.rstudio.com/web/packages/grapherator/index.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/grapherator?color=orange)](http://cran.rstudio.com/web/packages/grapherator/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/grapherator.svg?branch=master)](https://travis-ci.org/jakobbossek/grapherator)
[![Build status](https://ci.appveyor.com/api/projects/status/f83u7suaxqxmtc80/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/mcmst/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakobbossek/grapherator/badge.svg?branch=master)](https://coveralls.io/github/jakobbossek/grapherator?branch=master)

## Introduction

...

## Example

Here we first generate a bi-criteria graph problem with n = 25 nodes. The first objective is the euclidean distance of node coordinates in the euclidean plane [0, 10] x [0, 10]. The second objective follows a normal distribution (N(5, 1.5)). 
```r
set.seed(1)
g = mcGP(lower = 0, upper = 10)
g = addCoordinates(g, n = 25, generator = coordUniform)
g = addWeights(g, method = "euclidean")
g = addWeights(g, method = "random", weight.fun = rnorm, mean = 5, sd = 1.5)
print(g)
```

Next, we apply the genetic algorithm proposed by Zhou & Gen with population size `mu = 10` and number of offspring `lambda = 10` for `max.iter = 100` generations.
```r
library(ggplot2)
res = mcMSTEmoaZhou(g, mu = 10L, lambda = 10L, max.iter = 100L)
ecr::plotFront(res$pareto.front)
```
See the package vignettes for more details.

## Installation Instructions

Install the [CRAN](http://cran.r-project.org) release version via:
```r
install.packages("grapherator")
```
If you are interested in trying out and playing around with the current development version use the [devtools](https://github.com/hadley/devtools) package and install directly from GitHub:

```r
install.packages("devtools", dependencies = TRUE)
devtools::install_github("jakobbossek/grapherator")
```

## Contributing to grapherator

If you encounter problems using this software, e.g., bugs or insufficient/misleading documentation, or you simply have a question, feel free to open an issue in the [issue tracker](https://github.com/jakobbossek/grapherator/issues).
In order to reproduce potential problems, please provide a minimal and reproducible code example.

Contributions to this software package are welcome via [pull requests](https://help.github.com/articles/about-pull-requests/) and will be merged at the sole discretion of the author. 

## Related work

The following packages provide some methods to generate random graphs:

* [igraph: Network Analysis and Visualization](https://cran.r-project.org/package=igraph)


