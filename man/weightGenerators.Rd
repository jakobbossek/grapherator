% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generators-weight.R
\name{weightGenerators}
\alias{weightGenerators}
\alias{addWeightsConcave}
\alias{addWeightsCorrelated}
\alias{addWeightsDistance}
\alias{addWeightsRandom}
\title{Weight generators.}
\usage{
addWeightsConcave(graph, xhi = 10, nu = 20, M = 100, ...)

addWeightsCorrelated(graph, rho, ...)

addWeightsDistance(graph, method, ...)

addWeightsRandom(graph, method, ...)
}
\arguments{
\item{graph}{[\code{\link{grapherator}}]\cr
Graph.}

\item{xhi}{[\code{integer(1)}]\cr
Positive integer for \code{addWeightsConcave}.
Default is 10.}

\item{nu}{[\code{integer(1)}]\cr
Positive integer for \code{addWeightsConcave}.
Default is 20.}

\item{M}{[\code{integer(1)}]\cr
Maximum weight for weights generated via \code{addWeightsConcave}.
Note that \code{M} minus \code{xhi} needs to be much bigger than \code{nu}.
Default is 100.}

\item{...}{[any]
Further arguments. Not used at the moment.
This may be useful for user-written weight generators.}

\item{rho}{[\code{numeric(1)}]\cr
Desired correlation, i.e., value between -1 and 1, of edge weights for
\code{addWeightsCorrelated}.}

\item{method}{[\code{character(1)} | \code{function(n, ...)}]\cr
String representing the distance measure to use for \code{addWeightsDistance}
(see \code{method} argument of \code{\link[stats]{dist}}) or \code{function(n, ...)}
used to generate random weights in case of \code{addWeightsRandom}.}
}
\value{
[\code{list}] A list with components
\describe{
  \item{weights [\code{list}]}{List of weight matrices. Even in the case of one weight matrix
  it is wrapped in a list of length one.}
  \item{generator [\code{character(1)}]}{String description of the generator used.}
}
}
\description{
Function for adding weight(s) to edges. The following functions
are implemented and may be passed as argument \code{generator} to \code{\link{addWeights}}:
\describe{
 \item{\code{addWeightsRandom}}{Add purely random weights. Calls the passed \code{method}, e.g., \code{method = runif} to generate weights.}
 \item{\code{addWeightsDistance}}{Weights correspond to a distance metric based on the node coordinates
 in the Euclidean plane. Internally function \code{\link[stats]{dist}} is called.}
 \item{\code{addWeightsCorrelated}}{This method generates two weight matrices with correlated weights. The
 correlation may be adjusted by the \code{rho} argument. Here, the first weight of an
 edge is the Euclidean distance between the nodes in the plane and the second one
 is generated in a way, that the correlation is close to \code{rho}.}
 \item{\code{addWeightsCocave}}{This method is interesting for generating bi-objective graphs to
 benchmark algorithms for the multi-criteria spanning tree problem. Graphs generated this way expose
 a concave Pareto-front.}
}
}
\note{
These functions are not meant to be called directly. Instead, they need
to be assigned to the \code{generator} argument of \code{\link{addWeights}}.
}
