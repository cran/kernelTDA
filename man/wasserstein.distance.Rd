% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wasserstein_distance.R
\name{wasserstein.distance}
\alias{wasserstein.distance}
\title{L_p q-Wasserstein Distance}
\usage{
wasserstein.distance(d1, d2, dimension, q, p = 2)
}
\arguments{
\item{d1}{A persistence diagram (matrix with 3 col where the first one is the dimension, the second is the birth-time and the third is the death-time).}

\item{d2}{A persistence diagram (matrix with 3 col where the first one is the dimension, the second is the birth-time and the third is the death-time).}

\item{dimension}{Dimension of the topological features of interest (0 for connected components, 1 for cycles etc).}

\item{q}{Order of the q-Wasserstein distance.}

\item{p}{Order of the L_p norm to be used as a ground metric in the computation of the Wasserstein distance.}
}
\value{
The value for the L_p q-Wassesterstein between \code{d1} and \code{d2}.
}
\description{
Compute the q-Wasserstein distance between persistence diagrams using an arbitrary L_p norm 
as ground metric.
}
\details{
This function provides an R interface for the efficient C++ library `HERA` by Michael Kerber, Dmitriy Morozov, and Arnur Nigmetov (\url{https://bitbucket.org/grey_narn/hera/src/master/}).
}
\examples{
diag1 <- matrix(c(1,1,1,0,2,3,2,2.5,4), ncol = 3, byrow = FALSE)
diag2 <- matrix(c(1,1,0,1,1,2), ncol = 3, byrow = FALSE)
wasserstein.distance(diag1, diag2, dimension = 1, q = 1, p = 2)
}
\references{
\insertRef{kerber2017geometry}{kernelTDA}
}
\author{
Tullia Padellini, Francesco Palini. The included C++ library is authored by Michael Kerber, Dmitriy Morozov, and Arnur Nigmetov
}
