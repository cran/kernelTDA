% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/krein_svm.R
\name{krein.svm.default}
\alias{krein.svm.default}
\title{Krein Support Vector Machine}
\usage{
\method{krein.svm}{default}(
  kernelmat = NULL,
  y = NULL,
  cost = 1,
  class.weights = NULL,
  cross = 0,
  probability = FALSE,
  fitted = TRUE,
  subset,
  ...
)
}
\arguments{
\item{kernelmat}{the kernel matrix computed for all observations}

\item{y}{a vector of labels}

\item{cost}{cost of violating the constraint}

\item{class.weights}{a named vector of weights for the different classes, used for asymmetric class sizes. Not all factor levels have to be supplied (default weight: 1). All components have to be named. Specifying "inverse" will choose the weights inversely proportional to the class distribution.}

\item{cross}{number of fold in a k-fold cross validation}

\item{probability}{logical indicating whether the model should allow for probability predictions (default: \code{FALSE}).}

\item{fitted}{logical indicating whether the fitted values should be computed and included in the model or not (default: \code{TRUE})}

\item{subset}{an index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.)}

\item{...}{additional parameters}
}
\value{
An object of class \code{krein.svm} containing the fitted model, including: 
\describe{
  \item{\code{SV}}{a matrix containing the Support Vectors}
  \item{\code{index}}{index of the resulting support vectors in the data matrix}
  \item{\code{coefs}}{a matrix containing corresponding coefficients times the training labels}
  \item{\code{rho}}{value of the (negative) intercept}
}
}
\description{
Solves a kernelized Support Vector Machine in the case where the kernel used may not be positive semidefinite.
}
\details{
This function implements the Krein Support Vector Machine solver as defined by Loosli et al. (2015). 
The implementation of the solver is a modified version of the popular C++ library `LIBSVM`, while the connection to `R` 
heavily relies on the `R`-package \pkg{e1701}.
}
\examples{
## DO NOT RUN:
# library(TDA)
# set.seed(123)
# foo.data = list()
# for(i in 1:20){
#    foo = circleUnif(100)
#    foo.data[[i]] = ripsDiag(foo, 1,1)$diagram}
#    for(i in 21:40){   
#     foo = cbind(runif(100), runif(100))
#     foo.data[[i]] = ripsDiag(foo, 1,1)$diagram
#     }
# GSWkernel = gaus.kernel(foo.data, h =1, dimension = 1,  q = 2)
# GGKclass = krein.svm(kernelmat = GSWkernel, y = rep(c(1,2), c(20,20)))
}
\references{
\insertRef{loosli2015learning}{kernelTDA}

\insertRef{chang2011libsvm}{kernelTDA}

\insertRef{dimitriadou2008misc}{kernelTDA}
}
\author{
Tullia Padellini, Francesco Palini, David Meyer. The included C++ library LIBSVM is authored by Chih-Chung Chang and Chih-Jen Lin)
}
