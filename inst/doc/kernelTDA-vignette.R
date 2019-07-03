## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----toy_examples--------------------------------------------------------
library(TDA)
x1 = circleUnif(100)
x2 = cbind(runif(100), runif(100))

## ------------------------------------------------------------------------
diag1 = ripsDiag(x1, maxdimension = 1, maxscale = 1)$diagram
diag2 = ripsDiag(x2, maxdimension = 1, maxscale = 1)$diagram

## ---- echo = F, fig.align='center', fig.height= 4, fig.width= 7----------
par(bty = "n")
par(mfrow = c(1,2))
plot(x1, pch = 20, main = "Model 1 - Sample", xlab = expression(X[1]), ylab = expression(X[2]))

plot(x2, pch = 20, main = "Model 2 - Sample", xlab = expression(X[1]), ylab = expression(X[2]))

par(mfrow = c(1,1))

## ------------------------------------------------------------------------
library(kernelTDA)
pi1 = pers.image(diag1, nbins = 20, dimension = 1, h = 1)
pi2 = pers.image(diag2, nbins = 20, dimension = 1, h = 1)

## ---- fig.align='center', echo = F, fig.height= 4, fig.width= 7----------
par(bty = "n")
par(mfrow = c(1,2))
plot(diag1, main = "Model 1 - Persistence Diagram")
image(pi1, main  = "Model 1 - Persistence Image", col = viridis::viridis(100))

plot(diag2, main = "Model 2 - Persistence Diagram")
image(pi2, main  = "Model 2 - Persistence Image", col = viridis::viridis(100))
par(mfrow = c(1,1))

## ----wass----------------------------------------------------------------
wasserstein.distance(d1 = diag1, d2 = diag2, dimension = 1, q = 1, p = 2)

## ---- echo = F-----------------------------------------------------------
set.seed(123)
foo.data = list()
for(i in 1:20){
  foo = circleUnif(100)
  foo.data[[i]] = ripsDiag(foo, 1,1)$diagram
}

for(i in 21:40){
  foo = cbind(runif(100), runif(100))
  foo.data[[i]] = ripsDiag(foo, 1,1)$diagram
}

## ---- fig.align = "center", fig.height= 5, fig.width= 4------------------
GSWkernel = gaus.kernel(foo.data, h =1, dimension = 1)
image(GSWkernel, col = viridis::viridis(100, option = "A"), main = "Kernel Matrix", axes = F)


## ------------------------------------------------------------------------
library(kernlab)
kmatGSW = as.kernelMatrix(GSWkernel)
GSWclust = specc(kmatGSW, centers = 2)

## ------------------------------------------------------------------------
GSWclust@.Data

## ------------------------------------------------------------------------
PSSkernel = pss.kernel(foo.data, h =0.1, dimension = 1)
kmatPSS = as.kernelMatrix(PSSkernel)
PSSclass = ksvm(x = kmatPSS, y = rep(c(1,2), c(20,20)) )
PSSclass

## ------------------------------------------------------------------------
GGKclass = krein.svm(kernelmat = GSWkernel, y = rep(c(1,2), c(20,20)))

#accuracy:
mean(GGKclass$fitted == rep(c(1,2), c(20,20)))

