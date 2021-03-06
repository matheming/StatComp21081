% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelAve.R
\name{ModelAve}
\alias{ModelAve}
\title{A linear model averaging function}
\usage{
ModelAve(
  Formula,
  Data,
  method,
  nested = FALSE,
  Subset = NULL,
  equalweight = FALSE
)
}
\arguments{
\item{Formula}{formula like `y~.` or `y~X1+X2` which mean whole model}

\item{Data}{Data set including response variable and explanatory variables, the first column is response variable}

\item{method}{One of "MMA" and "JMA"}

\item{nested}{Whether using nested model, `default=FALSE`}

\item{Subset}{An index matrix of n*p with 0-1 element, \code{n} is model number and \code{p} is whole explanatory variables number}

\item{equalweight}{Whether use equal weight, `default=FALSE`}
}
\value{
The optimal weight, the coefficients of all models
}
\description{
Model averaging function including MMA and JMA
}
\examples{
\dontrun{
n <- 15
p <-3
X <- matrix(rnorm(n*p), n, p)
beta <- c(1, 2, 3)
y <- X \%*\% beta+rnorm(n)
y<-data.frame(y)
Data <- cbind(y, data.frame(X))
subset <- matrix(c(1,0,0,0,1,0),ncol=p,byrow = TRUE)
ModelAve(y~., Data[1:10,], method="MMA", Subset = subset)
ModelAve(y~., Data[1:10,], method = "MMA", nested = TRUE)
}
}
