\name{StatComp21081-package}
\alias{StatComp21081-package}
\docType{package}
\title{
  Model Averaging and prediction
}
\description{
  A package with a model averaging function, and after model averaging, a prediction function is avaliable for predicting new data.
}
\details{
  Function ModelAve() averages models, and PredictMA() predicts with new data. Auxiliary function `ff2nC()` generate an index matrix. 
}
\author{
Jianming Wu

Maintainer: Jianming Wu <matheming@mail.ustc.edu.cn>
}
\references{
  Hansen, B. E. (2007). Least squares model averaging. Econometrica, 75(4), 1175-1189.
  
  Hansen, B. E., & Racine, J. S. (2012). Jackknife model averaging. Journal of Econometrics, 167(1), 38-46.
}
\keyword{Model Averaging}
\examples{
  \dontrun{
n <- 10
p <-3
X <- data.frame(matrix(rnorm(n*p), n, p))
beta <- c(1, 2, 3)
y <- X \%*\% beta
Data <- cbind(y, X)
subset <- matrix(c(1,0,0,0,1,0),ncol=p)
get_MA<-MA(y~., Data, method="MMA", Subset = subset)
PredictMA(data.frame(matrix(rnorm(5*p),ncol=p)),get_MA)
  }
}
