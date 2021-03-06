% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StatCompR.R
\name{Spearman}
\alias{Spearman}
\title{feature screening by spearman rank correlation}
\usage{
Spearman(X, Y, d)
}
\arguments{
\item{X}{n*p matrix X}

\item{Y}{n*1 matrix Y}

\item{d}{integer}
}
\value{
d columns numbers of x associated with y
}
\description{
use the spearman rank correlation for ultrahigh dimensional data to reduce the dimension of the predictor variable to a moderate level.
}
\examples{
\dontrun{
library(MASS)
Sig=matrix(0,nrow=1000,ncol=1000)
for(i in 1:1000){
  for (j in 1:1000) Sig[i,j]=0.8^abs(i-j)
}
mu=rep(0,1000)
x=mvrnorm(100,mu,Sig)
beta=cbind(c(c(1,0.8,0.6,0.4,0.2),rep(0,995)))
epsion=rnorm(100,0,1)
y=x\%*\%beta+epsion
Spearman(x,y,10)
}
}
