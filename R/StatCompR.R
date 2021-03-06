#' @title feature screening by spearman rank correlation
#' @description use the spearman rank correlation for ultrahigh dimensional data to reduce the dimension of the predictor variable to a moderate level.  
#' @param X n*p matrix X
#' @param Y n*1 matrix Y
#' @param d integer
#' @return d columns numbers of x associated with y
#' @useDynLib StatComp20010
#' @examples
#' \dontrun{
#' library(MASS)
#' Sig=matrix(0,nrow=1000,ncol=1000)
#' for(i in 1:1000){
#'   for (j in 1:1000) Sig[i,j]=0.8^abs(i-j)
#' }
#' mu=rep(0,1000)
#' x=mvrnorm(100,mu,Sig)
#' beta=cbind(c(c(1,0.8,0.6,0.4,0.2),rep(0,995)))
#' epsion=rnorm(100,0,1)
#' y=x%*%beta+epsion
#' Spearman(x,y,10)
#' }
#' @export
Spearman=function(X,Y,d){   
  n=length(Y)
  p=ncol(X)
  a=rep(0,p)
  for(k in 1:p){
    for(i in 1:n){
      ##Use the spearman rank correlation to estimate the correlation between column
      ##k of predictive variable matrix X and the response variable Y.
      a[k]=a[k]+length(X[X[,k]<=X[i,k],k])*length(Y[Y<=Y[i]])
    }
  }
  A=cbind((a/n^3-1/4),seq(1,p))
  ##The predictive variables are arranged in descending order of correlation coefficient.
  B=A[order(A[,1],decreasing=TRUE),]
  ##Take the first d variables as active variables. 
  C=B[1:d,]
  C
}



#' @title A robust sure independence screening for ultrahigh dimensional data
#' @description A robust sure independence screening for ultrahigh dimensional data and use the hard thresholding rule to reduce the dimension of the predictor variable
#' @param X n*p matrix X
#' @param Y n*1 matrix Y
#' @return some columns numbers of x associated with y
#' @examples
#' \dontrun{
#' library(MASS)
#' Sig=matrix(0,nrow=1000,ncol=1000)
#' for(i in 1:1000){
#'   for (j in 1:1000) Sig[i,j]=0.8^abs(i-j)
#' }
#' mu=rep(0,1000)
#' x=mvrnorm(100,mu,Sig)
#' beta=cbind(c(c(1,0.8,0.6,0.4,0.2),rep(0,995)))
#' epsion=rnorm(100,0,1)
#' y=exp(x%*%beta)+epsion
#' Rosis(x,y)
#' }
#' @export
Rosis=function(X,Y){  
  n=length(Y)
  p=ncol(X)
  a=rep(0,p)
  for(k in 1:p){
    for(i in 1:n){
      ##The RoSIS method is used to estimate the correlation between column k of predictive variable matrix X and the response variable Y.
      a[k]=a[k]+X[i,k]*length(Y[Y<=Y[i]])
    }
  }
  A=cbind((a/n^2)^2,seq(1,p))
  ##The predictive variables are arranged in descending order of correlation coefficient
  B=A[order(A[,1],decreasing=TRUE),]
  ##Take the hard thresholding rule as d=[n/logn] to take first d variables as active variables. 
  d=floor(n/log(n))
  C=B[1:d,]
  C
}




#' @title choose tuning parameters in scad by GCV
#' @description use GCV to choose tuning parameters when using smoothly clipped absolute deviation (SCAD) for variable selection and estimation.
#' @param a vector of tuning parameter a
#' @param lambda vector of tuning parameter lambda
#' @param x n*p matrix x
#' @param y n*1 matrix y
#' @return The a and the lambda that minimize GCV
#' @examples
#' \dontrun{
#' library(MASS)
#' n=40
#' d=8
#' Sig=matrix(0,nrow=d,ncol=d)
#' for(i in 1:d){
#'   for (j in 1:d) Sig[i,j]=0.5^abs(i-j)
#' }
#' mu = rep(0,d)
#' x=mvrnorm(n, mu, Sig)
#' beta=c(3,1.5,0,0,2,0,0,0)
#' epsilon=rnorm(n,0,3)
#' y=x%*%beta+epsilon
#' a=seq(3,4,0.1)
#' lambda=seq(0,1,0.1)
#' c=min_gcv(a,lambda,x,y)
#' c
#' }
#' @export
min_gcv=function(a,lambda,x,y){
  ##Define the derivative of the SCAD penalty function
  p_lambda_d=function(theta,a,lambda){
    l=length(theta)
    p1=rep(0,l)
    for(i in 1:l){
      if(abs(theta[i])>lambda){
        if(a*lambda>=abs(theta[i])){
          p1[i]=(a*lambda-theta[i])/((a-1)*lambda)
        }
        else{
          p1[i]=0
        }
      }
      else{
        p1[i]=lambda
      }
    }
    return(p1)
  }
  ##Define the Diagonal matrix for the SCAD iterative algorithm.
  Sigmaf=function(beta,a,lambda) {
    l=length(beta)
    Sigmabeta=matrix(0,l,l)
    diag(Sigmabeta)=p_lambda_d(abs(beta),a,lambda)/abs(beta)
    return (Sigmabeta)
  }
  ##for given a and lambda to calculate GCV value use scad method.
  gcv=function(a,lambda,x,y){
    n=length(y)
    beta0=solve(t(x)%*%x)%*%t(x)%*%y
    betahat=beta0
    {
      if(min(abs(betahat))<1e-5){
        r=which(abs(betahat)<1e-5)
        z=x[,-r]
        betahat=betahat[-r]
      }
      else {
        z=x
        betahat=betahat
      }
    }
    iter=0
    while((max(abs(t(z)%*%z%*%betahat-t(z)%*%y+n*p_lambda_d(abs(betahat),a,lambda)*sign(betahat)))>0.1)&&(iter<=1000)){
      betahat=solve(t(z)%*%z+n*Sigmaf(betahat,a,lambda))%*%t(z)%*%y
      if(min(abs(betahat))<1e-5){
        r=which(abs(betahat)<1e-5)
        z=z[,-r]
        betahat=betahat[-r]
      }
      iter=iter+1
    }
    a=t(y-z%*%betahat)%*%(y-z%*%betahat)
    b=(1-sum(diag(z%*%solve(t(z)%*%z+n*Sigmaf(betahat,a,lambda))%*%t(z)))/n)^2
    return(a/b/n) ##GCV value
  }
  G=matrix(0,nrow=length(a),ncol=length(lambda))
  for (i in 1:length(a)) {
    for(j in 1:length(lambda)){
      ##Calculate the GCV value for each a and lambda value.
      G[i,j]=gcv(a[i],lambda[j],x,y)
    }
  }
  Ga=G-min(G)
  k=0
  l=0
  for(i in 1:length(a)){
    for(j in 1:length(lambda)){
      if(Ga[i,j]==0){
        k=i
        l=j
      }
    }
  }
  ##The a and lambda values that minimize the GCV value.
  return(list(a=a[k],lambda=lambda[l]))
}




