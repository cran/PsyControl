probs.plm <- function(thetas, ipar){

  a <- ipar[,1]
  b <- ipar[,2]
  c <- ipar[,3]

  N <- length(thetas); m <- length(b)
  theta <- matrix(thetas,N,m)
  a <- matrix(a,N,m,byrow=T)
  b <- matrix(b,N,m,byrow=T)
  c <- matrix(c,N,m,byrow=T)
  P <- matrix(NA,N,m,byrow=T)

  for (j in 1:m)  {
    for (i in 1:N) {
      P[i,j] <- c[i,j] + (1-c[i,j])/(1 + exp(-1*a[i,j]*(theta[i,j] - b[i,j])))
    }
  }
  return(P)
}

probs.grm <- function(theta, parm){
  N <- length(theta)
  m <- nrow(parm)
  K <- ncol(parm)
  a <- parm[,1]; b <- parm[,-1]
  P.star <- array(dim=c(N,K-1,m))
  P <- array(dim=c(N,K,m))

  for (i in 1:(K-1)) {
    par.temp <- cbind(a,b[,i],rep(0,length(a)))
    P.star[,i,] <- probs.plm(theta,par.temp)
  }

  P[,1,] <- 1 - P.star[,1,]
  for (k in 2:(K-1)) {
    P[,k,] <- P.star[,k-1,] - P.star[,k,] }
  P[,K,] <- P.star[,K-1,]

  return(list(P.star=P.star, P=P))
}

probs.gpcm <- function(theta, ipar, NCat){
  p <- array(NA,c(length(theta),nrow(ipar),NCat))
  for(i in 1:length(theta)){
    for(j in 1:nrow(ipar)){
      temp <- rep(0,NCat)
      temp[2:NCat] <- ipar[1,ncol(ipar)]*(theta[i]-ipar[j,-NCat])
      num <- exp(cumsum(temp))
      den <- sum(num)
      p[i,j,]<- num/den
    }
  }
  return(p)
}

print.PsyCu <- function(x) print(x[1])
