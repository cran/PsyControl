#' Generates critical values for CUSUM statisitcs.
#'
#' \code{cusum.cutoff} Generates a bootstrap sample for cut-off scores.
#' @param cusum.obj an object returned from cusum or cusum.poly
#' @param upp user specified upper tail cut off. Default is .975
#' @param low user specified lower tail cut off. Default is .025
#' @param Breps number of bootstrap samples
#' @return Returns a matrix of lower and upper cut off values and corresponding standard deviations based on bootstrap sample.
#' @export
#' @importFrom stats quantile median sd

cusum.cutoff <- function(cusum.obj, upp = .975, low = .025, Breps = 1000){

    cusum.chart <- cusum.obj$CusumChart
    N <- nrow(cusum.chart)/2
    bounds <- matrix(NA ,2, Breps)

  for(k in 1:Breps){
    bounds[1,k] <- quantile(sample(apply(cusum.chart[seq(1,N,by=2),],1,max),N,TRUE),upp)
    bounds[2,k] <- quantile(sample(apply(cusum.chart[seq(2,N,by=2),],1,min),N,TRUE),low)
  }

  result <- t(matrix(c(median(bounds[1,]),
  sd(bounds[1,]),
  median(bounds[2,]),
  sd(bounds[2,])),2,2))

  colnames(result) <- c("Cut off","CI")
  row.names(result) <- c(paste(upp,"%"),paste(low,"%"))

  return(result)
}
