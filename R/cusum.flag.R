#' Flags aberrant participants based on CUSUM statistics.
#'
#' @param cusum.obj an object returned from cusum or cusum.poly
#' @param cutoff.obj an object returned from cusum.cutoff
#' @param cut a vector for user specified cut offs (e.g c(1,1)). The first value is the upper limit. The second value is the lower limit.
#' @return Returns a true or false matrix whether a person is aberrantly responding.
#' @export

cusum.flag <- function(cusum.obj, cutoff.obj, cut = NULL){
  if(!is.null(cut)){
    upper <- cut[1]
    lower <- cut[2]
  } else{
    upper <- cutoff.obj[1,1]
    lower <- cutoff.obj[2,1]
  }

  chart <- cusum.obj$CusumChart
  up.ind <- apply(chart[seq(1,nrow(chart),by=2),],1,max) > upper
  low.ind <- apply(chart[seq(2,nrow(chart),by=2),],1,min) < lower

  cusum.obj$ability
  result <- matrix(up.ind+low.ind > 0,nrow= length(cusum.obj$ability))
  colnames(result) <- "Flagged"
  rownames(result) <- paste("Person",rep(1:length(cusum.obj$ability)))

  output <- list(result,cutoff.obj)
  class(output) <- "PsyCu"
  return(output)
}
