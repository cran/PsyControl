#' Generates CUSUM plot for specified IDs.
#'
#' @param cu.object an object returned from cusum or cusum.poly
#' @param ID a numeric ID.
#' @return Returns a plot for specified cusum person chart.
#' @export
#' @import graphics


cusum.plot <- function(cu.object, ID){
  cu.dat <- cu.object$CusumChart
  c.max <- cu.dat[seq(1,nrow(cu.dat),by=2),]
  c.min <- cu.dat[seq(2,nrow(cu.dat),by=2),]
  plot(0:ncol(cu.dat),c(0,c.max[ID,]),type = "l", col= "green",
    xlim=c(0, ncol(cu.dat)), ylim=c(min(c.max[ID,])-.5, max(c.max[ID,])+.5),
    ylab= "CUSUM", xlab = "Item", main = paste("Chart Number",ID))
  lines(0:ncol(cu.dat),c(0,c.min[ID,]),type = "l",col = "red")
}
