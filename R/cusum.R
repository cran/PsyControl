#' Generates CUSUM values for Rasch, 2PL and 3PL IRT model based on the Van Krimpen-Stoop & Meijer, (2002).
#'
#' @param dat a nxp matrix with n participants and p items. Responses are in 0 1 format.
#' @param ipar a pxk matrix with given item parameters p items and k item parameters. ipar[,1] discrimination; ipar[,2] item difficulty; ipar[,3] guessing-parameter.
#' @param abi a vector n ability. If not provided, estimated using Expected a Posteriori method.
#' @param IRTmodel specify the IRT model ("1PL", "2PL", "3PL"). Default is "2PL"
#' @return Returns matrix with with lower and upper cusum statistics for \code{dat}.
#' @examples
#' data(ex2PL)
#' cusum(dat = ex2PL)
#' @references Van Krimpen-Stoop, E. M., & Meijer, R. R. (2002). Detection of person misfit in computerized adaptive tests with polytomous items. Applied Psychological Measurement, 26(2), 164-180.
#' @export
#' @importFrom irtoys est
#' @importFrom irtoys ability


cusum <- function(dat, ipar = NULL, abi = NULL, IRTmodel = "2PL"){

  model <- est(dat, model = IRTmodel, engine = "ltm")

  if(!is.null(ipar)){
    if(IRTmodel == "1PL"){
      model$est[,2] <- ipar[,2]
    }
    if(IRTmodel == "2PL"){
      model$est[,1] <- ipar[,1]
      model$est[,2] <- ipar[,2]
    }
    if(IRTmodel == "3PL"){
      model$est[,1] <- ipar[,1]
      model$est[,2] <- ipar[,2]
      model$est[,3] <- ipar[,3]
    }
  }
  if(is.null(abi)){
    abi <- ability(dat, model, method = "EAP")[,1]
  }

  p <- probs.plm(abi, model$est)
  test.stat <- (dat-p)/nrow(model$est)
  cont <- array(dim=c(2,length(abi),nrow(model$est)))

  for(j in 1:length(abi)){
    cplus <- 0
    cplus.p <- rep(NA,nrow(model$est))
    for(i in 1:nrow(model$est)){
      cplus<- max(0, cplus + test.stat[j,i])
      cplus.p[i] <- cplus
    }
    cont[1,j,] <- cplus.p
    cmin <- 0
    cmin.p <- rep(NA,nrow(model$est))

    for(i in 1:nrow(model$est)){
      cmin <- min(0, cmin + test.stat[j,i])
      cmin.p[i] <- cmin
    }
    cont[2,j,] <- cmin.p
  }

  result <- matrix(cont, 2*length(abi), nrow(model$est))
  row.names(result) <- paste("Person",rep(1:length(abi),each = 2))
  colnames(result) <- paste("Item",1:nrow(model$est))
  output <- list(CusumChart = result, Parameters = model$est, ability = abi, P = p, model = IRTmodel, dat = dat)

  class(output) <- "PsyCu"
  return(output)
}

