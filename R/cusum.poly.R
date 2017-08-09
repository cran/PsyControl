#' Generates CUSUM values for polytomous IRT model based on Van Krimpen-Stoop & Meijer, (2002).
#'
#' @param dat a nxp matrix with n participants and p items. Responses are in 0 as the lowest scores format.
#' @param NCat number of categories for each item.
#' @param ipar a pxk matrix with given item parameters p items and k item parameters. Item difficulty under the "GRM" or item steps under "PCM" or "GPCM" are in the first columns. The last column is the discrimination parameter.
#' @param abi a vector n ability
#' @param IRTmodel specify the IRT model ("GRM","PCM","GPCM"). Default is "GRM".
#' @return Returns matrix with with lower and upper cusum statistics for \code{dat}.
#' @examples
#' data(exGRM)
#' cusum.poly(dat = exGRM, NCat = 6)
#' @references Van Krimpen-Stoop, E. M., & Meijer, R. R. (2002). Detection of person misfit in computerized adaptive tests with polytomous items. Applied Psychological Measurement, 26(2), 164-180.
#' @export
#' @import ltm stats


cusum.poly <- function(dat, NCat, ipar = NULL, abi = NULL ,IRTmodel = "GRM"){

  if(IRTmodel == "GRM"){

    model <- grm(dat, IRT.param = TRUE)
    if(!(is.null(ipar))){
    model <- grm(dat, start.val = unlist(apply(cbind(ipar[,c(2:ncol(ipar))]*ipar[,1],ipar[,1]), 1, list),recursive = FALSE) ,control = list(iter.qN = 0))
    }
    if(is.null(abi)){
      abi <- ltm::factor.scores.grm(model, resp.patterns = dat+1, method = "EAP")
      abi <- abi$score.dat[,"z1"]
    }
    parm <- coef(model)
    p <- probs.grm(abi,cbind(parm[,ncol(parm)],parm[,-ncol(parm)]))
    p <- p$P
    exp.p <- matrix(NA,ncol = nrow(parm), nrow = length(abi))
    for(j in 1:length(abi)){
      for(i in 1:nrow(parm)){
        exp.p[j,i]<- sum(p[j,,i]*c(0:(NCat-1)))
      }
    }
  }

  if(IRTmodel == "GPCM"){

    model <- gpcm(dat, constraint = "gpcm")
    if(!(is.null(ipar))){
      model <- gpcm(dat, start.val = unlist(apply(cbind(ipar[,c(2:ncol(ipar))]*ipar[,1],ipar[,1]), 1, list),recursive = FALSE) ,control = list(iter.qN = 0,optimizer = "optim"))
    }
    if(is.null(abi)){
      abi <- factor.scores(model, resp.patterns = dat+1, method = "EAP")
      abi <- abi$score.dat[,"z1"]
    }
    parm <- coef(model)
    p <- probs.gpcm(abi,parm,NCat)

    exp.p <- matrix(NA,ncol = nrow(parm), nrow = length(abi))

    for(j in 1:length(abi)){
      for(i in 1:nrow(parm)){
        exp.p[j,i]<- sum(p[j,i,]*c(0:(NCat-1)))
      }
    }
  }

  if(IRTmodel == "PCM"){

    model <- gpcm(dat, constraint = "rasch" , IRT.param = TRUE)
    if(!(is.null(ipar))){
      ipar[,ncol(ipar)] <- 1
      model <- gpcm(dat, IRT.param = TRUE, constraint = "rasch",start.val = unlist(apply(cbind(ipar[,c(2:ncol(ipar))]*ipar[,1],ipar[,1]), 1, list),recursive = FALSE) ,control = list(iter.qN = 0,optimizer = "optim"))
    }
    if(is.null(abi)){
      abi <- factor.scores(model, resp.patterns = dat+1, method = "EAP")
      abi <- abi$score.dat[,"z1"]
    }
    coef(model)
    parm <- coef(model,FALSE)
    p <- probs.gpcm(abi,parm,NCat)

    exp.p <- matrix(NA,ncol = nrow(parm), nrow = length(abi))

    for(j in 1:length(abi)){
      for(i in 1:nrow(parm)){
        exp.p[j,i]<- sum(p[j,i,]*c(0:(NCat-1)))
      }
    }
  }

  test.stat <- (dat - exp.p)/nrow(parm)

  cont <- array(dim=c(2,length(abi),nrow(parm)))

  for(j in 1:length(abi)){
    cplus <- 0
    cplus.p <- rep(NA,nrow(parm))
    for(i in 1:nrow(parm)){
      cplus<- max(0, cplus + test.stat[j,i])
      cplus.p[i] <- cplus
    }
    cont[1,j,] <- cplus.p
    cmin <- 0
    cmin.p <- rep(NA,nrow(parm))

    for(i in 1:nrow(parm)){
      cmin <- min(0, cmin + test.stat[j,i])
      cmin.p[i] <- cmin
    }
    cont[2,j,] <- cmin.p
  }

  result <- matrix(cont, 2*length(abi), nrow(parm))
  row.names(result) <- paste("Person",rep(1:length(abi),each = 2))
  colnames(result) <- paste("Item",1:nrow(parm))

  output <- list(CusumChart = result, Parameters = parm, ability = abi, P = p, model = IRTmodel, dat = dat)
  class(output) <- "PsyCu"

  return(output)
}
