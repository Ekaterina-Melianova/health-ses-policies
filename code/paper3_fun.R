clean <- function(..., improper = TRUE) {
  object.l <- list(...)
  paramOnly <- sapply(object.l, slot, name = "paramOnly")
  if(all(!paramOnly)) {
    converged <- sapply(object.l, slot, name = "converged")
    if(!is.matrix(converged)) converged <- as.matrix(converged)
    targetRep <- 0
    if(improper) targetRep <- c(0, 3:7)
    allConverged <- matrix(converged %in% targetRep, nrow(converged), ncol(converged))
    allConverged <- apply(allConverged, 1, all)
    
    if (all(!allConverged)) 
      stop("All replications in the result object are not convergent. Thus, the result object cannot be used.")
    object.l <- lapply(object.l, cleanSimResult, converged = allConverged, improper = improper)
  }
  if (length(object.l) == 1) 
    object.l <- object.l[[1]]
  return(object.l)
}

cleanSimResult <- function(object, converged = NULL, improper = TRUE) {
  if (is.null(converged)) {
    targetRep <- 0
    if(improper) targetRep <- c(0, 3:7)
    converged <- object@converged %in% targetRep
  }
  object@nRep <- sum(converged)
  object@coef <- object@coef[converged, , drop=FALSE]
  object@se <- object@se[converged, , drop=FALSE]
  object@fit <- object@fit[converged, , drop=FALSE]
  if (!is.null(object@paramValue) && (nrow(object@paramValue) > 1)) 
    object@paramValue <- object@paramValue[converged, , drop=FALSE]
  if (!is.null(object@stdParamValue) && (nrow(object@stdParamValue) > 1)) 
    object@stdParamValue <- object@stdParamValue[converged, , drop=FALSE]
  if (!is.null(object@misspecValue) && (nrow(object@misspecValue) > 1)) 
    object@misspecValue <- object@misspecValue[converged, , drop=FALSE]
  if (!is.null(object@popFit) && (nrow(object@popFit) > 1)) 
    object@popFit <- object@popFit[converged, , drop=FALSE]
  if (!is.null(object@extraOut) && (length(object@extraOut) > 1)) 
    object@extraOut <- object@extraOut[converged]
  if (!is.null(object@FMI1)) 
    object@FMI1 <- object@FMI1[converged, , drop=FALSE]
  if (!is.null(object@FMI2)) 
    object@FMI2 <- object@FMI2[converged, , drop=FALSE]
  if (!is.null(object@cilower)) 
    object@cilower <- object@cilower[converged, , drop=FALSE]
  if (!is.null(object@ciupper)) 
    object@ciupper <- object@ciupper[converged, , drop=FALSE]
  object@stdCoef <- object@stdCoef[converged, , drop=FALSE]
  object@stdSe <- object@stdSe[converged, , drop=FALSE]
  object@seed <- object@seed
  if (length(object@n) > 1) 
    object@n <- object@n[converged]
  if (length(object@pmMCAR) > 1) 
    object@pmMCAR <- object@pmMCAR[converged]
  if (length(object@pmMAR) > 1) 
    object@pmMAR <- object@pmMAR[converged]
  object@converged <- object@converged[converged]
  return(object)
} 

compute_stat = function(object, alpha = 0.05, std = FALSE, detail = T,
                        improper = TRUE, digits = 3, matchParam = FALSE){
  object <- clean(object, improper = improper)
  usedCoef <- object@coef
  usedSe <- object@se
  if (std) {
    if (length(object@stdCoef) == 0L)
      stop("The standardized coefficients cannot be summarized because there",
           " are no standardized coefficients in the object")
    usedCoef <- object@stdCoef
    usedSe <- object@stdSe
  }
  coef <- colMeans(usedCoef, na.rm = TRUE)
  real.se <- sapply(usedCoef, sd, na.rm = TRUE)
  result <- cbind(coef, real.se)
  colnames(result) <- c("Estimate Average", "Estimate SD")
  
  estimated.se <- colMeans(usedSe, na.rm = TRUE)
  estimated.se[estimated.se == 0] <- NA
  crit <- qnorm(1 - alpha/2)
  if (!all(is.na(usedSe))) {
    z <- usedCoef / usedSe
    sig <- abs(z) > crit
    pow <- apply(sig, 2, mean, na.rm = TRUE)
    result <- cbind(result, "Average SE" = estimated.se, "Power (Not equal 0)" = pow)
  }
  if (!std && length(object@stdCoef) != 0) {
    stdCoef <- colMeans(object@stdCoef, na.rm = TRUE)
    stdRealSE <- sapply(object@stdCoef, sd, na.rm = TRUE)
    stdEstSE <- colMeans(object@stdSe, na.rm = TRUE)
    leftover <- setdiff(rownames(result), names(stdCoef))
    if (length(leftover) > 0) {
      temp <- rep(NA, length(leftover))
      names(temp) <- leftover
      stdCoef <- c(stdCoef, temp)
      stdRealSE <- c(stdRealSE, temp)
      stdEstSE <- c(stdEstSE, temp)
    }
    resultStd <- cbind("Std Est" = stdCoef, "Std Est SD" = stdRealSE,
                       "Std Ave SE" = stdEstSE)
    result <- cbind(result, resultStd[rownames(result),])
  }
  
  paramExist <- !(all(dim(object@paramValue) == 1) && is.na(object@paramValue))
  stdParamExist <- !(all(dim(object@stdParamValue) == 1) && is.na(object@stdParamValue))
  
  if ((!std & paramExist) | (std & stdParamExist)) {
    paramValue <- object@paramValue
    if (std) paramValue <- object@stdParamValue
    targetVar <- match(colnames(usedCoef), colnames(paramValue))
    targetVar <- targetVar[!is.na(targetVar)]
    paramValue <- paramValue[, targetVar]
    
    if (matchParam) result <- result[colnames(paramValue),]
    if ((nrow(result) == ncol(paramValue)) && all(rownames(result) ==
                                                  colnames(paramValue))) {
      nRep <- object@nRep
      nParam <- nrow(result)
      if (nrow(paramValue) == 1)
        paramValue <- matrix(unlist(rep(paramValue, nRep)), nRep, nParam,
                             byrow = T)
      biasParam <- usedCoef[,rownames(result)] - paramValue
      lowerBound <- object@cilower
      upperBound <- object@ciupper
      if (std) {
        lowerBound <- usedCoef - crit * usedSe
        upperBound <- usedCoef + crit * usedSe
      }
      selectci <- colnames(lowerBound) %in% rownames(result)
      lowerBound <- lowerBound[,selectci]
      upperBound <- upperBound[,selectci]
      
      noci <- setdiff(rownames(result), colnames(lowerBound))
      if (length(noci) > 0) {
        if (length(selectci) > 0) warning("Some CIs are Wald CI and others",
                                          " are calculated inside the simulation.")
        selectCoef <- usedCoef[,noci]
        selectSE <- usedSe[,noci]
        
        lowerBound <- cbind(lowerBound, selectCoef - crit * selectSE)
        upperBound <- cbind(upperBound, selectCoef + crit * selectSE)
      }
      cover <- (paramValue > lowerBound) & (paramValue < upperBound)
      average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
      sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
      average.bias <- apply(biasParam, 2, mean, na.rm = TRUE)
      perc.cover <- apply(cover, 2, mean, na.rm = TRUE)
      sd.bias <- apply(biasParam, 2, sd, na.rm = TRUE)
      perc.cover[estimated.se[rownames(result)] == 0] <- NA
      result2 <- cbind(average.param, sd.param, average.bias, sd.bias, perc.cover)
      colnames(result2) <- c("Average Param", "SD Param", "Average Bias", "SD Bias",
                             "Coverage")
      #if (nrow(object@paramValue) == 1)
       # result2 <- result2[, c(1, 3, 5)]
      result <- cbind(result, result2)
      if (detail) {
        relative.bias <- biasParam/paramValue
        relBias <- apply(relative.bias, 2, mean, na.rm = TRUE)
        relBias[is.nan(relBias)] <- NA
        std.bias <- NULL
        relative.bias.se <- NULL
        if (nrow(object@paramValue) == 1) {
          std.bias <- average.bias/real.se
          relative.bias.se <- (estimated.se - real.se) / real.se
        } else {
          std.bias <- average.bias/sd.bias
          relative.bias.se <- (estimated.se - sd.bias) / sd.bias
        }
        width <- upperBound - lowerBound
        average.width <- apply(width, 2, mean, na.rm = TRUE)
        sd.width <- apply(width, 2, sd, na.rm = TRUE)
        belowLowerBound <- paramValue < lowerBound
        aboveUpperBound <- paramValue > upperBound
        perc.lower <- apply(belowLowerBound, 2, mean, na.rm = TRUE)
        perc.upper <- apply(aboveUpperBound, 2, mean, na.rm = TRUE)
        result3 <- cbind(relBias, std.bias, relative.bias.se, perc.lower,
                         perc.upper, average.width, sd.width)
        colnames(result3) <- c("Rel Bias", "Std Bias", "Rel SE Bias",
                               "Not Cover Below", "Not Cover Above",
                               "Average CI Width", "SD CI Width")
        result <- cbind(result, result3)
      }
    }
  }
  if (!is.null(digits)) {
    result <- round(result, digits)
  }
  return(as.data.frame(result))
}

