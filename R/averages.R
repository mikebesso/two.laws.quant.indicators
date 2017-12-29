#' @include attributes.R
NULL

#' @export
CalculateSMA <- function(x, n){

  Indicator <- InitializeIndicator(
    name = "SMA",
    parameters = list(n = n),
    SMA = TTR::runMean(x, n)
  )
  return(Indicator)
}

#' @export
CalculateEMA <- function(x, n){

  Indicator <- InitializeIndicator(
    name = "EMA",
    parameters = list(n = n),
    EMA = TTR::EMA(x, n)
  )
  return(Indicator)
}


#' @export
CalculateWMA <- function(x, n){

  Indicator <- InitializeIndicator(
    name = "WMA",
    parameters = list(n = n),
    WMA = TTR::WMA(x, n)
  )
  return(Indicator)
}


#' @export
CalculateATR <- function(x, n){

  Indicator <- InitializeIndicator(
    name = "ATR",
    parameters = list(n = n),
    ATR = TTR::ATR(x, n)
  )
  return(Indicator)

}



#' @export
AddAverages = function(dataSet, n = c(MA.Faster = 5, MA.Fast = 12, MA = 20, MA.Slow = 50, MA.Slower = 100), maType = CalculateSMA) {

  # Need to add check for length between 1 and 0
  n %>%
    AssertAllArePositive()

  Close <- dataSet$Close


  Averages <- plyr::llply(
    n,
    function(n){
      Average <- maType(x = Close, n = n)
      return(Average)
    }

  )

  Type <- attr(Averages[[1]], "Name")

  names(Averages) <- names(n)

  DataSet <-
    cbind(dataSet, Averages)

  attr(DataSet, "Averages") <- list(Type = Type, n = n)

  return(DataSet)

}






