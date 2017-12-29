#' @include attributes.R
#' @include averages.R
NULL


#' @export
CalculateMACD <- function(x, nFast = 12, nSlow = 26, nSignal = 9, maType = CalculateEMA){

  MA.Fast = maType(x, nFast)
  MA.Slow = maType(x, nSlow)


  Indicator <- InitializeIndicator(
    name = "MACD",
    parameters = list(
      nFast = nFast,
      nSlow = nSlow,
      nSignal = nSignal,
      maType = attr(MA.Fast, "Name")
    ),
    MA.Fast = MA.Fast[[1]],
    MA.Slow = MA.Slow[[1]]
  )

  Indicator %<>%
    dplyr::mutate(
        MACD = 100 * (MA.Fast/MA.Slow - 1),
        MACD.Signal = maType(MACD, nSignal)[[1]],
        MACD.Histogram = dplyr::if_else(is.na(MACD), 0.0, MACD - MACD.Signal)
      )

  return(Indicator)
}

#' @export
AddMACD <- function(dataSet, nFast = 12, nSlow = 26, maType = CalculateEMA){

  Close <- dataSet$Close

  Indicator <- CalculateMACD(x = Close, nFast = 12, nSlow = 26, maType = maType)


  DataSet <-
    dataSet %>%
    dplyr::bind_cols(Indicator)

  attr(DataSet, "MACD") <- list(nFast = nFast, nSlow = nSlow)

  return(DataSet)

}
