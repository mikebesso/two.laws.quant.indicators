#' @include attributes.R
#' @include averages.R
NULL


#' @export
CalculateBBands <- function(hlc, n = 20, sd = 2, maType = CalculateSMA){

  BB <- dplyr::as_data_frame(TTR::BBands(hlc, n))

  Indicator <- InitializeIndicator(
    name = "BBands",
    parameters = list(n = n, sd = sd),
    BB.Lower = BB$dn,
    BB.Upper = BB$up,
    BB.Mid = BB$mavg,
    BB.Percent = BB$pctB

  )

  return(Indicator)
}



#' @export
AddBBands <- function(dataSet, n = 20, sd = 2, maType = CalculateSMA){


  HLC <- dataSet %>%
    dplyr::select(dplyr::one_of("High", "Low", "Close"))

  BB <- CalculateBBands(HLC, n = n, maType = maType, sd = sd)

  DataSet <-
    dataSet %>%
      dplyr::bind_cols(BB)

  attr(DataSet, attr(BB, "Name")) <- attr(BB, attr(BB, "Name"))

  return(DataSet)

}


