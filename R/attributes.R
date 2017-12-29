#' @export
InitializeIndicator <- function(name, parameters, ...){

  # Need to assert name is scalar character
  # Need to assert parameters is a list


  Indicator <- dplyr::data_frame(...)
  attr(Indicator, "Name") = name
  attr(Indicator, name) = parameters

  return(Indicator)

}

