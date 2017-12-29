library(testthat)
library(two.laws.big.bang)
library(two.laws.quant.data)



SetTestContext("MACD")


CreateTestCase(
  "Calculate MACD",
  {
    DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5))
    CalculateMACD(DataSet$Close)
  }
)

CreateTestCase(
  "Add with defaults",
  {
    DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5))

    DataSet %<>%
      dplyr::filter(Date > lubridate::ymd("2017-07-01")) %>%
      AddMACD(DataSet)

    expect_true("MACD" %in% names(DataSet))
    expect_true("MACD.Signal" %in% names(DataSet))
    expect_true("MACD.Histogram" %in% names(DataSet))
  }
)
