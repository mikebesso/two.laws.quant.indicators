library(testthat)
library(two.laws.big.bang)
library(two.laws.quant.data)



SetTestContext("Bollinger Bands")


CreateTestCase(
  "Add with defaults",
  {
    DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5))

    DataSet %<>%
      dplyr::filter(Date > lubridate::ymd("2017-07-01"))

    DataSetWithBB <- AddBBands(DataSet)

    expect_true("BB.Upper" %in% names(DataSetWithBB))
    expect_true("BB.Lower" %in% names(DataSetWithBB))
    expect_true("BB.Mid" %in% names(DataSetWithBB))
  }
)
