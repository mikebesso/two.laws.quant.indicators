library(testthat)
library(two.laws.big.bang)
library(two.laws.quant.data)


SetTestContext("Averages")


CreateTestCase(
  "Default",
  {
      DataSet <- LoadTestSymbol()
      DataSet %<>% AddAverages()

      expect_true("MA" %in% names(DataSet))

      Averages <- attr(DataSet, "Averages")

      expect_true(is.list(Averages))
  }
)