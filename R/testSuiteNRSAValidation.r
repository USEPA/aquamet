# testSuiteNRSAValidation.r
#
# Defines and runs the test suite for individual NRSA validation functions
#
# 12/15/11 cws Recreated after it was overwritten.
# 

require(RUnit)

testSuiteNRSAValidation <- function()
# Define and run the test suite for the NRSA validation functions
#
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite('NRSA validation'
                              ,dirs='l:/Priv/CORFiles/IM/Rwork/nrsa/code'
                              ,testFileRegexp="^val[Legal|Missing|Range|Logical|Struct|Update|Insert|GIS].*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/docs/testResults_NRSAValidation.html'
  printHTMLProtocol(testResult, fileName=testResultFile)

}
