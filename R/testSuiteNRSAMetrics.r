# testSuiteNRSAMetrics.r
#
# Defines and runs the test suite for individual NLA metrics calculation functions
#
# 12/15/11 cws Recreated after being overwritten
#

require(RUnit)

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the NRSA metrics functions
#
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite('NRSA metrics'
                              ,dirs='l:/Priv/CORFiles/IM/Rwork/nrsa/code'
                              ,testFileRegexp="^mets.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/docs/testResults_NRSAMetrics.html'
  printHTMLProtocol(testResult, fileName=testResultFile)

}
