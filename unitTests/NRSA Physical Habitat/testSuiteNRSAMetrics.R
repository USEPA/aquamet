# testSuiteNRSAMetrics.R
#
# 10/21/15 cws Changed to use nrsa*.r and aquametPathList
#

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the NRSA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("NRSA metrics",
                               dirs=aquametPathList,                     # defined in aquamet.r
                               testFileRegexp="^nrsa.*\\.r$",
                               testFuncRegexp="^.+Test$")

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_NRSAMetrics.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
