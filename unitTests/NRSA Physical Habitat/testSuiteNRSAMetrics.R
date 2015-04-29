# testSuiteNRSAMetrics.R

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the NRSA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("NRSA metrics",
                               dirs=".",
                               testFileRegexp="^mets.*\\.r$",
                               testFuncRegexp="^.+Test$")

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_NRSAMetrics.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
