# testSuiteNRSAMetrics.R
#
# 10/21/15 cws Changed to use nrsa*.r and aquametPathList
#  2/20/24 cws Added rngKind and rngNormalKind values to correct defaults as was
#          recently done for other test suites.
#

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the NRSA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("NRSA metrics"
                              ,dirs=aquametPathList                     # defined in aquamet.r
                              ,testFileRegexp="^nrsa.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              ,rngKind = "Mersenne-Twister"
                              ,rngNormalKind = "Inversion"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_NRSAMetrics.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
