# testSuiteNLAMetrics.R
#
#  2/20/24 cws Added rngKind and rngNormalKind values to correct defaults as was
#          recently done for other test suites.
#

testSuiteNLAMetrics <- function()
# Define and run the test suite for the NLA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("NLA metrics"
                              ,dirs=aquametPathList
                              ,testFileRegexp="^nla.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              ,rngKind = "Mersenne-Twister"
                              ,rngNormalKind = "Inversion"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_NLAMetrics.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
