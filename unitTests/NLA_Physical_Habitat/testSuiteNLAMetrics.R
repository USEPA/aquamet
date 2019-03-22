# testSuiteNLAMetrics.R

testSuiteNLAMetrics <- function()
# Define and run the test suite for the NLA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("NLA metrics",
                               dirs=aquametPathList,
                               testFileRegexp="^nla.*\\.r$",
                               testFuncRegexp="^.+Test$")

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_NLAMetrics.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
