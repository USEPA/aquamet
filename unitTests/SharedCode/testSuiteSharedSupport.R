# testSuiteSharedSupport.R
testSuiteSharedSupport <- function()
# Define and run the test suite for the NRSA and NLA physical habitat shared
# support functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("Shared Support Functions",
                               dirs=".",
                               testFileRegexp="sharedSupport.r",
                               testFuncRegexp="^.+Test$")

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_SharedSupport.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
