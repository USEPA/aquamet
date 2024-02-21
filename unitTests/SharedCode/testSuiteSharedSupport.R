# testSuiteSharedSupport.R
#
#  3/17/16 cws changed defineTestSuite argument dirs from '.' to aquametPathList
#          and testFileRegexp from sharedSupport.r to sharedSupporTest.r.
#  2/20/24 cws Added rngKind and rngNormalKind values to correct defaults as was
#          recently done for other test suites.
#


testSuiteSharedSupport <- function()
# Define and run the test suite for the NRSA and NLA physical habitat shared
# support functions
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite("Shared Support Functions"
                              ,dirs=aquametPathList
                              ,testFileRegexp="sharedSupportTest.r"
                              ,testFuncRegexp="^.+Test$"
                              ,rngKind = "Mersenne-Twister"
                              ,rngNormalKind = "Inversion"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- "testResults_SharedSupport.html"
  printHTMLProtocol(testResult, fileName=testResultFile)
}
