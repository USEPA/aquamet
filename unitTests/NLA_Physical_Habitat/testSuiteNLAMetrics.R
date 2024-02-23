# testSuiteNLAMetrics.R
#
#  2/20/24 cws Added rngKind and rngNormalKind values to correct defaults as was
#          recently done for other test suites.
#  2/21/24 cws Specifying full path for output file. Improved title of test suite
#

testSuiteNLAMetrics <- function()
# Define and run the test suite for the NLA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testsuiteTitle <- sprintf('Aquamet NLA metrics in %s and %s'
                            ,ifelse(Sys.getenv('RSTUDIO') == '', 'RGui', 'RStudio')
                            ,R.version.string
                           )
  testSuite <- defineTestSuite(testsuiteTitle
                               ,dirs=aquametPathList
                              ,testFileRegexp="^nla.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              ,rngKind = "Mersenne-Twister"
                              ,rngNormalKind = "Inversion"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- paste0(mainPath, "/testResults_NLAMetrics.html")
  printHTMLProtocol(testResult, fileName=testResultFile)
}
