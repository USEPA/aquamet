# testSuiteNRSAMetrics.R
#
# 10/21/15 cws Changed to use nrsa*.r and aquametPathList
#  2/20/24 cws Added rngKind and rngNormalKind values to correct defaults as was
#          recently done for other test suites.
#  2/21/24 cws Specifying full path for output file
#

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the NRSA physical habitat metrics functions
# Results are saved to timestamped HTML file
{
  testsuiteTitle <- sprintf('Aquamet NRSA metrics in %s and %s'
                           ,ifelse(Sys.getenv('RSTUDIO') == '', 'RGui', 'RStudio')
                           ,R.version.string
                           )
  testSuite <- defineTestSuite(testsuiteTitle
                              ,dirs=aquametPathList                     # defined in aquamet.r
                              ,testFileRegexp="^nrsa.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              ,rngKind = "Mersenne-Twister"
                              ,rngNormalKind = "Inversion"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- paste0(mainPath, "/testResults_NRSAMetrics.html")
  printHTMLProtocol(testResult, fileName=testResultFile)
}
