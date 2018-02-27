# runTest.r
#
# Runs the unit tests for aquamet.  This file should reside in, and be called from, the
# top of the eForms project directory, e.g. c:/Users/yourName/local/aquamet as follows:.
#    R runTest.r
#
#  5/15/15 cws Created
#  1/04/16 cws updated pathList and fileList in testSuite arguments
#  2/26/18 cws
#
require(foreach)
require(Hmisc)
#require(aquamet)
require(RUnit)
require(plyr)
require(dplyr)
require(reshape2)

# Get aquamet function definitions
srcList <- grep('^.+\\.[rR]$'
               ,list.files('C:/Users/cseelige/local/aquamet/R/')
               ,value=TRUE
               )
for(src in srcList) 
    source(sprintf("C:/Users/cseelige/local/aquamet/R/%s", src))

# # Get unit test definitions

# Create test harness and run it
testSuiteAquamet <- function()
{
    
    pathList <- c('C:/Users/cseelige/local/aquamet/R'
                 ,'C:/Users/cseelige/local/aquamet/UnitTests/NLA_Physical_Habitat'
                 ,'C:/Users/cseelige/local/aquamet/UnitTests/NRSA_Physical_Habitat'
                 ,'C:/Users/cseelige/local/aquamet/UnitTests/SharedCode'
                 )
    fileList <- grep('(run|Deprecated)', ignore.case=TRUE, invert=TRUE
                    ,grep('^.+Test\\.[rR]$', list.files(pathList), value=TRUE)
                    ,value=TRUE
                    )
    testSuite <- defineTestSuite('aquamet package development'
                                ,dirs = pathList
                                ,testFileRegexp= sprintf("^(%s)$", paste(fileList,collapse='|'))
                                ,testFuncRegexp="^.+Test$"
                                )

    testResult <- runTestSuite(testSuite)

    printHTMLProtocol(testResult, fileName='testResults_aquamet.html')

}

testSuiteAquamet()

load('c:/Users/cseelige/local/aquamet/data/nlaPhabEx.rda')
load('c:/Users/cseelige/local/aquamet/data/nlaPhabEx07.rda')

# end of file
