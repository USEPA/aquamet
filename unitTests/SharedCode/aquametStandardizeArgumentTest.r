# aquametStandardizeArgumentTest.r
#
#  6/29/17 cws Tests moved here from aquametStandardizeArgument.r
#

require(RUnit)
require(dplyr)

aquametStandardizeArgument.checkStructureTest <- function(arg, struct)
# Checks the provided argument for expected structure.  Returns NULL on success,
# or a character string describing the error if one is found.
#
{
    testdata <- data.frame(SITE=1:10, VALUE=runif(10))
    
    expected <- NULL
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUE='double'))
    checkEquals(expected, actual, "Incorrect response when argument matches structure")
    
    expected <- 'Argument testdata %>% mutate(SITE = paste(\"X\", SITE)) has errors: column SITE should have type integer rather than character'
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(SITE=paste('X', SITE))
                                        ,c(SITE='integer', VALUE='double')
                                        )
    checkEquals(expected, actual, "Incorrect response when argument does not match structure by column type")
    
    expected <- 'Argument testdata has errors: missing column VALUEQQ; unexpected column VALUE' 
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUEQQ='double'))
    checkEquals(expected, actual, "Incorrect response when argument does not match structure by column name")
    
    expected <- 'Argument testdata has errors: missing column FOO'
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUE='double', FOO='character'))
    checkEquals(expected, actual, "Incorrect response when argument is missing an expected column")
    
    expected <- 'Argument testdata %>% mutate(FOO = 1:10) has errors: unexpected column FOO'
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(FOO=1:10), c(SITE='integer', VALUE='double'))
    checkEquals(expected, actual, "Incorrect response when argument has an unexpected column")
}

aquametStandardizeArgumentTest <- function()
# unit test for aquametStandardizeArgument
{
    testdata <- data.frame(SITE=1:10, VALUE=runif(10))
    
    # Test with default ifdf = NULL
    expected <- testdata
    actual <- aquametStandardizeArgument(testdata)
    checkEquals(expected, actual, "Incorrect with good data and default arguments")
    
    expected <- testdata %>% mutate(SITE=paste('X', SITE))
    actual <- aquametStandardizeArgument(testdata %>% mutate(SITE=paste('X', SITE))
                                        ,struct=c(SITE='character', VALUE='double')
                                        )
    checkEquals(expected, actual, "Incorrect with good data using character SITEs and default arguments")
    
    actual <- try(aquametStandardizeArgument(testdata %>% mutate(SITE=paste('X', SITE))
                                            ,struct=c(SITE='integer', VALUE='double')
                                            )
                 ,silent=TRUE
                 )
    checkTrue(class(actual) == 'try-error', "Incorrect with data arg not matching expected structure")
    
    
    actual <- try(aquametStandardizeArgument('not data'), silent=TRUE)
    checkTrue(class(actual) == 'try-error', "Incorrect with non-data and default arguments")
    
    
    # Test with ifdf processing function
    testpf <- function(df, ...) {
        
        args <- list(...)
        
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(PARAMETER=args[[1]])
        return(rc)
        
    }
   
    expected <- testdata %>% mutate(PARAMETER='new column')
    actual <- aquametStandardizeArgument(testdata, ifdf=testpf, 'new column')
    checkEquals(expected, actual, "Incorrect with good data and test function with expected argument")

    actual <- aquametStandardizeArgument(testdata, ifdf=testpf)
    checkEquals(NULL, actual, "Incorrect with good data and test function with absent argument")
 
}

# end of file