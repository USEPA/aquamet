# aquametStandardizeArgumentTest.r
#
#  6/29/17 cws Tests moved here from aquametStandardizeArgument.r
#  7/19/17 cws Updated aquametStandardizeArgument.checkStructureTest to reflect 
#          refactoring of error message generation
#  2/20/18 cws Corrected aquametStandardizeArgument.checkRangeTest to correctly
#          handle named elements in returned value. Completed unit test
#          aquametStandardizeArgumentTest.with_ifdf_Validation.
#

require(RUnit)
require(dplyr)

aquametStandardizeArgument.checkLegalTest <- function()
# unit test for aquametStandardizeArgument.checkLegal
{
    testLegalValues <- letters[1:10]
    testdata <- data.frame(SITE=rep(1:2, each=5), STATION=LETTERS[1:5], VALUE=testLegalValues)
    
    # Test cases where data take on legal values
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(VALUE=testLegalValues))
    checkEquals(NULL, actual, "Incorrect response when all values are legal in one column")

    actual <- aquametStandardizeArgument.checkLegal(testdata, list(VALUE=testLegalValues, STATION=NULL))
    checkEquals(NULL, actual, "Incorrect response when all values are legal in one column and another column is specified but not tested")

    actual <- aquametStandardizeArgument.checkLegal(testdata, list(VALUE=testLegalValues, STATION=LETTERS[1:12]))
    checkEquals(NULL, actual, "Incorrect response when all values are legal in two columns")

    
    # Test case where data take on some illegal values
    expected <- 'Column STATION is expected to have values <A,B,C,D>, but has illegal values <E>'
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(VALUE=testLegalValues, STATION=LETTERS[1:4]))
    checkEquals(expected, actual, "Incorrect response when some values are legal")
    
    
    # Test case where all of data is illegal
    expected <- 'Column VALUE is expected to have values <1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>, but has illegal values <a,b,c,d,e,f,g,h,i,j>.  Column STATION is expected to have values <a,b,c,d,e,f,g,h,i,j,k,l>, but has illegal values <A,B,C,D,E>'
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(VALUE=1:20, STATION=letters[1:12]))
    checkEquals(expected, actual, "Incorrect response when no values are legal")
    
    
    # Test cases where legal checks are not done
    actual <- aquametStandardizeArgument.checkLegal(testdata, NULL)
    checkEquals(NULL, actual, "Incorrect response when expectedLegal is NULL")
    
    actual <- aquametStandardizeArgument.checkLegal(testdata, list())
    checkEquals(NULL, actual, "Incorrect response when expectedLegal is empty list")
    
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(STATION=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedLegal has one element with no values specified")
    
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(STATION=NULL, VALUE=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedLegal has two elements with no values specified")

    
    # Test bad input cases    
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(STATION=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedLegal specifies a single column that does not exist")
    
    expected <- 'Legal values include specification for columns not in the data: ASDF'
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(STATION=LETTERS[1:12], ASDF=1:4))
    checkEquals(expected, actual, "Incorrect response when expectedLegal specifies two columns, one of which does not exist")
    
    expected <- "Legal value specification must be a named list"
    actual <- aquametStandardizeArgument.checkLegal(testdata, c(STATION=LETTERS[1:12], ASDF=1:4))
    checkEquals(expected, actual, "Incorrect response when expectedLegal is not a list")
    
    expected <- "Legal value specification elements must all be named"
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(LETTERS[1:12], VALUE=testLegalValues))
    checkEquals(expected, actual, "Incorrect response when expectedLegal has two elements and one is not named")
    
    actual <- aquametStandardizeArgument.checkLegal(testdata, list(LETTERS[1:12], testLegalValues))
    checkEquals(expected, actual, "Incorrect response when expectedLegal elements are all unnamed")
}


aquametStandardizeArgument.checkRangeTest <- function()
# unit test for aquametStandardizeArgument.checkRange
{
#    DEACTIVATED("Not yet implemented")    
    testdata <- data.frame(SITE=rep(1:2, each=6), STATION=LETTERS[1:6], VALUE=c(0:11), stringsAsFactors=FALSE)
    
    # test cases where data is within range
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c()))
    checkEquals(NULL, actual, "Incorrect response when expectedRange specifies a column with no values")

    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedRange specifies a column with NULL value")

    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(NA,NA)))
    checkEquals(NULL, actual, "Incorrect response when expectedRange specifies a column with unspecified min & max")

    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(0,11)))
    checkEquals(NULL, actual, "Incorrect response when all values are within range in one column")

    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, OTHERVALUE=3), list(VALUE=c(0,11), OTHERVALUE=NULL))
    checkEquals(NULL, actual, "Incorrect response when all values are within range in one column and another column is specified but not tested")

    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, OTHERVALUE=3), list(VALUE=c(0,11), OTHERVALUE=c(1,4)))
    checkEquals(NULL, actual, "Incorrect response when all values are within range in two columns")

    
    # test cases where data has some values outside of range
    expected <- c(warning = 'Column VALUE has 2 values below 2, and 0 values above 11')
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(2,11)))
    checkEquals(expected, actual, "Incorrect response when some values are below range in one column")
    
    expected <- c(warning = "Column VALUE has 2 values below 2, and unspecified maximum")
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(2,NA)))
    checkEquals(expected, actual, "Incorrect response when some values are below range in one column, with maximum unspecified")
    
    expected <- c(warning = 'Column VALUE has 0 values below 0, and 2 values above 9')
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(0,9)))
    checkEquals(expected, actual, "Incorrect response when some values are above range in one column")
    
    expected <- c(warning = 'Column VALUE has unspecified minimum, and 2 values above 9')
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(NA,9)))
    checkEquals(expected, actual, "Incorrect response when some values are above range in one column, with minimum unspecified")
    
    expected <- c(warning = 'Column VALUE has 2 values below 2, and 2 values above 9')
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(2,9)))
    checkEquals(expected, actual, "Incorrect response when some values are below and above range in one column")

    expected <- c(warning = 'Column VALUE has 2 values below 2, and 2 values above 9')
    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, OTHERVALUE=3), list(VALUE=c(2,9), OTHERVALUE=c(1,4)))
    checkEquals(expected, actual, "Incorrect response when some values are below and above range in one column when two columns are specified")
    
    expected <- c(warning = 'Column VALUE has 2 values below 2, and 2 values above 9.  Column OTHERVALUE has 0 values below 1, and 12 values above 2')
    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, OTHERVALUE=3), list(VALUE=c(2,9), OTHERVALUE=c(1,2)))
    checkEquals(expected, actual, "Incorrect response when some values are below and above range in two column when two columns are specified")
    
    
    # test cases where range checks are not done
    actual <- aquametStandardizeArgument.checkRange(testdata, NULL)
    checkEquals(NULL, actual, "Incorrect response when expectedRange is NULL")
    
    actual <- aquametStandardizeArgument.checkRange(testdata, list())
    checkEquals(NULL, actual, "Incorrect response when expectedRange is empty list")
    
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedRange has one element with no values specified")
    
    actual <- aquametStandardizeArgument.checkRange(testdata %>% mutate(OTHERVALUE=7), list(VALUE=NULL, OTHERVALUE=NULL))
    checkEquals(NULL, actual, "Incorrect response when expectedRange has two elements with no values specified")
    
    
    # test case where data is character
    expected <- c(error = 'Range checks on column VALUE require it to be either integer or double')
    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, VALUE=as.character(VALUE)), list(VALUE=c(0,11)))
    checkEquals(expected, actual, "Incorrect response when all values are within range in one column of integers cast as character")

    expected <- c(error = 'Range checks on column OTHERVALUE require it to be either integer or double')
    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, OTHERVALUE='3'), list(VALUE=c(2,9), OTHERVALUE=c(1,2)))
    checkEquals(expected, actual, "Incorrect response when some values are below and above range in two columns when one column is character")

    expected <- c(error = 'Range checks on column VALUE, OTHERVALUE require it to be either integer or double')
    actual <- aquametStandardizeArgument.checkRange(mutate(testdata, VALUE=as.character(VALUE), OTHERVALUE='asdf'), list(VALUE=c(2,9), OTHERVALUE=c(1,2)))
    checkEquals(expected, actual, "Incorrect response when some values are below and above range in two column when both columns are character")

    
    # test cases with bad input 
    expected <- c(error = 'Range specifications for VALUE must either be NULL or have two values - low, high')
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(0,20,100)))
    checkEquals(expected, actual, "Incorrect response when expectedRange specifies a column with three values")

    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(20)))
    checkEquals(expected, actual, "Incorrect response when expectedRange specifies a column with one values")

    expected <- c(error = "Range values include specification for columns not in the data: ASDF")
    actual <- aquametStandardizeArgument.checkRange(testdata, list(ASDF=NULL))
    checkEquals(expected, actual, "Incorrect response when expectedRange specifies a single column that does not exist and no range specified")
    actual <- aquametStandardizeArgument.checkRange(testdata, list(ASDF=c(0,1)))
    checkEquals(expected, actual, "Incorrect response when expectedRange specifies a single column that does not exist")
    actual <- aquametStandardizeArgument.checkRange(testdata, list(VALUE=c(2,9), ASDF=c(0,20)))
    checkEquals(expected, actual, "Incorrect response when expectedRange specifies two columns, one of which does not exist")
    
    expected <- c(error = "Range value specification must be a named list")
    actual <- aquametStandardizeArgument.checkRange(testdata, c(VALUE=c(2,9), ASDF=c(0,20)))
    checkEquals(expected, actual, "Incorrect response when expectedRange is not a list")
    
    expected <- c(error = "Range value specification elements must all be named")
    actual <- aquametStandardizeArgument.checkRange(testdata, list(c(2,9), VALUE=c(0,20)))
    checkEquals(expected, actual, "Incorrect response when expectedRange has two elements and one is not named")
    
    actual <- aquametStandardizeArgument.checkRange(testdata, list(c(2,9), c(0,20)))
    checkEquals(expected, actual, "Incorrect response when expectedRange elements are all unnamed")
}


aquametStandardizeArgument.checkStructureTest <- function()
# Checks the provided argument for expected structure.  Returns NULL on success,
# or a character string describing the error if one is found.
#
{
    testdata <- data.frame(SITE=1:10, VALUE=runif(10))
    
    # test cases when argument structure is as expected
    expected <- NULL
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUE='double'))
    checkEquals(expected, actual, "Incorrect response when argument matches structure when allowing only one column type, specified as a vector")
    actual <- aquametStandardizeArgument.checkStructure(testdata, list(SITE='integer', VALUE='double'))
    checkEquals(expected, actual, "Incorrect response when argument matches structure when allowing only one column type, specified as a list")
    actual <- aquametStandardizeArgument.checkStructure(testdata, list(SITE=c('integer','character'), VALUE=c('double','character')))
    checkEquals(expected, actual, "Incorrect response when argument matches structure allowing multiple types - 1")
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(SITE=as.character(SITE), VALUE=as.character(VALUE)), list(SITE=c('integer','character'), VALUE=c('double','character')))
    checkEquals(expected, actual, "Incorrect response when argument matches structure allowing multiple types - 2")
    
    # Test cases when argument has columns of wrong type
    expected <- 'column SITE should have type integer rather than character'
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(SITE=paste('X', SITE))
                                        ,c(SITE='integer', VALUE='double')
                                        )
    checkEquals(expected, actual, "Incorrect response when argument does not match structure by column type, specified as a vector")
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(SITE=paste('X', SITE))
                                        ,list(SITE='integer', VALUE='double')
                                        )
    checkEquals(expected, actual, "Incorrect response when argument does not match structure by column type, specified as a list")
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(SITE=paste('X', SITE))
                                        ,list(SITE=c('integer','double'), VALUE=c('double','character'))
                                        )
    checkEquals("column SITE should have type integer or double rather than character"
               ,actual
               ,"Incorrect response when argument does not match structure allowing multiple types"
               )
    
    # Test cases when argument has unexpected columns in some manner
    expected <- 'missing column VALUEQQ; unexpected column VALUE' 
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUEQQ='double'))
    checkEquals(expected, actual, "Incorrect response when argument does not match structure by column name")
    
    expected <- 'missing column FOO'
    actual <- aquametStandardizeArgument.checkStructure(testdata, c(SITE='integer', VALUE='double', FOO='character'))
    checkEquals(expected, actual, "Incorrect response when argument is missing an expected column")
    
    expected <- 'unexpected column FOO'
    actual <- aquametStandardizeArgument.checkStructure(testdata %>% mutate(FOO=1:10), c(SITE='integer', VALUE='double'))
    checkEquals(expected, actual, "Incorrect response when argument has an unexpected column")
}

aquametStandardizeArgumentTest <- function()
# unit test for aquametStandardizeArgument
{
    aquametStandardizeArgumentTest.no_ifdf_noValidation()
    aquametStandardizeArgumentTest.with_ifdf_noValidation()
    aquametStandardizeArgumentTest.with_ifdf_Validation()
}

aquametStandardizeArgumentTest.no_ifdf_noValidation <- function()
# Test with default ifdf = NULL
{
    testdata <- data.frame(SITE=1:10, VALUE=runif(10))

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
    
    actual <- try(aquametStandardizeArgument(testdata %>% mutate(SITE=paste('X', SITE))
                                            ,struct=list(SITE=c('integer','character'), VALUE='double')
                                            )
                 ,silent=TRUE
                 )
    checkEquals(expected, actual, "Incorrect with data arg matching expected structure with more than one possible type")
 
    
    # Test case with nondata argument.   
    actual <- try(aquametStandardizeArgument('not data'), silent=TRUE)
    checkTrue(class(actual) == 'try-error', "Incorrect with non-data and default arguments")
}


aquametStandardizeArgumentTest.with_ifdf_noValidation <- function()
# Test with ifdf processing function
{
    testdata <- data.frame(SITE=1:10, VALUE=runif(10))
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


aquametStandardizeArgumentTest.with_ifdf_Validation <- function()
# Test with ifdf processing function
{
    testdata <- data.frame(SITE=1:10, VALUE_D=runif(10), VALUE_I=1:10)
    testpf <- function(df, ...) {
        
        args <- list(...)
        
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(PARAMETER=args[[1]])
        return(rc)
        
    }
   
    # Test case in which data that passes validation specs.
    expected <- testdata %>% mutate(PARAMETER='new column')
    actual <- aquametStandardizeArgument(testdata
                                        ,ifdf=        testpf, 'new column'
                                        ,struct=      list(SITE=c('integer','character'),VALUE_D='double',VALUE_I=c('integer','character'))
                                        ,rangeLimits= list(SITE=c(1,NA), VALUE_D=c(0,1), VALUE_I=c(1,10))
                                        ,legalValues= list(VALUE_I=1:10)
                                        ,stopOnError= FALSE
                                        )
    checkEquals(expected, actual, "Incorrect with data passing validation specs, data are not character")
    
    # Test case when asking to do range checks on character values
    expected <- "You blockhead, argument <<testdata %>% mutate(SITE = as.character(SITE), VALUE_D = as.character(VALUE_D),      VALUE_I = as.character(VALUE_I))>> can not be range checked: Range checks on column SITE, VALUE_D, VALUE_I require it to be either integer or double"
    actual <- aquametStandardizeArgument(testdata %>% mutate(SITE=as.character(SITE), VALUE_D=as.character(VALUE_D), VALUE_I=as.character(VALUE_I))
                                        ,ifdf=        testpf, 'new column'
                                        ,struct=      list(SITE=c('integer','character'),VALUE_D='character',VALUE_I=c('integer','character'))
                                        ,rangeLimits= list(SITE=c(1,NA), VALUE_D=c(0,1), VALUE_I=c(1,10))
                                        ,legalValues= list(VALUE_I=1:10)
                                        ,stopOnError= FALSE
                                        )
    checkEquals(expected, actual, "Incorrect with data passing validation specs, data are character")


    # Test cases in which data do NOT pass validation specs. Only the error
    # due to the illegal VALUE_I value is expected in the returned value, the
    # range errors are merely written to the screen and the process continues.
    expected <- "You blockhead, argument <<testdata %>% mutate(SITE = SITE - 1L, VALUE_D = VALUE_D + 1,      VALUE_I = VALUE_I + 1L)>> failed the check for illegal values: Column VALUE_I is expected to have values <1,2,3,4,5,6,7,8,9,10>, but has illegal values <11>"
    actual <- aquametStandardizeArgument(testdata %>% mutate(SITE=SITE-1L, VALUE_D=VALUE_D+1, VALUE_I=VALUE_I+1L)
                                        ,ifdf=        testpf, 'new column'
                                        ,struct=      list(SITE=c('integer','character'),VALUE_D='double',VALUE_I=c('integer','character'))
                                        ,rangeLimits= list(SITE=c(1,NA), VALUE_D=c(0,1), VALUE_I=c(1,10))
                                        ,legalValues= list(VALUE_I=1:10)
                                        ,stopOnError= FALSE
                                        )

    checkEquals(expected, actual, "Incorrect with data passing validation specs")
    
}

# end of file