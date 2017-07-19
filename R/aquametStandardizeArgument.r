# aquametStandardizeArgument.r
#
#  6/28/17 cws created as a common means for standardizing aquamet data arguments
#  6/29/17 cws Separated unit tests into their own file.
#  7/11/17 cws Allowing multiple column types in structure checks.  Now SITE can
#          be either integer or character. Unit tests extended to check this as 
#          well.
#  7/19/17 cws Refactored generation of error messages in preparation for adding
#          range and legal value checks.
#

require(RUnit)
require(dplyr)

aquametStandardizeArgument <- function(arg, ..., ifdf=NULL, struct=list(SITE='integer', VALUE='double'), rangeLimits=NULL, legalValues=NULL)
# Used to standardize argument to aquamet functions. Returns a dataframe with
# expected column names or NULL.
#
# ARGUMENTS
# arg       one argument provided to the aquamet function
# ...       Additional arguments to ifdf, if any.
# ifdf      closure to call if argument is a dataframe with more than one row; 
#           default value is NULL, which results in no action.  This function is
#           intended to modify 'arg' in some way, such as renaming columns. The
#           arguments to this function are ifdf(arg, ...), and the object it 
#           returns is the 
# struct    named list of character vectors, specifying the expected column names
#           as the names of the elements in 'arg', and the vector values as the 
#           types of each column. It is possible to allow multiple types for 
#           a column.
# rangeLimits named list of integer or numieric vectors specifying the expected
#           limits of values for that column.  Two values for each specified 
#           column are expected, in order of low, high. If one of those values
#           is NA, that end of the range check will not be performed.  If a 
#           column is not specified, no range check is performed on it. The
#           occurence of out-of-range values is noted, but does not prevent
#           calculations to proceed. Thus a depth value might be specified as 
#           c(0,150).
# legalValues named list of integer or character values specifying the
#           values that a column can have. If a column is not specified, a legal
#           check will not be performed. The occurence of values not specified 
#           as legal will prevent calculations from proceding. Thus a substrate
#           value might be specified as c('RR','RS','RC','XB','SB','CB','GR','SA','FN')
#
{
    if(is.null(arg)) {
        rc <- NULL
    }
    else if(!is.data.frame(arg)) {
        msg <- sprintf("You blockhead. The value of %s should be either NULL or a data.frame, rather than class '%s'"
                      ,deparse(substitute(arg)), class(arg)
                      )
        stop(msg)
        rc <- NULL
    }
    else if(nrow(arg) == 0) {
        rc <- NULL
    }
    else {
        
        # Check dataframe argument structure and modify it if a function was
        # provided.
        argName <- deparse(substitute(arg))
        rc <- aquametStandardizeArgument.checkStructure(arg, struct)
        if(is.character(rc)) {
            msg <- sprintf("You blockhead, argument %s has a structure problem: %s", argName, rc)
            stop(msg)
            rc <- NULL
        }
        
        rc <- aquametStandardizeArgument.checkRange(arg, rangeLimits)
        if(is.character(rc)) {
            msg <- sprintf("Warning for argument %s: %s", argName, rc)
            print(msg)
        }

        rc <- aquametStandardizeArgument.checkLegal(arg, legalValues)
        if(is.character(rc)) {
            msg <- sprintf("You blockhead, argument %s has illegal values: %s", argName, rc)
            stop(msg)
            rc <- NULL
        }
        
        if(is.function(ifdf)) {
            rc <- ifdf(arg, ...)
        } else {
            rc <- arg
        }
        
    }
     
    return(rc)   
}


aquametStandardizeArgument.checkLegal <- function(arg, expectedLegal)
# Checks the provided argument for expected structure.  Returns NULL on success,
# or a character string describing the error if one is found.
#
{
    if(length(expectedLegal) == 0) return(NULL)


    # Make sure specified columns exist
    if(names(arg))
    
    return(NULL)
}


aquametStandardizeArgument.checkRange <- function(arg, expectedRange)
# Checks the provided argument for expected structure.  Returns NULL on success,
# or a character string describing the error if one is found.
#
{
    if(length(expectedRange) == 0) return(NULL)

    return(NULL)
}


aquametStandardizeArgument.checkStructure <- function(arg, expectedStruct)
# Checks the provided argument for expected structure.  Returns NULL on success,
# or a character string describing the error if one is found.
#
{
    
    if(length(expectedStruct) == 0) {
        return("No reference for structure check is provided")
    }
    
    # Determine actual argument structure
    argStruct <- arg %>% lapply(typeof) %>% unlist
    
    # List missing and unexpected columns 
    missingColumns <- setdiff(names(expectedStruct), names(argStruct))
    
    unexpectedColumns <- setdiff(names(argStruct), names(expectedStruct))
    
    # List expected columns with wrong types
    commonColumns <- intersect(names(expectedStruct), names(argStruct))
    wrongTypeFlag <- lapply(commonColumns
                           ,function(cname) {
                                wrong <- argStruct[cname] %nin% expectedStruct[[cname]]
                            }
                           ) %>% 
                     unlist()
    wrongTypeNames <- names(arg[commonColumns])[wrongTypeFlag]
    wrongTypes <- argStruct[commonColumns][wrongTypeFlag]
    expectedTypes <- expectedStruct[commonColumns][wrongTypeFlag]

    # Build error message
    errs <- NULL
    if(length(missingColumns) > 0) {
        errs <- paste(c(errs
                       ,sprintf("missing column %s", paste(missingColumns, collapse=','))
                       )
                     ,collapse='; '
                     )
    }
    if(length(unexpectedColumns) > 0) {
        errs <- paste(c(errs
                       ,sprintf("unexpected column %s", paste(unexpectedColumns, collapse=','))
                       )
                     ,collapse='; '
                     )
    }
    if(length(wrongTypeNames) > 0) {
        typeerrs <- lapply(wrongTypeNames
                          ,function(wtn) {
                                sprintf("column %s should have type %s rather than %s"
                                       ,wtn
                                       ,paste(expectedTypes[[wtn]], collapse=' or ')
                                       ,wrongTypes
                                       ) %>%
                                       paste(collapse=', ')
                           }
                          )
                    
        errs <- paste(c(errs, typeerrs),collapse='; ')
    }

    # if(!is.null(errs)) {
    #     errs <- sprintf("Argument %s has errors: %s", argName, errs)
    # }

    return(errs)
}

# end of file