# is.number.r
#
#  9/11/13 cws created
# 12/12/13 cws Added ability to include exponential notation as a number using 
# 		   Douglas Crockfords regexp for use in JSON parsing, modified for R.
#          Unit test updated accordingly.
# 12/19/23 cws Copied to aquamet for use with dfDifferences. Unit test separated
#          to file in aquamet/unitTests/SharedCode/is.numberTest.r
#

require(RUnit)


is.number <- function(v)
# Determines which elements of the argument are convertable to numeric values
# for the purposes of validation.
# Returns logical vector with TRUE indicating that the corresponding input value
# is a number.
#
# Currently regards '' and NA input values as not convertable to numbers and hence
# outputs FALSE for these values.
#
# ARGUMENTS:
# v		character vector of values.
{
	# First rx follows expected number organizations; the last rx
	# handles pathological cases where there are no digits in the value, e.g. '.'.
	rxStdNotation <- '^ *[-+]?[0-9]*\\.?[0-9]* *$'
	rxExpNotation <- '^[+\\-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]*)?(?:[eE][+\\-]?[0-9]+)?$'
	rc <- (grepl(rxStdNotation, v) | grepl(rxExpNotation, v)) & grepl('[0-9]', v)
	
	return(rc)
	
}

# end of file