# is.numberTest.r
#
#  9/11/13 cws created
# 12/12/13 cws Added ability to include exponential notation as a number using 
# 		   Douglas Crockfords regexp for use in JSON parsing, modified for R.
#          Unit test updated accordingly.
# 12/19/23 cws Copied to aquamet for use with dfDifferences. Unit test separated
#          to file in aquamet/unitTests/SharedCode/is.numberTest.r
#

require(RUnit)

is.numberTest <- function()
# unit test for is.number
{
	numbers <- c('12',  '1.2',  '.12',  '0.12',   '012',   '01.2 ',  '12.'
				,'+12', '+1.2', '+.12', '+0.12',  '+012',  '+01.2 ', '+12.'
				,'-12', '-1.2', '-.12', '-0.12',  '-012',  '-01.2 ', '-12.'
				,' 12 ',' 1.2 ',' .12 ',' 0.12 ', ' 012 ', ' 01.2 ', ' 12. '
				)
	expected <- rep(TRUE, length(numbers))
	actual <- is.number(numbers)
	checkEquals(expected, actual
			   ,sprintf("Failing to detect actual number values: '%s'"
					   ,paste(numbers[actual], collapse="', '")
			   		   )
			   )
	
			   
	notNumbers <- c('35 meters', '35m', '-35m', 'five', 'NA'
				   ,'- 12', '+ 12', '. 12', '+. 12', '-0 .12'
				   ,'-+12', '+-1.2', '1.2.0', '+1-2.'
                   ,'.', '+', '-.', '+.', '-+'
				   )
	expected <- rep(FALSE, length(notNumbers))
	actual <- is.number(notNumbers)
	checkEquals(expected, actual
			   ,sprintf("Failing to detect non-number values: '%s'"
					   ,paste(notNumbers[actual], collapse="', '")
					   )
	           )


	expNotationNumbers <- c('1e4','-2e5','3e-6','-4e-7'
					       ,'5.01e8','-6.02e9','7.03e-10','-8.04e-11'
						   )
	expected <- rep(TRUE, length(expNotationNumbers))
	actual <- is.number(expNotationNumbers)
	checkEquals(expected, actual
			   ,sprintf("Failing to detect exponential notation values: '%s'"
					   ,paste(expNotationNumbers[actual], collapse="', '")
					   )
			   )


	NotexpNotationNumbers <- c('1e4.1','-2e5.2','3e-6.3','-4e-7.4'
							  ,'5.01e8.5','-6.02e9.6','7.03e-10.7','-8.04e-11.8'
							  ,'1e4e3','--2e5','3e-6-3','-4e-7e-8','1e2e3'
							  )
	expected <- rep(FALSE, length(NotexpNotationNumbers))
	actual <- is.number(NotexpNotationNumbers)
	checkEquals(expected, actual
			   ,sprintf("Failing to detect misformated exponential notation values: '%s'"
					   ,paste(NotexpNotationNumbers[actual], collapse="', '")
			           )
	           )
	
	
	missingValues <- c('',NA)
	expected <- rep(FALSE, length(missingValues))
	actual <- is.number(missingValues)
	checkEquals(expected, actual
			,sprintf("Failing to detect missing values: '%s'"
					,paste(missingValues[actual], collapse="', '")
			)
	)
	
}

# end of file