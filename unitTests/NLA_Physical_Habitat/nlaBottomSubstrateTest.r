# nlaBottomSubstrate.r
# RUnit tests
#
#  7/07/17 cws Renamed from metsBottomSubstrate.r to nlaBottomSubstrate.r and
#          modified for updated calling interface of nlaBottomSubstrate. Removed
#          stubs for undeveloped functions.  Added test cases for absent arguments.
#  7/10/17 cws Extended unit test to include absent arguments and missing values
#          for specific values (e.g. ODOR=NONE, COLOR=BROWN). Case of missing
#          values for all of a substrate code is not tested as the independent
#          calculations would be time consuming for now.
#  7/11/17 cws Split substrate argument into individual classes, to be consistent
#          with general interface.
#  7/17/17 cws Changed boulders argument to boulder, so all the classes are singular.
#


nlaBottomSubstrateTest <- function()
# unit test for nlaBottomSubstrate.
# Test with 2012 data is not performed because the only difference between the
# years is the coding of odor and color, and the code (and 2007 test data) are
# modified to use the 2012 coding.
{
    nlaBottomSubstrateTest.fullData()       # all arguments present with all codes
    nlaBottomSubstrateTest.absentData()     # some arguments present
    nlaBottomSubstrateTest.partialData()    # all arguments present but not all codes
#    nlaBottomSubstrateTest.miscodedData()   # all arguments present but codes are not expected.
}

nlaBottomSubstrateTest.fullData <- function()
# Test case with full data
{ 
	testData <- nlaBottomSubstrateTest.createTestData()
	
	expected <- nlaBottomSubstrateTest.createExpectedResults()
	actual <- nlaBottomSubstrate(color = testData %>% subset(PARAMETER %in% 'BS_COLOR') %>% 
	                                     select(SITE, STATION, VALUE)
	                            ,odor = testData %>% subset(PARAMETER %in% 'ODOR') %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,bedrock = testData %>% 
	                                       subset(PARAMETER %in% c('BS_BEDROCK')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,boulder = testData %>% 
	                                        subset(PARAMETER %in% c('BS_BOULDERS')) %>% 
	                                        select(SITE, STATION, VALUE)
	                            ,cobble = testData %>% 
	                                      subset(PARAMETER %in% c('BS_COBBLE')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,gravel = testData %>% 
	                                      subset(PARAMETER %in% c('BS_GRAVEL')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,organic = testData %>% 
	                                       subset(PARAMETER %in% c('BS_ORGANIC')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,sand = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SAND')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,silt = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SILT')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,wood = testData %>% 
	                                    subset(PARAMETER %in% c('BS_WOOD')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}

nlaBottomSubstrateTest.absentData <- function()
# Test cases with absent arguments
{	
	testData <- nlaBottomSubstrateTest.createTestData()
	expected <- nlaBottomSubstrateTest.createExpectedResults()

	actual <- nlaBottomSubstrate(color=NULL
	                            ,odor=testData %>% subset(PARAMETER %in% 'ODOR') %>% 
	                                  select(SITE, STATION, VALUE)
	                            ,bedrock = testData %>% 
	                                       subset(PARAMETER %in% c('BS_BEDROCK')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,boulder = testData %>% 
	                                        subset(PARAMETER %in% c('BS_BOULDERS')) %>% 
	                                        select(SITE, STATION, VALUE)
	                            ,cobble = testData %>% 
	                                      subset(PARAMETER %in% c('BS_COBBLE')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,gravel = testData %>% 
	                                      subset(PARAMETER %in% c('BS_GRAVEL')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,organic = testData %>% 
	                                       subset(PARAMETER %in% c('BS_ORGANIC')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,sand = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SAND')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,silt = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SILT')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,wood = testData %>% 
	                                    subset(PARAMETER %in% c('BS_WOOD')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            )
	
	diff <- dfCompare(expected %>% subset(METRIC %nin% c('BSFBLACK','BSFBROWN','BSFGRAY','BSFRED','BSFOTHERCOLOR','BSNCOLOR','BSOCOLOR'))
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics when color is NULL")
	
	actual <- nlaBottomSubstrate(color=testData %>% subset(PARAMETER %in% 'BS_COLOR') %>% 
	                                   select(SITE, STATION, VALUE)
	                            ,odor=NULL
	                            ,bedrock = testData %>% 
	                                       subset(PARAMETER %in% c('BS_BEDROCK')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,boulder = testData %>% 
	                                        subset(PARAMETER %in% c('BS_BOULDERS')) %>% 
	                                        select(SITE, STATION, VALUE)
	                            ,cobble = testData %>% 
	                                      subset(PARAMETER %in% c('BS_COBBLE')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,gravel = testData %>% 
	                                      subset(PARAMETER %in% c('BS_GRAVEL')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,organic = testData %>% 
	                                       subset(PARAMETER %in% c('BS_ORGANIC')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,sand = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SAND')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,silt = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SILT')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,wood = testData %>% 
	                                    subset(PARAMETER %in% c('BS_WOOD')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            )
	
	diff <- dfCompare(expected %>% subset(METRIC %nin% c('BSFANOXIC','BSFCHEMICAL','BSFH2S','BSFNONEODOR','BSFOIL','BSFOTHERODOR','BSNODOR','BSOODOR'))
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics when odor is NULL")
	
	actual <- nlaBottomSubstrate(color=testData %>% subset(PARAMETER %in% 'BS_COLOR') %>% 
	                                   select(SITE, STATION, VALUE)
	                            ,odor=testData %>% subset(PARAMETER %in% 'ODOR') %>% 
	                                  select(SITE, STATION, VALUE)
	                            # all substrates default to null so are not specified
	                            )
	
	diff <- dfCompare(expected %>% subset(METRIC %in% c('BSFBLACK','BSFBROWN','BSFGRAY','BSFRED','BSFOTHERCOLOR','BSNCOLOR','BSOCOLOR'
	                                                   ,'BSFANOXIC','BSFCHEMICAL','BSFH2S','BSFNONEODOR','BSFOIL','BSFOTHERODOR','BSNODOR','BSOODOR'
	                                                   )
	                                     )
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics when substrate is NULL")
	
	
}

nlaBottomSubstrateTest.partialData <- function()
# Test cases with arguments do not have all values present.
{	
	testData <- nlaBottomSubstrateTest.createTestData()

	# Test case when data have no BROWN color
	expected <- nlaBottomSubstrateTest.createExpectedResults() %>% 
	            subset(!(SITE %in% c(7470,7519,7545) & 
	                     METRIC %in% c('BSFBLACK','BSFBROWN','BSFGRAY','BSFRED','BSFOTHERCOLOR','BSNCOLOR','BSOCOLOR')
	                    )
	                  ) %>%
	            mutate(VALUE=ifelse(SITE==7472,
    	                            ifelse(METRIC=='BSFGRAY',  1
	                               ,ifelse(METRIC=='BSFBROWN', 0
	                               ,ifelse(METRIC=='BSNCOLOR', 3
	                               ,ifelse(METRIC=='BSOCOLOR', 'GRAY', VALUE
	                                ))))
	                        ,ifelse(SITE==7498,
	                                ifelse(METRIC=='BSFBLACK', 0.75
	                               ,ifelse(METRIC=='BSFBROWN', 0
	                               ,ifelse(METRIC=='BSFGRAY',  0.25
	                               ,ifelse(METRIC=='BSNCOLOR', 4, VALUE
	                                ))))
	                        ,VALUE
	                        ))
	                  )

	actual <- nlaBottomSubstrate(color=testData %>% subset(PARAMETER %in% 'BS_COLOR') %>%
	                                   mutate(VALUE = ifelse(VALUE == 'BROWN', NA, VALUE)) %>% 
	                                   select(SITE, STATION, VALUE)
	                            ,odor=testData %>% subset(PARAMETER %in% 'ODOR') %>% 
	                                  select(SITE, STATION, VALUE)
	                            ,bedrock = testData %>% 
	                                       subset(PARAMETER %in% c('BS_BEDROCK')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,boulder = testData %>% 
	                                        subset(PARAMETER %in% c('BS_BOULDERS')) %>% 
	                                        select(SITE, STATION, VALUE)
	                            ,cobble = testData %>% 
	                                      subset(PARAMETER %in% c('BS_COBBLE')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,gravel = testData %>% 
	                                      subset(PARAMETER %in% c('BS_GRAVEL')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,organic = testData %>% 
	                                       subset(PARAMETER %in% c('BS_ORGANIC')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,sand = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SAND')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,silt = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SILT')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,wood = testData %>% 
	                                    subset(PARAMETER %in% c('BS_WOOD')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            )
	
	diff <- dfCompare(expected 
	                ,actual
	                ,c('SITE','METRIC'), zeroFudge=1e-14
	                )
	checkTrue(is.null(diff), "Incorrect calculation of metrics when color BROWN is all absent")
	

	# Test case when data have no NONE odor
	actual <- nlaBottomSubstrate(color=testData %>% subset(PARAMETER %in% 'BS_COLOR') %>%
	                                   select(SITE, STATION, VALUE)
	                            ,odor=testData %>% subset(PARAMETER %in% 'ODOR') %>% 
                                      mutate(VALUE = ifelse(VALUE == 'NONE', NA, VALUE)) %>% 
	                                  select(SITE, STATION, VALUE)
	                            ,bedrock = testData %>% 
	                                       subset(PARAMETER %in% c('BS_BEDROCK')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,boulder = testData %>% 
	                                        subset(PARAMETER %in% c('BS_BOULDERS')) %>% 
	                                        select(SITE, STATION, VALUE)
	                            ,cobble = testData %>% 
	                                      subset(PARAMETER %in% c('BS_COBBLE')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,gravel = testData %>% 
	                                      subset(PARAMETER %in% c('BS_GRAVEL')) %>% 
	                                      select(SITE, STATION, VALUE)
	                            ,organic = testData %>% 
	                                       subset(PARAMETER %in% c('BS_ORGANIC')) %>% 
	                                       select(SITE, STATION, VALUE)
	                            ,sand = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SAND')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,silt = testData %>% 
	                                    subset(PARAMETER %in% c('BS_SILT')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            ,wood = testData %>% 
	                                    subset(PARAMETER %in% c('BS_WOOD')) %>% 
	                                    select(SITE, STATION, VALUE)
	                            )	
	expected <- nlaBottomSubstrateTest.createExpectedResults() %>% 
	            mutate(VALUE=ifelse(SITE==7470,
    	                            ifelse(METRIC=='BSFNONEODOR',  0        # was 1 
	                               ,ifelse(METRIC=='BSNODOR', 0             # was 10 
	                               ,ifelse(METRIC=='BSOODOR', '', VALUE     # was NONE
	                                )))
	                        ,ifelse(SITE==7472,
	                                ifelse(METRIC=='BSFNONEODOR', 0         # was 2/9
	                               ,ifelse(METRIC=='BSNODOR', 7, VALUE      # was 9
	                                ))
	                        ,ifelse(SITE==7498,
	                                ifelse(METRIC=='BSFNONEODOR', 0         # was 0.6
	                               ,ifelse(METRIC=='BSNODOR', 2             # was 5
	                               ,ifelse(METRIC=='BSOODOR', 'ANOXIC', VALUE  # was NONE
	                                )))
	                        ,ifelse(SITE==7519,
	                                ifelse(METRIC=='BSFNONEODOR', 0         # was 10/11
	                               ,ifelse(METRIC=='BSNODOR', 1             # was 11
	                               ,ifelse(METRIC=='BSOODOR', 'H2S', VALUE  # was 9
	                                )))
	                        ,ifelse(SITE==7545,
	                                ifelse(METRIC=='BSFNONEODOR', 0         # was 1
	                               ,ifelse(METRIC=='BSNODOR', 0             # was 10
	                               ,ifelse(METRIC=='BSOODOR', '', VALUE     # was NONE
	                                )))
	                        ,VALUE
	                        )))))
	                  )
	diff <- dfCompare(expected 
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics when odor NONE is all missing")
	
	
	# Test case when data have no SAND substrate is not implemented, would require
	# complex independent calcualtions.
# 	actual <- nlaBottomSubstrate(color=testData %>% subset(PARAMETER %in% 'BS_COLOR') %>%
# 	                                   select(SITE, STATION, VALUE)
# 	                            ,odor=testData %>% subset(PARAMETER %in% 'ODOR') %>% 
# 	                                  select(SITE, STATION, VALUE)
# 	                            ,substrate=testData %>% 
# 	                             dplyr::rename(CLASS=PARAMETER) %>%
#                                  mutate(VALUE = ifelse(VALUE == 'BS_SAND', NA, VALUE)) %>% 
# 	                             select(SITE, STATION, CLASS, VALUE)
# 	                            )	
# 
# 	diff <- dfCompare(expected 
# 	                 ,actual
# 	                 ,c('SITE','METRIC'), zeroFudge=1e-14
# 	                 )
# 	checkTrue(is.null(diff), "Incorrect calculation of metrics when substrate lacks BS_SAND")
}

nlaBottomSubstrateTest.createTestData <- function()
# Returns dataframe with test input using 2007 sites
#	SITE		Description
#	7470	10 stations, each with all expected parameters 
#	7472	10 stations, some parameters missing from 5 stations
#	7498	5 stations with data, 5 have no data
#	7519	11 stations, each with all expected parameters
#	7545	10 stations, some parameters missing from each.  This includes the
#           case where substrate cover is NA but should probably be '0'.
#
{
	tc <- textConnection("   SITE STATION   PARAMETER VALUE
							7470       A  BS_BEDROCK      0
							7470       A BS_BOULDERS      0
							7470       A   BS_COBBLE      0
							7470       A    BS_COLOR     BR
							7470       A   BS_GRAVEL      0
							7470       A     BS_ODOR      N
							7470       A  BS_ORGANIC      4
							7470       A     BS_SAND      0
							7470       A     BS_SILT      2
							7470       A     BS_WOOD      0
							7470       B  BS_BEDROCK      0
							7470       B BS_BOULDERS      0
							7470       B   BS_COBBLE      0
							7470       B    BS_COLOR     BR
							7470       B   BS_GRAVEL      0
							7470       B     BS_ODOR      N
							7470       B  BS_ORGANIC      4
							7470       B     BS_SAND      0
							7470       B     BS_SILT      1
							7470       B     BS_WOOD      0
							7470       C  BS_BEDROCK      0
							7470       C BS_BOULDERS      0
							7470       C   BS_COBBLE      0
							7470       C    BS_COLOR     BR
							7470       C   BS_GRAVEL      0
							7470       C     BS_ODOR      N
							7470       C  BS_ORGANIC      4
							7470       C     BS_SAND      0
							7470       C     BS_SILT      1
							7470       C     BS_WOOD      0
							7470       D  BS_BEDROCK      0
							7470       D BS_BOULDERS      0
							7470       D   BS_COBBLE      0
							7470       D    BS_COLOR     BR
							7470       D   BS_GRAVEL      0
							7470       D     BS_ODOR      N
							7470       D  BS_ORGANIC      4
							7470       D     BS_SAND      0
							7470       D     BS_SILT      1
							7470       D     BS_WOOD      0
							7470       E  BS_BEDROCK      0
							7470       E BS_BOULDERS      0
							7470       E   BS_COBBLE      0
							7470       E    BS_COLOR     BR
							7470       E   BS_GRAVEL      0
							7470       E     BS_ODOR      N
							7470       E  BS_ORGANIC      4
							7470       E     BS_SAND      0
							7470       E     BS_SILT      1
							7470       E     BS_WOOD      0
							7470       F  BS_BEDROCK      0
							7470       F BS_BOULDERS      0
							7470       F   BS_COBBLE      0
							7470       F    BS_COLOR     BR
							7470       F   BS_GRAVEL      0
							7470       F     BS_ODOR      N
							7470       F  BS_ORGANIC      4
							7470       F     BS_SAND      0
							7470       F     BS_SILT      0
							7470       F     BS_WOOD      0
							7470       G  BS_BEDROCK      0
							7470       G BS_BOULDERS      0
							7470       G   BS_COBBLE      0
							7470       G    BS_COLOR     BR
							7470       G   BS_GRAVEL      0
							7470       G     BS_ODOR      N
							7470       G  BS_ORGANIC      4
							7470       G     BS_SAND      0
							7470       G     BS_SILT      0
							7470       G     BS_WOOD      0
							7470       H  BS_BEDROCK      0
							7470       H BS_BOULDERS      0
							7470       H   BS_COBBLE      0
							7470       H    BS_COLOR     BR
							7470       H   BS_GRAVEL      0
							7470       H     BS_ODOR      N
							7470       H  BS_ORGANIC      4
							7470       H     BS_SAND      0
							7470       H     BS_SILT      0
							7470       H     BS_WOOD      0
							7470       I  BS_BEDROCK      0
							7470       I BS_BOULDERS      0
							7470       I   BS_COBBLE      0
							7470       I    BS_COLOR     BR
							7470       I   BS_GRAVEL      0
							7470       I     BS_ODOR      N
							7470       I  BS_ORGANIC      4
							7470       I     BS_SAND      0
							7470       I     BS_SILT      0
							7470       I     BS_WOOD      0
							7470       J  BS_BEDROCK      0
							7470       J BS_BOULDERS      0
							7470       J   BS_COBBLE      0
							7470       J    BS_COLOR     BR
							7470       J   BS_GRAVEL      0
							7470       J     BS_ODOR      N
							7470       J  BS_ORGANIC      4
							7470       J     BS_SAND      0
							7470       J     BS_SILT      0
							7470       J     BS_WOOD      0
							7472       A    BS_COLOR     BR
							7472       A     BS_ODOR      H
							7472       A     BS_SAND      2
							7472       A     BS_SILT      3
							7472       A     BS_WOOD      1
							7472       B    BS_COLOR     BR
							7472       B     BS_ODOR      H
							7472       B     BS_SAND      2
							7472       B     BS_SILT      4
							7472       C    BS_COLOR     BR
							7472       C     BS_ODOR      H
							7472       C     BS_SILT      4
							7472       D  BS_BEDROCK      0
							7472       D BS_BOULDERS      0
							7472       D   BS_COBBLE      0
							7472       D   BS_GRAVEL      3
							7472       D  BS_ORGANIC      0
							7472       D     BS_SAND      0
							7472       D     BS_SILT      2
							7472       D     BS_WOOD      0
							7472       E  BS_BEDROCK      0
							7472       E BS_BOULDERS      0
							7472       E   BS_COBBLE      0
							7472       E    BS_COLOR     BR
							7472       E   BS_GRAVEL      0
							7472       E     BS_ODOR      H
							7472       E  BS_ORGANIC      0
							7472       E     BS_SAND      2
							7472       E     BS_SILT      3
							7472       E     BS_WOOD      0
							7472       F  BS_BEDROCK      0
							7472       F BS_BOULDERS      0
							7472       F   BS_COBBLE      0
							7472       F    BS_COLOR     GY
							7472       F   BS_GRAVEL      0
							7472       F     BS_ODOR      H
							7472       F  BS_ORGANIC      0
							7472       F     BS_SAND      3
							7472       F     BS_SILT      3
							7472       F     BS_WOOD      0
							7472       G    BS_COLOR     BR
							7472       G     BS_ODOR      H
							7472       G     BS_SILT      4
							7472       H  BS_BEDROCK      0
							7472       H BS_BOULDERS      0
							7472       H   BS_COBBLE      0
							7472       H    BS_COLOR     GY
							7472       H   BS_GRAVEL      0
							7472       H     BS_ODOR      N
							7472       H  BS_ORGANIC      1
							7472       H     BS_SAND      3
							7472       H     BS_SILT      3
							7472       H     BS_WOOD      1
							7472       I  BS_BEDROCK      0
							7472       I BS_BOULDERS      0
							7472       I   BS_COBBLE      0
							7472       I    BS_COLOR     GY
							7472       I   BS_GRAVEL      0
							7472       I     BS_ODOR      N
							7472       I  BS_ORGANIC      0
							7472       I     BS_SAND      4
							7472       I     BS_SILT      2
							7472       I     BS_WOOD      0
							7472       J  BS_BEDROCK      0
							7472       J BS_BOULDERS      0
							7472       J   BS_COBBLE      0
							7472       J    BS_COLOR     BR
							7472       J   BS_GRAVEL      0
							7472       J     BS_ODOR      H
							7472       J  BS_ORGANIC      0
							7472       J     BS_SAND      4
							7472       J     BS_SILT      2
							7472       J     BS_WOOD      0
							7498       B    BS_COLOR     BR
							7498       B     BS_ODOR      N
							7498       B     BS_SAND      2
							7498       B     BS_SILT      2
							7498       C    BS_COLOR     GY
							7498       C     BS_ODOR      A
							7498       C     BS_SILT      4
							7498       H  BS_BEDROCK      0
							7498       H BS_BOULDERS      0
							7498       H   BS_COBBLE      0
							7498       H    BS_COLOR     BL
							7498       H   BS_GRAVEL      1
							7498       H     BS_ODOR      A
							7498       H  BS_ORGANIC      0
							7498       H     BS_SAND      1
							7498       H     BS_SILT      4
							7498       H     BS_WOOD      0
							7498       I  BS_BEDROCK      0
							7498       I BS_BOULDERS      0
							7498       I   BS_COBBLE      0
							7498       I    BS_COLOR     BL
							7498       I   BS_GRAVEL      1
							7498       I     BS_ODOR      N
							7498       I  BS_ORGANIC      1
							7498       I     BS_SAND      1
							7498       I     BS_SILT      3
							7498       I     BS_WOOD      1
							7498       J  BS_BEDROCK      0
							7498       J BS_BOULDERS      0
							7498       J   BS_COBBLE      1
							7498       J    BS_COLOR     BL
							7498       J   BS_GRAVEL      1
							7498       J     BS_ODOR      N
							7498       J  BS_ORGANIC      1
							7498       J     BS_SAND      2
							7498       J     BS_SILT      2
							7498       J     BS_WOOD      1
							7519       A  BS_BEDROCK      0
							7519       A BS_BOULDERS      1
							7519       A   BS_COBBLE      1
							7519       A    BS_COLOR     BR
							7519       A   BS_GRAVEL      2
							7519       A     BS_ODOR      N
							7519       A  BS_ORGANIC      1
							7519       A     BS_SAND      4
							7519       A     BS_SILT      0
							7519       A     BS_WOOD      1
							7519       B  BS_BEDROCK      0
							7519       B BS_BOULDERS      0
							7519       B   BS_COBBLE      1
							7519       B    BS_COLOR     BR
							7519       B   BS_GRAVEL      3
							7519       B     BS_ODOR      N
							7519       B  BS_ORGANIC      3
							7519       B     BS_SAND      3
							7519       B     BS_SILT      1
							7519       B     BS_WOOD      0
							7519       C  BS_BEDROCK      0
							7519       C BS_BOULDERS      0
							7519       C   BS_COBBLE      0
							7519       C    BS_COLOR     BR
							7519       C   BS_GRAVEL      0
							7519       C     BS_ODOR      N
							7519       C  BS_ORGANIC      3
							7519       C     BS_SAND      1
							7519       C     BS_SILT      4
							7519       C     BS_WOOD      0
							7519       D  BS_BEDROCK      0
							7519       D BS_BOULDERS      0
							7519       D   BS_COBBLE      3
							7519       D    BS_COLOR     BR
							7519       D   BS_GRAVEL      3
							7519       D     BS_ODOR      N
							7519       D  BS_ORGANIC      1
							7519       D     BS_SAND      2
							7519       D     BS_SILT      0
							7519       D     BS_WOOD      0
							7519       E  BS_BEDROCK      0
							7519       E BS_BOULDERS      1
							7519       E   BS_COBBLE      3
							7519       E    BS_COLOR     BR
							7519       E   BS_GRAVEL      2
							7519       E     BS_ODOR      H
							7519       E  BS_ORGANIC      1
							7519       E     BS_SAND      2
							7519       E     BS_SILT      0
							7519       E     BS_WOOD      0
							7519       F  BS_BEDROCK      0
							7519       F BS_BOULDERS      0
							7519       F   BS_COBBLE      1
							7519       F    BS_COLOR     BR
							7519       F   BS_GRAVEL      2
							7519       F     BS_ODOR      N
							7519       F  BS_ORGANIC      1
							7519       F     BS_SAND      4
							7519       F     BS_SILT      0
							7519       F     BS_WOOD      1
							7519       G  BS_BEDROCK      0
							7519       G BS_BOULDERS      0
							7519       G   BS_COBBLE      1
							7519       G    BS_COLOR     BR
							7519       G   BS_GRAVEL      2
							7519       G     BS_ODOR      N
							7519       G  BS_ORGANIC      1
							7519       G     BS_SAND      4
							7519       G     BS_SILT      0
							7519       G     BS_WOOD      0
							7519       H  BS_BEDROCK      0
							7519       H BS_BOULDERS      0
							7519       H   BS_COBBLE      0
							7519       H    BS_COLOR     BR
							7519       H   BS_GRAVEL      1
							7519       H     BS_ODOR      N
							7519       H  BS_ORGANIC      1
							7519       H     BS_SAND      4
							7519       H     BS_SILT      0
							7519       H     BS_WOOD      1
							7519       I  BS_BEDROCK      0
							7519       I BS_BOULDERS      0
							7519       I   BS_COBBLE      1
							7519       I    BS_COLOR     BR
							7519       I   BS_GRAVEL      1
							7519       I     BS_ODOR      N
							7519       I  BS_ORGANIC      1
							7519       I     BS_SAND      4
							7519       I     BS_SILT      0
							7519       I     BS_WOOD      1
							7519       J  BS_BEDROCK      0
							7519       J BS_BOULDERS      0
							7519       J   BS_COBBLE      0
							7519       J    BS_COLOR     BR
							7519       J   BS_GRAVEL      2
							7519       J     BS_ODOR      N
							7519       J  BS_ORGANIC      2
							7519       J     BS_SAND      4
							7519       J     BS_SILT      1
							7519       J     BS_WOOD      0
							7519       K  BS_BEDROCK      0
							7519       K BS_BOULDERS      1
							7519       K   BS_COBBLE      3
							7519       K    BS_COLOR     BR
							7519       K   BS_GRAVEL      2
							7519       K     BS_ODOR      N
							7519       K  BS_ORGANIC      1
							7519       K     BS_SAND      1
							7519       K     BS_SILT      0
							7519       K     BS_WOOD      0
							7545       A    BS_COLOR     BR
							7545       A   BS_GRAVEL      1
							7545       A     BS_ODOR      N
							7545       A     BS_SAND      0
							7545       A     BS_SILT      4
							7545       B    BS_COLOR     BR
							7545       B   BS_GRAVEL      1
							7545       B     BS_ODOR      N
							7545       B     BS_SAND      0
							7545       B     BS_SILT      4
							7545       C    BS_COLOR     BR
							7545       C     BS_ODOR      N
							7545       C     BS_SILT      4
							7545       D    BS_COLOR     BR
							7545       D     BS_ODOR      N
							7545       D     BS_SILT      4
							7545       E    BS_COLOR     BR
							7545       E     BS_ODOR      N
							7545       E     BS_SILT      4
							7545       F    BS_COLOR     BR
							7545       F     BS_ODOR      N
							7545       F     BS_SILT      4
							7545       G    BS_COLOR     BR
							7545       G   BS_GRAVEL      4
							7545       G     BS_ODOR      N
							7545       G     BS_SAND      1
							7545       G     BS_SILT      1
							7545       H    BS_COLOR     BR
							7545       H     BS_ODOR      N
							7545       I    BS_COLOR     BR
							7545       I   BS_GRAVEL      4
							7545       I     BS_ODOR      N
							7545       I     BS_SAND      1
							7545       I     BS_SILT      1
							7545       J    BS_COLOR     BR
							7545       J     BS_ODOR      N
						 ")
		 
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	# Modify 2007 data to use 2012 conventions
	fake <- within(fake
				  ,{VALUE <- ifelse(PARAMETER=='BS_COLOR'
							  	 ,ifelse(VALUE == 'BL', 'BLACK',
								  ifelse(VALUE == 'BR', 'BROWN',
								  ifelse(VALUE == 'GY', 'GRAY',
								  ifelse(VALUE == 'O',  'OTHER',
								  ifelse(VALUE == 'RD', 'RED', 'UNKNOWNCOLOR'
								  )))))
							 ,ifelse(PARAMETER == 'BS_ODOR'
								 ,ifelse(VALUE == 'A', 'ANOXIC',
								  ifelse(VALUE == 'C', 'CHEMICAL',
								  ifelse(VALUE == 'H', 'H2S',
							  	  ifelse(VALUE == 'N', 'NONE',
								  ifelse(VALUE == 'O', 'OTHER',
							 	  ifelse(VALUE == 'P', 'OIL', 'UNKNOWNODOR'
								  ))))))
							 ,VALUE
							 ))
					PARAMETER <- ifelse(PARAMETER=='BS_ODOR', 'ODOR', PARAMETER)
				   }
				  )
	
	return(fake)		
}


nlaBottomSubstrateTest.createExpectedResults <- function()
# Expected values taken from 2007 database and edited to correct
# for floating point issues and change in how counts of absent data
# are handled (changed from 0 to NA).
{
	tc <- textConnection("  SITE         METRIC                VALUE
							7470       BS16LDIA   -0.351820729134696
							7470       BS25LDIA   -0.351820729134696
							7470       BS50LDIA   -0.175910364567348
							7470       BS75LDIA                    0
							7470       BS84LDIA                    0
							7470      BSFANOXIC                    0
							7470       BSFBLACK                    0
							7470       BSFBROWN                    1
							7470    BSFCBEDROCK                    0
							7470   BSFCBOULDERS                    0
							7470     BSFCCOBBLE                    0
							7470     BSFCGRAVEL                    0
							7470    BSFCHEMICAL                    0
							7470    BSFCORGANIC    0.956156156156156
							7470       BSFCSAND                    0
							7470       BSFCSILT   0.0438438438438438
							7470       BSFCWOOD                    0
							7470        BSFGRAY                    0
							7470         BSFH2S                    0
							7470    BSFNONEODOR                    1
							7470         BSFOIL                    0
							7470  BSFOTHERCOLOR                    0
							7470   BSFOTHERODOR                    0
							7470    BSFPBEDROCK                    0
							7470   BSFPBOULDERS                    0
							7470     BSFPCOBBLE                    0
							7470     BSFPGRAVEL                    0
							7470    BSFPORGANIC                    1
							7470       BSFPSAND                    0
							7470       BSFPSILT                  0.5
							7470       BSFPWOOD                    0
							7470         BSFRED                    0
							7470 BSISITEVARIETY                    2
							7470  BSISTAVARIETY                  1.5
							7470     BSNBEDROCK                   10
							7470    BSNBOULDERS                   10
							7470      BSNCOBBLE                   10
							7470       BSNCOLOR                   10
							7470      BSNGRAVEL                   10
							7470        BSNODOR                   10
							7470     BSNORGANIC                   10
							7470        BSNSAND                   10
							7470        BSNSILT                   10
							7470        BSNWOOD                   10
							7470       BSOCOLOR                   BR
							7470      BSOFCLASS              Organic
							7470        BSOODOR                    N
							7470      BSOPCLASS              Organic
							7470     BSVBEDROCK                    0
							7470    BSVBOULDERS                    0
							7470      BSVCOBBLE                    0
							7470      BSVGRAVEL                    0
							7470        BSVLDIA    0.185425805354467
							7470     BSVORGANIC   0.0681886701716996
							7470        BSVSAND                    0
							7470        BSVSILT   0.0681886701716996
							7470        BSVWOOD                    0
							7470        BSXLDIA   -0.175910364567348
							7472       BS16LDIA    -2.11092437480818
							7472       BS25LDIA   -0.872071632089423
							7472       BS50LDIA   -0.241369599229273
							7472       BS75LDIA   -0.137865081267586
							7472       BS84LDIA   -0.137865081267586
							7472      BSFANOXIC                    0
							7472       BSFBLACK                    0
							7472       BSFBROWN    0.666666666666667
							7472    BSFCBEDROCK                    0
							7472   BSFCBOULDERS                    0
							7472     BSFCCOBBLE                    0
							7472     BSFCGRAVEL    0.116161616161616
							7472    BSFCHEMICAL                    0
							7472    BSFCORGANIC  0.00666666666666667
							7472       BSFCSAND    0.415815295815296
							7472       BSFCSILT    0.583936507936508
							7472       BSFCWOOD   0.0138775510204082
							7472        BSFGRAY    0.333333333333333
							7472         BSFH2S    0.777777777777778
							7472    BSFNONEODOR    0.222222222222222
							7472         BSFOIL                    0
							7472  BSFOTHERCOLOR                    0
							7472   BSFOTHERODOR                    0
							7472    BSFPBEDROCK                    0
							7472   BSFPBOULDERS                    0
							7472     BSFPCOBBLE                    0
							7472     BSFPGRAVEL    0.166666666666667
							7472    BSFPORGANIC    0.166666666666667
							7472       BSFPSAND                0.875
							7472       BSFPSILT                    1
							7472       BSFPWOOD    0.285714285714286
							7472         BSFRED                    0
							7472 BSISITEVARIETY                    5
							7472  BSISTAVARIETY                  2.1
							7472     BSNBEDROCK                    6
							7472    BSNBOULDERS                    6
							7472      BSNCOBBLE                    6
							7472       BSNCOLOR                    9
							7472      BSNGRAVEL                    6
							7472        BSNODOR                    9
							7472     BSNORGANIC                    6
							7472        BSNSAND                    8
							7472        BSNSILT                   10
							7472        BSNWOOD                    7
							7472       BSOCOLOR                   BR
							7472      BSOFCLASS                 Silt
							7472        BSOODOR                    H
							7472      BSOPCLASS                 Silt
							7472     BSVBEDROCK                    0
							7472    BSVBOULDERS                    0
							7472      BSVCOBBLE                    0
							7472      BSVGRAVEL    0.284536687292995
							7472        BSVLDIA    0.804326386435922
							7472     BSVORGANIC   0.0163299316185545
							7472        BSVSAND    0.270211661745652
							7472        BSVSILT    0.291531608432167
							7472        BSVWOOD   0.0242115714971883
							7472        BSXLDIA   -0.685627559710669
							7498       BS16LDIA    -2.11092437480818
							7498       BS25LDIA   -0.642833437946091
							7498       BS50LDIA   -0.310666503857225
							7498       BS75LDIA   -0.292375737067238
							7498       BS84LDIA   -0.134736163649582
							7498      BSFANOXIC                  0.4
							7498       BSFBLACK                  0.6
							7498       BSFBROWN                  0.2
							7498    BSFCBEDROCK                    0
							7498   BSFCBOULDERS                    0
							7498     BSFCCOBBLE   0.0238095238095238
							7498     BSFCGRAVEL    0.0624089172476269	# modified to accomodate floating point diff
							7498    BSFCHEMICAL                    0
							7498    BSFCORGANIC   0.0453149001536098
							7498       BSFCSAND    0.243235259364292
							7498       BSFCSILT    0.699302847689945
							7498       BSFCWOOD   0.0453149001536098
							7498        BSFGRAY                  0.2
							7498         BSFH2S                    0
							7498    BSFNONEODOR                  0.6
							7498         BSFOIL                    0
							7498  BSFOTHERCOLOR                    0
							7498   BSFOTHERODOR                    0
							7498    BSFPBEDROCK                    0
							7498   BSFPBOULDERS                    0
							7498     BSFPCOBBLE    0.333333333333333
							7498     BSFPGRAVEL                    1
							7498    BSFPORGANIC    0.666666666666667
							7498       BSFPSAND                    1
							7498       BSFPSILT                    1
							7498       BSFPWOOD    0.666666666666667
							7498         BSFRED                    0
							7498 BSISITEVARIETY                    6
							7498  BSISTAVARIETY                  3.4
							7498     BSNBEDROCK                    3
							7498    BSNBOULDERS                    3
							7498      BSNCOBBLE                    3
							7498       BSNCOLOR                    5
							7498      BSNGRAVEL                    3
							7498        BSNODOR                    5
							7498     BSNORGANIC                    3
							7498        BSNSAND                    4
							7498        BSNSILT                    5
							7498        BSNWOOD                    3
							7498       BSOCOLOR                   BL
							7498      BSOFCLASS                 Silt
							7498        BSOODOR                    N
							7498      BSOPCLASS 'Gravel, Sand, Silt'
							7498     BSVBEDROCK                    0
							7498    BSVBOULDERS                    0
							7498      BSVCOBBLE   0.0412393049421161
							7498      BSVGRAVEL   0.0102372273898992
							7498        BSVLDIA    0.811052186323006
							7498     BSVORGANIC   0.0393957560775182
							7498        BSVSAND    0.221878070499023
							7498        BSVSILT    0.268462338469131
							7498        BSVWOOD   0.0393957560775182
							7498        BSXLDIA   -0.698307243465663
							7519       BS16LDIA  -0.0630951271374716
							7519       BS25LDIA  -0.0418931548797554
							7519       BS50LDIA -0.00487290199741594
							7519       BS75LDIA    0.202310121076583
							7519       BS84LDIA    0.223256799551928
							7519      BSFANOXIC                    0
							7519       BSFBLACK                    0
							7519       BSFBROWN                    1
							7519    BSFCBEDROCK                    0
							7519   BSFCBOULDERS   0.0119610083480015
							7519     BSFCCOBBLE    0.151575796913805
							7519     BSFCGRAVEL    0.18548785933249 	# modified to accomodate floating point diff
							7519    BSFCHEMICAL                    0
							7519    BSFCORGANIC    0.110474400176448
							7519       BSFCSAND    0.46613166577939		# modified to accomodate floating point diff
							7519       BSFCSILT   0.0587107557186865
							7519       BSFCWOOD   0.0156585137311789
							7519        BSFGRAY                    0
							7519         BSFH2S    0.0909090909090909	# modified to accomodate floating point diff
							7519    BSFNONEODOR    0.909090909090909
							7519         BSFOIL                    0
							7519  BSFOTHERCOLOR                    0
							7519   BSFOTHERODOR                    0
							7519    BSFPBEDROCK                    0
							7519   BSFPBOULDERS    0.272727272727273
							7519     BSFPCOBBLE    0.727272727272727
							7519     BSFPGRAVEL    0.909090909090909
							7519    BSFPORGANIC                    1
							7519       BSFPSAND                    1
							7519       BSFPSILT    0.272727272727273
							7519       BSFPWOOD    0.363636363636364
							7519         BSFRED                    0
							7519 BSISITEVARIETY                    7
							7519  BSISTAVARIETY     4.54545454545455
							7519     BSNBEDROCK                   11
							7519    BSNBOULDERS                   11
							7519      BSNCOBBLE                   11
							7519       BSNCOLOR                   11
							7519      BSNGRAVEL                   11
							7519        BSNODOR                   11
							7519     BSNORGANIC                   11
							7519        BSNSAND                   11
							7519        BSNSILT                   11
							7519        BSNWOOD                   11
							7519       BSOCOLOR                   BR
							7519      BSOFCLASS                 Sand
							7519        BSOODOR                    N
							7519      BSOPCLASS      'Organic, Sand'	# 2012 ties are alphabetized
							7519     BSVBEDROCK                    0
							7519    BSVBOULDERS   0.0207144738430341
							7519      BSVCOBBLE    0.223459164284133
							7519      BSVGRAVEL    0.11836016317692		# modified to accomodate floating point diff
							7519        BSVLDIA    0.170018357050459
							7519     BSVORGANIC    0.125515403373414
							7519        BSVSAND    0.312010618893256
							7519        BSVSILT    0.174454307624145
							7519        BSVWOOD   0.0219263592510881
							7519        BSXLDIA   0.0262184222173992
							7545       BS16LDIA    -2.11092437480818
							7545       BS25LDIA    -2.11092437480818
							7545       BS50LDIA    -1.37877364302859
							7545       BS75LDIA   -0.187698177995143
							7545       BS84LDIA    0.271226555258709
							7545      BSFANOXIC                    0
							7545       BSFBLACK                    0
							7545       BSFBROWN                    1
							#7545    BSFCBEDROCK                   NA	# absent data no longer included in mets
							#7545   BSFCBOULDERS                   NA	# absent data no longer included in mets
							#7545     BSFCCOBBLE                   NA	# absent data no longer included in mets
							7545     BSFCGRAVEL    0.475744975744976
							7545    BSFCHEMICAL                    0
							#7545    BSFCORGANIC                   NA	# absent data no longer included in mets
							7545       BSFCSAND   0.0256410256410256
							7545       BSFCSILT    0.749306999306999 	# modified to accomodate floating point diff
							#7545       BSFCWOOD                   NA	# absent data no longer included in mets
							7545        BSFGRAY                    0
							7545         BSFH2S                    0
							7545    BSFNONEODOR                    1
							7545         BSFOIL                    0
							7545  BSFOTHERCOLOR                    0
							7545   BSFOTHERODOR                    0
							#7545    BSFPBEDROCK                   NA	# absent data no longer included in mets
							#7545   BSFPBOULDERS                   NA	# absent data no longer included in mets
							#7545     BSFPCOBBLE                   NA	# absent data no longer included in mets
							7545     BSFPGRAVEL                    1
							#7545    BSFPORGANIC                   NA	# absent data no longer included in mets
							7545       BSFPSAND                  0.5
							7545       BSFPSILT                    1
							#7545       BSFPWOOD                   NA	# absent data no longer included in mets
							7545         BSFRED                    0
							7545 BSISITEVARIETY                    3	# was NA in 2007, missing data handled better now
							7545  BSISTAVARIETY                 1.75
							#7545     BSNBEDROCK                   0	# absent data no longer included in mets
							#7545    BSNBOULDERS                   0	# absent data no longer included in mets
							#7545      BSNCOBBLE                   0	# absent data no longer included in mets
							7545       BSNCOLOR                   10
							7545      BSNGRAVEL                    4
							7545        BSNODOR                   10
							#7545     BSNORGANIC                   0	# absent data no longer included in mets
							7545        BSNSAND                    4
							7545        BSNSILT                    8
							#7545        BSNWOOD                   0	# absent data no longer included in mets
							7545       BSOCOLOR                   BR
							7545      BSOFCLASS                 Silt
							7545        BSOODOR                    N
							7545      BSOPCLASS       'Gravel, Silt'
							#7545     BSVBEDROCK                   NA	# absent data no longer included in mets
							#7545    BSVBOULDERS                   NA	# absent data no longer included in mets
							#7545      BSVCOBBLE                   NA	# absent data no longer included in mets
							7545      BSVGRAVEL     0.48692673430615
							7545        BSVLDIA     1.08496515094688
							#7545     BSVORGANIC                   NA	# absent data no longer included in mets
							7545        BSVSAND   0.0296077061122885
							7545        BSVSILT    0.431475761475635
							#7545        BSVWOOD                   NA	# absent data no longer included in mets
							7545        BSXLDIA    -1.14931127640166
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)

	
	# Modify 2007 data to use 2012 coding
	fake <- within(fake
				  ,VALUE <- ifelse(METRIC=='BSOCOLOR'
								   ,gsub('BL', 'BLACK', 
								 	gsub('BR', 'BROWN',
									gsub('GY', 'GRAY',
									gsub('O',  'OTHER',
									gsub('RD', 'RED', VALUE
									)))))
							,ifelse(METRIC=='BSOODOR'
								   ,gsub('A', 'ANOXIC',
									gsub('C', 'CHEMICAL',
									gsub('H', 'H2S',
									gsub('N', 'NONE',
									gsub('O', 'OTHER',
									gsub('P', 'OIL', VALUE
									))))))
							,VALUE
							))
				  )
	
	return(fake)		
	
}

# end of file