# nlaFishCoverNLA.r
# RUnit tests
#
#  7/10/17 cws renamed functions from metsFishCoverNLA* to nlaFishCover, changed
#          from UID & RESULT to SITE and VALUE.
#


nlaFishCover.splitParameterNamesTest <- function()
# Unit test for nlaFishCover.splitParameterNames
{
	testParams <- c('FC_a_DD','FC_b_LIT','FC_c_SYN','FC_c_SIM','FC_c_SYN0','FC_d')
	expected <- list(base=c('FC_a','FC_b','FC_c','FC_c','FC_c','FC_d')
	                ,suffix=c('_DD','_LIT','_SYN','_SIM','_SYN0','')
			        )
	actual <- nlaFishCover.splitParameterNames(testParams)
	
	checkEquals(expected, actual, 'Incorrect splitting of parameter names')
	
}



nlaFishCoverTest <- function()
# unit test for nlaFishCover
{
	nlaFishCoverTest.2007()	
	nlaFishCoverTest.withDrawDown()	
	nlaFishCoverTest.withDrawDownAndFillin()
}



nlaFishCoverTest.2007 <- function()
# Tests fish cover calculation with 2007 data
# The complex testing here is due to the precision differences between
# calculated and retrieved values, requiring separate tests for presence (names)
# type and value of the metrics.
{
	testData <- nlaFishCoverTest.createTestData2007() #%>% mutate(SITE=as.integer(SITE), VALUE=as.character(VALUE))
	expected <- nlaFishCoverTest.expectedResults2007()
	actual <- nlaFishCover(aquatic = testData %>% subset(PARAMETER=='FC_AQUATIC') %>% select(SITE, STATION, VALUE)
                          ,aquatic_dd = NULL
                          ,boulders = testData %>% subset(PARAMETER=='FC_BOULDERS') %>% select(SITE, STATION, VALUE)
                          ,boulders_dd = NULL
                          ,brush = testData %>% subset(PARAMETER=='FC_BRUSH') %>% select(SITE, STATION, VALUE)
                          ,brush_dd = NULL
                          ,ledges = testData %>% subset(PARAMETER=='FC_LEDGES') %>% select(SITE, STATION, VALUE)
                          ,ledges_dd = NULL
                          ,livetrees = testData %>% subset(PARAMETER=='FC_LIVETREES') %>% select(SITE, STATION, VALUE)
                          ,livetrees_dd = NULL
                          ,overhang = testData %>% subset(PARAMETER=='FC_OVERHANG') %>% select(SITE, STATION, VALUE)
                          ,overhang_dd = NULL
                          ,snags = testData %>% subset(PARAMETER=='FC_SNAGS') %>% select(SITE, STATION, VALUE)
                          ,snags_dd = NULL
                          ,structures = testData %>% subset(PARAMETER=='FC_STRUCTURES') %>% select(SITE, STATION, VALUE)
                          ,structures_dd = NULL
	                      ,createSyntheticCovers=FALSE
	                      ,fillinDrawdown=FALSE
	                      )

	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns in 2007")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics in 2007")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics in 2007")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-15)
	checkTrue(is.null(diff), "Incorrect calculation of metrics in 2007")
	
	# Test case when fillinDrawdown is TRUE, but data for drawdown and horizontal 
	# distance of drawdown is absent
	actual <- nlaFishCover(aquatic = testData %>% subset(PARAMETER=='FC_AQUATIC') %>% select(SITE, STATION, VALUE)
                          ,boulders = testData %>% subset(PARAMETER=='FC_BOULDERS') %>% select(SITE, STATION, VALUE)
                          ,brush = testData %>% subset(PARAMETER=='FC_BRUSH') %>% select(SITE, STATION, VALUE)
                          ,ledges = testData %>% subset(PARAMETER=='FC_LEDGES') %>% select(SITE, STATION, VALUE)
                          ,livetrees = testData %>% subset(PARAMETER=='FC_LIVETREES') %>% select(SITE, STATION, VALUE)
                          ,overhang = testData %>% subset(PARAMETER=='FC_OVERHANG') %>% select(SITE, STATION, VALUE)
                          ,snags = testData %>% subset(PARAMETER=='FC_SNAGS') %>% select(SITE, STATION, VALUE)
                          ,structures = testData %>% subset(PARAMETER=='FC_STRUCTURES') %>% select(SITE, STATION, VALUE)
	                      ,fillinDrawdown=TRUE
	                      ,createSyntheticCovers=FALSE
	                      )
	expected <- "Data for both horizontalDistance_dd and drawdown are required to fill-in missing cover values.  Either provide that data or set fillinDrawdown to FALSE"
	checkEquals(expected, actual, "Incorrect response when attempting to fill-in drawdown values without that data")
	
	actual <- nlaFishCover(aquatic = testData %>% subset(PARAMETER=='FC_AQUATIC') %>% select(SITE, STATION, VALUE)
                          ,boulders = testData %>% subset(PARAMETER=='FC_BOULDERS') %>% select(SITE, STATION, VALUE)
                          ,brush = testData %>% subset(PARAMETER=='FC_BRUSH') %>% select(SITE, STATION, VALUE)
                          ,ledges = testData %>% subset(PARAMETER=='FC_LEDGES') %>% select(SITE, STATION, VALUE)
                          ,livetrees = testData %>% subset(PARAMETER=='FC_LIVETREES') %>% select(SITE, STATION, VALUE)
                          ,overhang = testData %>% subset(PARAMETER=='FC_OVERHANG') %>% select(SITE, STATION, VALUE)
                          ,snags = testData %>% subset(PARAMETER=='FC_SNAGS') %>% select(SITE, STATION, VALUE)
                          ,structures = testData %>% subset(PARAMETER=='FC_STRUCTURES') %>% select(SITE, STATION, VALUE)
	                      ,fillinDrawdown=FALSE
                          ,createSyntheticCovers=TRUE
	                      )
	expected <- "Data for both horizontalDistance_dd is required to calculate synthetic drawdown cover values.  Either provide that data or set createSyntheticCovers to FALSE"
	checkEquals(expected, actual, "Incorrect response when attempting to fill-in drawdown values without that data")
}



nlaFishCoverTest.withDrawDown <- function()
# Tests fish cover calculation with drawdown data, but do NOT fill in drawdown values.
# The complex testing here is due to the precision differences between
# calculated and retrieved values, requiring separate tests for presence (names)
# type and value of the metrics.
{
	testData <- nlaFishCoverTest.createTestDataWithDrawDown()
	expected <- nlaFishCoverTest.expectedResultsWithDrawDownAndNoFillin()
	expected$VALUE <- as.numeric(expected$VALUE)
	actual <- nlaFishCover(aquatic =       testData %>% subset(PARAMETER=='FC_AQUATIC') %>% select(SITE, STATION, VALUE)
                          ,aquatic_dd =    testData %>% subset(PARAMETER=='FC_AQUATIC_DD') %>% select(SITE, STATION, VALUE)
                          ,boulders =      testData %>% subset(PARAMETER=='FC_BOULDERS') %>% select(SITE, STATION, VALUE)
                          ,boulders_dd =   testData %>% subset(PARAMETER=='FC_BOULDERS_DD') %>% select(SITE, STATION, VALUE)
                          ,brush =         testData %>% subset(PARAMETER=='FC_BRUSH') %>% select(SITE, STATION, VALUE)
                          ,brush_dd =      testData %>% subset(PARAMETER=='FC_BRUSH_DD') %>% select(SITE, STATION, VALUE)
                          ,ledges =        testData %>% subset(PARAMETER=='FC_LEDGES') %>% select(SITE, STATION, VALUE)
                          ,ledges_dd =     testData %>% subset(PARAMETER=='FC_LEDGES_DD') %>% select(SITE, STATION, VALUE)
                          ,livetrees =     testData %>% subset(PARAMETER=='FC_LIVETREES') %>% select(SITE, STATION, VALUE)
                          ,livetrees_dd =  testData %>% subset(PARAMETER=='FC_LIVETREES_DD') %>% select(SITE, STATION, VALUE)
                          ,overhang =      testData %>% subset(PARAMETER=='FC_OVERHANG') %>% select(SITE, STATION, VALUE)
                          ,overhang_dd =   testData %>% subset(PARAMETER=='FC_OVERHANG_DD') %>% select(SITE, STATION, VALUE)
                          ,snags =         testData %>% subset(PARAMETER=='FC_SNAGS') %>% select(SITE, STATION, VALUE)
                          ,snags_dd =      testData %>% subset(PARAMETER=='FC_SNAGS_DD') %>% select(SITE, STATION, VALUE)
                          ,structures =    testData %>% subset(PARAMETER=='FC_STRUCTURES') %>% select(SITE, STATION, VALUE)
                          ,structures_dd = testData %>% subset(PARAMETER=='FC_STRUCTURES_DD') %>% select(SITE, STATION, VALUE)
                          ,horizontalDistance_dd = testData %>% subset(PARAMETER=='HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
                          ,drawdown =              testData %>% subset(PARAMETER=='DRAWDOWN') %>% select(SITE, STATION, VALUE)
                          ,fillinDrawdown=FALSE
                          )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawdown")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
return(diff)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawDown")
	
}



nlaFishCoverTest.withDrawDownAndFillin <- function()
# Tests fish cover calculation with drawdown data, and filling in drawdown values.
{
	testData <- nlaFishCoverTest.createTestDataWithDrawDown()
	expected <- nlaFishCoverTest.expectedResultsWithDrawDownAndFillin()
	actual <- nlaFishCover(aquatic =       testData %>% subset(PARAMETER=='FC_AQUATIC') %>% select(SITE, STATION, VALUE)
                          ,aquatic_dd =    testData %>% subset(PARAMETER=='FC_AQUATIC_DD') %>% select(SITE, STATION, VALUE)
                          ,boulders =      testData %>% subset(PARAMETER=='FC_BOULDERS') %>% select(SITE, STATION, VALUE)
                          ,boulders_dd =   testData %>% subset(PARAMETER=='FC_BOULDERS_DD') %>% select(SITE, STATION, VALUE)
                          ,brush =         testData %>% subset(PARAMETER=='FC_BRUSH') %>% select(SITE, STATION, VALUE)
                          ,brush_dd =      testData %>% subset(PARAMETER=='FC_BRUSH_DD') %>% select(SITE, STATION, VALUE)
                          ,ledges =        testData %>% subset(PARAMETER=='FC_LEDGES') %>% select(SITE, STATION, VALUE)
                          ,ledges_dd =     testData %>% subset(PARAMETER=='FC_LEDGES_DD') %>% select(SITE, STATION, VALUE)
                          ,livetrees =     testData %>% subset(PARAMETER=='FC_LIVETREES') %>% select(SITE, STATION, VALUE)
                          ,livetrees_dd =  testData %>% subset(PARAMETER=='FC_LIVETREES_DD') %>% select(SITE, STATION, VALUE)
                          ,overhang =      testData %>% subset(PARAMETER=='FC_OVERHANG') %>% select(SITE, STATION, VALUE)
                          ,overhang_dd =   testData %>% subset(PARAMETER=='FC_OVERHANG_DD') %>% select(SITE, STATION, VALUE)
                          ,snags =         testData %>% subset(PARAMETER=='FC_SNAGS') %>% select(SITE, STATION, VALUE)
                          ,snags_dd =      testData %>% subset(PARAMETER=='FC_SNAGS_DD') %>% select(SITE, STATION, VALUE)
                          ,structures =    testData %>% subset(PARAMETER=='FC_STRUCTURES') %>% select(SITE, STATION, VALUE)
                          ,structures_dd = testData %>% subset(PARAMETER=='FC_STRUCTURES_DD') %>% select(SITE, STATION, VALUE)
                          ,horizontalDistance_dd = testData %>% subset(PARAMETER=='HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
                          ,drawdown =              testData %>% subset(PARAMETER=='DRAWDOWN') %>% select(SITE, STATION, VALUE)
                          )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawdown")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-9)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawDown")
	
}



nlaFishCoverTest.createTestData2007 <- function()
# This data is based on NLA2007 data for the following VISIT_ID:
#	7469	Full complement of data stations A - J (as expected)
#	7472	Incomplete data at A, B; else full complement
#	7545	Incomplete data at stations all stations A-J
#   7546	Incomplete or missing data at most stations A-J
#	7723	Full complement of data, but with two relocated stations (as expected)
#	7797	Full complement of data stations A - J plus K, L and M stations
#	8152	Data missing at all but 3 stations
#	8905	Incomplete data at B, C, I, J, plus extra station K
#	
#fc07 <- dbFetch(nla07, 'tblRESULTS', where="PARAMETER in ('FC_AQUATIC', 'FC_BOULDERS','FC_BRUSH', 'FC_LEDGES','FC_LIVETREES', 'FC_OVERHANG','FC_SNAGS', 'FC_STRUCTURES')")
#tt<-subset(fc07, VISIT_ID %in% c(7469,7797,8905,7472, 7545,7546,8152, 7723), select=c(VISIT_ID,STATION_NO,PARAMETER,VALUE,UNITS))
#tt <- tt[order(tt$VISIT_ID,tt$STATION_NO,tt$PARAMETER),]	
{
	tc <- textConnection("SITE     STATION   PARAMETER   VALUE   UNITS
				          7469          A    FC_AQUATIC      4 X         
				          7469          A   FC_BOULDERS      0 X         
				          7469          A      FC_BRUSH      0 X         
				          7469          A     FC_LEDGES      0 X         
				          7469          A  FC_LIVETREES      0 X         
				          7469          A   FC_OVERHANG      0 X         
				          7469          A      FC_SNAGS      0 X         
				          7469          A FC_STRUCTURES      0 X         
				          7469          B    FC_AQUATIC      4 X         
				          7469          B   FC_BOULDERS      0 X         
				          7469          B      FC_BRUSH      2 X         
				          7469          B     FC_LEDGES      0 X         
				          7469          B  FC_LIVETREES      0 X         
				          7469          B   FC_OVERHANG      0 X         
				          7469          B      FC_SNAGS      0 X         
				          7469          B FC_STRUCTURES      0 X         
				          7469          C    FC_AQUATIC      4 X         
				          7469          C   FC_BOULDERS      0 X         
				          7469          C      FC_BRUSH      0 X         
				          7469          C     FC_LEDGES      0 X         
				          7469          C  FC_LIVETREES      0 X         
				          7469          C   FC_OVERHANG      0 X         
				          7469          C      FC_SNAGS      0 X         
				          7469          C FC_STRUCTURES      0 X         
				          7469          D    FC_AQUATIC      4 X         
				          7469          D   FC_BOULDERS      0 X         
				          7469          D      FC_BRUSH      1 X         
				          7469          D     FC_LEDGES      0 X         
				          7469          D  FC_LIVETREES      0 X         
				          7469          D   FC_OVERHANG      0 X         
				          7469          D      FC_SNAGS      0 X         
				          7469          D FC_STRUCTURES      0 X         
				          7469          E    FC_AQUATIC      4 X         
				          7469          E   FC_BOULDERS      0 X         
				          7469          E      FC_BRUSH      0 X         
				          7469          E     FC_LEDGES      0 X         
				          7469          E  FC_LIVETREES      0 X         
				          7469          E   FC_OVERHANG      0 X         
				          7469          E      FC_SNAGS      0 X         
				          7469          E FC_STRUCTURES      0 X         
				          7469          F    FC_AQUATIC      3 X         
				          7469          F   FC_BOULDERS      0 X         
				          7469          F      FC_BRUSH      0 X         
				          7469          F     FC_LEDGES      0 X         
				          7469          F  FC_LIVETREES      0 X         
				          7469          F   FC_OVERHANG      1 X         
				          7469          F      FC_SNAGS      0 X         
				          7469          F FC_STRUCTURES      0 X         
				          7469          G    FC_AQUATIC      4 X         
				          7469          G   FC_BOULDERS      0 X         
				          7469          G      FC_BRUSH      0 X         
				          7469          G     FC_LEDGES      0 X         
				          7469          G  FC_LIVETREES      0 X         
				          7469          G   FC_OVERHANG      0 X         
				          7469          G      FC_SNAGS      0 X         
				          7469          G FC_STRUCTURES      0 X         
				          7469          H    FC_AQUATIC      3 X         
				          7469          H   FC_BOULDERS      0 X         
				          7469          H      FC_BRUSH      0 X         
				          7469          H     FC_LEDGES      0 X         
				          7469          H  FC_LIVETREES      0 X         
				          7469          H   FC_OVERHANG      0 X         
				          7469          H      FC_SNAGS      0 X         
				          7469          H FC_STRUCTURES      0 X         
				          7469          I    FC_AQUATIC      4 X         
				          7469          I   FC_BOULDERS      0 X         
				          7469          I      FC_BRUSH      0 X         
				          7469          I     FC_LEDGES      0 X         
				          7469          I  FC_LIVETREES      0 X         
				          7469          I   FC_OVERHANG      0 X         
				          7469          I      FC_SNAGS      0 X         
				          7469          I FC_STRUCTURES      0 X         
				          7469          J    FC_AQUATIC      4 X         
				          7469          J   FC_BOULDERS      0 X         
				          7469          J      FC_BRUSH      0 X         
				          7469          J     FC_LEDGES      0 X         
				          7469          J  FC_LIVETREES      0 X         
				          7469          J   FC_OVERHANG      0 X         
				          7469          J      FC_SNAGS      0 X         
				          7469          J FC_STRUCTURES      0 X         
				          7472          A    FC_AQUATIC      1 X         
				          7472          A      FC_BRUSH      1 X         
				          7472          A  FC_LIVETREES      0 X         
				          7472          A   FC_OVERHANG      1 X         
				          7472          B    FC_AQUATIC      4 X         
				          7472          C    FC_AQUATIC      1 X         
				          7472          C   FC_BOULDERS      0 X         
				          7472          C      FC_BRUSH      1 X         
				          7472          C     FC_LEDGES      0 X         
				          7472          C  FC_LIVETREES      0 X         
				          7472          C   FC_OVERHANG      1 X         
				          7472          C      FC_SNAGS      0 X         
				          7472          C FC_STRUCTURES      0 X         
				          7472          D    FC_AQUATIC      1 X         
				          7472          D   FC_BOULDERS      0 X         
				          7472          D      FC_BRUSH      0 X         
				          7472          D     FC_LEDGES      0 X         
				          7472          D  FC_LIVETREES      0 X         
				          7472          D   FC_OVERHANG      0 X         
				          7472          D      FC_SNAGS      0 X         
				          7472          D FC_STRUCTURES      1 X         
				          7472          E    FC_AQUATIC      1 X         
				          7472          E   FC_BOULDERS      0 X         
				          7472          E      FC_BRUSH      0 X         
				          7472          E     FC_LEDGES      0 X         
				          7472          E  FC_LIVETREES      0 X         
				          7472          E   FC_OVERHANG      1 X         
				          7472          E      FC_SNAGS      0 X         
				          7472          E FC_STRUCTURES      0 X         
				          7472          F    FC_AQUATIC      1 X         
				          7472          F   FC_BOULDERS      0 X         
				          7472          F      FC_BRUSH      0 X         
				          7472          F     FC_LEDGES      0 X         
				          7472          F  FC_LIVETREES      0 X         
				          7472          F   FC_OVERHANG      0 X         
				          7472          F      FC_SNAGS      0 X         
				          7472          F FC_STRUCTURES      0 X         
				          7472          G    FC_AQUATIC      1 X         
				          7472          G   FC_BOULDERS      0 X         
				          7472          G      FC_BRUSH      2 X         
				          7472          G     FC_LEDGES      0 X         
				          7472          G  FC_LIVETREES      0 X         
				          7472          G   FC_OVERHANG      2 X         
				          7472          G      FC_SNAGS      0 X         
				          7472          G FC_STRUCTURES      0 X         
				          7472          H    FC_AQUATIC      1 X         
				          7472          H   FC_BOULDERS      0 X         
				          7472          H      FC_BRUSH      1 X         
				          7472          H     FC_LEDGES      0 X         
				          7472          H  FC_LIVETREES      0 X         
				          7472          H   FC_OVERHANG      1 X         
				          7472          H      FC_SNAGS      0 X         
				          7472          H FC_STRUCTURES      0 X         
				          7472          I    FC_AQUATIC      1 X         
				          7472          I   FC_BOULDERS      0 X         
				          7472          I      FC_BRUSH      1 X         
				          7472          I     FC_LEDGES      0 X         
				          7472          I  FC_LIVETREES      0 X         
				          7472          I   FC_OVERHANG      1 X         
				          7472          I      FC_SNAGS      0 X         
				          7472          I FC_STRUCTURES      0 X         
				          7472          J    FC_AQUATIC      1 X         
				          7472          J   FC_BOULDERS      0 X         
				          7472          J      FC_BRUSH      1 X         
				          7472          J     FC_LEDGES      0 X         
				          7472          J  FC_LIVETREES      0 X         
				          7472          J   FC_OVERHANG      2 X         
				          7472          J      FC_SNAGS      0 X         
				          7472          J FC_STRUCTURES      0 X         
				          7545          A   FC_BOULDERS      0 X         
				          7545          A     FC_LEDGES      0 X         
				          7545          A  FC_LIVETREES      0 X         
				          7545          A   FC_OVERHANG      0 X         
				          7545          A FC_STRUCTURES      0 X         
				          7545          B   FC_BOULDERS      0 X         
				          7545          B     FC_LEDGES      0 X         
				          7545          B  FC_LIVETREES      0 X         
				          7545          B   FC_OVERHANG      0 X         
				          7545          B FC_STRUCTURES      0 X         
				          7545          C   FC_BOULDERS      0 X         
				          7545          C     FC_LEDGES      0 X         
				          7545          C  FC_LIVETREES      0 X         
				          7545          C   FC_OVERHANG      0 X         
				          7545          C FC_STRUCTURES      0 X         
				          7545          D   FC_BOULDERS      0 X         
				          7545          D     FC_LEDGES      0 X         
				          7545          D  FC_LIVETREES      0 X         
				          7545          D   FC_OVERHANG      0 X         
				          7545          D FC_STRUCTURES      0 X         
				          7545          E   FC_BOULDERS      0 X         
				          7545          E     FC_LEDGES      0 X         
				          7545          E  FC_LIVETREES      0 X         
				          7545          E   FC_OVERHANG      0 X         
				          7545          E FC_STRUCTURES      0 X         
				          7545          F   FC_BOULDERS      0 X         
				          7545          F     FC_LEDGES      0 X         
				          7545          F  FC_LIVETREES      0 X         
				          7545          F   FC_OVERHANG      0 X         
				          7545          F FC_STRUCTURES      0 X         
				          7545          G    FC_AQUATIC      2 X         
				          7545          G   FC_BOULDERS      0 X         
				          7545          G     FC_LEDGES      0 X         
				          7545          G  FC_LIVETREES      0 X         
				          7545          G   FC_OVERHANG      0 X         
				          7545          G FC_STRUCTURES      0 X         
				          7545          H   FC_BOULDERS      0 X         
				          7545          H     FC_LEDGES      0 X         
				          7545          H  FC_LIVETREES      0 X         
				          7545          H   FC_OVERHANG      0 X         
				          7545          H FC_STRUCTURES      0 X         
				          7545          I   FC_BOULDERS      0 X         
				          7545          I     FC_LEDGES      0 X         
				          7545          I  FC_LIVETREES      0 X         
				          7545          I   FC_OVERHANG      0 X         
				          7545          I FC_STRUCTURES      0 X         
				          7545          J   FC_BOULDERS      0 X         
				          7545          J     FC_LEDGES      0 X         
				          7545          J  FC_LIVETREES      0 X         
				          7545          J   FC_OVERHANG      0 X         
				          7545          J FC_STRUCTURES      0 X         
				          7546          A   FC_BOULDERS      0 X         
				          7546          A     FC_LEDGES      0 X         
				          7546          A  FC_LIVETREES      0 X         
				          7546          A   FC_OVERHANG      0 X         
				          7546          A FC_STRUCTURES      0 X         
				          7546          B   FC_BOULDERS      0 X         
				          7546          B     FC_LEDGES      0 X         
				          7546          B  FC_LIVETREES      0 X         
				          7546          B   FC_OVERHANG      0 X         
				          7546          B FC_STRUCTURES      0 X         
				          7546          C   FC_BOULDERS      0 X         
				          7546          C     FC_LEDGES      0 X         
				          7546          C  FC_LIVETREES      0 X         
				          7546          C   FC_OVERHANG      0 X         
				          7546          C FC_STRUCTURES      0 X         
				          7546          D   FC_BOULDERS      0 X         
				          7546          D     FC_LEDGES      0 X         
				          7546          D  FC_LIVETREES      0 X         
				          7546          D   FC_OVERHANG      0 X         
				          7546          D FC_STRUCTURES      0 X         
				          7546          E   FC_BOULDERS      0 X         
				          7546          E     FC_LEDGES      0 X         
				          7546          E  FC_LIVETREES      0 X         
				          7546          E   FC_OVERHANG      0 X         
				          7546          E FC_STRUCTURES      0 X         
				          7546          H    FC_AQUATIC      4 X         
				          7546          H   FC_BOULDERS      0 X         
				          7546          H      FC_BRUSH      0 X         
				          7546          H     FC_LEDGES      0 X         
				          7546          H  FC_LIVETREES      0 X         
				          7546          H   FC_OVERHANG      0 X         
				          7546          H      FC_SNAGS      0 X         
				          7546          H FC_STRUCTURES      0 X         
				          7546          I    FC_AQUATIC      3 X         
				          7546          I   FC_BOULDERS      0 X         
				          7546          I      FC_BRUSH      0 X         
				          7546          I     FC_LEDGES      0 X         
				          7546          I  FC_LIVETREES      0 X         
				          7546          I   FC_OVERHANG      0 X         
				          7546          I      FC_SNAGS      0 X         
				          7546          I FC_STRUCTURES      0 X         
				          7546          J    FC_AQUATIC      3 X         
				          7546          J   FC_BOULDERS      0 X         
				          7546          J      FC_BRUSH      0 X         
				          7546          J     FC_LEDGES      0 X         
				          7546          J  FC_LIVETREES      0 X         
				          7546          J   FC_OVERHANG      0 X         
				          7546          J      FC_SNAGS      0 X         
				          7546          J FC_STRUCTURES      0 X         
				          7723          A    FC_AQUATIC      4 X         
				          7723          A   FC_BOULDERS      0 X         
				          7723          A      FC_BRUSH      1 X         
				          7723          A     FC_LEDGES      0 X         
				          7723          A  FC_LIVETREES      0 X         
				          7723          A   FC_OVERHANG      2 X         
				          7723          A      FC_SNAGS      0 X         
				          7723          A FC_STRUCTURES      0 X         
				          7723          B    FC_AQUATIC      4 X         
				          7723          B   FC_BOULDERS      0 X         
				          7723          B      FC_BRUSH      0 X         
				          7723          B     FC_LEDGES      0 X         
				          7723          B  FC_LIVETREES      0 X         
				          7723          B   FC_OVERHANG      1 X         
				          7723          B      FC_SNAGS      0 X         
				          7723          B FC_STRUCTURES      0 X         
				          7723          C    FC_AQUATIC      4 X         
				          7723          C   FC_BOULDERS      0 X         
				          7723          C      FC_BRUSH      1 X         
				          7723          C     FC_LEDGES      0 X         
				          7723          C  FC_LIVETREES      0 X         
				          7723          C   FC_OVERHANG      2 X         
				          7723          C      FC_SNAGS      0 X         
				          7723          C FC_STRUCTURES      0 X         
				          7723          D    FC_AQUATIC      3 X         
				          7723          D   FC_BOULDERS      0 X         
				          7723          D      FC_BRUSH      1 X         
				          7723          D     FC_LEDGES      0 X         
				          7723          D  FC_LIVETREES      1 X         
				          7723          D   FC_OVERHANG      1 X         
				          7723          D      FC_SNAGS      0 X         
				          7723          D FC_STRUCTURES      0 X         
				          7723          E    FC_AQUATIC      4 X         
				          7723          E   FC_BOULDERS      1 X         
				          7723          E      FC_BRUSH      1 X         
				          7723          E     FC_LEDGES      1 X         
				          7723          E  FC_LIVETREES      0 X         
				          7723          E   FC_OVERHANG      0 X         
				          7723          E      FC_SNAGS      0 X         
				          7723          E FC_STRUCTURES      1 X         
				          7723          F    FC_AQUATIC      4 X         
				          7723          F   FC_BOULDERS      0 X         
				          7723          F      FC_BRUSH      1 X         
				          7723          F     FC_LEDGES      0 X         
				          7723          F  FC_LIVETREES      0 X         
				          7723          F   FC_OVERHANG      1 X         
				          7723          F      FC_SNAGS      0 X         
				          7723          F FC_STRUCTURES      1 X         
				          7723          I    FC_AQUATIC      4 X         
				          7723          I   FC_BOULDERS      0 X         
				          7723          I      FC_BRUSH      2 X         
				          7723          I     FC_LEDGES      0 X         
				          7723          I  FC_LIVETREES      0 X         
				          7723          I   FC_OVERHANG      1 X         
				          7723          I      FC_SNAGS      0 X         
				          7723          I FC_STRUCTURES      0 X         
				          7723          J    FC_AQUATIC      4 X         
				          7723          J   FC_BOULDERS      0 X         
				          7723          J      FC_BRUSH      0 X         
				          7723          J     FC_LEDGES      0 X         
				          7723          J  FC_LIVETREES      0 X         
				          7723          J   FC_OVERHANG      0 X         
				          7723          J      FC_SNAGS      0 X         
				          7723          J FC_STRUCTURES      0 X         
				          7723          Q    FC_AQUATIC      4 X         
				          7723          Q   FC_BOULDERS      0 X         
				          7723          Q      FC_BRUSH      1 X         
				          7723          Q     FC_LEDGES      0 X         
				          7723          Q  FC_LIVETREES      1 X         
				          7723          Q   FC_OVERHANG      1 X         
				          7723          Q      FC_SNAGS      0 X         
				          7723          Q FC_STRUCTURES      0 X         
				          7723          R    FC_AQUATIC      4 X         
				          7723          R   FC_BOULDERS      0 X         
				          7723          R      FC_BRUSH      1 X         
				          7723          R     FC_LEDGES      0 X         
				          7723          R  FC_LIVETREES      0 X         
				          7723          R   FC_OVERHANG      1 X         
				          7723          R      FC_SNAGS      0 X         
				          7723          R FC_STRUCTURES      0 X         
				          7797          A    FC_AQUATIC      3 X         
				          7797          A   FC_BOULDERS      1 X         
				          7797          A      FC_BRUSH      0 X         
				          7797          A     FC_LEDGES      0 X         
				          7797          A  FC_LIVETREES      0 X         
				          7797          A   FC_OVERHANG      0 X         
				          7797          A      FC_SNAGS      0 X         
				          7797          A FC_STRUCTURES      0 X         
				          7797          B    FC_AQUATIC      4 X         
				          7797          B   FC_BOULDERS      0 X         
				          7797          B      FC_BRUSH      1 X         
				          7797          B     FC_LEDGES      0 X         
				          7797          B  FC_LIVETREES      0 X         
				          7797          B   FC_OVERHANG      0 X         
				          7797          B      FC_SNAGS      1 X         
				          7797          B FC_STRUCTURES      0 X         
				          7797          C    FC_AQUATIC      0 X         
				          7797          C   FC_BOULDERS      0 X         
				          7797          C      FC_BRUSH      1 X         
				          7797          C     FC_LEDGES      0 X         
				          7797          C  FC_LIVETREES      0 X         
				          7797          C   FC_OVERHANG      1 X         
				          7797          C      FC_SNAGS      1 X         
				          7797          C FC_STRUCTURES      0 X         
				          7797          D    FC_AQUATIC      4 X         
				          7797          D   FC_BOULDERS      0 X         
				          7797          D      FC_BRUSH      1 X         
				          7797          D     FC_LEDGES      0 X         
				          7797          D  FC_LIVETREES      0 X         
				          7797          D   FC_OVERHANG      0 X         
				          7797          D      FC_SNAGS      1 X         
				          7797          D FC_STRUCTURES      0 X         
				          7797          E    FC_AQUATIC      4 X         
				          7797          E   FC_BOULDERS      0 X         
				          7797          E      FC_BRUSH      1 X         
				          7797          E     FC_LEDGES      0 X         
				          7797          E  FC_LIVETREES      0 X         
				          7797          E   FC_OVERHANG      0 X         
				          7797          E      FC_SNAGS      1 X         
				          7797          E FC_STRUCTURES      0 X         
				          7797          F    FC_AQUATIC      4 X         
				          7797          F   FC_BOULDERS      0 X         
				          7797          F      FC_BRUSH      1 X         
				          7797          F     FC_LEDGES      0 X         
				          7797          F  FC_LIVETREES      0 X         
				          7797          F   FC_OVERHANG      0 X         
				          7797          F      FC_SNAGS      1 X         
				          7797          F FC_STRUCTURES      0 X         
				          7797          G    FC_AQUATIC      4 X         
				          7797          G   FC_BOULDERS      0 X         
				          7797          G      FC_BRUSH      2 X         
				          7797          G     FC_LEDGES      0 X         
				          7797          G  FC_LIVETREES      0 X         
				          7797          G   FC_OVERHANG      0 X         
				          7797          G      FC_SNAGS      1 X         
				          7797          G FC_STRUCTURES      0 X         
				          7797          H    FC_AQUATIC      2 X         
				          7797          H   FC_BOULDERS      0 X         
				          7797          H      FC_BRUSH      1 X         
				          7797          H     FC_LEDGES      0 X         
				          7797          H  FC_LIVETREES      0 X         
				          7797          H   FC_OVERHANG      1 X         
				          7797          H      FC_SNAGS      1 X         
				          7797          H FC_STRUCTURES      0 X         
				          7797          I    FC_AQUATIC      1 X         
				          7797          I   FC_BOULDERS      0 X         
				          7797          I      FC_BRUSH      0 X         
				          7797          I     FC_LEDGES      0 X         
				          7797          I  FC_LIVETREES      0 X         
				          7797          I   FC_OVERHANG      0 X         
				          7797          I      FC_SNAGS      0 X         
				          7797          I FC_STRUCTURES      1 X         
				          7797          J    FC_AQUATIC      2 X         
				          7797          J   FC_BOULDERS      0 X         
				          7797          J      FC_BRUSH      0 X         
				          7797          J     FC_LEDGES      0 X         
				          7797          J  FC_LIVETREES      0 X         
				          7797          J   FC_OVERHANG      0 X         
				          7797          J      FC_SNAGS      0 X         
				          7797          J FC_STRUCTURES      0 X         
				          7797          K    FC_AQUATIC      3 X         
				          7797          K   FC_BOULDERS      0 X         
				          7797          K      FC_BRUSH      2 X         
				          7797          K     FC_LEDGES      0 X         
				          7797          K  FC_LIVETREES      0 X         
				          7797          K   FC_OVERHANG      0 X         
				          7797          K      FC_SNAGS      2 X         
				          7797          K FC_STRUCTURES      0 X         
				          7797          L    FC_AQUATIC      2 X         
				          7797          L   FC_BOULDERS      0 X         
				          7797          L      FC_BRUSH      0 X         
				          7797          L     FC_LEDGES      0 X         
				          7797          L  FC_LIVETREES      0 X         
				          7797          L   FC_OVERHANG      0 X         
				          7797          L      FC_SNAGS      0 X         
				          7797          L FC_STRUCTURES      0 X         
				          7797          M    FC_AQUATIC      3 X         
				          7797          M   FC_BOULDERS      0 X         
				          7797          M      FC_BRUSH      0 X         
				          7797          M     FC_LEDGES      0 X         
				          7797          M  FC_LIVETREES      0 X         
				          7797          M   FC_OVERHANG      0 X         
				          7797          M      FC_SNAGS      0 X         
				          7797          M FC_STRUCTURES      0 X         
				          8152          E    FC_AQUATIC      2 X         
				          8152          E   FC_BOULDERS      0 X         
				          8152          E      FC_BRUSH      0 X         
				          8152          E     FC_LEDGES      0 X         
				          8152          E  FC_LIVETREES      0 X         
				          8152          E   FC_OVERHANG      0 X         
				          8152          E      FC_SNAGS      0 X         
				          8152          E FC_STRUCTURES      0 X         
				          8152          F    FC_AQUATIC      0 X         
				          8152          F   FC_BOULDERS      1 X         
				          8152          F      FC_BRUSH      0 X         
				          8152          F     FC_LEDGES      0 X         
				          8152          F  FC_LIVETREES      0 X         
				          8152          F   FC_OVERHANG      0 X         
				          8152          F      FC_SNAGS      0 X         
				          8152          F FC_STRUCTURES      0 X         
				          8152          G    FC_AQUATIC      0 X         
				          8152          G   FC_BOULDERS      1 X         
				          8152          G      FC_BRUSH      0 X         
				          8152          G     FC_LEDGES      0 X         
				          8152          G  FC_LIVETREES      0 X         
				          8152          G   FC_OVERHANG      0 X         
				          8152          G      FC_SNAGS      0 X         
				          8152          G FC_STRUCTURES      0 X         
				          8905          B    FC_AQUATIC      0 X         
				          8905          B   FC_BOULDERS      0 X         
				          8905          B      FC_BRUSH      0 X         
				          8905          B     FC_LEDGES      0 X         
				          8905          B  FC_LIVETREES      0 X         
				          8905          B   FC_OVERHANG      0 X         
				          8905          B      FC_SNAGS      0 X         
				          8905          B FC_STRUCTURES      0 X         
				          8905          C    FC_AQUATIC      4 X         
				          8905          C   FC_BOULDERS      0 X         
				          8905          C      FC_BRUSH      0 X         
				          8905          C     FC_LEDGES      0 X         
				          8905          C  FC_LIVETREES      0 X         
				          8905          C   FC_OVERHANG      0 X         
				          8905          C      FC_SNAGS      0 X         
				          8905          C FC_STRUCTURES      0 X         
				          8905          I    FC_AQUATIC      3 X         
				          8905          I   FC_BOULDERS      0 X         
				          8905          I      FC_BRUSH      0 X         
				          8905          I     FC_LEDGES      0 X         
				          8905          I  FC_LIVETREES      0 X         
				          8905          I   FC_OVERHANG      0 X         
				          8905          I      FC_SNAGS      0 X         
				          8905          I FC_STRUCTURES      0 X         
				          8905          J    FC_AQUATIC      1 X         
				          8905          J   FC_BOULDERS      0 X         
				          8905          J      FC_BRUSH      0 X         
				          8905          J     FC_LEDGES      0 X         
				          8905          J  FC_LIVETREES      0 X         
				          8905          J   FC_OVERHANG      0 X         
				          8905          J      FC_SNAGS      0 X         
				          8905          J FC_STRUCTURES      4 X         
				          8905          K    FC_AQUATIC      4 X         
				          8905          K   FC_BOULDERS      0 X         
				          8905          K      FC_BRUSH      0 X         
				          8905          K     FC_LEDGES      0 X         
				          8905          K  FC_LIVETREES      0 X         
				          8905          K   FC_OVERHANG      0 X         
				          8905          K      FC_SNAGS      0 X         
				          8905          K FC_STRUCTURES      0 X   
                          hasNAvalues   A      FC_SNAGS      1 X
                          hasNAvalues   B      FC_SNAGS      1 X
                          hasNAvalues   C      FC_SNAGS      1 X
                          hasNAvalues   D      FC_SNAGS     NA X
                          hasNAvalues   E      FC_SNAGS      1 X
                          hasNAvalues   F      FC_SNAGS      1 X
                          hasNAvalues   G      FC_SNAGS      1 X
                          has2NAvalues  A      FC_SNAGS      1 X
                          has2NAvalues  B      FC_SNAGS      1 X
                          has2NAvalues  C      FC_SNAGS      1 X
                          has2NAvalues  D      FC_SNAGS     NA X
                          has2NAvalues  D      FC_BRUSH      1 X
                          has2NAvalues  E      FC_SNAGS     NA X
                          has2NAvalues  F      FC_SNAGS      1 X
                          has2NAvalues  G      FC_SNAGS      1 X"
						)
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	
	return(fake)
}



nlaFishCoverTest.expectedResults2007 <- function()
# Values expected based on test data.  These were taken directly from tblPHABMET_LONG
# in the 2007 NLA databse.
#phab <- sqlFetch(nla07, 'tblPHABMET_LONG', stringsAsFactors=FALSE) 
#tt<-subset(phab, VISIT_ID %in% c(7469,7797,8905,7472, 7545,7546,8152, 7723) & grepl('^fc', PARAMETER))
#tt <- tt[order(tt$VISIT_ID,tt$PARAMETER), c('VISIT_ID','PARAMETER','VALUE')]	
{
	
	tc <- textConnection("  SITE         METRIC               VALUE
						  	7469    FCFPAQUATIC                   1
						  	7472    FCFPAQUATIC                   1
						  	7545    FCFPAQUATIC                   1
						  	7546    FCFPAQUATIC                   1
						  	7723    FCFPAQUATIC                   1
						  	7797    FCFPAQUATIC   0.923076923076923
						  	8152    FCFPAQUATIC   0.333333333333333
							8905    FCFPAQUATIC                 0.8
							7469   FCFPBOULDERS                   0
							7472   FCFPBOULDERS                   0
							7545   FCFPBOULDERS                   0
							7546   FCFPBOULDERS                   0
							7723   FCFPBOULDERS                 0.1
							7797   FCFPBOULDERS  0.0769230769230769
							8152   FCFPBOULDERS   0.666666666666667
							8905   FCFPBOULDERS                   0
							7469      FCFPBRUSH                 0.2
							7472      FCFPBRUSH   0.666666666666667
#							7545      FCFPBRUSH                  NA
							7546      FCFPBRUSH                   0
							7723      FCFPBRUSH                 0.8
							7797      FCFPBRUSH   0.615384615384615
							8152      FCFPBRUSH                   0
							8905      FCFPBRUSH                   0
							7469     FCFPLEDGES                   0
							7472     FCFPLEDGES                   0
							7545     FCFPLEDGES                   0
							7546     FCFPLEDGES                   0
							7723     FCFPLEDGES                 0.1
							7797     FCFPLEDGES                   0
							8152     FCFPLEDGES                   0
							8905     FCFPLEDGES                   0
							7469  FCFPLIVETREES                   0
							7472  FCFPLIVETREES                   0
							7545  FCFPLIVETREES                   0
							7546  FCFPLIVETREES                   0
							7723  FCFPLIVETREES                 0.2
							7797  FCFPLIVETREES                   0
							8152  FCFPLIVETREES                   0
							8905  FCFPLIVETREES                   0
							7469   FCFPOVERHANG                 0.1
							7472   FCFPOVERHANG   0.777777777777778
							7545   FCFPOVERHANG                   0
							7546   FCFPOVERHANG                   0
							7723   FCFPOVERHANG                 0.8
							7797   FCFPOVERHANG   0.153846153846154
							8152   FCFPOVERHANG                   0
							8905   FCFPOVERHANG                   0
							7469      FCFPSNAGS                   0
							7472      FCFPSNAGS                   0
#							7545      FCFPSNAGS                  NA
							7546      FCFPSNAGS                   0
							7723      FCFPSNAGS                   0
							7797      FCFPSNAGS   0.615384615384615
							8152      FCFPSNAGS                   0
							8905      FCFPSNAGS                   0
							7469 FCFPSTRUCTURES                   0
							7472 FCFPSTRUCTURES               0.125
							7545 FCFPSTRUCTURES                   0
							7546 FCFPSTRUCTURES                   0
							7723 FCFPSTRUCTURES                 0.2
							7797 FCFPSTRUCTURES  0.0769230769230769
							8152 FCFPSTRUCTURES                   0
							8905 FCFPSTRUCTURES                 0.2
							7469    FCFCAQUATIC               0.815
							7472    FCFCAQUATIC              0.1325
							7545    FCFCAQUATIC                0.25
							7546    FCFCAQUATIC               0.675
							7723    FCFCAQUATIC               0.845
							7797    FCFCAQUATIC   0.530769230769231
							8152    FCFCAQUATIC  0.0833333333333333
							8905    FCFCAQUATIC               0.475
							7469   FCFCBOULDERS                   0
							7472   FCFCBOULDERS                   0
							7545   FCFCBOULDERS                   0
							7546   FCFCBOULDERS                   0
							7723   FCFCBOULDERS               0.005
							7797   FCFCBOULDERS 0.00384615384615385
							8152   FCFCBOULDERS  0.0333333333333333
							8905   FCFCBOULDERS                   0
							7469      FCFCBRUSH                0.03
							7472      FCFCBRUSH  0.0555555555555556
#							7545      FCFCBRUSH                  NA
							7546      FCFCBRUSH                   0
							7723      FCFCBRUSH                0.06
							7797      FCFCBRUSH  0.0615384615384615
							8152      FCFCBRUSH                   0
							8905      FCFCBRUSH                   0
							7469     FCFCLEDGES                   0
							7472     FCFCLEDGES                   0
							7545     FCFCLEDGES                   0
							7546     FCFCLEDGES                   0
							7723     FCFCLEDGES               0.005
							7797     FCFCLEDGES                   0
							8152     FCFCLEDGES                   0
							8905     FCFCLEDGES                   0
							7469  FCFCLIVETREES                   0
							7472  FCFCLIVETREES                   0
							7545  FCFCLIVETREES                   0
							7546  FCFCLIVETREES                   0
							7723  FCFCLIVETREES                0.01
							7797  FCFCLIVETREES                   0
							8152  FCFCLIVETREES                   0
							8905  FCFCLIVETREES                   0
							7469   FCFCOVERHANG               0.005
							7472   FCFCOVERHANG  0.0833333333333333
							7545   FCFCOVERHANG                   0
							7546   FCFCOVERHANG                   0
							7723   FCFCOVERHANG                0.08
							7797   FCFCOVERHANG 0.00769230769230769
							8152   FCFCOVERHANG                   0
							8905   FCFCOVERHANG                   0
							7469      FCFCSNAGS                   0
							7472      FCFCSNAGS                   0
#							7545      FCFCSNAGS                  NA
							7546      FCFCSNAGS                   0
							7723      FCFCSNAGS                   0
							7797      FCFCSNAGS  0.0461538461538462
							8152      FCFCSNAGS                   0
							8905      FCFCSNAGS                   0
							7469 FCFCSTRUCTURES                   0
							7472 FCFCSTRUCTURES             0.00625
							7545 FCFCSTRUCTURES                   0
							7546 FCFCSTRUCTURES                   0
							7723 FCFCSTRUCTURES                0.01
							7797 FCFCSTRUCTURES 0.00384615384615385
							8152 FCFCSTRUCTURES                   0
							8905 FCFCSTRUCTURES               0.175
							7469         FCIALL                0.85
							7472         FCIALL   0.277638888888889
							7545         FCIALL                0.25
							7546         FCIALL               0.675
							7723         FCIALL               1.015
							7797         FCIALL   0.653846153846154
							8152         FCIALL   0.116666666666667
							8905         FCIALL                0.65
							7469         FCIBIG               0.005
							7472         FCIBIG  0.0895833333333333
							7545         FCIBIG                   0
							7546         FCIBIG                   0
							7723         FCIBIG                 0.1
							7797         FCIBIG  0.0153846153846154
							8152         FCIBIG  0.0333333333333333
							8905         FCIBIG               0.175
							7469     FCINATURAL                0.85
							7472     FCINATURAL   0.271388888888889
							7545     FCINATURAL                0.25
							7546     FCINATURAL               0.675
							7723     FCINATURAL               1.005
							7797     FCINATURAL                0.65
							8152     FCINATURAL   0.116666666666667
							8905     FCINATURAL               0.475
							7469      FCIRIPVEG                0.03
							7472      FCIRIPVEG  0.0555555555555556
							7545      FCIRIPVEG                   0
							7546      FCIRIPVEG                   0
							7723      FCIRIPVEG                0.07
							7797      FCIRIPVEG   0.107692307692308
							8152      FCIRIPVEG                   0
							8905      FCIRIPVEG                   0
							7469     FCVAQUATIC   0.126491106406735
							7472     FCVAQUATIC   0.260887906963891
							7545     FCVAQUATIC                  NA
							7546     FCVAQUATIC   0.173205080756888
							7723     FCVAQUATIC  0.0948683298050514
							7797     FCVAQUATIC   0.335123398627569
							8152     FCVAQUATIC   0.144337567297406
							8905     FCVAQUATIC   0.429025057543263
							7469    FCVBOULDERS                   0
							7472    FCVBOULDERS                   0
							7545    FCVBOULDERS                   0
							7546    FCVBOULDERS                   0
							7723    FCVBOULDERS  0.0158113883008419
							7797    FCVBOULDERS  0.0138675049056307
							8152    FCVBOULDERS  0.0288675134594813
							8905    FCVBOULDERS                   0
							7469       FCVBRUSH  0.0788810637746615
							7472       FCVBRUSH  0.0768295371441074
#							7545       FCVBRUSH                  NA
							7546       FCVBRUSH                   0
							7723       FCVBRUSH  0.0699205898780101
							7797       FCVBRUSH  0.0869718492622904
							8152       FCVBRUSH                   0
							8905       FCVBRUSH                   0
							7469      FCVLEDGES                   0
							7472      FCVLEDGES                   0
							7545      FCVLEDGES                   0
							7546      FCVLEDGES                   0
							7723      FCVLEDGES  0.0158113883008419
							7797      FCVLEDGES                   0
							8152      FCVLEDGES                   0
							8905      FCVLEDGES                   0
							7469   FCVLIVETREES                   0
							7472   FCVLIVETREES                   0
							7545   FCVLIVETREES                   0
							7546   FCVLIVETREES                   0
							7723   FCVLIVETREES  0.0210818510677892
							7797   FCVLIVETREES                   0
							8152   FCVLIVETREES                   0
							8905   FCVLIVETREES                   0
							7469    FCVOVERHANG  0.0158113883008419
							7472    FCVOVERHANG  0.0968245836551854
							7545    FCVOVERHANG                   0
							7546    FCVOVERHANG                   0
							7723    FCVOVERHANG  0.0918936583472681
							7797    FCVOVERHANG  0.0187766904049703
							8152    FCVOVERHANG                   0
							8905    FCVOVERHANG                   0
							7469       FCVSNAGS                   0
							7472       FCVSNAGS                   0
#							7545       FCVSNAGS                  NA
							7546       FCVSNAGS                   0
							7723       FCVSNAGS                   0
							7797       FCVSNAGS  0.0660225291773525
							8152       FCVSNAGS                   0
							8905       FCVSNAGS                   0
							7469  FCVSTRUCTURES                   0
							7472  FCVSTRUCTURES  0.0176776695296637
							7545  FCVSTRUCTURES                   0
							7546  FCVSTRUCTURES                   0
							7723  FCVSTRUCTURES  0.0210818510677892
							7797  FCVSTRUCTURES  0.0138675049056307
							8152  FCVSTRUCTURES                   0
							8905  FCVSTRUCTURES   0.391311896062463
							7469     FCNAQUATIC                  10
							7472     FCNAQUATIC                  10
							7545     FCNAQUATIC                   1
							7546     FCNAQUATIC                   3
							7723     FCNAQUATIC                  10
							7797     FCNAQUATIC                  13
							8152     FCNAQUATIC                   3
							8905     FCNAQUATIC                   5
							7469    FCNBOULDERS                  10
							7472    FCNBOULDERS                   8
							7545    FCNBOULDERS                  10
							7546    FCNBOULDERS                   8
							7723    FCNBOULDERS                  10
							7797    FCNBOULDERS                  13
							8152    FCNBOULDERS                   3
							8905    FCNBOULDERS                   5
							7469       FCNBRUSH                  10
							7472       FCNBRUSH                   9
							7545       FCNBRUSH                   0
							7546       FCNBRUSH                   3
							7723       FCNBRUSH                  10
							7797       FCNBRUSH                  13
							8152       FCNBRUSH                   3
							8905       FCNBRUSH                   5
							7469      FCNLEDGES                  10
							7472      FCNLEDGES                   8
							7545      FCNLEDGES                  10
							7546      FCNLEDGES                   8
							7723      FCNLEDGES                  10
							7797      FCNLEDGES                  13
							8152      FCNLEDGES                   3
							8905      FCNLEDGES                   5
							7469   FCNLIVETREES                  10
							7472   FCNLIVETREES                   9
							7545   FCNLIVETREES                  10
							7546   FCNLIVETREES                   8
							7723   FCNLIVETREES                  10
							7797   FCNLIVETREES                  13
							8152   FCNLIVETREES                   3
							8905   FCNLIVETREES                   5
							7469    FCNOVERHANG                  10
							7472    FCNOVERHANG                   9
							7545    FCNOVERHANG                  10
							7546    FCNOVERHANG                   8
							7723    FCNOVERHANG                  10
							7797    FCNOVERHANG                  13
							8152    FCNOVERHANG                   3
							8905    FCNOVERHANG                   5
							7469       FCNSNAGS                  10
							7472       FCNSNAGS                   8
							7545       FCNSNAGS                   0
							7546       FCNSNAGS                   3
							7723       FCNSNAGS                  10
							7797       FCNSNAGS                  13
							8152       FCNSNAGS                   3
							8905       FCNSNAGS                   5
							7469  FCNSTRUCTURES                  10
							7472  FCNSTRUCTURES                   8
							7545  FCNSTRUCTURES                  10
							7546  FCNSTRUCTURES                   8
							7723  FCNSTRUCTURES                  10
							7797  FCNSTRUCTURES                  13
							8152  FCNSTRUCTURES                   3
							8905  FCNSTRUCTURES                   5
							7469        FCFPALL                   1
							7472        FCFPALL                   1
							7545        FCFPALL                 0.1
							7546        FCFPALL               0.375
							7723        FCFPALL                   1
							7797        FCFPALL                   1
							8152        FCFPALL                   1
							8905        FCFPALL                 0.8
							7469         FCNALL                  10
							7472         FCNALL                  10
							7545         FCNALL                  10
							7546         FCNALL                   8
							7723         FCNALL                  10
							7797         FCNALL                  13
							8152         FCNALL                   3
							8905         FCNALL                   5
                            hasNAvalues  FCFPSNAGS                1
                            hasNAvalues  FCFCSNAGS             0.05
                            hasNAvalues  FCVSNAGS                 0
                            hasNAvalues  FCIALL                0.05
                            hasNAvalues  FCINATURAL            0.05
                            hasNAvalues  FCIRIPVEG             0.05
                            hasNAvalues  FCNSNAGS                 6
                            hasNAvalues  FCNALL                   6
							hasNAvalues	 FCFPALL                  1
                            hasNAvalues  FCNAQUATIC               0
                            hasNAvalues  FCNBOULDERS              0
                            hasNAvalues  FCNBRUSH                 0
                            hasNAvalues  FCNLEDGES                0
                            hasNAvalues  FCNLIVETREES             0
                            hasNAvalues  FCNOVERHANG              0
                            hasNAvalues  FCNSTRUCTURES            0
							has2NAvalues FCFPBRUSH   		   1.00
							has2NAvalues FCFPSNAGS   		   1.00
							has2NAvalues FCFCBRUSH   		   0.05
							has2NAvalues FCFCSNAGS             0.05
							has2NAvalues FCIALL                0.10
							has2NAvalues FCINATURAL            0.10
							has2NAvalues FCIRIPVEG             0.10
							has2NAvalues FCVBRUSH                NA
							has2NAvalues FCVSNAGS              0.00
							has2NAvalues FCNAQUATIC               0
							has2NAvalues FCNBOULDERS              0
							has2NAvalues FCNBRUSH                 1
							has2NAvalues FCNLEDGES                0
							has2NAvalues FCNLIVETREES             0
							has2NAvalues FCNOVERHANG              0
							has2NAvalues FCNSNAGS                 5
							has2NAvalues FCNSTRUCTURES            0
							has2NAvalues FCFPALL               1.00
							has2NAvalues FCNALL                   6
							"
               			   )

	longMets <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
#	wideMets <- within(dcast(longMets, SITE~PARAMETER, value.var='VALUE')
#					  ,{FCNALL <- as.integer(FCNALL)
#						FCNAQUATIC <- as.integer(FCNAQUATIC)
#						FCNBOULDERS <- as.integer(FCNBOULDERS)
#						FCNBRUSH <- as.integer(FCNBRUSH)
#						FCNLEDGES <- as.integer(FCNLEDGES)
#						FCNLIVETREES <- as.integer(FCNLIVETREES)
#						FCNOVERHANG <- as.integer(FCNOVERHANG)
#						FCNSNAGS <- as.integer(FCNSNAGS)
#						FCNSTRUCTURES <- as.integer(FCNSTRUCTURES)
#					   }
#			   		  )
	
	return(longMets)
}



nlaFishCoverTest.createTestDataWithDrawDown <- function()
# This data is based on NLA2012 data for the following SITEs:
#	6160	Full complement of data stations A - J (as expected), and DRAWDOWN all YES
#	6189	Full complement of data stations A - J but no drawdown data, and DRAWDOWN all NO
#	6227	Incomplete drawdown data at C,D,E,F,G; else full complement, and DRAWDOWN never missing
#	6235	Incomplete data at stations all stations A-J; H is entirely absent, and DRAWDOWN all NO
#	6281	Full complement of data stations A - I; J is absent, and DRAWDOWN all YES
#	6449	Full complement of data stations A - J plus K and L stations, and DRAWDOWN all YES
#	6683	Data missing at all but station J, and DRAWDOWN all missing except YES at J
#	7263	Drawdown absent for Aquatic and snags at all stations but A, and DRAWDOWN never missing
#	7913	Data very incomplete (1-4 rows) at each station, and DRAWDOWN all NO
#	1000057	Data very incomplete (3-4 rows) at each station, and DRAWDOWN all NO
#
# NOTE: DRAWDOWN values appended at the end rather than interleaved.
#phab12 <- fetchNLATable('tblPHAB')
#fc12 <- subset(phab12, PARAMETER %in% coverClasses) 
#tt<-subset(fc12, SITE %in% c(6160,6189,6227,6235,6281,6449,6683,7263,7913,1000057))
#tt <- tt[order(tt$SITE,tt$STATION,tt$PARAMETER),]	
{
	tc <- textConnection("	SITE SAMPLE_TYPE STATION        PARAMETER VALUE FLAG  FORM_TYPE
							6160        PHAB       A       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       A    FC_AQUATIC_DD      2 NA   PHAB_FRONT
							6160        PHAB       A      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       A   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       A         FC_BRUSH      0 NA   PHAB_FRONT
							6160        PHAB       A      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       A        FC_LEDGES      3 NA   PHAB_FRONT
							6160        PHAB       A     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       A  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       A      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       A   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       A      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       A    FC_STRUCTURES      2 F1   PHAB_FRONT
							6160        PHAB       A FC_STRUCTURES_DD      2 F1   PHAB_FRONT
							6160        PHAB       A    HORIZ_DIST_DD    6.2 NA   PHAB_FRONT
							6160        PHAB       B       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       B    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       B      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       B   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       B         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       B      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       B     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       B  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       B      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       B   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6160        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       B      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       B FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       B    HORIZ_DIST_DD    5.0 NA   PHAB_FRONT
							6160        PHAB       C       FC_AQUATIC      2 NA   PHAB_FRONT
							6160        PHAB       C    FC_AQUATIC_DD      4 NA   PHAB_FRONT
							6160        PHAB       C      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       C   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       C      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       C        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       C     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       C  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       C      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       C   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       C      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       C    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       C FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       C    HORIZ_DIST_DD    4.9 NA   PHAB_FRONT
							6160        PHAB       D       FC_AQUATIC      2 NA   PHAB_FRONT
							6160        PHAB       D    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       D      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       D   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       D         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       D      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       D     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       D  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       D      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       D   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       D      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       D FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       D    HORIZ_DIST_DD    4.0 NA   PHAB_FRONT
							6160        PHAB       E       FC_AQUATIC      2 NA   PHAB_FRONT
							6160        PHAB       E    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       E      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       E   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       E         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       E      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       E        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       E     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       E  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       E      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       E   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       E      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       E FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       E    HORIZ_DIST_DD    4.0 NA   PHAB_FRONT
							6160        PHAB       F       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       F    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       F      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       F   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       F         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       F      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       F        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       F     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       F  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       F      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       F   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       F         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       F      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       F FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       F    HORIZ_DIST_DD    3.5 NA   PHAB_FRONT
							6160        PHAB       G       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       G    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       G      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       G   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       G         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       G      FC_BRUSH_DD      4 NA   PHAB_FRONT
							6160        PHAB       G        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       G     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       G  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       G      FC_OVERHANG      1 NA   PHAB_FRONT
							6160        PHAB       G   FC_OVERHANG_DD      2 NA   PHAB_FRONT
							6160        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       G      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       G FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       G    HORIZ_DIST_DD    7.0 NA   PHAB_FRONT
							6160        PHAB       H       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       H    FC_AQUATIC_DD      3 NA   PHAB_FRONT
							6160        PHAB       H      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       H   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       H         FC_BRUSH      0 NA   PHAB_FRONT
							6160        PHAB       H      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       H        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       H     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       H  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       H      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       H   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6160        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       H      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       H    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       H FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       H    HORIZ_DIST_DD    23. NA   PHAB_FRONT
							6160        PHAB       I       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       I    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6160        PHAB       I      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       I   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       I         FC_BRUSH      1 NA   PHAB_FRONT
							6160        PHAB       I      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6160        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       I     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       I     FC_LIVETREES      1 NA   PHAB_FRONT
							6160        PHAB       I  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       I      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       I   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       I      FC_SNAGS_DD      1 NA   PHAB_FRONT
							6160        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       I FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       I    HORIZ_DIST_DD    4.3 NA   PHAB_FRONT
							6160        PHAB       J       FC_AQUATIC      1 NA   PHAB_FRONT
							6160        PHAB       J    FC_AQUATIC_DD      3 NA   PHAB_FRONT
							6160        PHAB       J      FC_BOULDERS      0 NA   PHAB_FRONT
							6160        PHAB       J   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6160        PHAB       J         FC_BRUSH      0 NA   PHAB_FRONT
							6160        PHAB       J      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6160        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							6160        PHAB       J     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6160        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6160        PHAB       J  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6160        PHAB       J      FC_OVERHANG      0 NA   PHAB_FRONT
							6160        PHAB       J   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6160        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6160        PHAB       J      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6160        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6160        PHAB       J FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6160        PHAB       J    HORIZ_DIST_DD    4.5 NA   PHAB_FRONT
							6189        PHAB       A       FC_AQUATIC      3 NA   PHAB_FRONT
							6189        PHAB       A      FC_BOULDERS      1 NA   PHAB_FRONT
							6189        PHAB       A         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       A        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       A      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       B       FC_AQUATIC      3 NA   PHAB_FRONT
							6189        PHAB       B      FC_BOULDERS      0 NA   PHAB_FRONT
							6189        PHAB       B         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       B      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       C       FC_AQUATIC      2 NA   PHAB_FRONT
							6189        PHAB       C      FC_BOULDERS      3 NA   PHAB_FRONT
							6189        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       C        FC_LEDGES      1 NA   PHAB_FRONT
							6189        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       C      FC_OVERHANG      2 NA   PHAB_FRONT
							6189        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       C    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       D       FC_AQUATIC      2 NA   PHAB_FRONT
							6189        PHAB       D      FC_BOULDERS      2 NA   PHAB_FRONT
							6189        PHAB       D         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       D      FC_OVERHANG      2 NA   PHAB_FRONT
							6189        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       E       FC_AQUATIC      3 NA   PHAB_FRONT
							6189        PHAB       E      FC_BOULDERS      2 NA   PHAB_FRONT
							6189        PHAB       E         FC_BRUSH      0 NA   PHAB_FRONT
							6189        PHAB       E        FC_LEDGES      1 NA   PHAB_FRONT
							6189        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       E      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       F       FC_AQUATIC      3 NA   PHAB_FRONT
							6189        PHAB       F      FC_BOULDERS      1 NA   PHAB_FRONT
							6189        PHAB       F         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       F        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       F      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       F         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       G       FC_AQUATIC      2 NA   PHAB_FRONT
							6189        PHAB       G      FC_BOULDERS      2 NA   PHAB_FRONT
							6189        PHAB       G         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       G        FC_LEDGES      1 NA   PHAB_FRONT
							6189        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       G      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       H       FC_AQUATIC      3 NA   PHAB_FRONT
							6189        PHAB       H      FC_BOULDERS      0 NA   PHAB_FRONT
							6189        PHAB       H         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       H        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       H      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       H    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       I       FC_AQUATIC      2 NA   PHAB_FRONT
							6189        PHAB       I      FC_BOULDERS      0 NA   PHAB_FRONT
							6189        PHAB       I         FC_BRUSH      2 NA   PHAB_FRONT
							6189        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       I     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       I      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6189        PHAB       J       FC_AQUATIC      2 NA   PHAB_FRONT
							6189        PHAB       J      FC_BOULDERS      0 NA   PHAB_FRONT
							6189        PHAB       J         FC_BRUSH      1 NA   PHAB_FRONT
							6189        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							6189        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6189        PHAB       J      FC_OVERHANG      1 NA   PHAB_FRONT
							6189        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6189        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       A       FC_AQUATIC      0 NA   PHAB_FRONT
							6227        PHAB       A    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6227        PHAB       A      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       A   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6227        PHAB       A         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       A      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6227        PHAB       A        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       A     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6227        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       A  FC_LIVETREES_DD      1 NA   PHAB_FRONT
							6227        PHAB       A      FC_OVERHANG      0 NA   PHAB_FRONT
							6227        PHAB       A   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6227        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       A      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6227        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       A FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6227        PHAB       A    HORIZ_DIST_DD    2.4 NA   PHAB_FRONT
							6227        PHAB       B       FC_AQUATIC      1 NA   PHAB_FRONT
							6227        PHAB       B    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6227        PHAB       B      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       B   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6227        PHAB       B         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       B      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6227        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       B     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6227        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       B  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6227        PHAB       B      FC_OVERHANG      0 NA   PHAB_FRONT
							6227        PHAB       B   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6227        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       B      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6227        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       B FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6227        PHAB       B    HORIZ_DIST_DD    1.0 NA   PHAB_FRONT
							6227        PHAB       C       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       C      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							6227        PHAB       C        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       C      FC_OVERHANG      1 NA   PHAB_FRONT
							6227        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       C    FC_STRUCTURES      1 F2   PHAB_FRONT
							6227        PHAB       D       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       D      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       D         FC_BRUSH      1 NA   PHAB_FRONT
							6227        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       D      FC_OVERHANG      0 NA   PHAB_FRONT
							6227        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       E       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       E      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       E         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       E        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       E      FC_OVERHANG      2 NA   PHAB_FRONT
							6227        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       F       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       F      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       F         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       F        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       F      FC_OVERHANG      2 NA   PHAB_FRONT
							6227        PHAB       F         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       G       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       G      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       G         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       G        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       G      FC_OVERHANG      2 NA   PHAB_FRONT
							6227        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       H       FC_AQUATIC      0 NA   PHAB_FRONT
							6227        PHAB       H    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6227        PHAB       H      FC_BOULDERS      3 NA   PHAB_FRONT
							6227        PHAB       H   FC_BOULDERS_DD      3 NA   PHAB_FRONT
							6227        PHAB       H         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       H      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6227        PHAB       H        FC_LEDGES      1 NA   PHAB_FRONT
							6227        PHAB       H     FC_LEDGES_DD      1 NA   PHAB_FRONT
							6227        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       H  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6227        PHAB       H      FC_OVERHANG      0 NA   PHAB_FRONT
							6227        PHAB       H   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6227        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       H      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6227        PHAB       H    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       H FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6227        PHAB       H    HORIZ_DIST_DD    0.8 NA   PHAB_FRONT
							6227        PHAB       I       FC_AQUATIC      1 NA   PHAB_FRONT
							6227        PHAB       I    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6227        PHAB       I      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       I   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6227        PHAB       I         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       I      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6227        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       I     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6227        PHAB       I     FC_LIVETREES      2 NA   PHAB_FRONT
							6227        PHAB       I  FC_LIVETREES_DD      2 NA   PHAB_FRONT
							6227        PHAB       I      FC_OVERHANG      2 NA   PHAB_FRONT
							6227        PHAB       I   FC_OVERHANG_DD      2 NA   PHAB_FRONT
							6227        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       I      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6227        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       I FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6227        PHAB       I    HORIZ_DIST_DD    2.0 NA   PHAB_FRONT
							6227        PHAB       J       FC_AQUATIC      3 NA   PHAB_FRONT
							6227        PHAB       J    FC_AQUATIC_DD      2 NA   PHAB_FRONT
							6227        PHAB       J      FC_BOULDERS      0 NA   PHAB_FRONT
							6227        PHAB       J   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6227        PHAB       J         FC_BRUSH      0 NA   PHAB_FRONT
							6227        PHAB       J      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6227        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							6227        PHAB       J     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6227        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6227        PHAB       J  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6227        PHAB       J      FC_OVERHANG      2 NA   PHAB_FRONT
							6227        PHAB       J   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6227        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6227        PHAB       J      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6227        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6227        PHAB       J FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6227        PHAB       J    HORIZ_DIST_DD    2.9 NA   PHAB_FRONT
							6235        PHAB       A       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       A         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       A        FC_LEDGES      4 NA   PHAB_FRONT
							6235        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       A      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       B       FC_AQUATIC      0 NA   PHAB_FRONT
							6235        PHAB       B         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       B        FC_LEDGES      4 NA   PHAB_FRONT
							6235        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       B      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       C       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       C        FC_LEDGES      3 NA   PHAB_FRONT
							6235        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       C      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       C    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       D       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       D         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       D        FC_LEDGES      3 NA   PHAB_FRONT
							6235        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       D      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       D         FC_SNAGS      1 NA   PHAB_FRONT
							6235        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       E       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       E         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       E        FC_LEDGES      4 NA   PHAB_FRONT
							6235        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       E      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       F       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       F         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       F        FC_LEDGES      2 NA   PHAB_FRONT
							6235        PHAB       F     FC_LIVETREES      1 NA   PHAB_FRONT
							6235        PHAB       F      FC_OVERHANG      2 NA   PHAB_FRONT
							6235        PHAB       F         FC_SNAGS      1 NA   PHAB_FRONT
							6235        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       G       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       G         FC_BRUSH      2 NA   PHAB_FRONT
							6235        PHAB       G        FC_LEDGES      2 NA   PHAB_FRONT
							6235        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       G      FC_OVERHANG      3 NA   PHAB_FRONT
							6235        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       I       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       I         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       I        FC_LEDGES      3 NA   PHAB_FRONT
							6235        PHAB       I     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       I      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6235        PHAB       J       FC_AQUATIC      1 NA   PHAB_FRONT
							6235        PHAB       J         FC_BRUSH      1 NA   PHAB_FRONT
							6235        PHAB       J        FC_LEDGES      3 NA   PHAB_FRONT
							6235        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6235        PHAB       J      FC_OVERHANG      1 NA   PHAB_FRONT
							6235        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6235        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       A       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       A    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       A      FC_BOULDERS      0 NA   PHAB_FRONT
							6281        PHAB       A   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       A         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       A      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       A        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       A     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       A  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       A      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       A   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       A      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       A FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       A    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6281        PHAB       B       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       B    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       B      FC_BOULDERS      1 NA   PHAB_FRONT
							6281        PHAB       B   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       B         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       B      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       B     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       B  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       B      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       B   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       B      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       B FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       B    HORIZ_DIST_DD    10. NA   PHAB_FRONT
							6281        PHAB       C       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       C    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       C      FC_BOULDERS      1 NA   PHAB_FRONT
							6281        PHAB       C   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       C         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       C      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       C        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       C     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       C  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       C      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       C   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       C      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       C    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       C FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       C    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6281        PHAB       D       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       D    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       D      FC_BOULDERS      0 NA   PHAB_FRONT
							6281        PHAB       D   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       D         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       D      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       D     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       D  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       D      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       D   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       D      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       D FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       D    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6281        PHAB       E       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       E    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       E      FC_BOULDERS      1 NA   PHAB_FRONT
							6281        PHAB       E   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       E         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       E      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       E        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       E     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       E  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       E      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       E   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       E      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       E FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       E    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6281        PHAB       F       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       F    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       F      FC_BOULDERS      0 NA   PHAB_FRONT
							6281        PHAB       F   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       F         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       F      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       F        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       F     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       F  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       F      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       F   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       F         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       F      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       F FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       F    HORIZ_DIST_DD   15.0 NA   PHAB_FRONT
							6281        PHAB       G       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       G    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       G      FC_BOULDERS      0 NA   PHAB_FRONT
							6281        PHAB       G   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       G         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       G      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       G        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       G     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       G  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       G      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       G   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       G      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       G FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       G    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6281        PHAB       H       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       H    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       H      FC_BOULDERS      0 NA   PHAB_FRONT
							6281        PHAB       H   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       H         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       H      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       H        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       H     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       H  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       H      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       H   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       H      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       H    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       H FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       H    HORIZ_DIST_DD    8.0 NA   PHAB_FRONT
							6281        PHAB       I       FC_AQUATIC      0 NA   PHAB_FRONT
							6281        PHAB       I    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6281        PHAB       I      FC_BOULDERS      1 NA   PHAB_FRONT
							6281        PHAB       I   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6281        PHAB       I         FC_BRUSH      0 NA   PHAB_FRONT
							6281        PHAB       I      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6281        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							6281        PHAB       I     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6281        PHAB       I     FC_LIVETREES      0 NA   PHAB_FRONT
							6281        PHAB       I  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6281        PHAB       I      FC_OVERHANG      0 NA   PHAB_FRONT
							6281        PHAB       I   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6281        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6281        PHAB       I      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6281        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6281        PHAB       I FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6281        PHAB       I    HORIZ_DIST_DD    6.0 NA   PHAB_FRONT
							6281        PHAB       J    HORIZ_DIST_DD   10.0 NA   PHAB_FRONT
							6449        PHAB       A       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       A    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       A      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       A   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6449        PHAB       A         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       A      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       A        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       A     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       A  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       A      FC_OVERHANG      1 NA   PHAB_FRONT
							6449        PHAB       A   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       A      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							6449        PHAB       A FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6449        PHAB       A    HORIZ_DIST_DD    0.9 NA   PHAB_FRONT
							6449        PHAB       B       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       B    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       B      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       B   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6449        PHAB       B         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       B      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       B     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       B  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       B      FC_OVERHANG      1 NA   PHAB_FRONT
							6449        PHAB       B   FC_OVERHANG_DD      2 NA   PHAB_FRONT
							6449        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       B      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							6449        PHAB       B FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6449        PHAB       B    HORIZ_DIST_DD    1.1 NA   PHAB_FRONT
							6449        PHAB       C       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       C    FC_AQUATIC_DD      2 NA   PHAB_FRONT
							6449        PHAB       C      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       C   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6449        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       C      FC_BRUSH_DD      2 NA   PHAB_FRONT
							6449        PHAB       C        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       C     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       C     FC_LIVETREES      1 NA   PHAB_FRONT
							6449        PHAB       C  FC_LIVETREES_DD      2 NA   PHAB_FRONT
							6449        PHAB       C      FC_OVERHANG      2 NA   PHAB_FRONT
							6449        PHAB       C   FC_OVERHANG_DD      3 NA   PHAB_FRONT
							6449        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       C      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       C    FC_STRUCTURES      0 NA   PHAB_FRONT
							6449        PHAB       C FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6449        PHAB       C    HORIZ_DIST_DD    0.2 NA   PHAB_FRONT
							6449        PHAB       D       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       D    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6449        PHAB       D      FC_BOULDERS      1 NA   PHAB_FRONT
							6449        PHAB       D   FC_BOULDERS_DD      1 NA   PHAB_FRONT
							6449        PHAB       D         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       D      FC_BRUSH_DD      2 NA   PHAB_FRONT
							6449        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       D     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       D  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       D      FC_OVERHANG      1 NA   PHAB_FRONT
							6449        PHAB       D   FC_OVERHANG_DD      2 NA   PHAB_FRONT
							6449        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       D      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       D    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       D FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       D    HORIZ_DIST_DD    0.6 NA   PHAB_FRONT
							6449        PHAB       E       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       E    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6449        PHAB       E      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       E   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6449        PHAB       E         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       E      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       E        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       E     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       E  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       E      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       E   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       E      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       E    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       E FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       E    HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
							6449        PHAB       F       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       F    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6449        PHAB       F      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       F   FC_BOULDERS_DD      1 NA   PHAB_FRONT
							6449        PHAB       F         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       F      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       F        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       F     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       F  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       F      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       F   FC_OVERHANG_DD      1 NA   PHAB_FRONT
							6449        PHAB       F         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       F      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       F    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       F FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       F    HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
							6449        PHAB       G       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       G    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       G      FC_BOULDERS      1 NA   PHAB_FRONT
							6449        PHAB       G   FC_BOULDERS_DD      3 NA   PHAB_FRONT
							6449        PHAB       G         FC_BRUSH      0 NA   PHAB_FRONT
							6449        PHAB       G      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6449        PHAB       G        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       G     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       G  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       G      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       G      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       G    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       G FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       G    HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
							6449        PHAB       H       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       H    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       H      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       H   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6449        PHAB       H         FC_BRUSH      0 NA   PHAB_FRONT
							6449        PHAB       H      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6449        PHAB       H        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       H     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       H  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       H      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       H   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       H      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       H    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       H FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       H    HORIZ_DIST_DD    0.2 NA   PHAB_FRONT
							6449        PHAB       I       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       I    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6449        PHAB       I      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       I   FC_BOULDERS_DD      1 NA   PHAB_FRONT
							6449        PHAB       I         FC_BRUSH      1 NA   PHAB_FRONT
							6449        PHAB       I      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       I     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       I     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       I  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       I      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       I   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       I      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							6449        PHAB       I FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6449        PHAB       I    HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
							6449        PHAB       J       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       J    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       J      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       J   FC_BOULDERS_DD      2 NA   PHAB_FRONT
							6449        PHAB       J         FC_BRUSH      0 NA   PHAB_FRONT
							6449        PHAB       J      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       J     FC_LEDGES_DD      1 NA   PHAB_FRONT
							6449        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       J  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       J      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       J   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       J      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6449        PHAB       J FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       J    HORIZ_DIST_DD    0.5 NA   PHAB_FRONT
							6449        PHAB       K       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       K    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							6449        PHAB       K      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       K   FC_BOULDERS_DD      1 NA   PHAB_FRONT
							6449        PHAB       K         FC_BRUSH      0 NA   PHAB_FRONT
							6449        PHAB       K      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6449        PHAB       K        FC_LEDGES      1 NA   PHAB_FRONT
							6449        PHAB       K     FC_LEDGES_DD      1 NA   PHAB_FRONT
							6449        PHAB       K     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       K  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       K      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       K   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       K         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       K      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       K    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       K FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       K    HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
							6449        PHAB       L       FC_AQUATIC      1 NA   PHAB_FRONT
							6449        PHAB       L    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6449        PHAB       L      FC_BOULDERS      0 NA   PHAB_FRONT
							6449        PHAB       L   FC_BOULDERS_DD      1 NA   PHAB_FRONT
							6449        PHAB       L         FC_BRUSH      0 NA   PHAB_FRONT
							6449        PHAB       L      FC_BRUSH_DD      1 NA   PHAB_FRONT
							6449        PHAB       L        FC_LEDGES      0 NA   PHAB_FRONT
							6449        PHAB       L     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6449        PHAB       L     FC_LIVETREES      0 NA   PHAB_FRONT
							6449        PHAB       L  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6449        PHAB       L      FC_OVERHANG      0 NA   PHAB_FRONT
							6449        PHAB       L   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6449        PHAB       L         FC_SNAGS      0 NA   PHAB_FRONT
							6449        PHAB       L      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6449        PHAB       L    FC_STRUCTURES      1 NA   PHAB_FRONT
							6449        PHAB       L FC_STRUCTURES_DD      1 NA   PHAB_FRONT
							6449        PHAB       L    HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
							6683        PHAB       J       FC_AQUATIC      4 NA   PHAB_FRONT
							6683        PHAB       J    FC_AQUATIC_DD      0 NA   PHAB_FRONT
							6683        PHAB       J      FC_BOULDERS      0 NA   PHAB_FRONT
							6683        PHAB       J   FC_BOULDERS_DD      0 NA   PHAB_FRONT
							6683        PHAB       J         FC_BRUSH      0 NA   PHAB_FRONT
							6683        PHAB       J      FC_BRUSH_DD      0 NA   PHAB_FRONT
							6683        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							6683        PHAB       J     FC_LEDGES_DD      0 NA   PHAB_FRONT
							6683        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							6683        PHAB       J  FC_LIVETREES_DD      0 NA   PHAB_FRONT
							6683        PHAB       J      FC_OVERHANG      0 NA   PHAB_FRONT
							6683        PHAB       J   FC_OVERHANG_DD      0 NA   PHAB_FRONT
							6683        PHAB       J         FC_SNAGS      0 NA   PHAB_FRONT
							6683        PHAB       J      FC_SNAGS_DD      0 NA   PHAB_FRONT
							6683        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							6683        PHAB       J FC_STRUCTURES_DD      0 NA   PHAB_FRONT
							6683        PHAB       J    HORIZ_DIST_DD   20.0 NA   PHAB_FRONT
							7263        PHAB       A       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       A    FC_AQUATIC_DD      1 NA   PHAB_FRONT
							7263        PHAB       A      FC_BOULDERS      0 NA   PHAB_FRONT
							7263        PHAB       A         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       A        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       A     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       A      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       A         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       A      FC_SNAGS_DD      0 NA   PHAB_FRONT
							7263        PHAB       A    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       B       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       B      FC_BOULDERS      1 NA   PHAB_FRONT
							7263        PHAB       B         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       B        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       B     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       B      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       B         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       B    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       C       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       C      FC_BOULDERS      2 NA   PHAB_FRONT
							7263        PHAB       C         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       C        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       C     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       C      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       C         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       C    FC_STRUCTURES      1 F2   PHAB_FRONT
							7263        PHAB       D       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       D      FC_BOULDERS      0 NA   PHAB_FRONT
							7263        PHAB       D         FC_BRUSH      0 NA   PHAB_FRONT
							7263        PHAB       D        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       D     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       D      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       D         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       D    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       E       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       E      FC_BOULDERS      0 NA   PHAB_FRONT
							7263        PHAB       E         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       E        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       E     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       E      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       E         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       E    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       F       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       F      FC_BOULDERS      1 NA   PHAB_FRONT
							7263        PHAB       F         FC_BRUSH      0 NA   PHAB_FRONT
							7263        PHAB       F        FC_LEDGES      1 NA   PHAB_FRONT
							7263        PHAB       F     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       F      FC_OVERHANG      0 NA   PHAB_FRONT
							7263        PHAB       F         FC_SNAGS      1 NA   PHAB_FRONT
							7263        PHAB       F    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       G       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       G      FC_BOULDERS      2 NA   PHAB_FRONT
							7263        PHAB       G         FC_BRUSH      0 NA   PHAB_FRONT
							7263        PHAB       G        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       G     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       G      FC_OVERHANG      0 NA   PHAB_FRONT
							7263        PHAB       G         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       G    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       H       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       H      FC_BOULDERS      0 NA   PHAB_FRONT
							7263        PHAB       H         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       H        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       H     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       H      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       H         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       H    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       I       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       I      FC_BOULDERS      1 NA   PHAB_FRONT
							7263        PHAB       I         FC_BRUSH      0 NA   PHAB_FRONT
							7263        PHAB       I        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       I     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       I      FC_OVERHANG      0 NA   PHAB_FRONT
							7263        PHAB       I         FC_SNAGS      0 NA   PHAB_FRONT
							7263        PHAB       I    FC_STRUCTURES      0 NA   PHAB_FRONT
							7263        PHAB       J       FC_AQUATIC      1 NA   PHAB_FRONT
							7263        PHAB       J      FC_BOULDERS      1 NA   PHAB_FRONT
							7263        PHAB       J         FC_BRUSH      1 NA   PHAB_FRONT
							7263        PHAB       J        FC_LEDGES      0 NA   PHAB_FRONT
							7263        PHAB       J     FC_LIVETREES      0 NA   PHAB_FRONT
							7263        PHAB       J      FC_OVERHANG      1 NA   PHAB_FRONT
							7263        PHAB       J         FC_SNAGS      1 NA   PHAB_FRONT
							7263        PHAB       J    FC_STRUCTURES      0 NA   PHAB_FRONT
							7913        PHAB       A       FC_AQUATIC      4 ''       eForms
							7913        PHAB       B       FC_AQUATIC      2 ''       eForms
							7913        PHAB       B      FC_BOULDERS      1 ''       eForms
							7913        PHAB       C       FC_AQUATIC      1 ''       eForms
							7913        PHAB       C      FC_BOULDERS      1 ''       eForms
							7913        PHAB       C         FC_BRUSH      1 ''       eForms
							7913        PHAB       C         FC_SNAGS      0 ''       eForms
							7913        PHAB       D       FC_AQUATIC      1 ''       eForms
							7913        PHAB       D         FC_BRUSH      1 ''       eForms
							7913        PHAB       D      FC_OVERHANG      1 ''       eForms
							7913        PHAB       D         FC_SNAGS      0 ''       eForms
							7913        PHAB       E       FC_AQUATIC      4 ''       eForms
							7913        PHAB       E      FC_OVERHANG      1 ''       eForms
							7913        PHAB       F       FC_AQUATIC      4 ''       eForms
							7913        PHAB       F      FC_OVERHANG      1 ''       eForms
							7913        PHAB       G       FC_AQUATIC      4 ''       eForms
							7913        PHAB       G      FC_OVERHANG      1 ''       eForms
							7913        PHAB       H       FC_AQUATIC      4 ''       eForms
							7913        PHAB       H      FC_OVERHANG      1 ''       eForms
							7913        PHAB       I       FC_AQUATIC      4 ''       eForms
							7913        PHAB       I      FC_OVERHANG      1 ''       eForms
							7913        PHAB       J       FC_AQUATIC      4 ''       eForms
							7913        PHAB       J      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       A      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       A         FC_BRUSH      1 ''       eForms
							1000057     PHAB       A     FC_LIVETREES      0 ''       eForms
							1000057     PHAB       A      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       B       FC_AQUATIC      2 ''       eForms
							1000057     PHAB       B      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       B         FC_BRUSH      1 ''       eForms
							1000057     PHAB       B      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       C       FC_AQUATIC      4 ''       eForms
							1000057     PHAB       C         FC_BRUSH      1 ''       eForms
							1000057     PHAB       C      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       C         FC_SNAGS      1 ''       eForms
							1000057     PHAB       D       FC_AQUATIC      3 ''       eForms
							1000057     PHAB       D         FC_BRUSH      1 ''       eForms
							1000057     PHAB       D      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       D         FC_SNAGS      1 ''       eForms
							1000057     PHAB       E       FC_AQUATIC      1 ''       eForms
							1000057     PHAB       E      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       E         FC_BRUSH      1 ''       eForms
							1000057     PHAB       E      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       F       FC_AQUATIC      1 ''       eForms
							1000057     PHAB       F      FC_BOULDERS      2 ''       eForms
							1000057     PHAB       F         FC_BRUSH      1 ''       eForms
							1000057     PHAB       F      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       G      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       G         FC_BRUSH      1 ''       eForms
							1000057     PHAB       G        FC_LEDGES      2 ''       eForms
							1000057     PHAB       G      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       H       FC_AQUATIC      1 ''       eForms
							1000057     PHAB       H      FC_BOULDERS      2 ''       eForms
							1000057     PHAB       H         FC_BRUSH      1 ''       eForms
							1000057     PHAB       H      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       I      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       I         FC_BRUSH      1 ''       eForms
							1000057     PHAB       I      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       J      FC_BOULDERS      1 ''       eForms
							1000057     PHAB       J         FC_BRUSH      1 ''       eForms
							1000057     PHAB       J      FC_OVERHANG      1 ''       eForms
							1000057     PHAB       J         FC_SNAGS      0 ''       eForms
							6160        PHAB       A  		 DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       B  		 DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       C         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       D         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       E         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       F         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       G         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       H         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       I         DRAWDOWN    YES NA   PHAB_FRONT
							6160        PHAB       J         DRAWDOWN    YES NA   PHAB_FRONT
							6189        PHAB       A         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       B         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       C         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       D         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       E         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       F         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       G         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       H         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       I         DRAWDOWN     NO NA   PHAB_FRONT
							6189        PHAB       J         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       A         DRAWDOWN    YES NA   PHAB_FRONT
							6227        PHAB       B         DRAWDOWN    YES NA   PHAB_FRONT
							6227        PHAB       C         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       D         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       E         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       F         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       G         DRAWDOWN     NO NA   PHAB_FRONT
							6227        PHAB       H         DRAWDOWN    YES NA   PHAB_FRONT
							6227        PHAB       I         DRAWDOWN    YES NA   PHAB_FRONT
							6227        PHAB       J         DRAWDOWN    YES NA   PHAB_FRONT
							6235        PHAB       A         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       B         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       C         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       D         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       E         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       F         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       G         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       H         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       I         DRAWDOWN     NO NA   PHAB_FRONT
							6235        PHAB       J         DRAWDOWN     NO NA   PHAB_FRONT
							6281        PHAB       A         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       B         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       C         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       D         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       E         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       F         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       G         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       H         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       I         DRAWDOWN    YES NA   PHAB_FRONT
							6281        PHAB       J         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       A         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       B         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       C         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       D         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       E         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       F         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       G         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       H         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       I         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       J         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       K         DRAWDOWN    YES NA   PHAB_FRONT
							6449        PHAB       L         DRAWDOWN    YES NA   PHAB_FRONT
							6683        PHAB       J         DRAWDOWN    YES NA   PHAB_FRONT
							7263        PHAB       A         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       B         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       C         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       D         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       E         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       F         DRAWDOWN    YES NA   PHAB_FRONT
							7263        PHAB       G         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       H         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       I         DRAWDOWN     NO NA   PHAB_FRONT
							7263        PHAB       J         DRAWDOWN     NO NA   PHAB_FRONT
							7913        PHAB       A         DRAWDOWN     NO ''       eForms
							7913        PHAB       B         DRAWDOWN     NO ''       eForms
							7913        PHAB       C         DRAWDOWN     NO ''       eForms
							7913        PHAB       D         DRAWDOWN     NO ''       eForms
							7913        PHAB       E         DRAWDOWN     NO ''       eForms
							7913        PHAB       F         DRAWDOWN     NO ''       eForms
							7913        PHAB       G         DRAWDOWN     NO ''       eForms
							7913        PHAB       H         DRAWDOWN     NO ''       eForms
							7913        PHAB       I         DRAWDOWN     NO ''       eForms
							7913        PHAB       J         DRAWDOWN     NO ''       eForms
							1000057     PHAB       A         DRAWDOWN     NO ''       eForms
							1000057     PHAB       B         DRAWDOWN     NO ''       eForms
							1000057     PHAB       C         DRAWDOWN     NO ''       eForms
							1000057     PHAB       D         DRAWDOWN     NO ''       eForms
							1000057     PHAB       E         DRAWDOWN     NO ''       eForms
							1000057     PHAB       F         DRAWDOWN     NO ''       eForms
							1000057     PHAB       G         DRAWDOWN     NO ''       eForms
							1000057     PHAB       H         DRAWDOWN     NO ''       eForms
							1000057     PHAB       I         DRAWDOWN     NO ''       eForms
							1000057     PHAB       J         DRAWDOWN     NO ''       eForms
						 ")
		 
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	within(fake
		  ,{SAMPLE_TYPE <- 'PHAB'
			FLAG <- as.character(NA)
			FORM_TYPE <- 'PHAB_FRONT'
		   }
          )
	return(fake)
}



nlaFishCoverTest.expectedResultsWithDrawDown <- function()
# Expected results for test data with drawdown values using 10 m maximum riparian drawdown
{
	tc <- textConnection("	SITE              METRIC                       VALUE
							6160      FCFPAQUATIC_DD  1.000000000000000000000000
							6227      FCFPAQUATIC_DD  0.400000000000000022204460
							6281      FCFPAQUATIC_DD  0.000000000000000000000000
							6449      FCFPAQUATIC_DD  0.583333333333333370340767
							6683      FCFPAQUATIC_DD  0.000000000000000000000000
							7263      FCFPAQUATIC_DD  1.000000000000000000000000
							6160     FCFPBOULDERS_DD  0.000000000000000000000000
							6227     FCFPBOULDERS_DD  0.200000000000000011102230
							6281     FCFPBOULDERS_DD  0.000000000000000000000000
							6449     FCFPBOULDERS_DD  0.583333333333333370340767
							6683     FCFPBOULDERS_DD  0.000000000000000000000000
							6160        FCFPBRUSH_DD  0.900000000000000022204460
							6227        FCFPBRUSH_DD  0.400000000000000022204460
							6281        FCFPBRUSH_DD  0.000000000000000000000000
							6449        FCFPBRUSH_DD  0.750000000000000000000000
							6683        FCFPBRUSH_DD  0.000000000000000000000000
							6160       FCFPLEDGES_DD  0.000000000000000000000000
							6227       FCFPLEDGES_DD  0.200000000000000011102230
							6281       FCFPLEDGES_DD  0.000000000000000000000000
							6449       FCFPLEDGES_DD  0.166666666666666657414808
							6683       FCFPLEDGES_DD  0.000000000000000000000000
							6160    FCFPLIVETREES_DD  0.000000000000000000000000
							6227    FCFPLIVETREES_DD  0.400000000000000022204460
							6281    FCFPLIVETREES_DD  0.000000000000000000000000
							6449    FCFPLIVETREES_DD  0.083333333333333328707404
							6683    FCFPLIVETREES_DD  0.000000000000000000000000
							6160     FCFPOVERHANG_DD  0.800000000000000044408921
							6227     FCFPOVERHANG_DD  0.599999999999999977795540
							6281     FCFPOVERHANG_DD  0.000000000000000000000000
							6449     FCFPOVERHANG_DD  0.363636363636363646456573
							6683     FCFPOVERHANG_DD  0.000000000000000000000000
							6160        FCFPSNAGS_DD  0.100000000000000005551115
							6227        FCFPSNAGS_DD  0.000000000000000000000000
							6281        FCFPSNAGS_DD  0.000000000000000000000000
							6449        FCFPSNAGS_DD  0.000000000000000000000000
							6683        FCFPSNAGS_DD  0.000000000000000000000000
							7263        FCFPSNAGS_DD  0.000000000000000000000000
							6160   FCFPSTRUCTURES_DD  0.100000000000000005551115
							6227   FCFPSTRUCTURES_DD  0.000000000000000000000000
							6281   FCFPSTRUCTURES_DD  0.000000000000000000000000
							6449   FCFPSTRUCTURES_DD  0.666666666666666629659233
							6683   FCFPSTRUCTURES_DD  0.000000000000000000000000
							6160     FCFPAQUATIC_LIT  1.000000000000000000000000
							6189     FCFPAQUATIC_LIT  1.000000000000000000000000
							6227     FCFPAQUATIC_LIT  0.800000000000000044408921
							6235     FCFPAQUATIC_LIT  0.888888888888888839545643
							6281     FCFPAQUATIC_LIT  0.000000000000000000000000
							6449     FCFPAQUATIC_LIT  1.000000000000000000000000
							6683     FCFPAQUATIC_LIT  1.000000000000000000000000
							7263     FCFPAQUATIC_LIT  1.000000000000000000000000
							7913     FCFPAQUATIC_LIT  1.000000000000000000000000
						 1000057     FCFPAQUATIC_LIT  1.000000000000000000000000
							6160    FCFPBOULDERS_LIT  0.000000000000000000000000
							6189    FCFPBOULDERS_LIT  0.599999999999999977795540
							6227    FCFPBOULDERS_LIT  0.100000000000000005551115
							6281    FCFPBOULDERS_LIT  0.444444444444444419772822
							6449    FCFPBOULDERS_LIT  0.166666666666666657414808
							6683    FCFPBOULDERS_LIT  0.000000000000000000000000
							7263    FCFPBOULDERS_LIT  0.599999999999999977795540
							7913    FCFPBOULDERS_LIT  1.000000000000000000000000
						 1000057    FCFPBOULDERS_LIT  1.000000000000000000000000
							6160       FCFPBRUSH_LIT  0.699999999999999955591079
							6189       FCFPBRUSH_LIT  0.900000000000000022204460
							6227       FCFPBRUSH_LIT  0.200000000000000011102230
							6235       FCFPBRUSH_LIT  1.000000000000000000000000
							6281       FCFPBRUSH_LIT  0.000000000000000000000000
							6449       FCFPBRUSH_LIT  0.583333333333333370340767
							6683       FCFPBRUSH_LIT  0.000000000000000000000000
							7263       FCFPBRUSH_LIT  0.599999999999999977795540
							7913       FCFPBRUSH_LIT  1.000000000000000000000000
						 1000057       FCFPBRUSH_LIT  1.000000000000000000000000
							6160      FCFPLEDGES_LIT  0.100000000000000005551115
							6189      FCFPLEDGES_LIT  0.299999999999999988897770
							6227      FCFPLEDGES_LIT  0.100000000000000005551115
							6235      FCFPLEDGES_LIT  1.000000000000000000000000
							6281      FCFPLEDGES_LIT  0.000000000000000000000000
							6449      FCFPLEDGES_LIT  0.083333333333333328707404
							6683      FCFPLEDGES_LIT  0.000000000000000000000000
							7263      FCFPLEDGES_LIT  0.100000000000000005551115
					 	 1000057      FCFPLEDGES_LIT  1.000000000000000000000000
							6160   FCFPLIVETREES_LIT  0.100000000000000005551115
							6189   FCFPLIVETREES_LIT  0.000000000000000000000000
							6227   FCFPLIVETREES_LIT  0.100000000000000005551115
							6235   FCFPLIVETREES_LIT  0.111111111111111104943205
							6281   FCFPLIVETREES_LIT  0.000000000000000000000000
							6449   FCFPLIVETREES_LIT  0.083333333333333328707404
							6683   FCFPLIVETREES_LIT  0.000000000000000000000000
							7263   FCFPLIVETREES_LIT  0.000000000000000000000000
						 1000057   FCFPLIVETREES_LIT  0.000000000000000000000000
							6160    FCFPOVERHANG_LIT  0.100000000000000005551115
							6189    FCFPOVERHANG_LIT  1.000000000000000000000000
							6227    FCFPOVERHANG_LIT  0.599999999999999977795540
							6235    FCFPOVERHANG_LIT  1.000000000000000000000000
							6281    FCFPOVERHANG_LIT  0.000000000000000000000000
							6449    FCFPOVERHANG_LIT  0.333333333333333314829616
							6683    FCFPOVERHANG_LIT  0.000000000000000000000000
							7263    FCFPOVERHANG_LIT  0.699999999999999955591079
							7913    FCFPOVERHANG_LIT  1.000000000000000000000000
						 1000057    FCFPOVERHANG_LIT  1.000000000000000000000000
							6160       FCFPSNAGS_LIT  0.000000000000000000000000
							6189       FCFPSNAGS_LIT  0.000000000000000000000000
							6227       FCFPSNAGS_LIT  0.000000000000000000000000
							6235       FCFPSNAGS_LIT  0.222222222222222209886411
							6281       FCFPSNAGS_LIT  0.000000000000000000000000
							6449       FCFPSNAGS_LIT  0.000000000000000000000000
							6683       FCFPSNAGS_LIT  0.000000000000000000000000
							7263       FCFPSNAGS_LIT  0.200000000000000011102230
							7913       FCFPSNAGS_LIT  0.000000000000000000000000
						 1000057       FCFPSNAGS_LIT  0.666666666666666629659233
							6160  FCFPSTRUCTURES_LIT  0.100000000000000005551115
							6189  FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6227  FCFPSTRUCTURES_LIT  0.100000000000000005551115
							6235  FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6281  FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6449  FCFPSTRUCTURES_LIT  0.583333333333333370340767
							6683  FCFPSTRUCTURES_LIT  0.000000000000000000000000
							7263  FCFPSTRUCTURES_LIT  0.100000000000000005551115
							6160     FCFPAQUATIC_SIM  1.000000000000000000000000
							6189     FCFPAQUATIC_SIM  1.000000000000000000000000
							6227     FCFPAQUATIC_SIM  0.900000000000000022204460
							6235     FCFPAQUATIC_SIM  0.888888888888888839545643
							6281     FCFPAQUATIC_SIM  0.000000000000000000000000
							6449     FCFPAQUATIC_SIM  1.000000000000000000000000
							6683     FCFPAQUATIC_SIM  0.000000000000000000000000
							7263     FCFPAQUATIC_SIM  1.000000000000000000000000
							7913     FCFPAQUATIC_SIM  1.000000000000000000000000
						 1000057     FCFPAQUATIC_SIM  1.000000000000000000000000
							6160    FCFPBOULDERS_SIM  0.000000000000000000000000
							6189    FCFPBOULDERS_SIM  0.599999999999999977795540
							6227    FCFPBOULDERS_SIM  0.100000000000000005551115
							6281    FCFPBOULDERS_SIM  0.111111111111111104943205
							6449    FCFPBOULDERS_SIM  0.583333333333333370340767
							6683    FCFPBOULDERS_SIM  0.000000000000000000000000
							7263    FCFPBOULDERS_SIM  0.599999999999999977795540
							7913    FCFPBOULDERS_SIM  1.000000000000000000000000
						 1000057    FCFPBOULDERS_SIM  1.000000000000000000000000
							6160       FCFPBRUSH_SIM  0.900000000000000022204460
							6189       FCFPBRUSH_SIM  0.900000000000000022204460
							6227       FCFPBRUSH_SIM  0.400000000000000022204460
							6235       FCFPBRUSH_SIM  1.000000000000000000000000
							6281       FCFPBRUSH_SIM  0.000000000000000000000000
							6449       FCFPBRUSH_SIM  0.750000000000000000000000
							6683       FCFPBRUSH_SIM  0.000000000000000000000000
							7263       FCFPBRUSH_SIM  0.599999999999999977795540
							7913       FCFPBRUSH_SIM  1.000000000000000000000000
						 1000057       FCFPBRUSH_SIM  1.000000000000000000000000
							6160      FCFPLEDGES_SIM  0.100000000000000005551115
							6189      FCFPLEDGES_SIM  0.299999999999999988897770
							6227      FCFPLEDGES_SIM  0.100000000000000005551115
							6235      FCFPLEDGES_SIM  1.000000000000000000000000
							6281      FCFPLEDGES_SIM  0.000000000000000000000000
							6449      FCFPLEDGES_SIM  0.166666666666666657414808
							6683      FCFPLEDGES_SIM  0.000000000000000000000000
							7263      FCFPLEDGES_SIM  0.100000000000000005551115
						 1000057      FCFPLEDGES_SIM  1.000000000000000000000000
							6160   FCFPLIVETREES_SIM  0.100000000000000005551115
							6189   FCFPLIVETREES_SIM  0.000000000000000000000000
							6227   FCFPLIVETREES_SIM  0.200000000000000011102230
							6235   FCFPLIVETREES_SIM  0.111111111111111104943205
							6281   FCFPLIVETREES_SIM  0.000000000000000000000000
							6449   FCFPLIVETREES_SIM  0.083333333333333328707404
							6683   FCFPLIVETREES_SIM  0.000000000000000000000000
							7263   FCFPLIVETREES_SIM  0.000000000000000000000000
						 1000057   FCFPLIVETREES_SIM  0.000000000000000000000000
							6160    FCFPOVERHANG_SIM  0.800000000000000044408921
							6189    FCFPOVERHANG_SIM  1.000000000000000000000000
							6227    FCFPOVERHANG_SIM  0.699999999999999955591079
							6235    FCFPOVERHANG_SIM  1.000000000000000000000000
							6281    FCFPOVERHANG_SIM  0.000000000000000000000000
							6449    FCFPOVERHANG_SIM  0.454545454545454530315141
							6683    FCFPOVERHANG_SIM  0.000000000000000000000000
							7263    FCFPOVERHANG_SIM  0.699999999999999955591079
							7913    FCFPOVERHANG_SIM  1.000000000000000000000000
							1000057    FCFPOVERHANG_SIM  1.000000000000000000000000
							6160       FCFPSNAGS_SIM  0.100000000000000005551115
							6189       FCFPSNAGS_SIM  0.000000000000000000000000
							6227       FCFPSNAGS_SIM  0.000000000000000000000000
							6235       FCFPSNAGS_SIM  0.222222222222222209886411
							6281       FCFPSNAGS_SIM  0.000000000000000000000000
							6449       FCFPSNAGS_SIM  0.000000000000000000000000
							6683       FCFPSNAGS_SIM  0.000000000000000000000000
							7263       FCFPSNAGS_SIM  0.200000000000000011102230
							7913       FCFPSNAGS_SIM  0.000000000000000000000000
							1000057       FCFPSNAGS_SIM  0.666666666666666629659233
							6160  FCFPSTRUCTURES_SIM  0.100000000000000005551115
							6189  FCFPSTRUCTURES_SIM  0.000000000000000000000000
							6227  FCFPSTRUCTURES_SIM  0.100000000000000005551115
							6235  FCFPSTRUCTURES_SIM  0.000000000000000000000000
							6281  FCFPSTRUCTURES_SIM  0.000000000000000000000000
							6449  FCFPSTRUCTURES_SIM  0.666666666666666629659233
							6683  FCFPSTRUCTURES_SIM  0.000000000000000000000000
							7263  FCFPSTRUCTURES_SIM  0.100000000000000005551115
#							6160    FCFPAQUATIC_SYN0  1.000000000000000000000000
#							6189    FCFPAQUATIC_SYN0                         NaN
#							6227    FCFPAQUATIC_SYN0  0.800000000000000044408921
#							6235    FCFPAQUATIC_SYN0                         NaN
#							6281    FCFPAQUATIC_SYN0  0.000000000000000000000000
#							6449    FCFPAQUATIC_SYN0  1.000000000000000000000000
#							6683    FCFPAQUATIC_SYN0  0.000000000000000000000000
#							7263    FCFPAQUATIC_SYN0  1.000000000000000000000000
#							7913    FCFPAQUATIC_SYN0                         NaN
#							1000057    FCFPAQUATIC_SYN0                         NaN
#							6160   FCFPBOULDERS_SYN0  0.000000000000000000000000
#							6189   FCFPBOULDERS_SYN0                         NaN
#							6227   FCFPBOULDERS_SYN0  0.200000000000000011102230
#							6281   FCFPBOULDERS_SYN0  0.111111111111111104943205
#							6449   FCFPBOULDERS_SYN0  0.583333333333333370340767
#							6683   FCFPBOULDERS_SYN0  0.000000000000000000000000
#							7263   FCFPBOULDERS_SYN0                         NaN
#							7913   FCFPBOULDERS_SYN0                         NaN
#							1000057   FCFPBOULDERS_SYN0                         NaN
#							6160      FCFPBRUSH_SYN0  0.900000000000000022204460
#							6189      FCFPBRUSH_SYN0                         NaN
#							6227      FCFPBRUSH_SYN0  0.400000000000000022204460
#							6235      FCFPBRUSH_SYN0                         NaN
#							6281      FCFPBRUSH_SYN0  0.000000000000000000000000
#							6449      FCFPBRUSH_SYN0  0.750000000000000000000000
#							6683      FCFPBRUSH_SYN0  0.000000000000000000000000
#							7263      FCFPBRUSH_SYN0                         NaN
#							7913      FCFPBRUSH_SYN0                         NaN
#							1000057      FCFPBRUSH_SYN0                         NaN
#							6160     FCFPLEDGES_SYN0  0.100000000000000005551115
#							6189     FCFPLEDGES_SYN0                         NaN
#							6227     FCFPLEDGES_SYN0  0.200000000000000011102230
#							6235     FCFPLEDGES_SYN0                         NaN
#							6281     FCFPLEDGES_SYN0  0.000000000000000000000000
#							6449     FCFPLEDGES_SYN0  0.166666666666666657414808
#							6683     FCFPLEDGES_SYN0  0.000000000000000000000000
#							7263     FCFPLEDGES_SYN0                         NaN
#							1000057     FCFPLEDGES_SYN0                         NaN
#							6160  FCFPLIVETREES_SYN0  0.100000000000000005551115
#							6189  FCFPLIVETREES_SYN0                         NaN
#							6227  FCFPLIVETREES_SYN0  0.400000000000000022204460
#							6235  FCFPLIVETREES_SYN0                         NaN
#							6281  FCFPLIVETREES_SYN0  0.000000000000000000000000
#							6449  FCFPLIVETREES_SYN0  0.083333333333333328707404
#							6683  FCFPLIVETREES_SYN0  0.000000000000000000000000
#							7263  FCFPLIVETREES_SYN0                         NaN
#							1000057  FCFPLIVETREES_SYN0                         NaN
#							6160   FCFPOVERHANG_SYN0  0.800000000000000044408921
#							6189   FCFPOVERHANG_SYN0                         NaN
#							6227   FCFPOVERHANG_SYN0  0.599999999999999977795540
#							6235   FCFPOVERHANG_SYN0                         NaN
#							6281   FCFPOVERHANG_SYN0  0.000000000000000000000000
#							6449   FCFPOVERHANG_SYN0  0.454545454545454530315141
#							6683   FCFPOVERHANG_SYN0  0.000000000000000000000000
#							7263   FCFPOVERHANG_SYN0                         NaN
#							7913   FCFPOVERHANG_SYN0                         NaN
#							1000057   FCFPOVERHANG_SYN0                         NaN
#							6160      FCFPSNAGS_SYN0  0.100000000000000005551115
#							6189      FCFPSNAGS_SYN0                         NaN
#							6227      FCFPSNAGS_SYN0  0.000000000000000000000000
#							6235      FCFPSNAGS_SYN0                         NaN
#							6281      FCFPSNAGS_SYN0  0.000000000000000000000000
#							6449      FCFPSNAGS_SYN0  0.000000000000000000000000
#							6683      FCFPSNAGS_SYN0  0.000000000000000000000000
#							7263      FCFPSNAGS_SYN0  0.000000000000000000000000
#							7913      FCFPSNAGS_SYN0                         NaN
#							1000057      FCFPSNAGS_SYN0                         NaN
#							6160 FCFPSTRUCTURES_SYN0  0.100000000000000005551115
#							6189 FCFPSTRUCTURES_SYN0                         NaN
#							6227 FCFPSTRUCTURES_SYN0  0.000000000000000000000000
#							6235 FCFPSTRUCTURES_SYN0                         NaN
#							6281 FCFPSTRUCTURES_SYN0  0.000000000000000000000000
#							6449 FCFPSTRUCTURES_SYN0  0.666666666666666629659233
#							6683 FCFPSTRUCTURES_SYN0  0.000000000000000000000000
#							7263 FCFPSTRUCTURES_SYN0                         NaN
							6160       FCNAQUATIC_DD 10.000000000000000000000000
							6160      FCNAQUATIC_LIT 10.000000000000000000000000
							6160      FCNAQUATIC_SIM 10.000000000000000000000000
#							6160     FCNAQUATIC_SYN0 10.000000000000000000000000
							6160      FCNBOULDERS_DD 10.000000000000000000000000
							6160     FCNBOULDERS_LIT 10.000000000000000000000000
							6160     FCNBOULDERS_SIM 10.000000000000000000000000
#							6160    FCNBOULDERS_SYN0 10.000000000000000000000000
							6160         FCNBRUSH_DD 10.000000000000000000000000
							6160        FCNBRUSH_LIT 10.000000000000000000000000
							6160        FCNBRUSH_SIM 10.000000000000000000000000
#							6160       FCNBRUSH_SYN0 10.000000000000000000000000
							6160        FCNLEDGES_DD 10.000000000000000000000000
							6160       FCNLEDGES_LIT 10.000000000000000000000000
							6160       FCNLEDGES_SIM 10.000000000000000000000000
#							6160      FCNLEDGES_SYN0 10.000000000000000000000000
							6160     FCNLIVETREES_DD 10.000000000000000000000000
							6160    FCNLIVETREES_LIT 10.000000000000000000000000
							6160    FCNLIVETREES_SIM 10.000000000000000000000000
#							6160   FCNLIVETREES_SYN0 10.000000000000000000000000
							6160      FCNOVERHANG_DD 10.000000000000000000000000
							6160     FCNOVERHANG_LIT 10.000000000000000000000000
							6160     FCNOVERHANG_SIM 10.000000000000000000000000
#							6160    FCNOVERHANG_SYN0 10.000000000000000000000000
							6160         FCNSNAGS_DD 10.000000000000000000000000
							6160        FCNSNAGS_LIT 10.000000000000000000000000
							6160        FCNSNAGS_SIM 10.000000000000000000000000
#							6160       FCNSNAGS_SYN0 10.000000000000000000000000
							6160    FCNSTRUCTURES_DD 10.000000000000000000000000
							6160   FCNSTRUCTURES_LIT 10.000000000000000000000000
							6160   FCNSTRUCTURES_SIM 10.000000000000000000000000
#							6160  FCNSTRUCTURES_SYN0 10.000000000000000000000000
							6189       FCNAQUATIC_DD  0.000000000000000000000000
							6189      FCNAQUATIC_LIT 10.000000000000000000000000
							6189      FCNAQUATIC_SIM 10.000000000000000000000000
#							6189     FCNAQUATIC_SYN0  0.000000000000000000000000
							6189      FCNBOULDERS_DD  0.000000000000000000000000
							6189     FCNBOULDERS_LIT 10.000000000000000000000000
							6189     FCNBOULDERS_SIM 10.000000000000000000000000
#							6189    FCNBOULDERS_SYN0  0.000000000000000000000000
							6189         FCNBRUSH_DD  0.000000000000000000000000
							6189        FCNBRUSH_LIT 10.000000000000000000000000
							6189        FCNBRUSH_SIM 10.000000000000000000000000
#							6189       FCNBRUSH_SYN0  0.000000000000000000000000
							6189        FCNLEDGES_DD  0.000000000000000000000000
							6189       FCNLEDGES_LIT 10.000000000000000000000000
							6189       FCNLEDGES_SIM 10.000000000000000000000000
#							6189      FCNLEDGES_SYN0  0.000000000000000000000000
							6189     FCNLIVETREES_DD  0.000000000000000000000000
							6189    FCNLIVETREES_LIT 10.000000000000000000000000
							6189    FCNLIVETREES_SIM 10.000000000000000000000000
#							6189   FCNLIVETREES_SYN0  0.000000000000000000000000
							6189      FCNOVERHANG_DD  0.000000000000000000000000
							6189     FCNOVERHANG_LIT 10.000000000000000000000000
							6189     FCNOVERHANG_SIM 10.000000000000000000000000
#							6189    FCNOVERHANG_SYN0  0.000000000000000000000000
							6189         FCNSNAGS_DD  0.000000000000000000000000
							6189        FCNSNAGS_LIT 10.000000000000000000000000
							6189        FCNSNAGS_SIM 10.000000000000000000000000
#							6189       FCNSNAGS_SYN0  0.000000000000000000000000
							6189    FCNSTRUCTURES_DD  0.000000000000000000000000
							6189   FCNSTRUCTURES_LIT 10.000000000000000000000000
							6189   FCNSTRUCTURES_SIM 10.000000000000000000000000
#							6189  FCNSTRUCTURES_SYN0  0.000000000000000000000000
							6227       FCNAQUATIC_DD  5.000000000000000000000000
							6227      FCNAQUATIC_LIT 10.000000000000000000000000
							6227      FCNAQUATIC_SIM 10.000000000000000000000000
#							6227     FCNAQUATIC_SYN0  5.000000000000000000000000
							6227      FCNBOULDERS_DD  5.000000000000000000000000
							6227     FCNBOULDERS_LIT 10.000000000000000000000000
							6227     FCNBOULDERS_SIM 10.000000000000000000000000
#							6227    FCNBOULDERS_SYN0  5.000000000000000000000000
							6227         FCNBRUSH_DD  5.000000000000000000000000
							6227        FCNBRUSH_LIT 10.000000000000000000000000
							6227        FCNBRUSH_SIM 10.000000000000000000000000
#							6227       FCNBRUSH_SYN0  5.000000000000000000000000
							6227        FCNLEDGES_DD  5.000000000000000000000000
							6227       FCNLEDGES_LIT 10.000000000000000000000000
							6227       FCNLEDGES_SIM 10.000000000000000000000000
#							6227      FCNLEDGES_SYN0  5.000000000000000000000000
							6227     FCNLIVETREES_DD  5.000000000000000000000000
							6227    FCNLIVETREES_LIT 10.000000000000000000000000
							6227    FCNLIVETREES_SIM 10.000000000000000000000000
#							6227   FCNLIVETREES_SYN0  5.000000000000000000000000
							6227      FCNOVERHANG_DD  5.000000000000000000000000
							6227     FCNOVERHANG_LIT 10.000000000000000000000000
							6227     FCNOVERHANG_SIM 10.000000000000000000000000
#							6227    FCNOVERHANG_SYN0  5.000000000000000000000000
							6227         FCNSNAGS_DD  5.000000000000000000000000
							6227        FCNSNAGS_LIT 10.000000000000000000000000
							6227        FCNSNAGS_SIM 10.000000000000000000000000
#							6227       FCNSNAGS_SYN0  5.000000000000000000000000
							6227    FCNSTRUCTURES_DD  5.000000000000000000000000
							6227   FCNSTRUCTURES_LIT 10.000000000000000000000000
							6227   FCNSTRUCTURES_SIM 10.000000000000000000000000
#							6227  FCNSTRUCTURES_SYN0  5.000000000000000000000000
							6235       FCNAQUATIC_DD  0.000000000000000000000000
							6235      FCNAQUATIC_LIT  9.000000000000000000000000
							6235      FCNAQUATIC_SIM  9.000000000000000000000000
#							6235     FCNAQUATIC_SYN0  0.000000000000000000000000
							6235      FCNBOULDERS_DD  0.000000000000000000000000
							6235     FCNBOULDERS_LIT  0.000000000000000000000000
							6235     FCNBOULDERS_SIM  0.000000000000000000000000
#							6235    FCNBOULDERS_SYN0  0.000000000000000000000000
							6235         FCNBRUSH_DD  0.000000000000000000000000
							6235        FCNBRUSH_LIT  9.000000000000000000000000
							6235        FCNBRUSH_SIM  9.000000000000000000000000
#							6235       FCNBRUSH_SYN0  0.000000000000000000000000
							6235        FCNLEDGES_DD  0.000000000000000000000000
							6235       FCNLEDGES_LIT  9.000000000000000000000000
							6235       FCNLEDGES_SIM  9.000000000000000000000000
#							6235      FCNLEDGES_SYN0  0.000000000000000000000000
							6235     FCNLIVETREES_DD  0.000000000000000000000000
							6235    FCNLIVETREES_LIT  9.000000000000000000000000
							6235    FCNLIVETREES_SIM  9.000000000000000000000000
#							6235   FCNLIVETREES_SYN0  0.000000000000000000000000
							6235      FCNOVERHANG_DD  0.000000000000000000000000
							6235     FCNOVERHANG_LIT  9.000000000000000000000000
							6235     FCNOVERHANG_SIM  9.000000000000000000000000
#							6235    FCNOVERHANG_SYN0  0.000000000000000000000000
							6235         FCNSNAGS_DD  0.000000000000000000000000
							6235        FCNSNAGS_LIT  9.000000000000000000000000
							6235        FCNSNAGS_SIM  9.000000000000000000000000
#							6235       FCNSNAGS_SYN0  0.000000000000000000000000
							6235    FCNSTRUCTURES_DD  0.000000000000000000000000
							6235   FCNSTRUCTURES_LIT  9.000000000000000000000000
							6235   FCNSTRUCTURES_SIM  9.000000000000000000000000
#							6235  FCNSTRUCTURES_SYN0  0.000000000000000000000000
							6281       FCNAQUATIC_DD  9.000000000000000000000000
							6281      FCNAQUATIC_LIT  9.000000000000000000000000
							6281      FCNAQUATIC_SIM  9.000000000000000000000000
#							6281     FCNAQUATIC_SYN0  9.000000000000000000000000
							6281      FCNBOULDERS_DD  9.000000000000000000000000
							6281     FCNBOULDERS_LIT  9.000000000000000000000000
							6281     FCNBOULDERS_SIM  9.000000000000000000000000
#							6281    FCNBOULDERS_SYN0  9.000000000000000000000000
							6281         FCNBRUSH_DD  9.000000000000000000000000
							6281        FCNBRUSH_LIT  9.000000000000000000000000
							6281        FCNBRUSH_SIM  9.000000000000000000000000
#							6281       FCNBRUSH_SYN0  9.000000000000000000000000
							6281        FCNLEDGES_DD  9.000000000000000000000000
							6281       FCNLEDGES_LIT  9.000000000000000000000000
							6281       FCNLEDGES_SIM  9.000000000000000000000000
#							6281      FCNLEDGES_SYN0  9.000000000000000000000000
							6281     FCNLIVETREES_DD  9.000000000000000000000000
							6281    FCNLIVETREES_LIT  9.000000000000000000000000
							6281    FCNLIVETREES_SIM  9.000000000000000000000000
#							6281   FCNLIVETREES_SYN0  9.000000000000000000000000
							6281      FCNOVERHANG_DD  9.000000000000000000000000
							6281     FCNOVERHANG_LIT  9.000000000000000000000000
							6281     FCNOVERHANG_SIM  9.000000000000000000000000
#							6281    FCNOVERHANG_SYN0  9.000000000000000000000000
							6281         FCNSNAGS_DD  9.000000000000000000000000
							6281        FCNSNAGS_LIT  9.000000000000000000000000
							6281        FCNSNAGS_SIM  9.000000000000000000000000
#							6281       FCNSNAGS_SYN0  9.000000000000000000000000
							6281    FCNSTRUCTURES_DD  9.000000000000000000000000
							6281   FCNSTRUCTURES_LIT  9.000000000000000000000000
							6281   FCNSTRUCTURES_SIM  9.000000000000000000000000
#							6281  FCNSTRUCTURES_SYN0  9.000000000000000000000000
							6449       FCNAQUATIC_DD 12.000000000000000000000000
							6449      FCNAQUATIC_LIT 12.000000000000000000000000
							6449      FCNAQUATIC_SIM 12.000000000000000000000000
#							6449     FCNAQUATIC_SYN0 12.000000000000000000000000
							6449      FCNBOULDERS_DD 12.000000000000000000000000
							6449     FCNBOULDERS_LIT 12.000000000000000000000000
							6449     FCNBOULDERS_SIM 12.000000000000000000000000
#							6449    FCNBOULDERS_SYN0 12.000000000000000000000000
							6449         FCNBRUSH_DD 12.000000000000000000000000
							6449        FCNBRUSH_LIT 12.000000000000000000000000
							6449        FCNBRUSH_SIM 12.000000000000000000000000
#							6449       FCNBRUSH_SYN0 12.000000000000000000000000
							6449        FCNLEDGES_DD 12.000000000000000000000000
							6449       FCNLEDGES_LIT 12.000000000000000000000000
							6449       FCNLEDGES_SIM 12.000000000000000000000000
#							6449      FCNLEDGES_SYN0 12.000000000000000000000000
							6449     FCNLIVETREES_DD 12.000000000000000000000000
							6449    FCNLIVETREES_LIT 12.000000000000000000000000
							6449    FCNLIVETREES_SIM 12.000000000000000000000000
#							6449   FCNLIVETREES_SYN0 12.000000000000000000000000
							6449      FCNOVERHANG_DD 11.000000000000000000000000
							6449     FCNOVERHANG_LIT 12.000000000000000000000000
							6449     FCNOVERHANG_SIM 11.000000000000000000000000
#							6449    FCNOVERHANG_SYN0 11.000000000000000000000000
							6449         FCNSNAGS_DD 12.000000000000000000000000
							6449        FCNSNAGS_LIT 12.000000000000000000000000
							6449        FCNSNAGS_SIM 12.000000000000000000000000
#							6449       FCNSNAGS_SYN0 12.000000000000000000000000
							6449    FCNSTRUCTURES_DD 12.000000000000000000000000
							6449   FCNSTRUCTURES_LIT 12.000000000000000000000000
							6449   FCNSTRUCTURES_SIM 12.000000000000000000000000
#							6449  FCNSTRUCTURES_SYN0 12.000000000000000000000000
							6683       FCNAQUATIC_DD  1.000000000000000000000000
							6683      FCNAQUATIC_LIT  1.000000000000000000000000
							6683      FCNAQUATIC_SIM  1.000000000000000000000000
#							6683     FCNAQUATIC_SYN0  1.000000000000000000000000
							6683      FCNBOULDERS_DD  1.000000000000000000000000
							6683     FCNBOULDERS_LIT  1.000000000000000000000000
							6683     FCNBOULDERS_SIM  1.000000000000000000000000
#							6683    FCNBOULDERS_SYN0  1.000000000000000000000000
							6683         FCNBRUSH_DD  1.000000000000000000000000
							6683        FCNBRUSH_LIT  1.000000000000000000000000
							6683        FCNBRUSH_SIM  1.000000000000000000000000
#							6683       FCNBRUSH_SYN0  1.000000000000000000000000
							6683        FCNLEDGES_DD  1.000000000000000000000000
							6683       FCNLEDGES_LIT  1.000000000000000000000000
							6683       FCNLEDGES_SIM  1.000000000000000000000000
#							6683      FCNLEDGES_SYN0  1.000000000000000000000000
							6683     FCNLIVETREES_DD  1.000000000000000000000000
							6683    FCNLIVETREES_LIT  1.000000000000000000000000
							6683    FCNLIVETREES_SIM  1.000000000000000000000000
#							6683   FCNLIVETREES_SYN0  1.000000000000000000000000
							6683      FCNOVERHANG_DD  1.000000000000000000000000
							6683     FCNOVERHANG_LIT  1.000000000000000000000000
							6683     FCNOVERHANG_SIM  1.000000000000000000000000
#							6683    FCNOVERHANG_SYN0  1.000000000000000000000000
							6683         FCNSNAGS_DD  1.000000000000000000000000
							6683        FCNSNAGS_LIT  1.000000000000000000000000
							6683        FCNSNAGS_SIM  1.000000000000000000000000
#							6683       FCNSNAGS_SYN0  1.000000000000000000000000
							6683    FCNSTRUCTURES_DD  1.000000000000000000000000
							6683   FCNSTRUCTURES_LIT  1.000000000000000000000000
							6683   FCNSTRUCTURES_SIM  1.000000000000000000000000
#							6683  FCNSTRUCTURES_SYN0  1.000000000000000000000000
							7263       FCNAQUATIC_DD  1.000000000000000000000000
							7263      FCNAQUATIC_LIT 10.000000000000000000000000
							7263      FCNAQUATIC_SIM 10.000000000000000000000000
#							7263     FCNAQUATIC_SYN0  1.000000000000000000000000
							7263      FCNBOULDERS_DD  0.000000000000000000000000
							7263     FCNBOULDERS_LIT 10.000000000000000000000000
							7263     FCNBOULDERS_SIM 10.000000000000000000000000
#							7263    FCNBOULDERS_SYN0  0.000000000000000000000000
							7263         FCNBRUSH_DD  0.000000000000000000000000
							7263        FCNBRUSH_LIT 10.000000000000000000000000
							7263        FCNBRUSH_SIM 10.000000000000000000000000
#							7263       FCNBRUSH_SYN0  0.000000000000000000000000
							7263        FCNLEDGES_DD  0.000000000000000000000000
							7263       FCNLEDGES_LIT 10.000000000000000000000000
							7263       FCNLEDGES_SIM 10.000000000000000000000000
#							7263      FCNLEDGES_SYN0  0.000000000000000000000000
							7263     FCNLIVETREES_DD  0.000000000000000000000000
							7263    FCNLIVETREES_LIT 10.000000000000000000000000
							7263    FCNLIVETREES_SIM 10.000000000000000000000000
#							7263   FCNLIVETREES_SYN0  0.000000000000000000000000
							7263      FCNOVERHANG_DD  0.000000000000000000000000
							7263     FCNOVERHANG_LIT 10.000000000000000000000000
							7263     FCNOVERHANG_SIM 10.000000000000000000000000
#							7263    FCNOVERHANG_SYN0  0.000000000000000000000000
							7263         FCNSNAGS_DD  1.000000000000000000000000
							7263        FCNSNAGS_LIT 10.000000000000000000000000
							7263        FCNSNAGS_SIM 10.000000000000000000000000
#							7263       FCNSNAGS_SYN0  1.000000000000000000000000
							7263    FCNSTRUCTURES_DD  0.000000000000000000000000
							7263   FCNSTRUCTURES_LIT 10.000000000000000000000000
							7263   FCNSTRUCTURES_SIM 10.000000000000000000000000
#							7263  FCNSTRUCTURES_SYN0  0.000000000000000000000000
							7913       FCNAQUATIC_DD  0.000000000000000000000000
							7913      FCNAQUATIC_LIT 10.000000000000000000000000
							7913      FCNAQUATIC_SIM 10.000000000000000000000000
#							7913     FCNAQUATIC_SYN0  0.000000000000000000000000
							7913      FCNBOULDERS_DD  0.000000000000000000000000
							7913     FCNBOULDERS_LIT  2.000000000000000000000000
							7913     FCNBOULDERS_SIM  2.000000000000000000000000
#							7913    FCNBOULDERS_SYN0  0.000000000000000000000000
							7913         FCNBRUSH_DD  0.000000000000000000000000
							7913        FCNBRUSH_LIT  2.000000000000000000000000
							7913        FCNBRUSH_SIM  2.000000000000000000000000
#							7913       FCNBRUSH_SYN0  0.000000000000000000000000
							7913        FCNLEDGES_DD  0.000000000000000000000000
							7913       FCNLEDGES_LIT  0.000000000000000000000000
							7913       FCNLEDGES_SIM  0.000000000000000000000000
#							7913      FCNLEDGES_SYN0  0.000000000000000000000000
							7913     FCNLIVETREES_DD  0.000000000000000000000000
							7913    FCNLIVETREES_LIT  0.000000000000000000000000
							7913    FCNLIVETREES_SIM  0.000000000000000000000000
#							7913   FCNLIVETREES_SYN0  0.000000000000000000000000
							7913      FCNOVERHANG_DD  0.000000000000000000000000
							7913     FCNOVERHANG_LIT  7.000000000000000000000000
							7913     FCNOVERHANG_SIM  7.000000000000000000000000
#							7913    FCNOVERHANG_SYN0  0.000000000000000000000000
							7913         FCNSNAGS_DD  0.000000000000000000000000
							7913        FCNSNAGS_LIT  2.000000000000000000000000
							7913        FCNSNAGS_SIM  2.000000000000000000000000
#							7913       FCNSNAGS_SYN0  0.000000000000000000000000
							7913    FCNSTRUCTURES_DD  0.000000000000000000000000
							7913   FCNSTRUCTURES_LIT  0.000000000000000000000000
							7913   FCNSTRUCTURES_SIM  0.000000000000000000000000
#							7913  FCNSTRUCTURES_SYN0  0.000000000000000000000000
							1000057       FCNAQUATIC_DD  0.000000000000000000000000
							1000057      FCNAQUATIC_LIT  6.000000000000000000000000
							1000057      FCNAQUATIC_SIM  6.000000000000000000000000
#							1000057     FCNAQUATIC_SYN0  0.000000000000000000000000
							1000057      FCNBOULDERS_DD  0.000000000000000000000000
							1000057     FCNBOULDERS_LIT  8.000000000000000000000000
							1000057     FCNBOULDERS_SIM  8.000000000000000000000000
#							1000057    FCNBOULDERS_SYN0  0.000000000000000000000000
							1000057         FCNBRUSH_DD  0.000000000000000000000000
							1000057        FCNBRUSH_LIT 10.000000000000000000000000
							1000057        FCNBRUSH_SIM 10.000000000000000000000000
#							1000057       FCNBRUSH_SYN0  0.000000000000000000000000
							1000057        FCNLEDGES_DD  0.000000000000000000000000
							1000057       FCNLEDGES_LIT  1.000000000000000000000000
							1000057       FCNLEDGES_SIM  1.000000000000000000000000
#							1000057      FCNLEDGES_SYN0  0.000000000000000000000000
							1000057     FCNLIVETREES_DD  0.000000000000000000000000
							1000057    FCNLIVETREES_LIT  1.000000000000000000000000
							1000057    FCNLIVETREES_SIM  1.000000000000000000000000
#							1000057   FCNLIVETREES_SYN0  0.000000000000000000000000
							1000057      FCNOVERHANG_DD  0.000000000000000000000000
							1000057     FCNOVERHANG_LIT 10.000000000000000000000000
							1000057     FCNOVERHANG_SIM 10.000000000000000000000000
#							1000057    FCNOVERHANG_SYN0  0.000000000000000000000000
							1000057         FCNSNAGS_DD  0.000000000000000000000000
							1000057        FCNSNAGS_LIT  3.000000000000000000000000
							1000057        FCNSNAGS_SIM  3.000000000000000000000000
#							1000057       FCNSNAGS_SYN0  0.000000000000000000000000
							1000057    FCNSTRUCTURES_DD  0.000000000000000000000000
							1000057   FCNSTRUCTURES_LIT  0.000000000000000000000000
							1000057   FCNSTRUCTURES_SIM  0.000000000000000000000000
#							1000057  FCNSTRUCTURES_SYN0  0.000000000000000000000000
							6160       FCVAQUATIC_DD  0.305743628987860782686425
							6227       FCVAQUATIC_DD  0.108397416943393998245426
							6281       FCVAQUATIC_DD  0.000000000000000000000000
							6449       FCVAQUATIC_DD  0.068947718445122457842089
							6683       FCVAQUATIC_DD                          NA
							7263       FCVAQUATIC_DD                          NA
							6160      FCVBOULDERS_DD  0.000000000000000000000000
							6227      FCVBOULDERS_DD  0.257147817412475776510661
							6281      FCVBOULDERS_DD  0.000000000000000000000000
							6449      FCVBOULDERS_DD  0.167690981127534555206182
							6683      FCVBOULDERS_DD                          NA
							6160         FCVBRUSH_DD  0.263114360434140215350141
							6227         FCVBRUSH_DD  0.027386127875258306496598
							6281         FCVBRUSH_DD  0.000000000000000000000000
							6449         FCVBRUSH_DD  0.086493124617281605392982
							6683         FCVBRUSH_DD                          NA
							6160        FCVLEDGES_DD  0.000000000000000000000000
							6227        FCVLEDGES_DD  0.022360679774997897079070
							6281        FCVLEDGES_DD  0.000000000000000000000000
							6449        FCVLEDGES_DD  0.019462473604038073998757
							6683        FCVLEDGES_DD                          NA
							6160     FCVLIVETREES_DD  0.000000000000000000000000
							6227     FCVLIVETREES_DD  0.108397416943393998245426
							6281     FCVLIVETREES_DD  0.000000000000000000000000
							6449     FCVLIVETREES_DD  0.072168783648703216382359
							6683     FCVLIVETREES_DD                          NA
							6160      FCVOVERHANG_DD  0.069920589878010100393091
							6227      FCVOVERHANG_DD  0.103682206766638598804953
							6281      FCVOVERHANG_DD  0.000000000000000000000000
							6449      FCVOVERHANG_DD  0.185220998220553212387784
							6683      FCVOVERHANG_DD                          NA
							6160         FCVSNAGS_DD  0.015811388300841895671045
							6227         FCVSNAGS_DD  0.000000000000000000000000
							6281         FCVSNAGS_DD  0.000000000000000000000000
							6449         FCVSNAGS_DD  0.000000000000000000000000
							6683         FCVSNAGS_DD                          NA
							7263         FCVSNAGS_DD                          NA
							6160    FCVSTRUCTURES_DD  0.079056941504209485294119
							6227    FCVSTRUCTURES_DD  0.000000000000000000000000
							6281    FCVSTRUCTURES_DD  0.000000000000000000000000
							6449    FCVSTRUCTURES_DD  0.024618298195866548938593
							6683    FCVSTRUCTURES_DD                          NA
							6160      FCVAQUATIC_LIT  0.096609178307929588491731
							6189      FCVAQUATIC_LIT  0.171290039925787201946505
							6227      FCVAQUATIC_LIT  0.284507371511608542213878
							6235      FCVAQUATIC_LIT  0.016666666666666669904817
							6281      FCVAQUATIC_LIT  0.000000000000000000000000
							6449      FCVAQUATIC_LIT  0.000000000000000000000000
							6683      FCVAQUATIC_LIT                          NA
							7263      FCVAQUATIC_LIT  0.000000000000000000000000
							7913      FCVAQUATIC_LIT  0.370332058203625302805762
							1000057      FCVAQUATIC_LIT  0.345205252953466323884157
							6160     FCVBOULDERS_LIT  0.000000000000000000000000
							6189     FCVBOULDERS_LIT  0.188580221656461100021573
							6227     FCVBOULDERS_LIT  0.181830965459681803686465
							6281     FCVBOULDERS_LIT  0.026352313834736493941557
							6449     FCVBOULDERS_LIT  0.019462473604038073998757
							6683     FCVBOULDERS_LIT                          NA
							7263     FCVBOULDERS_LIT  0.097752521990767865522898
							7913     FCVBOULDERS_LIT  0.000000000000000000000000
							1000057     FCVBOULDERS_LIT  0.092582009977255144694830
							6160        FCVBRUSH_LIT  0.024152294576982397122933
							6189        FCVBRUSH_LIT  0.066874675492462926085224
							6227        FCVBRUSH_LIT  0.021081851067789196541025
							6235        FCVBRUSH_LIT  0.066666666666666665741481
							6281        FCVBRUSH_LIT  0.000000000000000000000000
							6449        FCVBRUSH_LIT  0.025746432527221863040268
							6683        FCVBRUSH_LIT                          NA
							7263        FCVBRUSH_LIT  0.025819888974716112550745
							7913        FCVBRUSH_LIT  0.000000000000000000000000
							1000057        FCVBRUSH_LIT  0.000000000000000000000000
							6160       FCVLEDGES_LIT  0.181830965459681803686465
							6189       FCVLEDGES_LIT  0.024152294576982397122933
							6227       FCVLEDGES_LIT  0.015811388300841895671045
							6235       FCVLEDGES_LIT  0.243491672228116351472949
							6281       FCVLEDGES_LIT  0.000000000000000000000000
							6449       FCVLEDGES_LIT  0.014433756729740645358140
							6683       FCVLEDGES_LIT                          NA
							7263       FCVLEDGES_LIT  0.015811388300841895671045
							1000057       FCVLEDGES_LIT                          NA
							6160    FCVLIVETREES_LIT  0.015811388300841895671045
							6189    FCVLIVETREES_LIT  0.000000000000000000000000
							6227    FCVLIVETREES_LIT  0.079056941504209485294119
							6235    FCVLIVETREES_LIT  0.016666666666666669904817
							6281    FCVLIVETREES_LIT  0.000000000000000000000000
							6449    FCVLIVETREES_LIT  0.014433756729740645358140
							6683    FCVLIVETREES_LIT                          NA
							7263    FCVLIVETREES_LIT  0.000000000000000000000000
							1000057    FCVLIVETREES_LIT                          NA
							6160     FCVOVERHANG_LIT  0.015811388300841895671045
							6189     FCVOVERHANG_LIT  0.084327404271156772286311
							6227     FCVOVERHANG_LIT  0.127366487830285340931979
							6235     FCVOVERHANG_LIT  0.179311956594583432611500
							6281     FCVOVERHANG_LIT  0.000000000000000000000000
							6449     FCVOVERHANG_LIT  0.071774056256527343777840
							6683     FCVOVERHANG_LIT                          NA
							7263     FCVOVERHANG_LIT  0.024152294576982397122933
							7913     FCVOVERHANG_LIT  0.000000000000000000000000
							1000057     FCVOVERHANG_LIT  0.000000000000000000000000
							6160        FCVSNAGS_LIT  0.000000000000000000000000
							6189        FCVSNAGS_LIT  0.000000000000000000000000
							6227        FCVSNAGS_LIT  0.000000000000000000000000
							6235        FCVSNAGS_LIT  0.022047927592204922403463
							6281        FCVSNAGS_LIT  0.000000000000000000000000
							6449        FCVSNAGS_LIT  0.000000000000000000000000
							6683        FCVSNAGS_LIT                          NA
							7263        FCVSNAGS_LIT  0.021081851067789196541025
							7913        FCVSNAGS_LIT  0.000000000000000000000000
							1000057        FCVSNAGS_LIT  0.028867513459481290716280
							6160   FCVSTRUCTURES_LIT  0.079056941504209485294119
							6189   FCVSTRUCTURES_LIT  0.000000000000000000000000
							6227   FCVSTRUCTURES_LIT  0.015811388300841895671045
							6235   FCVSTRUCTURES_LIT  0.000000000000000000000000
							6281   FCVSTRUCTURES_LIT  0.000000000000000000000000
							6449   FCVSTRUCTURES_LIT  0.025746432527221863040268
							6683   FCVSTRUCTURES_LIT                          NA
							7263   FCVSTRUCTURES_LIT  0.015811388300841895671045
							6160      FCVAQUATIC_SIM  0.201583977537898556553486
							6189      FCVAQUATIC_SIM  0.171290039925787201946505
							6227      FCVAQUATIC_SIM  0.278062004725964317941589
							6235      FCVAQUATIC_SIM  0.016666666666666669904817
							6281      FCVAQUATIC_SIM  0.000000000000000000000000
							6449      FCVAQUATIC_SIM  0.001630671919513826829268
							6683      FCVAQUATIC_SIM                          NA
							7263      FCVAQUATIC_SIM  0.000000000000000000000000
							7913      FCVAQUATIC_SIM  0.370332058203625302805762
							1000057      FCVAQUATIC_SIM  0.345205252953466323884157
							6160     FCVBOULDERS_SIM  0.000000000000000000000000
							6189     FCVBOULDERS_SIM  0.188580221656461100021573
							6227     FCVBOULDERS_SIM  0.181830965459681803686465
							6281     FCVBOULDERS_SIM  0.006666666666666667094565
							6449     FCVBOULDERS_SIM  0.022415107104699533852044
							6683     FCVBOULDERS_SIM                          NA
							7263     FCVBOULDERS_SIM  0.097752521990767865522898
							7913     FCVBOULDERS_SIM  0.000000000000000000000000
							1000057     FCVBOULDERS_SIM  0.092582009977255144694830
							6160        FCVBRUSH_SIM  0.185744754673958095692043
							6189        FCVBRUSH_SIM  0.066874675492462926085224
							6227        FCVBRUSH_SIM  0.020427650106874480073760
							6235        FCVBRUSH_SIM  0.066666666666666665741481
							6281        FCVBRUSH_SIM  0.000000000000000000000000
							6449        FCVBRUSH_SIM  0.026731266450742773022053
							6683        FCVBRUSH_SIM                          NA
							7263        FCVBRUSH_SIM  0.025819888974716112550745
							7913        FCVBRUSH_SIM  0.000000000000000000000000
							1000057        FCVBRUSH_SIM  0.000000000000000000000000
							6160       FCVLEDGES_SIM  0.069095766874679082070188
							6189       FCVLEDGES_SIM  0.024152294576982397122933
							6227       FCVLEDGES_SIM  0.015811388300841895671045
							6235       FCVLEDGES_SIM  0.243491672228116351472949
							6281       FCVLEDGES_SIM  0.000000000000000000000000
							6449       FCVLEDGES_SIM  0.014386112305717875872735
							6683       FCVLEDGES_SIM                          NA
							7263       FCVLEDGES_SIM  0.015811388300841895671045
							1000057       FCVLEDGES_SIM                          NA
							6160    FCVLIVETREES_SIM  0.009012491331479880948829
							6189    FCVLIVETREES_SIM  0.000000000000000000000000
							6227    FCVLIVETREES_SIM  0.078725684076629873220377
							6235    FCVLIVETREES_SIM  0.016666666666666669904817
							6281    FCVLIVETREES_SIM  0.000000000000000000000000
							6449    FCVLIVETREES_SIM  0.015588457268119896084735
							6683    FCVLIVETREES_SIM                          NA
							7263    FCVLIVETREES_SIM  0.000000000000000000000000
							1000057    FCVLIVETREES_SIM                          NA
							6160     FCVOVERHANG_SIM  0.055472816165517802033946
							6189     FCVOVERHANG_SIM  0.084327404271156772286311
							6227     FCVOVERHANG_SIM  0.121223576731407975404231
							6235     FCVOVERHANG_SIM  0.179311956594583432611500
							6281     FCVOVERHANG_SIM  0.000000000000000000000000
							6449     FCVOVERHANG_SIM  0.077181368453556428721463
							6683     FCVOVERHANG_SIM                          NA
							7263     FCVOVERHANG_SIM  0.024152294576982397122933
							7913     FCVOVERHANG_SIM  0.000000000000000000000000
							1000057     FCVOVERHANG_SIM  0.000000000000000000000000
							6160        FCVSNAGS_SIM  0.006798896969362014722216
							6189        FCVSNAGS_SIM  0.000000000000000000000000
							6227        FCVSNAGS_SIM  0.000000000000000000000000
							6235        FCVSNAGS_SIM  0.022047927592204922403463
							6281        FCVSNAGS_SIM  0.000000000000000000000000
							6449        FCVSNAGS_SIM  0.000000000000000000000000
							6683        FCVSNAGS_SIM                          NA
							7263        FCVSNAGS_SIM  0.021081851067789196541025
							7913        FCVSNAGS_SIM  0.000000000000000000000000
							1000057        FCVSNAGS_SIM  0.028867513459481290716280
							6160   FCVSTRUCTURES_SIM  0.079056941504209485294119
							6189   FCVSTRUCTURES_SIM  0.000000000000000000000000
							6227   FCVSTRUCTURES_SIM  0.015811388300841895671045
							6235   FCVSTRUCTURES_SIM  0.000000000000000000000000
							6281   FCVSTRUCTURES_SIM  0.000000000000000000000000
							6449   FCVSTRUCTURES_SIM  0.025497883156343499611030
							6683   FCVSTRUCTURES_SIM                          NA
							7263   FCVSTRUCTURES_SIM  0.015811388300841895671045
#							6160     FCVAQUATIC_SYN0  0.201583977537898556553486
#							6189     FCVAQUATIC_SYN0                          NA
#							6227     FCVAQUATIC_SYN0  0.205018627202505687723288
#							6235     FCVAQUATIC_SYN0                          NA
#							6281     FCVAQUATIC_SYN0  0.000000000000000000000000
#							6449     FCVAQUATIC_SYN0  0.001630671919513826829268
#							6683     FCVAQUATIC_SYN0                          NA
#							7263     FCVAQUATIC_SYN0                          NA
#							7913     FCVAQUATIC_SYN0                          NA
#							1000057     FCVAQUATIC_SYN0                          NA
#							6160    FCVBOULDERS_SYN0  0.000000000000000000000000
#							6189    FCVBOULDERS_SYN0                          NA
#							6227    FCVBOULDERS_SYN0  0.257147817412475776510661
#							6281    FCVBOULDERS_SYN0  0.006666666666666667094565
#							6449    FCVBOULDERS_SYN0  0.022415107104699533852044
#							6683    FCVBOULDERS_SYN0                          NA
#							7263    FCVBOULDERS_SYN0                          NA
#							7913    FCVBOULDERS_SYN0                          NA
#							1000057    FCVBOULDERS_SYN0                          NA
#							6160       FCVBRUSH_SYN0  0.185744754673958095692043
#							6189       FCVBRUSH_SYN0                          NA
#							6227       FCVBRUSH_SYN0  0.006066300355241240234438
#							6235       FCVBRUSH_SYN0                          NA
#							6281       FCVBRUSH_SYN0  0.000000000000000000000000
#							6449       FCVBRUSH_SYN0  0.026731266450742773022053
#							6683       FCVBRUSH_SYN0                          NA
#							7263       FCVBRUSH_SYN0                          NA
#							7913       FCVBRUSH_SYN0                          NA
#							1000057       FCVBRUSH_SYN0                          NA
#							6160      FCVLEDGES_SYN0  0.069095766874679082070188
#							6189      FCVLEDGES_SYN0                          NA
#							6227      FCVLEDGES_SYN0  0.022360679774997897079070
#							6235      FCVLEDGES_SYN0                          NA
#							6281      FCVLEDGES_SYN0  0.000000000000000000000000
#							6449      FCVLEDGES_SYN0  0.014386112305717875872735
#							6683      FCVLEDGES_SYN0                          NA
#							7263      FCVLEDGES_SYN0                          NA
#							1000057      FCVLEDGES_SYN0                          NA
#							6160   FCVLIVETREES_SYN0  0.009012491331479880948829
#							6189   FCVLIVETREES_SYN0                          NA
#							6227   FCVLIVETREES_SYN0  0.110583904796312915141954
#							6235   FCVLIVETREES_SYN0                          NA
#							6281   FCVLIVETREES_SYN0  0.000000000000000000000000
#							6449   FCVLIVETREES_SYN0  0.015588457268119896084735
#							6683   FCVLIVETREES_SYN0                          NA
#							7263   FCVLIVETREES_SYN0                          NA
#							1000057   FCVLIVETREES_SYN0                          NA
#							6160    FCVOVERHANG_SYN0  0.055472816165517802033946
#							6189    FCVOVERHANG_SYN0                          NA
#							6227    FCVOVERHANG_SYN0  0.120711225658593990228695
#							6235    FCVOVERHANG_SYN0                          NA
#							6281    FCVOVERHANG_SYN0  0.000000000000000000000000
#							6449    FCVOVERHANG_SYN0  0.077181368453556428721463
#							6683    FCVOVERHANG_SYN0                          NA
#							7263    FCVOVERHANG_SYN0                          NA
#							7913    FCVOVERHANG_SYN0                          NA
#							1000057    FCVOVERHANG_SYN0                          NA
#							6160       FCVSNAGS_SYN0  0.006798896969362014722216
#							6189       FCVSNAGS_SYN0                          NA
#							6227       FCVSNAGS_SYN0  0.000000000000000000000000
#							6235       FCVSNAGS_SYN0                          NA
#							6281       FCVSNAGS_SYN0  0.000000000000000000000000
#							6449       FCVSNAGS_SYN0  0.000000000000000000000000
#							6683       FCVSNAGS_SYN0                          NA
#							7263       FCVSNAGS_SYN0                          NA
#							7913       FCVSNAGS_SYN0                          NA
#							1000057       FCVSNAGS_SYN0                          NA
#							6160  FCVSTRUCTURES_SYN0  0.079056941504209485294119
#							6189  FCVSTRUCTURES_SYN0                          NA
#							6227  FCVSTRUCTURES_SYN0  0.000000000000000000000000
#							6235  FCVSTRUCTURES_SYN0                          NA
#							6281  FCVSTRUCTURES_SYN0  0.000000000000000000000000
#							6449  FCVSTRUCTURES_SYN0  0.025497883156343499611030
#							6683  FCVSTRUCTURES_SYN0                          NA
#							7263  FCVSTRUCTURES_SYN0                          NA
							6160      FCFCAQUATIC_DD  0.257500000000000006661338
							6227      FCFCAQUATIC_DD  0.059999999999999997779554
							6281      FCFCAQUATIC_DD  0.000000000000000000000000
							6449      FCFCAQUATIC_DD  0.045833333333333337034077
							6683      FCFCAQUATIC_DD  0.000000000000000000000000
							7263      FCFCAQUATIC_DD  0.050000000000000002775558
							6160     FCFCBOULDERS_DD  0.000000000000000000000000
							6227     FCFCBOULDERS_DD  0.114999999999999991118216
							6281     FCFCBOULDERS_DD  0.000000000000000000000000
							6449     FCFCBOULDERS_DD  0.089583333333333334258519
							6683     FCFCBOULDERS_DD  0.000000000000000000000000
							6160        FCFCBRUSH_DD  0.127500000000000002220446
							6227        FCFCBRUSH_DD  0.020000000000000000416334
							6281        FCFCBRUSH_DD  0.000000000000000000000000
							6449        FCFCBRUSH_DD  0.070833333333333331482962
							6683        FCFCBRUSH_DD  0.000000000000000000000000
							6160       FCFCLEDGES_DD  0.000000000000000000000000
							6227       FCFCLEDGES_DD  0.010000000000000000208167
							6281       FCFCLEDGES_DD  0.000000000000000000000000
							6449       FCFCLEDGES_DD  0.008333333333333333217685
							6683       FCFCLEDGES_DD  0.000000000000000000000000
							6160    FCFCLIVETREES_DD  0.000000000000000000000000
							6227    FCFCLIVETREES_DD  0.059999999999999997779554
							6281    FCFCLIVETREES_DD  0.000000000000000000000000
							6449    FCFCLIVETREES_DD  0.020833333333333332176851
							6683    FCFCLIVETREES_DD  0.000000000000000000000000
							6160     FCFCOVERHANG_DD  0.060000000000000004718448
							6227     FCFCOVERHANG_DD  0.070000000000000006661338
							6281     FCFCOVERHANG_DD  0.000000000000000000000000
							6449     FCFCOVERHANG_DD  0.102272727272727265157570
							6683     FCFCOVERHANG_DD  0.000000000000000000000000
							6160        FCFCSNAGS_DD  0.005000000000000000104083
							6227        FCFCSNAGS_DD  0.000000000000000000000000
							6281        FCFCSNAGS_DD  0.000000000000000000000000
							6449        FCFCSNAGS_DD  0.000000000000000000000000
							6683        FCFCSNAGS_DD  0.000000000000000000000000
							7263        FCFCSNAGS_DD  0.000000000000000000000000
							6160   FCFCSTRUCTURES_DD  0.025000000000000001387779
							6227   FCFCSTRUCTURES_DD  0.000000000000000000000000
							6281   FCFCSTRUCTURES_DD  0.000000000000000000000000
							6449   FCFCSTRUCTURES_DD  0.033333333333333332870740
							6683   FCFCSTRUCTURES_DD  0.000000000000000000000000
							6160     FCFCAQUATIC_LIT  0.110000000000000000555112
							6189     FCFCAQUATIC_LIT  0.412499999999999977795540
							6227     FCFCAQUATIC_LIT  0.354999999999999982236432
							6235     FCFCAQUATIC_LIT  0.044444444444444446140619
							6281     FCFCAQUATIC_LIT  0.000000000000000000000000
							6449     FCFCAQUATIC_LIT  0.050000000000000002775558
							6683     FCFCAQUATIC_LIT  0.875000000000000000000000
							7263     FCFCAQUATIC_LIT  0.050000000000000002775558
							7913     FCFCAQUATIC_LIT  0.647499999999999964472863
							1000057     FCFCAQUATIC_LIT  0.308333333333333348136307
							6160    FCFCBOULDERS_LIT  0.000000000000000000000000
							6189    FCFCBOULDERS_LIT  0.142499999999999987787547
							6227    FCFCBOULDERS_LIT  0.057499999999999995559108
							6281    FCFCBOULDERS_LIT  0.022222222222222223070309
							6449    FCFCBOULDERS_LIT  0.008333333333333333217685
							6683    FCFCBOULDERS_LIT  0.000000000000000000000000
							7263    FCFCBOULDERS_LIT  0.070000000000000006661338
							7913    FCFCBOULDERS_LIT  0.050000000000000002775558
							1000057    FCFCBOULDERS_LIT  0.100000000000000005551115
							6160       FCFCBRUSH_LIT  0.035000000000000003330669
							6189       FCFCBRUSH_LIT  0.065000000000000002220446
							6227       FCFCBRUSH_LIT  0.010000000000000000208167
							6235       FCFCBRUSH_LIT  0.072222222222222229315314
							6281       FCFCBRUSH_LIT  0.000000000000000000000000
							6449       FCFCBRUSH_LIT  0.029166666666666667129260
							6683       FCFCBRUSH_LIT  0.000000000000000000000000
							7263       FCFCBRUSH_LIT  0.030000000000000002359224
							7913       FCFCBRUSH_LIT  0.050000000000000002775558
							1000057       FCFCBRUSH_LIT  0.050000000000000002775558
							6160      FCFCLEDGES_LIT  0.057499999999999995559108
							6189      FCFCLEDGES_LIT  0.015000000000000001179612
							6227      FCFCLEDGES_LIT  0.005000000000000000104083
							6235      FCFCLEDGES_LIT  0.602777777777777745704668
							6281      FCFCLEDGES_LIT  0.000000000000000000000000
							6449      FCFCLEDGES_LIT  0.004166666666666666608843
							6683      FCFCLEDGES_LIT  0.000000000000000000000000
							7263      FCFCLEDGES_LIT  0.005000000000000000104083
							1000057      FCFCLEDGES_LIT  0.250000000000000000000000
							6160   FCFCLIVETREES_LIT  0.005000000000000000104083
							6189   FCFCLIVETREES_LIT  0.000000000000000000000000
							6227   FCFCLIVETREES_LIT  0.025000000000000001387779
							6235   FCFCLIVETREES_LIT  0.005555555555555555767577
							6281   FCFCLIVETREES_LIT  0.000000000000000000000000
							6449   FCFCLIVETREES_LIT  0.004166666666666666608843
							6683   FCFCLIVETREES_LIT  0.000000000000000000000000
							7263   FCFCLIVETREES_LIT  0.000000000000000000000000
							1000057   FCFCLIVETREES_LIT  0.000000000000000000000000
							6160    FCFCOVERHANG_LIT  0.005000000000000000104083
							6189    FCFCOVERHANG_LIT  0.089999999999999996669331
							6227    FCFCOVERHANG_LIT  0.130000000000000004440892
							6235    FCFCOVERHANG_LIT  0.130555555555555563573833
							6281    FCFCOVERHANG_LIT  0.000000000000000000000000
							6449    FCFCOVERHANG_LIT  0.033333333333333332870740
							6683    FCFCOVERHANG_LIT  0.000000000000000000000000
							7263    FCFCOVERHANG_LIT  0.035000000000000003330669
							7913    FCFCOVERHANG_LIT  0.050000000000000002775558
							1000057    FCFCOVERHANG_LIT  0.050000000000000002775558
							6160       FCFCSNAGS_LIT  0.000000000000000000000000
							6189       FCFCSNAGS_LIT  0.000000000000000000000000
							6227       FCFCSNAGS_LIT  0.000000000000000000000000
							6235       FCFCSNAGS_LIT  0.011111111111111111535155
							6281       FCFCSNAGS_LIT  0.000000000000000000000000
							6449       FCFCSNAGS_LIT  0.000000000000000000000000
							6683       FCFCSNAGS_LIT  0.000000000000000000000000
							7263       FCFCSNAGS_LIT  0.010000000000000000208167
							7913       FCFCSNAGS_LIT  0.000000000000000000000000
							1000057       FCFCSNAGS_LIT  0.033333333333333332870740
							6160  FCFCSTRUCTURES_LIT  0.025000000000000001387779
							6189  FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6227  FCFCSTRUCTURES_LIT  0.005000000000000000104083
							6235  FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6281  FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6449  FCFCSTRUCTURES_LIT  0.029166666666666667129260
							6683  FCFCSTRUCTURES_LIT  0.000000000000000000000000
							7263  FCFCSTRUCTURES_LIT  0.005000000000000000104083
							6160     FCFCAQUATIC_SIM  0.213150000000000006128431
							6189     FCFCAQUATIC_SIM  0.412499999999999977795540
							6227     FCFCAQUATIC_SIM  0.345274999999999998578915
							6235     FCFCAQUATIC_SIM  0.044444444444444446140619
							6281     FCFCAQUATIC_SIM  0.000000000000000000000000
							6449     FCFCAQUATIC_SIM  0.049750000000000002553513
							6683     FCFCAQUATIC_SIM  0.000000000000000000000000
							7263     FCFCAQUATIC_SIM  0.050000000000000002775558
							7913     FCFCAQUATIC_SIM  0.647499999999999964472863
							1000057     FCFCAQUATIC_SIM  0.308333333333333348136307
							6160    FCFCBOULDERS_SIM  0.000000000000000000000000
							6189    FCFCBOULDERS_SIM  0.142499999999999987787547
							6227    FCFCBOULDERS_SIM  0.057499999999999995559108
							6281    FCFCBOULDERS_SIM  0.002222222222222222220295
							6449    FCFCBOULDERS_SIM  0.011020833333333333869941
							6683    FCFCBOULDERS_SIM  0.000000000000000000000000
							7263    FCFCBOULDERS_SIM  0.070000000000000006661338
							7913    FCFCBOULDERS_SIM  0.050000000000000002775558
							1000057    FCFCBOULDERS_SIM  0.100000000000000005551115
							6160       FCFCBRUSH_SIM  0.100849999999999995203837
							6189       FCFCBRUSH_SIM  0.065000000000000002220446
							6227       FCFCBRUSH_SIM  0.012200000000000000774381
							6235       FCFCBRUSH_SIM  0.072222222222222229315314
							6281       FCFCBRUSH_SIM  0.000000000000000000000000
							6449       FCFCBRUSH_SIM  0.030833333333333334119741
							6683       FCFCBRUSH_SIM  0.000000000000000000000000
							7263       FCFCBRUSH_SIM  0.030000000000000002359224
							7913       FCFCBRUSH_SIM  0.050000000000000002775558
							1000057       FCFCBRUSH_SIM  0.050000000000000002775558
							6160      FCFCLEDGES_SIM  0.021850000000000001365574
							6189      FCFCLEDGES_SIM  0.015000000000000001179612
							6227      FCFCLEDGES_SIM  0.005000000000000000104083
							6235      FCFCLEDGES_SIM  0.602777777777777745704668
							6281      FCFCLEDGES_SIM  0.000000000000000000000000
							6449      FCFCLEDGES_SIM  0.004375000000000000416334
							6683      FCFCLEDGES_SIM  0.000000000000000000000000
							7263      FCFCLEDGES_SIM  0.005000000000000000104083
							1000057      FCFCLEDGES_SIM  0.250000000000000000000000
							6160   FCFCLIVETREES_SIM  0.002850000000000000102696
							6189   FCFCLIVETREES_SIM  0.000000000000000000000000
							6227   FCFCLIVETREES_SIM  0.026200000000000001065814
							6235   FCFCLIVETREES_SIM  0.005555555555555555767577
							6281   FCFCLIVETREES_SIM  0.000000000000000000000000
							6449   FCFCLIVETREES_SIM  0.004499999999999999659994
							6683   FCFCLIVETREES_SIM  0.000000000000000000000000
							7263   FCFCLIVETREES_SIM  0.000000000000000000000000
							1000057   FCFCLIVETREES_SIM  0.000000000000000000000000
							6160    FCFCOVERHANG_SIM  0.034700000000000001676437
							6189    FCFCOVERHANG_SIM  0.089999999999999996669331
							6227    FCFCOVERHANG_SIM  0.125400000000000011457502
							6235    FCFCOVERHANG_SIM  0.130555555555555563573833
							6281    FCFCOVERHANG_SIM  0.000000000000000000000000
							6449    FCFCOVERHANG_SIM  0.039681818181818179214204
							6683    FCFCOVERHANG_SIM  0.000000000000000000000000
							7263    FCFCOVERHANG_SIM  0.035000000000000003330669
							7913    FCFCOVERHANG_SIM  0.050000000000000002775558
							1000057    FCFCOVERHANG_SIM  0.050000000000000002775558
							6160       FCFCSNAGS_SIM  0.002150000000000000001388
							6189       FCFCSNAGS_SIM  0.000000000000000000000000
							6227       FCFCSNAGS_SIM  0.000000000000000000000000
							6235       FCFCSNAGS_SIM  0.011111111111111111535155
							6281       FCFCSNAGS_SIM  0.000000000000000000000000
							6449       FCFCSNAGS_SIM  0.000000000000000000000000
							6683       FCFCSNAGS_SIM  0.000000000000000000000000
							7263       FCFCSNAGS_SIM  0.010000000000000000208167
							7913       FCFCSNAGS_SIM  0.000000000000000000000000
							1000057       FCFCSNAGS_SIM  0.033333333333333332870740
							6160  FCFCSTRUCTURES_SIM  0.025000000000000001387779
							6189  FCFCSTRUCTURES_SIM  0.000000000000000000000000
							6227  FCFCSTRUCTURES_SIM  0.005000000000000000104083
							6235  FCFCSTRUCTURES_SIM  0.000000000000000000000000
							6281  FCFCSTRUCTURES_SIM  0.000000000000000000000000
							6449  FCFCSTRUCTURES_SIM  0.029375000000000001804112
							6683  FCFCSTRUCTURES_SIM  0.000000000000000000000000
							7263  FCFCSTRUCTURES_SIM  0.005000000000000000104083
##							6160    FCFCAQUATIC_SYN0  0.213150000000000006128431
#							6189    FCFCAQUATIC_SYN0                         NaN
#							6227    FCFCAQUATIC_SYN0  0.115549999999999999933387
#							6235    FCFCAQUATIC_SYN0                         NaN
#							6281    FCFCAQUATIC_SYN0  0.000000000000000000000000
#							6449    FCFCAQUATIC_SYN0  0.049750000000000002553513
#							6683    FCFCAQUATIC_SYN0  0.000000000000000000000000
#							7263    FCFCAQUATIC_SYN0  0.050000000000000002775558
#							7913    FCFCAQUATIC_SYN0                         NaN
#							1000057    FCFCAQUATIC_SYN0                         NaN
#							6160   FCFCBOULDERS_SYN0  0.000000000000000000000000
#							6189   FCFCBOULDERS_SYN0                         NaN
#							6227   FCFCBOULDERS_SYN0  0.114999999999999991118216
#							6281   FCFCBOULDERS_SYN0  0.002222222222222222220295
#							6449   FCFCBOULDERS_SYN0  0.011020833333333333869941
#							6683   FCFCBOULDERS_SYN0  0.000000000000000000000000
#							7263   FCFCBOULDERS_SYN0                         NaN
#							7913   FCFCBOULDERS_SYN0                         NaN
#							1000057   FCFCBOULDERS_SYN0                         NaN
#							6160      FCFCBRUSH_SYN0  0.100849999999999995203837
#							6189      FCFCBRUSH_SYN0                         NaN
#							6227      FCFCBRUSH_SYN0  0.004400000000000000265066
#							6235      FCFCBRUSH_SYN0                         NaN
#							6281      FCFCBRUSH_SYN0  0.000000000000000000000000
#							6449      FCFCBRUSH_SYN0  0.030833333333333334119741
#							6683      FCFCBRUSH_SYN0  0.000000000000000000000000
#							7263      FCFCBRUSH_SYN0                         NaN
#							7913      FCFCBRUSH_SYN0                         NaN
#							1000057      FCFCBRUSH_SYN0                         NaN
#							6160     FCFCLEDGES_SYN0  0.021850000000000001365574
#							6189     FCFCLEDGES_SYN0                         NaN
#							6227     FCFCLEDGES_SYN0  0.010000000000000000208167
#							6235     FCFCLEDGES_SYN0                         NaN
#							6281     FCFCLEDGES_SYN0  0.000000000000000000000000
#							6449     FCFCLEDGES_SYN0  0.004375000000000000416334
#							6683     FCFCLEDGES_SYN0  0.000000000000000000000000
#							7263     FCFCLEDGES_SYN0                         NaN
#							1000057     FCFCLEDGES_SYN0                         NaN
#							6160  FCFCLIVETREES_SYN0  0.002850000000000000102696
#							6189  FCFCLIVETREES_SYN0                         NaN
#							6227  FCFCLIVETREES_SYN0  0.052400000000000002131628
#							6235  FCFCLIVETREES_SYN0                         NaN
#							6281  FCFCLIVETREES_SYN0  0.000000000000000000000000
#							6449  FCFCLIVETREES_SYN0  0.004499999999999999659994
#							6683  FCFCLIVETREES_SYN0  0.000000000000000000000000
#							7263  FCFCLIVETREES_SYN0                         NaN
#							1000057  FCFCLIVETREES_SYN0                         NaN
#							6160   FCFCOVERHANG_SYN0  0.034700000000000001676437
#							6189   FCFCOVERHANG_SYN0                         NaN
#							6227   FCFCOVERHANG_SYN0  0.090800000000000005706546
#							6235   FCFCOVERHANG_SYN0                         NaN
#							6281   FCFCOVERHANG_SYN0  0.000000000000000000000000
#							6449   FCFCOVERHANG_SYN0  0.039681818181818179214204
#							6683   FCFCOVERHANG_SYN0  0.000000000000000000000000
#							7263   FCFCOVERHANG_SYN0                         NaN
#							7913   FCFCOVERHANG_SYN0                         NaN
#							1000057   FCFCOVERHANG_SYN0                         NaN
#							6160      FCFCSNAGS_SYN0  0.002150000000000000001388
#							6189      FCFCSNAGS_SYN0                         NaN
#							6227      FCFCSNAGS_SYN0  0.000000000000000000000000
#							6235      FCFCSNAGS_SYN0                         NaN
#							6281      FCFCSNAGS_SYN0  0.000000000000000000000000
#							6449      FCFCSNAGS_SYN0  0.000000000000000000000000
#							6683      FCFCSNAGS_SYN0  0.000000000000000000000000
#							7263      FCFCSNAGS_SYN0  0.000000000000000000000000
#							7913      FCFCSNAGS_SYN0                         NaN
#							1000057      FCFCSNAGS_SYN0                         NaN
#							6160 FCFCSTRUCTURES_SYN0  0.025000000000000001387779
#							6189 FCFCSTRUCTURES_SYN0                         NaN
#							6227 FCFCSTRUCTURES_SYN0  0.000000000000000000000000
#							6235 FCFCSTRUCTURES_SYN0                         NaN
#							6281 FCFCSTRUCTURES_SYN0  0.000000000000000000000000
#							6449 FCFCSTRUCTURES_SYN0  0.029375000000000001804112
#							6683 FCFCSTRUCTURES_SYN0  0.000000000000000000000000
#							7263 FCFCSTRUCTURES_SYN0                         NaN
							6160           FCIALL_DD  0.475000000000000033306691
							6227           FCIALL_DD  0.335000000000000019984014
							6281           FCIALL_DD  0.000000000000000000000000
							6449           FCIALL_DD  0.371022727272727281810916
							6683           FCIALL_DD  0.000000000000000000000000
							7263           FCIALL_DD  0.050000000000000002775558
							6160          FCIALL_LIT  0.237499999999999988897770
							6189          FCIALL_LIT  0.724999999999999977795540
							6227          FCIALL_LIT  0.587500000000000022204460
							6235          FCIALL_LIT  0.866666666666666696272614
							6281          FCIALL_LIT  0.022222222222222223070309
							6449          FCIALL_LIT  0.158333333333333325931847
							6683          FCIALL_LIT  0.875000000000000000000000
							7263          FCIALL_LIT  0.205000000000000015543122
							7913          FCIALL_LIT  0.797499999999999986677324
							1000057          FCIALL_LIT  0.791666666666666740681535
							6160          FCIALL_SIM  0.400550000000000017141844
							6189          FCIALL_SIM  0.724999999999999977795540
							6227          FCIALL_SIM  0.576575000000000059685590
							6235          FCIALL_SIM  0.866666666666666696272614
							6281          FCIALL_SIM  0.002222222222222222220295
							6449          FCIALL_SIM  0.169535984848484838627414
							6683          FCIALL_SIM  0.000000000000000000000000
							7263          FCIALL_SIM  0.205000000000000015543122
							7913          FCIALL_SIM  0.797499999999999986677324
							1000057          FCIALL_SIM  0.791666666666666740681535
#							6160         FCIALL_SYN0  0.400550000000000017141844
#							6189         FCIALL_SYN0                          NA
#							6227         FCIALL_SYN0  0.388149999999999995026201
#							6235         FCIALL_SYN0                          NA
#							6281         FCIALL_SYN0  0.002222222222222222220295
#							6449         FCIALL_SYN0  0.169535984848484838627414
#							6683         FCIALL_SYN0  0.000000000000000000000000
#							7263         FCIALL_SYN0  0.050000000000000002775558
#							7913         FCIALL_SYN0                          NA
#							1000057         FCIALL_SYN0                          NA
							6160           FCIBIG_DD  0.085000000000000006106227
							6227           FCIBIG_DD  0.195000000000000006661338
							6281           FCIBIG_DD  0.000000000000000000000000
							6449           FCIBIG_DD  0.233522727272727270708685
							6683           FCIBIG_DD  0.000000000000000000000000
							6160          FCIBIG_LIT  0.087499999999999994448885
							6189          FCIBIG_LIT  0.247499999999999997779554
							6227          FCIBIG_LIT  0.197500000000000008881784
							6235          FCIBIG_LIT  0.733333333333333281522926
							6281          FCIBIG_LIT  0.022222222222222223070309
							6449          FCIBIG_LIT  0.074999999999999997224442
							6683          FCIBIG_LIT  0.000000000000000000000000
							7263          FCIBIG_LIT  0.115000000000000004996004
							7913          FCIBIG_LIT  0.100000000000000005551115
							1000057          FCIBIG_LIT  0.400000000000000022204460
							6160          FCIBIG_SIM  0.081550000000000011368684
							6189          FCIBIG_SIM  0.247499999999999997779554
							6227          FCIBIG_SIM  0.192900000000000015898394
							6235          FCIBIG_SIM  0.733333333333333281522926
							6281          FCIBIG_SIM  0.002222222222222222220295
							6449          FCIBIG_SIM  0.084452651515151522243485
							6683          FCIBIG_SIM  0.000000000000000000000000
							7263          FCIBIG_SIM  0.115000000000000004996004
							7913          FCIBIG_SIM  0.100000000000000005551115
							1000057          FCIBIG_SIM  0.400000000000000022204460
#							6160         FCIBIG_SYN0  0.081550000000000011368684
#							6189         FCIBIG_SYN0                          NA
#							6227         FCIBIG_SYN0  0.215799999999999991828759
#							6235         FCIBIG_SYN0                          NA
#							6281         FCIBIG_SYN0  0.002222222222222222220295
#							6449         FCIBIG_SYN0  0.084452651515151522243485
#							6683         FCIBIG_SYN0  0.000000000000000000000000
#							7263         FCIBIG_SYN0                          NA
#							7913         FCIBIG_SYN0                          NA
#							1000057         FCIBIG_SYN0                          NA
							6160       FCINATURAL_DD  0.450000000000000011102230
							6227       FCINATURAL_DD  0.335000000000000019984014
							6281       FCINATURAL_DD  0.000000000000000000000000
							6449       FCINATURAL_DD  0.337689393939393955879069
							6683       FCINATURAL_DD  0.000000000000000000000000
							7263       FCINATURAL_DD  0.050000000000000002775558
							6160      FCINATURAL_LIT  0.212499999999999994448885
							6189      FCINATURAL_LIT  0.724999999999999977795540
							6227      FCINATURAL_LIT  0.582500000000000017763568
							6235      FCINATURAL_LIT  0.866666666666666696272614
							6281      FCINATURAL_LIT  0.022222222222222223070309
							6449      FCINATURAL_LIT  0.129166666666666679619269
							6683      FCINATURAL_LIT  0.875000000000000000000000
							7263      FCINATURAL_LIT  0.200000000000000011102230
							7913      FCINATURAL_LIT  0.797499999999999986677324
							1000057      FCINATURAL_LIT  0.791666666666666740681535
							6160      FCINATURAL_SIM  0.375549999999999994937383
							6189      FCINATURAL_SIM  0.724999999999999977795540
							6227      FCINATURAL_SIM  0.571575000000000055244698
							6235      FCINATURAL_SIM  0.866666666666666696272614
							6281      FCINATURAL_SIM  0.002222222222222222220295
							6449      FCINATURAL_SIM  0.140160984848484854170536
							6683      FCINATURAL_SIM  0.000000000000000000000000
							7263      FCINATURAL_SIM  0.200000000000000011102230
							7913      FCINATURAL_SIM  0.797499999999999986677324
							1000057      FCINATURAL_SIM  0.791666666666666740681535
#							6160     FCINATURAL_SYN0  0.375549999999999994937383
#							6189     FCINATURAL_SYN0                          NA
#							6227     FCINATURAL_SYN0  0.388149999999999995026201
#							6235     FCINATURAL_SYN0                          NA
#							6281     FCINATURAL_SYN0  0.002222222222222222220295
#							6449     FCINATURAL_SYN0  0.140160984848484854170536
#							6683     FCINATURAL_SYN0  0.000000000000000000000000
#							7263     FCINATURAL_SYN0  0.050000000000000002775558
#							7913     FCINATURAL_SYN0                          NA
#							1000057     FCINATURAL_SYN0                          NA
							6160        FCIRIPVEG_DD  0.132500000000000006661338
							6227        FCIRIPVEG_DD  0.080000000000000001665335
							6281        FCIRIPVEG_DD  0.000000000000000000000000
							6449        FCIRIPVEG_DD  0.091666666666666660190366
							6683        FCIRIPVEG_DD  0.000000000000000000000000
							7263        FCIRIPVEG_DD  0.000000000000000000000000
							6160       FCIRIPVEG_LIT  0.040000000000000000832667
							6189       FCIRIPVEG_LIT  0.065000000000000002220446
							6227       FCIRIPVEG_LIT  0.035000000000000003330669
							6235       FCIRIPVEG_LIT  0.088888888888888892281237
							6281       FCIRIPVEG_LIT  0.000000000000000000000000
							6449       FCIRIPVEG_LIT  0.033333333333333332870740
							6683       FCIRIPVEG_LIT  0.000000000000000000000000
							7263       FCIRIPVEG_LIT  0.040000000000000000832667
							7913       FCIRIPVEG_LIT  0.050000000000000002775558
							1000057       FCIRIPVEG_LIT  0.083333333333333342585192
							6160       FCIRIPVEG_SIM  0.105849999999999999644729
							6189       FCIRIPVEG_SIM  0.065000000000000002220446
							6227       FCIRIPVEG_SIM  0.038400000000000003574918
							6235       FCIRIPVEG_SIM  0.088888888888888892281237
							6281       FCIRIPVEG_SIM  0.000000000000000000000000
							6449       FCIRIPVEG_SIM  0.035333333333333334647097
							6683       FCIRIPVEG_SIM  0.000000000000000000000000
							7263       FCIRIPVEG_SIM  0.040000000000000000832667
							7913       FCIRIPVEG_SIM  0.050000000000000002775558
							1000057       FCIRIPVEG_SIM  0.083333333333333342585192
#							6160      FCIRIPVEG_SYN0  0.105849999999999999644729
#							6189      FCIRIPVEG_SYN0                          NA
#							6227      FCIRIPVEG_SYN0  0.056800000000000003264056
#							6235      FCIRIPVEG_SYN0                          NA
#							6281      FCIRIPVEG_SYN0  0.000000000000000000000000
#							6449      FCIRIPVEG_SYN0  0.035333333333333334647097
#							6683      FCIRIPVEG_SYN0  0.000000000000000000000000
#							7263      FCIRIPVEG_SYN0  0.000000000000000000000000
#							7913      FCIRIPVEG_SYN0                          NA
#							1000057      FCIRIPVEG_SYN0                          NA
							6160          FCFPALL_DD  1.000000000000000000000000
							6227          FCFPALL_DD  0.800000000000000044408921
							6281          FCFPALL_DD  0.000000000000000000000000
							6449          FCFPALL_DD  1.000000000000000000000000
							6683          FCFPALL_DD  0.000000000000000000000000
							7263          FCFPALL_DD  1.000000000000000000000000
							6160         FCFPALL_LIT  1.000000000000000000000000
							6189         FCFPALL_LIT  1.000000000000000000000000
							6227         FCFPALL_LIT  0.900000000000000022204460
							6235         FCFPALL_LIT  1.000000000000000000000000
							6281         FCFPALL_LIT  0.444444444444444419772822
							6449         FCFPALL_LIT  1.000000000000000000000000
							6683         FCFPALL_LIT  1.000000000000000000000000
							7263         FCFPALL_LIT  1.000000000000000000000000
							7913         FCFPALL_LIT  1.000000000000000000000000
							1000057         FCFPALL_LIT  1.000000000000000000000000
							6160         FCFPALL_SIM  1.000000000000000000000000
							6189         FCFPALL_SIM  1.000000000000000000000000
							6227         FCFPALL_SIM  1.000000000000000000000000
							6235         FCFPALL_SIM  1.000000000000000000000000
							6281         FCFPALL_SIM  0.111111111111111104943205
							6449         FCFPALL_SIM  1.000000000000000000000000
							6683         FCFPALL_SIM  0.000000000000000000000000
							7263         FCFPALL_SIM  1.000000000000000000000000
							7913         FCFPALL_SIM  1.000000000000000000000000
							1000057         FCFPALL_SIM  1.000000000000000000000000
#							6160        FCFPALL_SYN0  1.000000000000000000000000
#							6189        FCFPALL_SYN0                         NaN
#							6227        FCFPALL_SYN0  1.000000000000000000000000
#							6235        FCFPALL_SYN0                         NaN
#							6281        FCFPALL_SYN0  0.111111111111111104943205
#							6449        FCFPALL_SYN0  1.000000000000000000000000
#							6683        FCFPALL_SYN0  0.000000000000000000000000
#							7263        FCFPALL_SYN0  1.000000000000000000000000
#							7913        FCFPALL_SYN0                         NaN
#							1000057        FCFPALL_SYN0                         NaN
							6160           FCNALL_DD 10.000000000000000000000000
							6227           FCNALL_DD  5.000000000000000000000000
							6281           FCNALL_DD  9.000000000000000000000000
							6449           FCNALL_DD 12.000000000000000000000000
							6683           FCNALL_DD  1.000000000000000000000000
							7263           FCNALL_DD  1.000000000000000000000000
							6160          FCNALL_LIT 10.000000000000000000000000
							6189          FCNALL_LIT 10.000000000000000000000000
							6227          FCNALL_LIT 10.000000000000000000000000
							6235          FCNALL_LIT  9.000000000000000000000000
							6281          FCNALL_LIT  9.000000000000000000000000
							6449          FCNALL_LIT 12.000000000000000000000000
							6683          FCNALL_LIT  1.000000000000000000000000
							7263          FCNALL_LIT 10.000000000000000000000000
							7913          FCNALL_LIT 10.000000000000000000000000
							1000057          FCNALL_LIT 10.000000000000000000000000
							6160          FCNALL_SIM 10.000000000000000000000000
							6189          FCNALL_SIM 10.000000000000000000000000
							6227          FCNALL_SIM 10.000000000000000000000000
							6235          FCNALL_SIM  9.000000000000000000000000
							6281          FCNALL_SIM  9.000000000000000000000000
							6449          FCNALL_SIM 12.000000000000000000000000
							6683          FCNALL_SIM  1.000000000000000000000000
							7263          FCNALL_SIM 10.000000000000000000000000
							7913          FCNALL_SIM 10.000000000000000000000000
							1000057          FCNALL_SIM 10.000000000000000000000000
#							6160         FCNALL_SYN0 10.000000000000000000000000
#							6189         FCNALL_SYN0  0.000000000000000000000000
#							6227         FCNALL_SYN0  5.000000000000000000000000
#							6235         FCNALL_SYN0  0.000000000000000000000000
#							6281         FCNALL_SYN0  9.000000000000000000000000
#							6449         FCNALL_SYN0 12.000000000000000000000000
#							6683         FCNALL_SYN0  1.000000000000000000000000
#							7263         FCNALL_SYN0  1.000000000000000000000000
#							7913         FCNALL_SYN0  0.000000000000000000000000
#							1000057         FCNALL_SYN0  0.000000000000000000000000
						 ")
		 
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	within(fake
		  ,{SAMPLE_TYPE <- 'PHAB'
			FLAG <- as.character(NA)
			FORM_TYPE <- 'PHAB_FRONT'
		   }
          )
	return(fake)
}



nlaFishCoverTest.expectedResultsWithDrawDownAndNoFillin <- function()
# Expected results for test data with drawdown values which were NOT 'filled in' based
# on the value of the DRAWDOWN parameter.  These values were calculated with SAS on
# 14 Nov 2013, which erred in 15 cases by including _SIM metrics (with NA values) where
# there is no data to create them (e.g. SITE 7913 is missing all data for LEDGES
# LIVETREES and STRUCTURES and so SAS does not include FC[FP|FC|V]LEDGES_[DD|LIT]
# but rows of FC[FP|FC|V]LEDGES_SIM = NA are included.  This is a bug in the %calcSimCover
# macro, but I don't have time to debug it now.)  These 15 rows are removed from the
# expected output.
{
	tc <- textConnection("  SITE             METRIC                       VALUE
							6160     FCFPAQUATIC_DD  1.000000000000000000000000
							6160    FCFPAQUATIC_LIT  1.000000000000000000000000
							6160    FCFPAQUATIC_SIM  1.000000000000000000000000
							6160    FCFPBOULDERS_DD  0.000000000000000000000000
							6160   FCFPBOULDERS_LIT  0.000000000000000000000000
							6160   FCFPBOULDERS_SIM  0.000000000000000000000000
							6160       FCFPBRUSH_DD  0.900000000000000022204460
							6160      FCFPBRUSH_LIT  0.699999999999999955591079
							6160      FCFPBRUSH_SIM  0.900000000000000022204460
							6160      FCFPLEDGES_DD  0.000000000000000000000000
							6160     FCFPLEDGES_LIT  0.100000000000000005551115
							6160     FCFPLEDGES_SIM  0.100000000000000005551115
							6160   FCFPLIVETREES_DD  0.000000000000000000000000
							6160  FCFPLIVETREES_LIT  0.100000000000000005551115
							6160  FCFPLIVETREES_SIM  0.100000000000000005551115
							6160    FCFPOVERHANG_DD  0.800000000000000044408921
							6160   FCFPOVERHANG_LIT  0.100000000000000005551115
							6160   FCFPOVERHANG_SIM  0.800000000000000044408921
							6160       FCFPSNAGS_DD  0.100000000000000005551115
							6160      FCFPSNAGS_LIT  0.000000000000000000000000
							6160      FCFPSNAGS_SIM  0.100000000000000005551115
							6160  FCFPSTRUCTURES_DD  0.100000000000000005551115
							6160 FCFPSTRUCTURES_LIT  0.100000000000000005551115
							6160 FCFPSTRUCTURES_SIM  0.100000000000000005551115
							6189    FCFPAQUATIC_LIT  1.000000000000000000000000
							6189    FCFPAQUATIC_SIM                          NA
							6189   FCFPBOULDERS_LIT  0.599999999999999977795540
							6189   FCFPBOULDERS_SIM                          NA
							6189      FCFPBRUSH_LIT  0.900000000000000022204460
							6189      FCFPBRUSH_SIM                          NA
							6189     FCFPLEDGES_LIT  0.299999999999999988897770
							6189     FCFPLEDGES_SIM                          NA
							6189  FCFPLIVETREES_LIT  0.000000000000000000000000
							6189  FCFPLIVETREES_SIM                          NA
							6189   FCFPOVERHANG_LIT  1.000000000000000000000000
							6189   FCFPOVERHANG_SIM                          NA
							6189      FCFPSNAGS_LIT  0.000000000000000000000000
							6189      FCFPSNAGS_SIM                          NA
							6189 FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6189 FCFPSTRUCTURES_SIM                          NA
							6227     FCFPAQUATIC_DD  0.400000000000000022204460
							6227    FCFPAQUATIC_LIT  0.800000000000000044408921
							6227    FCFPAQUATIC_SIM  0.800000000000000044408921
							6227    FCFPBOULDERS_DD  0.200000000000000011102230
							6227   FCFPBOULDERS_LIT  0.100000000000000005551115
							6227   FCFPBOULDERS_SIM  0.200000000000000011102230
							6227       FCFPBRUSH_DD  0.400000000000000022204460
							6227      FCFPBRUSH_LIT  0.200000000000000011102230
							6227      FCFPBRUSH_SIM  0.400000000000000022204460
							6227      FCFPLEDGES_DD  0.200000000000000011102230
							6227     FCFPLEDGES_LIT  0.100000000000000005551115
							6227     FCFPLEDGES_SIM  0.200000000000000011102230
							6227   FCFPLIVETREES_DD  0.400000000000000022204460
							6227  FCFPLIVETREES_LIT  0.100000000000000005551115
							6227  FCFPLIVETREES_SIM  0.400000000000000022204460
							6227    FCFPOVERHANG_DD  0.599999999999999977795540
							6227   FCFPOVERHANG_LIT  0.599999999999999977795540
							6227   FCFPOVERHANG_SIM  0.599999999999999977795540
							6227       FCFPSNAGS_DD  0.000000000000000000000000
							6227      FCFPSNAGS_LIT  0.000000000000000000000000
							6227      FCFPSNAGS_SIM  0.000000000000000000000000
							6227  FCFPSTRUCTURES_DD  0.000000000000000000000000
							6227 FCFPSTRUCTURES_LIT  0.100000000000000005551115
							6227 FCFPSTRUCTURES_SIM  0.000000000000000000000000
							6235    FCFPAQUATIC_LIT  0.888888888888879957761446
							6235    FCFPAQUATIC_SIM                          NA
#							6235   FCFPBOULDERS_SIM                          NA
							6235      FCFPBRUSH_LIT  1.000000000000000000000000
							6235      FCFPBRUSH_SIM                          NA
							6235     FCFPLEDGES_LIT  1.000000000000000000000000
							6235     FCFPLEDGES_SIM                          NA
							6235  FCFPLIVETREES_LIT  0.111111111111109994720181
							6235  FCFPLIVETREES_SIM                          NA
							6235   FCFPOVERHANG_LIT  1.000000000000000000000000
							6235   FCFPOVERHANG_SIM                          NA
							6235      FCFPSNAGS_LIT  0.222222222222219989440362
							6235      FCFPSNAGS_SIM                          NA
							6235 FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6235 FCFPSTRUCTURES_SIM                          NA
							6281     FCFPAQUATIC_DD  0.000000000000000000000000
							6281    FCFPAQUATIC_LIT  0.000000000000000000000000
							6281    FCFPAQUATIC_SIM  0.000000000000000000000000
							6281    FCFPBOULDERS_DD  0.000000000000000000000000
							6281   FCFPBOULDERS_LIT  0.444444444444439978880723
							6281   FCFPBOULDERS_SIM  0.111111111111109994720181
							6281       FCFPBRUSH_DD  0.000000000000000000000000
							6281      FCFPBRUSH_LIT  0.000000000000000000000000
							6281      FCFPBRUSH_SIM  0.000000000000000000000000
							6281      FCFPLEDGES_DD  0.000000000000000000000000
							6281     FCFPLEDGES_LIT  0.000000000000000000000000
							6281     FCFPLEDGES_SIM  0.000000000000000000000000
							6281   FCFPLIVETREES_DD  0.000000000000000000000000
							6281  FCFPLIVETREES_LIT  0.000000000000000000000000
							6281  FCFPLIVETREES_SIM  0.000000000000000000000000
							6281    FCFPOVERHANG_DD  0.000000000000000000000000
							6281   FCFPOVERHANG_LIT  0.000000000000000000000000
							6281   FCFPOVERHANG_SIM  0.000000000000000000000000
							6281       FCFPSNAGS_DD  0.000000000000000000000000
							6281      FCFPSNAGS_LIT  0.000000000000000000000000
							6281      FCFPSNAGS_SIM  0.000000000000000000000000
							6281  FCFPSTRUCTURES_DD  0.000000000000000000000000
							6281 FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6281 FCFPSTRUCTURES_SIM  0.000000000000000000000000
							6449     FCFPAQUATIC_DD  0.583333333333330039671694
							6449    FCFPAQUATIC_LIT  1.000000000000000000000000
							6449    FCFPAQUATIC_SIM  1.000000000000000000000000
							6449    FCFPBOULDERS_DD  0.583333333333330039671694
							6449   FCFPBOULDERS_LIT  0.166666666666659996076660
							6449   FCFPBOULDERS_SIM  0.583333333333330039671694
							6449       FCFPBRUSH_DD  0.750000000000000000000000
							6449      FCFPBRUSH_LIT  0.583333333333330039671694
							6449      FCFPBRUSH_SIM  0.750000000000000000000000
							6449      FCFPLEDGES_DD  0.166666666666659996076660
							6449     FCFPLEDGES_LIT  0.083333333333329998038330
							6449     FCFPLEDGES_SIM  0.166666666666659996076660
							6449   FCFPLIVETREES_DD  0.083333333333329998038330
							6449  FCFPLIVETREES_LIT  0.083333333333329998038330
							6449  FCFPLIVETREES_SIM  0.083333333333329998038330
							6449    FCFPOVERHANG_DD  0.363636363636359982720592
							6449   FCFPOVERHANG_LIT  0.333333333333329984160542
							6449   FCFPOVERHANG_SIM  0.454545454545449978400740
							6449       FCFPSNAGS_DD  0.000000000000000000000000
							6449      FCFPSNAGS_LIT  0.000000000000000000000000
							6449      FCFPSNAGS_SIM  0.000000000000000000000000
							6449  FCFPSTRUCTURES_DD  0.666666666666659968321085
							6449 FCFPSTRUCTURES_LIT  0.583333333333330039671694
							6449 FCFPSTRUCTURES_SIM  0.666666666666659968321085
							6683     FCFPAQUATIC_DD  0.000000000000000000000000
							6683    FCFPAQUATIC_LIT  1.000000000000000000000000
							6683    FCFPAQUATIC_SIM  0.000000000000000000000000
							6683    FCFPBOULDERS_DD  0.000000000000000000000000
							6683   FCFPBOULDERS_LIT  0.000000000000000000000000
							6683   FCFPBOULDERS_SIM  0.000000000000000000000000
							6683       FCFPBRUSH_DD  0.000000000000000000000000
							6683      FCFPBRUSH_LIT  0.000000000000000000000000
							6683      FCFPBRUSH_SIM  0.000000000000000000000000
							6683      FCFPLEDGES_DD  0.000000000000000000000000
							6683     FCFPLEDGES_LIT  0.000000000000000000000000
							6683     FCFPLEDGES_SIM  0.000000000000000000000000
							6683   FCFPLIVETREES_DD  0.000000000000000000000000
							6683  FCFPLIVETREES_LIT  0.000000000000000000000000
							6683  FCFPLIVETREES_SIM  0.000000000000000000000000
							6683    FCFPOVERHANG_DD  0.000000000000000000000000
							6683   FCFPOVERHANG_LIT  0.000000000000000000000000
							6683   FCFPOVERHANG_SIM  0.000000000000000000000000
							6683       FCFPSNAGS_DD  0.000000000000000000000000
							6683      FCFPSNAGS_LIT  0.000000000000000000000000
							6683      FCFPSNAGS_SIM  0.000000000000000000000000
							6683  FCFPSTRUCTURES_DD  0.000000000000000000000000
							6683 FCFPSTRUCTURES_LIT  0.000000000000000000000000
							6683 FCFPSTRUCTURES_SIM  0.000000000000000000000000
							7263     FCFPAQUATIC_DD  1.000000000000000000000000
							7263    FCFPAQUATIC_LIT  1.000000000000000000000000
							7263    FCFPAQUATIC_SIM  1.000000000000000000000000
							7263   FCFPBOULDERS_LIT  0.599999999999999977795540
							7263   FCFPBOULDERS_SIM                          NA
							7263      FCFPBRUSH_LIT  0.599999999999999977795540
							7263      FCFPBRUSH_SIM                          NA
							7263     FCFPLEDGES_LIT  0.100000000000000005551115
							7263     FCFPLEDGES_SIM                          NA
							7263  FCFPLIVETREES_LIT  0.000000000000000000000000
							7263  FCFPLIVETREES_SIM                          NA
							7263   FCFPOVERHANG_LIT  0.699999999999999955591079
							7263   FCFPOVERHANG_SIM                          NA
							7263       FCFPSNAGS_DD  0.000000000000000000000000
							7263      FCFPSNAGS_LIT  0.200000000000000011102230
							7263      FCFPSNAGS_SIM  0.000000000000000000000000
							7263 FCFPSTRUCTURES_LIT  0.100000000000000005551115
							7263 FCFPSTRUCTURES_SIM                          NA
							7913    FCFPAQUATIC_LIT  1.000000000000000000000000
							7913    FCFPAQUATIC_SIM                          NA
							7913   FCFPBOULDERS_LIT  1.000000000000000000000000
							7913   FCFPBOULDERS_SIM                          NA
							7913      FCFPBRUSH_LIT  1.000000000000000000000000
							7913      FCFPBRUSH_SIM                          NA
#							7913     FCFPLEDGES_SIM                          NA
#							7913  FCFPLIVETREES_SIM                          NA
							7913   FCFPOVERHANG_LIT  1.000000000000000000000000
							7913   FCFPOVERHANG_SIM                          NA
							7913      FCFPSNAGS_LIT  0.000000000000000000000000
							7913      FCFPSNAGS_SIM                          NA
#							7913 FCFPSTRUCTURES_SIM                          NA
							1000057    FCFPAQUATIC_LIT  1.000000000000000000000000
							1000057    FCFPAQUATIC_SIM                          NA
							1000057   FCFPBOULDERS_LIT  1.000000000000000000000000
							1000057   FCFPBOULDERS_SIM                          NA
							1000057      FCFPBRUSH_LIT  1.000000000000000000000000
							1000057      FCFPBRUSH_SIM                          NA
							1000057     FCFPLEDGES_LIT  1.000000000000000000000000
							1000057     FCFPLEDGES_SIM                          NA
							1000057  FCFPLIVETREES_LIT  0.000000000000000000000000
							1000057  FCFPLIVETREES_SIM                          NA
							1000057   FCFPOVERHANG_LIT  1.000000000000000000000000
							1000057   FCFPOVERHANG_SIM                          NA
							1000057      FCFPSNAGS_LIT  0.666666666666659968321085
							1000057      FCFPSNAGS_SIM                          NA
#							1000057 FCFPSTRUCTURES_SIM                          NA
							6160     FCFCAQUATIC_DD  0.257500000000000006661338
							6160    FCFCAQUATIC_LIT  0.110000000000000000555112
							6160    FCFCAQUATIC_SIM  0.213150000000000006128431
							6160    FCFCBOULDERS_DD  0.000000000000000000000000
							6160   FCFCBOULDERS_LIT  0.000000000000000000000000
							6160   FCFCBOULDERS_SIM  0.000000000000000000000000
							6160       FCFCBRUSH_DD  0.127500000000000002220446
							6160      FCFCBRUSH_LIT  0.035000000000000003330669
							6160      FCFCBRUSH_SIM  0.100849999999999995203837
							6160      FCFCLEDGES_DD  0.000000000000000000000000
							6160     FCFCLEDGES_LIT  0.057500000000000002498002
							6160     FCFCLEDGES_SIM  0.021850000000000001365574
							6160   FCFCLIVETREES_DD  0.000000000000000000000000
							6160  FCFCLIVETREES_LIT  0.005000000000000000104083
							6160  FCFCLIVETREES_SIM  0.002850000000000000102696
							6160    FCFCOVERHANG_DD  0.059999999999999997779554
							6160   FCFCOVERHANG_LIT  0.005000000000000000104083
							6160   FCFCOVERHANG_SIM  0.034700000000000001676437
							6160       FCFCSNAGS_DD  0.005000000000000000104083
							6160      FCFCSNAGS_LIT  0.000000000000000000000000
							6160      FCFCSNAGS_SIM  0.002150000000000000001388
							6160  FCFCSTRUCTURES_DD  0.025000000000000001387779
							6160 FCFCSTRUCTURES_LIT  0.025000000000000001387779
							6160 FCFCSTRUCTURES_SIM  0.025000000000000001387779
							6189    FCFCAQUATIC_LIT  0.412499999999999977795540
							6189    FCFCAQUATIC_SIM                          NA
							6189   FCFCBOULDERS_LIT  0.142499999999999987787547
							6189   FCFCBOULDERS_SIM                          NA
							6189      FCFCBRUSH_LIT  0.065000000000000002220446
							6189      FCFCBRUSH_SIM                          NA
							6189     FCFCLEDGES_LIT  0.014999999999999999444888
							6189     FCFCLEDGES_SIM                          NA
							6189  FCFCLIVETREES_LIT  0.000000000000000000000000
							6189  FCFCLIVETREES_SIM                          NA
							6189   FCFCOVERHANG_LIT  0.089999999999999996669331
							6189   FCFCOVERHANG_SIM                          NA
							6189      FCFCSNAGS_LIT  0.000000000000000000000000
							6189      FCFCSNAGS_SIM                          NA
							6189 FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6189 FCFCSTRUCTURES_SIM                          NA
							6227     FCFCAQUATIC_DD  0.059999999999999997779554
							6227    FCFCAQUATIC_LIT  0.354999999999999982236432
							6227    FCFCAQUATIC_SIM  0.115549999999999999933387
							6227    FCFCBOULDERS_DD  0.115000000000000004996004
							6227   FCFCBOULDERS_LIT  0.057500000000000002498002
							6227   FCFCBOULDERS_SIM  0.115000000000000004996004
							6227       FCFCBRUSH_DD  0.020000000000000000416334
							6227      FCFCBRUSH_LIT  0.010000000000000000208167
							6227      FCFCBRUSH_SIM  0.004400000000000000265066
							6227      FCFCLEDGES_DD  0.010000000000000000208167
							6227     FCFCLEDGES_LIT  0.005000000000000000104083
							6227     FCFCLEDGES_SIM  0.010000000000000000208167
							6227   FCFCLIVETREES_DD  0.059999999999999997779554
							6227  FCFCLIVETREES_LIT  0.025000000000000001387779
							6227  FCFCLIVETREES_SIM  0.052400000000000002131628
							6227    FCFCOVERHANG_DD  0.070000000000000006661338
							6227   FCFCOVERHANG_LIT  0.130000000000000004440892
							6227   FCFCOVERHANG_SIM  0.090800000000000005706546
							6227       FCFCSNAGS_DD  0.000000000000000000000000
							6227      FCFCSNAGS_LIT  0.000000000000000000000000
							6227      FCFCSNAGS_SIM  0.000000000000000000000000
							6227  FCFCSTRUCTURES_DD  0.000000000000000000000000
							6227 FCFCSTRUCTURES_LIT  0.005000000000000000104083
							6227 FCFCSTRUCTURES_SIM  0.000000000000000000000000
							6235    FCFCAQUATIC_LIT  0.044444444444444397568361
							6235    FCFCAQUATIC_SIM                          NA
#							6235   FCFCBOULDERS_SIM                          NA
							6235      FCFCBRUSH_LIT  0.072222222222219994991477
							6235      FCFCBRUSH_SIM                          NA
							6235     FCFCLEDGES_LIT  0.602777777777769974143496
							6235     FCFCLEDGES_SIM                          NA
							6235  FCFCLIVETREES_LIT  0.005555555555555549696045
							6235  FCFCLIVETREES_SIM                          NA
							6235   FCFCOVERHANG_LIT  0.130555555555550012458710
							6235   FCFCOVERHANG_SIM                          NA
							6235      FCFCSNAGS_LIT  0.011111111111111099392090
							6235      FCFCSNAGS_SIM                          NA
							6235 FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6235 FCFCSTRUCTURES_SIM                          NA
							6281     FCFCAQUATIC_DD  0.000000000000000000000000
							6281    FCFCAQUATIC_LIT  0.000000000000000000000000
							6281    FCFCAQUATIC_SIM  0.000000000000000000000000
							6281    FCFCBOULDERS_DD  0.000000000000000000000000
							6281   FCFCBOULDERS_LIT  0.022222222222222198784181
							6281   FCFCBOULDERS_SIM  0.002222222222222220051890
							6281       FCFCBRUSH_DD  0.000000000000000000000000
							6281      FCFCBRUSH_LIT  0.000000000000000000000000
							6281      FCFCBRUSH_SIM  0.000000000000000000000000
							6281      FCFCLEDGES_DD  0.000000000000000000000000
							6281     FCFCLEDGES_LIT  0.000000000000000000000000
							6281     FCFCLEDGES_SIM  0.000000000000000000000000
							6281   FCFCLIVETREES_DD  0.000000000000000000000000
							6281  FCFCLIVETREES_LIT  0.000000000000000000000000
							6281  FCFCLIVETREES_SIM  0.000000000000000000000000
							6281    FCFCOVERHANG_DD  0.000000000000000000000000
							6281   FCFCOVERHANG_LIT  0.000000000000000000000000
							6281   FCFCOVERHANG_SIM  0.000000000000000000000000
							6281       FCFCSNAGS_DD  0.000000000000000000000000
							6281      FCFCSNAGS_LIT  0.000000000000000000000000
							6281      FCFCSNAGS_SIM  0.000000000000000000000000
							6281  FCFCSTRUCTURES_DD  0.000000000000000000000000
							6281 FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6281 FCFCSTRUCTURES_SIM  0.000000000000000000000000
							6449     FCFCAQUATIC_DD  0.045833333333333302339607
							6449    FCFCAQUATIC_LIT  0.050000000000000002775558
							6449    FCFCAQUATIC_SIM  0.049750000000000002553513
							6449    FCFCBOULDERS_DD  0.089583333333330003589445
							6449   FCFCBOULDERS_LIT  0.008333333333333300257939
							6449   FCFCBOULDERS_SIM  0.011020833333333299175472
							6449       FCFCBRUSH_DD  0.070833333333330000813888
							6449      FCFCBRUSH_LIT  0.029166666666666601209768
							6449      FCFCBRUSH_SIM  0.030833333333333299425272
							6449      FCFCLEDGES_DD  0.008333333333333300257939
							6449     FCFCLEDGES_LIT  0.004166666666666659669949
							6449     FCFCLEDGES_SIM  0.004375000000000000416334
							6449   FCFCLIVETREES_DD  0.020833333333333300951828
							6449  FCFCLIVETREES_LIT  0.004166666666666659669949
							6449  FCFCLIVETREES_SIM  0.004499999999999999659994
							6449    FCFCOVERHANG_DD  0.102272727272719993196759
							6449   FCFCOVERHANG_LIT  0.033333333333333298176271
							6449   FCFCOVERHANG_SIM  0.039681818181818102886371
							6449       FCFCSNAGS_DD  0.000000000000000000000000
							6449      FCFCSNAGS_LIT  0.000000000000000000000000
							6449      FCFCSNAGS_SIM  0.000000000000000000000000
							6449  FCFCSTRUCTURES_DD  0.033333333333333298176271
							6449 FCFCSTRUCTURES_LIT  0.029166666666666601209768
							6449 FCFCSTRUCTURES_SIM  0.029374999999999998334665
							6683     FCFCAQUATIC_DD  0.000000000000000000000000
							6683    FCFCAQUATIC_LIT  0.875000000000000000000000
							6683    FCFCAQUATIC_SIM  0.000000000000000000000000
							6683    FCFCBOULDERS_DD  0.000000000000000000000000
							6683   FCFCBOULDERS_LIT  0.000000000000000000000000
							6683   FCFCBOULDERS_SIM  0.000000000000000000000000
							6683       FCFCBRUSH_DD  0.000000000000000000000000
							6683      FCFCBRUSH_LIT  0.000000000000000000000000
							6683      FCFCBRUSH_SIM  0.000000000000000000000000
							6683      FCFCLEDGES_DD  0.000000000000000000000000
							6683     FCFCLEDGES_LIT  0.000000000000000000000000
							6683     FCFCLEDGES_SIM  0.000000000000000000000000
							6683   FCFCLIVETREES_DD  0.000000000000000000000000
							6683  FCFCLIVETREES_LIT  0.000000000000000000000000
							6683  FCFCLIVETREES_SIM  0.000000000000000000000000
							6683    FCFCOVERHANG_DD  0.000000000000000000000000
							6683   FCFCOVERHANG_LIT  0.000000000000000000000000
							6683   FCFCOVERHANG_SIM  0.000000000000000000000000
							6683       FCFCSNAGS_DD  0.000000000000000000000000
							6683      FCFCSNAGS_LIT  0.000000000000000000000000
							6683      FCFCSNAGS_SIM  0.000000000000000000000000
							6683  FCFCSTRUCTURES_DD  0.000000000000000000000000
							6683 FCFCSTRUCTURES_LIT  0.000000000000000000000000
							6683 FCFCSTRUCTURES_SIM  0.000000000000000000000000
							7263     FCFCAQUATIC_DD  0.050000000000000002775558
							7263    FCFCAQUATIC_LIT  0.050000000000000002775558
							7263    FCFCAQUATIC_SIM  0.050000000000000002775558
							7263   FCFCBOULDERS_LIT  0.070000000000000006661338
							7263   FCFCBOULDERS_SIM                          NA
							7263      FCFCBRUSH_LIT  0.029999999999999998889777
							7263      FCFCBRUSH_SIM                          NA
							7263     FCFCLEDGES_LIT  0.005000000000000000104083
							7263     FCFCLEDGES_SIM                          NA
							7263  FCFCLIVETREES_LIT  0.000000000000000000000000
							7263  FCFCLIVETREES_SIM                          NA
							7263   FCFCOVERHANG_LIT  0.035000000000000003330669
							7263   FCFCOVERHANG_SIM                          NA
							7263       FCFCSNAGS_DD  0.000000000000000000000000
							7263      FCFCSNAGS_LIT  0.010000000000000000208167
							7263      FCFCSNAGS_SIM  0.000000000000000000000000
							7263 FCFCSTRUCTURES_LIT  0.005000000000000000104083
							7263 FCFCSTRUCTURES_SIM                          NA
							7913    FCFCAQUATIC_LIT  0.647499999999999964472863
							7913    FCFCAQUATIC_SIM                          NA
							7913   FCFCBOULDERS_LIT  0.050000000000000002775558
							7913   FCFCBOULDERS_SIM                          NA
							7913      FCFCBRUSH_LIT  0.050000000000000002775558
							7913      FCFCBRUSH_SIM                          NA
#							7913     FCFCLEDGES_SIM                          NA
#							7913  FCFCLIVETREES_SIM                          NA
							7913   FCFCOVERHANG_LIT  0.050000000000000002775558
							7913   FCFCOVERHANG_SIM                          NA
							7913      FCFCSNAGS_LIT  0.000000000000000000000000
							7913      FCFCSNAGS_SIM                          NA
#							7913 FCFCSTRUCTURES_SIM                          NA
							1000057    FCFCAQUATIC_LIT  0.308333333333330017467233
							1000057    FCFCAQUATIC_SIM                          NA
							1000057   FCFCBOULDERS_LIT  0.100000000000000005551115
							1000057   FCFCBOULDERS_SIM                          NA
							1000057      FCFCBRUSH_LIT  0.050000000000000002775558
							1000057      FCFCBRUSH_SIM                          NA
							1000057     FCFCLEDGES_LIT  0.250000000000000000000000
							1000057     FCFCLEDGES_SIM                          NA
							1000057  FCFCLIVETREES_LIT  0.000000000000000000000000
							1000057  FCFCLIVETREES_SIM                          NA
							1000057   FCFCOVERHANG_LIT  0.050000000000000002775558
							1000057   FCFCOVERHANG_SIM                          NA
							1000057      FCFCSNAGS_LIT  0.033333333333333298176271
							1000057      FCFCSNAGS_SIM                          NA
#							1000057 FCFCSTRUCTURES_SIM                          NA
							6160      FCVAQUATIC_DD  0.305743628987860005530308
							6160     FCVAQUATIC_LIT  0.096609178307919998940356
							6160     FCVAQUATIC_SIM  0.201583977537890007836197
							6160     FCVBOULDERS_DD  0.000000000000000000000000
							6160    FCVBOULDERS_LIT  0.000000000000000000000000
							6160    FCVBOULDERS_SIM  0.000000000000000000000000
							6160        FCVBRUSH_DD  0.263114360434139993305536
							6160       FCVBRUSH_LIT  0.024152294576982400592380
							6160       FCVBRUSH_SIM  0.185744754673949991063964
							6160       FCVLEDGES_DD  0.000000000000000000000000
							6160      FCVLEDGES_LIT  0.181830965459679999574050
							6160      FCVLEDGES_SIM  0.069095766874670005996961
							6160    FCVLIVETREES_DD  0.000000000000000000000000
							6160   FCVLIVETREES_LIT  0.015811388300841899140492
							6160   FCVLIVETREES_SIM  0.009012491331479799416826
							6160     FCVOVERHANG_DD  0.069920589878010003248576
							6160    FCVOVERHANG_LIT  0.015811388300841899140492
							6160    FCVOVERHANG_SIM  0.055472816165517802033946
							6160        FCVSNAGS_DD  0.015811388300841899140492
							6160       FCVSNAGS_LIT  0.000000000000000000000000
							6160       FCVSNAGS_SIM  0.006798896969362010385407
							6160   FCVSTRUCTURES_DD  0.079056941504200006765046
							6160  FCVSTRUCTURES_LIT  0.079056941504200006765046
							6160  FCVSTRUCTURES_SIM  0.079056941504200006765046
							6189     FCVAQUATIC_LIT  0.171290039925780013252421
							6189     FCVAQUATIC_SIM                          NA
							6189    FCVBOULDERS_LIT  0.188580221656459989798549
							6189    FCVBOULDERS_SIM                          NA
							6189       FCVBRUSH_LIT  0.066874675492459997871997
							6189       FCVBRUSH_SIM                          NA
							6189      FCVLEDGES_LIT  0.024152294576982400592380
							6189      FCVLEDGES_SIM                          NA
							6189   FCVLIVETREES_LIT  0.000000000000000000000000
							6189   FCVLIVETREES_SIM                          NA
							6189    FCVOVERHANG_LIT  0.084327404271149999925861
							6189    FCVOVERHANG_SIM                          NA
							6189       FCVSNAGS_LIT  0.000000000000000000000000
							6189       FCVSNAGS_SIM                          NA
							6189  FCVSTRUCTURES_LIT  0.000000000000000000000000
							6189  FCVSTRUCTURES_SIM                          NA
							6227      FCVAQUATIC_DD  0.108397416943390001442538
							6227     FCVAQUATIC_LIT  0.284507371511599993496588
							6227     FCVAQUATIC_SIM  0.205018627202499997830287
							6227     FCVBOULDERS_DD  0.257147817412470003350933
							6227    FCVBOULDERS_LIT  0.181830965459679999574050
							6227    FCVBOULDERS_SIM  0.257147817412470003350933
							6227        FCVBRUSH_DD  0.027386127875258299557704
							6227       FCVBRUSH_LIT  0.021081851067789200010472
							6227       FCVBRUSH_SIM  0.006066300355241240234438
							6227       FCVLEDGES_DD  0.022360679774997900548517
							6227      FCVLEDGES_LIT  0.015811388300841899140492
							6227      FCVLEDGES_SIM  0.022360679774997900548517
							6227    FCVLIVETREES_DD  0.108397416943390001442538
							6227   FCVLIVETREES_LIT  0.079056941504200006765046
							6227   FCVLIVETREES_SIM  0.110583904796310000806514
							6227     FCVOVERHANG_DD  0.103682206766629994576512
							6227    FCVOVERHANG_LIT  0.127366487830280011861461
							6227    FCVOVERHANG_SIM  0.120711225658589993425807
							6227        FCVSNAGS_DD  0.000000000000000000000000
							6227       FCVSNAGS_LIT  0.000000000000000000000000
							6227       FCVSNAGS_SIM  0.000000000000000000000000
							6227   FCVSTRUCTURES_DD  0.000000000000000000000000
							6227  FCVSTRUCTURES_LIT  0.015811388300841899140492
							6227  FCVSTRUCTURES_SIM  0.000000000000000000000000
							6235     FCVAQUATIC_LIT  0.016666666666666600515878
							6235     FCVAQUATIC_SIM                          NA
#							6235    FCVBOULDERS_SIM                          NA
							6235       FCVBRUSH_LIT  0.066666666666660004403333
							6235       FCVBRUSH_SIM                          NA
							6235      FCVLEDGES_LIT  0.243491672228109995446133
							6235      FCVLEDGES_SIM                          NA
							6235   FCVLIVETREES_LIT  0.016666666666666600515878
							6235   FCVLIVETREES_SIM                          NA
							6235    FCVOVERHANG_LIT  0.179311956594579990920124
							6235    FCVOVERHANG_SIM                          NA
							6235       FCVSNAGS_LIT  0.022047927592204901586781
							6235       FCVSNAGS_SIM                          NA
							6235  FCVSTRUCTURES_LIT  0.000000000000000000000000
							6235  FCVSTRUCTURES_SIM                          NA
							6281      FCVAQUATIC_DD  0.000000000000000000000000
							6281     FCVAQUATIC_LIT  0.000000000000000000000000
							6281     FCVAQUATIC_SIM  0.000000000000000000000000
							6281     FCVBOULDERS_DD  0.000000000000000000000000
							6281    FCVBOULDERS_LIT  0.026352313834736500880451
							6281    FCVBOULDERS_SIM  0.006666666666666660155671
							6281        FCVBRUSH_DD  0.000000000000000000000000
							6281       FCVBRUSH_LIT  0.000000000000000000000000
							6281       FCVBRUSH_SIM  0.000000000000000000000000
							6281       FCVLEDGES_DD  0.000000000000000000000000
							6281      FCVLEDGES_LIT  0.000000000000000000000000
							6281      FCVLEDGES_SIM  0.000000000000000000000000
							6281    FCVLIVETREES_DD  0.000000000000000000000000
							6281   FCVLIVETREES_LIT  0.000000000000000000000000
							6281   FCVLIVETREES_SIM  0.000000000000000000000000
							6281     FCVOVERHANG_DD  0.000000000000000000000000
							6281    FCVOVERHANG_LIT  0.000000000000000000000000
							6281    FCVOVERHANG_SIM  0.000000000000000000000000
							6281        FCVSNAGS_DD  0.000000000000000000000000
							6281       FCVSNAGS_LIT  0.000000000000000000000000
							6281       FCVSNAGS_SIM  0.000000000000000000000000
							6281   FCVSTRUCTURES_DD  0.000000000000000000000000
							6281  FCVSTRUCTURES_LIT  0.000000000000000000000000
							6281  FCVSTRUCTURES_SIM  0.000000000000000000000000
							6449      FCVAQUATIC_DD  0.068947718445120001473647
							6449     FCVAQUATIC_LIT  0.000000000000000000000000
							6449     FCVAQUATIC_SIM  0.001630671919513820107214
							6449     FCVBOULDERS_DD  0.167690981127530003291781
							6449    FCVBOULDERS_LIT  0.019462473604038001140371
							6449    FCVBOULDERS_SIM  0.022415107104699499157574
							6449        FCVBRUSH_DD  0.086493124617279995569596
							6449       FCVBRUSH_LIT  0.025746432527221800590223
							6449       FCVBRUSH_SIM  0.026731266450742700163667
							6449       FCVLEDGES_DD  0.019462473604038001140371
							6449      FCVLEDGES_LIT  0.014433756729740600255329
							6449      FCVLEDGES_SIM  0.014386112305717799544902
							6449    FCVLIVETREES_DD  0.072168783648699996735587
							6449   FCVLIVETREES_LIT  0.014433756729740600255329
							6449   FCVLIVETREES_SIM  0.015588457268119800674944
							6449     FCVOVERHANG_DD  0.185220998220549992741013
							6449    FCVOVERHANG_LIT  0.071774056256520002428090
							6449    FCVOVERHANG_SIM  0.077181368453550003305708
							6449        FCVSNAGS_DD  0.000000000000000000000000
							6449       FCVSNAGS_LIT  0.000000000000000000000000
							6449       FCVSNAGS_SIM  0.000000000000000000000000
							6449   FCVSTRUCTURES_DD  0.024618298195866500366336
							6449  FCVSTRUCTURES_LIT  0.025746432527221800590223
							6449  FCVSTRUCTURES_SIM  0.025497883156343499611030
							6683      FCVAQUATIC_DD                          NA
							6683     FCVAQUATIC_LIT                          NA
							6683     FCVAQUATIC_SIM                          NA
							6683     FCVBOULDERS_DD                          NA
							6683    FCVBOULDERS_LIT                          NA
							6683    FCVBOULDERS_SIM                          NA
							6683        FCVBRUSH_DD                          NA
							6683       FCVBRUSH_LIT                          NA
							6683       FCVBRUSH_SIM                          NA
							6683       FCVLEDGES_DD                          NA
							6683      FCVLEDGES_LIT                          NA
							6683      FCVLEDGES_SIM                          NA
							6683    FCVLIVETREES_DD                          NA
							6683   FCVLIVETREES_LIT                          NA
							6683   FCVLIVETREES_SIM                          NA
							6683     FCVOVERHANG_DD                          NA
							6683    FCVOVERHANG_LIT                          NA
							6683    FCVOVERHANG_SIM                          NA
							6683        FCVSNAGS_DD                          NA
							6683       FCVSNAGS_LIT                          NA
							6683       FCVSNAGS_SIM                          NA
							6683   FCVSTRUCTURES_DD                          NA
							6683  FCVSTRUCTURES_LIT                          NA
							6683  FCVSTRUCTURES_SIM                          NA
							7263      FCVAQUATIC_DD                          NA
							7263     FCVAQUATIC_LIT  0.000000000000000000000000
							7263     FCVAQUATIC_SIM                          NA
							7263    FCVBOULDERS_LIT  0.097752521990759996817211
							7263    FCVBOULDERS_SIM                          NA
							7263       FCVBRUSH_LIT  0.025819888974716098672957
							7263       FCVBRUSH_SIM                          NA
							7263      FCVLEDGES_LIT  0.015811388300841899140492
							7263      FCVLEDGES_SIM                          NA
							7263   FCVLIVETREES_LIT  0.000000000000000000000000
							7263   FCVLIVETREES_SIM                          NA
							7263    FCVOVERHANG_LIT  0.024152294576982400592380
							7263    FCVOVERHANG_SIM                          NA
							7263        FCVSNAGS_DD                          NA
							7263       FCVSNAGS_LIT  0.021081851067789200010472
							7263       FCVSNAGS_SIM                          NA
							7263  FCVSTRUCTURES_LIT  0.015811388300841899140492
							7263  FCVSTRUCTURES_SIM                          NA
							7913     FCVAQUATIC_LIT  0.370332058203619973735243
							7913     FCVAQUATIC_SIM                          NA
							7913    FCVBOULDERS_LIT  0.000000000000000000000000
							7913    FCVBOULDERS_SIM                          NA
							7913       FCVBRUSH_LIT  0.000000000000000000000000
							7913       FCVBRUSH_SIM                          NA
#							7913      FCVLEDGES_SIM                          NA
#							7913   FCVLIVETREES_SIM                          NA
							7913    FCVOVERHANG_LIT  0.000000000000000000000000
							7913    FCVOVERHANG_SIM                          NA
							7913       FCVSNAGS_LIT  0.000000000000000000000000
							7913       FCVSNAGS_SIM                          NA
#							7913  FCVSTRUCTURES_SIM                          NA
							1000057     FCVAQUATIC_LIT  0.345205252953459995612917
							1000057     FCVAQUATIC_SIM                          NA
							1000057    FCVBOULDERS_LIT  0.092582009977249996035553
							1000057    FCVBOULDERS_SIM                          NA
							1000057       FCVBRUSH_LIT  0.000000000000000000000000
							1000057       FCVBRUSH_SIM                          NA
							1000057      FCVLEDGES_LIT                          NA
							1000057      FCVLEDGES_SIM                          NA
							1000057   FCVLIVETREES_LIT                          NA
							1000057   FCVLIVETREES_SIM                          NA
							1000057    FCVOVERHANG_LIT  0.000000000000000000000000
							1000057    FCVOVERHANG_SIM                          NA
							1000057       FCVSNAGS_LIT  0.028867513459481200510659
							1000057       FCVSNAGS_SIM                          NA
#							1000057  FCVSTRUCTURES_SIM                          NA
							6160      FCNAQUATIC_DD 10.000000000000000000000000
							6160     FCNAQUATIC_LIT 10.000000000000000000000000
							6160     FCNAQUATIC_SIM 10.000000000000000000000000
							6160     FCNBOULDERS_DD 10.000000000000000000000000
							6160    FCNBOULDERS_LIT 10.000000000000000000000000
							6160    FCNBOULDERS_SIM 10.000000000000000000000000
							6160        FCNBRUSH_DD 10.000000000000000000000000
							6160       FCNBRUSH_LIT 10.000000000000000000000000
							6160       FCNBRUSH_SIM 10.000000000000000000000000
							6160       FCNLEDGES_DD 10.000000000000000000000000
							6160      FCNLEDGES_LIT 10.000000000000000000000000
							6160      FCNLEDGES_SIM 10.000000000000000000000000
							6160    FCNLIVETREES_DD 10.000000000000000000000000
							6160   FCNLIVETREES_LIT 10.000000000000000000000000
							6160   FCNLIVETREES_SIM 10.000000000000000000000000
							6160     FCNOVERHANG_DD 10.000000000000000000000000
							6160    FCNOVERHANG_LIT 10.000000000000000000000000
							6160    FCNOVERHANG_SIM 10.000000000000000000000000
							6160        FCNSNAGS_DD 10.000000000000000000000000
							6160       FCNSNAGS_LIT 10.000000000000000000000000
							6160       FCNSNAGS_SIM 10.000000000000000000000000
							6160   FCNSTRUCTURES_DD 10.000000000000000000000000
							6160  FCNSTRUCTURES_LIT 10.000000000000000000000000
							6160  FCNSTRUCTURES_SIM 10.000000000000000000000000
							6189      FCNAQUATIC_DD  0.000000000000000000000000
							6189     FCNAQUATIC_LIT 10.000000000000000000000000
							6189     FCNAQUATIC_SIM  0.000000000000000000000000
							6189     FCNBOULDERS_DD  0.000000000000000000000000
							6189    FCNBOULDERS_LIT 10.000000000000000000000000
							6189    FCNBOULDERS_SIM  0.000000000000000000000000
							6189        FCNBRUSH_DD  0.000000000000000000000000
							6189       FCNBRUSH_LIT 10.000000000000000000000000
							6189       FCNBRUSH_SIM  0.000000000000000000000000
							6189       FCNLEDGES_DD  0.000000000000000000000000
							6189      FCNLEDGES_LIT 10.000000000000000000000000
							6189      FCNLEDGES_SIM  0.000000000000000000000000
							6189    FCNLIVETREES_DD  0.000000000000000000000000
							6189   FCNLIVETREES_LIT 10.000000000000000000000000
							6189   FCNLIVETREES_SIM  0.000000000000000000000000
							6189     FCNOVERHANG_DD  0.000000000000000000000000
							6189    FCNOVERHANG_LIT 10.000000000000000000000000
							6189    FCNOVERHANG_SIM  0.000000000000000000000000
							6189        FCNSNAGS_DD  0.000000000000000000000000
							6189       FCNSNAGS_LIT 10.000000000000000000000000
							6189       FCNSNAGS_SIM  0.000000000000000000000000
							6189   FCNSTRUCTURES_DD  0.000000000000000000000000
							6189  FCNSTRUCTURES_LIT 10.000000000000000000000000
							6189  FCNSTRUCTURES_SIM  0.000000000000000000000000
							6227      FCNAQUATIC_DD  5.000000000000000000000000
							6227     FCNAQUATIC_LIT 10.000000000000000000000000
							6227     FCNAQUATIC_SIM  5.000000000000000000000000
							6227     FCNBOULDERS_DD  5.000000000000000000000000
							6227    FCNBOULDERS_LIT 10.000000000000000000000000
							6227    FCNBOULDERS_SIM  5.000000000000000000000000
							6227        FCNBRUSH_DD  5.000000000000000000000000
							6227       FCNBRUSH_LIT 10.000000000000000000000000
							6227       FCNBRUSH_SIM  5.000000000000000000000000
							6227       FCNLEDGES_DD  5.000000000000000000000000
							6227      FCNLEDGES_LIT 10.000000000000000000000000
							6227      FCNLEDGES_SIM  5.000000000000000000000000
							6227    FCNLIVETREES_DD  5.000000000000000000000000
							6227   FCNLIVETREES_LIT 10.000000000000000000000000
							6227   FCNLIVETREES_SIM  5.000000000000000000000000
							6227     FCNOVERHANG_DD  5.000000000000000000000000
							6227    FCNOVERHANG_LIT 10.000000000000000000000000
							6227    FCNOVERHANG_SIM  5.000000000000000000000000
							6227        FCNSNAGS_DD  5.000000000000000000000000
							6227       FCNSNAGS_LIT 10.000000000000000000000000
							6227       FCNSNAGS_SIM  5.000000000000000000000000
							6227   FCNSTRUCTURES_DD  5.000000000000000000000000
							6227  FCNSTRUCTURES_LIT 10.000000000000000000000000
							6227  FCNSTRUCTURES_SIM  5.000000000000000000000000
							6235      FCNAQUATIC_DD  0.000000000000000000000000
							6235     FCNAQUATIC_LIT  9.000000000000000000000000
							6235     FCNAQUATIC_SIM  0.000000000000000000000000
							6235     FCNBOULDERS_DD  0.000000000000000000000000
							6235    FCNBOULDERS_LIT  0.000000000000000000000000
							6235    FCNBOULDERS_SIM  0.000000000000000000000000
							6235        FCNBRUSH_DD  0.000000000000000000000000
							6235       FCNBRUSH_LIT  9.000000000000000000000000
							6235       FCNBRUSH_SIM  0.000000000000000000000000
							6235       FCNLEDGES_DD  0.000000000000000000000000
							6235      FCNLEDGES_LIT  9.000000000000000000000000
							6235      FCNLEDGES_SIM  0.000000000000000000000000
							6235    FCNLIVETREES_DD  0.000000000000000000000000
							6235   FCNLIVETREES_LIT  9.000000000000000000000000
							6235   FCNLIVETREES_SIM  0.000000000000000000000000
							6235     FCNOVERHANG_DD  0.000000000000000000000000
							6235    FCNOVERHANG_LIT  9.000000000000000000000000
							6235    FCNOVERHANG_SIM  0.000000000000000000000000
							6235        FCNSNAGS_DD  0.000000000000000000000000
							6235       FCNSNAGS_LIT  9.000000000000000000000000
							6235       FCNSNAGS_SIM  0.000000000000000000000000
							6235   FCNSTRUCTURES_DD  0.000000000000000000000000
							6235  FCNSTRUCTURES_LIT  9.000000000000000000000000
							6235  FCNSTRUCTURES_SIM  0.000000000000000000000000
							6281      FCNAQUATIC_DD  9.000000000000000000000000
							6281     FCNAQUATIC_LIT  9.000000000000000000000000
							6281     FCNAQUATIC_SIM  9.000000000000000000000000
							6281     FCNBOULDERS_DD  9.000000000000000000000000
							6281    FCNBOULDERS_LIT  9.000000000000000000000000
							6281    FCNBOULDERS_SIM  9.000000000000000000000000
							6281        FCNBRUSH_DD  9.000000000000000000000000
							6281       FCNBRUSH_LIT  9.000000000000000000000000
							6281       FCNBRUSH_SIM  9.000000000000000000000000
							6281       FCNLEDGES_DD  9.000000000000000000000000
							6281      FCNLEDGES_LIT  9.000000000000000000000000
							6281      FCNLEDGES_SIM  9.000000000000000000000000
							6281    FCNLIVETREES_DD  9.000000000000000000000000
							6281   FCNLIVETREES_LIT  9.000000000000000000000000
							6281   FCNLIVETREES_SIM  9.000000000000000000000000
							6281     FCNOVERHANG_DD  9.000000000000000000000000
							6281    FCNOVERHANG_LIT  9.000000000000000000000000
							6281    FCNOVERHANG_SIM  9.000000000000000000000000
							6281        FCNSNAGS_DD  9.000000000000000000000000
							6281       FCNSNAGS_LIT  9.000000000000000000000000
							6281       FCNSNAGS_SIM  9.000000000000000000000000
							6281   FCNSTRUCTURES_DD  9.000000000000000000000000
							6281  FCNSTRUCTURES_LIT  9.000000000000000000000000
							6281  FCNSTRUCTURES_SIM  9.000000000000000000000000
							6449      FCNAQUATIC_DD 12.000000000000000000000000
							6449     FCNAQUATIC_LIT 12.000000000000000000000000
							6449     FCNAQUATIC_SIM 12.000000000000000000000000
							6449     FCNBOULDERS_DD 12.000000000000000000000000
							6449    FCNBOULDERS_LIT 12.000000000000000000000000
							6449    FCNBOULDERS_SIM 12.000000000000000000000000
							6449        FCNBRUSH_DD 12.000000000000000000000000
							6449       FCNBRUSH_LIT 12.000000000000000000000000
							6449       FCNBRUSH_SIM 12.000000000000000000000000
							6449       FCNLEDGES_DD 12.000000000000000000000000
							6449      FCNLEDGES_LIT 12.000000000000000000000000
							6449      FCNLEDGES_SIM 12.000000000000000000000000
							6449    FCNLIVETREES_DD 12.000000000000000000000000
							6449   FCNLIVETREES_LIT 12.000000000000000000000000
							6449   FCNLIVETREES_SIM 12.000000000000000000000000
							6449     FCNOVERHANG_DD 11.000000000000000000000000
							6449    FCNOVERHANG_LIT 12.000000000000000000000000
							6449    FCNOVERHANG_SIM 11.000000000000000000000000
							6449        FCNSNAGS_DD 12.000000000000000000000000
							6449       FCNSNAGS_LIT 12.000000000000000000000000
							6449       FCNSNAGS_SIM 12.000000000000000000000000
							6449   FCNSTRUCTURES_DD 12.000000000000000000000000
							6449  FCNSTRUCTURES_LIT 12.000000000000000000000000
							6449  FCNSTRUCTURES_SIM 12.000000000000000000000000
							6683      FCNAQUATIC_DD  1.000000000000000000000000
							6683     FCNAQUATIC_LIT  1.000000000000000000000000
							6683     FCNAQUATIC_SIM  1.000000000000000000000000
							6683     FCNBOULDERS_DD  1.000000000000000000000000
							6683    FCNBOULDERS_LIT  1.000000000000000000000000
							6683    FCNBOULDERS_SIM  1.000000000000000000000000
							6683        FCNBRUSH_DD  1.000000000000000000000000
							6683       FCNBRUSH_LIT  1.000000000000000000000000
							6683       FCNBRUSH_SIM  1.000000000000000000000000
							6683       FCNLEDGES_DD  1.000000000000000000000000
							6683      FCNLEDGES_LIT  1.000000000000000000000000
							6683      FCNLEDGES_SIM  1.000000000000000000000000
							6683    FCNLIVETREES_DD  1.000000000000000000000000
							6683   FCNLIVETREES_LIT  1.000000000000000000000000
							6683   FCNLIVETREES_SIM  1.000000000000000000000000
							6683     FCNOVERHANG_DD  1.000000000000000000000000
							6683    FCNOVERHANG_LIT  1.000000000000000000000000
							6683    FCNOVERHANG_SIM  1.000000000000000000000000
							6683        FCNSNAGS_DD  1.000000000000000000000000
							6683       FCNSNAGS_LIT  1.000000000000000000000000
							6683       FCNSNAGS_SIM  1.000000000000000000000000
							6683   FCNSTRUCTURES_DD  1.000000000000000000000000
							6683  FCNSTRUCTURES_LIT  1.000000000000000000000000
							6683  FCNSTRUCTURES_SIM  1.000000000000000000000000
							7263      FCNAQUATIC_DD  1.000000000000000000000000
							7263     FCNAQUATIC_LIT 10.000000000000000000000000
							7263     FCNAQUATIC_SIM  1.000000000000000000000000
							7263     FCNBOULDERS_DD  0.000000000000000000000000
							7263    FCNBOULDERS_LIT 10.000000000000000000000000
							7263    FCNBOULDERS_SIM  0.000000000000000000000000
							7263        FCNBRUSH_DD  0.000000000000000000000000
							7263       FCNBRUSH_LIT 10.000000000000000000000000
							7263       FCNBRUSH_SIM  0.000000000000000000000000
							7263       FCNLEDGES_DD  0.000000000000000000000000
							7263      FCNLEDGES_LIT 10.000000000000000000000000
							7263      FCNLEDGES_SIM  0.000000000000000000000000
							7263    FCNLIVETREES_DD  0.000000000000000000000000
							7263   FCNLIVETREES_LIT 10.000000000000000000000000
							7263   FCNLIVETREES_SIM  0.000000000000000000000000
							7263     FCNOVERHANG_DD  0.000000000000000000000000
							7263    FCNOVERHANG_LIT 10.000000000000000000000000
							7263    FCNOVERHANG_SIM  0.000000000000000000000000
							7263        FCNSNAGS_DD  1.000000000000000000000000
							7263       FCNSNAGS_LIT 10.000000000000000000000000
							7263       FCNSNAGS_SIM  1.000000000000000000000000
							7263   FCNSTRUCTURES_DD  0.000000000000000000000000
							7263  FCNSTRUCTURES_LIT 10.000000000000000000000000
							7263  FCNSTRUCTURES_SIM  0.000000000000000000000000
							7913      FCNAQUATIC_DD  0.000000000000000000000000
							7913     FCNAQUATIC_LIT 10.000000000000000000000000
							7913     FCNAQUATIC_SIM  0.000000000000000000000000
							7913     FCNBOULDERS_DD  0.000000000000000000000000
							7913    FCNBOULDERS_LIT  2.000000000000000000000000
							7913    FCNBOULDERS_SIM  0.000000000000000000000000
							7913        FCNBRUSH_DD  0.000000000000000000000000
							7913       FCNBRUSH_LIT  2.000000000000000000000000
							7913       FCNBRUSH_SIM  0.000000000000000000000000
							7913       FCNLEDGES_DD  0.000000000000000000000000
							7913      FCNLEDGES_LIT  0.000000000000000000000000
							7913      FCNLEDGES_SIM  0.000000000000000000000000
							7913    FCNLIVETREES_DD  0.000000000000000000000000
							7913   FCNLIVETREES_LIT  0.000000000000000000000000
							7913   FCNLIVETREES_SIM  0.000000000000000000000000
							7913     FCNOVERHANG_DD  0.000000000000000000000000
							7913    FCNOVERHANG_LIT  7.000000000000000000000000
							7913    FCNOVERHANG_SIM  0.000000000000000000000000
							7913        FCNSNAGS_DD  0.000000000000000000000000
							7913       FCNSNAGS_LIT  2.000000000000000000000000
							7913       FCNSNAGS_SIM  0.000000000000000000000000
							7913   FCNSTRUCTURES_DD  0.000000000000000000000000
							7913  FCNSTRUCTURES_LIT  0.000000000000000000000000
							7913  FCNSTRUCTURES_SIM  0.000000000000000000000000
							1000057      FCNAQUATIC_DD  0.000000000000000000000000
							1000057     FCNAQUATIC_LIT  6.000000000000000000000000
							1000057     FCNAQUATIC_SIM  0.000000000000000000000000
							1000057     FCNBOULDERS_DD  0.000000000000000000000000
							1000057    FCNBOULDERS_LIT  8.000000000000000000000000
							1000057    FCNBOULDERS_SIM  0.000000000000000000000000
							1000057        FCNBRUSH_DD  0.000000000000000000000000
							1000057       FCNBRUSH_LIT 10.000000000000000000000000
							1000057       FCNBRUSH_SIM  0.000000000000000000000000
							1000057       FCNLEDGES_DD  0.000000000000000000000000
							1000057      FCNLEDGES_LIT  1.000000000000000000000000
							1000057      FCNLEDGES_SIM  0.000000000000000000000000
							1000057    FCNLIVETREES_DD  0.000000000000000000000000
							1000057   FCNLIVETREES_LIT  1.000000000000000000000000
							1000057   FCNLIVETREES_SIM  0.000000000000000000000000
							1000057     FCNOVERHANG_DD  0.000000000000000000000000
							1000057    FCNOVERHANG_LIT 10.000000000000000000000000
							1000057    FCNOVERHANG_SIM  0.000000000000000000000000
							1000057        FCNSNAGS_DD  0.000000000000000000000000
							1000057       FCNSNAGS_LIT  3.000000000000000000000000
							1000057       FCNSNAGS_SIM  0.000000000000000000000000
							1000057   FCNSTRUCTURES_DD  0.000000000000000000000000
							1000057  FCNSTRUCTURES_LIT  0.000000000000000000000000
							1000057  FCNSTRUCTURES_SIM  0.000000000000000000000000
							6160      FCINATURAL_DD  0.450000000000000011102230
							6160     FCINATURAL_LIT  0.212499999999999994448885
							6160     FCINATURAL_SIM  0.375549999999999994937383
							6189     FCINATURAL_LIT  0.724999999999989985788318
							6189     FCINATURAL_SIM                          NA
							6227      FCINATURAL_DD  0.335000000000000019984014
							6227     FCINATURAL_LIT  0.582500000000000017763568
							6227     FCINATURAL_SIM  0.388149999999999995026201
							6235     FCINATURAL_LIT  0.866666666666660034934466
							6235     FCINATURAL_SIM                          NA
							6281      FCINATURAL_DD  0.000000000000000000000000
							6281     FCINATURAL_LIT  0.022222222222222198784181
							6281     FCINATURAL_SIM  0.002222222222222220051890
							6449      FCINATURAL_DD  0.337689393939390014587332
							6449     FCINATURAL_LIT  0.129166666666659990525545
							6449     FCINATURAL_SIM  0.140160984848479996944803
							6683      FCINATURAL_DD  0.000000000000000000000000
							6683     FCINATURAL_LIT  0.875000000000000000000000
							6683     FCINATURAL_SIM  0.000000000000000000000000
							7263      FCINATURAL_DD  0.050000000000000002775558
							7263     FCINATURAL_LIT  0.200000000000000011102230
							7263     FCINATURAL_SIM  0.050000000000000002775558
							7913     FCINATURAL_LIT  0.797499999999999986677324
							7913     FCINATURAL_SIM                          NA
							1000057     FCINATURAL_LIT  0.791666666666659968321085
							1000057     FCINATURAL_SIM                          NA
							6160          FCIALL_DD  0.474999999999999977795540
							6160         FCIALL_LIT  0.237499999999999988897770
							6160         FCIALL_SIM  0.400550000000000017141844
							6189         FCIALL_LIT  0.724999999999989985788318
							6189         FCIALL_SIM                          NA
							6227          FCIALL_DD  0.335000000000000019984014
							6227         FCIALL_LIT  0.587500000000000022204460
							6227         FCIALL_SIM  0.388149999999999995026201
							6235         FCIALL_LIT  0.866666666666660034934466
							6235         FCIALL_SIM                          NA
							6281          FCIALL_DD  0.000000000000000000000000
							6281         FCIALL_LIT  0.022222222222222198784181
							6281         FCIALL_SIM  0.002222222222222220051890
							6449          FCIALL_DD  0.371022727272720009850104
							6449         FCIALL_LIT  0.158333333333329995262773
							6449         FCIALL_SIM  0.169535984848480009157257
							6683          FCIALL_DD  0.000000000000000000000000
							6683         FCIALL_LIT  0.875000000000000000000000
							6683         FCIALL_SIM  0.000000000000000000000000
							7263          FCIALL_DD  0.050000000000000002775558
							7263         FCIALL_LIT  0.204999999999999987787547
							7263         FCIALL_SIM  0.050000000000000002775558
							7913         FCIALL_LIT  0.797499999999999986677324
							7913         FCIALL_SIM                          NA
							1000057         FCIALL_LIT  0.791666666666659968321085
							1000057         FCIALL_SIM                          NA
							6160          FCIBIG_DD  0.085000000000000006106227
							6160         FCIBIG_LIT  0.087499999999999994448885
							6160         FCIBIG_SIM  0.081549999999999997490896
							6189         FCIBIG_LIT  0.247499999999999997779554
							6189         FCIBIG_SIM                          NA
							6227          FCIBIG_DD  0.195000000000000006661338
							6227         FCIBIG_LIT  0.197500000000000008881784
							6227         FCIBIG_SIM  0.215799999999999991828759
							6235         FCIBIG_LIT  0.733333333333329950853852
							6235         FCIBIG_SIM                          NA
							6281          FCIBIG_DD  0.000000000000000000000000
							6281         FCIBIG_LIT  0.022222222222222198784181
							6281         FCIBIG_SIM  0.002222222222222220051890
							6449          FCIBIG_DD  0.233522727272719998747874
							6449         FCIBIG_LIT  0.074999999999999997224442
							6449         FCIBIG_SIM  0.084452651515149995686826
							6683          FCIBIG_DD  0.000000000000000000000000
							6683         FCIBIG_LIT  0.000000000000000000000000
							6683         FCIBIG_SIM  0.000000000000000000000000
							7263         FCIBIG_LIT  0.115000000000000004996004
							7263         FCIBIG_SIM                          NA
							7913         FCIBIG_LIT  0.100000000000000005551115
							7913         FCIBIG_SIM                          NA
							1000057         FCIBIG_LIT  0.400000000000000022204460
							1000057         FCIBIG_SIM                          NA
							6160       FCIRIPVEG_DD  0.132500000000000006661338
							6160      FCIRIPVEG_LIT  0.040000000000000000832667
							6160      FCIRIPVEG_SIM  0.105849999999999999644729
							6189      FCIRIPVEG_LIT  0.065000000000000002220446
							6189      FCIRIPVEG_SIM                          NA
							6227       FCIRIPVEG_DD  0.080000000000000001665335
							6227      FCIRIPVEG_LIT  0.035000000000000003330669
							6227      FCIRIPVEG_SIM  0.056800000000000003264056
							6235      FCIRIPVEG_LIT  0.088888888888879996619252
							6235      FCIRIPVEG_SIM                          NA
							6281       FCIRIPVEG_DD  0.000000000000000000000000
							6281      FCIRIPVEG_LIT  0.000000000000000000000000
							6281      FCIRIPVEG_SIM  0.000000000000000000000000
							6449       FCIRIPVEG_DD  0.091666666666659998852218
							6449      FCIRIPVEG_LIT  0.033333333333333298176271
							6449      FCIRIPVEG_SIM  0.035333333333333299952628
							6683       FCIRIPVEG_DD  0.000000000000000000000000
							6683      FCIRIPVEG_LIT  0.000000000000000000000000
							6683      FCIRIPVEG_SIM  0.000000000000000000000000
							7263       FCIRIPVEG_DD  0.000000000000000000000000
							7263      FCIRIPVEG_LIT  0.040000000000000000832667
							7263      FCIRIPVEG_SIM  0.000000000000000000000000
							7913      FCIRIPVEG_LIT  0.050000000000000002775558
							7913      FCIRIPVEG_SIM                          NA
							1000057      FCIRIPVEG_LIT  0.083333333333329998038330
							1000057      FCIRIPVEG_SIM                          NA
							6160          FCNALL_DD 10.000000000000000000000000
							6160         FCNALL_LIT 10.000000000000000000000000
							6160         FCNALL_SIM 10.000000000000000000000000
							6189         FCNALL_LIT 10.000000000000000000000000
							6189         FCNALL_SIM  0.000000000000000000000000
							6227          FCNALL_DD  5.000000000000000000000000
							6227         FCNALL_LIT 10.000000000000000000000000
							6227         FCNALL_SIM  5.000000000000000000000000
							6235         FCNALL_LIT  9.000000000000000000000000
							6235         FCNALL_SIM  0.000000000000000000000000
							6281          FCNALL_DD  9.000000000000000000000000
							6281         FCNALL_LIT  9.000000000000000000000000
							6281         FCNALL_SIM  9.000000000000000000000000
							6449          FCNALL_DD 12.000000000000000000000000
							6449         FCNALL_LIT 12.000000000000000000000000
							6449         FCNALL_SIM 12.000000000000000000000000
							6683          FCNALL_DD  1.000000000000000000000000
							6683         FCNALL_LIT  1.000000000000000000000000
							6683         FCNALL_SIM  1.000000000000000000000000
							7263          FCNALL_DD  1.000000000000000000000000
							7263         FCNALL_LIT 10.000000000000000000000000
							7263         FCNALL_SIM  1.000000000000000000000000
							7913         FCNALL_LIT 10.000000000000000000000000
							7913         FCNALL_SIM  0.000000000000000000000000
							1000057         FCNALL_LIT 10.000000000000000000000000
							1000057         FCNALL_SIM  0.000000000000000000000000
							6160         FCFPALL_DD  1.000000000000000000000000
							6160        FCFPALL_LIT  1.000000000000000000000000
							6160        FCFPALL_SIM  1.000000000000000000000000
							6189        FCFPALL_LIT  1.000000000000000000000000
							6189        FCFPALL_SIM                          NA
							6227         FCFPALL_DD  0.800000000000000044408921
							6227        FCFPALL_LIT  0.900000000000000022204460
							6227        FCFPALL_SIM  1.000000000000000000000000
							6235        FCFPALL_LIT  1.000000000000000000000000
							6235        FCFPALL_SIM                          NA
							6281         FCFPALL_DD  0.000000000000000000000000
							6281        FCFPALL_LIT  0.444444444444439978880723
							6281        FCFPALL_SIM  0.111111111111109994720181
							6449         FCFPALL_DD  1.000000000000000000000000
							6449        FCFPALL_LIT  1.000000000000000000000000
							6449        FCFPALL_SIM  1.000000000000000000000000
							6683         FCFPALL_DD  0.000000000000000000000000
							6683        FCFPALL_LIT  1.000000000000000000000000
							6683        FCFPALL_SIM  0.000000000000000000000000
							7263         FCFPALL_DD  1.000000000000000000000000
							7263        FCFPALL_LIT  1.000000000000000000000000
							7263        FCFPALL_SIM  1.000000000000000000000000
							7913        FCFPALL_LIT  1.000000000000000000000000
							7913        FCFPALL_SIM                          NA
							1000057        FCFPALL_LIT  1.000000000000000000000000
							1000057        FCFPALL_SIM                          NA")
		 
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
					
}



nlaFishCoverTest.expectedResultsWithDrawDownAndFillin <- function()
# Expected results for test data with drawdown values which were 'filled in' based
# on the value of the DRAWDOWN parameter.  These values were calculated with SAS on
# 26 Sept 2013, which erred for SITE 6235 by calculating a FCN*_DD values as 10 
# instead of 9 and by including a total of 15 missing littoral values (the R 
# function does not include those values) for6234, 7913 and 1000057.
{
	tc <- textConnection("  SITE             METRIC        VALUE
							6160     FCFPAQUATIC_DD  1.000000000
							6160    FCFPAQUATIC_LIT  1.000000000
							6160    FCFPAQUATIC_SIM  1.000000000
							6160    FCFPBOULDERS_DD  0.000000000
							6160   FCFPBOULDERS_LIT  0.000000000
							6160   FCFPBOULDERS_SIM  0.000000000
							6160       FCFPBRUSH_DD  0.900000000
							6160      FCFPBRUSH_LIT  0.700000000
							6160      FCFPBRUSH_SIM  0.900000000
							6160      FCFPLEDGES_DD  0.000000000
							6160     FCFPLEDGES_LIT  0.100000000
							6160     FCFPLEDGES_SIM  0.100000000
							6160   FCFPLIVETREES_DD  0.000000000
							6160  FCFPLIVETREES_LIT  0.100000000
							6160  FCFPLIVETREES_SIM  0.100000000
							6160    FCFPOVERHANG_DD  0.800000000
							6160   FCFPOVERHANG_LIT  0.100000000
							6160   FCFPOVERHANG_SIM  0.800000000
							6160       FCFPSNAGS_DD  0.100000000
							6160      FCFPSNAGS_LIT  0.000000000
							6160      FCFPSNAGS_SIM  0.100000000
							6160  FCFPSTRUCTURES_DD  0.100000000
							6160 FCFPSTRUCTURES_LIT  0.100000000
							6160 FCFPSTRUCTURES_SIM  0.100000000
							6189     FCFPAQUATIC_DD  0.000000000
							6189    FCFPAQUATIC_LIT  1.000000000
							6189    FCFPAQUATIC_SIM  1.000000000
							6189    FCFPBOULDERS_DD  0.000000000
							6189   FCFPBOULDERS_LIT  0.600000000
							6189   FCFPBOULDERS_SIM  0.600000000
							6189       FCFPBRUSH_DD  0.000000000
							6189      FCFPBRUSH_LIT  0.900000000
							6189      FCFPBRUSH_SIM  0.900000000
							6189      FCFPLEDGES_DD  0.000000000
							6189     FCFPLEDGES_LIT  0.300000000
							6189     FCFPLEDGES_SIM  0.300000000
							6189   FCFPLIVETREES_DD  0.000000000
							6189  FCFPLIVETREES_LIT  0.000000000
							6189  FCFPLIVETREES_SIM  0.000000000
							6189    FCFPOVERHANG_DD  0.000000000
							6189   FCFPOVERHANG_LIT  1.000000000
							6189   FCFPOVERHANG_SIM  1.000000000
							6189       FCFPSNAGS_DD  0.000000000
							6189      FCFPSNAGS_LIT  0.000000000
							6189      FCFPSNAGS_SIM  0.000000000
							6189  FCFPSTRUCTURES_DD  0.000000000
							6189 FCFPSTRUCTURES_LIT  0.000000000
							6189 FCFPSTRUCTURES_SIM  0.000000000
							6227     FCFPAQUATIC_DD  0.200000000
							6227    FCFPAQUATIC_LIT  0.800000000
							6227    FCFPAQUATIC_SIM  0.900000000
							6227    FCFPBOULDERS_DD  0.100000000
							6227   FCFPBOULDERS_LIT  0.100000000
							6227   FCFPBOULDERS_SIM  0.100000000
							6227       FCFPBRUSH_DD  0.200000000
							6227      FCFPBRUSH_LIT  0.200000000
							6227      FCFPBRUSH_SIM  0.400000000
							6227      FCFPLEDGES_DD  0.100000000
							6227     FCFPLEDGES_LIT  0.100000000
							6227     FCFPLEDGES_SIM  0.100000000
							6227   FCFPLIVETREES_DD  0.200000000
							6227  FCFPLIVETREES_LIT  0.100000000
							6227  FCFPLIVETREES_SIM  0.200000000
							6227    FCFPOVERHANG_DD  0.300000000
							6227   FCFPOVERHANG_LIT  0.600000000
							6227   FCFPOVERHANG_SIM  0.700000000
							6227       FCFPSNAGS_DD  0.000000000
							6227      FCFPSNAGS_LIT  0.000000000
							6227      FCFPSNAGS_SIM  0.000000000
							6227  FCFPSTRUCTURES_DD  0.000000000
							6227 FCFPSTRUCTURES_LIT  0.100000000
							6227 FCFPSTRUCTURES_SIM  0.100000000
							6235     FCFPAQUATIC_DD  0.000000000
							6235    FCFPAQUATIC_LIT  0.888888889
							6235    FCFPAQUATIC_SIM  0.888888889
							6235    FCFPBOULDERS_DD  0.000000000
#							6235   FCFPBOULDERS_LIT           NA
							6235   FCFPBOULDERS_SIM           NA
							6235       FCFPBRUSH_DD  0.000000000
							6235      FCFPBRUSH_LIT  1.000000000
							6235      FCFPBRUSH_SIM  1.000000000
							6235      FCFPLEDGES_DD  0.000000000
							6235     FCFPLEDGES_LIT  1.000000000
							6235     FCFPLEDGES_SIM  1.000000000
							6235   FCFPLIVETREES_DD  0.000000000
							6235  FCFPLIVETREES_LIT  0.111111111
							6235  FCFPLIVETREES_SIM  0.111111111
							6235    FCFPOVERHANG_DD  0.000000000
							6235   FCFPOVERHANG_LIT  1.000000000
							6235   FCFPOVERHANG_SIM  1.000000000
							6235       FCFPSNAGS_DD  0.000000000
							6235      FCFPSNAGS_LIT  0.222222222
							6235      FCFPSNAGS_SIM  0.222222222
							6235  FCFPSTRUCTURES_DD  0.000000000
							6235 FCFPSTRUCTURES_LIT  0.000000000
							6235 FCFPSTRUCTURES_SIM  0.000000000
							6281     FCFPAQUATIC_DD  0.000000000
							6281    FCFPAQUATIC_LIT  0.000000000
							6281    FCFPAQUATIC_SIM  0.000000000
							6281    FCFPBOULDERS_DD  0.000000000
							6281   FCFPBOULDERS_LIT  0.444444444
							6281   FCFPBOULDERS_SIM  0.111111111
							6281       FCFPBRUSH_DD  0.000000000
							6281      FCFPBRUSH_LIT  0.000000000
							6281      FCFPBRUSH_SIM  0.000000000
							6281      FCFPLEDGES_DD  0.000000000
							6281     FCFPLEDGES_LIT  0.000000000
							6281     FCFPLEDGES_SIM  0.000000000
							6281   FCFPLIVETREES_DD  0.000000000
							6281  FCFPLIVETREES_LIT  0.000000000
							6281  FCFPLIVETREES_SIM  0.000000000
							6281    FCFPOVERHANG_DD  0.000000000
							6281   FCFPOVERHANG_LIT  0.000000000
							6281   FCFPOVERHANG_SIM  0.000000000
							6281       FCFPSNAGS_DD  0.000000000
							6281      FCFPSNAGS_LIT  0.000000000
							6281      FCFPSNAGS_SIM  0.000000000
							6281  FCFPSTRUCTURES_DD  0.000000000
							6281 FCFPSTRUCTURES_LIT  0.000000000
							6281 FCFPSTRUCTURES_SIM  0.000000000
							6449     FCFPAQUATIC_DD  0.583333333
							6449    FCFPAQUATIC_LIT  1.000000000
							6449    FCFPAQUATIC_SIM  1.000000000
							6449    FCFPBOULDERS_DD  0.583333333
							6449   FCFPBOULDERS_LIT  0.166666667
							6449   FCFPBOULDERS_SIM  0.583333333
							6449       FCFPBRUSH_DD  0.750000000
							6449      FCFPBRUSH_LIT  0.583333333
							6449      FCFPBRUSH_SIM  0.750000000
							6449      FCFPLEDGES_DD  0.166666667
							6449     FCFPLEDGES_LIT  0.083333333
							6449     FCFPLEDGES_SIM  0.166666667
							6449   FCFPLIVETREES_DD  0.083333333
							6449  FCFPLIVETREES_LIT  0.083333333
							6449  FCFPLIVETREES_SIM  0.083333333
							6449    FCFPOVERHANG_DD  0.363636364
							6449   FCFPOVERHANG_LIT  0.333333333
							6449   FCFPOVERHANG_SIM  0.454545455
							6449       FCFPSNAGS_DD  0.000000000
							6449      FCFPSNAGS_LIT  0.000000000
							6449      FCFPSNAGS_SIM  0.000000000
							6449  FCFPSTRUCTURES_DD  0.666666667
							6449 FCFPSTRUCTURES_LIT  0.583333333
							6449 FCFPSTRUCTURES_SIM  0.666666667
							6683     FCFPAQUATIC_DD  0.000000000
							6683    FCFPAQUATIC_LIT  1.000000000
							6683    FCFPAQUATIC_SIM  0.000000000
							6683    FCFPBOULDERS_DD  0.000000000
							6683   FCFPBOULDERS_LIT  0.000000000
							6683   FCFPBOULDERS_SIM  0.000000000
							6683       FCFPBRUSH_DD  0.000000000
							6683      FCFPBRUSH_LIT  0.000000000
							6683      FCFPBRUSH_SIM  0.000000000
							6683      FCFPLEDGES_DD  0.000000000
							6683     FCFPLEDGES_LIT  0.000000000
							6683     FCFPLEDGES_SIM  0.000000000
							6683   FCFPLIVETREES_DD  0.000000000
							6683  FCFPLIVETREES_LIT  0.000000000
							6683  FCFPLIVETREES_SIM  0.000000000
							6683    FCFPOVERHANG_DD  0.000000000
							6683   FCFPOVERHANG_LIT  0.000000000
							6683   FCFPOVERHANG_SIM  0.000000000
							6683       FCFPSNAGS_DD  0.000000000
							6683      FCFPSNAGS_LIT  0.000000000
							6683      FCFPSNAGS_SIM  0.000000000
							6683  FCFPSTRUCTURES_DD  0.000000000
							6683 FCFPSTRUCTURES_LIT  0.000000000
							6683 FCFPSTRUCTURES_SIM  0.000000000
							7263     FCFPAQUATIC_DD  0.111111111
							7263    FCFPAQUATIC_LIT  1.000000000
							7263    FCFPAQUATIC_SIM  1.000000000
							7263    FCFPBOULDERS_DD  0.000000000
							7263   FCFPBOULDERS_LIT  0.600000000
							7263   FCFPBOULDERS_SIM  0.555555556
							7263       FCFPBRUSH_DD  0.000000000
							7263      FCFPBRUSH_LIT  0.600000000
							7263      FCFPBRUSH_SIM  0.666666667
							7263      FCFPLEDGES_DD  0.000000000
							7263     FCFPLEDGES_LIT  0.100000000
							7263     FCFPLEDGES_SIM  0.000000000
							7263   FCFPLIVETREES_DD  0.000000000
							7263  FCFPLIVETREES_LIT  0.000000000
							7263  FCFPLIVETREES_SIM  0.000000000
							7263    FCFPOVERHANG_DD  0.000000000
							7263   FCFPOVERHANG_LIT  0.700000000
							7263   FCFPOVERHANG_SIM  0.777777778
							7263       FCFPSNAGS_DD  0.000000000
							7263      FCFPSNAGS_LIT  0.200000000
							7263      FCFPSNAGS_SIM  0.111111111
							7263  FCFPSTRUCTURES_DD  0.000000000
							7263 FCFPSTRUCTURES_LIT  0.100000000
							7263 FCFPSTRUCTURES_SIM  0.111111111
							7913     FCFPAQUATIC_DD  0.000000000
							7913    FCFPAQUATIC_LIT  1.000000000
							7913    FCFPAQUATIC_SIM  1.000000000
							7913    FCFPBOULDERS_DD  0.000000000
							7913   FCFPBOULDERS_LIT  1.000000000
							7913   FCFPBOULDERS_SIM  1.000000000
							7913       FCFPBRUSH_DD  0.000000000
							7913      FCFPBRUSH_LIT  1.000000000
							7913      FCFPBRUSH_SIM  1.000000000
							7913      FCFPLEDGES_DD  0.000000000
#							7913     FCFPLEDGES_LIT           NA
							7913     FCFPLEDGES_SIM           NA
							7913   FCFPLIVETREES_DD  0.000000000
#							7913  FCFPLIVETREES_LIT           NA
							7913  FCFPLIVETREES_SIM           NA
							7913    FCFPOVERHANG_DD  0.000000000
							7913   FCFPOVERHANG_LIT  1.000000000
							7913   FCFPOVERHANG_SIM  1.000000000
							7913       FCFPSNAGS_DD  0.000000000
							7913      FCFPSNAGS_LIT  0.000000000
							7913      FCFPSNAGS_SIM  0.000000000
							7913  FCFPSTRUCTURES_DD  0.000000000
#							7913 FCFPSTRUCTURES_LIT           NA
							7913 FCFPSTRUCTURES_SIM           NA
						 1000057     FCFPAQUATIC_DD  0.000000000
						 1000057    FCFPAQUATIC_LIT  1.000000000
						 1000057    FCFPAQUATIC_SIM  1.000000000
						 1000057    FCFPBOULDERS_DD  0.000000000
						 1000057   FCFPBOULDERS_LIT  1.000000000
						 1000057   FCFPBOULDERS_SIM  1.000000000
						 1000057       FCFPBRUSH_DD  0.000000000
						 1000057      FCFPBRUSH_LIT  1.000000000
						 1000057      FCFPBRUSH_SIM  1.000000000
						 1000057      FCFPLEDGES_DD  0.000000000
						 1000057     FCFPLEDGES_LIT  1.000000000
						 1000057     FCFPLEDGES_SIM  1.000000000
						 1000057   FCFPLIVETREES_DD  0.000000000
						 1000057  FCFPLIVETREES_LIT  0.000000000
						 1000057  FCFPLIVETREES_SIM  0.000000000
						 1000057    FCFPOVERHANG_DD  0.000000000
						 1000057   FCFPOVERHANG_LIT  1.000000000
						 1000057   FCFPOVERHANG_SIM  1.000000000
						 1000057       FCFPSNAGS_DD  0.000000000
						 1000057      FCFPSNAGS_LIT  0.666666667
						 1000057      FCFPSNAGS_SIM  0.666666667
						 1000057  FCFPSTRUCTURES_DD  0.000000000
#						 1000057 FCFPSTRUCTURES_LIT           NA
						 1000057 FCFPSTRUCTURES_SIM           NA
							6160     FCFCAQUATIC_DD  0.257500000
							6160    FCFCAQUATIC_LIT  0.110000000
							6160    FCFCAQUATIC_SIM  0.213150000
							6160    FCFCBOULDERS_DD  0.000000000
							6160   FCFCBOULDERS_LIT  0.000000000
							6160   FCFCBOULDERS_SIM  0.000000000
							6160       FCFCBRUSH_DD  0.127500000
							6160      FCFCBRUSH_LIT  0.035000000
							6160      FCFCBRUSH_SIM  0.100850000
							6160      FCFCLEDGES_DD  0.000000000
							6160     FCFCLEDGES_LIT  0.057500000
							6160     FCFCLEDGES_SIM  0.021850000
							6160   FCFCLIVETREES_DD  0.000000000
							6160  FCFCLIVETREES_LIT  0.005000000
							6160  FCFCLIVETREES_SIM  0.002850000
							6160    FCFCOVERHANG_DD  0.060000000
							6160   FCFCOVERHANG_LIT  0.005000000
							6160   FCFCOVERHANG_SIM  0.034700000
							6160       FCFCSNAGS_DD  0.005000000
							6160      FCFCSNAGS_LIT  0.000000000
							6160      FCFCSNAGS_SIM  0.002150000
							6160  FCFCSTRUCTURES_DD  0.025000000
							6160 FCFCSTRUCTURES_LIT  0.025000000
							6160 FCFCSTRUCTURES_SIM  0.025000000
							6189     FCFCAQUATIC_DD  0.000000000
							6189    FCFCAQUATIC_LIT  0.412500000
							6189    FCFCAQUATIC_SIM  0.412500000
							6189    FCFCBOULDERS_DD  0.000000000
							6189   FCFCBOULDERS_LIT  0.142500000
							6189   FCFCBOULDERS_SIM  0.142500000
							6189       FCFCBRUSH_DD  0.000000000
							6189      FCFCBRUSH_LIT  0.065000000
							6189      FCFCBRUSH_SIM  0.065000000
							6189      FCFCLEDGES_DD  0.000000000
							6189     FCFCLEDGES_LIT  0.015000000
							6189     FCFCLEDGES_SIM  0.015000000
							6189   FCFCLIVETREES_DD  0.000000000
							6189  FCFCLIVETREES_LIT  0.000000000
							6189  FCFCLIVETREES_SIM  0.000000000
							6189    FCFCOVERHANG_DD  0.000000000
							6189   FCFCOVERHANG_LIT  0.090000000
							6189   FCFCOVERHANG_SIM  0.090000000
							6189       FCFCSNAGS_DD  0.000000000
							6189      FCFCSNAGS_LIT  0.000000000
							6189      FCFCSNAGS_SIM  0.000000000
							6189  FCFCSTRUCTURES_DD  0.000000000
							6189 FCFCSTRUCTURES_LIT  0.000000000
							6189 FCFCSTRUCTURES_SIM  0.000000000
							6227     FCFCAQUATIC_DD  0.030000000
							6227    FCFCAQUATIC_LIT  0.355000000
							6227    FCFCAQUATIC_SIM  0.345275000
							6227    FCFCBOULDERS_DD  0.057500000
							6227   FCFCBOULDERS_LIT  0.057500000
							6227   FCFCBOULDERS_SIM  0.057500000
							6227       FCFCBRUSH_DD  0.010000000
							6227      FCFCBRUSH_LIT  0.010000000
							6227      FCFCBRUSH_SIM  0.012200000
							6227      FCFCLEDGES_DD  0.005000000
							6227     FCFCLEDGES_LIT  0.005000000
							6227     FCFCLEDGES_SIM  0.005000000
							6227   FCFCLIVETREES_DD  0.030000000
							6227  FCFCLIVETREES_LIT  0.025000000
							6227  FCFCLIVETREES_SIM  0.026200000
							6227    FCFCOVERHANG_DD  0.035000000
							6227   FCFCOVERHANG_LIT  0.130000000
							6227   FCFCOVERHANG_SIM  0.125400000
							6227       FCFCSNAGS_DD  0.000000000
							6227      FCFCSNAGS_LIT  0.000000000
							6227      FCFCSNAGS_SIM  0.000000000
							6227  FCFCSTRUCTURES_DD  0.000000000
							6227 FCFCSTRUCTURES_LIT  0.005000000
							6227 FCFCSTRUCTURES_SIM  0.005000000
							6235     FCFCAQUATIC_DD  0.000000000
							6235    FCFCAQUATIC_LIT  0.044444444
							6235    FCFCAQUATIC_SIM  0.044444444
							6235    FCFCBOULDERS_DD  0.000000000
#							6235   FCFCBOULDERS_LIT           NA
							6235   FCFCBOULDERS_SIM           NA
							6235       FCFCBRUSH_DD  0.000000000
							6235      FCFCBRUSH_LIT  0.072222222
							6235      FCFCBRUSH_SIM  0.072222222
							6235      FCFCLEDGES_DD  0.000000000
							6235     FCFCLEDGES_LIT  0.602777778
							6235     FCFCLEDGES_SIM  0.602777778
							6235   FCFCLIVETREES_DD  0.000000000
							6235  FCFCLIVETREES_LIT  0.005555556
							6235  FCFCLIVETREES_SIM  0.005555556
							6235    FCFCOVERHANG_DD  0.000000000
							6235   FCFCOVERHANG_LIT  0.130555556
							6235   FCFCOVERHANG_SIM  0.130555556
							6235       FCFCSNAGS_DD  0.000000000
							6235      FCFCSNAGS_LIT  0.011111111
							6235      FCFCSNAGS_SIM  0.011111111
							6235  FCFCSTRUCTURES_DD  0.000000000
							6235 FCFCSTRUCTURES_LIT  0.000000000
							6235 FCFCSTRUCTURES_SIM  0.000000000
							6281     FCFCAQUATIC_DD  0.000000000
							6281    FCFCAQUATIC_LIT  0.000000000
							6281    FCFCAQUATIC_SIM  0.000000000
							6281    FCFCBOULDERS_DD  0.000000000
							6281   FCFCBOULDERS_LIT  0.022222222
							6281   FCFCBOULDERS_SIM  0.002222222
							6281       FCFCBRUSH_DD  0.000000000
							6281      FCFCBRUSH_LIT  0.000000000
							6281      FCFCBRUSH_SIM  0.000000000
							6281      FCFCLEDGES_DD  0.000000000
							6281     FCFCLEDGES_LIT  0.000000000
							6281     FCFCLEDGES_SIM  0.000000000
							6281   FCFCLIVETREES_DD  0.000000000
							6281  FCFCLIVETREES_LIT  0.000000000
							6281  FCFCLIVETREES_SIM  0.000000000
							6281    FCFCOVERHANG_DD  0.000000000
							6281   FCFCOVERHANG_LIT  0.000000000
							6281   FCFCOVERHANG_SIM  0.000000000
							6281       FCFCSNAGS_DD  0.000000000
							6281      FCFCSNAGS_LIT  0.000000000
							6281      FCFCSNAGS_SIM  0.000000000
							6281  FCFCSTRUCTURES_DD  0.000000000
							6281 FCFCSTRUCTURES_LIT  0.000000000
							6281 FCFCSTRUCTURES_SIM  0.000000000
							6449     FCFCAQUATIC_DD  0.045833333
							6449    FCFCAQUATIC_LIT  0.050000000
							6449    FCFCAQUATIC_SIM  0.049750000
							6449    FCFCBOULDERS_DD  0.089583333
							6449   FCFCBOULDERS_LIT  0.008333333
							6449   FCFCBOULDERS_SIM  0.011020833
							6449       FCFCBRUSH_DD  0.070833333
							6449      FCFCBRUSH_LIT  0.029166667
							6449      FCFCBRUSH_SIM  0.030833333
							6449      FCFCLEDGES_DD  0.008333333
							6449     FCFCLEDGES_LIT  0.004166667
							6449     FCFCLEDGES_SIM  0.004375000
							6449   FCFCLIVETREES_DD  0.020833333
							6449  FCFCLIVETREES_LIT  0.004166667
							6449  FCFCLIVETREES_SIM  0.004500000
							6449    FCFCOVERHANG_DD  0.102272727
							6449   FCFCOVERHANG_LIT  0.033333333
							6449   FCFCOVERHANG_SIM  0.039681818
							6449       FCFCSNAGS_DD  0.000000000
							6449      FCFCSNAGS_LIT  0.000000000
							6449      FCFCSNAGS_SIM  0.000000000
							6449  FCFCSTRUCTURES_DD  0.033333333
							6449 FCFCSTRUCTURES_LIT  0.029166667
							6449 FCFCSTRUCTURES_SIM  0.029375000
							6683     FCFCAQUATIC_DD  0.000000000
							6683    FCFCAQUATIC_LIT  0.875000000
							6683    FCFCAQUATIC_SIM  0.000000000
							6683    FCFCBOULDERS_DD  0.000000000
							6683   FCFCBOULDERS_LIT  0.000000000
							6683   FCFCBOULDERS_SIM  0.000000000
							6683       FCFCBRUSH_DD  0.000000000
							6683      FCFCBRUSH_LIT  0.000000000
							6683      FCFCBRUSH_SIM  0.000000000
							6683      FCFCLEDGES_DD  0.000000000
							6683     FCFCLEDGES_LIT  0.000000000
							6683     FCFCLEDGES_SIM  0.000000000
							6683   FCFCLIVETREES_DD  0.000000000
							6683  FCFCLIVETREES_LIT  0.000000000
							6683  FCFCLIVETREES_SIM  0.000000000
							6683    FCFCOVERHANG_DD  0.000000000
							6683   FCFCOVERHANG_LIT  0.000000000
							6683   FCFCOVERHANG_SIM  0.000000000
							6683       FCFCSNAGS_DD  0.000000000
							6683      FCFCSNAGS_LIT  0.000000000
							6683      FCFCSNAGS_SIM  0.000000000
							6683  FCFCSTRUCTURES_DD  0.000000000
							6683 FCFCSTRUCTURES_LIT  0.000000000
							6683 FCFCSTRUCTURES_SIM  0.000000000
							7263     FCFCAQUATIC_DD  0.005555556
							7263    FCFCAQUATIC_LIT  0.050000000
							7263    FCFCAQUATIC_SIM  0.050000000
							7263    FCFCBOULDERS_DD  0.000000000
							7263   FCFCBOULDERS_LIT  0.070000000
							7263   FCFCBOULDERS_SIM  0.072222222
							7263       FCFCBRUSH_DD  0.000000000
							7263      FCFCBRUSH_LIT  0.030000000
							7263      FCFCBRUSH_SIM  0.033333333
							7263      FCFCLEDGES_DD  0.000000000
							7263     FCFCLEDGES_LIT  0.005000000
							7263     FCFCLEDGES_SIM  0.000000000
							7263   FCFCLIVETREES_DD  0.000000000
							7263  FCFCLIVETREES_LIT  0.000000000
							7263  FCFCLIVETREES_SIM  0.000000000
							7263    FCFCOVERHANG_DD  0.000000000
							7263   FCFCOVERHANG_LIT  0.035000000
							7263   FCFCOVERHANG_SIM  0.038888889
							7263       FCFCSNAGS_DD  0.000000000
							7263      FCFCSNAGS_LIT  0.010000000
							7263      FCFCSNAGS_SIM  0.005555556
							7263  FCFCSTRUCTURES_DD  0.000000000
							7263 FCFCSTRUCTURES_LIT  0.005000000
							7263 FCFCSTRUCTURES_SIM  0.005555556
							7913     FCFCAQUATIC_DD  0.000000000
							7913    FCFCAQUATIC_LIT  0.647500000
							7913    FCFCAQUATIC_SIM  0.647500000
							7913    FCFCBOULDERS_DD  0.000000000
							7913   FCFCBOULDERS_LIT  0.050000000
							7913   FCFCBOULDERS_SIM  0.050000000
							7913       FCFCBRUSH_DD  0.000000000
							7913      FCFCBRUSH_LIT  0.050000000
							7913      FCFCBRUSH_SIM  0.050000000
							7913      FCFCLEDGES_DD  0.000000000
#							7913     FCFCLEDGES_LIT           NA
							7913     FCFCLEDGES_SIM           NA
							7913   FCFCLIVETREES_DD  0.000000000
#							7913  FCFCLIVETREES_LIT           NA
							7913  FCFCLIVETREES_SIM           NA
							7913    FCFCOVERHANG_DD  0.000000000
							7913   FCFCOVERHANG_LIT  0.050000000
							7913   FCFCOVERHANG_SIM  0.050000000
							7913       FCFCSNAGS_DD  0.000000000
							7913      FCFCSNAGS_LIT  0.000000000
							7913      FCFCSNAGS_SIM  0.000000000
							7913  FCFCSTRUCTURES_DD  0.000000000
#							7913 FCFCSTRUCTURES_LIT           NA
							7913 FCFCSTRUCTURES_SIM           NA
						 1000057     FCFCAQUATIC_DD  0.000000000
						 1000057    FCFCAQUATIC_LIT  0.308333333
						 1000057    FCFCAQUATIC_SIM  0.308333333
						 1000057    FCFCBOULDERS_DD  0.000000000
						 1000057   FCFCBOULDERS_LIT  0.100000000
						 1000057   FCFCBOULDERS_SIM  0.100000000
						 1000057       FCFCBRUSH_DD  0.000000000
						 1000057      FCFCBRUSH_LIT  0.050000000
						 1000057      FCFCBRUSH_SIM  0.050000000
						 1000057      FCFCLEDGES_DD  0.000000000
						 1000057     FCFCLEDGES_LIT  0.250000000
						 1000057     FCFCLEDGES_SIM  0.250000000
						 1000057   FCFCLIVETREES_DD  0.000000000
						 1000057  FCFCLIVETREES_LIT  0.000000000
						 1000057  FCFCLIVETREES_SIM  0.000000000
						 1000057    FCFCOVERHANG_DD  0.000000000
						 1000057   FCFCOVERHANG_LIT  0.050000000
						 1000057   FCFCOVERHANG_SIM  0.050000000
						 1000057       FCFCSNAGS_DD  0.000000000
						 1000057      FCFCSNAGS_LIT  0.033333333
						 1000057      FCFCSNAGS_SIM  0.033333333
						 1000057  FCFCSTRUCTURES_DD  0.000000000
#						 1000057 FCFCSTRUCTURES_LIT           NA
						 1000057 FCFCSTRUCTURES_SIM           NA
							6160      FCVAQUATIC_DD  0.305743629
							6160     FCVAQUATIC_LIT  0.096609178
							6160     FCVAQUATIC_SIM  0.201583978
							6160     FCVBOULDERS_DD  0.000000000
							6160    FCVBOULDERS_LIT  0.000000000
							6160    FCVBOULDERS_SIM  0.000000000
							6160        FCVBRUSH_DD  0.263114360
							6160       FCVBRUSH_LIT  0.024152295
							6160       FCVBRUSH_SIM  0.185744755
							6160       FCVLEDGES_DD  0.000000000
							6160      FCVLEDGES_LIT  0.181830965
							6160      FCVLEDGES_SIM  0.069095767
							6160    FCVLIVETREES_DD  0.000000000
							6160   FCVLIVETREES_LIT  0.015811388
							6160   FCVLIVETREES_SIM  0.009012491
							6160     FCVOVERHANG_DD  0.069920590
							6160    FCVOVERHANG_LIT  0.015811388
							6160    FCVOVERHANG_SIM  0.055472816
							6160        FCVSNAGS_DD  0.015811388
							6160       FCVSNAGS_LIT  0.000000000
							6160       FCVSNAGS_SIM  0.006798897
							6160   FCVSTRUCTURES_DD  0.079056942
							6160  FCVSTRUCTURES_LIT  0.079056942
							6160  FCVSTRUCTURES_SIM  0.079056942
							6189      FCVAQUATIC_DD  0.000000000
							6189     FCVAQUATIC_LIT  0.171290040
							6189     FCVAQUATIC_SIM  0.171290040
							6189     FCVBOULDERS_DD  0.000000000
							6189    FCVBOULDERS_LIT  0.188580222
							6189    FCVBOULDERS_SIM  0.188580222
							6189        FCVBRUSH_DD  0.000000000
							6189       FCVBRUSH_LIT  0.066874675
							6189       FCVBRUSH_SIM  0.066874675
							6189       FCVLEDGES_DD  0.000000000
							6189      FCVLEDGES_LIT  0.024152295
							6189      FCVLEDGES_SIM  0.024152295
							6189    FCVLIVETREES_DD  0.000000000
							6189   FCVLIVETREES_LIT  0.000000000
							6189   FCVLIVETREES_SIM  0.000000000
							6189     FCVOVERHANG_DD  0.000000000
							6189    FCVOVERHANG_LIT  0.084327404
							6189    FCVOVERHANG_SIM  0.084327404
							6189        FCVSNAGS_DD  0.000000000
							6189       FCVSNAGS_LIT  0.000000000
							6189       FCVSNAGS_SIM  0.000000000
							6189   FCVSTRUCTURES_DD  0.000000000
							6189  FCVSTRUCTURES_LIT  0.000000000
							6189  FCVSTRUCTURES_SIM  0.000000000
							6227      FCVAQUATIC_DD  0.078881064
							6227     FCVAQUATIC_LIT  0.284507372
							6227     FCVAQUATIC_SIM  0.278062005
							6227     FCVBOULDERS_DD  0.181830965
							6227    FCVBOULDERS_LIT  0.181830965
							6227    FCVBOULDERS_SIM  0.181830965
							6227        FCVBRUSH_DD  0.021081851
							6227       FCVBRUSH_LIT  0.021081851
							6227       FCVBRUSH_SIM  0.020427650
							6227       FCVLEDGES_DD  0.015811388
							6227      FCVLEDGES_LIT  0.015811388
							6227      FCVLEDGES_SIM  0.015811388
							6227    FCVLIVETREES_DD  0.078881064
							6227   FCVLIVETREES_LIT  0.079056942
							6227   FCVLIVETREES_SIM  0.078725684
							6227     FCVOVERHANG_DD  0.078351062
							6227    FCVOVERHANG_LIT  0.127366488
							6227    FCVOVERHANG_SIM  0.121223577
							6227        FCVSNAGS_DD  0.000000000
							6227       FCVSNAGS_LIT  0.000000000
							6227       FCVSNAGS_SIM  0.000000000
							6227   FCVSTRUCTURES_DD  0.000000000
							6227  FCVSTRUCTURES_LIT  0.015811388
							6227  FCVSTRUCTURES_SIM  0.015811388
							6235      FCVAQUATIC_DD  0.000000000
							6235     FCVAQUATIC_LIT  0.016666667
							6235     FCVAQUATIC_SIM  0.016666667
							6235     FCVBOULDERS_DD  0.000000000
#							6235    FCVBOULDERS_LIT           NA
							6235    FCVBOULDERS_SIM           NA
							6235        FCVBRUSH_DD  0.000000000
							6235       FCVBRUSH_LIT  0.066666667
							6235       FCVBRUSH_SIM  0.066666667
							6235       FCVLEDGES_DD  0.000000000
							6235      FCVLEDGES_LIT  0.243491672
							6235      FCVLEDGES_SIM  0.243491672
							6235    FCVLIVETREES_DD  0.000000000
							6235   FCVLIVETREES_LIT  0.016666667
							6235   FCVLIVETREES_SIM  0.016666667
							6235     FCVOVERHANG_DD  0.000000000
							6235    FCVOVERHANG_LIT  0.179311957
							6235    FCVOVERHANG_SIM  0.179311957
							6235        FCVSNAGS_DD  0.000000000
							6235       FCVSNAGS_LIT  0.022047928
							6235       FCVSNAGS_SIM  0.022047928
							6235   FCVSTRUCTURES_DD  0.000000000
							6235  FCVSTRUCTURES_LIT  0.000000000
							6235  FCVSTRUCTURES_SIM  0.000000000
							6281      FCVAQUATIC_DD  0.000000000
							6281     FCVAQUATIC_LIT  0.000000000
							6281     FCVAQUATIC_SIM  0.000000000
							6281     FCVBOULDERS_DD  0.000000000
							6281    FCVBOULDERS_LIT  0.026352314
							6281    FCVBOULDERS_SIM  0.006666667
							6281        FCVBRUSH_DD  0.000000000
							6281       FCVBRUSH_LIT  0.000000000
							6281       FCVBRUSH_SIM  0.000000000
							6281       FCVLEDGES_DD  0.000000000
							6281      FCVLEDGES_LIT  0.000000000
							6281      FCVLEDGES_SIM  0.000000000
							6281    FCVLIVETREES_DD  0.000000000
							6281   FCVLIVETREES_LIT  0.000000000
							6281   FCVLIVETREES_SIM  0.000000000
							6281     FCVOVERHANG_DD  0.000000000
							6281    FCVOVERHANG_LIT  0.000000000
							6281    FCVOVERHANG_SIM  0.000000000
							6281        FCVSNAGS_DD  0.000000000
							6281       FCVSNAGS_LIT  0.000000000
							6281       FCVSNAGS_SIM  0.000000000
							6281   FCVSTRUCTURES_DD  0.000000000
							6281  FCVSTRUCTURES_LIT  0.000000000
							6281  FCVSTRUCTURES_SIM  0.000000000
							6449      FCVAQUATIC_DD  0.068947718
							6449     FCVAQUATIC_LIT  0.000000000
							6449     FCVAQUATIC_SIM  0.001630672
							6449     FCVBOULDERS_DD  0.167690981
							6449    FCVBOULDERS_LIT  0.019462474
							6449    FCVBOULDERS_SIM  0.022415107
							6449        FCVBRUSH_DD  0.086493125
							6449       FCVBRUSH_LIT  0.025746433
							6449       FCVBRUSH_SIM  0.026731266
							6449       FCVLEDGES_DD  0.019462474
							6449      FCVLEDGES_LIT  0.014433757
							6449      FCVLEDGES_SIM  0.014386112
							6449    FCVLIVETREES_DD  0.072168784
							6449   FCVLIVETREES_LIT  0.014433757
							6449   FCVLIVETREES_SIM  0.015588457
							6449     FCVOVERHANG_DD  0.185220998
							6449    FCVOVERHANG_LIT  0.071774056
							6449    FCVOVERHANG_SIM  0.077181368
							6449        FCVSNAGS_DD  0.000000000
							6449       FCVSNAGS_LIT  0.000000000
							6449       FCVSNAGS_SIM  0.000000000
							6449   FCVSTRUCTURES_DD  0.024618298
							6449  FCVSTRUCTURES_LIT  0.025746433
							6449  FCVSTRUCTURES_SIM  0.025497883
							6683      FCVAQUATIC_DD           NA
							6683     FCVAQUATIC_LIT           NA
							6683     FCVAQUATIC_SIM           NA
							6683     FCVBOULDERS_DD           NA
							6683    FCVBOULDERS_LIT           NA
							6683    FCVBOULDERS_SIM           NA
							6683        FCVBRUSH_DD           NA
							6683       FCVBRUSH_LIT           NA
							6683       FCVBRUSH_SIM           NA
							6683       FCVLEDGES_DD           NA
							6683      FCVLEDGES_LIT           NA
							6683      FCVLEDGES_SIM           NA
							6683    FCVLIVETREES_DD           NA
							6683   FCVLIVETREES_LIT           NA
							6683   FCVLIVETREES_SIM           NA
							6683     FCVOVERHANG_DD           NA
							6683    FCVOVERHANG_LIT           NA
							6683    FCVOVERHANG_SIM           NA
							6683        FCVSNAGS_DD           NA
							6683       FCVSNAGS_LIT           NA
							6683       FCVSNAGS_SIM           NA
							6683   FCVSTRUCTURES_DD           NA
							6683  FCVSTRUCTURES_LIT           NA
							6683  FCVSTRUCTURES_SIM           NA
							7263      FCVAQUATIC_DD  0.016666667
							7263     FCVAQUATIC_LIT  0.000000000
							7263     FCVAQUATIC_SIM  0.000000000
							7263     FCVBOULDERS_DD  0.000000000
							7263    FCVBOULDERS_LIT  0.097752522
							7263    FCVBOULDERS_SIM  0.103413947
							7263        FCVBRUSH_DD  0.000000000
							7263       FCVBRUSH_LIT  0.025819889
							7263       FCVBRUSH_SIM  0.025000000
							7263       FCVLEDGES_DD  0.000000000
							7263      FCVLEDGES_LIT  0.015811388
							7263      FCVLEDGES_SIM  0.000000000
							7263    FCVLIVETREES_DD  0.000000000
							7263   FCVLIVETREES_LIT  0.000000000
							7263   FCVLIVETREES_SIM  0.000000000
							7263     FCVOVERHANG_DD  0.000000000
							7263    FCVOVERHANG_LIT  0.024152295
							7263    FCVOVERHANG_SIM  0.022047928
							7263        FCVSNAGS_DD  0.000000000
							7263       FCVSNAGS_LIT  0.021081851
							7263       FCVSNAGS_SIM  0.016666667
							7263   FCVSTRUCTURES_DD  0.000000000
							7263  FCVSTRUCTURES_LIT  0.015811388
							7263  FCVSTRUCTURES_SIM  0.016666667
							7913      FCVAQUATIC_DD  0.000000000
							7913     FCVAQUATIC_LIT  0.370332058
							7913     FCVAQUATIC_SIM  0.370332058
							7913     FCVBOULDERS_DD  0.000000000
							7913    FCVBOULDERS_LIT  0.000000000
							7913    FCVBOULDERS_SIM  0.000000000
							7913        FCVBRUSH_DD  0.000000000
							7913       FCVBRUSH_LIT  0.000000000
							7913       FCVBRUSH_SIM  0.000000000
							7913       FCVLEDGES_DD  0.000000000
#							7913      FCVLEDGES_LIT           NA
							7913      FCVLEDGES_SIM           NA
							7913    FCVLIVETREES_DD  0.000000000
#							7913   FCVLIVETREES_LIT           NA
							7913   FCVLIVETREES_SIM           NA
							7913     FCVOVERHANG_DD  0.000000000
							7913    FCVOVERHANG_LIT  0.000000000
							7913    FCVOVERHANG_SIM  0.000000000
							7913        FCVSNAGS_DD  0.000000000
							7913       FCVSNAGS_LIT  0.000000000
							7913       FCVSNAGS_SIM  0.000000000
							7913   FCVSTRUCTURES_DD  0.000000000
#							7913  FCVSTRUCTURES_LIT           NA
							7913  FCVSTRUCTURES_SIM           NA
						 1000057      FCVAQUATIC_DD  0.000000000
						 1000057     FCVAQUATIC_LIT  0.345205253
						 1000057     FCVAQUATIC_SIM  0.345205253
						 1000057     FCVBOULDERS_DD  0.000000000
						 1000057    FCVBOULDERS_LIT  0.092582010
						 1000057    FCVBOULDERS_SIM  0.092582010
						 1000057        FCVBRUSH_DD  0.000000000
						 1000057       FCVBRUSH_LIT  0.000000000
						 1000057       FCVBRUSH_SIM  0.000000000
						 1000057       FCVLEDGES_DD  0.000000000
						 1000057      FCVLEDGES_LIT           NA
						 1000057      FCVLEDGES_SIM           NA
						 1000057    FCVLIVETREES_DD  0.000000000
						 1000057   FCVLIVETREES_LIT           NA
						 1000057   FCVLIVETREES_SIM           NA
						 1000057     FCVOVERHANG_DD  0.000000000
						 1000057    FCVOVERHANG_LIT  0.000000000
						 1000057    FCVOVERHANG_SIM  0.000000000
						 1000057        FCVSNAGS_DD  0.000000000
						 1000057       FCVSNAGS_LIT  0.028867513
						 1000057       FCVSNAGS_SIM  0.028867513
						 1000057   FCVSTRUCTURES_DD  0.000000000
#						 1000057  FCVSTRUCTURES_LIT           NA
						 1000057  FCVSTRUCTURES_SIM           NA
							6160      FCNAQUATIC_DD 10.000000000
							6160     FCNAQUATIC_LIT 10.000000000
							6160     FCNAQUATIC_SIM 10.000000000
							6160     FCNBOULDERS_DD 10.000000000
							6160    FCNBOULDERS_LIT 10.000000000
							6160    FCNBOULDERS_SIM 10.000000000
							6160        FCNBRUSH_DD 10.000000000
							6160       FCNBRUSH_LIT 10.000000000
							6160       FCNBRUSH_SIM 10.000000000
							6160       FCNLEDGES_DD 10.000000000
							6160      FCNLEDGES_LIT 10.000000000
							6160      FCNLEDGES_SIM 10.000000000
							6160    FCNLIVETREES_DD 10.000000000
							6160   FCNLIVETREES_LIT 10.000000000
							6160   FCNLIVETREES_SIM 10.000000000
							6160     FCNOVERHANG_DD 10.000000000
							6160    FCNOVERHANG_LIT 10.000000000
							6160    FCNOVERHANG_SIM 10.000000000
							6160        FCNSNAGS_DD 10.000000000
							6160       FCNSNAGS_LIT 10.000000000
							6160       FCNSNAGS_SIM 10.000000000
							6160   FCNSTRUCTURES_DD 10.000000000
							6160  FCNSTRUCTURES_LIT 10.000000000
							6160  FCNSTRUCTURES_SIM 10.000000000
							6189      FCNAQUATIC_DD 10.000000000
							6189     FCNAQUATIC_LIT 10.000000000
							6189     FCNAQUATIC_SIM 10.000000000
							6189     FCNBOULDERS_DD 10.000000000
							6189    FCNBOULDERS_LIT 10.000000000
							6189    FCNBOULDERS_SIM 10.000000000
							6189        FCNBRUSH_DD 10.000000000
							6189       FCNBRUSH_LIT 10.000000000
							6189       FCNBRUSH_SIM 10.000000000
							6189       FCNLEDGES_DD 10.000000000
							6189      FCNLEDGES_LIT 10.000000000
							6189      FCNLEDGES_SIM 10.000000000
							6189    FCNLIVETREES_DD 10.000000000
							6189   FCNLIVETREES_LIT 10.000000000
							6189   FCNLIVETREES_SIM 10.000000000
							6189     FCNOVERHANG_DD 10.000000000
							6189    FCNOVERHANG_LIT 10.000000000
							6189    FCNOVERHANG_SIM 10.000000000
							6189        FCNSNAGS_DD 10.000000000
							6189       FCNSNAGS_LIT 10.000000000
							6189       FCNSNAGS_SIM 10.000000000
							6189   FCNSTRUCTURES_DD 10.000000000
							6189  FCNSTRUCTURES_LIT 10.000000000
							6189  FCNSTRUCTURES_SIM 10.000000000
							6227      FCNAQUATIC_DD 10.000000000
							6227     FCNAQUATIC_LIT 10.000000000
							6227     FCNAQUATIC_SIM 10.000000000
							6227     FCNBOULDERS_DD 10.000000000
							6227    FCNBOULDERS_LIT 10.000000000
							6227    FCNBOULDERS_SIM 10.000000000
							6227        FCNBRUSH_DD 10.000000000
							6227       FCNBRUSH_LIT 10.000000000
							6227       FCNBRUSH_SIM 10.000000000
							6227       FCNLEDGES_DD 10.000000000
							6227      FCNLEDGES_LIT 10.000000000
							6227      FCNLEDGES_SIM 10.000000000
							6227    FCNLIVETREES_DD 10.000000000
							6227   FCNLIVETREES_LIT 10.000000000
							6227   FCNLIVETREES_SIM 10.000000000
							6227     FCNOVERHANG_DD 10.000000000
							6227    FCNOVERHANG_LIT 10.000000000
							6227    FCNOVERHANG_SIM 10.000000000
							6227        FCNSNAGS_DD 10.000000000
							6227       FCNSNAGS_LIT 10.000000000
							6227       FCNSNAGS_SIM 10.000000000
							6227   FCNSTRUCTURES_DD 10.000000000
							6227  FCNSTRUCTURES_LIT 10.000000000
							6227  FCNSTRUCTURES_SIM 10.000000000
							6235      FCNAQUATIC_DD 10.000000000
							6235     FCNAQUATIC_LIT  9.000000000
							6235     FCNAQUATIC_SIM  9.000000000
							6235     FCNBOULDERS_DD 10.000000000
							6235    FCNBOULDERS_LIT  0.000000000
							6235    FCNBOULDERS_SIM  0.000000000
							6235        FCNBRUSH_DD 10.000000000
							6235       FCNBRUSH_LIT  9.000000000
							6235       FCNBRUSH_SIM  9.000000000
							6235       FCNLEDGES_DD 10.000000000
							6235      FCNLEDGES_LIT  9.000000000
							6235      FCNLEDGES_SIM  9.000000000
							6235    FCNLIVETREES_DD 10.000000000
							6235   FCNLIVETREES_LIT  9.000000000
							6235   FCNLIVETREES_SIM  9.000000000
							6235     FCNOVERHANG_DD 10.000000000
							6235    FCNOVERHANG_LIT  9.000000000
							6235    FCNOVERHANG_SIM  9.000000000
							6235        FCNSNAGS_DD 10.000000000
							6235       FCNSNAGS_LIT  9.000000000
							6235       FCNSNAGS_SIM  9.000000000
							6235   FCNSTRUCTURES_DD 10.000000000
							6235  FCNSTRUCTURES_LIT  9.000000000
							6235  FCNSTRUCTURES_SIM  9.000000000
							6281      FCNAQUATIC_DD  9.000000000
							6281     FCNAQUATIC_LIT  9.000000000
							6281     FCNAQUATIC_SIM  9.000000000
							6281     FCNBOULDERS_DD  9.000000000
							6281    FCNBOULDERS_LIT  9.000000000
							6281    FCNBOULDERS_SIM  9.000000000
							6281        FCNBRUSH_DD  9.000000000
							6281       FCNBRUSH_LIT  9.000000000
							6281       FCNBRUSH_SIM  9.000000000
							6281       FCNLEDGES_DD  9.000000000
							6281      FCNLEDGES_LIT  9.000000000
							6281      FCNLEDGES_SIM  9.000000000
							6281    FCNLIVETREES_DD  9.000000000
							6281   FCNLIVETREES_LIT  9.000000000
							6281   FCNLIVETREES_SIM  9.000000000
							6281     FCNOVERHANG_DD  9.000000000
							6281    FCNOVERHANG_LIT  9.000000000
							6281    FCNOVERHANG_SIM  9.000000000
							6281        FCNSNAGS_DD  9.000000000
							6281       FCNSNAGS_LIT  9.000000000
							6281       FCNSNAGS_SIM  9.000000000
							6281   FCNSTRUCTURES_DD  9.000000000
							6281  FCNSTRUCTURES_LIT  9.000000000
							6281  FCNSTRUCTURES_SIM  9.000000000
							6449      FCNAQUATIC_DD 12.000000000
							6449     FCNAQUATIC_LIT 12.000000000
							6449     FCNAQUATIC_SIM 12.000000000
							6449     FCNBOULDERS_DD 12.000000000
							6449    FCNBOULDERS_LIT 12.000000000
							6449    FCNBOULDERS_SIM 12.000000000
							6449        FCNBRUSH_DD 12.000000000
							6449       FCNBRUSH_LIT 12.000000000
							6449       FCNBRUSH_SIM 12.000000000
							6449       FCNLEDGES_DD 12.000000000
							6449      FCNLEDGES_LIT 12.000000000
							6449      FCNLEDGES_SIM 12.000000000
							6449    FCNLIVETREES_DD 12.000000000
							6449   FCNLIVETREES_LIT 12.000000000
							6449   FCNLIVETREES_SIM 12.000000000
							6449     FCNOVERHANG_DD 11.000000000
							6449    FCNOVERHANG_LIT 12.000000000
							6449    FCNOVERHANG_SIM 11.000000000
							6449        FCNSNAGS_DD 12.000000000
							6449       FCNSNAGS_LIT 12.000000000
							6449       FCNSNAGS_SIM 12.000000000
							6449   FCNSTRUCTURES_DD 12.000000000
							6449  FCNSTRUCTURES_LIT 12.000000000
							6449  FCNSTRUCTURES_SIM 12.000000000
							6683      FCNAQUATIC_DD  1.000000000
							6683     FCNAQUATIC_LIT  1.000000000
							6683     FCNAQUATIC_SIM  1.000000000
							6683     FCNBOULDERS_DD  1.000000000
							6683    FCNBOULDERS_LIT  1.000000000
							6683    FCNBOULDERS_SIM  1.000000000
							6683        FCNBRUSH_DD  1.000000000
							6683       FCNBRUSH_LIT  1.000000000
							6683       FCNBRUSH_SIM  1.000000000
							6683       FCNLEDGES_DD  1.000000000
							6683      FCNLEDGES_LIT  1.000000000
							6683      FCNLEDGES_SIM  1.000000000
							6683    FCNLIVETREES_DD  1.000000000
							6683   FCNLIVETREES_LIT  1.000000000
							6683   FCNLIVETREES_SIM  1.000000000
							6683     FCNOVERHANG_DD  1.000000000
							6683    FCNOVERHANG_LIT  1.000000000
							6683    FCNOVERHANG_SIM  1.000000000
							6683        FCNSNAGS_DD  1.000000000
							6683       FCNSNAGS_LIT  1.000000000
							6683       FCNSNAGS_SIM  1.000000000
							6683   FCNSTRUCTURES_DD  1.000000000
							6683  FCNSTRUCTURES_LIT  1.000000000
							6683  FCNSTRUCTURES_SIM  1.000000000
							7263      FCNAQUATIC_DD  9.000000000
							7263     FCNAQUATIC_LIT 10.000000000
							7263     FCNAQUATIC_SIM  9.000000000
							7263     FCNBOULDERS_DD  9.000000000
							7263    FCNBOULDERS_LIT 10.000000000
							7263    FCNBOULDERS_SIM  9.000000000
							7263        FCNBRUSH_DD  9.000000000
							7263       FCNBRUSH_LIT 10.000000000
							7263       FCNBRUSH_SIM  9.000000000
							7263       FCNLEDGES_DD  9.000000000
							7263      FCNLEDGES_LIT 10.000000000
							7263      FCNLEDGES_SIM  9.000000000
							7263    FCNLIVETREES_DD  9.000000000
							7263   FCNLIVETREES_LIT 10.000000000
							7263   FCNLIVETREES_SIM  9.000000000
							7263     FCNOVERHANG_DD  9.000000000
							7263    FCNOVERHANG_LIT 10.000000000
							7263    FCNOVERHANG_SIM  9.000000000
							7263        FCNSNAGS_DD  9.000000000
							7263       FCNSNAGS_LIT 10.000000000
							7263       FCNSNAGS_SIM  9.000000000
							7263   FCNSTRUCTURES_DD  9.000000000
							7263  FCNSTRUCTURES_LIT 10.000000000
							7263  FCNSTRUCTURES_SIM  9.000000000
							7913      FCNAQUATIC_DD 10.000000000
							7913     FCNAQUATIC_LIT 10.000000000
							7913     FCNAQUATIC_SIM 10.000000000
							7913     FCNBOULDERS_DD 10.000000000
							7913    FCNBOULDERS_LIT  2.000000000
							7913    FCNBOULDERS_SIM  2.000000000
							7913        FCNBRUSH_DD 10.000000000
							7913       FCNBRUSH_LIT  2.000000000
							7913       FCNBRUSH_SIM  2.000000000
							7913       FCNLEDGES_DD 10.000000000
							7913      FCNLEDGES_LIT  0.000000000
							7913      FCNLEDGES_SIM  0.000000000
							7913    FCNLIVETREES_DD 10.000000000
							7913   FCNLIVETREES_LIT  0.000000000
							7913   FCNLIVETREES_SIM  0.000000000
							7913     FCNOVERHANG_DD 10.000000000
							7913    FCNOVERHANG_LIT  7.000000000
							7913    FCNOVERHANG_SIM  7.000000000
							7913        FCNSNAGS_DD 10.000000000
							7913       FCNSNAGS_LIT  2.000000000
							7913       FCNSNAGS_SIM  2.000000000
							7913   FCNSTRUCTURES_DD 10.000000000
							7913  FCNSTRUCTURES_LIT  0.000000000
							7913  FCNSTRUCTURES_SIM  0.000000000
						 1000057      FCNAQUATIC_DD 10.000000000
						 1000057     FCNAQUATIC_LIT  6.000000000
						 1000057     FCNAQUATIC_SIM  6.000000000
						 1000057     FCNBOULDERS_DD 10.000000000
						 1000057    FCNBOULDERS_LIT  8.000000000
						 1000057    FCNBOULDERS_SIM  8.000000000
						 1000057        FCNBRUSH_DD 10.000000000
						 1000057       FCNBRUSH_LIT 10.000000000
						 1000057       FCNBRUSH_SIM 10.000000000
						 1000057       FCNLEDGES_DD 10.000000000
						 1000057      FCNLEDGES_LIT  1.000000000
						 1000057      FCNLEDGES_SIM  1.000000000
						 1000057    FCNLIVETREES_DD 10.000000000
						 1000057   FCNLIVETREES_LIT  1.000000000
						 1000057   FCNLIVETREES_SIM  1.000000000
						 1000057     FCNOVERHANG_DD 10.000000000
						 1000057    FCNOVERHANG_LIT 10.000000000
						 1000057    FCNOVERHANG_SIM 10.000000000
						 1000057        FCNSNAGS_DD 10.000000000
						 1000057       FCNSNAGS_LIT  3.000000000
						 1000057       FCNSNAGS_SIM  3.000000000
						 1000057   FCNSTRUCTURES_DD 10.000000000
						 1000057  FCNSTRUCTURES_LIT  0.000000000
						 1000057  FCNSTRUCTURES_SIM  0.000000000
							6160      FCINATURAL_DD  0.450000000
							6160     FCINATURAL_LIT  0.212500000
							6160     FCINATURAL_SIM  0.375550000
							6189      FCINATURAL_DD  0.000000000
							6189     FCINATURAL_LIT  0.725000000
							6189     FCINATURAL_SIM  0.725000000
							6227      FCINATURAL_DD  0.167500000
							6227     FCINATURAL_LIT  0.582500000
							6227     FCINATURAL_SIM  0.571575000
							6235      FCINATURAL_DD  0.000000000
							6235     FCINATURAL_LIT  0.866666667
							6235     FCINATURAL_SIM  0.866666667
							6281      FCINATURAL_DD  0.000000000
							6281     FCINATURAL_LIT  0.022222222
							6281     FCINATURAL_SIM  0.002222222
							6449      FCINATURAL_DD  0.337689394
							6449     FCINATURAL_LIT  0.129166667
							6449     FCINATURAL_SIM  0.140160985
							6683      FCINATURAL_DD  0.000000000
							6683     FCINATURAL_LIT  0.875000000
							6683     FCINATURAL_SIM  0.000000000
							7263      FCINATURAL_DD  0.005555556
							7263     FCINATURAL_LIT  0.200000000
							7263     FCINATURAL_SIM  0.200000000
							7913      FCINATURAL_DD  0.000000000
							7913     FCINATURAL_LIT  0.797500000
							7913     FCINATURAL_SIM  0.797500000
						 1000057      FCINATURAL_DD  0.000000000
						 1000057     FCINATURAL_LIT  0.791666667
						 1000057     FCINATURAL_SIM  0.791666667
							6160          FCIALL_DD  0.475000000
							6160         FCIALL_LIT  0.237500000
							6160         FCIALL_SIM  0.400550000
							6189          FCIALL_DD  0.000000000
							6189         FCIALL_LIT  0.725000000
							6189         FCIALL_SIM  0.725000000
							6227          FCIALL_DD  0.167500000
							6227         FCIALL_LIT  0.587500000
							6227         FCIALL_SIM  0.576575000
							6235          FCIALL_DD  0.000000000
							6235         FCIALL_LIT  0.866666667
							6235         FCIALL_SIM  0.866666667
							6281          FCIALL_DD  0.000000000
							6281         FCIALL_LIT  0.022222222
							6281         FCIALL_SIM  0.002222222
							6449          FCIALL_DD  0.371022727
							6449         FCIALL_LIT  0.158333333
							6449         FCIALL_SIM  0.169535985
							6683          FCIALL_DD  0.000000000
							6683         FCIALL_LIT  0.875000000
							6683         FCIALL_SIM  0.000000000
							7263          FCIALL_DD  0.005555556
							7263         FCIALL_LIT  0.205000000
							7263         FCIALL_SIM  0.205555556
							7913          FCIALL_DD  0.000000000
							7913         FCIALL_LIT  0.797500000
							7913         FCIALL_SIM  0.797500000
						 1000057          FCIALL_DD  0.000000000
						 1000057         FCIALL_LIT  0.791666667
						 1000057         FCIALL_SIM  0.791666667
							6160          FCIBIG_DD  0.085000000
							6160         FCIBIG_LIT  0.087500000
							6160         FCIBIG_SIM  0.081550000
							6189          FCIBIG_DD  0.000000000
							6189         FCIBIG_LIT  0.247500000
							6189         FCIBIG_SIM  0.247500000
							6227          FCIBIG_DD  0.097500000
							6227         FCIBIG_LIT  0.197500000
							6227         FCIBIG_SIM  0.192900000
							6235          FCIBIG_DD  0.000000000
							6235         FCIBIG_LIT  0.733333333
							6235         FCIBIG_SIM  0.733333333
							6281          FCIBIG_DD  0.000000000
							6281         FCIBIG_LIT  0.022222222
							6281         FCIBIG_SIM  0.002222222
							6449          FCIBIG_DD  0.233522727
							6449         FCIBIG_LIT  0.075000000
							6449         FCIBIG_SIM  0.084452652
							6683          FCIBIG_DD  0.000000000
							6683         FCIBIG_LIT  0.000000000
							6683         FCIBIG_SIM  0.000000000
							7263          FCIBIG_DD  0.000000000
							7263         FCIBIG_LIT  0.115000000
							7263         FCIBIG_SIM  0.116666667
							7913          FCIBIG_DD  0.000000000
							7913         FCIBIG_LIT  0.100000000
							7913         FCIBIG_SIM  0.100000000
						 1000057          FCIBIG_DD  0.000000000
						 1000057         FCIBIG_LIT  0.400000000
						 1000057         FCIBIG_SIM  0.400000000
							6160       FCIRIPVEG_DD  0.132500000
							6160      FCIRIPVEG_LIT  0.040000000
							6160      FCIRIPVEG_SIM  0.105850000
							6189       FCIRIPVEG_DD  0.000000000
							6189      FCIRIPVEG_LIT  0.065000000
							6189      FCIRIPVEG_SIM  0.065000000
							6227       FCIRIPVEG_DD  0.040000000
							6227      FCIRIPVEG_LIT  0.035000000
							6227      FCIRIPVEG_SIM  0.038400000
							6235       FCIRIPVEG_DD  0.000000000
							6235      FCIRIPVEG_LIT  0.088888889
							6235      FCIRIPVEG_SIM  0.088888889
							6281       FCIRIPVEG_DD  0.000000000
							6281      FCIRIPVEG_LIT  0.000000000
							6281      FCIRIPVEG_SIM  0.000000000
							6449       FCIRIPVEG_DD  0.091666667
							6449      FCIRIPVEG_LIT  0.033333333
							6449      FCIRIPVEG_SIM  0.035333333
							6683       FCIRIPVEG_DD  0.000000000
							6683      FCIRIPVEG_LIT  0.000000000
							6683      FCIRIPVEG_SIM  0.000000000
							7263       FCIRIPVEG_DD  0.000000000
							7263      FCIRIPVEG_LIT  0.040000000
							7263      FCIRIPVEG_SIM  0.038888889
							7913       FCIRIPVEG_DD  0.000000000
							7913      FCIRIPVEG_LIT  0.050000000
							7913      FCIRIPVEG_SIM  0.050000000
						 1000057       FCIRIPVEG_DD  0.000000000
						 1000057      FCIRIPVEG_LIT  0.083333333
						 1000057      FCIRIPVEG_SIM  0.083333333
							6160          FCNALL_DD 10.000000000
							6160         FCNALL_LIT 10.000000000
							6160         FCNALL_SIM 10.000000000
							6189          FCNALL_DD 10.000000000
							6189         FCNALL_LIT 10.000000000
							6189         FCNALL_SIM 10.000000000
							6227          FCNALL_DD 10.000000000
							6227         FCNALL_LIT 10.000000000
							6227         FCNALL_SIM 10.000000000
							6235          FCNALL_DD 10.000000000
							6235         FCNALL_LIT  9.000000000
							6235         FCNALL_SIM  9.000000000
							6281          FCNALL_DD  9.000000000
							6281         FCNALL_LIT  9.000000000
							6281         FCNALL_SIM  9.000000000
							6449          FCNALL_DD 12.000000000
							6449         FCNALL_LIT 12.000000000
							6449         FCNALL_SIM 12.000000000
							6683          FCNALL_DD  1.000000000
							6683         FCNALL_LIT  1.000000000
							6683         FCNALL_SIM  1.000000000
							7263          FCNALL_DD  9.000000000
							7263         FCNALL_LIT 10.000000000
							7263         FCNALL_SIM  9.000000000
							7913          FCNALL_DD 10.000000000
							7913         FCNALL_LIT 10.000000000
							7913         FCNALL_SIM 10.000000000
						 1000057          FCNALL_DD 10.000000000
						 1000057         FCNALL_LIT 10.000000000
						 1000057         FCNALL_SIM 10.000000000
							6160         FCFPALL_DD  1.000000000
							6160        FCFPALL_LIT  1.000000000
							6160        FCFPALL_SIM  1.000000000
							6189         FCFPALL_DD  0.000000000
							6189        FCFPALL_LIT  1.000000000
							6189        FCFPALL_SIM  1.000000000
							6227         FCFPALL_DD  0.400000000
							6227        FCFPALL_LIT  0.900000000
							6227        FCFPALL_SIM  1.000000000
							6235         FCFPALL_DD  0.000000000
							6235        FCFPALL_LIT  1.000000000
							6235        FCFPALL_SIM  1.000000000
							6281         FCFPALL_DD  0.000000000
							6281        FCFPALL_LIT  0.444444444
							6281        FCFPALL_SIM  0.111111111
							6449         FCFPALL_DD  1.000000000
							6449        FCFPALL_LIT  1.000000000
							6449        FCFPALL_SIM  1.000000000
							6683         FCFPALL_DD  0.000000000
							6683        FCFPALL_LIT  1.000000000
							6683        FCFPALL_SIM  0.000000000
							7263         FCFPALL_DD  0.111111111
							7263        FCFPALL_LIT  1.000000000
							7263        FCFPALL_SIM  1.000000000
							7913         FCFPALL_DD  0.000000000
							7913        FCFPALL_LIT  1.000000000
							7913        FCFPALL_SIM  1.000000000
						 1000057         FCFPALL_DD  0.000000000
						 1000057        FCFPALL_LIT  1.000000000
						 1000057        FCFPALL_SIM  1.000000000

						 ")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	within(fake
		  ,{SAMPLE_TYPE <- 'PHAB'
			FLAG <- as.character(NA)
			FORM_TYPE <- 'PHAB_FRONT'
		   }
		  )
	return(fake)
		 
}
					


nlaFishCoverTest.expectedResultsWithDrawDownWith15mRiparian <- function()
# Expected results for test data with drawdown values.
# Using 15m maximum riparian drawdown.  DEPRECATED
{
	tc <- textConnection("	SITE           METRIC                                  VALUE
							6160    FCFCAQUATIC_LIT           0.110000000000000000555112
							6160     FCFCAQUATIC_DD           0.257500000000000006661338
							6160    FCFCAQUATIC_SYN0          0.196266666666666672602659
							6160   FCFCBOULDERS_LIT           0.000000000000000000000000
							6160    FCFCBOULDERS_DD           0.000000000000000000000000
							6160   FCFCBOULDERS_SYN0          0.000000000000000000000000
							6160      FCFCBRUSH_LIT           0.035000000000000003330669
							6160       FCFCBRUSH_DD           0.127500000000000002220446
							6160      FCFCBRUSH_SYN0          0.080566666666666675311603
							6160     FCFCLEDGES_LIT           0.057499999999999995559108
							6160      FCFCLEDGES_DD           0.000000000000000000000000
							6160     FCFCLEDGES_SYN0          0.033733333333333330450454
							6160  FCFCLIVETREES_LIT           0.005000000000000000104083
							6160   FCFCLIVETREES_DD           0.000000000000000000000000
							6160  FCFCLIVETREES_SYN0          0.003566666666666667203506
							6160   FCFCOVERHANG_LIT           0.005000000000000000104083
							6160    FCFCOVERHANG_DD           0.060000000000000004718448
							6160   FCFCOVERHANG_SYN0          0.024800000000000002597922
							6160      FCFCSNAGS_LIT           0.000000000000000000000000
							6160       FCFCSNAGS_DD           0.005000000000000000104083
							6160      FCFCSNAGS_SYN0          0.001433333333333333551099
							6160 FCFCSTRUCTURES_LIT           0.025000000000000001387779
							6160  FCFCSTRUCTURES_DD           0.025000000000000001387779
							6160 FCFCSTRUCTURES_SYN0          0.025000000000000001387779
							6160        FCFPALL_LIT           1.000000000000000000000000
							6160         FCFPALL_DD           1.000000000000000000000000
							6160        FCFPALL_SYN0          1.000000000000000000000000
							6160    FCFPAQUATIC_LIT           1.000000000000000000000000
							6160     FCFPAQUATIC_DD           1.000000000000000000000000
							6160    FCFPAQUATIC_SYN0          1.000000000000000000000000
							6160   FCFPBOULDERS_LIT           0.000000000000000000000000
							6160    FCFPBOULDERS_DD           0.000000000000000000000000
							6160   FCFPBOULDERS_SYN0          0.000000000000000000000000
							6160      FCFPBRUSH_LIT           0.699999999999999955591079
							6160       FCFPBRUSH_DD           0.900000000000000022204460
							6160      FCFPBRUSH_SYN0          0.900000000000000022204460
							6160     FCFPLEDGES_LIT           0.100000000000000005551115
							6160      FCFPLEDGES_DD           0.000000000000000000000000
							6160     FCFPLEDGES_SYN0          0.100000000000000005551115
							6160  FCFPLIVETREES_LIT           0.100000000000000005551115
							6160   FCFPLIVETREES_DD           0.000000000000000000000000
							6160  FCFPLIVETREES_SYN0          0.100000000000000005551115
							6160   FCFPOVERHANG_LIT           0.100000000000000005551115
							6160    FCFPOVERHANG_DD           0.800000000000000044408921
							6160   FCFPOVERHANG_SYN0          0.800000000000000044408921
							6160      FCFPSNAGS_LIT           0.000000000000000000000000
							6160       FCFPSNAGS_DD           0.100000000000000005551115
							6160      FCFPSNAGS_SYN0          0.100000000000000005551115
							6160 FCFPSTRUCTURES_LIT           0.100000000000000005551115
							6160  FCFPSTRUCTURES_DD           0.100000000000000005551115
							6160 FCFPSTRUCTURES_SYN0          0.100000000000000005551115
							6160         FCIALL_LIT           0.237499999999999988897770
							6160          FCIALL_DD           0.475000000000000033306691
							6160         FCIALL_SYN0          0.365366666666666672913522
							6160         FCIBIG_LIT           0.087499999999999994448885
							6160          FCIBIG_DD           0.085000000000000006106227
							6160         FCIBIG_SYN0          0.083533333333333334436155
							6160     FCINATURAL_LIT           0.212499999999999994448885
							6160      FCINATURAL_DD           0.450000000000000011102230
							6160     FCINATURAL_SYN0          0.340366666666666706220212
							6160      FCIRIPVEG_LIT           0.040000000000000000832667
							6160       FCIRIPVEG_DD           0.132500000000000006661338
							6160      FCIRIPVEG_SYN0          0.085566666666666679752495
							6160         FCNALL_LIT          10.000000000000000000000000
							6160          FCNALL_DD          10.000000000000000000000000
							6160         FCNALL_SYN0         10.000000000000000000000000
							6160     FCNAQUATIC_LIT          10.000000000000000000000000
							6160      FCNAQUATIC_DD          10.000000000000000000000000
							6160     FCNAQUATIC_SYN0         10.000000000000000000000000
							6160    FCNBOULDERS_LIT          10.000000000000000000000000
							6160     FCNBOULDERS_DD          10.000000000000000000000000
							6160    FCNBOULDERS_SYN0         10.000000000000000000000000
							6160       FCNBRUSH_LIT          10.000000000000000000000000
							6160        FCNBRUSH_DD          10.000000000000000000000000
							6160       FCNBRUSH_SYN0         10.000000000000000000000000
							6160      FCNLEDGES_LIT          10.000000000000000000000000
							6160       FCNLEDGES_DD          10.000000000000000000000000
							6160      FCNLEDGES_SYN0         10.000000000000000000000000
							6160   FCNLIVETREES_LIT          10.000000000000000000000000
							6160    FCNLIVETREES_DD          10.000000000000000000000000
							6160   FCNLIVETREES_SYN0         10.000000000000000000000000
							6160    FCNOVERHANG_LIT          10.000000000000000000000000
							6160     FCNOVERHANG_DD          10.000000000000000000000000
							6160    FCNOVERHANG_SYN0         10.000000000000000000000000
							6160       FCNSNAGS_LIT          10.000000000000000000000000
							6160        FCNSNAGS_DD          10.000000000000000000000000
							6160       FCNSNAGS_SYN0         10.000000000000000000000000
							6160  FCNSTRUCTURES_LIT          10.000000000000000000000000
							6160   FCNSTRUCTURES_DD          10.000000000000000000000000
							6160  FCNSTRUCTURES_SYN0         10.000000000000000000000000
							6160     FCVAQUATIC_LIT           0.096609178307929588491731
							6160      FCVAQUATIC_DD           0.305743628987860782686425
							6160     FCVAQUATIC_SYN0          0.182482439540742474859414
							6160    FCVBOULDERS_LIT           0.000000000000000000000000
							6160     FCVBOULDERS_DD           0.000000000000000000000000
							6160    FCVBOULDERS_SYN0          0.000000000000000000000000
							6160       FCVBRUSH_LIT           0.024152294576982397122933
							6160        FCVBRUSH_DD           0.263114360434140215350141
							6160       FCVBRUSH_SYN0          0.125716715645106613674642
							6160      FCVLEDGES_LIT           0.181830965459681803686465
							6160       FCVLEDGES_DD           0.000000000000000000000000
							6160      FCVLEDGES_SYN0          0.106674166403013331860805
							6160   FCVLIVETREES_LIT           0.015811388300841895671045
							6160    FCVLIVETREES_DD           0.000000000000000000000000
							6160   FCVLIVETREES_SYN0          0.011278790321267221502533
							6160    FCVOVERHANG_LIT           0.015811388300841895671045
							6160     FCVOVERHANG_DD           0.069920589878010100393091
							6160    FCVOVERHANG_SYN0          0.042176934776729499421144
							6160       FCVSNAGS_LIT           0.000000000000000000000000
							6160        FCVSNAGS_DD           0.015811388300841895671045
							6160       FCVSNAGS_SYN0          0.004532597979574677637959
							6160  FCVSTRUCTURES_LIT           0.079056941504209485294119
							6160   FCVSTRUCTURES_DD           0.079056941504209485294119
							6160  FCVSTRUCTURES_SYN0          0.079056941504209485294119
							6189    FCFCAQUATIC_LIT           0.412499999999999977795540
							6189    FCFCAQUATIC_SYN0                        NaN
							6189   FCFCBOULDERS_LIT           0.142499999999999987787547
							6189   FCFCBOULDERS_SYN0                        NaN
							6189      FCFCBRUSH_LIT           0.065000000000000002220446
							6189      FCFCBRUSH_SYN0                        NaN
							6189     FCFCLEDGES_LIT           0.015000000000000001179612
							6189     FCFCLEDGES_SYN0                        NaN
							6189  FCFCLIVETREES_LIT           0.000000000000000000000000
							6189  FCFCLIVETREES_SYN0                        NaN
							6189   FCFCOVERHANG_LIT           0.089999999999999996669331
							6189   FCFCOVERHANG_SYN0                        NaN
							6189      FCFCSNAGS_LIT           0.000000000000000000000000
							6189      FCFCSNAGS_SYN0                        NaN
							6189 FCFCSTRUCTURES_LIT           0.000000000000000000000000
							6189 FCFCSTRUCTURES_SYN0                        NaN
							6189        FCFPALL_LIT           1.000000000000000000000000
							6189        FCFPALL_SYN0                        NaN
							6189    FCFPAQUATIC_LIT           1.000000000000000000000000
							6189    FCFPAQUATIC_SYN0                        NaN
							6189   FCFPBOULDERS_LIT           0.599999999999999977795540
							6189   FCFPBOULDERS_SYN0                        NaN
							6189      FCFPBRUSH_LIT           0.900000000000000022204460
							6189      FCFPBRUSH_SYN0                        NaN
							6189     FCFPLEDGES_LIT           0.299999999999999988897770
							6189     FCFPLEDGES_SYN0                        NaN
							6189  FCFPLIVETREES_LIT           0.000000000000000000000000
							6189  FCFPLIVETREES_SYN0                        NaN
							6189   FCFPOVERHANG_LIT           1.000000000000000000000000
							6189   FCFPOVERHANG_SYN0                        NaN
							6189      FCFPSNAGS_LIT           0.000000000000000000000000
							6189      FCFPSNAGS_SYN0                        NaN
							6189 FCFPSTRUCTURES_LIT           0.000000000000000000000000
							6189 FCFPSTRUCTURES_SYN0                        NaN
							6189         FCIALL_LIT           0.724999999999999977795540
							6189         FCIALL_SYN0          0.000000000000000000000000
							6189         FCIBIG_LIT           0.247499999999999997779554
							6189         FCIBIG_SYN0          0.000000000000000000000000
							6189     FCINATURAL_LIT           0.724999999999999977795540
							6189     FCINATURAL_SYN0          0.000000000000000000000000
							6189      FCIRIPVEG_LIT           0.065000000000000002220446
							6189      FCIRIPVEG_SYN0          0.000000000000000000000000
							6189         FCNALL_LIT          10.000000000000000000000000
							6189         FCNALL_SYN0          0.000000000000000000000000
							6189     FCNAQUATIC_LIT          10.000000000000000000000000
							6189      FCNAQUATIC_DD           0.000000000000000000000000
							6189     FCNAQUATIC_SYN0          0.000000000000000000000000
							6189    FCNBOULDERS_LIT          10.000000000000000000000000
							6189     FCNBOULDERS_DD           0.000000000000000000000000
							6189    FCNBOULDERS_SYN0          0.000000000000000000000000
							6189       FCNBRUSH_LIT          10.000000000000000000000000
							6189        FCNBRUSH_DD           0.000000000000000000000000
							6189       FCNBRUSH_SYN0          0.000000000000000000000000
							6189      FCNLEDGES_LIT          10.000000000000000000000000
							6189       FCNLEDGES_DD           0.000000000000000000000000
							6189      FCNLEDGES_SYN0          0.000000000000000000000000
							6189   FCNLIVETREES_LIT          10.000000000000000000000000
							6189    FCNLIVETREES_DD           0.000000000000000000000000
							6189   FCNLIVETREES_SYN0          0.000000000000000000000000
							6189    FCNOVERHANG_LIT          10.000000000000000000000000
							6189     FCNOVERHANG_DD           0.000000000000000000000000
							6189    FCNOVERHANG_SYN0          0.000000000000000000000000
							6189       FCNSNAGS_LIT          10.000000000000000000000000
							6189        FCNSNAGS_DD           0.000000000000000000000000
							6189       FCNSNAGS_SYN0          0.000000000000000000000000
							6189  FCNSTRUCTURES_LIT          10.000000000000000000000000
							6189   FCNSTRUCTURES_DD           0.000000000000000000000000
							6189  FCNSTRUCTURES_SYN0          0.000000000000000000000000
							6189     FCVAQUATIC_LIT           0.171290039925787201946505
							6189     FCVAQUATIC_SYN0                         NA
							6189    FCVBOULDERS_LIT           0.188580221656461100021573
							6189    FCVBOULDERS_SYN0                         NA
							6189       FCVBRUSH_LIT           0.066874675492462926085224
							6189       FCVBRUSH_SYN0                         NA
							6189      FCVLEDGES_LIT           0.024152294576982397122933
							6189      FCVLEDGES_SYN0                         NA
							6189   FCVLIVETREES_LIT           0.000000000000000000000000
							6189   FCVLIVETREES_SYN0                         NA
							6189    FCVOVERHANG_LIT           0.084327404271156772286311
							6189    FCVOVERHANG_SYN0                         NA
							6189       FCVSNAGS_LIT           0.000000000000000000000000
							6189       FCVSNAGS_SYN0                         NA
							6189  FCVSTRUCTURES_LIT           0.000000000000000000000000
							6189  FCVSTRUCTURES_SYN0                         NA
							6227    FCFCAQUATIC_LIT           0.354999999999999982236432
							6227     FCFCAQUATIC_DD           0.059999999999999997779554
							6227    FCFCAQUATIC_SYN0          0.122033333333333313119873
							6227   FCFCBOULDERS_LIT           0.057499999999999995559108
							6227    FCFCBOULDERS_DD           0.114999999999999991118216
							6227   FCFCBOULDERS_SYN0          0.114999999999999991118216
							6227      FCFCBRUSH_LIT           0.010000000000000000208167
							6227       FCFCBRUSH_DD           0.020000000000000000416334
							6227      FCFCBRUSH_SYN0          0.002933333333333333365484
							6227     FCFCLEDGES_LIT           0.005000000000000000104083
							6227      FCFCLEDGES_DD           0.010000000000000000208167
							6227     FCFCLEDGES_SYN0          0.010000000000000000208167
							6227  FCFCLIVETREES_LIT           0.025000000000000001387779
							6227   FCFCLIVETREES_DD           0.059999999999999997779554
							6227  FCFCLIVETREES_SYN0          0.051600000000000000033307
							6227   FCFCOVERHANG_LIT           0.130000000000000004440892
							6227    FCFCOVERHANG_DD           0.070000000000000006661338
							6227   FCFCOVERHANG_SYN0          0.093866666666666667695473
							6227      FCFCSNAGS_LIT           0.000000000000000000000000
							6227       FCFCSNAGS_DD           0.000000000000000000000000
							6227      FCFCSNAGS_SYN0          0.000000000000000000000000
							6227 FCFCSTRUCTURES_LIT           0.005000000000000000104083
							6227  FCFCSTRUCTURES_DD           0.000000000000000000000000
							6227 FCFCSTRUCTURES_SYN0          0.000000000000000000000000
							6227        FCFPALL_LIT           0.900000000000000022204460
							6227         FCFPALL_DD           0.800000000000000044408921
							6227        FCFPALL_SYN0          1.000000000000000000000000
							6227    FCFPAQUATIC_LIT           0.800000000000000044408921
							6227     FCFPAQUATIC_DD           0.400000000000000022204460
							6227    FCFPAQUATIC_SYN0          0.800000000000000044408921
							6227   FCFPBOULDERS_LIT           0.100000000000000005551115
							6227    FCFPBOULDERS_DD           0.200000000000000011102230
							6227   FCFPBOULDERS_SYN0          0.200000000000000011102230
							6227      FCFPBRUSH_LIT           0.200000000000000011102230
							6227       FCFPBRUSH_DD           0.400000000000000022204460
							6227      FCFPBRUSH_SYN0          0.400000000000000022204460
							6227     FCFPLEDGES_LIT           0.100000000000000005551115
							6227      FCFPLEDGES_DD           0.200000000000000011102230
							6227     FCFPLEDGES_SYN0          0.200000000000000011102230
							6227  FCFPLIVETREES_LIT           0.100000000000000005551115
							6227   FCFPLIVETREES_DD           0.400000000000000022204460
							6227  FCFPLIVETREES_SYN0          0.400000000000000022204460
							6227   FCFPOVERHANG_LIT           0.599999999999999977795540
							6227    FCFPOVERHANG_DD           0.599999999999999977795540
							6227   FCFPOVERHANG_SYN0          0.599999999999999977795540
							6227      FCFPSNAGS_LIT           0.000000000000000000000000
							6227       FCFPSNAGS_DD           0.000000000000000000000000
							6227      FCFPSNAGS_SYN0          0.000000000000000000000000
							6227 FCFPSTRUCTURES_LIT           0.100000000000000005551115
							6227  FCFPSTRUCTURES_DD           0.000000000000000000000000
							6227 FCFPSTRUCTURES_SYN0          0.000000000000000000000000
							6227         FCIALL_LIT           0.587500000000000022204460
							6227          FCIALL_DD           0.335000000000000019984014
							6227         FCIALL_SYN0          0.395433333333333303372115
							6227         FCIBIG_LIT           0.197500000000000008881784
							6227          FCIBIG_DD           0.195000000000000006661338
							6227         FCIBIG_SYN0          0.218866666666666653817686
							6227     FCINATURAL_LIT           0.582500000000000017763568
							6227      FCINATURAL_DD           0.335000000000000019984014
							6227     FCINATURAL_SYN0          0.395433333333333303372115
							6227      FCIRIPVEG_LIT           0.035000000000000003330669
							6227       FCIRIPVEG_DD           0.080000000000000001665335
							6227      FCIRIPVEG_SYN0          0.054533333333333336434556
							6227         FCNALL_LIT          10.000000000000000000000000
							6227          FCNALL_DD           5.000000000000000000000000
							6227         FCNALL_SYN0          5.000000000000000000000000
							6227     FCNAQUATIC_LIT          10.000000000000000000000000
							6227      FCNAQUATIC_DD           5.000000000000000000000000
							6227     FCNAQUATIC_SYN0          5.000000000000000000000000
							6227    FCNBOULDERS_LIT          10.000000000000000000000000
							6227     FCNBOULDERS_DD           5.000000000000000000000000
							6227    FCNBOULDERS_SYN0          5.000000000000000000000000
							6227       FCNBRUSH_LIT          10.000000000000000000000000
							6227        FCNBRUSH_DD           5.000000000000000000000000
							6227       FCNBRUSH_SYN0          5.000000000000000000000000
							6227      FCNLEDGES_LIT          10.000000000000000000000000
							6227       FCNLEDGES_DD           5.000000000000000000000000
							6227      FCNLEDGES_SYN0          5.000000000000000000000000
							6227   FCNLIVETREES_LIT          10.000000000000000000000000
							6227    FCNLIVETREES_DD           5.000000000000000000000000
							6227   FCNLIVETREES_SYN0          5.000000000000000000000000
							6227    FCNOVERHANG_LIT          10.000000000000000000000000
							6227     FCNOVERHANG_DD           5.000000000000000000000000
							6227    FCNOVERHANG_SYN0          5.000000000000000000000000
							6227       FCNSNAGS_LIT          10.000000000000000000000000
							6227        FCNSNAGS_DD           5.000000000000000000000000
							6227       FCNSNAGS_SYN0          5.000000000000000000000000
							6227  FCNSTRUCTURES_LIT          10.000000000000000000000000
							6227   FCNSTRUCTURES_DD           5.000000000000000000000000
							6227  FCNSTRUCTURES_SYN0          5.000000000000000000000000
							6227     FCVAQUATIC_LIT           0.284507371511608542213878
							6227      FCVAQUATIC_DD           0.108397416943393998245426
							6227     FCVAQUATIC_SYN0          0.219073947434295396297088
							6227    FCVBOULDERS_LIT           0.181830965459681803686465
							6227     FCVBOULDERS_DD           0.257147817412475776510661
							6227    FCVBOULDERS_SYN0          0.257147817412475776510661
							6227       FCVBRUSH_LIT           0.021081851067789196541025
							6227        FCVBRUSH_DD           0.027386127875258306496598
							6227       FCVBRUSH_SYN0          0.004044200236827493778746
							6227      FCVLEDGES_LIT           0.015811388300841895671045
							6227       FCVLEDGES_DD           0.022360679774997897079070
							6227      FCVLEDGES_SYN0          0.022360679774997897079070
							6227   FCVLIVETREES_LIT           0.079056941504209485294119
							6227    FCVLIVETREES_DD           0.108397416943393998245426
							6227   FCVLIVETREES_SYN0          0.110963056915353588949280
							6227    FCVOVERHANG_LIT           0.127366487830285340931979
							6227     FCVOVERHANG_DD           0.103682206766638598804953
							6227    FCVOVERHANG_SYN0          0.125669230743072318778886
							6227       FCVSNAGS_LIT           0.000000000000000000000000
							6227        FCVSNAGS_DD           0.000000000000000000000000
							6227       FCVSNAGS_SYN0          0.000000000000000000000000
							6227  FCVSTRUCTURES_LIT           0.015811388300841895671045
							6227   FCVSTRUCTURES_DD           0.000000000000000000000000
							6227  FCVSTRUCTURES_SYN0          0.000000000000000000000000
							6235    FCFCAQUATIC_LIT           0.044444444444444446140619
							6235    FCFCAQUATIC_SYN0                        NaN
							6235      FCFCBRUSH_LIT           0.072222222222222229315314
							6235      FCFCBRUSH_SYN0                        NaN
							6235     FCFCLEDGES_LIT           0.602777777777777745704668
							6235     FCFCLEDGES_SYN0                        NaN
							6235  FCFCLIVETREES_LIT           0.005555555555555555767577
							6235  FCFCLIVETREES_SYN0                        NaN
							6235   FCFCOVERHANG_LIT           0.130555555555555563573833
							6235   FCFCOVERHANG_SYN0                        NaN
							6235      FCFCSNAGS_LIT           0.011111111111111111535155
							6235      FCFCSNAGS_SYN0                        NaN
							6235 FCFCSTRUCTURES_LIT           0.000000000000000000000000
							6235 FCFCSTRUCTURES_SYN0                        NaN
							6235        FCFPALL_LIT           1.000000000000000000000000
							6235        FCFPALL_SYN0                        NaN
							6235    FCFPAQUATIC_LIT           0.888888888888888839545643
							6235    FCFPAQUATIC_SYN0                        NaN
							6235      FCFPBRUSH_LIT           1.000000000000000000000000
							6235      FCFPBRUSH_SYN0                        NaN
							6235     FCFPLEDGES_LIT           1.000000000000000000000000
							6235     FCFPLEDGES_SYN0                        NaN
							6235  FCFPLIVETREES_LIT           0.111111111111111104943205
							6235  FCFPLIVETREES_SYN0                        NaN
							6235   FCFPOVERHANG_LIT           1.000000000000000000000000
							6235   FCFPOVERHANG_SYN0                        NaN
							6235      FCFPSNAGS_LIT           0.222222222222222209886411
							6235      FCFPSNAGS_SYN0                        NaN
							6235 FCFPSTRUCTURES_LIT           0.000000000000000000000000
							6235 FCFPSTRUCTURES_SYN0                        NaN
							6235         FCIALL_LIT           0.866666666666666696272614
							6235         FCIALL_SYN0          0.000000000000000000000000
							6235         FCIBIG_LIT           0.733333333333333281522926
							6235         FCIBIG_SYN0          0.000000000000000000000000
							6235     FCINATURAL_LIT           0.866666666666666696272614
							6235     FCINATURAL_SYN0          0.000000000000000000000000
							6235      FCIRIPVEG_LIT           0.088888888888888892281237
							6235      FCIRIPVEG_SYN0          0.000000000000000000000000
							6235         FCNALL_LIT           9.000000000000000000000000
							6235         FCNALL_SYN0          0.000000000000000000000000
							6235     FCNAQUATIC_LIT           9.000000000000000000000000
							6235      FCNAQUATIC_DD           0.000000000000000000000000
							6235     FCNAQUATIC_SYN0          0.000000000000000000000000
							6235    FCNBOULDERS_LIT           0.000000000000000000000000
							6235     FCNBOULDERS_DD           0.000000000000000000000000
							6235    FCNBOULDERS_SYN0          0.000000000000000000000000
							6235       FCNBRUSH_LIT           9.000000000000000000000000
							6235        FCNBRUSH_DD           0.000000000000000000000000
							6235       FCNBRUSH_SYN0          0.000000000000000000000000
							6235      FCNLEDGES_LIT           9.000000000000000000000000
							6235       FCNLEDGES_DD           0.000000000000000000000000
							6235      FCNLEDGES_SYN0          0.000000000000000000000000
							6235   FCNLIVETREES_LIT           9.000000000000000000000000
							6235    FCNLIVETREES_DD           0.000000000000000000000000
							6235   FCNLIVETREES_SYN0          0.000000000000000000000000
							6235    FCNOVERHANG_LIT           9.000000000000000000000000
							6235     FCNOVERHANG_DD           0.000000000000000000000000
							6235    FCNOVERHANG_SYN0          0.000000000000000000000000
							6235       FCNSNAGS_LIT           9.000000000000000000000000
							6235        FCNSNAGS_DD           0.000000000000000000000000
							6235       FCNSNAGS_SYN0          0.000000000000000000000000
							6235  FCNSTRUCTURES_LIT           9.000000000000000000000000
							6235   FCNSTRUCTURES_DD           0.000000000000000000000000
							6235  FCNSTRUCTURES_SYN0          0.000000000000000000000000
							6235     FCVAQUATIC_LIT           0.016666666666666669904817
							6235     FCVAQUATIC_SYN0                         NA
							6235       FCVBRUSH_LIT           0.066666666666666665741481
							6235       FCVBRUSH_SYN0                         NA
							6235      FCVLEDGES_LIT           0.243491672228116351472949
							6235      FCVLEDGES_SYN0                         NA
							6235   FCVLIVETREES_LIT           0.016666666666666669904817
							6235   FCVLIVETREES_SYN0                         NA
							6235    FCVOVERHANG_LIT           0.179311956594583432611500
							6235    FCVOVERHANG_SYN0                         NA
							6235       FCVSNAGS_LIT           0.022047927592204922403463
							6235       FCVSNAGS_SYN0                         NA
							6235  FCVSTRUCTURES_LIT           0.000000000000000000000000
							6235  FCVSTRUCTURES_SYN0                         NA
							6281    FCFCAQUATIC_LIT           0.000000000000000000000000
							6281     FCFCAQUATIC_DD           0.000000000000000000000000
							6281    FCFCAQUATIC_SYN0          0.000000000000000000000000
							6281   FCFCBOULDERS_LIT           0.022222222222222223070309
							6281    FCFCBOULDERS_DD           0.000000000000000000000000
							6281   FCFCBOULDERS_SYN0          0.008888888888888890615902
							6281      FCFCBRUSH_LIT           0.000000000000000000000000
							6281       FCFCBRUSH_DD           0.000000000000000000000000
							6281      FCFCBRUSH_SYN0          0.000000000000000000000000
							6281     FCFCLEDGES_LIT           0.000000000000000000000000
							6281      FCFCLEDGES_DD           0.000000000000000000000000
							6281     FCFCLEDGES_SYN0          0.000000000000000000000000
							6281  FCFCLIVETREES_LIT           0.000000000000000000000000
							6281   FCFCLIVETREES_DD           0.000000000000000000000000
							6281  FCFCLIVETREES_SYN0          0.000000000000000000000000
							6281   FCFCOVERHANG_LIT           0.000000000000000000000000
							6281    FCFCOVERHANG_DD           0.000000000000000000000000
							6281   FCFCOVERHANG_SYN0          0.000000000000000000000000
							6281      FCFCSNAGS_LIT           0.000000000000000000000000
							6281       FCFCSNAGS_DD           0.000000000000000000000000
							6281      FCFCSNAGS_SYN0          0.000000000000000000000000
							6281 FCFCSTRUCTURES_LIT           0.000000000000000000000000
							6281  FCFCSTRUCTURES_DD           0.000000000000000000000000
							6281 FCFCSTRUCTURES_SYN0          0.000000000000000000000000
							6281        FCFPALL_LIT           0.444444444444444419772822
							6281         FCFPALL_DD           0.000000000000000000000000
							6281        FCFPALL_SYN0          0.444444444444444419772822
							6281    FCFPAQUATIC_LIT           0.000000000000000000000000
							6281     FCFPAQUATIC_DD           0.000000000000000000000000
							6281    FCFPAQUATIC_SYN0          0.000000000000000000000000
							6281   FCFPBOULDERS_LIT           0.444444444444444419772822
							6281    FCFPBOULDERS_DD           0.000000000000000000000000
							6281   FCFPBOULDERS_SYN0          0.444444444444444419772822
							6281      FCFPBRUSH_LIT           0.000000000000000000000000
							6281       FCFPBRUSH_DD           0.000000000000000000000000
							6281      FCFPBRUSH_SYN0          0.000000000000000000000000
							6281     FCFPLEDGES_LIT           0.000000000000000000000000
							6281      FCFPLEDGES_DD           0.000000000000000000000000
							6281     FCFPLEDGES_SYN0          0.000000000000000000000000
							6281  FCFPLIVETREES_LIT           0.000000000000000000000000
							6281   FCFPLIVETREES_DD           0.000000000000000000000000
							6281  FCFPLIVETREES_SYN0          0.000000000000000000000000
							6281   FCFPOVERHANG_LIT           0.000000000000000000000000
							6281    FCFPOVERHANG_DD           0.000000000000000000000000
							6281   FCFPOVERHANG_SYN0          0.000000000000000000000000
							6281      FCFPSNAGS_LIT           0.000000000000000000000000
							6281       FCFPSNAGS_DD           0.000000000000000000000000
							6281      FCFPSNAGS_SYN0          0.000000000000000000000000
							6281 FCFPSTRUCTURES_LIT           0.000000000000000000000000
							6281  FCFPSTRUCTURES_DD           0.000000000000000000000000
							6281 FCFPSTRUCTURES_SYN0          0.000000000000000000000000
							6281         FCIALL_LIT           0.022222222222222223070309
							6281          FCIALL_DD           0.000000000000000000000000
							6281         FCIALL_SYN0          0.008888888888888890615902
							6281         FCIBIG_LIT           0.022222222222222223070309
							6281          FCIBIG_DD           0.000000000000000000000000
							6281         FCIBIG_SYN0          0.008888888888888890615902
							6281     FCINATURAL_LIT           0.022222222222222223070309
							6281      FCINATURAL_DD           0.000000000000000000000000
							6281     FCINATURAL_SYN0          0.008888888888888890615902
							6281      FCIRIPVEG_LIT           0.000000000000000000000000
							6281       FCIRIPVEG_DD           0.000000000000000000000000
							6281      FCIRIPVEG_SYN0          0.000000000000000000000000
							6281         FCNALL_LIT           9.000000000000000000000000
							6281          FCNALL_DD           9.000000000000000000000000
							6281         FCNALL_SYN0          9.000000000000000000000000
							6281     FCNAQUATIC_LIT           9.000000000000000000000000
							6281      FCNAQUATIC_DD           9.000000000000000000000000
							6281     FCNAQUATIC_SYN0          9.000000000000000000000000
							6281    FCNBOULDERS_LIT           9.000000000000000000000000
							6281     FCNBOULDERS_DD           9.000000000000000000000000
							6281    FCNBOULDERS_SYN0          9.000000000000000000000000
							6281       FCNBRUSH_LIT           9.000000000000000000000000
							6281        FCNBRUSH_DD           9.000000000000000000000000
							6281       FCNBRUSH_SYN0          9.000000000000000000000000
							6281      FCNLEDGES_LIT           9.000000000000000000000000
							6281       FCNLEDGES_DD           9.000000000000000000000000
							6281      FCNLEDGES_SYN0          9.000000000000000000000000
							6281   FCNLIVETREES_LIT           9.000000000000000000000000
							6281    FCNLIVETREES_DD           9.000000000000000000000000
							6281   FCNLIVETREES_SYN0          9.000000000000000000000000
							6281    FCNOVERHANG_LIT           9.000000000000000000000000
							6281     FCNOVERHANG_DD           9.000000000000000000000000
							6281    FCNOVERHANG_SYN0          9.000000000000000000000000
							6281       FCNSNAGS_LIT           9.000000000000000000000000
							6281        FCNSNAGS_DD           9.000000000000000000000000
							6281       FCNSNAGS_SYN0          9.000000000000000000000000
							6281  FCNSTRUCTURES_LIT           9.000000000000000000000000
							6281   FCNSTRUCTURES_DD           9.000000000000000000000000
							6281  FCNSTRUCTURES_SYN0          9.000000000000000000000000
							6281     FCVAQUATIC_LIT           0.000000000000000000000000
							6281      FCVAQUATIC_DD           0.000000000000000000000000
							6281     FCVAQUATIC_SYN0          0.000000000000000000000000
							6281    FCVBOULDERS_LIT           0.026352313834736493941557
							6281     FCVBOULDERS_DD           0.000000000000000000000000
							6281    FCVBOULDERS_SYN0          0.011303883305208780804541
							6281       FCVBRUSH_LIT           0.000000000000000000000000
							6281        FCVBRUSH_DD           0.000000000000000000000000
							6281       FCVBRUSH_SYN0          0.000000000000000000000000
							6281      FCVLEDGES_LIT           0.000000000000000000000000
							6281       FCVLEDGES_DD           0.000000000000000000000000
							6281      FCVLEDGES_SYN0          0.000000000000000000000000
							6281   FCVLIVETREES_LIT           0.000000000000000000000000
							6281    FCVLIVETREES_DD           0.000000000000000000000000
							6281   FCVLIVETREES_SYN0          0.000000000000000000000000
							6281    FCVOVERHANG_LIT           0.000000000000000000000000
							6281     FCVOVERHANG_DD           0.000000000000000000000000
							6281    FCVOVERHANG_SYN0          0.000000000000000000000000
							6281       FCVSNAGS_LIT           0.000000000000000000000000
							6281        FCVSNAGS_DD           0.000000000000000000000000
							6281       FCVSNAGS_SYN0          0.000000000000000000000000
							6281  FCVSTRUCTURES_LIT           0.000000000000000000000000
							6281   FCVSTRUCTURES_DD           0.000000000000000000000000
							6281  FCVSTRUCTURES_SYN0          0.000000000000000000000000
							6449    FCFCAQUATIC_LIT           0.050000000000000002775558
							6449     FCFCAQUATIC_DD           0.045833333333333337034077
							6449    FCFCAQUATIC_SYN0          0.049833333333333333647897
							6449   FCFCBOULDERS_LIT           0.008333333333333333217685
							6449    FCFCBOULDERS_DD           0.089583333333333334258519
							6449   FCFCBOULDERS_SYN0          0.010125000000000000319189
							6449      FCFCBRUSH_LIT           0.029166666666666667129260
							6449       FCFCBRUSH_DD           0.070833333333333331482962
							6449      FCFCBRUSH_SYN0          0.030277777777777778456247
							6449     FCFCLEDGES_LIT           0.004166666666666666608843
							6449      FCFCLEDGES_DD           0.008333333333333333217685
							6449     FCFCLEDGES_SYN0          0.004305555555555555524716
							6449  FCFCLIVETREES_LIT           0.004166666666666666608843
							6449   FCFCLIVETREES_DD           0.020833333333333332176851
							6449  FCFCLIVETREES_SYN0          0.004388888888888889221185
							6449   FCFCOVERHANG_LIT           0.033333333333333332870740
							6449    FCFCOVERHANG_DD           0.102272727272727265157570
							6449   FCFCOVERHANG_SYN0          0.038575757575757575745801
							6449      FCFCSNAGS_LIT           0.000000000000000000000000
							6449       FCFCSNAGS_DD           0.000000000000000000000000
							6449      FCFCSNAGS_SYN0          0.000000000000000000000000
							6449 FCFCSTRUCTURES_LIT           0.029166666666666667129260
							6449  FCFCSTRUCTURES_DD           0.033333333333333332870740
							6449 FCFCSTRUCTURES_SYN0          0.029305555555555556912495
							6449        FCFPALL_LIT           1.000000000000000000000000
							6449         FCFPALL_DD           1.000000000000000000000000
							6449        FCFPALL_SYN0          1.000000000000000000000000
							6449    FCFPAQUATIC_LIT           1.000000000000000000000000
							6449     FCFPAQUATIC_DD           0.583333333333333370340767
							6449    FCFPAQUATIC_SYN0          1.000000000000000000000000
							6449   FCFPBOULDERS_LIT           0.166666666666666657414808
							6449    FCFPBOULDERS_DD           0.583333333333333370340767
							6449   FCFPBOULDERS_SYN0          0.583333333333333370340767
							6449      FCFPBRUSH_LIT           0.583333333333333370340767
							6449       FCFPBRUSH_DD           0.750000000000000000000000
							6449      FCFPBRUSH_SYN0          0.750000000000000000000000
							6449     FCFPLEDGES_LIT           0.083333333333333328707404
							6449      FCFPLEDGES_DD           0.166666666666666657414808
							6449     FCFPLEDGES_SYN0          0.166666666666666657414808
							6449  FCFPLIVETREES_LIT           0.083333333333333328707404
							6449   FCFPLIVETREES_DD           0.083333333333333328707404
							6449  FCFPLIVETREES_SYN0          0.083333333333333328707404
							6449   FCFPOVERHANG_LIT           0.333333333333333314829616
							6449    FCFPOVERHANG_DD           0.363636363636363646456573
							6449   FCFPOVERHANG_SYN0          0.454545454545454530315141
							6449      FCFPSNAGS_LIT           0.000000000000000000000000
							6449       FCFPSNAGS_DD           0.000000000000000000000000
							6449      FCFPSNAGS_SYN0          0.000000000000000000000000
							6449 FCFPSTRUCTURES_LIT           0.583333333333333370340767
							6449  FCFPSTRUCTURES_DD           0.666666666666666629659233
							6449 FCFPSTRUCTURES_SYN0          0.666666666666666629659233
							6449         FCIALL_LIT           0.158333333333333325931847
							6449          FCIALL_DD           0.371022727272727281810916
							6449         FCIALL_SYN0          0.166811868686868702837955
							6449         FCIBIG_LIT           0.074999999999999997224442
							6449          FCIBIG_DD           0.233522727272727270708685
							6449         FCIBIG_SYN0          0.082311868686868683298030
							6449     FCINATURAL_LIT           0.129166666666666679619269
							6449      FCINATURAL_DD           0.337689393939393955879069
							6449     FCINATURAL_SYN0          0.137506313131313145925461
							6449      FCIRIPVEG_LIT           0.033333333333333332870740
							6449       FCIRIPVEG_DD           0.091666666666666660190366
							6449      FCIRIPVEG_SYN0          0.034666666666666665075347
							6449         FCNALL_LIT          12.000000000000000000000000
							6449          FCNALL_DD          12.000000000000000000000000
							6449         FCNALL_SYN0         11.000000000000000000000000
							6449     FCNAQUATIC_LIT          12.000000000000000000000000
							6449      FCNAQUATIC_DD          12.000000000000000000000000
							6449     FCNAQUATIC_SYN0         12.000000000000000000000000
							6449    FCNBOULDERS_LIT          12.000000000000000000000000
							6449     FCNBOULDERS_DD          12.000000000000000000000000
							6449    FCNBOULDERS_SYN0         12.000000000000000000000000
							6449       FCNBRUSH_LIT          12.000000000000000000000000
							6449        FCNBRUSH_DD          12.000000000000000000000000
							6449       FCNBRUSH_SYN0         12.000000000000000000000000
							6449      FCNLEDGES_LIT          12.000000000000000000000000
							6449       FCNLEDGES_DD          12.000000000000000000000000
							6449      FCNLEDGES_SYN0         12.000000000000000000000000
							6449   FCNLIVETREES_LIT          12.000000000000000000000000
							6449    FCNLIVETREES_DD          12.000000000000000000000000
							6449   FCNLIVETREES_SYN0         12.000000000000000000000000
							6449    FCNOVERHANG_LIT          12.000000000000000000000000
							6449     FCNOVERHANG_DD          11.000000000000000000000000
							6449    FCNOVERHANG_SYN0         11.000000000000000000000000
							6449       FCNSNAGS_LIT          12.000000000000000000000000
							6449        FCNSNAGS_DD          12.000000000000000000000000
							6449       FCNSNAGS_SYN0         12.000000000000000000000000
							6449  FCNSTRUCTURES_LIT          12.000000000000000000000000
							6449   FCNSTRUCTURES_DD          12.000000000000000000000000
							6449  FCNSTRUCTURES_SYN0         12.000000000000000000000000
							6449     FCVAQUATIC_LIT           0.000000000000000000000000
							6449      FCVAQUATIC_DD           0.068947718445122457842089
							6449     FCVAQUATIC_SYN0          0.001087114613009219693182
							6449    FCVBOULDERS_LIT           0.019462473604038073998757
							6449     FCVBOULDERS_DD           0.167690981127534555206182
							6449    FCVBOULDERS_SYN0          0.021323757912599539815490
							6449       FCVBRUSH_LIT           0.025746432527221863040268
							6449        FCVBRUSH_DD           0.086493124617281605392982
							6449       FCVBRUSH_SYN0          0.026355316230367898527875
							6449      FCVLEDGES_LIT           0.014433756729740645358140
							6449       FCVLEDGES_DD           0.019462473604038073998757
							6449      FCVLEDGES_SYN0          0.014397992518132120623453
							6449   FCVLIVETREES_LIT           0.014433756729740645358140
							6449    FCVLIVETREES_DD           0.072168783648703216382359
							6449   FCVLIVETREES_SYN0          0.015203557088660146420778
							6449    FCVOVERHANG_LIT           0.071774056256527343777840
							6449     FCVOVERHANG_DD           0.185220998220553212387784
							6449    FCVOVERHANG_SYN0          0.076204562550646226526396
							6449       FCVSNAGS_LIT           0.000000000000000000000000
							6449        FCVSNAGS_DD           0.000000000000000000000000
							6449       FCVSNAGS_SYN0          0.000000000000000000000000
							6449  FCVSTRUCTURES_LIT           0.025746432527221863040268
							6449   FCVSTRUCTURES_DD           0.024618298195866548938593
							6449  FCVSTRUCTURES_SYN0          0.025578738935332318710225
							6683    FCFCAQUATIC_LIT           0.875000000000000000000000
							6683     FCFCAQUATIC_DD           0.000000000000000000000000
							6683    FCFCAQUATIC_SYN0          0.000000000000000000000000
							6683   FCFCBOULDERS_LIT           0.000000000000000000000000
							6683    FCFCBOULDERS_DD           0.000000000000000000000000
							6683   FCFCBOULDERS_SYN0          0.000000000000000000000000
							6683      FCFCBRUSH_LIT           0.000000000000000000000000
							6683       FCFCBRUSH_DD           0.000000000000000000000000
							6683      FCFCBRUSH_SYN0          0.000000000000000000000000
							6683     FCFCLEDGES_LIT           0.000000000000000000000000
							6683      FCFCLEDGES_DD           0.000000000000000000000000
							6683     FCFCLEDGES_SYN0          0.000000000000000000000000
							6683  FCFCLIVETREES_LIT           0.000000000000000000000000
							6683   FCFCLIVETREES_DD           0.000000000000000000000000
							6683  FCFCLIVETREES_SYN0          0.000000000000000000000000
							6683   FCFCOVERHANG_LIT           0.000000000000000000000000
							6683    FCFCOVERHANG_DD           0.000000000000000000000000
							6683   FCFCOVERHANG_SYN0          0.000000000000000000000000
							6683      FCFCSNAGS_LIT           0.000000000000000000000000
							6683       FCFCSNAGS_DD           0.000000000000000000000000
							6683      FCFCSNAGS_SYN0          0.000000000000000000000000
							6683 FCFCSTRUCTURES_LIT           0.000000000000000000000000
							6683  FCFCSTRUCTURES_DD           0.000000000000000000000000
							6683 FCFCSTRUCTURES_SYN0          0.000000000000000000000000
							6683        FCFPALL_LIT           1.000000000000000000000000
							6683         FCFPALL_DD           0.000000000000000000000000
							6683        FCFPALL_SYN0          0.000000000000000000000000
							6683    FCFPAQUATIC_LIT           1.000000000000000000000000
							6683     FCFPAQUATIC_DD           0.000000000000000000000000
							6683    FCFPAQUATIC_SYN0          0.000000000000000000000000
							6683   FCFPBOULDERS_LIT           0.000000000000000000000000
							6683    FCFPBOULDERS_DD           0.000000000000000000000000
							6683   FCFPBOULDERS_SYN0          0.000000000000000000000000
							6683      FCFPBRUSH_LIT           0.000000000000000000000000
							6683       FCFPBRUSH_DD           0.000000000000000000000000
							6683      FCFPBRUSH_SYN0          0.000000000000000000000000
							6683     FCFPLEDGES_LIT           0.000000000000000000000000
							6683      FCFPLEDGES_DD           0.000000000000000000000000
							6683     FCFPLEDGES_SYN0          0.000000000000000000000000
							6683  FCFPLIVETREES_LIT           0.000000000000000000000000
							6683   FCFPLIVETREES_DD           0.000000000000000000000000
							6683  FCFPLIVETREES_SYN0          0.000000000000000000000000
							6683   FCFPOVERHANG_LIT           0.000000000000000000000000
							6683    FCFPOVERHANG_DD           0.000000000000000000000000
							6683   FCFPOVERHANG_SYN0          0.000000000000000000000000
							6683      FCFPSNAGS_LIT           0.000000000000000000000000
							6683       FCFPSNAGS_DD           0.000000000000000000000000
							6683      FCFPSNAGS_SYN0          0.000000000000000000000000
							6683 FCFPSTRUCTURES_LIT           0.000000000000000000000000
							6683  FCFPSTRUCTURES_DD           0.000000000000000000000000
							6683 FCFPSTRUCTURES_SYN0          0.000000000000000000000000
							6683         FCIALL_LIT           0.875000000000000000000000
							6683          FCIALL_DD           0.000000000000000000000000
							6683         FCIALL_SYN0          0.000000000000000000000000
							6683         FCIBIG_LIT           0.000000000000000000000000
							6683          FCIBIG_DD           0.000000000000000000000000
							6683         FCIBIG_SYN0          0.000000000000000000000000
							6683     FCINATURAL_LIT           0.875000000000000000000000
							6683      FCINATURAL_DD           0.000000000000000000000000
							6683     FCINATURAL_SYN0          0.000000000000000000000000
							6683      FCIRIPVEG_LIT           0.000000000000000000000000
							6683       FCIRIPVEG_DD           0.000000000000000000000000
							6683      FCIRIPVEG_SYN0          0.000000000000000000000000
							6683         FCNALL_LIT           1.000000000000000000000000
							6683          FCNALL_DD           1.000000000000000000000000
							6683         FCNALL_SYN0          1.000000000000000000000000
							6683     FCNAQUATIC_LIT           1.000000000000000000000000
							6683      FCNAQUATIC_DD           1.000000000000000000000000
							6683     FCNAQUATIC_SYN0          1.000000000000000000000000
							6683    FCNBOULDERS_LIT           1.000000000000000000000000
							6683     FCNBOULDERS_DD           1.000000000000000000000000
							6683    FCNBOULDERS_SYN0          1.000000000000000000000000
							6683       FCNBRUSH_LIT           1.000000000000000000000000
							6683        FCNBRUSH_DD           1.000000000000000000000000
							6683       FCNBRUSH_SYN0          1.000000000000000000000000
							6683      FCNLEDGES_LIT           1.000000000000000000000000
							6683       FCNLEDGES_DD           1.000000000000000000000000
							6683      FCNLEDGES_SYN0          1.000000000000000000000000
							6683   FCNLIVETREES_LIT           1.000000000000000000000000
							6683    FCNLIVETREES_DD           1.000000000000000000000000
							6683   FCNLIVETREES_SYN0          1.000000000000000000000000
							6683    FCNOVERHANG_LIT           1.000000000000000000000000
							6683     FCNOVERHANG_DD           1.000000000000000000000000
							6683    FCNOVERHANG_SYN0          1.000000000000000000000000
							6683       FCNSNAGS_LIT           1.000000000000000000000000
							6683        FCNSNAGS_DD           1.000000000000000000000000
							6683       FCNSNAGS_SYN0          1.000000000000000000000000
							6683  FCNSTRUCTURES_LIT           1.000000000000000000000000
							6683   FCNSTRUCTURES_DD           1.000000000000000000000000
							6683  FCNSTRUCTURES_SYN0          1.000000000000000000000000
							6683     FCVAQUATIC_LIT                          NA
							6683      FCVAQUATIC_DD                          NA
							6683     FCVAQUATIC_SYN0                         NA
							6683    FCVBOULDERS_LIT                          NA
							6683     FCVBOULDERS_DD                          NA
							6683    FCVBOULDERS_SYN0                         NA
							6683       FCVBRUSH_LIT                          NA
							6683        FCVBRUSH_DD                          NA
							6683       FCVBRUSH_SYN0                         NA
							6683      FCVLEDGES_LIT                          NA
							6683       FCVLEDGES_DD                          NA
							6683      FCVLEDGES_SYN0                         NA
							6683   FCVLIVETREES_LIT                          NA
							6683    FCVLIVETREES_DD                          NA
							6683   FCVLIVETREES_SYN0                         NA
							6683    FCVOVERHANG_LIT                          NA
							6683     FCVOVERHANG_DD                          NA
							6683    FCVOVERHANG_SYN0                         NA
							6683       FCVSNAGS_LIT                          NA
							6683        FCVSNAGS_DD                          NA
							6683       FCVSNAGS_SYN0                         NA
							6683  FCVSTRUCTURES_LIT                          NA
							6683   FCVSTRUCTURES_DD                          NA
							6683  FCVSTRUCTURES_SYN0                         NA
							7263    FCFCAQUATIC_LIT           0.050000000000000002775558
							7263     FCFCAQUATIC_DD           0.050000000000000002775558
							7263    FCFCAQUATIC_SYN0          0.05
							7263   FCFCBOULDERS_LIT           0.070000000000000006661338
							7263   FCFCBOULDERS_SYN0                        NaN
							7263      FCFCBRUSH_LIT           0.030000000000000002359224
							7263      FCFCBRUSH_SYN0                        NaN
							7263     FCFCLEDGES_LIT           0.005000000000000000104083
							7263     FCFCLEDGES_SYN0                        NaN
							7263  FCFCLIVETREES_LIT           0.000000000000000000000000
							7263  FCFCLIVETREES_SYN0                        NaN
							7263   FCFCOVERHANG_LIT           0.035000000000000003330669
							7263   FCFCOVERHANG_SYN0                        NaN
							7263      FCFCSNAGS_LIT           0.010000000000000000208167
							7263       FCFCSNAGS_DD           0.000000000000000000000000
							7263      FCFCSNAGS_SYN0          0
							7263 FCFCSTRUCTURES_LIT           0.005000000000000000104083
							7263 FCFCSTRUCTURES_SYN0                        NaN
							7263        FCFPALL_LIT           1.000000000000000000000000
							7263         FCFPALL_DD           1.000000000000000000000000
							7263        FCFPALL_SYN0                        NaN
							7263    FCFPAQUATIC_LIT           1.000000000000000000000000
							7263     FCFPAQUATIC_DD           1.000000000000000000000000
							7263    FCFPAQUATIC_SYN0          1
							7263   FCFPBOULDERS_LIT           0.599999999999999977795540
							7263   FCFPBOULDERS_SYN0                        NaN
							7263      FCFPBRUSH_LIT           0.599999999999999977795540
							7263      FCFPBRUSH_SYN0                        NaN
							7263     FCFPLEDGES_LIT           0.100000000000000005551115
							7263     FCFPLEDGES_SYN0                        NaN
							7263  FCFPLIVETREES_LIT           0.000000000000000000000000
							7263  FCFPLIVETREES_SYN0                        NaN
							7263   FCFPOVERHANG_LIT           0.699999999999999955591079
							7263   FCFPOVERHANG_SYN0                        NaN
							7263      FCFPSNAGS_LIT           0.200000000000000011102230
							7263       FCFPSNAGS_DD           0.000000000000000000000000
							7263      FCFPSNAGS_SYN0          0
							7263 FCFPSTRUCTURES_LIT           0.100000000000000005551115
							7263 FCFPSTRUCTURES_SYN0                        NaN
							7263         FCIALL_LIT           0.205000000000000015543122
							7263          FCIALL_DD           0.050000000000000002775558
							7263         FCIALL_SYN0          0.05
							7263         FCIBIG_LIT           0.115000000000000004996004
							7263         FCIBIG_SYN0          0.000000000000000000000000
							7263     FCINATURAL_LIT           0.200000000000000011102230
							7263      FCINATURAL_DD           0.050000000000000002775558
							7263     FCINATURAL_SYN0          0.05
							7263      FCIRIPVEG_LIT           0.040000000000000000832667
							7263       FCIRIPVEG_DD           0.000000000000000000000000
							7263      FCIRIPVEG_SYN0          0.000000000000000000000000
							7263         FCNALL_LIT          10.000000000000000000000000
							7263          FCNALL_DD           1.000000000000000000000000
							7263         FCNALL_SYN0          0.000000000000000000000000
							7263     FCNAQUATIC_LIT          10.000000000000000000000000
							7263      FCNAQUATIC_DD           1.000000000000000000000000
							7263     FCNAQUATIC_SYN0          1
							7263    FCNBOULDERS_LIT          10.000000000000000000000000
							7263     FCNBOULDERS_DD           0.000000000000000000000000
							7263    FCNBOULDERS_SYN0          0.000000000000000000000000
							7263       FCNBRUSH_LIT          10.000000000000000000000000
							7263        FCNBRUSH_DD           0.000000000000000000000000
							7263       FCNBRUSH_SYN0          0.000000000000000000000000
							7263      FCNLEDGES_LIT          10.000000000000000000000000
							7263       FCNLEDGES_DD           0.000000000000000000000000
							7263      FCNLEDGES_SYN0          0.000000000000000000000000
							7263   FCNLIVETREES_LIT          10.000000000000000000000000
							7263    FCNLIVETREES_DD           0.000000000000000000000000
							7263   FCNLIVETREES_SYN0          0.000000000000000000000000
							7263    FCNOVERHANG_LIT          10.000000000000000000000000
							7263     FCNOVERHANG_DD           0.000000000000000000000000
							7263    FCNOVERHANG_SYN0          0.000000000000000000000000
							7263       FCNSNAGS_LIT          10.000000000000000000000000
							7263        FCNSNAGS_DD           1.000000000000000000000000
							7263       FCNSNAGS_SYN0          1
							7263  FCNSTRUCTURES_LIT          10.000000000000000000000000
							7263   FCNSTRUCTURES_DD           0.000000000000000000000000
							7263  FCNSTRUCTURES_SYN0          0.000000000000000000000000
							7263     FCVAQUATIC_LIT           0.000000000000000000000000
							7263      FCVAQUATIC_DD                          NA
							7263     FCVAQUATIC_SYN0                         NA
							7263    FCVBOULDERS_LIT           0.097752521990767865522898
							7263    FCVBOULDERS_SYN0                         NA
							7263       FCVBRUSH_LIT           0.025819888974716112550745
							7263       FCVBRUSH_SYN0                         NA
							7263      FCVLEDGES_LIT           0.015811388300841895671045
							7263      FCVLEDGES_SYN0                         NA
							7263   FCVLIVETREES_LIT           0.000000000000000000000000
							7263   FCVLIVETREES_SYN0                         NA
							7263    FCVOVERHANG_LIT           0.024152294576982397122933
							7263    FCVOVERHANG_SYN0                         NA
							7263       FCVSNAGS_LIT           0.021081851067789196541025
							7263        FCVSNAGS_DD                          NA
							7263       FCVSNAGS_SYN0                         NA
							7263  FCVSTRUCTURES_LIT           0.015811388300841895671045
							7263  FCVSTRUCTURES_SYN0                         NA
							7913    FCFCAQUATIC_LIT           0.647499999999999964472863
							7913    FCFCAQUATIC_SYN0                        NaN
							7913   FCFCBOULDERS_LIT           0.050000000000000002775558
							7913   FCFCBOULDERS_SYN0                        NaN
							7913      FCFCBRUSH_LIT           0.050000000000000002775558
							7913      FCFCBRUSH_SYN0                        NaN
							7913   FCFCOVERHANG_LIT           0.050000000000000002775558
							7913   FCFCOVERHANG_SYN0                        NaN
							7913      FCFCSNAGS_LIT           0.000000000000000000000000
							7913      FCFCSNAGS_SYN0                        NaN
							7913        FCFPALL_LIT           1.000000000000000000000000
							7913        FCFPALL_SYN0                        NaN
							7913    FCFPAQUATIC_LIT           1.000000000000000000000000
							7913    FCFPAQUATIC_SYN0                        NaN
							7913   FCFPBOULDERS_LIT           1.000000000000000000000000
							7913   FCFPBOULDERS_SYN0                        NaN
							7913      FCFPBRUSH_LIT           1.000000000000000000000000
							7913      FCFPBRUSH_SYN0                        NaN
							7913   FCFPOVERHANG_LIT           1.000000000000000000000000
							7913   FCFPOVERHANG_SYN0                        NaN
							7913      FCFPSNAGS_LIT           0.000000000000000000000000
							7913      FCFPSNAGS_SYN0                        NaN
							7913         FCIALL_LIT           0.797499999999999986677324
							7913         FCIALL_SYN0          0.000000000000000000000000
							7913         FCIBIG_LIT           0.100000000000000005551115
							7913         FCIBIG_SYN0          0.000000000000000000000000
							7913     FCINATURAL_LIT           0.797499999999999986677324
							7913     FCINATURAL_SYN0          0.000000000000000000000000
							7913      FCIRIPVEG_LIT           0.050000000000000002775558
							7913      FCIRIPVEG_SYN0          0.000000000000000000000000
							7913         FCNALL_LIT          10.000000000000000000000000
							7913         FCNALL_SYN0          0.000000000000000000000000
							7913     FCNAQUATIC_LIT          10.000000000000000000000000
							7913      FCNAQUATIC_DD           0.000000000000000000000000
							7913     FCNAQUATIC_SYN0          0.000000000000000000000000
							7913    FCNBOULDERS_LIT           2.000000000000000000000000
							7913     FCNBOULDERS_DD           0.000000000000000000000000
							7913    FCNBOULDERS_SYN0          0.000000000000000000000000
							7913       FCNBRUSH_LIT           2.000000000000000000000000
							7913        FCNBRUSH_DD           0.000000000000000000000000
							7913       FCNBRUSH_SYN0          0.000000000000000000000000
							7913      FCNLEDGES_LIT           0.000000000000000000000000
							7913       FCNLEDGES_DD           0.000000000000000000000000
							7913      FCNLEDGES_SYN0          0.000000000000000000000000
							7913   FCNLIVETREES_LIT           0.000000000000000000000000
							7913    FCNLIVETREES_DD           0.000000000000000000000000
							7913   FCNLIVETREES_SYN0          0.000000000000000000000000
							7913    FCNOVERHANG_LIT           7.000000000000000000000000
							7913     FCNOVERHANG_DD           0.000000000000000000000000
							7913    FCNOVERHANG_SYN0          0.000000000000000000000000
							7913       FCNSNAGS_LIT           2.000000000000000000000000
							7913        FCNSNAGS_DD           0.000000000000000000000000
							7913       FCNSNAGS_SYN0          0.000000000000000000000000
							7913  FCNSTRUCTURES_LIT           0.000000000000000000000000
							7913   FCNSTRUCTURES_DD           0.000000000000000000000000
							7913  FCNSTRUCTURES_SYN0          0.000000000000000000000000
							7913     FCVAQUATIC_LIT           0.370332058203625302805762
							7913     FCVAQUATIC_SYN0                         NA
							7913    FCVBOULDERS_LIT           0.000000000000000000000000
							7913    FCVBOULDERS_SYN0                         NA
							7913       FCVBRUSH_LIT           0.000000000000000000000000
							7913       FCVBRUSH_SYN0                         NA
							7913    FCVOVERHANG_LIT           0.000000000000000000000000
							7913    FCVOVERHANG_SYN0                         NA
							7913       FCVSNAGS_LIT           0.000000000000000000000000
							7913       FCVSNAGS_SYN0                         NA
							1000057    FCFCAQUATIC_LIT           0.308333333333333348136307
							1000057    FCFCAQUATIC_SYN0                        NaN
							1000057   FCFCBOULDERS_LIT           0.100000000000000005551115
							1000057   FCFCBOULDERS_SYN0                        NaN
							1000057      FCFCBRUSH_LIT           0.050000000000000002775558
							1000057      FCFCBRUSH_SYN0                        NaN
							1000057     FCFCLEDGES_LIT           0.250000000000000000000000
							1000057     FCFCLEDGES_SYN0                        NaN
							1000057  FCFCLIVETREES_LIT           0.000000000000000000000000
							1000057  FCFCLIVETREES_SYN0                        NaN
							1000057   FCFCOVERHANG_LIT           0.050000000000000002775558
							1000057   FCFCOVERHANG_SYN0                        NaN
							1000057      FCFCSNAGS_LIT           0.033333333333333332870740
							1000057      FCFCSNAGS_SYN0                        NaN
							1000057        FCFPALL_LIT           1.000000000000000000000000
							1000057        FCFPALL_SYN0                        NaN
							1000057    FCFPAQUATIC_LIT           1.000000000000000000000000
							1000057    FCFPAQUATIC_SYN0                        NaN
							1000057   FCFPBOULDERS_LIT           1.000000000000000000000000
							1000057   FCFPBOULDERS_SYN0                        NaN
							1000057      FCFPBRUSH_LIT           1.000000000000000000000000
							1000057      FCFPBRUSH_SYN0                        NaN
							1000057     FCFPLEDGES_LIT           1.000000000000000000000000
							1000057     FCFPLEDGES_SYN0                        NaN
							1000057  FCFPLIVETREES_LIT           0.000000000000000000000000
							1000057  FCFPLIVETREES_SYN0                        NaN
							1000057   FCFPOVERHANG_LIT           1.000000000000000000000000
							1000057   FCFPOVERHANG_SYN0                        NaN
							1000057      FCFPSNAGS_LIT           0.666666666666666629659233
							1000057      FCFPSNAGS_SYN0                        NaN
							1000057         FCIALL_LIT           0.791666666666666740681535
							1000057         FCIALL_SYN0          0.000000000000000000000000
							1000057         FCIBIG_LIT           0.400000000000000022204460
							1000057         FCIBIG_SYN0          0.000000000000000000000000
							1000057     FCINATURAL_LIT           0.791666666666666740681535
							1000057     FCINATURAL_SYN0          0.000000000000000000000000
							1000057      FCIRIPVEG_LIT           0.083333333333333342585192
							1000057      FCIRIPVEG_SYN0          0.000000000000000000000000
							1000057         FCNALL_LIT          10.000000000000000000000000
							1000057         FCNALL_SYN0          0.000000000000000000000000
							1000057     FCNAQUATIC_LIT           6.000000000000000000000000
							1000057      FCNAQUATIC_DD           0.000000000000000000000000
							1000057     FCNAQUATIC_SYN0          0.000000000000000000000000
							1000057    FCNBOULDERS_LIT           8.000000000000000000000000
							1000057     FCNBOULDERS_DD           0.000000000000000000000000
							1000057    FCNBOULDERS_SYN0          0.000000000000000000000000
							1000057       FCNBRUSH_LIT          10.000000000000000000000000
							1000057        FCNBRUSH_DD           0.000000000000000000000000
							1000057       FCNBRUSH_SYN0          0.000000000000000000000000
							1000057      FCNLEDGES_LIT           1.000000000000000000000000
							1000057       FCNLEDGES_DD           0.000000000000000000000000
							1000057      FCNLEDGES_SYN0          0.000000000000000000000000
							1000057   FCNLIVETREES_LIT           1.000000000000000000000000
							1000057    FCNLIVETREES_DD           0.000000000000000000000000
							1000057   FCNLIVETREES_SYN0          0.000000000000000000000000
							1000057    FCNOVERHANG_LIT          10.000000000000000000000000
							1000057     FCNOVERHANG_DD           0.000000000000000000000000
							1000057    FCNOVERHANG_SYN0          0.000000000000000000000000
							1000057       FCNSNAGS_LIT           3.000000000000000000000000
							1000057        FCNSNAGS_DD           0.000000000000000000000000
							1000057       FCNSNAGS_SYN0          0.000000000000000000000000
							1000057  FCNSTRUCTURES_LIT           0.000000000000000000000000
							1000057   FCNSTRUCTURES_DD           0.000000000000000000000000
							1000057  FCNSTRUCTURES_SYN0          0.000000000000000000000000
							1000057     FCVAQUATIC_LIT           0.345205252953466323884157
							1000057     FCVAQUATIC_SYN0                         NA
							1000057    FCVBOULDERS_LIT           0.092582009977255144694830
							1000057    FCVBOULDERS_SYN0                         NA
							1000057       FCVBRUSH_LIT           0.000000000000000000000000
							1000057       FCVBRUSH_SYN0                         NA
							1000057      FCVLEDGES_LIT                          NA
							1000057      FCVLEDGES_SYN0                         NA
							1000057   FCVLIVETREES_LIT                          NA
							1000057   FCVLIVETREES_SYN0                         NA
							1000057    FCVOVERHANG_LIT           0.000000000000000000000000
							1000057    FCVOVERHANG_SYN0                         NA
							1000057       FCVSNAGS_LIT           0.028867513459481290716280
							1000057       FCVSNAGS_SYN0                         NA
						 ")
		 
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	return(rc)
}



# end of file