# nlaRiparianVegetationTest.r
# RUnit tests
#  6/02/17 cws Renamed to include NLA in all the test function names.
#            Changed UID to SITE, RESULT to VALUE, PARAMETER to CLASS for input
#            and METRIC for output.
#  7/17/17 cws Updated to test with new calling interface.
#

nlaRiparianVegetationTest <- function()
# unit test for nlaRiparianVegetation
#
# The complex testing here is due to the precision differences between
# calculated and retrieved values, requiring separate tests for presence (names)
# type and value of the metrics.
{
	nlaRiparianVegetationTest.2007()
	nlaRiparianVegetationTest.withDrawDown()
	nlaRiparianVegetationTest.withDrawDownAndFillin()
}


nlaRiparianVegetationTest.2007 <- function()
# Unit test with 2007 data
{		
	testData <- nlaRiparianVegetationTest.createTestData2007()
	expected <- nlaRiparianVegetationTest.expectedResults2007()
	actual <- nlaRiparianVegetation(bigTrees = testData %>% subset(CLASS == 'C_BIGTREES') %>% select(SITE, STATION, VALUE)
                                   ,smallTrees = testData %>% subset(CLASS == 'C_SMALLTREES') %>% select(SITE, STATION, VALUE)
                                   ,canopyType = testData %>% subset(CLASS == 'CANOPY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverBare = testData %>% subset(CLASS == 'GC_BARE') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverInundated = testData %>% subset(CLASS == 'GC_INUNDATED') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverNonwoody = testData %>% subset(CLASS == 'GC_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverWoody = testData %>% subset(CLASS == 'GC_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryNonwoody = testData %>% subset(CLASS == 'U_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryWoody = testData %>% subset(CLASS == 'U_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryType = testData %>% subset(CLASS == 'UNDERSTORY') %>% select(SITE, STATION, VALUE)
                                   ,drawdown = testData %>% subset(CLASS == 'DRAWDOWN') %>% select(SITE, STATION, VALUE)
	                               ,createSyntheticCovers=FALSE
	                               )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)

	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}


nlaRiparianVegetationTest.withDrawDown <- function()
# Unit test with 2012 data, but do NOT fill in unrecorded drawdown values
{
	testData <- nlaRiparianVegetationTest.createTestDataWithDrawDown()
	expected <- nlaRiparianVegetationTest.expectedResultsWithDrawDownAndNoFillin()
	expected$VALUE <- as.numeric(expected$VALUE)
	actual <- nlaRiparianVegetation(bigTrees = testData %>% subset(CLASS == 'C_BIGTREES') %>% select(SITE, STATION, VALUE)
                                   ,bigTrees_dd = testData %>% subset(CLASS == 'C_BIGTREES_DD') %>% select(SITE, STATION, VALUE)
                                   ,smallTrees = testData %>% subset(CLASS == 'C_SMALLTREES') %>% select(SITE, STATION, VALUE)
                                   ,smallTrees_dd= testData %>% subset(CLASS == 'C_SMALLTREES_DD') %>% select(SITE, STATION, VALUE)
                                   ,canopyType = testData %>% subset(CLASS == 'CANOPY') %>% select(SITE, STATION, VALUE)
                                   ,canopyType_dd= testData %>% subset(CLASS == 'CANOPY_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverBare = testData %>% subset(CLASS == 'GC_BARE') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverBare_dd= testData %>% subset(CLASS == 'GC_BARE_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverInundated = testData %>% subset(CLASS == 'GC_INUNDATED') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverInundated_dd= testData %>% subset(CLASS == 'GC_INUNDATED_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverNonwoody = testData %>% subset(CLASS == 'GC_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverNonwoody_dd= testData %>% subset(CLASS == 'GC_NONWOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverWoody = testData %>% subset(CLASS == 'GC_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverWoody_dd= testData %>% subset(CLASS == 'GC_WOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryNonwoody = testData %>% subset(CLASS == 'U_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryNonwoody_dd= testData %>% subset(CLASS == 'U_NONWOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryWoody = testData %>% subset(CLASS == 'U_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryWoody_dd= testData %>% subset(CLASS == 'U_WOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryType = testData %>% subset(CLASS == 'UNDERSTORY') %>% select(SITE, STATION, VALUE)
                                   ,understoryType_dd= testData %>% subset(CLASS == 'UNDERSTORY_DD') %>% select(SITE, STATION, VALUE)
                                   ,drawdown = testData %>% subset(CLASS == 'DRAWDOWN') %>% select(SITE, STATION, VALUE)
                                   ,horizontalDistance_dd = testData %>% subset(CLASS == 'HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
	                               ,fillinDrawdown=FALSE
	                               )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawdown")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawDown")
}


nlaRiparianVegetationTest.withDrawDownAndFillin <- function()
# Unit test with 2012 data using the option to fill in unrecorded drawdown values.
{
	testData <- nlaRiparianVegetationTest.createTestDataWithDrawDown()
	expected <- nlaRiparianVegetationTest.expectedResultsWithDrawDown()
	expected$VALUE <- as.numeric(expected$VALUE)
	actual <- nlaRiparianVegetation(bigTrees = testData %>% subset(CLASS == 'C_BIGTREES') %>% select(SITE, STATION, VALUE)
                                   ,bigTrees_dd = testData %>% subset(CLASS == 'C_BIGTREES_DD') %>% select(SITE, STATION, VALUE)
                                   ,smallTrees = testData %>% subset(CLASS == 'C_SMALLTREES') %>% select(SITE, STATION, VALUE)
                                   ,smallTrees_dd= testData %>% subset(CLASS == 'C_SMALLTREES_DD') %>% select(SITE, STATION, VALUE)
                                   ,canopyType = testData %>% subset(CLASS == 'CANOPY') %>% select(SITE, STATION, VALUE)
                                   ,canopyType_dd= testData %>% subset(CLASS == 'CANOPY_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverBare = testData %>% subset(CLASS == 'GC_BARE') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverBare_dd= testData %>% subset(CLASS == 'GC_BARE_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverInundated = testData %>% subset(CLASS == 'GC_INUNDATED') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverInundated_dd= testData %>% subset(CLASS == 'GC_INUNDATED_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverNonwoody = testData %>% subset(CLASS == 'GC_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverNonwoody_dd= testData %>% subset(CLASS == 'GC_NONWOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverWoody = testData %>% subset(CLASS == 'GC_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,groundcoverWoody_dd= testData %>% subset(CLASS == 'GC_WOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryNonwoody = testData %>% subset(CLASS == 'U_NONWOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryNonwoody_dd= testData %>% subset(CLASS == 'U_NONWOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryWoody = testData %>% subset(CLASS == 'U_WOODY') %>% select(SITE, STATION, VALUE)
                                   ,understoryWoody_dd= testData %>% subset(CLASS == 'U_WOODY_DD') %>% select(SITE, STATION, VALUE)
                                   ,understoryType = testData %>% subset(CLASS == 'UNDERSTORY') %>% select(SITE, STATION, VALUE)
                                   ,understoryType_dd= testData %>% subset(CLASS == 'UNDERSTORY_DD') %>% select(SITE, STATION, VALUE)
                                   ,drawdown = testData %>% subset(CLASS == 'DRAWDOWN') %>% select(SITE, STATION, VALUE)
                                   ,horizontalDistance_dd = testData %>% subset(CLASS == 'HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
                                   )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown and DD fill-in")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawdown and DD fill-in")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown and DD fill-in")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawDown and DD fill-in")
}


nlaRiparianVegetationTest.createTestData2007 <- function()
# This data is based on NLA2007 data for the following VISIT_ID:
#	7469	full complement of data, stations A-J (normal)
#	7472	partial data, stations A-J
#	7492	partial data, some stations missing
#	7518	full complement of data, stations A-J plus K
#	7611	full complement of data, stations A,B,C,D,J only
#	7723	site with two relocated stations, station J not quite complete
#	7784	site consists only of transect J, which has all parameters
#	7797	full complement of data, stations A-J plus K,L,M
#rv07 <- dbFetch(nla07, 'tblRESULTS', where="PARAMETER in ('CANOPY', 'C_BIGTREES', 'C_SMALLTREES','UNDERSTORY', 'U_NONWOODY', 'U_WOODY','GC_BARE', 'GC_INUNDATED', 'GC_NONWOODY','GC_WOODY')")
#tt<-subset(rv07, VISIT_ID %in% c(7469,7472,7492,7518,7611,7723,7784,7797), select=c(VISIT_ID,STATION_NO,PARAMETER,RESULT,UNITS))
#tt <- tt[order(tt$VISIT_ID,tt$STATION_NO,tt$PARAMETER),]	
{
	tc <- textConnection("   SITE    STATION    CLASS VALUE UNITS
							7469          A   C_BIGTREES      1 X         
							7469          A C_SMALLTREES      3 X         
							7469          A       CANOPY      C X         
							7469          A      GC_BARE      0 X         
							7469          A GC_INUNDATED      1 X         
							7469          A  GC_NONWOODY      3 X         
							7469          A     GC_WOODY      3 X         
							7469          A   U_NONWOODY      1 X         
							7469          A      U_WOODY      3 X         
							7469          A   UNDERSTORY      D X         
							7469          B   C_BIGTREES      0 X         
							7469          B C_SMALLTREES      0 X         
							7469          B       CANOPY      N X         
							7469          B      GC_BARE      0 X         
							7469          B GC_INUNDATED      0 X         
							7469          B  GC_NONWOODY      1 X         
							7469          B     GC_WOODY      4 X         
							7469          B   U_NONWOODY      1 X         
							7469          B      U_WOODY      4 X         
							7469          B   UNDERSTORY      D X         
							7469          C   C_BIGTREES      0 X         
							7469          C C_SMALLTREES      0 X         
							7469          C       CANOPY      N X         
							7469          C      GC_BARE      0 X         
							7469          C GC_INUNDATED      2 X         
							7469          C  GC_NONWOODY      2 X         
							7469          C     GC_WOODY      3 X         
							7469          C   U_NONWOODY      2 X         
							7469          C      U_WOODY      3 X         
							7469          C   UNDERSTORY      D X         
							7469          D   C_BIGTREES      2 X         
							7469          D C_SMALLTREES      2 X         
							7469          D       CANOPY      M X         
							7469          D      GC_BARE      0 X         
							7469          D GC_INUNDATED      0 X         
							7469          D  GC_NONWOODY      4 X         
							7469          D     GC_WOODY      2 X         
							7469          D   U_NONWOODY      3 X         
							7469          D      U_WOODY      2 X         
							7469          D   UNDERSTORY      M X         
							7469          E   C_BIGTREES      0 X         
							7469          E C_SMALLTREES      2 X         
							7469          E       CANOPY      M X         
							7469          E      GC_BARE      0 X         
							7469          E GC_INUNDATED      0 X         
							7469          E  GC_NONWOODY      3 X         
							7469          E     GC_WOODY      2 X         
							7469          E   U_NONWOODY      3 X         
							7469          E      U_WOODY      2 X         
							7469          E   UNDERSTORY      M X         
							7469          F   C_BIGTREES      0 X         
							7469          F C_SMALLTREES      1 X         
							7469          F       CANOPY      D X         
							7469          F      GC_BARE      0 X         
							7469          F GC_INUNDATED      0 X         
							7469          F  GC_NONWOODY      3 X         
							7469          F     GC_WOODY      2 X         
							7469          F   U_NONWOODY      2 X         
							7469          F      U_WOODY      3 X         
							7469          F   UNDERSTORY      D X         
							7469          G   C_BIGTREES      2 X         
							7469          G C_SMALLTREES      1 X         
							7469          G       CANOPY      D X         
							7469          G      GC_BARE      0 X         
							7469          G GC_INUNDATED      0 X         
							7469          G  GC_NONWOODY      3 X         
							7469          G     GC_WOODY      2 X         
							7469          G   U_NONWOODY      3 X         
							7469          G      U_WOODY      2 X         
							7469          G   UNDERSTORY      D X         
							7469          H   C_BIGTREES      2 X         
							7469          H C_SMALLTREES      1 X         
							7469          H       CANOPY      M X         
							7469          H      GC_BARE      0 X         
							7469          H GC_INUNDATED      0 X         
							7469          H  GC_NONWOODY      3 X         
							7469          H     GC_WOODY      1 X         
							7469          H   U_NONWOODY      3 X         
							7469          H      U_WOODY      1 X         
							7469          H   UNDERSTORY      M X         
							7469          I   C_BIGTREES      2 X         
							7469          I C_SMALLTREES      2 X         
							7469          I       CANOPY      M X         
							7469          I      GC_BARE      0 X         
							7469          I GC_INUNDATED      0 X         
							7469          I  GC_NONWOODY      4 X         
							7469          I     GC_WOODY      1 X         
							7469          I   U_NONWOODY      3 X         
							7469          I      U_WOODY      3 X         
							7469          I   UNDERSTORY      M X         
							7469          J   C_BIGTREES      2 X         
							7469          J C_SMALLTREES      3 X         
							7469          J       CANOPY      C X         
							7469          J      GC_BARE      0 X         
							7469          J GC_INUNDATED      0 X         
							7469          J  GC_NONWOODY      3 X         
							7469          J     GC_WOODY      2 X         
							7469          J   U_NONWOODY      3 X         
							7469          J      U_WOODY      3 X         
							7469          J   UNDERSTORY      M X         
							7472          A C_SMALLTREES      1 X         
							7472          A       CANOPY      D X         
							7472          A      GC_BARE      0 X         
							7472          A GC_INUNDATED      0 X         
							7472          A  GC_NONWOODY      4 X         
							7472          A     GC_WOODY      0 X         
							7472          A   U_NONWOODY      1 X         
							7472          A      U_WOODY      1 X         
							7472          A   UNDERSTORY      D X         
							7472          B       CANOPY      N X         
							7472          B  GC_NONWOODY      1 X         
							7472          B   U_NONWOODY      4 X         
							7472          B      U_WOODY      1 X         
							7472          C   C_BIGTREES      1 X         
							7472          C C_SMALLTREES      1 X         
							7472          C       CANOPY      D X         
							7472          C  GC_NONWOODY      2 X         
							7472          C   U_NONWOODY      4 X         
							7472          C      U_WOODY      1 X         
							7472          C   UNDERSTORY      D X         
							7472          D       CANOPY      N X         
							7472          D      GC_BARE      1 X         
							7472          D GC_INUNDATED      0 X         
							7472          D  GC_NONWOODY      4 X         
							7472          D     GC_WOODY      0 X         
							7472          D   U_NONWOODY      1 X         
							7472          D      U_WOODY      1 X         
							7472          D   UNDERSTORY      D X         
							7472          E       CANOPY      N X         
							7472          E      GC_BARE      0 X         
							7472          E GC_INUNDATED      0 X         
							7472          E  GC_NONWOODY      3 X         
							7472          E     GC_WOODY      0 X         
							7472          E   U_NONWOODY      1 X         
							7472          E      U_WOODY      2 X         
							7472          E   UNDERSTORY      D X         
							7472          F       CANOPY      N X         
							7472          F      GC_BARE      0 X         
							7472          F GC_INUNDATED      0 X         
							7472          F  GC_NONWOODY      2 X         
							7472          F     GC_WOODY      2 X         
							7472          F   U_NONWOODY      1 X         
							7472          F      U_WOODY      2 X         
							7472          F   UNDERSTORY      D X         
							7472          G   C_BIGTREES      1 X         
							7472          G C_SMALLTREES      0 X         
							7472          G       CANOPY      D X         
							7472          G      GC_BARE      0 X         
							7472          G GC_INUNDATED      0 X         
							7472          G  GC_NONWOODY      1 X         
							7472          G     GC_WOODY      0 X         
							7472          G   U_NONWOODY      3 X         
							7472          G      U_WOODY      2 X         
							7472          G   UNDERSTORY      D X         
							7472          H       CANOPY      N X         
							7472          H      GC_BARE      0 X         
							7472          H GC_INUNDATED      0 X         
							7472          H  GC_NONWOODY      3 X         
							7472          H     GC_WOODY      0 X         
							7472          H   U_NONWOODY      3 X         
							7472          H      U_WOODY      1 X         
							7472          H   UNDERSTORY      D X         
							7472          I   C_BIGTREES      1 X         
							7472          I C_SMALLTREES      1 X         
							7472          I       CANOPY      D X         
							7472          I      GC_BARE      0 X         
							7472          I GC_INUNDATED      0 X         
							7472          I  GC_NONWOODY      3 X         
							7472          I     GC_WOODY      0 X         
							7472          I   U_NONWOODY      2 X         
							7472          I      U_WOODY      1 X         
							7472          I   UNDERSTORY      D X         
							7472          J   C_BIGTREES      1 X         
							7472          J C_SMALLTREES      1 X         
							7472          J       CANOPY      D X         
							7472          J      GC_BARE      0 X         
							7472          J GC_INUNDATED      0 X         
							7472          J  GC_NONWOODY      3 X         
							7472          J     GC_WOODY      0 X         
							7472          J   U_NONWOODY      2 X         
							7472          J      U_WOODY      1 X         
							7472          J   UNDERSTORY      D X         
							7492          B       CANOPY      N X         
							7492          B      GC_BARE      4 X         
							7492          B GC_INUNDATED      1 X         
							7492          B  GC_NONWOODY      0 X         
							7492          B     GC_WOODY      0 X         
							7492          B   UNDERSTORY      N X         
							7492          C       CANOPY      N X         
							7492          C      GC_BARE      4 X         
							7492          C GC_INUNDATED      1 X         
							7492          C  GC_NONWOODY      0 X         
							7492          C     GC_WOODY      1 X         
							7492          C      U_WOODY      1 X         
							7492          C   UNDERSTORY      D X         
							7492          D       CANOPY      N X         
							7492          D      GC_BARE      4 X         
							7492          D GC_INUNDATED      0 X         
							7492          D  GC_NONWOODY      0 X         
							7492          D     GC_WOODY      0 X         
							7492          D   UNDERSTORY      N X         
							7492          F       CANOPY      N X         
							7492          F      GC_BARE      2 X         
							7492          F GC_INUNDATED      3 X         
							7492          F  GC_NONWOODY      0 X         
							7492          F     GC_WOODY      0 X         
							7492          F   UNDERSTORY      N X         
							7492          H       CANOPY      N X         
							7492          H      GC_BARE      4 X         
							7492          H GC_INUNDATED      0 X         
							7492          H  GC_NONWOODY      0 X         
							7492          H     GC_WOODY      0 X         
							7492          H   UNDERSTORY      N X         
							7492          I       CANOPY      N X         
							7492          I      GC_BARE      4 X         
							7492          I GC_INUNDATED      0 X         
							7492          I  GC_NONWOODY      0 X         
							7492          I     GC_WOODY      0 X         
							7492          I   UNDERSTORY      N X         
							7492          J       CANOPY      N X         
							7492          J      GC_BARE      4 X         
							7492          J GC_INUNDATED      0 X         
							7492          J  GC_NONWOODY      0 X         
							7492          J     GC_WOODY      0 X         
							7518          A   C_BIGTREES      2 X         
							7518          A C_SMALLTREES      0 X         
							7518          A       CANOPY      C X         
							7518          A      GC_BARE      2 X         
							7518          A GC_INUNDATED      0 X         
							7518          A  GC_NONWOODY      3 X         
							7518          A     GC_WOODY      0 X         
							7518          A   U_NONWOODY      0 X         
							7518          A      U_WOODY      0 X         
							7518          A   UNDERSTORY      N X         
							7518          B   C_BIGTREES      1 X         
							7518          B C_SMALLTREES      0 X         
							7518          B       CANOPY      D X         
							7518          B      GC_BARE      3 X         
							7518          B GC_INUNDATED      0 X         
							7518          B  GC_NONWOODY      2 X         
							7518          B     GC_WOODY      1 X         
							7518          B   U_NONWOODY      1 X         
							7518          B      U_WOODY      1 X         
							7518          B   UNDERSTORY      M X         
							7518          C   C_BIGTREES      3 X         
							7518          C C_SMALLTREES      2 X         
							7518          C       CANOPY      D X         
							7518          C      GC_BARE      2 X         
							7518          C GC_INUNDATED      0 X         
							7518          C  GC_NONWOODY      3 X         
							7518          C     GC_WOODY      2 X         
							7518          C   U_NONWOODY      1 X         
							7518          C      U_WOODY      2 X         
							7518          C   UNDERSTORY      M X         
							7518          D   C_BIGTREES      2 X         
							7518          D C_SMALLTREES      2 X         
							7518          D       CANOPY      M X         
							7518          D      GC_BARE      1 X         
							7518          D GC_INUNDATED      0 X         
							7518          D  GC_NONWOODY      3 X         
							7518          D     GC_WOODY      2 X         
							7518          D   U_NONWOODY      1 X         
							7518          D      U_WOODY      2 X         
							7518          D   UNDERSTORY      M X         
							7518          E   C_BIGTREES      0 X         
							7518          E C_SMALLTREES      1 X         
							7518          E       CANOPY      D X         
							7518          E      GC_BARE      2 X         
							7518          E GC_INUNDATED      0 X         
							7518          E  GC_NONWOODY      4 X         
							7518          E     GC_WOODY      0 X         
							7518          E   U_NONWOODY      0 X         
							7518          E      U_WOODY      1 X         
							7518          E   UNDERSTORY      D X         
							7518          F   C_BIGTREES      1 X         
							7518          F C_SMALLTREES      3 X         
							7518          F       CANOPY      D X         
							7518          F      GC_BARE      2 X         
							7518          F GC_INUNDATED      0 X         
							7518          F  GC_NONWOODY      2 X         
							7518          F     GC_WOODY      2 X         
							7518          F   U_NONWOODY      1 X         
							7518          F      U_WOODY      3 X         
							7518          F   UNDERSTORY      D X         
							7518          G   C_BIGTREES      4 X         
							7518          G C_SMALLTREES      2 X         
							7518          G       CANOPY      M X         
							7518          G      GC_BARE      3 X         
							7518          G GC_INUNDATED      0 X         
							7518          G  GC_NONWOODY      1 X         
							7518          G     GC_WOODY      2 X         
							7518          G   U_NONWOODY      1 X         
							7518          G      U_WOODY      2 X         
							7518          G   UNDERSTORY      M X         
							7518          H   C_BIGTREES      1 X         
							7518          H C_SMALLTREES      0 X         
							7518          H       CANOPY      C X         
							7518          H      GC_BARE      4 X         
							7518          H GC_INUNDATED      0 X         
							7518          H  GC_NONWOODY      2 X         
							7518          H     GC_WOODY      0 X         
							7518          H   U_NONWOODY      0 X         
							7518          H      U_WOODY      1 X         
							7518          H   UNDERSTORY      M X         
							7518          I   C_BIGTREES      3 X         
							7518          I C_SMALLTREES      2 X         
							7518          I       CANOPY      M X         
							7518          I      GC_BARE      3 X         
							7518          I GC_INUNDATED      0 X         
							7518          I  GC_NONWOODY      1 X         
							7518          I     GC_WOODY      2 X         
							7518          I   U_NONWOODY      1 X         
							7518          I      U_WOODY      1 X         
							7518          I   UNDERSTORY      M X         
							7518          J   C_BIGTREES      1 X         
							7518          J C_SMALLTREES      1 X         
							7518          J       CANOPY      M X         
							7518          J      GC_BARE      3 X         
							7518          J GC_INUNDATED      0 X         
							7518          J  GC_NONWOODY      2 X         
							7518          J     GC_WOODY      0 X         
							7518          J   U_NONWOODY      1 X         
							7518          J      U_WOODY      2 X         
							7518          J   UNDERSTORY      M X         
							7518          K   C_BIGTREES      4 X         
							7518          K C_SMALLTREES      1 X         
							7518          K       CANOPY      M X         
							7518          K      GC_BARE      2 X         
							7518          K GC_INUNDATED      0 X         
							7518          K  GC_NONWOODY      2 X         
							7518          K     GC_WOODY      1 X         
							7518          K   U_NONWOODY      1 X         
							7518          K      U_WOODY      2 X         
							7518          K   UNDERSTORY      M X         
							7611          A   C_BIGTREES      3 X         
							7611          A C_SMALLTREES      1 X         
							7611          A       CANOPY      M X         
							7611          A      GC_BARE      1 X         
							7611          A GC_INUNDATED      0 X         
							7611          A  GC_NONWOODY      1 X         
							7611          A     GC_WOODY      1 X         
							7611          A   U_NONWOODY      1 X         
							7611          A      U_WOODY      3 X         
							7611          A   UNDERSTORY      D X         
							7611          B   C_BIGTREES      3 X         
							7611          B C_SMALLTREES      2 X         
							7611          B       CANOPY      M X         
							7611          B      GC_BARE      1 X         
							7611          B GC_INUNDATED      0 X         
							7611          B  GC_NONWOODY      1 X         
							7611          B     GC_WOODY      1 X         
							7611          B   U_NONWOODY      1 X         
							7611          B      U_WOODY      2 X         
							7611          B   UNDERSTORY      D X         
							7611          C   C_BIGTREES      1 X         
							7611          C C_SMALLTREES      2 X         
							7611          C       CANOPY      M X         
							7611          C      GC_BARE      1 X         
							7611          C GC_INUNDATED      0 X         
							7611          C  GC_NONWOODY      1 X         
							7611          C     GC_WOODY      1 X         
							7611          C   U_NONWOODY      0 X         
							7611          C      U_WOODY      3 X         
							7611          C   UNDERSTORY      M X         
							7611          D   C_BIGTREES      2 X         
							7611          D C_SMALLTREES      2 X         
							7611          D       CANOPY      M X         
							7611          D      GC_BARE      1 X         
							7611          D GC_INUNDATED      0 X         
							7611          D  GC_NONWOODY      2 X         
							7611          D     GC_WOODY      2 X         
							7611          D   U_NONWOODY      1 X         
							7611          D      U_WOODY      1 X         
							7611          D   UNDERSTORY      M X         
							7611          J   C_BIGTREES      1 X         
							7611          J C_SMALLTREES      1 X         
							7611          J       CANOPY      C X         
							7611          J      GC_BARE      1 X         
							7611          J GC_INUNDATED      0 X         
							7611          J  GC_NONWOODY      2 X         
							7611          J     GC_WOODY      3 X         
							7611          J   U_NONWOODY      0 X         
							7611          J      U_WOODY      1 X         
							7611          J   UNDERSTORY      M X         
							7723          A   C_BIGTREES      2 X         
							7723          A C_SMALLTREES      2 X         
							7723          A       CANOPY      D X         
							7723          A      GC_BARE      1 X         
							7723          A GC_INUNDATED      0 X         
							7723          A  GC_NONWOODY      4 X         
							7723          A     GC_WOODY      2 X         
							7723          A   U_NONWOODY      2 X         
							7723          A      U_WOODY      2 X         
							7723          A   UNDERSTORY      D X         
							7723          B   C_BIGTREES      0 X         
							7723          B C_SMALLTREES      1 X         
							7723          B       CANOPY      D X         
							7723          B      GC_BARE      0 X         
							7723          B GC_INUNDATED      4 X         
							7723          B  GC_NONWOODY      1 X         
							7723          B     GC_WOODY      1 X         
							7723          B   U_NONWOODY      3 X         
							7723          B      U_WOODY      3 X         
							7723          B   UNDERSTORY      D X         
							7723          C   C_BIGTREES      1 X         
							7723          C C_SMALLTREES      1 X         
							7723          C       CANOPY      D X         
							7723          C      GC_BARE      1 X         
							7723          C GC_INUNDATED      1 X         
							7723          C  GC_NONWOODY      4 X         
							7723          C     GC_WOODY      1 X         
							7723          C   U_NONWOODY      3 X         
							7723          C      U_WOODY      1 X         
							7723          C   UNDERSTORY      D X         
							7723          D   C_BIGTREES      0 X         
							7723          D C_SMALLTREES      1 X         
							7723          D       CANOPY      D X         
							7723          D      GC_BARE      1 X         
							7723          D GC_INUNDATED      1 X         
							7723          D  GC_NONWOODY      4 X         
							7723          D     GC_WOODY      1 X         
							7723          D   U_NONWOODY      1 X         
							7723          D      U_WOODY      2 X         
							7723          D   UNDERSTORY      D X         
							7723          E   C_BIGTREES      1 X         
							7723          E C_SMALLTREES      1 X         
							7723          E       CANOPY      M X         
							7723          E      GC_BARE      1 X         
							7723          E GC_INUNDATED      0 X         
							7723          E  GC_NONWOODY      4 X         
							7723          E     GC_WOODY      1 X         
							7723          E   U_NONWOODY      1 X         
							7723          E      U_WOODY      1 X         
							7723          E   UNDERSTORY      M X         
							7723          F   C_BIGTREES      2 X         
							7723          F C_SMALLTREES      1 X         
							7723          F       CANOPY      D X         
							7723          F      GC_BARE      1 X         
							7723          F GC_INUNDATED      0 X         
							7723          F  GC_NONWOODY      4 X         
							7723          F     GC_WOODY      1 X         
							7723          F   U_NONWOODY      1 X         
							7723          F      U_WOODY      1 X         
							7723          F   UNDERSTORY      D X         
							7723          I   C_BIGTREES      0 X         
							7723          I C_SMALLTREES      1 X         
							7723          I       CANOPY      D X         
							7723          I      GC_BARE      0 X         
							7723          I GC_INUNDATED      3 X         
							7723          I  GC_NONWOODY      3 X         
							7723          I     GC_WOODY      0 X         
							7723          I   U_NONWOODY      3 X         
							7723          I      U_WOODY      3 X         
							7723          I   UNDERSTORY      D X         
							7723          J   C_BIGTREES      0 X         
							7723          J C_SMALLTREES      1 X         
							7723          J       CANOPY      D X         
							7723          J      GC_BARE      2 X         
							7723          J GC_INUNDATED      1 X         
							7723          J  GC_NONWOODY      3 X         
							7723          J     GC_WOODY      0 X         
							7723          J   U_NONWOODY      1 X         
							7723          J      U_WOODY      0 X         
							7723          Q   C_BIGTREES      0 X         
							7723          Q C_SMALLTREES      1 X         
							7723          Q       CANOPY      D X         
							7723          Q      GC_BARE      0 X         
							7723          Q GC_INUNDATED      4 X         
							7723          Q  GC_NONWOODY      4 X         
							7723          Q     GC_WOODY      1 X         
							7723          Q   U_NONWOODY      4 X         
							7723          Q      U_WOODY      1 X         
							7723          Q   UNDERSTORY      D X         
							7723          R   C_BIGTREES      1 X         
							7723          R C_SMALLTREES      1 X         
							7723          R       CANOPY      D X         
							7723          R      GC_BARE      1 X         
							7723          R GC_INUNDATED      1 X         
							7723          R  GC_NONWOODY      4 X         
							7723          R     GC_WOODY      1 X         
							7723          R   U_NONWOODY      1 X         
							7723          R      U_WOODY      1 X         
							7723          R   UNDERSTORY      D X         
							7784          J   C_BIGTREES      0 X         
							7784          J C_SMALLTREES      1 X         
							7784          J       CANOPY      D X         
							7784          J      GC_BARE      1 X         
							7784          J GC_INUNDATED      2 X         
							7784          J  GC_NONWOODY      2 X         
							7784          J     GC_WOODY      2 X         
							7784          J   U_NONWOODY      1 X         
							7784          J      U_WOODY      2 X         
							7784          J   UNDERSTORY      D X         
							7797          A   C_BIGTREES      0 X         
							7797          A C_SMALLTREES      0 X         
							7797          A       CANOPY      N X         
							7797          A      GC_BARE      4 X         
							7797          A GC_INUNDATED      0 X         
							7797          A  GC_NONWOODY      1 X         
							7797          A     GC_WOODY      1 X         
							7797          A   U_NONWOODY      1 X         
							7797          A      U_WOODY      1 X         
							7797          A   UNDERSTORY      D X         
							7797          B   C_BIGTREES      0 X         
							7797          B C_SMALLTREES      0 X         
							7797          B       CANOPY      N X         
							7797          B      GC_BARE      0 X         
							7797          B GC_INUNDATED      2 X         
							7797          B  GC_NONWOODY      4 X         
							7797          B     GC_WOODY      1 X         
							7797          B   U_NONWOODY      4 X         
							7797          B      U_WOODY      1 X         
							7797          B   UNDERSTORY      D X         
							7797          C   C_BIGTREES      1 X         
							7797          C C_SMALLTREES      4 X         
							7797          C       CANOPY      D X         
							7797          C      GC_BARE      0 X         
							7797          C GC_INUNDATED      0 X         
							7797          C  GC_NONWOODY      3 X         
							7797          C     GC_WOODY      2 X         
							7797          C   U_NONWOODY      1 X         
							7797          C      U_WOODY      3 X         
							7797          C   UNDERSTORY      D X         
							7797          D   C_BIGTREES      3 X         
							7797          D C_SMALLTREES      2 X         
							7797          D       CANOPY      M X         
							7797          D      GC_BARE      1 X         
							7797          D GC_INUNDATED      0 X         
							7797          D  GC_NONWOODY      2 X         
							7797          D     GC_WOODY      3 X         
							7797          D   U_NONWOODY      1 X         
							7797          D      U_WOODY      3 X         
							7797          D   UNDERSTORY      M X         
							7797          E   C_BIGTREES      3 X         
							7797          E C_SMALLTREES      1 X         
							7797          E       CANOPY      D X         
							7797          E      GC_BARE      0 X         
							7797          E GC_INUNDATED      1 X         
							7797          E  GC_NONWOODY      2 X         
							7797          E     GC_WOODY      2 X         
							7797          E   U_NONWOODY      2 X         
							7797          E      U_WOODY      2 X         
							7797          E   UNDERSTORY      D X         
							7797          F   C_BIGTREES      0 X         
							7797          F C_SMALLTREES      0 X         
							7797          F       CANOPY      N X         
							7797          F      GC_BARE      0 X         
							7797          F GC_INUNDATED      3 X         
							7797          F  GC_NONWOODY      3 X         
							7797          F     GC_WOODY      0 X         
							7797          F   U_NONWOODY      0 X         
							7797          F      U_WOODY      0 X         
							7797          F   UNDERSTORY      N X         
							7797          G   C_BIGTREES      0 X         
							7797          G C_SMALLTREES      0 X         
							7797          G       CANOPY      N X         
							7797          G      GC_BARE      0 X         
							7797          G GC_INUNDATED      1 X         
							7797          G  GC_NONWOODY      4 X         
							7797          G     GC_WOODY      0 X         
							7797          G   U_NONWOODY      4 X         
							7797          G      U_WOODY      4 X         
							7797          G   UNDERSTORY      D X         
							7797          H   C_BIGTREES      2 X         
							7797          H C_SMALLTREES      3 X         
							7797          H       CANOPY      M X         
							7797          H      GC_BARE      1 X         
							7797          H GC_INUNDATED      0 X         
							7797          H  GC_NONWOODY      2 X         
							7797          H     GC_WOODY      2 X         
							7797          H   U_NONWOODY      1 X         
							7797          H      U_WOODY      3 X         
							7797          H   UNDERSTORY      M X         
							7797          I   C_BIGTREES      2 X         
							7797          I C_SMALLTREES      2 X         
							7797          I       CANOPY      D X         
							7797          I      GC_BARE      0 X         
							7797          I GC_INUNDATED      0 X         
							7797          I  GC_NONWOODY      4 X         
							7797          I     GC_WOODY      1 X         
							7797          I   U_NONWOODY      1 X         
							7797          I      U_WOODY      2 X         
							7797          I   UNDERSTORY      D X         
							7797          J   C_BIGTREES      0 X         
							7797          J C_SMALLTREES      0 X         
							7797          J       CANOPY      N X         
							7797          J      GC_BARE      0 X         
							7797          J GC_INUNDATED      2 X         
							7797          J  GC_NONWOODY      3 X         
							7797          J     GC_WOODY      0 X         
							7797          J   U_NONWOODY      3 X         
							7797          J      U_WOODY      0 X         
							7797          J   UNDERSTORY      D X         
							7797          K   C_BIGTREES      0 X         
							7797          K C_SMALLTREES      0 X         
							7797          K       CANOPY      N X         
							7797          K      GC_BARE      0 X         
							7797          K GC_INUNDATED      1 X         
							7797          K  GC_NONWOODY      4 X         
							7797          K     GC_WOODY      0 X         
							7797          K   U_NONWOODY      4 X         
							7797          K      U_WOODY      0 X         
							7797          K   UNDERSTORY      D X         
							7797          L   C_BIGTREES      1 X         
							7797          L C_SMALLTREES      2 X         
							7797          L       CANOPY      D X         
							7797          L      GC_BARE      0 X         
							7797          L GC_INUNDATED      0 X         
							7797          L  GC_NONWOODY      1 X         
							7797          L     GC_WOODY      3 X         
							7797          L   U_NONWOODY      3 X         
							7797          L      U_WOODY      3 X         
							7797          L   UNDERSTORY      D X         
							7797          M   C_BIGTREES      1 X         
							7797          M C_SMALLTREES      3 X         
							7797          M       CANOPY      D X         
							7797          M      GC_BARE      0 X         
							7797          M GC_INUNDATED      0 X         
							7797          M  GC_NONWOODY      1 X         
							7797          M     GC_WOODY      4 X         
							7797          M   U_NONWOODY      0 X         
							7797          M      U_WOODY      4 X         
							7797          M   UNDERSTORY      D X"
						)
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
						
	return(fake)		
}


nlaRiparianVegetationTest.expectedResults2007 <- function()
# Values expected based on test data.  These were taken directly from tblPHABMET_LONG
# in the 2007 NLA databse.
#phab <- sqlFetch(nla07, 'tblPHABMET_LONG', stringsAsFactors=FALSE) 
#tt<-subset(phab, VISIT_ID %in% c(7469,7472,7492,7518,7611,7723,7784,7797) & grepl('^rv', PARAMETER))
#tt <- tt[order(tt$VISIT_ID,tt$PARAMETER), c('VISIT_ID','PARAMETER','RESULT')]	
{
	tc <- textConnection("  SITE            METRIC               VALUE
							7469        RVFCCANBIG                0.13
							7469      RVFCCANSMALL               0.205
							7469       RVFCGNDBARE                   0
							7469  RVFCGNDINUNDATED  0.0274224806201550
							7469       RVFCGNDNONW   0.619738137185811
							7469      RVFCGNDWOODY   0.352839382194033
							7469       RVFCUNDNONW                0.39
							7469      RVFCUNDWOODY                0.44
							7469        RVFPCANBIG                 0.6
							7469  RVFPCANBROADLEAF                   0
							7469 RVFPCANCONIFEROUS                 0.2
							7469  RVFPCANDECIDUOUS                 0.2
							7469      RVFPCANMIXED                 0.4
							7469       RVFPCANNONE                 0.2
							7469      RVFPCANSMALL                 0.8
							7469       RVFPGNDBARE                   0
							7469  RVFPGNDINUNDATED                 0.2
							7469       RVFPGNDNONW                   1
							7469      RVFPGNDWOODY                   1
							7469  RVFPUNDBROADLEAF                   0
							7469 RVFPUNDCONIFEROUS                   0
							7469  RVFPUNDDECIDUOUS                 0.5
							7469      RVFPUNDMIXED                 0.5
							7469       RVFPUNDNONE                   0
							7469       RVFPUNDNONW                   1
							7469      RVFPUNDWOODY                   1
							7469         RVICANOPY               0.335
							7469         RVICANUND               1.195
							7469         RVIGROUND              0.9175
							7469          RVIHERBS               0.955
							7469       RVITALLWOOD                0.79
							7469       RVITOTALVEG              2.0825
							7469     RVIUNDERSTORY                0.86
							7469          RVIWOODY              1.1275
							7469         RVNCANBIG                  10
							7469         RVNCANOPY                  10
							7469       RVNCANSMALL                  10
							7469        RVNGNDBARE                  10
							7469   RVNGNDINUNDATED                  10
							7469        RVNGNDNONW                  10
							7469       RVNGNDWOODY                  10
							7469     RVNUNDERSTORY                  10
							7469        RVNUNDNONW                  10
							7469       RVNUNDWOODY                  10
							7469         RVVCANBIG   0.127366487830285
							7469       RVVCANSMALL   0.220100986922922
							7469        RVVGNDBARE                   0
							7469   RVVGNDINUNDATED  0.0732571384018193
							7469        RVVGNDNONW   0.285787659105942
							7469       RVVGNDWOODY   0.256379220160103
							7469        RVVUNDNONW   0.218962198665533
							7469       RVVUNDWOODY   0.238397427279183
							7472        RVFCCANBIG                0.05
							7472      RVFCCANSMALL                0.04
							7472       RVFCGNDBARE 0.00675675675675676
							7472  RVFCGNDINUNDATED                   0
							7472       RVFCGNDNONW   0.944594594594595
							7472      RVFCGNDWOODY              0.0625
							7472       RVFCUNDNONW                0.36
							7472      RVFCUNDWOODY                0.11
							7472        RVFPCANBIG                   1
							7472  RVFPCANBROADLEAF                   0
							7472 RVFPCANCONIFEROUS                   0
							7472  RVFPCANDECIDUOUS                 0.5
							7472      RVFPCANMIXED                   0
							7472       RVFPCANNONE                 0.5
							7472      RVFPCANSMALL                 0.8
							7472       RVFPGNDBARE               0.125
							7472  RVFPGNDINUNDATED                   0
							7472       RVFPGNDNONW                   1
							7472      RVFPGNDWOODY               0.125
							7472  RVFPUNDBROADLEAF                   0
							7472 RVFPUNDCONIFEROUS                   0
							7472  RVFPUNDDECIDUOUS                   1
							7472      RVFPUNDMIXED                   0
							7472       RVFPUNDNONE                   0
							7472       RVFPUNDNONW                   1
							7472      RVFPUNDWOODY                   1
							7472         RVICANOPY                0.08
							7472         RVICANUND                0.51
							7472         RVIGROUND                0.49
							7472          RVIHERBS               0.825
							7472       RVITALLWOOD                0.15
							7472       RVITOTALVEG                   1
							7472     RVIUNDERSTORY                0.47
							7472          RVIWOODY               0.175
							7472         RVNCANBIG                   4
							7472         RVNCANOPY                  10
							7472       RVNCANSMALL                   5
							7472        RVNGNDBARE                   8
							7472   RVNGNDINUNDATED                   8
							7472        RVNGNDNONW                  10
							7472       RVNGNDWOODY                   8
							7472     RVNUNDERSTORY                   9
							7472        RVNUNDNONW                  10
							7472       RVNUNDWOODY                  10
							7472         RVVCANBIG                   0
							7472       RVVCANSMALL  0.0223606797749979
							7472        RVVGNDBARE  0.0191109940861229
							7472   RVVGNDINUNDATED                   0
							7472        RVVGNDNONW   0.157135557641698
							7472       RVVGNDWOODY   0.176776695296637
							7472        RVVUNDNONW   0.338542628203763
							7472       RVVUNDWOODY  0.0966091783079296
							7492        RVFCCANBIG                  NA
							7492      RVFCCANSMALL                  NA
							7492       RVFCGNDBARE   0.878058878058878
							7492  RVFCGNDINUNDATED   0.114615114615115
							7492       RVFCGNDNONW                   0
							7492      RVFCGNDWOODY 0.00732600732600733
							7492       RVFCUNDNONW                  NA
							7492      RVFCUNDWOODY                0.05
							7492        RVFPCANBIG                  NA
							7492  RVFPCANBROADLEAF                   0
							7492 RVFPCANCONIFEROUS                   0
							7492  RVFPCANDECIDUOUS                   0
							7492      RVFPCANMIXED                   0
							7492       RVFPCANNONE                   1
							7492      RVFPCANSMALL                  NA
							7492       RVFPGNDBARE                   1
							7492  RVFPGNDINUNDATED   0.428571428571429
							7492       RVFPGNDNONW                   0
							7492      RVFPGNDWOODY   0.142857142857143
							7492  RVFPUNDBROADLEAF                   0
							7492 RVFPUNDCONIFEROUS                   0
							7492  RVFPUNDDECIDUOUS   0.166666666666667
							7492      RVFPUNDMIXED                   0
							7492       RVFPUNDNONE   0.833333333333333
							7492       RVFPUNDNONW                  NA
							7492      RVFPUNDWOODY                   1
							7492         RVICANOPY                  NA
							7492         RVICANUND                0.05
							7492         RVIGROUND   0.103571428571429
							7492          RVIHERBS                   0
							7492       RVITALLWOOD                0.05
							7492       RVITOTALVEG  0.0142857142857143
							7492     RVIUNDERSTORY                0.05
							7492          RVIWOODY  0.0142857142857143
							7492         RVNCANBIG                   0
							7492         RVNCANOPY                   7
							7492       RVNCANSMALL                   0
							7492        RVNGNDBARE                   7
							7492   RVNGNDINUNDATED                   7
							7492        RVNGNDNONW                   7
							7492       RVNGNDWOODY                   7
							7492     RVNUNDERSTORY                   6
							7492        RVNUNDNONW                   0
							7492       RVNUNDWOODY                   1
							7492         RVVCANBIG                  NA
							7492       RVVCANSMALL                  NA
							7492        RVVGNDBARE   0.256619216559826
							7492   RVVGNDINUNDATED    0.25799291868504
							7492        RVVGNDNONW                   0
							7492       RVVGNDWOODY  0.0193827934876527
							7492        RVVUNDNONW                  NA
							7492       RVVUNDWOODY                  NA
							7518        RVFCCANBIG   0.318434343434343
							7518      RVFCCANSMALL   0.154292929292929
							7518       RVFCGNDBARE   0.459000759635009
							7518  RVFCGNDINUNDATED                   0
							7518       RVFCGNDNONW   0.398173215086534
							7518      RVFCGNDWOODY   0.142826025278457
							7518       RVFCUNDNONW  0.0363636363636364
							7518      RVFCUNDWOODY   0.184090909090909
							7518        RVFPCANBIG   0.909090909090909
							7518  RVFPCANBROADLEAF                   0
							7518 RVFPCANCONIFEROUS   0.181818181818182
							7518  RVFPCANDECIDUOUS   0.363636363636364
							7518      RVFPCANMIXED   0.454545454545455
							7518       RVFPCANNONE                   0
							7518      RVFPCANSMALL   0.727272727272727
							7518       RVFPGNDBARE                   1
							7518  RVFPGNDINUNDATED                   0
							7518       RVFPGNDNONW                   1
							7518      RVFPGNDWOODY   0.636363636363636
							7518  RVFPUNDBROADLEAF                   0
							7518 RVFPUNDCONIFEROUS                   0
							7518  RVFPUNDDECIDUOUS   0.181818181818182
							7518      RVFPUNDMIXED   0.727272727272727
							7518       RVFPUNDNONE   0.090909090909091
							7518       RVFPUNDNONW   0.727272727272727
							7518      RVFPUNDWOODY   0.909090909090909
							7518         RVICANOPY   0.484090909090909
							7518         RVICANUND   0.704545454545455
							7518         RVIGROUND   0.481818181818182
							7518          RVIHERBS   0.395454545454545
							7518       RVITALLWOOD   0.668181818181818
							7518       RVITOTALVEG    1.18636363636364
							7518     RVIUNDERSTORY   0.220454545454545
							7518          RVIWOODY   0.790909090909091
							7518         RVNCANBIG                  11
							7518         RVNCANOPY                  11
							7518       RVNCANSMALL                  11
							7518        RVNGNDBARE                  11
							7518   RVNGNDINUNDATED                  11
							7518        RVNGNDNONW                  11
							7518       RVNGNDWOODY                  11
							7518     RVNUNDERSTORY                  11
							7518        RVNUNDNONW                  11
							7518       RVNUNDWOODY                  11
							7518         RVVCANBIG   0.324254302938583
							7518       RVVCANSMALL   0.176957149536734
							7518        RVVGNDBARE   0.241867302405549
							7518   RVVGNDINUNDATED                   0
							7518        RVVGNDNONW   0.247341340184447
							7518       RVVGNDWOODY   0.140438641059294
							7518        RVVUNDNONW  0.0233549683248457
							7518       RVVUNDWOODY   0.167433840394022
							7611        RVFCCANBIG                 0.3
							7611      RVFCCANSMALL                0.17
							7611       RVFCGNDBARE   0.229610389610390
							7611  RVFCGNDINUNDATED                   0
							7611       RVFCGNDNONW   0.348051948051948
							7611      RVFCGNDWOODY   0.422337662337662
							7611       RVFCUNDNONW                0.03
							7611      RVFCUNDWOODY                 0.3
							7611        RVFPCANBIG                   1
							7611  RVFPCANBROADLEAF                   0
							7611 RVFPCANCONIFEROUS                 0.2
							7611  RVFPCANDECIDUOUS                   0
							7611      RVFPCANMIXED                 0.8
							7611       RVFPCANNONE                   0
							7611      RVFPCANSMALL                   1
							7611       RVFPGNDBARE                   1
							7611  RVFPGNDINUNDATED                   0
							7611       RVFPGNDNONW                   1
							7611      RVFPGNDWOODY                   1
							7611  RVFPUNDBROADLEAF                   0
							7611 RVFPUNDCONIFEROUS                   0
							7611  RVFPUNDDECIDUOUS                 0.4
							7611      RVFPUNDMIXED                 0.6
							7611       RVFPUNDNONE                   0
							7611       RVFPUNDNONW                 0.6
							7611      RVFPUNDWOODY                   1
							7611         RVICANOPY                0.47
							7611         RVICANUND                 0.8
							7611         RVIGROUND               0.325
							7611          RVIHERBS                0.16
							7611       RVITALLWOOD                0.77
							7611       RVITOTALVEG               1.125
							7611     RVIUNDERSTORY                0.33
							7611          RVIWOODY               0.965
							7611         RVNCANBIG                   5
							7611         RVNCANOPY                   5
							7611       RVNCANSMALL                   5
							7611        RVNGNDBARE                   5
							7611   RVNGNDINUNDATED                   5
							7611        RVNGNDNONW                   5
							7611       RVNGNDWOODY                   5
							7611     RVNUNDERSTORY                   5
							7611        RVNUNDNONW                   5
							7611       RVNUNDWOODY                   5
							7611         RVVCANBIG   0.263983901024286
							7611       RVVCANSMALL   0.109544511501033
							7611        RVVGNDBARE   0.142529336612261
							7611   RVVGNDINUNDATED                   0
							7611        RVVGNDNONW  0.0630015157133382
							7611       RVVGNDWOODY   0.141364894095863
							7611        RVVUNDNONW  0.0273861278752583
							7611       RVVUNDWOODY   0.263983901024286
							7723        RVFCCANBIG               0.065
							7723      RVFCCANSMALL                0.07
							7723       RVFCGNDBARE  0.0577173043182384
							7723  RVFCGNDINUNDATED    0.20870313291045
							7723       RVFCGNDNONW   0.679506427522774
							7723      RVFCGNDWOODY  0.0540731352485374
							7723       RVFCUNDNONW               0.295
							7723      RVFCUNDWOODY               0.175
							7723        RVFPCANBIG                 0.5
							7723  RVFPCANBROADLEAF                   0
							7723 RVFPCANCONIFEROUS                   0
							7723  RVFPCANDECIDUOUS                 0.9
							7723      RVFPCANMIXED                 0.1
							7723       RVFPCANNONE                   0
							7723      RVFPCANSMALL                   1
							7723       RVFPGNDBARE                 0.7
							7723  RVFPGNDINUNDATED                 0.7
							7723       RVFPGNDNONW                   1
							7723      RVFPGNDWOODY                 0.8
							7723  RVFPUNDBROADLEAF                   0
							7723 RVFPUNDCONIFEROUS                   0
							7723  RVFPUNDDECIDUOUS   0.888888888888889
							7723      RVFPUNDMIXED   0.111111111111111
							7723       RVFPUNDNONE                   0
							7723       RVFPUNDNONW                   1
							7723      RVFPUNDWOODY                 0.9
							7723         RVICANOPY               0.135
							7723         RVICANUND               0.635
							7723         RVIGROUND               1.045
							7723          RVIHERBS              1.0425
							7723       RVITALLWOOD               0.325
							7723       RVITOTALVEG              1.4275
							7723     RVIUNDERSTORY                 0.5
							7723          RVIWOODY               0.385
							7723         RVNCANBIG                  10
							7723         RVNCANOPY                  10
							7723       RVNCANSMALL                  10
							7723        RVNGNDBARE                  10
							7723   RVNGNDINUNDATED                  10
							7723        RVNGNDNONW                  10
							7723       RVNGNDWOODY                  10
							7723     RVNUNDERSTORY                   9
							7723        RVNUNDNONW                  10
							7723       RVNUNDWOODY                  10
							7723         RVVCANBIG   0.100138792571999
							7723       RVVCANSMALL  0.0632455532033676
							7723        RVVGNDBARE  0.0833525714649316
							7723   RVVGNDINUNDATED   0.310291823142126
							7723        RVVGNDNONW   0.269775912479048
							7723       RVVGNDWOODY  0.0594699479223591
							7723        RVVUNDNONW   0.298328677803526
							7723       RVVUNDWOODY   0.191847740553689
							7784        RVFCCANBIG                   0
							7784      RVFCCANSMALL                0.05
							7784       RVFCGNDBARE              0.0625
							7784  RVFCGNDINUNDATED              0.3125
							7784       RVFCGNDNONW              0.3125
							7784      RVFCGNDWOODY              0.3125
							7784       RVFCUNDNONW                0.05
							7784      RVFCUNDWOODY                0.25
							7784        RVFPCANBIG                   0
							7784  RVFPCANBROADLEAF                   0
							7784 RVFPCANCONIFEROUS                   0
							7784  RVFPCANDECIDUOUS                   1
							7784      RVFPCANMIXED                   0
							7784       RVFPCANNONE                   0
							7784      RVFPCANSMALL                   1
							7784       RVFPGNDBARE                   1
							7784  RVFPGNDINUNDATED                   1
							7784       RVFPGNDNONW                   1
							7784      RVFPGNDWOODY                   1
							7784  RVFPUNDBROADLEAF                   0
							7784 RVFPUNDCONIFEROUS                   0
							7784  RVFPUNDDECIDUOUS                   1
							7784      RVFPUNDMIXED                   0
							7784       RVFPUNDNONE                   0
							7784       RVFPUNDNONW                   1
							7784      RVFPUNDWOODY                   1
							7784         RVICANOPY                0.05
							7784         RVICANUND                0.35
							7784         RVIGROUND                0.75
							7784          RVIHERBS                 0.3
							7784       RVITALLWOOD                 0.3
							7784       RVITOTALVEG                0.85
							7784     RVIUNDERSTORY                 0.3
							7784          RVIWOODY                0.55
							7784         RVNCANBIG                   1
							7784         RVNCANOPY                   1
							7784       RVNCANSMALL                   1
							7784        RVNGNDBARE                   1
							7784   RVNGNDINUNDATED                   1
							7784        RVNGNDNONW                   1
							7784       RVNGNDWOODY                   1
							7784     RVNUNDERSTORY                   1
							7784        RVNUNDNONW                   1
							7784       RVNUNDWOODY                   1
							7784         RVVCANBIG                  NA
							7784       RVVCANSMALL                  NA
							7784        RVVGNDBARE                  NA
							7784   RVVGNDINUNDATED                  NA
							7784        RVVGNDNONW                  NA
							7784       RVVGNDWOODY                  NA
							7784        RVVUNDNONW                  NA
							7784       RVVUNDWOODY                  NA
							7797        RVFCCANBIG   0.138461538461538
							7797      RVFCCANSMALL   0.217307692307692
							7797       RVFCGNDBARE  0.0804221419606035
							7797  RVFCGNDINUNDATED    0.09344718919187
							7797       RVFCGNDNONW   0.527430721767874
							7797      RVFCGNDWOODY   0.298699947079652
							7797       RVFCUNDNONW   0.294230769230769
							7797      RVFCUNDWOODY   0.323076923076923
							7797        RVFPCANBIG   0.538461538461538
							7797  RVFPCANBROADLEAF                   0
							7797 RVFPCANCONIFEROUS                   0
							7797  RVFPCANDECIDUOUS   0.384615384615385
							7797      RVFPCANMIXED   0.153846153846154
							7797       RVFPCANNONE   0.461538461538462
							7797      RVFPCANSMALL   0.538461538461538
							7797       RVFPGNDBARE   0.230769230769231
							7797  RVFPGNDINUNDATED   0.461538461538462
							7797       RVFPGNDNONW                   1
							7797      RVFPGNDWOODY   0.692307692307692
							7797  RVFPUNDBROADLEAF                   0
							7797 RVFPUNDCONIFEROUS                   0
							7797  RVFPUNDDECIDUOUS   0.769230769230769
							7797      RVFPUNDMIXED   0.153846153846154
							7797       RVFPUNDNONE  0.0769230769230769
							7797       RVFPUNDNONW   0.846153846153846
							7797      RVFPUNDWOODY   0.769230769230769
							7797         RVICANOPY   0.355769230769231
							7797         RVICANUND    1.04230769230769
							7797         RVIGROUND   0.790384615384615
							7797          RVIHERBS                 0.8
							7797       RVITALLWOOD   0.713461538461538
							7797       RVITOTALVEG    1.73846153846154
							7797     RVIUNDERSTORY   0.686538461538462
							7797          RVIWOODY   0.938461538461538
							7797         RVNCANBIG                  13
							7797         RVNCANOPY                  13
							7797       RVNCANSMALL                  13
							7797        RVNGNDBARE                  13
							7797   RVNGNDINUNDATED                  13
							7797        RVNGNDNONW                  13
							7797       RVNGNDWOODY                  13
							7797     RVNUNDERSTORY                  13
							7797        RVNUNDNONW                  13
							7797       RVNUNDWOODY                  13
							7797         RVVCANBIG   0.213017845647032
							7797       RVVCANSMALL   0.289105051441912
							7797        RVVGNDBARE   0.247123549904873
							7797   RVVGNDINUNDATED   0.154834308846041
							7797        RVVGNDNONW   0.335528265891371
							7797       RVVGNDWOODY   0.356555497904981
							7797        RVVUNDNONW   0.330585630320434
							7797       RVVUNDWOODY   0.293055302385762"
						)
	
	longMets <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)

	return(longMets)
}


nlaRiparianVegetationTest.createTestDataWithDrawDown <- function()
# This data is based on NLA2012 data for the following UIDs:
#	6362	data exists only at station J, HORIZ_DIST_DD present
#	6396	full data exists at stations A-J
#	6449	Data at A-J + K,L; some missing canopy and understory, LIT and DD
#	6518	Frequently missing DD data, but something at all stations
#	6530	Missing DD at all stations, full LIT data
#	6618	Data nearly full, missing 2 understory and 1 HORIZ_DIST_DD
#	6647	Missing DD at some stations
#	7431	Missing DD at all stations, missing some LIT data
#	1000222	Full LIT and nearly full DD data for stations A,B,C,I,J; otherwise missing.
{
	tc <- textConnection("	 SITE SAMPLE_TYPE STATION       CLASS VALUE FLAG  FORM_TYPE
							6362        PHAB       J      C_BIGTREES      0 NA    PHAB_BACK
							6362        PHAB       J   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6362        PHAB       J    C_SMALLTREES      0 NA    PHAB_BACK
						    6362        PHAB       J C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6362        PHAB       J         GC_BARE      0 NA    PHAB_BACK
						    6362        PHAB       J      GC_BARE_DD      4 NA    PHAB_BACK
						    6362        PHAB       J    GC_INUNDATED      0 NA    PHAB_BACK
						    6362        PHAB       J GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6362        PHAB       J     GC_NONWOODY      2 NA    PHAB_BACK
						    6362        PHAB       J  GC_NONWOODY_DD      1 NA    PHAB_BACK
						    6362        PHAB       J        GC_WOODY      3 NA    PHAB_BACK
						    6362        PHAB       J     GC_WOODY_DD      0 NA    PHAB_BACK
						    6362        PHAB       J   HORIZ_DIST_DD   20.0 NA   PHAB_FRONT
						    6362        PHAB       J      U_NONWOODY      2 NA    PHAB_BACK
						    6362        PHAB       J   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6362        PHAB       J         U_WOODY      3 NA    PHAB_BACK
						    6362        PHAB       J      U_WOODY_DD      0 NA    PHAB_BACK
						    6362        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       A      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       A   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       A    C_SMALLTREES      1 NA    PHAB_BACK
						    6396        PHAB       A C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       A          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       A       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       A         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       A      GC_BARE_DD      1 NA    PHAB_BACK
						    6396        PHAB       A    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       A GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       A     GC_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       A  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       A        GC_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       A     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       A   HORIZ_DIST_DD    7.0 NA   PHAB_FRONT
						    6396        PHAB       A      U_NONWOODY      1 NA    PHAB_BACK
						    6396        PHAB       A   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       A         U_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       A      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       A      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       A   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       B      C_BIGTREES      2 NA    PHAB_BACK
						    6396        PHAB       B   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       B    C_SMALLTREES      2 NA    PHAB_BACK
						    6396        PHAB       B C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       B          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       B       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       B         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       B      GC_BARE_DD      2 NA    PHAB_BACK
						    6396        PHAB       B    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       B GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       B     GC_NONWOODY      2 NA    PHAB_BACK
						    6396        PHAB       B  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       B        GC_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       B     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       B   HORIZ_DIST_DD  007.0 NA   PHAB_FRONT
						    6396        PHAB       B      U_NONWOODY      1 NA    PHAB_BACK
						    6396        PHAB       B   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       B         U_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       B      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       B      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       B   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       C      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       C   C_BIGTREES_DD      1 NA    PHAB_BACK
						    6396        PHAB       C    C_SMALLTREES      2 NA    PHAB_BACK
						    6396        PHAB       C C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       C          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       C       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       C         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       C      GC_BARE_DD      3 NA    PHAB_BACK
						    6396        PHAB       C    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       C GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       C     GC_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       C  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6396        PHAB       C        GC_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       C     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       C   HORIZ_DIST_DD    9.0 NA   PHAB_FRONT
						    6396        PHAB       C      U_NONWOODY      2 NA    PHAB_BACK
						    6396        PHAB       C   U_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       C         U_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       C      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       C      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       C   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       D      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       D   C_BIGTREES_DD      1 NA    PHAB_BACK
						    6396        PHAB       D    C_SMALLTREES      1 NA    PHAB_BACK
						    6396        PHAB       D C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       D          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       D       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       D         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       D      GC_BARE_DD      3 NA    PHAB_BACK
						    6396        PHAB       D    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       D GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       D     GC_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       D  GC_NONWOODY_DD      1 NA    PHAB_BACK
						    6396        PHAB       D        GC_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       D     GC_WOODY_DD      1 NA    PHAB_BACK
						    6396        PHAB       D   HORIZ_DIST_DD    5.0 NA   PHAB_FRONT
						    6396        PHAB       D      U_NONWOODY      0 NA    PHAB_BACK
						    6396        PHAB       D   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       D         U_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       D      U_WOODY_DD      1 NA    PHAB_BACK
						    6396        PHAB       D      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       D   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       E      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       E   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       E    C_SMALLTREES      2 NA    PHAB_BACK
						    6396        PHAB       E C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       E          CANOPY      M NA    PHAB_BACK
						    6396        PHAB       E       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       E         GC_BARE      2 NA    PHAB_BACK
						    6396        PHAB       E      GC_BARE_DD      3 NA    PHAB_BACK
						    6396        PHAB       E    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       E GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       E     GC_NONWOODY      1 NA    PHAB_BACK
						    6396        PHAB       E  GC_NONWOODY_DD      1 NA    PHAB_BACK
						    6396        PHAB       E        GC_WOODY      3 NA    PHAB_BACK
						    6396        PHAB       E     GC_WOODY_DD      1 NA    PHAB_BACK
						    6396        PHAB       E   HORIZ_DIST_DD  004.0 NA   PHAB_FRONT
						    6396        PHAB       E      U_NONWOODY      0 NA    PHAB_BACK
						    6396        PHAB       E   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       E         U_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       E      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       E      UNDERSTORY      M NA    PHAB_BACK
						    6396        PHAB       E   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       F      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       F   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       F    C_SMALLTREES      1 NA    PHAB_BACK
						    6396        PHAB       F C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       F          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       F       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       F         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       F      GC_BARE_DD      3 NA    PHAB_BACK
						    6396        PHAB       F    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       F GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       F     GC_NONWOODY      2 NA    PHAB_BACK
						    6396        PHAB       F  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       F        GC_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       F     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       F   HORIZ_DIST_DD  004.0 NA   PHAB_FRONT
						    6396        PHAB       F      U_NONWOODY      0 NA    PHAB_BACK
						    6396        PHAB       F   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       F         U_WOODY      1 NA    PHAB_BACK
						    6396        PHAB       F      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       F      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       F   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       G      C_BIGTREES      0 NA    PHAB_BACK
						    6396        PHAB       G   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       G    C_SMALLTREES      1 NA    PHAB_BACK
						    6396        PHAB       G C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       G          CANOPY      M NA    PHAB_BACK
						    6396        PHAB       G       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       G         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       G      GC_BARE_DD      1 NA    PHAB_BACK
						    6396        PHAB       G    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       G GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       G     GC_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       G  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6396        PHAB       G        GC_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       G     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       G   HORIZ_DIST_DD    9.0 NA   PHAB_FRONT
						    6396        PHAB       G      U_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       G   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       G         U_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       G      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       G      UNDERSTORY      M NA    PHAB_BACK
						    6396        PHAB       G   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       H      C_BIGTREES      2 NA    PHAB_BACK
						    6396        PHAB       H   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       H    C_SMALLTREES      0 NA    PHAB_BACK
						    6396        PHAB       H C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       H          CANOPY      M NA    PHAB_BACK
						    6396        PHAB       H       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       H         GC_BARE      2 NA    PHAB_BACK
						    6396        PHAB       H      GC_BARE_DD      1 NA    PHAB_BACK
						    6396        PHAB       H    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       H GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       H     GC_NONWOODY      0 NA    PHAB_BACK
						    6396        PHAB       H  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6396        PHAB       H        GC_WOODY      0 NA    PHAB_BACK
						    6396        PHAB       H     GC_WOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       H   HORIZ_DIST_DD    4.0 NA   PHAB_FRONT
						    6396        PHAB       H      U_NONWOODY      2 NA    PHAB_BACK
						    6396        PHAB       H   U_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       H         U_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       H      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       H      UNDERSTORY      M NA    PHAB_BACK
						    6396        PHAB       H   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       I      C_BIGTREES      1 NA    PHAB_BACK
						    6396        PHAB       I   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       I    C_SMALLTREES      2 NA    PHAB_BACK
						    6396        PHAB       I C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       I          CANOPY      M NA    PHAB_BACK
						    6396        PHAB       I       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       I         GC_BARE      0 NA    PHAB_BACK
						    6396        PHAB       I      GC_BARE_DD      0 NA    PHAB_BACK
						    6396        PHAB       I    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       I GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       I     GC_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       I  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6396        PHAB       I        GC_WOODY      3 NA    PHAB_BACK
						    6396        PHAB       I     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       I   HORIZ_DIST_DD    9.0 NA   PHAB_FRONT
						    6396        PHAB       I      U_NONWOODY      3 NA    PHAB_BACK
						    6396        PHAB       I   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       I         U_WOODY      2 NA    PHAB_BACK
						    6396        PHAB       I      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       I      UNDERSTORY      M NA    PHAB_BACK
						    6396        PHAB       I   UNDERSTORY_DD      D NA    PHAB_BACK
						    6396        PHAB       J      C_BIGTREES      1 NA    PHAB_BACK
						    6396        PHAB       J   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       J    C_SMALLTREES      2 NA    PHAB_BACK
						    6396        PHAB       J C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6396        PHAB       J          CANOPY      D NA    PHAB_BACK
						    6396        PHAB       J       CANOPY_DD      D NA    PHAB_BACK
						    6396        PHAB       J         GC_BARE      1 NA    PHAB_BACK
						    6396        PHAB       J      GC_BARE_DD      1 NA    PHAB_BACK
						    6396        PHAB       J    GC_INUNDATED      0 NA    PHAB_BACK
						    6396        PHAB       J GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6396        PHAB       J     GC_NONWOODY      1 NA    PHAB_BACK
						    6396        PHAB       J  GC_NONWOODY_DD      4 NA    PHAB_BACK
						    6396        PHAB       J        GC_WOODY      3 NA    PHAB_BACK
						    6396        PHAB       J     GC_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       J   HORIZ_DIST_DD   13.0 NA   PHAB_FRONT
						    6396        PHAB       J      U_NONWOODY      0 NA    PHAB_BACK
						    6396        PHAB       J   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       J         U_WOODY      3 NA    PHAB_BACK
						    6396        PHAB       J      U_WOODY_DD      0 NA    PHAB_BACK
						    6396        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						    6396        PHAB       J   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       A      C_BIGTREES      0 NA    PHAB_BACK
						    6449        PHAB       A   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       A    C_SMALLTREES      3 NA    PHAB_BACK
						    6449        PHAB       A C_SMALLTREES_DD      1 NA    PHAB_BACK
						    6449        PHAB       A          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       A       CANOPY_DD      D NA    PHAB_BACK
						    6449        PHAB       A         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       A      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       A    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       A GC_INUNDATED_DD      2 NA    PHAB_BACK
						    6449        PHAB       A     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       A  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       A        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       A     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       A   HORIZ_DIST_DD    0.9 NA   PHAB_FRONT
						    6449        PHAB       A      U_NONWOODY      2 NA    PHAB_BACK
						    6449        PHAB       A   U_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       A         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       A      U_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       A      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       A   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       B      C_BIGTREES      0 NA    PHAB_BACK
						    6449        PHAB       B   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       B    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       B C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       B         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       B      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       B    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       B GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       B     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       B  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       B        GC_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       B     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       B   HORIZ_DIST_DD    1.1 NA   PHAB_FRONT
						    6449        PHAB       B      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       B   U_NONWOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       B         U_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       B      U_WOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       B      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       C      C_BIGTREES      2 NA    PHAB_BACK
						    6449        PHAB       C   C_BIGTREES_DD      3 NA    PHAB_BACK
						    6449        PHAB       C    C_SMALLTREES      3 NA    PHAB_BACK
						    6449        PHAB       C C_SMALLTREES_DD      3 NA    PHAB_BACK
						    6449        PHAB       C          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       C       CANOPY_DD      D NA    PHAB_BACK
						    6449        PHAB       C         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       C      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       C    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       C GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       C     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       C  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       C        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       C     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       C   HORIZ_DIST_DD    0.2 NA   PHAB_FRONT
						    6449        PHAB       C      U_NONWOODY      2 NA    PHAB_BACK
						    6449        PHAB       C   U_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       C         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       C      U_WOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       C      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       C   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       D      C_BIGTREES      1 NA    PHAB_BACK
						    6449        PHAB       D   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       D    C_SMALLTREES      2 NA    PHAB_BACK
						    6449        PHAB       D C_SMALLTREES_DD      2 NA    PHAB_BACK
						    6449        PHAB       D          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       D       CANOPY_DD      D NA    PHAB_BACK
						    6449        PHAB       D         GC_BARE      0 NA    PHAB_BACK
						    6449        PHAB       D      GC_BARE_DD      2 NA    PHAB_BACK
						    6449        PHAB       D    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       D GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       D     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       D  GC_NONWOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       D        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       D     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       D   HORIZ_DIST_DD    0.6 NA   PHAB_FRONT
						    6449        PHAB       D      U_NONWOODY      2 NA    PHAB_BACK
						    6449        PHAB       D   U_NONWOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       D         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       D      U_WOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       D      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       D   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       E      C_BIGTREES      3 NA    PHAB_BACK
						    6449        PHAB       E   C_BIGTREES_DD      3 NA    PHAB_BACK
						    6449        PHAB       E    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       E C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       E          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       E       CANOPY_DD      D NA    PHAB_BACK
						    6449        PHAB       E         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       E      GC_BARE_DD      3 NA    PHAB_BACK
						    6449        PHAB       E    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       E GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       E     GC_NONWOODY      4 NA    PHAB_BACK
						    6449        PHAB       E  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       E        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       E     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       E   HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
						    6449        PHAB       E      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       E   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       E         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       E      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       E      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       F      C_BIGTREES      2 NA    PHAB_BACK
						    6449        PHAB       F   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       F    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       F C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       F          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       F         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       F      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       F    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       F GC_INUNDATED_DD      1 NA    PHAB_BACK
						    6449        PHAB       F     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       F  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       F        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       F     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       F   HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
						    6449        PHAB       F      U_NONWOODY      1 NA    PHAB_BACK
						    6449        PHAB       F   U_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       F         U_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       F      U_WOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       F      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       F   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       G      C_BIGTREES      2 NA    PHAB_BACK
						    6449        PHAB       G   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       G    C_SMALLTREES      1 NA    PHAB_BACK
						    6449        PHAB       G C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       G          CANOPY      M NA    PHAB_BACK
						    6449        PHAB       G         GC_BARE      2   F1  PHAB_BACK
						    6449        PHAB       G      GC_BARE_DD      2 NA    PHAB_BACK
						    6449        PHAB       G    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       G GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       G     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       G  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       G        GC_WOODY      3 NA    PHAB_BACK
						    6449        PHAB       G     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       G   HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
						    6449        PHAB       G      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       G   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       G         U_WOODY      3 NA    PHAB_BACK
						    6449        PHAB       G      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       G      UNDERSTORY      M NA    PHAB_BACK
						    6449        PHAB       H      C_BIGTREES      0 NA    PHAB_BACK
						    6449        PHAB       H   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       H    C_SMALLTREES      1 NA    PHAB_BACK
						    6449        PHAB       H C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       H          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       H         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       H      GC_BARE_DD      2 NA    PHAB_BACK
						    6449        PHAB       H    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       H GC_INUNDATED_DD      1 NA    PHAB_BACK
						    6449        PHAB       H     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       H  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       H        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       H     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       H   HORIZ_DIST_DD    0.2 NA   PHAB_FRONT
						    6449        PHAB       H      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       H   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       H         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       H      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       H      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       I      C_BIGTREES      0 NA    PHAB_BACK
						    6449        PHAB       I   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       I    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       I C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       I         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       I      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       I    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       I GC_INUNDATED_DD      1 NA    PHAB_BACK
						    6449        PHAB       I     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       I  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6449        PHAB       I        GC_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       I     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       I   HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
						    6449        PHAB       I      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       I   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       I         U_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       I      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       I      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       J      C_BIGTREES      0 NA    PHAB_BACK
						    6449        PHAB       J   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       J    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       J C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       J         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       J      GC_BARE_DD      3 NA    PHAB_BACK
						    6449        PHAB       J    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       J GC_INUNDATED_DD      1 NA    PHAB_BACK
						    6449        PHAB       J     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       J  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       J        GC_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       J     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       J   HORIZ_DIST_DD    0.5 NA   PHAB_FRONT
						    6449        PHAB       J      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       J   U_NONWOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       J         U_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       J      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       J   UNDERSTORY_DD      D NA    PHAB_BACK
						    6449        PHAB       K      C_BIGTREES      2 NA    PHAB_BACK
						    6449        PHAB       K   C_BIGTREES_DD      1 NA    PHAB_BACK
						    6449        PHAB       K    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       K C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       K          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       K       CANOPY_DD      D NA    PHAB_BACK
						    6449        PHAB       K         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       K      GC_BARE_DD      1 NA    PHAB_BACK
						    6449        PHAB       K    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       K GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       K     GC_NONWOODY      4 NA    PHAB_BACK
						    6449        PHAB       K  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       K        GC_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       K     GC_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       K   HORIZ_DIST_DD    0.1 NA   PHAB_FRONT
						    6449        PHAB       K      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       K   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       K         U_WOODY      2 NA    PHAB_BACK
						    6449        PHAB       K      U_WOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       K      UNDERSTORY      D NA    PHAB_BACK
						    6449        PHAB       L      C_BIGTREES      2 NA    PHAB_BACK
						    6449        PHAB       L   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       L    C_SMALLTREES      0 NA    PHAB_BACK
						    6449        PHAB       L C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6449        PHAB       L          CANOPY      D NA    PHAB_BACK
						    6449        PHAB       L         GC_BARE      1 NA    PHAB_BACK
						    6449        PHAB       L      GC_BARE_DD      2 NA    PHAB_BACK
						    6449        PHAB       L    GC_INUNDATED      0 NA    PHAB_BACK
						    6449        PHAB       L GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6449        PHAB       L     GC_NONWOODY      3 NA    PHAB_BACK
						    6449        PHAB       L  GC_NONWOODY_DD      2 NA    PHAB_BACK
						    6449        PHAB       L        GC_WOODY      0 NA    PHAB_BACK
						    6449        PHAB       L     GC_WOODY_DD      1 NA    PHAB_BACK
						    6449        PHAB       L   HORIZ_DIST_DD    0.3 NA   PHAB_FRONT
						    6449        PHAB       L      U_NONWOODY      0 NA    PHAB_BACK
						    6449        PHAB       L   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6449        PHAB       L         U_WOODY      1 NA    PHAB_BACK
						    6449        PHAB       L      U_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       A      C_BIGTREES      0 NA    PHAB_BACK
						    6518        PHAB       A   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       A    C_SMALLTREES      0 NA    PHAB_BACK
						    6518        PHAB       A C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       A          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       A       CANOPY_DD      D NA    PHAB_BACK
						    6518        PHAB       A         GC_BARE      0 NA    PHAB_BACK
						    6518        PHAB       A      GC_BARE_DD      0 NA    PHAB_BACK
						    6518        PHAB       A    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       A GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6518        PHAB       A     GC_NONWOODY      4 NA    PHAB_BACK
						    6518        PHAB       A  GC_NONWOODY_DD      4 NA    PHAB_BACK
						    6518        PHAB       A        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       A     GC_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       A   HORIZ_DIST_DD    1.0 NA   PHAB_FRONT
						    6518        PHAB       A      U_NONWOODY      0 NA    PHAB_BACK
						    6518        PHAB       A   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       A         U_WOODY      0 NA    PHAB_BACK
						    6518        PHAB       A      U_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       A      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       A   UNDERSTORY_DD      D NA    PHAB_BACK
						    6518        PHAB       B      C_BIGTREES      1 NA    PHAB_BACK
						    6518        PHAB       B   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       B    C_SMALLTREES      1 NA    PHAB_BACK
						    6518        PHAB       B C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       B          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       B         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       B      GC_BARE_DD      1 NA    PHAB_BACK
						    6518        PHAB       B    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       B GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6518        PHAB       B     GC_NONWOODY      3 NA    PHAB_BACK
						    6518        PHAB       B  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6518        PHAB       B        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       B     GC_WOODY_DD      1 NA    PHAB_BACK
						    6518        PHAB       B   HORIZ_DIST_DD    1.0 NA   PHAB_FRONT
						    6518        PHAB       B      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       B   U_NONWOODY_DD      1 NA    PHAB_BACK
						    6518        PHAB       B         U_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       B      U_WOODY_DD      2 NA    PHAB_BACK
						    6518        PHAB       B      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       B   UNDERSTORY_DD      D NA    PHAB_BACK
						    6518        PHAB       C      C_BIGTREES      1 NA    PHAB_BACK
						    6518        PHAB       C    C_SMALLTREES      2 NA    PHAB_BACK
						    6518        PHAB       C          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       C         GC_BARE      2 NA    PHAB_BACK
						    6518        PHAB       C    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       C     GC_NONWOODY      3 NA    PHAB_BACK
						    6518        PHAB       C        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       C      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       C         U_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       C      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       D      C_BIGTREES      3 NA    PHAB_BACK
						    6518        PHAB       D    C_SMALLTREES      3 NA    PHAB_BACK
						    6518        PHAB       D          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       D         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       D    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       D     GC_NONWOODY      2 NA    PHAB_BACK
						    6518        PHAB       D        GC_WOODY      3 NA    PHAB_BACK
						    6518        PHAB       D      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       D         U_WOODY      3 NA    PHAB_BACK
						    6518        PHAB       D      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       E      C_BIGTREES      0 NA    PHAB_BACK
						    6518        PHAB       E    C_SMALLTREES      2 NA    PHAB_BACK
						    6518        PHAB       E          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       E         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       E    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       E     GC_NONWOODY      2 NA    PHAB_BACK
						    6518        PHAB       E        GC_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       E      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       E         U_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       E      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       F      C_BIGTREES      2 NA    PHAB_BACK
						    6518        PHAB       F    C_SMALLTREES      2 NA    PHAB_BACK
						    6518        PHAB       F          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       F         GC_BARE      2 NA    PHAB_BACK
						    6518        PHAB       F    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       F     GC_NONWOODY      2 NA    PHAB_BACK
						    6518        PHAB       F        GC_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       F      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       F         U_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       F      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       G      C_BIGTREES      2 NA    PHAB_BACK
						    6518        PHAB       G    C_SMALLTREES      2 NA    PHAB_BACK
						    6518        PHAB       G          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       G         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       G    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       G     GC_NONWOODY      3 NA    PHAB_BACK
						    6518        PHAB       G        GC_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       G      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       G         U_WOODY      2 NA    PHAB_BACK
						    6518        PHAB       G      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       H      C_BIGTREES      0 NA    PHAB_BACK
						    6518        PHAB       H   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       H    C_SMALLTREES      0 NA    PHAB_BACK
						    6518        PHAB       H C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       H          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       H         GC_BARE      2 NA    PHAB_BACK
						    6518        PHAB       H      GC_BARE_DD      2 NA    PHAB_BACK
						    6518        PHAB       H    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       H GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6518        PHAB       H     GC_NONWOODY      3 NA    PHAB_BACK
						    6518        PHAB       H  GC_NONWOODY_DD      3 NA    PHAB_BACK
						    6518        PHAB       H        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       H     GC_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       H   HORIZ_DIST_DD    2.0 NA   PHAB_FRONT
						    6518        PHAB       H      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       H   U_NONWOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       H         U_WOODY      0 NA    PHAB_BACK
						    6518        PHAB       H      U_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       H      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       I      C_BIGTREES      0 NA    PHAB_BACK
						    6518        PHAB       I    C_SMALLTREES      0 NA    PHAB_BACK
						    6518        PHAB       I          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       I         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       I    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       I     GC_NONWOODY      4 NA    PHAB_BACK
						    6518        PHAB       I        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       I      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       I         U_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       I      UNDERSTORY      D NA    PHAB_BACK
						    6518        PHAB       J      C_BIGTREES      0 NA    PHAB_BACK
						    6518        PHAB       J   C_BIGTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       J    C_SMALLTREES      0 NA    PHAB_BACK
						    6518        PHAB       J C_SMALLTREES_DD      0 NA    PHAB_BACK
						    6518        PHAB       J          CANOPY      D NA    PHAB_BACK
						    6518        PHAB       J         GC_BARE      1 NA    PHAB_BACK
						    6518        PHAB       J      GC_BARE_DD      1 NA    PHAB_BACK
						    6518        PHAB       J    GC_INUNDATED      0 NA    PHAB_BACK
						    6518        PHAB       J GC_INUNDATED_DD      0 NA    PHAB_BACK
						    6518        PHAB       J     GC_NONWOODY      4 NA    PHAB_BACK
						    6518        PHAB       J  GC_NONWOODY_DD      4 NA    PHAB_BACK
						    6518        PHAB       J        GC_WOODY      1 NA    PHAB_BACK
						    6518        PHAB       J     GC_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       J   HORIZ_DIST_DD    3.0 NA   PHAB_FRONT
						    6518        PHAB       J      U_NONWOODY      1 NA    PHAB_BACK
						    6518        PHAB       J   U_NONWOODY_DD      1 NA    PHAB_BACK
						    6518        PHAB       J         U_WOODY      0 NA    PHAB_BACK
						    6518        PHAB       J      U_WOODY_DD      0 NA    PHAB_BACK
						    6518        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       A      C_BIGTREES      1 NA    PHAB_BACK
						    6530        PHAB       A    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       A          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       A         GC_BARE      0 NA    PHAB_BACK
						    6530        PHAB       A    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       A     GC_NONWOODY      4 NA    PHAB_BACK
						    6530        PHAB       A        GC_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       A      U_NONWOODY      2 NA    PHAB_BACK
						    6530        PHAB       A         U_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       A      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       B      C_BIGTREES      1 NA    PHAB_BACK
						    6530        PHAB       B    C_SMALLTREES      1 NA    PHAB_BACK
						    6530        PHAB       B          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       B         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       B    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       B     GC_NONWOODY      1 NA    PHAB_BACK
						    6530        PHAB       B        GC_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       B      U_NONWOODY      1 NA    PHAB_BACK
						    6530        PHAB       B         U_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       B      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       C      C_BIGTREES      2 NA    PHAB_BACK
						    6530        PHAB       C    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       C          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       C         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       C    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       C     GC_NONWOODY      3 NA    PHAB_BACK
						    6530        PHAB       C        GC_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       C      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       C         U_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       C      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       D      C_BIGTREES      1 NA    PHAB_BACK
						    6530        PHAB       D    C_SMALLTREES      1 NA    PHAB_BACK
						    6530        PHAB       D          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       D         GC_BARE      1 NA    PHAB_BACK
						    6530        PHAB       D    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       D     GC_NONWOODY      3 NA    PHAB_BACK
						    6530        PHAB       D        GC_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       D      U_NONWOODY      1 NA    PHAB_BACK
						    6530        PHAB       D         U_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       D      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       E      C_BIGTREES      1 NA    PHAB_BACK
						    6530        PHAB       E    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       E          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       E         GC_BARE      1 NA    PHAB_BACK
						    6530        PHAB       E    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       E     GC_NONWOODY      2 NA    PHAB_BACK
						    6530        PHAB       E        GC_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       E      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       E         U_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       F      C_BIGTREES      1 NA    PHAB_BACK
						    6530        PHAB       F    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       F          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       F         GC_BARE      0 NA    PHAB_BACK
						    6530        PHAB       F    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       F     GC_NONWOODY      3 NA    PHAB_BACK
						    6530        PHAB       F        GC_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       F      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       F         U_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       F      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       G      C_BIGTREES      0 NA    PHAB_BACK
						    6530        PHAB       G    C_SMALLTREES      1 NA    PHAB_BACK
						    6530        PHAB       G          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       G         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       G    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       G     GC_NONWOODY      3 NA    PHAB_BACK
						    6530        PHAB       G        GC_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       G      U_NONWOODY      1 NA    PHAB_BACK
						    6530        PHAB       G         U_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       G      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       H      C_BIGTREES      2 NA    PHAB_BACK
						    6530        PHAB       H    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       H          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       H         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       H    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       H     GC_NONWOODY      2 NA    PHAB_BACK
						    6530        PHAB       H        GC_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       H      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       H         U_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       H      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       I      C_BIGTREES      2 NA    PHAB_BACK
						    6530        PHAB       I    C_SMALLTREES      0 NA    PHAB_BACK
						    6530        PHAB       I          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       I         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       I    GC_INUNDATED      0 NA    PHAB_BACK
						    6530        PHAB       I     GC_NONWOODY      2 NA    PHAB_BACK
						    6530        PHAB       I        GC_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       I      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       I         U_WOODY      0 NA    PHAB_BACK
						    6530        PHAB       I      UNDERSTORY      D NA    PHAB_BACK
						    6530        PHAB       J      C_BIGTREES      2 NA    PHAB_BACK
						    6530        PHAB       J    C_SMALLTREES      1 NA    PHAB_BACK
						    6530        PHAB       J          CANOPY      D NA    PHAB_BACK
						    6530        PHAB       J         GC_BARE      2 NA    PHAB_BACK
						    6530        PHAB       J    GC_INUNDATED      1 NA    PHAB_BACK
						    6530        PHAB       J     GC_NONWOODY      3 NA    PHAB_BACK
						    6530        PHAB       J        GC_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       J      U_NONWOODY      0 NA    PHAB_BACK
						    6530        PHAB       J         U_WOODY      1 NA    PHAB_BACK
						    6530        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       A      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       A   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       A    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       A C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       A         GC_BARE      1 NA    PHAB_BACK
						   6618        PHAB       A      GC_BARE_DD      0 NA    PHAB_BACK
						   6618        PHAB       A    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       A GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       A     GC_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       A  GC_NONWOODY_DD      3 NA    PHAB_BACK
						   6618        PHAB       A        GC_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       A     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       A   HORIZ_DIST_DD    5.0 NA   PHAB_FRONT
						   6618        PHAB       A      U_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       A   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       A         U_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       A      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       A      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       B      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       B   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       B    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       B C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       B         GC_BARE      1 NA    PHAB_BACK
						   6618        PHAB       B      GC_BARE_DD      2 NA    PHAB_BACK
						   6618        PHAB       B    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       B GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       B     GC_NONWOODY      4 NA    PHAB_BACK
						   6618        PHAB       B  GC_NONWOODY_DD      2 NA    PHAB_BACK
						   6618        PHAB       B        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       B     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       B   HORIZ_DIST_DD    6.5 NA   PHAB_FRONT
						   6618        PHAB       B      U_NONWOODY      0 NA    PHAB_BACK
						   6618        PHAB       B   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       B         U_WOODY      0 NA    PHAB_BACK
						   6618        PHAB       B      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       C      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       C   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       C    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       C C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       C         GC_BARE      1 NA    PHAB_BACK
						   6618        PHAB       C      GC_BARE_DD      1 NA    PHAB_BACK
						   6618        PHAB       C    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       C GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       C     GC_NONWOODY      1 NA    PHAB_BACK
						   6618        PHAB       C  GC_NONWOODY_DD      1 NA    PHAB_BACK
						   6618        PHAB       C        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       C     GC_WOODY_DD      1 NA    PHAB_BACK
						   6618        PHAB       C   HORIZ_DIST_DD    2.0 NA   PHAB_FRONT
						   6618        PHAB       C      U_NONWOODY      3 NA    PHAB_BACK
						   6618        PHAB       C   U_NONWOODY_DD      3 NA    PHAB_BACK
						   6618        PHAB       C         U_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       C      U_WOODY_DD      3 NA    PHAB_BACK
						   6618        PHAB       C      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       D      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       D   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       D    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       D C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       D         GC_BARE      0 NA    PHAB_BACK
						   6618        PHAB       D      GC_BARE_DD      0 NA    PHAB_BACK
						   6618        PHAB       D    GC_INUNDATED      1 NA    PHAB_BACK
						   6618        PHAB       D GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       D     GC_NONWOODY      3 NA    PHAB_BACK
						   6618        PHAB       D  GC_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       D        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       D     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       D   HORIZ_DIST_DD    1.0 NA   PHAB_FRONT
						   6618        PHAB       D      U_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       D   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       D         U_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       D      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       D      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       E      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       E   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       E    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       E C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       E         GC_BARE      3 NA    PHAB_BACK
						   6618        PHAB       E      GC_BARE_DD      3 NA    PHAB_BACK
						   6618        PHAB       E    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       E GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       E     GC_NONWOODY      0 NA    PHAB_BACK
						   6618        PHAB       E  GC_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       E        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       E     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       E   HORIZ_DIST_DD    0.5 NA   PHAB_FRONT
						   6618        PHAB       E      U_NONWOODY      0 NA    PHAB_BACK
						   6618        PHAB       E   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       E         U_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       E      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       F      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       F   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       F    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       F C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       F         GC_BARE      2 NA    PHAB_BACK
						   6618        PHAB       F      GC_BARE_DD      3 NA    PHAB_BACK
						   6618        PHAB       F    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       F GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       F     GC_NONWOODY      1 NA    PHAB_BACK
						   6618        PHAB       F  GC_NONWOODY_DD      1 NA    PHAB_BACK
						   6618        PHAB       F        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       F     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       F   HORIZ_DIST_DD    1.5 NA   PHAB_FRONT
						   6618        PHAB       F      U_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       F   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       F         U_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       F      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       F      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       G      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       G   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       G    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       G C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       G         GC_BARE      1 NA    PHAB_BACK
						   6618        PHAB       G      GC_BARE_DD      0 NA    PHAB_BACK
						   6618        PHAB       G    GC_INUNDATED      1 NA    PHAB_BACK
						   6618        PHAB       G GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       G     GC_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       G  GC_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       G        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       G     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       G      U_NONWOODY      1 NA    PHAB_BACK
						   6618        PHAB       G   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       G         U_WOODY      3 NA    PHAB_BACK
						   6618        PHAB       G      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       G      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       H      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       H   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       H    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       H C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       H         GC_BARE      2 NA    PHAB_BACK
						   6618        PHAB       H      GC_BARE_DD      2 NA    PHAB_BACK
						   6618        PHAB       H    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       H GC_INUNDATED_DD      1 NA    PHAB_BACK
						   6618        PHAB       H     GC_NONWOODY      2 NA    PHAB_BACK
						   6618        PHAB       H  GC_NONWOODY_DD      2 NA    PHAB_BACK
						   6618        PHAB       H        GC_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       H     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       H   HORIZ_DIST_DD    1.5 NA   PHAB_FRONT
						   6618        PHAB       H      U_NONWOODY      0 NA    PHAB_BACK
						   6618        PHAB       H   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       H         U_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       H      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       H      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       I      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       I   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       I    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       I C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       I         GC_BARE      2 NA    PHAB_BACK
						   6618        PHAB       I      GC_BARE_DD      1 NA    PHAB_BACK
						   6618        PHAB       I    GC_INUNDATED      0 NA    PHAB_BACK
						   6618        PHAB       I GC_INUNDATED_DD      0 NA    PHAB_BACK
						   6618        PHAB       I     GC_NONWOODY      1 NA    PHAB_BACK
						   6618        PHAB       I  GC_NONWOODY_DD      1 NA    PHAB_BACK
						   6618        PHAB       I        GC_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       I     GC_WOODY_DD      2 NA    PHAB_BACK
						   6618        PHAB       I   HORIZ_DIST_DD    1.0 NA   PHAB_FRONT
						   6618        PHAB       I      U_NONWOODY      0 NA    PHAB_BACK
						   6618        PHAB       I   U_NONWOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       I         U_WOODY      2 NA    PHAB_BACK
						   6618        PHAB       I      U_WOODY_DD      2 NA    PHAB_BACK
						   6618        PHAB       I      UNDERSTORY      D NA    PHAB_BACK
						   6618        PHAB       J      C_BIGTREES      0 NA    PHAB_BACK
						   6618        PHAB       J   C_BIGTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       J    C_SMALLTREES      0 NA    PHAB_BACK
						   6618        PHAB       J C_SMALLTREES_DD      0 NA    PHAB_BACK
						   6618        PHAB       J         GC_BARE      1 NA    PHAB_BACK
						   6618        PHAB       J      GC_BARE_DD      0 NA    PHAB_BACK
						   6618        PHAB       J    GC_INUNDATED      1 NA    PHAB_BACK
						   6618        PHAB       J GC_INUNDATED_DD      2 NA    PHAB_BACK
						   6618        PHAB       J     GC_NONWOODY      3 NA    PHAB_BACK
						   6618        PHAB       J  GC_NONWOODY_DD      2 NA    PHAB_BACK
						   6618        PHAB       J        GC_WOODY      0 NA    PHAB_BACK
						   6618        PHAB       J     GC_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       J   HORIZ_DIST_DD    2.0 NA   PHAB_FRONT
						   6618        PHAB       J      U_NONWOODY      3 NA    PHAB_BACK
						   6618        PHAB       J   U_NONWOODY_DD      3 NA    PHAB_BACK
						   6618        PHAB       J         U_WOODY      1 NA    PHAB_BACK
						   6618        PHAB       J      U_WOODY_DD      0 NA    PHAB_BACK
						   6618        PHAB       J      UNDERSTORY      D NA    PHAB_BACK
						   6647        PHAB       A      C_BIGTREES      0 ''       eForms
						   6647        PHAB       A   C_BIGTREES_DD      0 ''       eForms
						   6647        PHAB       A    C_SMALLTREES      0 ''       eForms
						   6647        PHAB       A C_SMALLTREES_DD      0 ''       eForms
						   6647        PHAB       A          CANOPY      N ''       eForms
						   6647        PHAB       A       CANOPY_DD      N ''       eForms
						   6647        PHAB       A         GC_BARE      2 ''       eForms
						   6647        PHAB       A      GC_BARE_DD      4 ''       eForms
						   6647        PHAB       A    GC_INUNDATED      0 ''       eForms
						   6647        PHAB       A GC_INUNDATED_DD      0 ''       eForms
						   6647        PHAB       A     GC_NONWOODY      2 ''       eForms
						   6647        PHAB       A  GC_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       A        GC_WOODY      1 ''       eForms
						   6647        PHAB       A     GC_WOODY_DD      0 ''       eForms
						   6647        PHAB       A   HORIZ_DIST_DD    3.0 ''       eForms
						   6647        PHAB       A      U_NONWOODY      2 ''       eForms
						   6647        PHAB       A   U_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       A         U_WOODY      1 ''       eForms
						   6647        PHAB       A      U_WOODY_DD      0 ''       eForms
						   6647        PHAB       A      UNDERSTORY      M ''       eForms
						   6647        PHAB       A   UNDERSTORY_DD      N ''       eForms
						   6647        PHAB       B      C_BIGTREES      2 ''       eForms
						   6647        PHAB       B    C_SMALLTREES      2 ''       eForms
						   6647        PHAB       B          CANOPY      M ''       eForms
						   6647        PHAB       B         GC_BARE      1 ''       eForms
						   6647        PHAB       B    GC_INUNDATED      1 ''       eForms
						   6647        PHAB       B     GC_NONWOODY      4 ''       eForms
						   6647        PHAB       B        GC_WOODY      0 ''       eForms
						   6647        PHAB       B      U_NONWOODY      2 ''       eForms
						   6647        PHAB       B         U_WOODY      2 ''       eForms
						   6647        PHAB       B      UNDERSTORY      M ''       eForms
						   6647        PHAB       C      C_BIGTREES      1 ''       eForms
						   6647        PHAB       C   C_BIGTREES_DD      1 ''       eForms
						   6647        PHAB       C    C_SMALLTREES      2 ''       eForms
						   6647        PHAB       C C_SMALLTREES_DD      1 ''       eForms
						   6647        PHAB       C          CANOPY      D ''       eForms
						   6647        PHAB       C       CANOPY_DD      D ''       eForms
						   6647        PHAB       C         GC_BARE      2 ''       eForms
						   6647        PHAB       C      GC_BARE_DD      3 ''       eForms
						   6647        PHAB       C    GC_INUNDATED      0 ''       eForms
						   6647        PHAB       C GC_INUNDATED_DD      0 ''       eForms
						   6647        PHAB       C     GC_NONWOODY      3 ''       eForms
						   6647        PHAB       C  GC_NONWOODY_DD      2 ''       eForms
						   6647        PHAB       C        GC_WOODY      1 ''       eForms
						   6647        PHAB       C     GC_WOODY_DD      1 ''       eForms
						   6647        PHAB       C   HORIZ_DIST_DD    5.0 ''       eForms
						   6647        PHAB       C      U_NONWOODY      1 ''       eForms
						   6647        PHAB       C   U_NONWOODY_DD      0 ''       eForms
						   6647        PHAB       C         U_WOODY      2 ''       eForms
						   6647        PHAB       C      U_WOODY_DD      1 ''       eForms
						   6647        PHAB       C      UNDERSTORY      D ''       eForms
						   6647        PHAB       C   UNDERSTORY_DD      D ''       eForms
						   6647        PHAB       D      C_BIGTREES      1 ''       eForms
						   6647        PHAB       D   C_BIGTREES_DD      0 ''       eForms
						   6647        PHAB       D    C_SMALLTREES      3 ''       eForms
						   6647        PHAB       D C_SMALLTREES_DD      0 ''       eForms
						   6647        PHAB       D          CANOPY      D ''       eForms
						   6647        PHAB       D       CANOPY_DD      N ''       eForms
						   6647        PHAB       D         GC_BARE      2 ''       eForms
						   6647        PHAB       D      GC_BARE_DD      3 ''       eForms
						   6647        PHAB       D    GC_INUNDATED      0 ''       eForms
						   6647        PHAB       D GC_INUNDATED_DD      0 ''       eForms
						   6647        PHAB       D     GC_NONWOODY      3 ''       eForms
						   6647        PHAB       D  GC_NONWOODY_DD      2 ''       eForms
						   6647        PHAB       D        GC_WOODY      1 ''       eForms
						   6647        PHAB       D     GC_WOODY_DD      1 ''       eForms
						   6647        PHAB       D   HORIZ_DIST_DD     28 ''       eForms
						   6647        PHAB       D      U_NONWOODY      1 ''       eForms
						   6647        PHAB       D   U_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       D         U_WOODY      3 ''       eForms
						   6647        PHAB       D      U_WOODY_DD      1 ''       eForms
						   6647        PHAB       D      UNDERSTORY      M ''       eForms
						   6647        PHAB       D   UNDERSTORY_DD      D ''       eForms
						   6647        PHAB       E      C_BIGTREES      0 ''       eForms
						   6647        PHAB       E   C_BIGTREES_DD      0 ''       eForms
						   6647        PHAB       E    C_SMALLTREES      3 ''       eForms
						   6647        PHAB       E C_SMALLTREES_DD      1 ''       eForms
						   6647        PHAB       E          CANOPY      D ''       eForms
						   6647        PHAB       E       CANOPY_DD      D ''       eForms
						   6647        PHAB       E         GC_BARE      3 ''       eForms
						   6647        PHAB       E      GC_BARE_DD      2 ''       eForms
						   6647        PHAB       E    GC_INUNDATED      0 ''       eForms
						   6647        PHAB       E GC_INUNDATED_DD      1 ''       eForms
						   6647        PHAB       E     GC_NONWOODY      1 ''       eForms
						   6647        PHAB       E  GC_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       E        GC_WOODY      2 ''       eForms
						   6647        PHAB       E     GC_WOODY_DD      2 ''       eForms
						   6647        PHAB       E   HORIZ_DIST_DD    1.5 ''       eForms
						   6647        PHAB       E      U_NONWOODY      1 ''       eForms
						   6647        PHAB       E   U_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       E         U_WOODY      2 ''       eForms
						   6647        PHAB       E      U_WOODY_DD      2 ''       eForms
						   6647        PHAB       E      UNDERSTORY      D ''       eForms
						   6647        PHAB       E   UNDERSTORY_DD      D ''       eForms
						   6647        PHAB       F      C_BIGTREES      0 ''       eForms
						   6647        PHAB       F    C_SMALLTREES      0 ''       eForms
						   6647        PHAB       F          CANOPY      N ''       eForms
						   6647        PHAB       F         GC_BARE      1 ''       eForms
						   6647        PHAB       F    GC_INUNDATED      1 ''       eForms
						   6647        PHAB       F     GC_NONWOODY      2 ''       eForms
						   6647        PHAB       F        GC_WOODY      1 ''       eForms
						   6647        PHAB       F      U_NONWOODY      2 ''       eForms
						   6647        PHAB       F         U_WOODY      2 ''       eForms
						   6647        PHAB       F      UNDERSTORY      D ''       eForms
						   6647        PHAB       G      C_BIGTREES      0 ''       eForms
						   6647        PHAB       G    C_SMALLTREES      1 ''       eForms
						   6647        PHAB       G          CANOPY      D ''       eForms
						   6647        PHAB       G         GC_BARE      0 ''       eForms
						   6647        PHAB       G    GC_INUNDATED      2 ''       eForms
						   6647        PHAB       G     GC_NONWOODY      3 ''       eForms
						   6647        PHAB       G        GC_WOODY      1 ''       eForms
						   6647        PHAB       G      U_NONWOODY      2 ''       eForms
						   6647        PHAB       G         U_WOODY      1 ''       eForms
						   6647        PHAB       G      UNDERSTORY      D ''       eForms
						   6647        PHAB       H      C_BIGTREES      0   F1     eForms
						   6647        PHAB       H    C_SMALLTREES      0   F1     eForms
						   6647        PHAB       H          CANOPY      N ''       eForms
						   6647        PHAB       H         GC_BARE      0 ''       eForms
						   6647        PHAB       H    GC_INUNDATED      4 ''       eForms
						   6647        PHAB       H     GC_NONWOODY      0 ''       eForms
						   6647        PHAB       H        GC_WOODY      0 ''       eForms
						   6647        PHAB       H      U_NONWOODY      0 ''       eForms
						   6647        PHAB       H         U_WOODY      0 ''       eForms
						   6647        PHAB       H      UNDERSTORY      N ''       eForms
						   6647        PHAB       I      C_BIGTREES      0   F1     eForms
						   6647        PHAB       I    C_SMALLTREES      0   F1     eForms
						   6647        PHAB       I          CANOPY      N ''       eForms
						   6647        PHAB       I         GC_BARE      0 ''       eForms
						   6647        PHAB       I    GC_INUNDATED      4   F1     eForms
						   6647        PHAB       I     GC_NONWOODY      0 ''       eForms
						   6647        PHAB       I        GC_WOODY      0 ''       eForms
						   6647        PHAB       I      U_NONWOODY      0 ''       eForms
						   6647        PHAB       I         U_WOODY      0 ''       eForms
						   6647        PHAB       I      UNDERSTORY      N ''       eForms
						   6647        PHAB       J      C_BIGTREES      0 ''       eForms
						   6647        PHAB       J   C_BIGTREES_DD      0 ''       eForms
						   6647        PHAB       J    C_SMALLTREES      0 ''       eForms
						   6647        PHAB       J C_SMALLTREES_DD      0 ''       eForms
						   6647        PHAB       J          CANOPY      N ''       eForms
						   6647        PHAB       J       CANOPY_DD      N ''       eForms
						   6647        PHAB       J         GC_BARE      0 ''       eForms
						   6647        PHAB       J      GC_BARE_DD      2 ''       eForms
						   6647        PHAB       J    GC_INUNDATED      0 ''       eForms
						   6647        PHAB       J GC_INUNDATED_DD      0 ''       eForms
						   6647        PHAB       J     GC_NONWOODY      4 ''       eForms
						   6647        PHAB       J  GC_NONWOODY_DD      3 ''       eForms
						   6647        PHAB       J        GC_WOODY      1 ''       eForms
						   6647        PHAB       J     GC_WOODY_DD      1 ''       eForms
						   6647        PHAB       J   HORIZ_DIST_DD    2.5 ''       eForms
						   6647        PHAB       J      U_NONWOODY      3 ''       eForms
						   6647        PHAB       J   U_NONWOODY_DD      1 ''       eForms
						   6647        PHAB       J         U_WOODY      1 ''       eForms
						   6647        PHAB       J      U_WOODY_DD      1 ''       eForms
						   6647        PHAB       J      UNDERSTORY      D ''       eForms
						   6647        PHAB       J   UNDERSTORY_DD      D ''       eForms
						   7431        PHAB       A      C_BIGTREES      0 ''       eForms
						   7431        PHAB       A    C_SMALLTREES      1 ''       eForms
						   7431        PHAB       A          CANOPY      M ''       eForms
						   7431        PHAB       A         GC_BARE      0 ''       eForms
						   7431        PHAB       A    GC_INUNDATED      2 ''       eForms
						   7431        PHAB       A     GC_NONWOODY      1 ''       eForms
						   7431        PHAB       A        GC_WOODY      1 ''       eForms
						   7431        PHAB       A      U_NONWOODY      1 ''       eForms
						   7431        PHAB       A         U_WOODY      0 ''       eForms
						   7431        PHAB       A      UNDERSTORY      D ''       eForms
						   7431        PHAB       B      C_BIGTREES      1 ''       eForms
						   7431        PHAB       B    C_SMALLTREES      3 ''       eForms
						   7431        PHAB       B          CANOPY      M ''       eForms
						   7431        PHAB       B         GC_BARE      2 ''       eForms
						   7431        PHAB       B    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       B     GC_NONWOODY      1 ''       eForms
						   7431        PHAB       B        GC_WOODY      2 ''       eForms
						   7431        PHAB       B      U_NONWOODY      0 ''       eForms
						   7431        PHAB       B         U_WOODY      2 ''       eForms
						   7431        PHAB       B      UNDERSTORY      M ''       eForms
						   7431        PHAB       D      C_BIGTREES      2 ''       eForms
						   7431        PHAB       D    C_SMALLTREES      2 ''       eForms
						   7431        PHAB       D          CANOPY      M ''       eForms
						   7431        PHAB       D         GC_BARE      2 ''       eForms
						   7431        PHAB       D    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       D     GC_NONWOODY      2 ''       eForms
						   7431        PHAB       D        GC_WOODY      1 ''       eForms
						   7431        PHAB       D      U_NONWOODY      2 ''       eForms
						   7431        PHAB       D         U_WOODY      2 ''       eForms
						   7431        PHAB       D      UNDERSTORY      M ''       eForms
						   7431        PHAB       E      C_BIGTREES      2 ''       eForms
						   7431        PHAB       E    C_SMALLTREES      2 ''       eForms
						   7431        PHAB       E          CANOPY      M ''       eForms
						   7431        PHAB       E         GC_BARE      4 ''       eForms
						   7431        PHAB       E    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       E     GC_NONWOODY      1 ''       eForms
						   7431        PHAB       E        GC_WOODY      1 ''       eForms
						   7431        PHAB       E      U_NONWOODY      1 ''       eForms
						   7431        PHAB       E         U_WOODY      2 ''       eForms
						   7431        PHAB       E      UNDERSTORY      M ''       eForms
						   7431        PHAB       G      C_BIGTREES      0 ''       eForms
						   7431        PHAB       G    C_SMALLTREES      0 ''       eForms
						   7431        PHAB       G         GC_BARE      0 ''       eForms
						   7431        PHAB       G    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       G     GC_NONWOODY      0 ''       eForms
						   7431        PHAB       G        GC_WOODY      1 ''       eForms
						   7431        PHAB       G      U_NONWOODY      0 ''       eForms
						   7431        PHAB       G         U_WOODY      1 ''       eForms
						   7431        PHAB       G      UNDERSTORY      C ''       eForms
						   7431        PHAB       H      C_BIGTREES      0 ''       eForms
						   7431        PHAB       H    C_SMALLTREES      3 ''       eForms
						   7431        PHAB       H          CANOPY      M ''       eForms
						   7431        PHAB       H         GC_BARE      2 ''       eForms
						   7431        PHAB       H    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       H     GC_NONWOODY      2 ''       eForms
						   7431        PHAB       H        GC_WOODY      2 ''       eForms
						   7431        PHAB       H      U_NONWOODY      0 ''       eForms
						   7431        PHAB       H         U_WOODY      2 ''       eForms
						   7431        PHAB       H      UNDERSTORY      M ''       eForms
						   7431        PHAB       I      C_BIGTREES      1 ''       eForms
						   7431        PHAB       I    C_SMALLTREES      2 ''       eForms
						   7431        PHAB       I          CANOPY      M ''       eForms
						   7431        PHAB       I         GC_BARE      0 ''       eForms
						   7431        PHAB       I    GC_INUNDATED      0 ''       eForms
						   7431        PHAB       I     GC_NONWOODY      2 ''       eForms
						   7431        PHAB       I        GC_WOODY      2 ''       eForms
						   7431        PHAB       I      U_NONWOODY      0 ''       eForms
						   7431        PHAB       I         U_WOODY      2 ''       eForms
						   7431        PHAB       I      UNDERSTORY      M ''       eForms
						   7431        PHAB       J      C_BIGTREES      0 ''       eForms
						   7431        PHAB       J    C_SMALLTREES      0 ''       eForms
						   7431        PHAB       J         GC_BARE      0 ''       eForms
						   7431        PHAB       J    GC_INUNDATED      2 ''       eForms
						   7431        PHAB       J     GC_NONWOODY      3 ''       eForms
						   7431        PHAB       J        GC_WOODY      2 ''       eForms
						   7431        PHAB       J      U_NONWOODY      1 ''       eForms
						   7431        PHAB       J         U_WOODY      1 ''       eForms
						   7431        PHAB       J      UNDERSTORY      D ''       eForms
						1000222        PHAB       A      C_BIGTREES      1 ''       eForms
						1000222        PHAB       A   C_BIGTREES_DD      0 ''       eForms
						1000222        PHAB       A    C_SMALLTREES      1 ''       eForms
						1000222        PHAB       A C_SMALLTREES_DD      0 ''       eForms
						1000222        PHAB       A          CANOPY      D ''       eForms
						1000222        PHAB       A       CANOPY_DD      N ''       eForms
						1000222        PHAB       A         GC_BARE      0 ''       eForms
						1000222        PHAB       A      GC_BARE_DD      2 ''       eForms
						1000222        PHAB       A    GC_INUNDATED      0 ''       eForms
						1000222        PHAB       A GC_INUNDATED_DD      0 ''       eForms
						1000222        PHAB       A     GC_NONWOODY      2 ''       eForms
						1000222        PHAB       A  GC_NONWOODY_DD      2 ''       eForms
						1000222        PHAB       A        GC_WOODY      1 ''       eForms
						1000222        PHAB       A     GC_WOODY_DD      0 ''       eForms
						1000222        PHAB       A   HORIZ_DIST_DD    5.0 ''       eForms
						1000222        PHAB       A      U_NONWOODY      4 ''       eForms
						1000222        PHAB       A   U_NONWOODY_DD      1 ''       eForms
						1000222        PHAB       A         U_WOODY      2 ''       eForms
						1000222        PHAB       A      U_WOODY_DD      0 ''       eForms
						1000222        PHAB       A      UNDERSTORY      D ''       eForms
						1000222        PHAB       A   UNDERSTORY_DD      D ''       eForms
						1000222        PHAB       B      C_BIGTREES      2 ''       eForms
						1000222        PHAB       B   C_BIGTREES_DD      2 ''       eForms
						1000222        PHAB       B    C_SMALLTREES      1 ''       eForms
						1000222        PHAB       B C_SMALLTREES_DD      0 ''       eForms
						1000222        PHAB       B          CANOPY      D ''       eForms
						1000222        PHAB       B       CANOPY_DD      D ''       eForms
						1000222        PHAB       B         GC_BARE      0 ''       eForms
						1000222        PHAB       B      GC_BARE_DD      2 ''       eForms
						1000222        PHAB       B    GC_INUNDATED      1 ''       eForms
						1000222        PHAB       B GC_INUNDATED_DD      0 ''       eForms
						1000222        PHAB       B     GC_NONWOODY      1 ''       eForms
						1000222        PHAB       B  GC_NONWOODY_DD      4 ''       eForms
						1000222        PHAB       B        GC_WOODY      1 ''       eForms
						1000222        PHAB       B     GC_WOODY_DD      0 ''       eForms
						1000222        PHAB       B   HORIZ_DIST_DD    6.0 ''       eForms
						1000222        PHAB       B      U_NONWOODY      4 ''       eForms
						1000222        PHAB       B   U_NONWOODY_DD      1 ''       eForms
						1000222        PHAB       B         U_WOODY      1 ''       eForms
						1000222        PHAB       B      U_WOODY_DD      1 ''       eForms
						1000222        PHAB       B      UNDERSTORY      D ''       eForms
						1000222        PHAB       B   UNDERSTORY_DD      D ''       eForms
						1000222        PHAB       C      C_BIGTREES      2 ''       eForms
						1000222        PHAB       C   C_BIGTREES_DD      2 ''       eForms
						1000222        PHAB       C    C_SMALLTREES      1 ''       eForms
						1000222        PHAB       C C_SMALLTREES_DD      0 ''       eForms
						1000222        PHAB       C          CANOPY      D ''       eForms
						1000222        PHAB       C       CANOPY_DD      D ''       eForms
						1000222        PHAB       C         GC_BARE      1 ''       eForms
						1000222        PHAB       C      GC_BARE_DD      2 ''       eForms
						1000222        PHAB       C    GC_INUNDATED      0 ''       eForms
						1000222        PHAB       C GC_INUNDATED_DD      2 ''       eForms
						1000222        PHAB       C     GC_NONWOODY      2 ''       eForms
						1000222        PHAB       C  GC_NONWOODY_DD      3 ''       eForms
						1000222        PHAB       C        GC_WOODY      2 ''       eForms
						1000222        PHAB       C     GC_WOODY_DD      0 ''       eForms
						1000222        PHAB       C   HORIZ_DIST_DD    5.0 ''       eForms
						1000222        PHAB       C      U_NONWOODY      2 ''       eForms
						1000222        PHAB       C   U_NONWOODY_DD      1 ''       eForms
						1000222        PHAB       C         U_WOODY      3 ''       eForms
						1000222        PHAB       C      U_WOODY_DD      1 ''       eForms
						1000222        PHAB       C      UNDERSTORY      D ''       eForms
						1000222        PHAB       I      C_BIGTREES      3 ''       eForms
						1000222        PHAB       I   C_BIGTREES_DD      2 ''       eForms
						1000222        PHAB       I    C_SMALLTREES      0 ''       eForms
						1000222        PHAB       I C_SMALLTREES_DD      0 ''       eForms
						1000222        PHAB       I          CANOPY      D ''       eForms
						1000222        PHAB       I       CANOPY_DD      D ''       eForms
						1000222        PHAB       I         GC_BARE      1 ''       eForms
						1000222        PHAB       I      GC_BARE_DD      3 ''       eForms
						1000222        PHAB       I    GC_INUNDATED      0 ''       eForms
						1000222        PHAB       I GC_INUNDATED_DD      0 ''       eForms
						1000222        PHAB       I     GC_NONWOODY      2 ''       eForms
						1000222        PHAB       I  GC_NONWOODY_DD      2 ''       eForms
						1000222        PHAB       I        GC_WOODY      2 ''       eForms
						1000222        PHAB       I     GC_WOODY_DD      1 ''       eForms
						1000222        PHAB       I   HORIZ_DIST_DD    1.5 ''       eForms
						1000222        PHAB       I      U_NONWOODY      3 ''       eForms
						1000222        PHAB       I   U_NONWOODY_DD      1 ''       eForms
						1000222        PHAB       I         U_WOODY      1 ''       eForms
						1000222        PHAB       I      U_WOODY_DD      1 ''       eForms
						1000222        PHAB       I      UNDERSTORY      D ''       eForms
						1000222        PHAB       I   UNDERSTORY_DD      D ''       eForms
						1000222        PHAB       J      C_BIGTREES      2 ''       eForms
						1000222        PHAB       J   C_BIGTREES_DD      2 ''       eForms
						1000222        PHAB       J    C_SMALLTREES      2 ''       eForms
						1000222        PHAB       J C_SMALLTREES_DD      0 ''       eForms
						1000222        PHAB       J          CANOPY      D ''       eForms
						1000222        PHAB       J       CANOPY_DD      D ''       eForms
						1000222        PHAB       J         GC_BARE      0 ''       eForms
						1000222        PHAB       J      GC_BARE_DD      3 ''       eForms
						1000222        PHAB       J    GC_INUNDATED      0 ''       eForms
						1000222        PHAB       J GC_INUNDATED_DD      0 ''       eForms
						1000222        PHAB       J     GC_NONWOODY      2 ''       eForms
						1000222        PHAB       J  GC_NONWOODY_DD      2 ''       eForms
						1000222        PHAB       J        GC_WOODY      1 ''       eForms
						1000222        PHAB       J     GC_WOODY_DD      0 ''       eForms
						1000222        PHAB       J   HORIZ_DIST_DD    4.0 ''       eForms
						1000222        PHAB       J      U_NONWOODY      3 ''       eForms
						1000222        PHAB       J   U_NONWOODY_DD      1 ''       eForms
						1000222        PHAB       J         U_WOODY      2 ''       eForms
						1000222        PHAB       J      U_WOODY_DD      1 ''       eForms
						1000222        PHAB       J      UNDERSTORY      D ''       eForms
							6362       PHAB       J        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       A        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       B        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       C        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       D        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       E        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       F        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       G        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       H        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       I        DRAWDOWN    YES NA   PHAB_FRONT
							6396       PHAB       J        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       A        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       B        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       C        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       D        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       E        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       F        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       G        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       H        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       I        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       J        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       K        DRAWDOWN    YES NA   PHAB_FRONT
							6449       PHAB       L        DRAWDOWN    YES NA   PHAB_FRONT
							6518       PHAB       A        DRAWDOWN    YES NA   PHAB_FRONT
							6518       PHAB       B        DRAWDOWN    YES NA   PHAB_FRONT
							6518       PHAB       C        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       D        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       E        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       F        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       G        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       H        DRAWDOWN    YES NA   PHAB_FRONT
							6518       PHAB       I        DRAWDOWN     NO NA   PHAB_FRONT
							6518       PHAB       J        DRAWDOWN    YES NA   PHAB_FRONT
							6530       PHAB       A        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       B        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       C        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       D        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       E        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       F        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       G        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       H        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       I        DRAWDOWN     NO NA   PHAB_FRONT
							6530       PHAB       J        DRAWDOWN     NO NA   PHAB_FRONT
							6618       PHAB       A        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       B        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       C        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       D        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       E        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       F        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       G        DRAWDOWN     NO NA   PHAB_FRONT
							6618       PHAB       H        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       I        DRAWDOWN    YES NA   PHAB_FRONT
							6618       PHAB       J        DRAWDOWN    YES NA   PHAB_FRONT
							6647       PHAB       A        DRAWDOWN      Y ''       eForms
							6647       PHAB       B        DRAWDOWN     NO ''       eForms
							6647       PHAB       C        DRAWDOWN      Y ''       eForms
							6647       PHAB       D        DRAWDOWN      Y ''       eForms
							6647       PHAB       E        DRAWDOWN      Y ''       eForms
							6647       PHAB       F        DRAWDOWN     NO ''       eForms
							6647       PHAB       G        DRAWDOWN     NO ''       eForms
							6647       PHAB       H        DRAWDOWN     NO ''       eForms
							6647       PHAB       I        DRAWDOWN     NO ''       eForms
							6647       PHAB       J        DRAWDOWN      Y ''       eForms
							7431       PHAB       A        DRAWDOWN      N ''       eForms
							7431       PHAB       B        DRAWDOWN      N ''       eForms
							7431       PHAB       D        DRAWDOWN      N ''       eForms
							7431       PHAB       E        DRAWDOWN      N ''       eForms
							7431       PHAB       G        DRAWDOWN      N ''       eForms
							7431       PHAB       H        DRAWDOWN      N ''       eForms
							7431       PHAB       I        DRAWDOWN      N ''       eForms
							7431       PHAB       J        DRAWDOWN      N ''       eForms
							1000222    PHAB       A        DRAWDOWN      Y ''       eForms
							1000222    PHAB       B        DRAWDOWN      Y ''       eForms
							1000222    PHAB       C        DRAWDOWN      Y ''       eForms
							1000222    PHAB       I        DRAWDOWN      Y ''       eForms
							1000222    PHAB       J        DRAWDOWN      Y ''       eForms
						 ")		
	longMets <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(longMets)	
}


nlaRiparianVegetationTest.expectedResultsWithDrawDownAndNoFillin <- function()
#
{
	tc <- textConnection("	 SITE             METRIC                      VALUE
							6362         RVFCCANBIG_DD  0.000000000000000000000000
							6362        RVFCCANBIG_RIP  0.000000000000000000000000
							6362        RVFCCANBIG_SYN  0.000000000000000000000000
							6362       RVFCCANSMALL_DD  0.000000000000000000000000
							6362      RVFCCANSMALL_RIP  0.000000000000000000000000
							6362      RVFCCANSMALL_SYN  0.000000000000000000000000
							6362        RVFCGNDBARE_DD  0.945945945945939947741010
							6362       RVFCGNDBARE_RIP  0.000000000000000000000000
							6362       RVFCGNDBARE_SYN  0.945945945945939947741010
							6362   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6362  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6362  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6362        RVFCGNDNONW_DD  0.054054054054049997801723
							6362       RVFCGNDNONW_RIP  0.303030303030299985600493
							6362       RVFCGNDNONW_SYN  0.054054054054049997801723
							6362       RVFCGNDWOODY_DD  0.000000000000000000000000
							6362      RVFCGNDWOODY_RIP  0.696969696969690022392285
							6362      RVFCGNDWOODY_SYN  0.000000000000000000000000
							6362        RVFCUNDNONW_DD  0.000000000000000000000000
							6362       RVFCUNDNONW_RIP  0.250000000000000000000000
							6362       RVFCUNDNONW_SYN  0.000000000000000000000000
							6362       RVFCUNDWOODY_DD  0.000000000000000000000000
							6362      RVFCUNDWOODY_RIP  0.574999999999999955591079
							6362      RVFCUNDWOODY_SYN  0.000000000000000000000000
							6362         RVFPCANBIG_DD  0.000000000000000000000000
							6362        RVFPCANBIG_RIP  0.000000000000000000000000
							6362        RVFPCANBIG_SYN  0.000000000000000000000000
							6362       RVFPCANSMALL_DD  0.000000000000000000000000
							6362      RVFPCANSMALL_RIP  0.000000000000000000000000
							6362      RVFPCANSMALL_SYN  0.000000000000000000000000
							6362        RVFPGNDBARE_DD  1.000000000000000000000000
							6362       RVFPGNDBARE_RIP  0.000000000000000000000000
							6362       RVFPGNDBARE_SYN  1.000000000000000000000000
							6362   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6362  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6362  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6362        RVFPGNDNONW_DD  1.000000000000000000000000
							6362       RVFPGNDNONW_RIP  1.000000000000000000000000
							6362       RVFPGNDNONW_SYN  1.000000000000000000000000
							6362       RVFPGNDWOODY_DD  0.000000000000000000000000
							6362      RVFPGNDWOODY_RIP  1.000000000000000000000000
							6362      RVFPGNDWOODY_SYN  0.000000000000000000000000
							6362  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6362        RVFPUNDNONW_DD  0.000000000000000000000000
							6362       RVFPUNDNONW_RIP  1.000000000000000000000000
							6362       RVFPUNDNONW_SYN  0.000000000000000000000000
							6362       RVFPUNDWOODY_DD  0.000000000000000000000000
							6362      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6362      RVFPUNDWOODY_SYN  0.000000000000000000000000
							6362          RVICANOPY_DD  0.000000000000000000000000
							6362         RVICANOPY_RIP  0.000000000000000000000000
							6362         RVICANOPY_SYN  0.000000000000000000000000
							6362          RVICANUND_DD  0.000000000000000000000000
							6362         RVICANUND_RIP  0.824999999999999955591079
							6362         RVICANUND_SYN  0.000000000000000000000000
							6362          RVIGROUND_DD  0.050000000000000002775558
							6362         RVIGROUND_RIP  0.824999999999999955591079
							6362         RVIGROUND_SYN  0.050000000000000002775558
							6362           RVIHERBS_DD  0.050000000000000002775558
							6362          RVIHERBS_RIP  0.500000000000000000000000
							6362          RVIHERBS_SYN  0.050000000000000002775558
							6362        RVITALLWOOD_DD  0.000000000000000000000000
							6362       RVITALLWOOD_RIP  0.574999999999999955591079
							6362       RVITALLWOOD_SYN  0.000000000000000000000000
							6362        RVITOTALVEG_DD  0.050000000000000002775558
							6362       RVITOTALVEG_RIP  1.649999999999999911182158
							6362       RVITOTALVEG_SYN  0.050000000000000002775558
							6362      RVIUNDERSTORY_DD  0.000000000000000000000000
							6362     RVIUNDERSTORY_RIP  0.824999999999999955591079
							6362     RVIUNDERSTORY_SYN  0.000000000000000000000000
							6362           RVIWOODY_DD  0.000000000000000000000000
							6362          RVIWOODY_RIP  1.149999999999999911182158
							6362          RVIWOODY_SYN  0.000000000000000000000000
							6362          RVNCANBIG_DD  1.000000000000000000000000
							6362         RVNCANBIG_RIP  1.000000000000000000000000
							6362         RVNCANBIG_SYN  1.000000000000000000000000
							6362        RVNCANSMALL_DD  1.000000000000000000000000
							6362       RVNCANSMALL_RIP  1.000000000000000000000000
							6362       RVNCANSMALL_SYN  1.000000000000000000000000
							6362         RVNGNDBARE_DD  1.000000000000000000000000
							6362        RVNGNDBARE_RIP  1.000000000000000000000000
							6362        RVNGNDBARE_SYN  1.000000000000000000000000
							6362    RVNGNDINUNDATED_DD  1.000000000000000000000000
							6362   RVNGNDINUNDATED_RIP  1.000000000000000000000000
							6362   RVNGNDINUNDATED_SYN  1.000000000000000000000000
							6362         RVNGNDNONW_DD  1.000000000000000000000000
							6362        RVNGNDNONW_RIP  1.000000000000000000000000
							6362        RVNGNDNONW_SYN  1.000000000000000000000000
							6362        RVNGNDWOODY_DD  1.000000000000000000000000
							6362       RVNGNDWOODY_RIP  1.000000000000000000000000
							6362       RVNGNDWOODY_SYN  1.000000000000000000000000
							6362     RVNUNDERSTORY_RIP  1.000000000000000000000000
							6362         RVNUNDNONW_DD  1.000000000000000000000000
							6362        RVNUNDNONW_RIP  1.000000000000000000000000
							6362        RVNUNDNONW_SYN  1.000000000000000000000000
							6362        RVNUNDWOODY_DD  1.000000000000000000000000
							6362       RVNUNDWOODY_RIP  1.000000000000000000000000
							6362       RVNUNDWOODY_SYN  1.000000000000000000000000
							6362          RVVCANBIG_DD                          NA
							6362         RVVCANBIG_RIP                          NA
							6362         RVVCANBIG_SYN                          NA
							6362        RVVCANSMALL_DD                          NA
							6362       RVVCANSMALL_RIP                          NA
							6362       RVVCANSMALL_SYN                          NA
							6362         RVVGNDBARE_DD                          NA
							6362        RVVGNDBARE_RIP                          NA
							6362        RVVGNDBARE_SYN                          NA
							6362    RVVGNDINUNDATED_DD                          NA
							6362   RVVGNDINUNDATED_RIP                          NA
							6362   RVVGNDINUNDATED_SYN                          NA
							6362         RVVGNDNONW_DD                          NA
							6362        RVVGNDNONW_RIP                          NA
							6362        RVVGNDNONW_SYN                          NA
							6362        RVVGNDWOODY_DD                          NA
							6362       RVVGNDWOODY_RIP                          NA
							6362       RVVGNDWOODY_SYN                          NA
							6362         RVVUNDNONW_DD                          NA
							6362        RVVUNDNONW_RIP                          NA
							6362        RVVUNDNONW_SYN                          NA
							6362        RVVUNDWOODY_DD                          NA
							6362       RVVUNDWOODY_RIP                          NA
							6362       RVVUNDWOODY_SYN                          NA
							6362   RVFPCANDECIDUOUS_DD                          NA
							6362  RVFPCANDECIDUOUS_RIP                          NA
							6362      RVFPCANMIXED_RIP                          NA
							6362   RVFPUNDDECIDUOUS_DD                          NA
							6362      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6362          RVNCANOPY_DD  0.000000000000000000000000
							6362         RVNCANOPY_RIP  0.000000000000000000000000
							6362      RVNUNDERSTORY_DD  0.000000000000000000000000
							6362        RVFPCANNONE_DD                          NA
							6362       RVFPCANNONE_RIP                          NA
							6362        RVFPUNDNONE_DD                          NA
							6362       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6362 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6396         RVFCCANBIG_DD  0.010000000000000000208167
							6396        RVFCCANBIG_RIP  0.059999999999999997779554
							6396        RVFCCANBIG_SYN  0.038999999999999999944489
							6396       RVFCCANSMALL_DD  0.000000000000000000000000
							6396      RVFCCANSMALL_RIP  0.144999999999999990007993
							6396      RVFCCANSMALL_SYN  0.066666666666660004403333
							6396        RVFCGNDBARE_DD  0.375853697853689994179405
							6396       RVFCGNDBARE_RIP  0.183905723905719992172791
							6396       RVFCGNDBARE_SYN  0.244424234346090007985453
							6396   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6396  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6396  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6396        RVFCGNDNONW_DD  0.580760058760049990844720
							6396       RVFCGNDNONW_RIP  0.462332852332849986165542
							6396       RVFCGNDNONW_SYN  0.554528859471799973057671
							6396       RVFCGNDWOODY_DD  0.043386243386240003205856
							6396      RVFCGNDWOODY_RIP  0.353761423761420001898870
							6396      RVFCGNDWOODY_SYN  0.201046906182090007186858
							6396        RVFCUNDNONW_DD  0.050000000000000002775558
							6396       RVFCUNDNONW_RIP  0.174999999999999988897770
							6396       RVFCUNDNONW_SYN  0.101333333333330000147754
							6396       RVFCUNDWOODY_DD  0.005000000000000000104083
							6396      RVFCUNDWOODY_RIP  0.202500000000000013322676
							6396      RVFCUNDWOODY_SYN  0.088333333333330002479222
							6396         RVFPCANBIG_DD  0.200000000000000011102230
							6396        RVFPCANBIG_RIP  0.400000000000000022204460
							6396        RVFPCANBIG_SYN  0.599999999999999977795540
							6396       RVFPCANSMALL_DD  0.000000000000000000000000
							6396      RVFPCANSMALL_RIP  0.900000000000000022204460
							6396      RVFPCANSMALL_SYN  0.900000000000000022204460
							6396        RVFPGNDBARE_DD  0.900000000000000022204460
							6396       RVFPGNDBARE_RIP  0.900000000000000022204460
							6396       RVFPGNDBARE_SYN  0.900000000000000022204460
							6396   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6396  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6396  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6396        RVFPGNDNONW_DD  1.000000000000000000000000
							6396       RVFPGNDNONW_RIP  0.900000000000000022204460
							6396       RVFPGNDNONW_SYN  1.000000000000000000000000
							6396       RVFPGNDWOODY_DD  0.299999999999999988897770
							6396      RVFPGNDWOODY_RIP  0.900000000000000022204460
							6396      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6396  RVFPUNDDECIDUOUS_RIP  0.599999999999999977795540
							6396        RVFPUNDNONW_DD  0.200000000000000011102230
							6396       RVFPUNDNONW_RIP  0.599999999999999977795540
							6396       RVFPUNDNONW_SYN  0.599999999999999977795540
							6396       RVFPUNDWOODY_DD  0.100000000000000005551115
							6396      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6396      RVFPUNDWOODY_SYN  1.000000000000000000000000
							6396          RVICANOPY_DD  0.010000000000000000208167
							6396         RVICANOPY_RIP  0.204999999999999987787547
							6396         RVICANOPY_SYN  0.105666666666659997408928
							6396          RVICANUND_DD  0.065000000000000002220446
							6396         RVICANUND_RIP  0.582500000000000017763568
							6396         RVICANUND_SYN  0.295333333333330005920914
							6396          RVIGROUND_DD  0.405000000000000026645353
							6396         RVIGROUND_RIP  0.630000000000000004440892
							6396         RVIGROUND_SYN  0.518333333333329981940096
							6396           RVIHERBS_DD  0.419999999999999984456878
							6396          RVIHERBS_RIP  0.522499999999999964472863
							6396          RVIHERBS_SYN  0.483499999999999985345056
							6396        RVITALLWOOD_DD  0.014999999999999999444888
							6396       RVITALLWOOD_RIP  0.407499999999999973354647
							6396       RVITALLWOOD_SYN  0.194000000000000005773160
							6396        RVITOTALVEG_DD  0.469999999999999973354647
							6396       RVITOTALVEG_RIP  1.212499999999999911182158
							6396       RVITOTALVEG_SYN  0.813666666666659987861010
							6396      RVIUNDERSTORY_DD  0.055000000000000000277556
							6396     RVIUNDERSTORY_RIP  0.377500000000000002220446
							6396     RVIUNDERSTORY_SYN  0.189666666666659988749188
							6396           RVIWOODY_DD  0.050000000000000002775558
							6396          RVIWOODY_RIP  0.689999999999999946709295
							6396          RVIWOODY_SYN  0.330166666666660002515954
							6396          RVNCANBIG_DD 10.000000000000000000000000
							6396         RVNCANBIG_RIP 10.000000000000000000000000
							6396         RVNCANBIG_SYN 10.000000000000000000000000
							6396        RVNCANSMALL_DD 10.000000000000000000000000
							6396       RVNCANSMALL_RIP 10.000000000000000000000000
							6396       RVNCANSMALL_SYN 10.000000000000000000000000
							6396         RVNGNDBARE_DD 10.000000000000000000000000
							6396        RVNGNDBARE_RIP 10.000000000000000000000000
							6396        RVNGNDBARE_SYN 10.000000000000000000000000
							6396    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6396   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6396   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6396         RVNGNDNONW_DD 10.000000000000000000000000
							6396        RVNGNDNONW_RIP 10.000000000000000000000000
							6396        RVNGNDNONW_SYN 10.000000000000000000000000
							6396        RVNGNDWOODY_DD 10.000000000000000000000000
							6396       RVNGNDWOODY_RIP 10.000000000000000000000000
							6396       RVNGNDWOODY_SYN 10.000000000000000000000000
							6396     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6396         RVNUNDNONW_DD 10.000000000000000000000000
							6396        RVNUNDNONW_RIP 10.000000000000000000000000
							6396        RVNUNDNONW_SYN 10.000000000000000000000000
							6396        RVNUNDWOODY_DD 10.000000000000000000000000
							6396       RVNUNDWOODY_RIP 10.000000000000000000000000
							6396       RVNUNDWOODY_SYN 10.000000000000000000000000
							6396          RVVCANBIG_DD  0.021081851067779999037155
							6396         RVVCANBIG_RIP  0.102198064778369998584218
							6396         RVVCANBIG_SYN  0.064808359466889994826566
							6396        RVVCANSMALL_DD  0.000000000000000000000000
							6396       RVVCANSMALL_RIP  0.111679103784999997350802
							6396       RVVCANSMALL_SYN  0.059275460748210002936975
							6396         RVVGNDBARE_DD  0.343959754793390026783584
							6396        RVVGNDBARE_RIP  0.296809325540589974412597
							6396        RVVGNDBARE_SYN  0.174853558544070003533122
							6396    RVVGNDINUNDATED_DD  0.000000000000000000000000
							6396   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6396   RVVGNDINUNDATED_SYN  0.000000000000000000000000
							6396         RVVGNDNONW_DD  0.349201271910390020902781
							6396        RVVGNDNONW_RIP  0.308979151503730009586945
							6396        RVVGNDNONW_SYN  0.226428136349729997034430
							6396        RVVGNDWOODY_DD  0.090543377455459997382725
							6396       RVVGNDWOODY_RIP  0.265292610762579983418874
							6396       RVVGNDWOODY_SYN  0.150298739575700002202296
							6396         RVVUNDNONW_DD  0.105409255338939994439684
							6396        RVVUNDNONW_RIP  0.232139804619729994694666
							6396        RVVUNDNONW_SYN  0.119971189957220003563521
							6396        RVVUNDWOODY_DD  0.015811388300840001353009
							6396       RVVUNDWOODY_RIP  0.164337897164479995737452
							6396       RVVUNDWOODY_SYN  0.058060207150169997059042
							6396   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6396  RVFPCANDECIDUOUS_RIP  0.599999999999999977795540
							6396      RVFPCANMIXED_RIP  0.400000000000000022204460
							6396   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6396      RVFPUNDMIXED_RIP  0.400000000000000022204460
							6396          RVNCANOPY_DD 10.000000000000000000000000
							6396         RVNCANOPY_RIP 10.000000000000000000000000
							6396      RVNUNDERSTORY_DD 10.000000000000000000000000
							6396        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6396       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6396        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6396       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6396 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6449         RVFCCANBIG_DD  0.093750000000000000000000
							6449        RVFCCANBIG_RIP  0.156250000000000000000000
							6449        RVFCCANBIG_SYN  0.155361111111110006266500
							6449       RVFCCANSMALL_DD  0.066666666666660004403333
							6449      RVFCCANSMALL_RIP  0.125000000000000000000000
							6449      RVFCCANSMALL_SYN  0.122236111111110004601166
							6449        RVFCGNDBARE_DD  0.316035661725310002001521
							6449       RVFCGNDBARE_RIP  0.069755229666569995572445
							6449       RVFCGNDBARE_SYN  0.074063805689980000135364
							6449   RVFCGNDINUNDATED_DD  0.046780150803129999514951
							6449  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6449  RVFCGNDINUNDATED_SYN  0.002217421353869999944841
							6449        RVFCGNDNONW_DD  0.595106358037389981952003
							6449       RVFCGNDNONW_RIP  0.788864667313240008894581
							6449       RVFCGNDNONW_SYN  0.784683080231180052344087
							6449       RVFCGNDWOODY_DD  0.042077829434149997822612
							6449      RVFCGNDWOODY_RIP  0.141380103020170011518530
							6449      RVFCGNDWOODY_SYN  0.139035692724950005194628
							6449        RVFCUNDNONW_DD  0.102083333333330000813888
							6449       RVFCUNDNONW_RIP  0.066666666666660004403333
							6449       RVFCUNDNONW_SYN  0.066736111111109996940627
							6449       RVFCUNDWOODY_DD  0.114583333333329998038330
							6449      RVFCUNDWOODY_RIP  0.193750000000000005551115
							6449      RVFCUNDWOODY_SYN  0.192624999999999990674127
							6449         RVFPCANBIG_DD  0.250000000000000000000000
							6449        RVFPCANBIG_RIP  0.583333333333330039671694
							6449        RVFPCANBIG_SYN  0.583333333333330039671694
							6449       RVFPCANSMALL_DD  0.250000000000000000000000
							6449      RVFPCANSMALL_RIP  0.416666666666660023832236
							6449      RVFPCANSMALL_SYN  0.416666666666660023832236
							6449        RVFPGNDBARE_DD  1.000000000000000000000000
							6449       RVFPGNDBARE_RIP  0.916666666666659968321085
							6449       RVFPGNDBARE_SYN  1.000000000000000000000000
							6449   RVFPGNDINUNDATED_DD  0.416666666666660023832236
							6449  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6449  RVFPGNDINUNDATED_SYN  0.416666666666660023832236
							6449        RVFPGNDNONW_DD  1.000000000000000000000000
							6449       RVFPGNDNONW_RIP  1.000000000000000000000000
							6449       RVFPGNDNONW_SYN  1.000000000000000000000000
							6449       RVFPGNDWOODY_DD  0.500000000000000000000000
							6449      RVFPGNDWOODY_RIP  0.916666666666659968321085
							6449      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6449  RVFPUNDDECIDUOUS_RIP  0.909090909090899956801479
							6449        RVFPUNDNONW_DD  0.500000000000000000000000
							6449       RVFPUNDNONW_RIP  0.333333333333329984160542
							6449       RVFPUNDNONW_SYN  0.500000000000000000000000
							6449       RVFPUNDWOODY_DD  0.416666666666660023832236
							6449      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6449      RVFPUNDWOODY_SYN  1.000000000000000000000000
							6449          RVICANOPY_DD  0.172916666666660001627776
							6449         RVICANOPY_RIP  0.281250000000000000000000
							6449         RVICANOPY_SYN  0.277597222222219996989878
							6449          RVICANUND_DD  0.389583333333330006365003
							6449         RVICANUND_RIP  0.541666666666659968321085
							6449         RVICANUND_SYN  0.536958333333329984604632
							6449          RVIGROUND_DD  0.458333333333329984160542
							6449         RVIGROUND_RIP  0.764583333333329950853852
							6449         RVIGROUND_SYN  0.758791666666659980755583
							6449           RVIHERBS_DD  0.497916666666660012730006
							6449          RVIHERBS_RIP  0.691666666666659990525545
							6449          RVIHERBS_SYN  0.687486111111110020921444
							6449        RVITALLWOOD_DD  0.287499999999989985788318
							6449       RVITALLWOOD_RIP  0.474999999999999977795540
							6449       RVITALLWOOD_SYN  0.470222222222219987664005
							6449        RVITOTALVEG_DD  0.810416666666660012730006
							6449       RVITOTALVEG_RIP  1.306249999999999911182158
							6449       RVITOTALVEG_SYN  1.294194444444439984209794
							6449      RVIUNDERSTORY_DD  0.216666666666660012730006
							6449     RVIUNDERSTORY_RIP  0.260416666666660023832236
							6449     RVIUNDERSTORY_SYN  0.259361111111109987614753
							6449           RVIWOODY_DD  0.312500000000000000000000
							6449          RVIWOODY_RIP  0.614583333333330039671694
							6449          RVIWOODY_SYN  0.606708333333329963288350
							6449          RVNCANBIG_DD 12.000000000000000000000000
							6449         RVNCANBIG_RIP 12.000000000000000000000000
							6449         RVNCANBIG_SYN 12.000000000000000000000000
							6449        RVNCANSMALL_DD 12.000000000000000000000000
							6449       RVNCANSMALL_RIP 12.000000000000000000000000
							6449       RVNCANSMALL_SYN 12.000000000000000000000000
							6449         RVNGNDBARE_DD 12.000000000000000000000000
							6449        RVNGNDBARE_RIP 12.000000000000000000000000
							6449        RVNGNDBARE_SYN 12.000000000000000000000000
							6449    RVNGNDINUNDATED_DD 12.000000000000000000000000
							6449   RVNGNDINUNDATED_RIP 12.000000000000000000000000
							6449   RVNGNDINUNDATED_SYN 12.000000000000000000000000
							6449         RVNGNDNONW_DD 12.000000000000000000000000
							6449        RVNGNDNONW_RIP 12.000000000000000000000000
							6449        RVNGNDNONW_SYN 12.000000000000000000000000
							6449        RVNGNDWOODY_DD 12.000000000000000000000000
							6449       RVNGNDWOODY_RIP 12.000000000000000000000000
							6449       RVNGNDWOODY_SYN 12.000000000000000000000000
							6449     RVNUNDERSTORY_RIP 11.000000000000000000000000
							6449         RVNUNDNONW_DD 12.000000000000000000000000
							6449        RVNUNDNONW_RIP 12.000000000000000000000000
							6449        RVNUNDNONW_SYN 12.000000000000000000000000
							6449        RVNUNDWOODY_DD 12.000000000000000000000000
							6449       RVNUNDWOODY_RIP 12.000000000000000000000000
							6449       RVNUNDWOODY_SYN 12.000000000000000000000000
							6449          RVVCANBIG_DD  0.208382569939389999946755
							6449         RVVCANBIG_RIP  0.179052112779789995844482
							6449         RVVCANBIG_SYN  0.178764472230879989389507
							6449        RVVCANSMALL_DD  0.154233196128059996121706
							6449       RVVCANSMALL_RIP  0.221820976137389991311721
							6449       RVVCANSMALL_SYN  0.216175859694860000859151
							6449         RVVGNDBARE_DD  0.267349341182989974630146
							6449        RVVGNDBARE_RIP  0.040792521257720003313718
							6449        RVVGNDBARE_SYN  0.038562900575719996631996
							6449    RVVGNDINUNDATED_DD  0.078946707475039998302968
							6449   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6449   RVVGNDINUNDATED_SYN  0.006183799423889999730131
							6449         RVVGNDNONW_DD  0.257587557654409993190825
							6449        RVVGNDNONW_RIP  0.149444054998849995552490
							6449        RVVGNDNONW_SYN  0.145779195396189997779857
							6449        RVVGNDWOODY_DD  0.048690228200570000172132
							6449       RVVGNDWOODY_RIP  0.125707297804319995115918
							6449       RVVGNDWOODY_SYN  0.122489265296690005846436
							6449         RVVUNDNONW_DD  0.175310871069509988107171
							6449        RVVUNDNONW_RIP  0.111464085804539994595252
							6449        RVVUNDNONW_SYN  0.109957830721760005387999
							6449        RVVUNDWOODY_DD  0.181677265467850013180851
							6449       RVVUNDWOODY_RIP  0.155988417402170009840034
							6449       RVVUNDWOODY_SYN  0.152030900052280010248396
							6449   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6449  RVFPCANDECIDUOUS_RIP  0.888888888888879957761446
							6449      RVFPCANMIXED_RIP  0.111111111111109994720181
							6449   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6449      RVFPUNDMIXED_RIP  0.090909090909089995680148
							6449          RVNCANOPY_DD  5.000000000000000000000000
							6449         RVNCANOPY_RIP  9.000000000000000000000000
							6449      RVNUNDERSTORY_DD  5.000000000000000000000000
							6449        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6449       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6449        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6449       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6449 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6518         RVFCCANBIG_DD  0.000000000000000000000000
							6518        RVFCCANBIG_RIP  0.110000000000000000555112
							6518        RVFCCANBIG_SYN  0.011666666666660000656330
							6518       RVFCCANSMALL_DD  0.000000000000000000000000
							6518      RVFCCANSMALL_RIP  0.154999999999999998889777
							6518      RVFCCANSMALL_SYN  0.011666666666660000656330
							6518        RVFCGNDBARE_DD  0.107789607789600005993691
							6518       RVFCGNDBARE_RIP  0.128659488659479986916168
							6518       RVFCGNDBARE_SYN  0.103448853764600001703400
							6518   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6518  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6518  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6518        RVFCGNDNONW_DD  0.873691873691869957951894
							6518       RVFCGNDNONW_RIP  0.663769123769120006350875
							6518       RVFCGNDNONW_SYN  0.842535697890710033597372
							6518       RVFCGNDWOODY_DD  0.018518518518509999998267
							6518      RVFCGNDWOODY_RIP  0.207571387571379994962939
							6518      RVFCGNDWOODY_SYN  0.054015448344680000447582
							6518        RVFCUNDNONW_DD  0.025000000000000001387779
							6518       RVFCUNDNONW_RIP  0.044999999999999998334665
							6518       RVFCUNDNONW_SYN  0.035833333333329997483219
							6518       RVFCUNDWOODY_DD  0.062500000000000000000000
							6518      RVFCUNDWOODY_RIP  0.187500000000000000000000
							6518      RVFCUNDWOODY_SYN  0.062500000000000000000000
							6518         RVFPCANBIG_DD  0.000000000000000000000000
							6518        RVFPCANBIG_RIP  0.500000000000000000000000
							6518        RVFPCANBIG_SYN  0.250000000000000000000000
							6518       RVFPCANSMALL_DD  0.000000000000000000000000
							6518      RVFPCANSMALL_RIP  0.599999999999999977795540
							6518      RVFPCANSMALL_SYN  0.250000000000000000000000
							6518        RVFPGNDBARE_DD  0.750000000000000000000000
							6518       RVFPGNDBARE_RIP  0.900000000000000022204460
							6518       RVFPGNDBARE_SYN  0.750000000000000000000000
							6518   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6518  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6518  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6518        RVFPGNDNONW_DD  1.000000000000000000000000
							6518       RVFPGNDNONW_RIP  1.000000000000000000000000
							6518       RVFPGNDNONW_SYN  1.000000000000000000000000
							6518       RVFPGNDWOODY_DD  0.250000000000000000000000
							6518      RVFPGNDWOODY_RIP  1.000000000000000000000000
							6518      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6518  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6518        RVFPUNDNONW_DD  0.500000000000000000000000
							6518       RVFPUNDNONW_RIP  0.900000000000000022204460
							6518       RVFPUNDNONW_SYN  0.750000000000000000000000
							6518       RVFPUNDWOODY_DD  0.250000000000000000000000
							6518      RVFPUNDWOODY_RIP  0.699999999999999955591079
							6518      RVFPUNDWOODY_SYN  0.250000000000000000000000
							6518          RVICANOPY_DD  0.000000000000000000000000
							6518         RVICANOPY_RIP  0.280000000000000026645353
							6518         RVICANOPY_SYN  0.0233333333333333 			# without protected*() is 0.009333333333329999967343
							6518          RVICANUND_DD  0.087499999999999994448885
							6518         RVICANUND_RIP  0.512499999999999955591079
							6518         RVICANUND_SYN  0.121666666666667 			# without protected*() is 0.048666666666660002293909
							6518          RVIGROUND_DD  0.737500000000000044408921
							6518         RVIGROUND_RIP  0.729999999999999982236432
							6518         RVIGROUND_SYN  0.77 						# without protected*() is 0.307999999999999996003197
							6518           RVIHERBS_DD  0.750000000000000000000000
							6518          RVIHERBS_RIP  0.612500000000000044408921
							6518          RVIHERBS_SYN  0.760833333333333 			# without protected*() is 0.304333333333330013914519
							6518        RVITALLWOOD_DD  0.062500000000000000000000
							6518       RVITALLWOOD_RIP  0.467500000000000026645353
							6518       RVITALLWOOD_SYN  0.0858333333333333 			# without protected*() is 0.034333333333330003089845
							6518        RVITOTALVEG_DD  0.824999999999999955591079
							6518       RVITOTALVEG_RIP  1.242499999999999937827511
							6518       RVITOTALVEG_SYN  0.891666666666667 			# without protected*() is 0.356666666666660026052682
							6518      RVIUNDERSTORY_DD  0.087499999999999994448885
							6518     RVIUNDERSTORY_RIP  0.232500000000000012212453
							6518     RVIUNDERSTORY_SYN  0.0983333333333333 			# without protected*() is 0.039333333333330000591843
							6518           RVIWOODY_DD  0.074999999999999997224442
							6518          RVIWOODY_RIP  0.630000000000000004440892
							6518          RVIWOODY_SYN  0.130833333333333 			# without protected*() is 0.052333333333329998260375
							6518          RVNCANBIG_DD  4.000000000000000000000000
							6518         RVNCANBIG_RIP 10.000000000000000000000000
							6518         RVNCANBIG_SYN  4.000000000000000000000000
							6518        RVNCANSMALL_DD  4.000000000000000000000000
							6518       RVNCANSMALL_RIP 10.000000000000000000000000
							6518       RVNCANSMALL_SYN  4.000000000000000000000000
							6518         RVNGNDBARE_DD  4.000000000000000000000000
							6518        RVNGNDBARE_RIP 10.000000000000000000000000
							6518        RVNGNDBARE_SYN  4.000000000000000000000000
							6518    RVNGNDINUNDATED_DD  4.000000000000000000000000
							6518   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6518   RVNGNDINUNDATED_SYN  4.000000000000000000000000
							6518         RVNGNDNONW_DD  4.000000000000000000000000
							6518        RVNGNDNONW_RIP 10.000000000000000000000000
							6518        RVNGNDNONW_SYN  4.000000000000000000000000
							6518        RVNGNDWOODY_DD  4.000000000000000000000000
							6518       RVNGNDWOODY_RIP 10.000000000000000000000000
							6518       RVNGNDWOODY_SYN  4.000000000000000000000000
							6518     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6518         RVNUNDNONW_DD  4.000000000000000000000000
							6518        RVNUNDNONW_RIP 10.000000000000000000000000
							6518        RVNUNDNONW_SYN  4.000000000000000000000000
							6518        RVNUNDWOODY_DD  4.000000000000000000000000
							6518       RVNUNDWOODY_RIP 10.000000000000000000000000
							6518       RVNUNDWOODY_SYN  4.000000000000000000000000
							6518          RVVCANBIG_DD  0.000000000000000000000000
							6518         RVVCANBIG_RIP  0.169640141999990001764331
							6518         RVVCANBIG_SYN  0.023333333333330000258776
							6518        RVVCANSMALL_DD  0.000000000000000000000000
							6518       RVVCANSMALL_RIP  0.170701168387590002106791
							6518       RVVCANSMALL_SYN  0.023333333333330000258776
							6518         RVVGNDBARE_DD  0.133867814870360007928340
							6518        RVVGNDBARE_RIP  0.122195434577600001158082
							6518        RVVGNDBARE_SYN  0.126827815308480001954550
							6518    RVVGNDINUNDATED_DD  0.000000000000000000000000
							6518   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6518   RVVGNDINUNDATED_SYN  0.000000000000000000000000
							6518         RVVGNDNONW_DD  0.132768103634570006077453
							6518        RVVGNDNONW_RIP  0.240420634161999990929814
							6518        RVVGNDNONW_SYN  0.126684173910900005344260
							6518        RVVGNDWOODY_DD  0.037037037037029998942650
							6518       RVVGNDWOODY_RIP  0.216161559629449995245665
							6518       RVVGNDWOODY_SYN  0.014006721461169999512486
							6518         RVVUNDNONW_DD  0.028867513459480000082014
							6518        RVVUNDNONW_RIP  0.015811388300840001353009
							6518        RVVUNDNONW_SYN  0.024094720491330001016506
							6518        RVVUNDWOODY_DD  0.125000000000000000000000
							6518       RVVUNDWOODY_RIP  0.180758433029029996763626
							6518       RVVUNDWOODY_SYN  0.125000000000000000000000
							6518   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6518  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							6518      RVFPCANMIXED_RIP                           0	# SAS calculates as NA
							6518   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6518      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6518          RVNCANOPY_DD  1.000000000000000000000000
							6518         RVNCANOPY_RIP 10.000000000000000000000000
							6518      RVNUNDERSTORY_DD  2.000000000000000000000000
							6518        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6518       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6518        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6518       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6518 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6530         RVFCCANBIG_DD                          NA
							6530        RVFCCANBIG_RIP  0.125000000000000000000000
							6530        RVFCCANBIG_SYN                          NA
							6530       RVFCCANSMALL_DD                          NA
							6530      RVFCCANSMALL_RIP  0.020000000000000000416334
							6530      RVFCCANSMALL_SYN                          NA
							6530        RVFCGNDBARE_DD                          NA
							6530       RVFCGNDBARE_RIP  0.283135733135729983356299
							6530       RVFCGNDBARE_SYN                          NA
							6530   RVFCGNDINUNDATED_DD                          NA
							6530  RVFCGNDINUNDATED_RIP  0.005405405405400000307115
							6530  RVFCGNDINUNDATED_SYN                          NA
							6530        RVFCGNDNONW_DD                          NA
							6530       RVFCGNDNONW_RIP  0.670954928954920037931231
							6530       RVFCGNDNONW_SYN                          NA
							6530       RVFCGNDWOODY_DD                          NA
							6530      RVFCGNDWOODY_RIP  0.040503932503930001329806
							6530      RVFCGNDWOODY_SYN                          NA
							6530        RVFCUNDNONW_DD                          NA
							6530       RVFCUNDNONW_RIP  0.040000000000000000832667
							6530       RVFCUNDNONW_SYN                          NA
							6530       RVFCUNDWOODY_DD                          NA
							6530      RVFCUNDWOODY_RIP  0.025000000000000001387779
							6530      RVFCUNDWOODY_SYN                          NA
							6530         RVFPCANBIG_DD                          NA
							6530        RVFPCANBIG_RIP  0.900000000000000022204460
							6530        RVFPCANBIG_SYN                          NA
							6530       RVFPCANSMALL_DD                          NA
							6530      RVFPCANSMALL_RIP  0.400000000000000022204460
							6530      RVFPCANSMALL_SYN                          NA
							6530        RVFPGNDBARE_DD                          NA
							6530       RVFPGNDBARE_RIP  0.800000000000000044408921
							6530       RVFPGNDBARE_SYN                          NA
							6530   RVFPGNDINUNDATED_DD                          NA
							6530  RVFPGNDINUNDATED_RIP  0.100000000000000005551115
							6530  RVFPGNDINUNDATED_SYN                          NA
							6530        RVFPGNDNONW_DD                          NA
							6530       RVFPGNDNONW_RIP  1.000000000000000000000000
							6530       RVFPGNDNONW_SYN                          NA
							6530       RVFPGNDWOODY_DD                          NA
							6530      RVFPGNDWOODY_RIP  0.500000000000000000000000
							6530      RVFPGNDWOODY_SYN                          NA
							6530  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6530        RVFPUNDNONW_DD                          NA
							6530       RVFPUNDNONW_RIP  0.400000000000000022204460
							6530       RVFPUNDNONW_SYN                          NA
							6530       RVFPUNDWOODY_DD                          NA
							6530      RVFPUNDWOODY_RIP  0.500000000000000000000000
							6530      RVFPUNDWOODY_SYN                          NA
							6530          RVICANOPY_DD                          NA
							6530         RVICANOPY_RIP  0.144999999999999990007993
							6530         RVICANOPY_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530          RVICANUND_DD                          NA
							6530         RVICANUND_RIP  0.209999999999999992228439
							6530         RVICANUND_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530          RVIGROUND_DD                          NA
							6530         RVIGROUND_RIP  0.484999999999999986677324
							6530         RVIGROUND_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530           RVIHERBS_DD                          NA
							6530          RVIHERBS_RIP  0.494999999999999995559108
							6530          RVIHERBS_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530        RVITALLWOOD_DD                          NA
							6530       RVITALLWOOD_RIP  0.170000000000000012212453
							6530       RVITALLWOOD_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530        RVITOTALVEG_DD                          NA
							6530       RVITOTALVEG_RIP  0.689999999999999946709295
							6530       RVITOTALVEG_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530      RVIUNDERSTORY_DD                          NA
							6530     RVIUNDERSTORY_RIP  0.065000000000000002220446
							6530     RVIUNDERSTORY_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530           RVIWOODY_DD                          NA
							6530          RVIWOODY_RIP  0.195000000000000006661338
							6530          RVIWOODY_SYN  NA				 			# without protected*() is 0.000000000000000000000000
							6530          RVNCANBIG_DD  0.000000000000000000000000
							6530         RVNCANBIG_RIP 10.000000000000000000000000
							6530         RVNCANBIG_SYN  0.000000000000000000000000
							6530        RVNCANSMALL_DD  0.000000000000000000000000
							6530       RVNCANSMALL_RIP 10.000000000000000000000000
							6530       RVNCANSMALL_SYN  0.000000000000000000000000
							6530         RVNGNDBARE_DD  0.000000000000000000000000
							6530        RVNGNDBARE_RIP 10.000000000000000000000000
							6530        RVNGNDBARE_SYN  0.000000000000000000000000
							6530    RVNGNDINUNDATED_DD  0.000000000000000000000000
							6530   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6530   RVNGNDINUNDATED_SYN  0.000000000000000000000000
							6530         RVNGNDNONW_DD  0.000000000000000000000000
							6530        RVNGNDNONW_RIP 10.000000000000000000000000
							6530        RVNGNDNONW_SYN  0.000000000000000000000000
							6530        RVNGNDWOODY_DD  0.000000000000000000000000
							6530       RVNGNDWOODY_RIP 10.000000000000000000000000
							6530       RVNGNDWOODY_SYN  0.000000000000000000000000
							6530     RVNUNDERSTORY_RIP  9.000000000000000000000000
							6530         RVNUNDNONW_DD  0.000000000000000000000000
							6530        RVNUNDNONW_RIP 10.000000000000000000000000
							6530        RVNUNDNONW_SYN  0.000000000000000000000000
							6530        RVNUNDWOODY_DD  0.000000000000000000000000
							6530       RVNUNDWOODY_RIP 10.000000000000000000000000
							6530       RVNUNDWOODY_SYN  0.000000000000000000000000
							6530          RVVCANBIG_DD                          NA
							6530         RVVCANBIG_RIP  0.108653373420040000318920
							6530         RVVCANBIG_SYN                          NA
							6530        RVVCANSMALL_DD                          NA
							6530       RVVCANSMALL_RIP  0.025819888974709999385215
							6530       RVVCANSMALL_SYN                          NA
							6530         RVVGNDBARE_DD                          NA
							6530        RVVGNDBARE_RIP  0.235351071611580003883901
							6530        RVVGNDBARE_SYN                          NA
							6530    RVVGNDINUNDATED_DD                          NA
							6530   RVVGNDINUNDATED_RIP  0.017093392757659999631192
							6530   RVVGNDINUNDATED_SYN                          NA
							6530         RVVGNDNONW_DD                          NA
							6530        RVVGNDNONW_RIP  0.244463064338319990831394
							6530        RVVGNDNONW_SYN                          NA
							6530        RVVGNDWOODY_DD                          NA
							6530       RVVGNDWOODY_RIP  0.049140532886990002936489
							6530       RVVGNDWOODY_SYN                          NA
							6530         RVVUNDNONW_DD                          NA
							6530        RVVUNDNONW_RIP  0.077459666924139997101761
							6530        RVVUNDNONW_SYN                          NA
							6530        RVVUNDWOODY_DD                          NA
							6530       RVVUNDWOODY_RIP  0.026352313834729999136863
							6530       RVVUNDWOODY_SYN                          NA
							6530   RVFPCANDECIDUOUS_DD                          NA
							6530  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							6530      RVFPCANMIXED_RIP                           0	# SAS calculates as NA
							6530   RVFPUNDDECIDUOUS_DD                          NA
							6530      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6530          RVNCANOPY_DD  0.000000000000000000000000
							6530         RVNCANOPY_RIP 10.000000000000000000000000
							6530      RVNUNDERSTORY_DD  0.000000000000000000000000
							6530        RVFPCANNONE_DD                          NA
							6530       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6530        RVFPUNDNONE_DD                          NA
							6530       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6530 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6618         RVFCCANBIG_DD  0.000000000000000000000000
							6618        RVFCCANBIG_RIP  0.000000000000000000000000
							6618        RVFCCANBIG_SYN  0.000000000000000000000000
							6618       RVFCCANSMALL_DD  0.000000000000000000000000
							6618      RVFCCANSMALL_RIP  0.000000000000000000000000
							6618      RVFCCANSMALL_SYN  0.000000000000000000000000
							6618        RVFCGNDBARE_DD  0.335073593073590025870345
							6618       RVFCGNDBARE_RIP  0.321797517297509994005367
							6618       RVFCGNDBARE_SYN  0.355950663364890007578367
							6618   RVFCGNDINUNDATED_DD  0.059090909090899999822621
							6618  RVFCGNDINUNDATED_RIP  0.027314814814810000481726
							6618  RVFCGNDINUNDATED_SYN  0.022312449109830000820720
							6618        RVFCGNDNONW_DD  0.301073593073589995672279
							6618       RVFCGNDNONW_RIP  0.470233007732999974681576
							6618       RVFCGNDNONW_SYN  0.456497624277759972422075
							6618       RVFCGNDWOODY_DD  0.104761904761900004334763
							6618      RVFCGNDWOODY_RIP  0.180654660154660001714078
							6618      RVFCGNDWOODY_SYN  0.165239263247510009824381
							6618        RVFCUNDNONW_DD  0.107499999999999998334665
							6618       RVFCUNDNONW_RIP  0.195000000000000006661338
							6618       RVFCUNDNONW_SYN  0.197222222222219994991477
							6618       RVFCUNDWOODY_DD  0.074999999999999997224442
							6618      RVFCUNDWOODY_RIP  0.197500000000000008881784
							6618      RVFCUNDWOODY_SYN  0.146481481481479991479944
							6618         RVFPCANBIG_DD  0.000000000000000000000000
							6618        RVFPCANBIG_RIP  0.000000000000000000000000
							6618        RVFPCANBIG_SYN  0.000000000000000000000000
							6618       RVFPCANSMALL_DD  0.000000000000000000000000
							6618      RVFPCANSMALL_RIP  0.000000000000000000000000
							6618      RVFPCANSMALL_SYN  0.000000000000000000000000
							6618        RVFPGNDBARE_DD  0.599999999999999977795540
							6618       RVFPGNDBARE_RIP  0.900000000000000022204460
							6618       RVFPGNDBARE_SYN  0.888888888888879957761446
							6618   RVFPGNDINUNDATED_DD  0.200000000000000011102230
							6618  RVFPGNDINUNDATED_RIP  0.299999999999999988897770
							6618  RVFPGNDINUNDATED_SYN  0.333333333333329984160542
							6618        RVFPGNDNONW_DD  0.699999999999999955591079
							6618       RVFPGNDNONW_RIP  0.900000000000000022204460
							6618       RVFPGNDNONW_SYN  0.888888888888879957761446
							6618       RVFPGNDWOODY_DD  0.200000000000000011102230
							6618      RVFPGNDWOODY_RIP  0.900000000000000022204460
							6618      RVFPGNDWOODY_SYN  0.888888888888879957761446
							6618  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6618        RVFPUNDNONW_DD  0.200000000000000011102230
							6618       RVFPUNDNONW_RIP  0.599999999999999977795540
							6618       RVFPUNDNONW_SYN  0.555555555555550029112055
							6618       RVFPUNDWOODY_DD  0.200000000000000011102230
							6618      RVFPUNDWOODY_RIP  0.900000000000000022204460
							6618      RVFPUNDWOODY_SYN  0.888888888888879957761446
							6618          RVICANOPY_DD  0.000000000000000000000000
							6618         RVICANOPY_RIP  0.000000000000000000000000
							6618         RVICANOPY_SYN  0.000000000000000000000000
							6618          RVICANUND_DD  0.197500000000000008881784
							6618         RVICANUND_RIP  0.392500000000000015543122
							6618         RVICANUND_SYN  0.309333333333330018355412
							6618          RVIGROUND_DD  0.207499999999999990007993
							6618         RVIGROUND_RIP  0.392500000000000015543122
							6618         RVIGROUND_SYN  0.359907407407408 			# without protected*() is 0.323916666666660024720414
							6618           RVIHERBS_DD  0.262500000000000011102230
							6618          RVIHERBS_RIP  0.487499999999999988897770
							6618          RVIHERBS_SYN  0.467314814814815 			# without protected*() is 0.420583333333329978387383
							6618        RVITALLWOOD_DD  0.082500000000000003885781
							6618       RVITALLWOOD_RIP  0.197500000000000008881784
							6618       RVITALLWOOD_SYN  0.131833333333329999481620
							6618        RVITOTALVEG_DD  0.375000000000000000000000
							6618       RVITOTALVEG_RIP  0.770000000000000017763568
							6618       RVITOTALVEG_SYN  0.620416666666659954998408
							6618      RVIUNDERSTORY_DD  0.197500000000000008881784
							6618     RVIUNDERSTORY_RIP  0.392500000000000015543122
							6618     RVIUNDERSTORY_SYN  0.343703703703704 			# without protected*() is 0.309333333333330018355412
							6618           RVIWOODY_DD  0.112500000000000002775558
							6618          RVIWOODY_RIP  0.282499999999999973354647
							6618          RVIWOODY_SYN  0.199833333333330004366601
							6618          RVNCANBIG_DD 10.000000000000000000000000
							6618         RVNCANBIG_RIP 10.000000000000000000000000
							6618         RVNCANBIG_SYN 10.000000000000000000000000
							6618        RVNCANSMALL_DD 10.000000000000000000000000
							6618       RVNCANSMALL_RIP 10.000000000000000000000000
							6618       RVNCANSMALL_SYN 10.000000000000000000000000
							6618         RVNGNDBARE_DD 10.000000000000000000000000
							6618        RVNGNDBARE_RIP 10.000000000000000000000000
							6618        RVNGNDBARE_SYN  9.000000000000000000000000
							6618    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6618   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6618   RVNGNDINUNDATED_SYN  9.000000000000000000000000
							6618         RVNGNDNONW_DD 10.000000000000000000000000
							6618        RVNGNDNONW_RIP 10.000000000000000000000000
							6618        RVNGNDNONW_SYN  9.000000000000000000000000
							6618        RVNGNDWOODY_DD 10.000000000000000000000000
							6618       RVNGNDWOODY_RIP 10.000000000000000000000000
							6618       RVNGNDWOODY_SYN  9.000000000000000000000000
							6618     RVNUNDERSTORY_RIP  8.000000000000000000000000
							6618         RVNUNDNONW_DD 10.000000000000000000000000
							6618        RVNUNDNONW_RIP 10.000000000000000000000000
							6618        RVNUNDNONW_SYN  9.000000000000000000000000
							6618        RVNUNDWOODY_DD 10.000000000000000000000000
							6618       RVNUNDWOODY_RIP 10.000000000000000000000000
							6618       RVNUNDWOODY_SYN  9.000000000000000000000000
							6618          RVVCANBIG_DD  0.000000000000000000000000
							6618         RVVCANBIG_RIP  0.000000000000000000000000
							6618         RVVCANBIG_SYN  0.000000000000000000000000
							6618        RVVCANSMALL_DD  0.000000000000000000000000
							6618       RVVCANSMALL_RIP  0.000000000000000000000000
							6618       RVVCANSMALL_SYN  0.000000000000000000000000
							6618         RVVGNDBARE_DD  0.381777671902820026073044
							6618        RVVGNDBARE_RIP  0.312233564978860012040229
							6618        RVVGNDBARE_SYN  0.320624117167290001439994
							6618    RVVGNDINUNDATED_DD  0.157532045219029998017746
							6618   RVVGNDINUNDATED_RIP  0.046113321546169996900666
							6618   RVVGNDINUNDATED_SYN  0.043199877112589997463221
							6618         RVVGNDNONW_DD  0.322848048221159977799744
							6618        RVVGNDNONW_RIP  0.331517622638559994463492
							6618        RVVGNDNONW_SYN  0.333276730716839997281653
							6618        RVVGNDWOODY_DD  0.238412487054200011860061
							6618       RVVGNDWOODY_RIP  0.168914058542550010733052
							6618       RVVGNDWOODY_SYN  0.160174780029840008177544
							6618         RVVUNDNONW_DD  0.227318303510980007642317
							6618        RVVUNDNONW_RIP  0.228764799156970011217993
							6618        RVVUNDNONW_SYN  0.235591749053779997424840
							6618        RVVUNDWOODY_DD  0.168737139427629995180880
							6618       RVVUNDWOODY_RIP  0.170151076660970013154639
							6618       RVVUNDWOODY_SYN  0.112908039167460005902655
							6618   RVFPCANDECIDUOUS_DD                          NA
							6618  RVFPCANDECIDUOUS_RIP                          NA
							6618      RVFPCANMIXED_RIP                          NA
							6618   RVFPUNDDECIDUOUS_DD                          NA
							6618      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6618          RVNCANOPY_DD  0.000000000000000000000000
							6618         RVNCANOPY_RIP  0.000000000000000000000000
							6618      RVNUNDERSTORY_DD  0.000000000000000000000000
							6618        RVFPCANNONE_DD                          NA
							6618       RVFPCANNONE_RIP                          NA
							6618        RVFPUNDNONE_DD                          NA
							6618       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6618 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6647         RVFCCANBIG_DD  0.010000000000000000208167
							6647        RVFCCANBIG_RIP  0.035000000000000003330669
							6647        RVFCCANBIG_SYN  0.010000000000000000208167
							6647       RVFCCANSMALL_DD  0.020000000000000000416334
							6647      RVFCCANSMALL_RIP  0.170000000000000012212453
							6647      RVFCCANSMALL_SYN  0.141166666666660001183686
							6647        RVFCGNDBARE_DD  0.592522522522520023180448
							6647       RVFCGNDBARE_RIP  0.185939893439890008286497
							6647       RVFCGNDBARE_SYN  0.470447841244300002383483
							6647   RVFCGNDINUNDATED_DD  0.016666666666660001627776
							6647  RVFCGNDINUNDATED_RIP  0.246199633699629993888180
							6647  RVFCGNDINUNDATED_SYN  0.001179941002939999912177
							6647        RVFCGNDNONW_DD  0.273191763191759973139483
							6647       RVFCGNDNONW_RIP  0.495149872649869982055293
							6647       RVFCGNDNONW_SYN  0.422808933839020018030652
							6647       RVFCGNDWOODY_DD  0.117619047619040004160063
							6647      RVFCGNDWOODY_RIP  0.072710600210599996007232
							6647      RVFCGNDWOODY_SYN  0.105563283913719996309766
							6647        RVFCUNDNONW_DD  0.040000000000000000832667
							6647       RVFCUNDNONW_RIP  0.172499999999999986677324
							6647       RVFCUNDNONW_SYN  0.166166666666659995632571
							6647       RVFCUNDWOODY_DD  0.080000000000000001665335
							6647      RVFCUNDWOODY_RIP  0.172499999999999986677324
							6647      RVFCUNDWOODY_SYN  0.114666666666660005402534
							6647         RVFPCANBIG_DD  0.200000000000000011102230
							6647        RVFPCANBIG_RIP  0.299999999999999988897770
							6647        RVFPCANBIG_SYN  0.200000000000000011102230
							6647       RVFPCANSMALL_DD  0.400000000000000022204460
							6647      RVFPCANSMALL_RIP  0.500000000000000000000000
							6647      RVFPCANSMALL_SYN  0.400000000000000022204460
							6647        RVFPGNDBARE_DD  1.000000000000000000000000
							6647       RVFPGNDBARE_RIP  0.599999999999999977795540
							6647       RVFPGNDBARE_SYN  1.000000000000000000000000
							6647   RVFPGNDINUNDATED_DD  0.200000000000000011102230
							6647  RVFPGNDINUNDATED_RIP  0.500000000000000000000000
							6647  RVFPGNDINUNDATED_SYN  0.200000000000000011102230
							6647        RVFPGNDNONW_DD  1.000000000000000000000000
							6647       RVFPGNDNONW_RIP  0.800000000000000044408921
							6647       RVFPGNDNONW_SYN  1.000000000000000000000000
							6647       RVFPGNDWOODY_DD  0.800000000000000044408921
							6647      RVFPGNDWOODY_RIP  0.699999999999999955591079
							6647      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6647  RVFPUNDDECIDUOUS_RIP  0.500000000000000000000000
							6647        RVFPUNDNONW_DD  0.800000000000000044408921
							6647       RVFPUNDNONW_RIP  0.800000000000000044408921
							6647       RVFPUNDNONW_SYN  1.000000000000000000000000
							6647       RVFPUNDWOODY_DD  0.800000000000000044408921
							6647      RVFPUNDWOODY_RIP  0.800000000000000044408921
							6647      RVFPUNDWOODY_SYN  1.000000000000000000000000
							6647          RVICANOPY_DD  0.029999999999999998889777
							6647         RVICANOPY_RIP  0.204999999999999987787547
							6647         RVICANOPY_SYN  0.151166666666667 			# without protected*() is 0.075583333333330005032735
							6647          RVICANUND_DD  0.149999999999999994448885
							6647         RVICANUND_RIP  0.550000000000000044408921
							6647         RVICANUND_SYN  0.432			 			# without protected*() is 0.215999999999999997557509
							6647          RVIGROUND_DD  0.325000000000000011102230
							6647         RVIGROUND_RIP  0.667499999999999982236432
							6647         RVIGROUND_SYN  0.449333333333333 			# without protected*() is 0.224666666666659992079857
							6647           RVIHERBS_DD  0.275000000000000022204460
							6647          RVIHERBS_RIP  0.574999999999999955591079
							6647          RVIHERBS_SYN  0.5265			 			# without protected*() is 0.263249999999999984012788
							6647        RVITALLWOOD_DD  0.110000000000000000555112
							6647       RVITALLWOOD_RIP  0.377500000000000002220446
							6647       RVITALLWOOD_SYN  0.265833333333333 			# without protected*() is 0.132916666666659993856214
							6647        RVITOTALVEG_DD  0.465000000000000024424907
							6647       RVITOTALVEG_RIP  1.007500000000000062172489
							6647       RVITOTALVEG_SYN  0.880333333333333 			# without protected*() is 0.440166666666659989193278
							6647      RVIUNDERSTORY_DD  0.119999999999999995559108
							6647     RVIUNDERSTORY_RIP  0.344999999999999973354647
							6647     RVIUNDERSTORY_SYN  0.280833333333333 			# without protected*() is 0.140416666666660000517552
							6647           RVIWOODY_DD  0.190000000000000002220446
							6647          RVIWOODY_RIP  0.432499999999999995559108
							6647          RVIWOODY_SYN  0.353833333333333 			# without protected*() is 0.176916666666660005180489
							6647          RVNCANBIG_DD  5.000000000000000000000000
							6647         RVNCANBIG_RIP 10.000000000000000000000000
							6647         RVNCANBIG_SYN  5.000000000000000000000000
							6647        RVNCANSMALL_DD  5.000000000000000000000000
							6647       RVNCANSMALL_RIP 10.000000000000000000000000
							6647       RVNCANSMALL_SYN  5.000000000000000000000000
							6647         RVNGNDBARE_DD  5.000000000000000000000000
							6647        RVNGNDBARE_RIP 10.000000000000000000000000
							6647        RVNGNDBARE_SYN  5.000000000000000000000000
							6647    RVNGNDINUNDATED_DD  5.000000000000000000000000
							6647   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6647   RVNGNDINUNDATED_SYN  5.000000000000000000000000
							6647         RVNGNDNONW_DD  5.000000000000000000000000
							6647        RVNGNDNONW_RIP 10.000000000000000000000000
							6647        RVNGNDNONW_SYN  5.000000000000000000000000
							6647        RVNGNDWOODY_DD  5.000000000000000000000000
							6647       RVNGNDWOODY_RIP 10.000000000000000000000000
							6647       RVNGNDWOODY_SYN  5.000000000000000000000000
							6647     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6647         RVNUNDNONW_DD  5.000000000000000000000000
							6647        RVNUNDNONW_RIP 10.000000000000000000000000
							6647        RVNUNDNONW_SYN  5.000000000000000000000000
							6647        RVNUNDWOODY_DD  5.000000000000000000000000
							6647       RVNUNDWOODY_RIP 10.000000000000000000000000
							6647       RVNUNDWOODY_SYN  5.000000000000000000000000
							6647          RVVCANBIG_DD  0.022360679774990000617807
							6647         RVVCANBIG_RIP  0.078351061823620005153401
							6647         RVVCANBIG_SYN  0.022360679774990000617807
							6647        RVVCANSMALL_DD  0.027386127875250000640595
							6647       RVVCANSMALL_RIP  0.235643327462870005861717
							6647       RVVCANSMALL_SYN  0.227473746958969991194266
							6647         RVVGNDBARE_DD  0.254124117179669983812573
							6647        RVVGNDBARE_RIP  0.229548353686959999420125
							6647        RVVGNDBARE_SYN  0.257273099389110027068739
							6647    RVVGNDINUNDATED_DD  0.037267799624990001483571
							6647   RVVGNDINUNDATED_RIP  0.407396190663819990351868
							6647   RVVGNDINUNDATED_SYN  0.002638428292029999962243
							6647         RVVGNDNONW_DD  0.240727869513129993928047
							6647        RVVGNDNONW_RIP  0.356601713596290004204548
							6647        RVVGNDNONW_SYN  0.315680525516429999477452
							6647        RVVGNDWOODY_DD  0.168993955264969991647561
							6647       RVVGNDWOODY_RIP  0.085226209563009996950100
							6647       RVVGNDWOODY_SYN  0.105948197785350001209892
							6647         RVVUNDNONW_DD  0.022360679774990000617807
							6647        RVVUNDNONW_RIP  0.179679498614119997901284
							6647        RVVUNDNONW_SYN  0.193526842812270005733666
							6647        RVVUNDWOODY_DD  0.097467943448080004986700
							6647       RVVUNDWOODY_RIP  0.179679498614119997901284
							6647       RVVUNDWOODY_SYN  0.096136477063709993573504
							6647   RVFPCANDECIDUOUS_DD  0.400000000000000022204460
							6647  RVFPCANDECIDUOUS_RIP  0.400000000000000022204460
							6647      RVFPCANMIXED_RIP  0.100000000000000005551115
							6647   RVFPUNDDECIDUOUS_DD  0.800000000000000044408921
							6647      RVFPUNDMIXED_RIP  0.299999999999999988897770
							6647          RVNCANOPY_DD  5.000000000000000000000000
							6647         RVNCANOPY_RIP 10.000000000000000000000000
							6647      RVNUNDERSTORY_DD  5.000000000000000000000000
							6647        RVFPCANNONE_DD  0.599999999999999977795540
							6647       RVFPCANNONE_RIP  0.500000000000000000000000
							6647        RVFPUNDNONE_DD  0.200000000000000011102230
							6647       RVFPUNDNONE_RIP  0.200000000000000011102230
							6647 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							7431         RVFCCANBIG_DD                          NA
							7431        RVFCCANBIG_RIP  0.074999999999999997224442
							7431        RVFCCANBIG_SYN                          NA
							7431       RVFCCANSMALL_DD                          NA
							7431      RVFCCANSMALL_RIP  0.243749999999999994448885
							7431      RVFCCANSMALL_SYN                          NA
							7431        RVFCGNDBARE_DD                          NA
							7431       RVFCGNDBARE_RIP  0.267482517482510007145180
							7431       RVFCGNDBARE_SYN                          NA
							7431   RVFCGNDINUNDATED_DD                          NA
							7431  RVFCGNDINUNDATED_RIP  0.118355481727570002736805
							7431  RVFCGNDINUNDATED_SYN                          NA
							7431        RVFCGNDNONW_DD                          NA
							7431       RVFCGNDNONW_RIP  0.263476349232160012014958
							7431       RVFCGNDNONW_SYN                          NA
							7431       RVFCGNDWOODY_DD                          NA
							7431      RVFCGNDWOODY_RIP  0.350685651557740007966402
							7431      RVFCGNDWOODY_SYN                          NA
							7431        RVFCUNDNONW_DD                          NA
							7431       RVFCUNDNONW_RIP  0.050000000000000002775558
							7431       RVFCUNDNONW_SYN                          NA
							7431       RVFCUNDWOODY_DD                          NA
							7431      RVFCUNDWOODY_RIP  0.168750000000000011102230
							7431      RVFCUNDWOODY_SYN                          NA
							7431         RVFPCANBIG_DD                          NA
							7431        RVFPCANBIG_RIP  0.500000000000000000000000
							7431        RVFPCANBIG_SYN                          NA
							7431       RVFPCANSMALL_DD                          NA
							7431      RVFPCANSMALL_RIP  0.750000000000000000000000
							7431      RVFPCANSMALL_SYN                          NA
							7431        RVFPGNDBARE_DD                          NA
							7431       RVFPGNDBARE_RIP  0.500000000000000000000000
							7431       RVFPGNDBARE_SYN                          NA
							7431   RVFPGNDINUNDATED_DD                          NA
							7431  RVFPGNDINUNDATED_RIP  0.250000000000000000000000
							7431  RVFPGNDINUNDATED_SYN                          NA
							7431        RVFPGNDNONW_DD                          NA
							7431       RVFPGNDNONW_RIP  0.875000000000000000000000
							7431       RVFPGNDNONW_SYN                          NA
							7431       RVFPGNDWOODY_DD                          NA
							7431      RVFPGNDWOODY_RIP  1.000000000000000000000000
							7431      RVFPGNDWOODY_SYN                          NA
							7431  RVFPUNDDECIDUOUS_RIP  0.250000000000000000000000
							7431        RVFPUNDNONW_DD                          NA
							7431       RVFPUNDNONW_RIP  0.500000000000000000000000
							7431       RVFPUNDNONW_SYN                          NA
							7431       RVFPUNDWOODY_DD                          NA
							7431      RVFPUNDWOODY_RIP  0.875000000000000000000000
							7431      RVFPUNDWOODY_SYN                          NA
							7431          RVICANOPY_DD                          NA
							7431         RVICANOPY_RIP  0.318749999999999977795540
							7431         RVICANOPY_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431          RVICANUND_DD                          NA
							7431         RVICANUND_RIP  0.537499999999999977795540
							7431         RVICANUND_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431          RVIGROUND_DD                          NA
							7431         RVIGROUND_RIP  0.396874999999999977795540
							7431         RVIGROUND_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431           RVIHERBS_DD                          NA
							7431          RVIHERBS_RIP  0.234375000000000000000000
							7431          RVIHERBS_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431        RVITALLWOOD_DD                          NA
							7431       RVITALLWOOD_RIP  0.487499999999999988897770
							7431       RVITALLWOOD_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431        RVITOTALVEG_DD                          NA
							7431       RVITOTALVEG_RIP  0.871874999999999955591079
							7431       RVITOTALVEG_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431      RVIUNDERSTORY_DD                          NA
							7431     RVIUNDERSTORY_RIP  0.218750000000000000000000
							7431     RVIUNDERSTORY_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431           RVIWOODY_DD                          NA
							7431          RVIWOODY_RIP  0.637499999999999955591079
							7431          RVIWOODY_SYN  NA 			# without protected*() is 0.000000000000000000000000
							7431          RVNCANBIG_DD  0.000000000000000000000000
							7431         RVNCANBIG_RIP  8.000000000000000000000000
							7431         RVNCANBIG_SYN  0.000000000000000000000000
							7431        RVNCANSMALL_DD  0.000000000000000000000000
							7431       RVNCANSMALL_RIP  8.000000000000000000000000
							7431       RVNCANSMALL_SYN  0.000000000000000000000000
							7431         RVNGNDBARE_DD  0.000000000000000000000000
							7431        RVNGNDBARE_RIP  8.000000000000000000000000
							7431        RVNGNDBARE_SYN  0.000000000000000000000000
							7431    RVNGNDINUNDATED_DD  0.000000000000000000000000
							7431   RVNGNDINUNDATED_RIP  8.000000000000000000000000
							7431   RVNGNDINUNDATED_SYN  0.000000000000000000000000
							7431         RVNGNDNONW_DD  0.000000000000000000000000
							7431        RVNGNDNONW_RIP  8.000000000000000000000000
							7431        RVNGNDNONW_SYN  0.000000000000000000000000
							7431        RVNGNDWOODY_DD  0.000000000000000000000000
							7431       RVNGNDWOODY_RIP  8.000000000000000000000000
							7431       RVNGNDWOODY_SYN  0.000000000000000000000000
							7431     RVNUNDERSTORY_RIP  8.000000000000000000000000
							7431         RVNUNDNONW_DD  0.000000000000000000000000
							7431        RVNUNDNONW_RIP  8.000000000000000000000000
							7431        RVNUNDNONW_SYN  0.000000000000000000000000
							7431        RVNUNDWOODY_DD  0.000000000000000000000000
							7431       RVNUNDWOODY_RIP  8.000000000000000000000000
							7431       RVNUNDWOODY_SYN  0.000000000000000000000000
							7431          RVVCANBIG_DD                          NA
							7431         RVVCANBIG_RIP  0.110194633003859995823426
							7431         RVVCANBIG_SYN                          NA
							7431        RVVCANSMALL_DD                          NA
							7431       RVVCANSMALL_RIP  0.231744163125749996767766
							7431       RVVCANSMALL_SYN                          NA
							7431         RVVGNDBARE_DD                          NA
							7431        RVVGNDBARE_RIP  0.328926883830099991978813
							7431        RVVGNDBARE_SYN                          NA
							7431    RVVGNDINUNDATED_DD                          NA
							7431   RVVGNDINUNDATED_RIP  0.254171823058120027738482
							7431   RVVGNDINUNDATED_SYN                          NA
							7431         RVVGNDNONW_DD                          NA
							7431        RVVGNDNONW_RIP  0.217047545251019990963925
							7431        RVVGNDNONW_SYN                          NA
							7431        RVVGNDWOODY_DD                          NA
							7431       RVVGNDWOODY_RIP  0.309207520425549997611370
							7431       RVVGNDWOODY_SYN                          NA
							7431         RVVUNDNONW_DD                          NA
							7431        RVVUNDNONW_RIP  0.084515425472850000399028
							7431        RVVUNDNONW_SYN                          NA
							7431        RVVUNDWOODY_DD                          NA
							7431       RVVUNDWOODY_RIP  0.113192314226710000202303
							7431       RVVUNDWOODY_SYN                          NA
							7431   RVFPCANDECIDUOUS_DD                          NA
							7431  RVFPCANDECIDUOUS_RIP                           0	# SAS calculates as NA
							7431      RVFPCANMIXED_RIP  1.000000000000000000000000
							7431   RVFPUNDDECIDUOUS_DD                          NA
							7431      RVFPUNDMIXED_RIP  0.625000000000000000000000
							7431          RVNCANOPY_DD  0.000000000000000000000000
							7431         RVNCANOPY_RIP  6.000000000000000000000000
							7431      RVNUNDERSTORY_DD  0.000000000000000000000000
							7431        RVFPCANNONE_DD                          NA
							7431       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							7431        RVFPUNDNONE_DD                          NA
							7431       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							7431 RVFPUNDCONIFEROUS_RIP  0.125000000000000000000000
							1000222         RVFCCANBIG_DD  0.200000000000000011102230
							1000222        RVFCCANBIG_RIP  0.275000000000000022204460
							1000222        RVFCCANBIG_SYN  0.265166666666660000295508
							1000222       RVFCCANSMALL_DD  0.000000000000000000000000
							1000222      RVFCCANSMALL_RIP  0.080000000000000001665335
							1000222      RVFCCANSMALL_SYN  0.056000000000000001165734
							1000222        RVFCGNDBARE_DD  0.461778583173930001759544
							1000222       RVFCGNDBARE_RIP  0.036363636363629998904390
							1000222       RVFCGNDBARE_SYN  0.219565593281730003782926
							1000222   RVFCGNDINUNDATED_DD  0.046511627906969998935693
							1000222  RVFCGNDINUNDATED_RIP  0.066666666666660004403333
							1000222  RVFCGNDINUNDATED_SYN  0.034099616858230000215890
							1000222        RVFCGNDNONW_DD  0.480281217490510003997883
							1000222       RVFCGNDNONW_RIP  0.581818181818180013564756
							1000222       RVFCGNDNONW_SYN  0.575428225324620012948174
							1000222       RVFCGNDWOODY_DD  0.011428571428569999149372
							1000222      RVFCGNDWOODY_RIP  0.315151515151510019929759
							1000222      RVFCGNDWOODY_SYN  0.170906564535389993153558
							1000222        RVFCUNDNONW_DD  0.050000000000000002775558
							1000222       RVFCUNDNONW_RIP  0.610555555555549966939566
							1000222       RVFCUNDNONW_SYN  0.457166666666660004292311
							1000222       RVFCUNDWOODY_DD  0.040000000000000000832667
							1000222      RVFCUNDWOODY_RIP  0.229444444444440009966968
							1000222      RVFCUNDWOODY_SYN  0.172666666666660001405731
							1000222         RVFPCANBIG_DD  0.800000000000000044408921
							1000222        RVFPCANBIG_RIP  1.000000000000000000000000
							1000222        RVFPCANBIG_SYN  1.000000000000000000000000
							1000222       RVFPCANSMALL_DD  0.000000000000000000000000
							1000222      RVFPCANSMALL_RIP  0.800000000000000044408921
							1000222      RVFPCANSMALL_SYN  0.800000000000000044408921
							1000222        RVFPGNDBARE_DD  1.000000000000000000000000
							1000222       RVFPGNDBARE_RIP  0.400000000000000022204460
							1000222       RVFPGNDBARE_SYN  1.000000000000000000000000
							1000222   RVFPGNDINUNDATED_DD  0.200000000000000011102230
							1000222  RVFPGNDINUNDATED_RIP  0.200000000000000011102230
							1000222  RVFPGNDINUNDATED_SYN  0.400000000000000022204460
							1000222        RVFPGNDNONW_DD  1.000000000000000000000000
							1000222       RVFPGNDNONW_RIP  1.000000000000000000000000
							1000222       RVFPGNDNONW_SYN  1.000000000000000000000000
							1000222       RVFPGNDWOODY_DD  0.200000000000000011102230
							1000222      RVFPGNDWOODY_RIP  1.000000000000000000000000
							1000222      RVFPGNDWOODY_SYN  1.000000000000000000000000
							1000222  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							1000222        RVFPUNDNONW_DD  1.000000000000000000000000
							1000222       RVFPUNDNONW_RIP  1.000000000000000000000000
							1000222       RVFPUNDNONW_SYN  1.000000000000000000000000
							1000222       RVFPUNDWOODY_DD  0.800000000000000044408921
							1000222      RVFPUNDWOODY_RIP  1.000000000000000000000000
							1000222      RVFPUNDWOODY_SYN  1.000000000000000000000000
							1000222          RVICANOPY_DD  0.200000000000000011102230
							1000222         RVICANOPY_RIP  0.354999999999999982236432
							1000222         RVICANOPY_SYN  0.321166666666659994522348
							1000222          RVICANUND_DD  0.289999999999999980015986
							1000222         RVICANUND_RIP  1.219999999999999973354647
							1000222         RVICANUND_SYN  0.950999999999999956479257
							1000222          RVIGROUND_DD  0.500000000000000000000000
							1000222         RVIGROUND_RIP  0.349999999999999977795540
							1000222         RVIGROUND_SYN  0.419666666666660026496771
							1000222           RVIHERBS_DD  0.489999999999999991118216
							1000222          RVIHERBS_RIP  0.839999999999999968913755
							1000222          RVIHERBS_SYN  0.754833333333330025460839
							1000222        RVITALLWOOD_DD  0.239999999999999991118216
							1000222       RVITALLWOOD_RIP  0.589999999999999968913755
							1000222       RVITALLWOOD_SYN  0.493833333333330015690876
							1000222        RVITOTALVEG_DD  0.739999999999999991118216
							1000222       RVITOTALVEG_RIP  1.560000000000000053290705
							1000222       RVITOTALVEG_SYN  1.348000000000000087041485
							1000222      RVIUNDERSTORY_DD  0.089999999999999996669331
							1000222     RVIUNDERSTORY_RIP  0.864999999999999991118216
							1000222     RVIUNDERSTORY_SYN  0.629833333333330025460839
							1000222           RVIWOODY_DD  0.250000000000000000000000
							1000222          RVIWOODY_RIP  0.719999999999999973354647
							1000222          RVIWOODY_SYN  0.593166666666659958551122
							1000222          RVNCANBIG_DD  5.000000000000000000000000
							1000222         RVNCANBIG_RIP  5.000000000000000000000000
							1000222         RVNCANBIG_SYN  5.000000000000000000000000
							1000222        RVNCANSMALL_DD  5.000000000000000000000000
							1000222       RVNCANSMALL_RIP  5.000000000000000000000000
							1000222       RVNCANSMALL_SYN  5.000000000000000000000000
							1000222         RVNGNDBARE_DD  5.000000000000000000000000
							1000222        RVNGNDBARE_RIP  5.000000000000000000000000
							1000222        RVNGNDBARE_SYN  5.000000000000000000000000
							1000222    RVNGNDINUNDATED_DD  5.000000000000000000000000
							1000222   RVNGNDINUNDATED_RIP  5.000000000000000000000000
							1000222   RVNGNDINUNDATED_SYN  5.000000000000000000000000
							1000222         RVNGNDNONW_DD  5.000000000000000000000000
							1000222        RVNGNDNONW_RIP  5.000000000000000000000000
							1000222        RVNGNDNONW_SYN  5.000000000000000000000000
							1000222        RVNGNDWOODY_DD  5.000000000000000000000000
							1000222       RVNGNDWOODY_RIP  5.000000000000000000000000
							1000222       RVNGNDWOODY_SYN  5.000000000000000000000000
							1000222     RVNUNDERSTORY_RIP  5.000000000000000000000000
							1000222         RVNUNDNONW_DD  5.000000000000000000000000
							1000222        RVNUNDNONW_RIP  5.000000000000000000000000
							1000222        RVNUNDNONW_SYN  5.000000000000000000000000
							1000222        RVNUNDWOODY_DD  5.000000000000000000000000
							1000222       RVNUNDWOODY_RIP  5.000000000000000000000000
							1000222       RVNUNDWOODY_SYN  5.000000000000000000000000
							1000222          RVVCANBIG_DD  0.111803398874980003396828
							1000222         RVVCANBIG_RIP  0.188745860881760013638697
							1000222         RVVCANBIG_SYN  0.181211585967579996703236
							1000222        RVVCANSMALL_DD  0.000000000000000000000000
							1000222       RVVCANSMALL_RIP  0.097467943448080004986700
							1000222       RVVCANSMALL_SYN  0.072548834128000005971870
							1000222         RVVGNDBARE_DD  0.226314050663340010638436
							1000222        RVVGNDBARE_RIP  0.049792959773190002825682
							1000222        RVVGNDBARE_SYN  0.076160849525050006159077
							1000222    RVVGNDINUNDATED_DD  0.104003161744170000146958
							1000222   RVVGNDINUNDATED_RIP  0.149071198499979989948727
							1000222   RVVGNDINUNDATED_SYN  0.051196399570270001433414
							1000222         RVVGNDNONW_DD  0.200716619121389994173654
							1000222        RVVGNDNONW_RIP  0.234872921504789999858076
							1000222        RVVGNDNONW_SYN  0.118112850811099998060705
							1000222        RVVGNDWOODY_DD  0.025555062599989998572969
							1000222       RVVGNDWOODY_RIP  0.144297799403739990253825
							1000222       RVVGNDWOODY_SYN  0.142305613633199989909173
							1000222         RVVUNDNONW_DD  0.000000000000000000000000
							1000222        RVVUNDNONW_RIP  0.240011895281340009722371
							1000222        RVVUNDNONW_SYN  0.164210654005420009227123
							1000222        RVVUNDWOODY_DD  0.022360679774990000617807
							1000222       RVVUNDWOODY_RIP  0.214641532920169991749404
							1000222       RVVUNDWOODY_SYN  0.143515388257379999936703
							1000222   RVFPCANDECIDUOUS_DD  0.800000000000000044408921
							1000222  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							1000222      RVFPCANMIXED_RIP                           0	# SAS calculates as NA
							1000222   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							1000222      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							1000222          RVNCANOPY_DD  5.000000000000000000000000
							1000222         RVNCANOPY_RIP  5.000000000000000000000000
							1000222      RVNUNDERSTORY_DD  3.000000000000000000000000
							1000222        RVFPCANNONE_DD  0.200000000000000011102230
							1000222       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							1000222        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							1000222       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							1000222 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6362   RVFPCANBROADLEAF_DD         NA		# These 81 rows should have but were not included in the SAS output
							6362  RVFPCANBROADLEAF_RIP         NA
							6362  RVFPCANCONIFEROUS_DD         NA
							6362 RVFPCANCONIFEROUS_RIP         NA
							6362       RVFPCANMIXED_DD         NA
							6362   RVFPUNDBROADLEAF_DD         NA
							6362  RVFPUNDBROADLEAF_RIP          0
							6362  RVFPUNDCONIFEROUS_DD         NA
							6362       RVFPUNDMIXED_DD         NA
							6396   RVFPCANBROADLEAF_DD          0
							6396  RVFPCANBROADLEAF_RIP          0
							6396  RVFPCANCONIFEROUS_DD          0
							6396 RVFPCANCONIFEROUS_RIP          0
							6396       RVFPCANMIXED_DD          0
							6396   RVFPUNDBROADLEAF_DD          0
							6396  RVFPUNDBROADLEAF_RIP          0
							6396  RVFPUNDCONIFEROUS_DD          0
							6396       RVFPUNDMIXED_DD          0
							6449   RVFPCANBROADLEAF_DD          0
							6449  RVFPCANBROADLEAF_RIP          0
							6449  RVFPCANCONIFEROUS_DD          0
							6449 RVFPCANCONIFEROUS_RIP          0
							6449       RVFPCANMIXED_DD          0
							6449   RVFPUNDBROADLEAF_DD          0
							6449  RVFPUNDBROADLEAF_RIP          0
							6449  RVFPUNDCONIFEROUS_DD          0
							6449       RVFPUNDMIXED_DD          0
							6518   RVFPCANBROADLEAF_DD          0
							6518  RVFPCANBROADLEAF_RIP          0
							6518  RVFPCANCONIFEROUS_DD          0
							6518 RVFPCANCONIFEROUS_RIP          0
							6518       RVFPCANMIXED_DD          0
							6518   RVFPUNDBROADLEAF_DD          0
							6518  RVFPUNDBROADLEAF_RIP          0
							6518  RVFPUNDCONIFEROUS_DD          0
							6518       RVFPUNDMIXED_DD          0
							6530   RVFPCANBROADLEAF_DD         NA
							6530  RVFPCANBROADLEAF_RIP          0
							6530  RVFPCANCONIFEROUS_DD         NA
							6530 RVFPCANCONIFEROUS_RIP          0
							6530       RVFPCANMIXED_DD         NA
							6530   RVFPUNDBROADLEAF_DD         NA
							6530  RVFPUNDBROADLEAF_RIP          0
							6530  RVFPUNDCONIFEROUS_DD         NA
							6530       RVFPUNDMIXED_DD         NA
							6618   RVFPCANBROADLEAF_DD         NA
							6618  RVFPCANBROADLEAF_RIP         NA
							6618  RVFPCANCONIFEROUS_DD         NA
							6618 RVFPCANCONIFEROUS_RIP         NA
							6618       RVFPCANMIXED_DD         NA
							6618   RVFPUNDBROADLEAF_DD         NA
							6618  RVFPUNDBROADLEAF_RIP          0
							6618  RVFPUNDCONIFEROUS_DD         NA
							6618       RVFPUNDMIXED_DD         NA
							6647   RVFPCANBROADLEAF_DD          0
							6647  RVFPCANBROADLEAF_RIP          0
							6647  RVFPCANCONIFEROUS_DD          0
							6647 RVFPCANCONIFEROUS_RIP          0
							6647       RVFPCANMIXED_DD          0
							6647   RVFPUNDBROADLEAF_DD          0
							6647  RVFPUNDBROADLEAF_RIP          0
							6647  RVFPUNDCONIFEROUS_DD          0
							6647       RVFPUNDMIXED_DD          0
							7431   RVFPCANBROADLEAF_DD         NA
							7431  RVFPCANBROADLEAF_RIP          0
							7431  RVFPCANCONIFEROUS_DD         NA
							7431 RVFPCANCONIFEROUS_RIP          0
							7431       RVFPCANMIXED_DD         NA
							7431   RVFPUNDBROADLEAF_DD         NA
							7431  RVFPUNDBROADLEAF_RIP          0
							7431  RVFPUNDCONIFEROUS_DD         NA
							7431       RVFPUNDMIXED_DD         NA
							1000222   RVFPCANBROADLEAF_DD          0
							1000222  RVFPCANBROADLEAF_RIP          0
							1000222  RVFPCANCONIFEROUS_DD          0
							1000222 RVFPCANCONIFEROUS_RIP          0
							1000222       RVFPCANMIXED_DD          0
							1000222   RVFPUNDBROADLEAF_DD          0
							1000222  RVFPUNDBROADLEAF_RIP          0
							1000222  RVFPUNDCONIFEROUS_DD          0
							1000222       RVFPUNDMIXED_DD          0
						 ")		
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(rc)						
}


nlaRiparianVegetationTest.expectedResultsWithDrawDown <- function()
# These values were calculated metsRiparianVegetationTest.sas on 20 Nov 2013,
# with the exception of 91 zero values which I did not have time to correct in 
# the SAS program -- these rows are appended at the end.
{
	tc <- textConnection("	 SITE             METRIC                      VALUE
							6362         RVFCCANBIG_DD  0.000000000000000000000000
							6362        RVFCCANBIG_RIP  0.000000000000000000000000
							6362        RVFCCANBIG_SYN  0.000000000000000000000000
							6362       RVFCCANSMALL_DD  0.000000000000000000000000
							6362      RVFCCANSMALL_RIP  0.000000000000000000000000
							6362      RVFCCANSMALL_SYN  0.000000000000000000000000
							6362        RVFCGNDBARE_DD  0.945945945945939947741010
							6362       RVFCGNDBARE_RIP  0.000000000000000000000000
							6362       RVFCGNDBARE_SYN  0.945945945945939947741010
							6362   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6362  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6362  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6362        RVFCGNDNONW_DD  0.054054054054049997801723
							6362       RVFCGNDNONW_RIP  0.303030303030299985600493
							6362       RVFCGNDNONW_SYN  0.054054054054049997801723
							6362       RVFCGNDWOODY_DD  0.000000000000000000000000
							6362      RVFCGNDWOODY_RIP  0.696969696969690022392285
							6362      RVFCGNDWOODY_SYN  0.000000000000000000000000
							6362        RVFCUNDNONW_DD  0.000000000000000000000000
							6362       RVFCUNDNONW_RIP  0.250000000000000000000000
							6362       RVFCUNDNONW_SYN  0.000000000000000000000000
							6362       RVFCUNDWOODY_DD  0.000000000000000000000000
							6362      RVFCUNDWOODY_RIP  0.574999999999999955591079
							6362      RVFCUNDWOODY_SYN  0.000000000000000000000000
							6362         RVFPCANBIG_DD  0.000000000000000000000000
							6362        RVFPCANBIG_RIP  0.000000000000000000000000
							6362        RVFPCANBIG_SYN  0.000000000000000000000000
							6362       RVFPCANSMALL_DD  0.000000000000000000000000
							6362      RVFPCANSMALL_RIP  0.000000000000000000000000
							6362      RVFPCANSMALL_SYN  0.000000000000000000000000
							6362        RVFPGNDBARE_DD  1.000000000000000000000000
							6362       RVFPGNDBARE_RIP  0.000000000000000000000000
							6362       RVFPGNDBARE_SYN  1.000000000000000000000000
							6362   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6362  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6362  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6362        RVFPGNDNONW_DD  1.000000000000000000000000
							6362       RVFPGNDNONW_RIP  1.000000000000000000000000
							6362       RVFPGNDNONW_SYN  1.000000000000000000000000
							6362       RVFPGNDWOODY_DD  0.000000000000000000000000
							6362      RVFPGNDWOODY_RIP  1.000000000000000000000000
							6362      RVFPGNDWOODY_SYN  0.000000000000000000000000
							6362  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6362        RVFPUNDNONW_DD  0.000000000000000000000000
							6362       RVFPUNDNONW_RIP  1.000000000000000000000000
							6362       RVFPUNDNONW_SYN  0.000000000000000000000000
							6362       RVFPUNDWOODY_DD  0.000000000000000000000000
							6362      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6362      RVFPUNDWOODY_SYN  0.000000000000000000000000
							6362          RVICANOPY_DD  0.000000000000000000000000
							6362         RVICANOPY_RIP  0.000000000000000000000000
							6362         RVICANOPY_SYN  0.000000000000000000000000
							6362          RVICANUND_DD  0.000000000000000000000000
							6362         RVICANUND_RIP  0.824999999999999955591079
							6362         RVICANUND_SYN  0.000000000000000000000000
							6362          RVIGROUND_DD  0.050000000000000002775558
							6362         RVIGROUND_RIP  0.824999999999999955591079
							6362         RVIGROUND_SYN  0.050000000000000002775558
							6362           RVIHERBS_DD  0.050000000000000002775558
							6362          RVIHERBS_RIP  0.500000000000000000000000
							6362          RVIHERBS_SYN  0.050000000000000002775558
							6362        RVITALLWOOD_DD  0.000000000000000000000000
							6362       RVITALLWOOD_RIP  0.574999999999999955591079
							6362       RVITALLWOOD_SYN  0.000000000000000000000000
							6362        RVITOTALVEG_DD  0.050000000000000002775558
							6362       RVITOTALVEG_RIP  1.649999999999999911182158
							6362       RVITOTALVEG_SYN  0.050000000000000002775558
							6362      RVIUNDERSTORY_DD  0.000000000000000000000000
							6362     RVIUNDERSTORY_RIP  0.824999999999999955591079
							6362     RVIUNDERSTORY_SYN  0.000000000000000000000000
							6362           RVIWOODY_DD  0.000000000000000000000000
							6362          RVIWOODY_RIP  1.149999999999999911182158
							6362          RVIWOODY_SYN  0.000000000000000000000000
							6362          RVNCANBIG_DD  1.000000000000000000000000
							6362         RVNCANBIG_RIP  1.000000000000000000000000
							6362         RVNCANBIG_SYN  1.000000000000000000000000
							6362        RVNCANSMALL_DD  1.000000000000000000000000
							6362       RVNCANSMALL_RIP  1.000000000000000000000000
							6362       RVNCANSMALL_SYN  1.000000000000000000000000
							6362         RVNGNDBARE_DD  1.000000000000000000000000
							6362        RVNGNDBARE_RIP  1.000000000000000000000000
							6362        RVNGNDBARE_SYN  1.000000000000000000000000
							6362    RVNGNDINUNDATED_DD  1.000000000000000000000000
							6362   RVNGNDINUNDATED_RIP  1.000000000000000000000000
							6362   RVNGNDINUNDATED_SYN  1.000000000000000000000000
							6362         RVNGNDNONW_DD  1.000000000000000000000000
							6362        RVNGNDNONW_RIP  1.000000000000000000000000
							6362        RVNGNDNONW_SYN  1.000000000000000000000000
							6362        RVNGNDWOODY_DD  1.000000000000000000000000
							6362       RVNGNDWOODY_RIP  1.000000000000000000000000
							6362       RVNGNDWOODY_SYN  1.000000000000000000000000
							6362     RVNUNDERSTORY_RIP  1.000000000000000000000000
							6362         RVNUNDNONW_DD  1.000000000000000000000000
							6362        RVNUNDNONW_RIP  1.000000000000000000000000
							6362        RVNUNDNONW_SYN  1.000000000000000000000000
							6362        RVNUNDWOODY_DD  1.000000000000000000000000
							6362       RVNUNDWOODY_RIP  1.000000000000000000000000
							6362       RVNUNDWOODY_SYN  1.000000000000000000000000
							6362          RVVCANBIG_DD                          NA
							6362         RVVCANBIG_RIP                          NA
							6362         RVVCANBIG_SYN                          NA
							6362        RVVCANSMALL_DD                          NA
							6362       RVVCANSMALL_RIP                          NA
							6362       RVVCANSMALL_SYN                          NA
							6362         RVVGNDBARE_DD                          NA
							6362        RVVGNDBARE_RIP                          NA
							6362        RVVGNDBARE_SYN                          NA
							6362    RVVGNDINUNDATED_DD                          NA
							6362   RVVGNDINUNDATED_RIP                          NA
							6362   RVVGNDINUNDATED_SYN                          NA
							6362         RVVGNDNONW_DD                          NA
							6362        RVVGNDNONW_RIP                          NA
							6362        RVVGNDNONW_SYN                          NA
							6362        RVVGNDWOODY_DD                          NA
							6362       RVVGNDWOODY_RIP                          NA
							6362       RVVGNDWOODY_SYN                          NA
							6362         RVVUNDNONW_DD                          NA
							6362        RVVUNDNONW_RIP                          NA
							6362        RVVUNDNONW_SYN                          NA
							6362        RVVUNDWOODY_DD                          NA
							6362       RVVUNDWOODY_RIP                          NA
							6362       RVVUNDWOODY_SYN                          NA
							6362   RVFPCANDECIDUOUS_DD                          NA
							6362  RVFPCANDECIDUOUS_RIP                          NA
							6362      RVFPCANMIXED_RIP                          NA
							6362   RVFPUNDDECIDUOUS_DD                          NA
							6362      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6362          RVNCANOPY_DD  0.000000000000000000000000
							6362         RVNCANOPY_RIP  0.000000000000000000000000
							6362      RVNUNDERSTORY_DD  0.000000000000000000000000
							6362        RVFPCANNONE_DD                          NA
							6362       RVFPCANNONE_RIP                          NA
							6362        RVFPUNDNONE_DD                          NA
							6362       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6362 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6396         RVFCCANBIG_DD  0.010000000000000000208167
							6396        RVFCCANBIG_RIP  0.059999999999999997779554
							6396        RVFCCANBIG_SYN  0.038999999999999999944489
							6396       RVFCCANSMALL_DD  0.000000000000000000000000
							6396      RVFCCANSMALL_RIP  0.144999999999999990007993
							6396      RVFCCANSMALL_SYN  0.066666666666660004403333
							6396        RVFCGNDBARE_DD  0.375853697853689994179405
							6396       RVFCGNDBARE_RIP  0.183905723905719992172791
							6396       RVFCGNDBARE_SYN  0.244424234346090007985453
							6396   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6396  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6396  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6396        RVFCGNDNONW_DD  0.580760058760049990844720
							6396       RVFCGNDNONW_RIP  0.462332852332849986165542
							6396       RVFCGNDNONW_SYN  0.554528859471799973057671
							6396       RVFCGNDWOODY_DD  0.043386243386240003205856
							6396      RVFCGNDWOODY_RIP  0.353761423761420001898870
							6396      RVFCGNDWOODY_SYN  0.201046906182090007186858
							6396        RVFCUNDNONW_DD  0.050000000000000002775558
							6396       RVFCUNDNONW_RIP  0.174999999999999988897770
							6396       RVFCUNDNONW_SYN  0.101333333333330000147754
							6396       RVFCUNDWOODY_DD  0.005000000000000000104083
							6396      RVFCUNDWOODY_RIP  0.202500000000000013322676
							6396      RVFCUNDWOODY_SYN  0.088333333333330002479222
							6396         RVFPCANBIG_DD  0.200000000000000011102230
							6396        RVFPCANBIG_RIP  0.400000000000000022204460
							6396        RVFPCANBIG_SYN  0.599999999999999977795540
							6396       RVFPCANSMALL_DD  0.000000000000000000000000
							6396      RVFPCANSMALL_RIP  0.900000000000000022204460
							6396      RVFPCANSMALL_SYN  0.900000000000000022204460
							6396        RVFPGNDBARE_DD  0.900000000000000022204460
							6396       RVFPGNDBARE_RIP  0.900000000000000022204460
							6396       RVFPGNDBARE_SYN  0.900000000000000022204460
							6396   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6396  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6396  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6396        RVFPGNDNONW_DD  1.000000000000000000000000
							6396       RVFPGNDNONW_RIP  0.900000000000000022204460
							6396       RVFPGNDNONW_SYN  1.000000000000000000000000
							6396       RVFPGNDWOODY_DD  0.299999999999999988897770
							6396      RVFPGNDWOODY_RIP  0.900000000000000022204460
							6396      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6396  RVFPUNDDECIDUOUS_RIP  0.599999999999999977795540
							6396        RVFPUNDNONW_DD  0.200000000000000011102230
							6396       RVFPUNDNONW_RIP  0.599999999999999977795540
							6396       RVFPUNDNONW_SYN  0.599999999999999977795540
							6396       RVFPUNDWOODY_DD  0.100000000000000005551115
							6396      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6396      RVFPUNDWOODY_SYN  1.000000000000000000000000
							6396          RVICANOPY_DD  0.010000000000000000208167
							6396         RVICANOPY_RIP  0.204999999999999987787547
							6396         RVICANOPY_SYN  0.105666666666659997408928
							6396          RVICANUND_DD  0.065000000000000002220446
							6396         RVICANUND_RIP  0.582500000000000017763568
							6396         RVICANUND_SYN  0.295333333333330005920914
							6396          RVIGROUND_DD  0.405000000000000026645353
							6396         RVIGROUND_RIP  0.630000000000000004440892
							6396         RVIGROUND_SYN  0.518333333333329981940096
							6396           RVIHERBS_DD  0.419999999999999984456878
							6396          RVIHERBS_RIP  0.522499999999999964472863
							6396          RVIHERBS_SYN  0.483499999999999985345056
							6396        RVITALLWOOD_DD  0.014999999999999999444888
							6396       RVITALLWOOD_RIP  0.407499999999999973354647
							6396       RVITALLWOOD_SYN  0.194000000000000005773160
							6396        RVITOTALVEG_DD  0.469999999999999973354647
							6396       RVITOTALVEG_RIP  1.212499999999999911182158
							6396       RVITOTALVEG_SYN  0.813666666666659987861010
							6396      RVIUNDERSTORY_DD  0.055000000000000000277556
							6396     RVIUNDERSTORY_RIP  0.377500000000000002220446
							6396     RVIUNDERSTORY_SYN  0.189666666666659988749188
							6396           RVIWOODY_DD  0.050000000000000002775558
							6396          RVIWOODY_RIP  0.689999999999999946709295
							6396          RVIWOODY_SYN  0.330166666666660002515954
							6396          RVNCANBIG_DD 10.000000000000000000000000
							6396         RVNCANBIG_RIP 10.000000000000000000000000
							6396         RVNCANBIG_SYN 10.000000000000000000000000
							6396        RVNCANSMALL_DD 10.000000000000000000000000
							6396       RVNCANSMALL_RIP 10.000000000000000000000000
							6396       RVNCANSMALL_SYN 10.000000000000000000000000
							6396         RVNGNDBARE_DD 10.000000000000000000000000
							6396        RVNGNDBARE_RIP 10.000000000000000000000000
							6396        RVNGNDBARE_SYN 10.000000000000000000000000
							6396    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6396   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6396   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6396         RVNGNDNONW_DD 10.000000000000000000000000
							6396        RVNGNDNONW_RIP 10.000000000000000000000000
							6396        RVNGNDNONW_SYN 10.000000000000000000000000
							6396        RVNGNDWOODY_DD 10.000000000000000000000000
							6396       RVNGNDWOODY_RIP 10.000000000000000000000000
							6396       RVNGNDWOODY_SYN 10.000000000000000000000000
							6396     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6396         RVNUNDNONW_DD 10.000000000000000000000000
							6396        RVNUNDNONW_RIP 10.000000000000000000000000
							6396        RVNUNDNONW_SYN 10.000000000000000000000000
							6396        RVNUNDWOODY_DD 10.000000000000000000000000
							6396       RVNUNDWOODY_RIP 10.000000000000000000000000
							6396       RVNUNDWOODY_SYN 10.000000000000000000000000
							6396          RVVCANBIG_DD  0.021081851067779999037155
							6396         RVVCANBIG_RIP  0.102198064778369998584218
							6396         RVVCANBIG_SYN  0.064808359466889994826566
							6396        RVVCANSMALL_DD  0.000000000000000000000000
							6396       RVVCANSMALL_RIP  0.111679103784999997350802
							6396       RVVCANSMALL_SYN  0.059275460748210002936975
							6396         RVVGNDBARE_DD  0.343959754793390026783584
							6396        RVVGNDBARE_RIP  0.296809325540589974412597
							6396        RVVGNDBARE_SYN  0.174853558544070003533122
							6396    RVVGNDINUNDATED_DD  0.000000000000000000000000
							6396   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6396   RVVGNDINUNDATED_SYN  0.000000000000000000000000
							6396         RVVGNDNONW_DD  0.349201271910390020902781
							6396        RVVGNDNONW_RIP  0.308979151503730009586945
							6396        RVVGNDNONW_SYN  0.226428136349729997034430
							6396        RVVGNDWOODY_DD  0.090543377455459997382725
							6396       RVVGNDWOODY_RIP  0.265292610762579983418874
							6396       RVVGNDWOODY_SYN  0.150298739575700002202296
							6396         RVVUNDNONW_DD  0.105409255338939994439684
							6396        RVVUNDNONW_RIP  0.232139804619729994694666
							6396        RVVUNDNONW_SYN  0.119971189957220003563521
							6396        RVVUNDWOODY_DD  0.015811388300840001353009
							6396       RVVUNDWOODY_RIP  0.164337897164479995737452
							6396       RVVUNDWOODY_SYN  0.058060207150169997059042
							6396   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6396  RVFPCANDECIDUOUS_RIP  0.599999999999999977795540
							6396      RVFPCANMIXED_RIP  0.400000000000000022204460
							6396   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6396      RVFPUNDMIXED_RIP  0.400000000000000022204460
							6396          RVNCANOPY_DD 10.000000000000000000000000
							6396         RVNCANOPY_RIP 10.000000000000000000000000
							6396      RVNUNDERSTORY_DD 10.000000000000000000000000
							6396        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6396       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6396        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6396       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6396 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6449         RVFCCANBIG_DD  0.093750000000000000000000
							6449        RVFCCANBIG_RIP  0.156250000000000000000000
							6449        RVFCCANBIG_SYN  0.155361111111110006266500
							6449       RVFCCANSMALL_DD  0.066666666666660004403333
							6449      RVFCCANSMALL_RIP  0.125000000000000000000000
							6449      RVFCCANSMALL_SYN  0.122236111111110004601166
							6449        RVFCGNDBARE_DD  0.316035661725310002001521
							6449       RVFCGNDBARE_RIP  0.069755229666569995572445
							6449       RVFCGNDBARE_SYN  0.074063805689980000135364
							6449   RVFCGNDINUNDATED_DD  0.046780150803129999514951
							6449  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6449  RVFCGNDINUNDATED_SYN  0.002217421353869999944841
							6449        RVFCGNDNONW_DD  0.595106358037389981952003
							6449       RVFCGNDNONW_RIP  0.788864667313240008894581
							6449       RVFCGNDNONW_SYN  0.784683080231180052344087
							6449       RVFCGNDWOODY_DD  0.042077829434149997822612
							6449      RVFCGNDWOODY_RIP  0.141380103020170011518530
							6449      RVFCGNDWOODY_SYN  0.139035692724950005194628
							6449        RVFCUNDNONW_DD  0.102083333333330000813888
							6449       RVFCUNDNONW_RIP  0.066666666666660004403333
							6449       RVFCUNDNONW_SYN  0.066736111111109996940627
							6449       RVFCUNDWOODY_DD  0.114583333333329998038330
							6449      RVFCUNDWOODY_RIP  0.193750000000000005551115
							6449      RVFCUNDWOODY_SYN  0.192624999999999990674127
							6449         RVFPCANBIG_DD  0.250000000000000000000000
							6449        RVFPCANBIG_RIP  0.583333333333330039671694
							6449        RVFPCANBIG_SYN  0.583333333333330039671694
							6449       RVFPCANSMALL_DD  0.250000000000000000000000
							6449      RVFPCANSMALL_RIP  0.416666666666660023832236
							6449      RVFPCANSMALL_SYN  0.416666666666660023832236
							6449        RVFPGNDBARE_DD  1.000000000000000000000000
							6449       RVFPGNDBARE_RIP  0.916666666666659968321085
							6449       RVFPGNDBARE_SYN  1.000000000000000000000000
							6449   RVFPGNDINUNDATED_DD  0.416666666666660023832236
							6449  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6449  RVFPGNDINUNDATED_SYN  0.416666666666660023832236
							6449        RVFPGNDNONW_DD  1.000000000000000000000000
							6449       RVFPGNDNONW_RIP  1.000000000000000000000000
							6449       RVFPGNDNONW_SYN  1.000000000000000000000000
							6449       RVFPGNDWOODY_DD  0.500000000000000000000000
							6449      RVFPGNDWOODY_RIP  0.916666666666659968321085
							6449      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6449  RVFPUNDDECIDUOUS_RIP  0.909090909090899956801479
							6449        RVFPUNDNONW_DD  0.500000000000000000000000
							6449       RVFPUNDNONW_RIP  0.333333333333329984160542
							6449       RVFPUNDNONW_SYN  0.500000000000000000000000
							6449       RVFPUNDWOODY_DD  0.416666666666660023832236
							6449      RVFPUNDWOODY_RIP  1.000000000000000000000000
							6449      RVFPUNDWOODY_SYN  1.000000000000000000000000
							6449          RVICANOPY_DD  0.172916666666660001627776
							6449         RVICANOPY_RIP  0.281250000000000000000000
							6449         RVICANOPY_SYN  0.277597222222219996989878
							6449          RVICANUND_DD  0.389583333333330006365003
							6449         RVICANUND_RIP  0.541666666666659968321085
							6449         RVICANUND_SYN  0.536958333333329984604632
							6449          RVIGROUND_DD  0.458333333333329984160542
							6449         RVIGROUND_RIP  0.764583333333329950853852
							6449         RVIGROUND_SYN  0.758791666666659980755583
							6449           RVIHERBS_DD  0.497916666666660012730006
							6449          RVIHERBS_RIP  0.691666666666659990525545
							6449          RVIHERBS_SYN  0.687486111111110020921444
							6449        RVITALLWOOD_DD  0.287499999999989985788318
							6449       RVITALLWOOD_RIP  0.474999999999999977795540
							6449       RVITALLWOOD_SYN  0.470222222222219987664005
							6449        RVITOTALVEG_DD  0.810416666666660012730006
							6449       RVITOTALVEG_RIP  1.306249999999999911182158
							6449       RVITOTALVEG_SYN  1.294194444444439984209794
							6449      RVIUNDERSTORY_DD  0.216666666666660012730006
							6449     RVIUNDERSTORY_RIP  0.260416666666660023832236
							6449     RVIUNDERSTORY_SYN  0.259361111111109987614753
							6449           RVIWOODY_DD  0.312500000000000000000000
							6449          RVIWOODY_RIP  0.614583333333330039671694
							6449          RVIWOODY_SYN  0.606708333333329963288350
							6449          RVNCANBIG_DD 12.000000000000000000000000
							6449         RVNCANBIG_RIP 12.000000000000000000000000
							6449         RVNCANBIG_SYN 12.000000000000000000000000
							6449        RVNCANSMALL_DD 12.000000000000000000000000
							6449       RVNCANSMALL_RIP 12.000000000000000000000000
							6449       RVNCANSMALL_SYN 12.000000000000000000000000
							6449         RVNGNDBARE_DD 12.000000000000000000000000
							6449        RVNGNDBARE_RIP 12.000000000000000000000000
							6449        RVNGNDBARE_SYN 12.000000000000000000000000
							6449    RVNGNDINUNDATED_DD 12.000000000000000000000000
							6449   RVNGNDINUNDATED_RIP 12.000000000000000000000000
							6449   RVNGNDINUNDATED_SYN 12.000000000000000000000000
							6449         RVNGNDNONW_DD 12.000000000000000000000000
							6449        RVNGNDNONW_RIP 12.000000000000000000000000
							6449        RVNGNDNONW_SYN 12.000000000000000000000000
							6449        RVNGNDWOODY_DD 12.000000000000000000000000
							6449       RVNGNDWOODY_RIP 12.000000000000000000000000
							6449       RVNGNDWOODY_SYN 12.000000000000000000000000
							6449     RVNUNDERSTORY_RIP 11.000000000000000000000000
							6449         RVNUNDNONW_DD 12.000000000000000000000000
							6449        RVNUNDNONW_RIP 12.000000000000000000000000
							6449        RVNUNDNONW_SYN 12.000000000000000000000000
							6449        RVNUNDWOODY_DD 12.000000000000000000000000
							6449       RVNUNDWOODY_RIP 12.000000000000000000000000
							6449       RVNUNDWOODY_SYN 12.000000000000000000000000
							6449          RVVCANBIG_DD  0.208382569939389999946755
							6449         RVVCANBIG_RIP  0.179052112779789995844482
							6449         RVVCANBIG_SYN  0.178764472230879989389507
							6449        RVVCANSMALL_DD  0.154233196128059996121706
							6449       RVVCANSMALL_RIP  0.221820976137389991311721
							6449       RVVCANSMALL_SYN  0.216175859694860000859151
							6449         RVVGNDBARE_DD  0.267349341182989974630146
							6449        RVVGNDBARE_RIP  0.040792521257720003313718
							6449        RVVGNDBARE_SYN  0.038562900575719996631996
							6449    RVVGNDINUNDATED_DD  0.078946707475039998302968
							6449   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6449   RVVGNDINUNDATED_SYN  0.006183799423889999730131
							6449         RVVGNDNONW_DD  0.257587557654409993190825
							6449        RVVGNDNONW_RIP  0.149444054998849995552490
							6449        RVVGNDNONW_SYN  0.145779195396189997779857
							6449        RVVGNDWOODY_DD  0.048690228200570000172132
							6449       RVVGNDWOODY_RIP  0.125707297804319995115918
							6449       RVVGNDWOODY_SYN  0.122489265296690005846436
							6449         RVVUNDNONW_DD  0.175310871069509988107171
							6449        RVVUNDNONW_RIP  0.111464085804539994595252
							6449        RVVUNDNONW_SYN  0.109957830721760005387999
							6449        RVVUNDWOODY_DD  0.181677265467850013180851
							6449       RVVUNDWOODY_RIP  0.155988417402170009840034
							6449       RVVUNDWOODY_SYN  0.152030900052280010248396
							6449   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6449  RVFPCANDECIDUOUS_RIP  0.888888888888879957761446
							6449      RVFPCANMIXED_RIP  0.111111111111109994720181
							6449   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6449      RVFPUNDMIXED_RIP  0.090909090909089995680148
							6449          RVNCANOPY_DD  5.000000000000000000000000
							6449         RVNCANOPY_RIP  9.000000000000000000000000
							6449      RVNUNDERSTORY_DD  5.000000000000000000000000
							6449        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6449       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6449        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6449       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6449 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6518         RVFCCANBIG_DD  0.000000000000000000000000
							6518        RVFCCANBIG_RIP  0.110000000000000000555112
							6518        RVFCCANBIG_SYN  0.109666666666660000961642
							6518       RVFCCANSMALL_DD  0.000000000000000000000000
							6518      RVFCCANSMALL_RIP  0.154999999999999998889777
							6518      RVFCCANSMALL_SYN  0.154666666666660013174095
							6518        RVFCGNDBARE_DD  0.043115843115839998234140
							6518       RVFCGNDBARE_RIP  0.128659488659479986916168
							6518       RVFCGNDBARE_SYN  0.128931989058290008554053
							6518   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6518  RVFCGNDINUNDATED_RIP  0.000000000000000000000000
							6518  RVFCGNDINUNDATED_SYN  0.000000000000000000000000
							6518        RVFCGNDNONW_DD  0.349476749476740011779441
							6518       RVFCGNDNONW_RIP  0.663769123769120006350875
							6518       RVFCGNDNONW_SYN  0.665545747687749988941164
							6518       RVFCGNDWOODY_DD  0.007407407407400000420861
							6518      RVFCGNDWOODY_RIP  0.207571387571379994962939
							6518      RVFCGNDWOODY_SYN  0.205522263253950010497562
							6518        RVFCUNDNONW_DD  0.010000000000000000208167
							6518       RVFCUNDNONW_RIP  0.044999999999999998334665
							6518       RVFCUNDNONW_SYN  0.044333333333329998093841
							6518       RVFCUNDWOODY_DD  0.025000000000000001387779
							6518      RVFCUNDWOODY_RIP  0.187500000000000000000000
							6518      RVFCUNDWOODY_SYN  0.187500000000000000000000
							6518         RVFPCANBIG_DD  0.000000000000000000000000
							6518        RVFPCANBIG_RIP  0.500000000000000000000000
							6518        RVFPCANBIG_SYN  0.500000000000000000000000
							6518       RVFPCANSMALL_DD  0.000000000000000000000000
							6518      RVFPCANSMALL_RIP  0.599999999999999977795540
							6518      RVFPCANSMALL_SYN  0.599999999999999977795540
							6518        RVFPGNDBARE_DD  0.299999999999999988897770
							6518       RVFPGNDBARE_RIP  0.900000000000000022204460
							6518       RVFPGNDBARE_SYN  0.900000000000000022204460
							6518   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6518  RVFPGNDINUNDATED_RIP  0.000000000000000000000000
							6518  RVFPGNDINUNDATED_SYN  0.000000000000000000000000
							6518        RVFPGNDNONW_DD  0.400000000000000022204460
							6518       RVFPGNDNONW_RIP  1.000000000000000000000000
							6518       RVFPGNDNONW_SYN  1.000000000000000000000000
							6518       RVFPGNDWOODY_DD  0.100000000000000005551115
							6518      RVFPGNDWOODY_RIP  1.000000000000000000000000
							6518      RVFPGNDWOODY_SYN  1.000000000000000000000000
							6518  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6518        RVFPUNDNONW_DD  0.200000000000000011102230
							6518       RVFPUNDNONW_RIP  0.900000000000000022204460
							6518       RVFPUNDNONW_SYN  0.900000000000000022204460
							6518       RVFPUNDWOODY_DD  0.100000000000000005551115
							6518      RVFPUNDWOODY_RIP  0.699999999999999955591079
							6518      RVFPUNDWOODY_SYN  0.699999999999999955591079
							6518          RVICANOPY_DD  0.000000000000000000000000
							6518         RVICANOPY_RIP  0.280000000000000026645353
							6518         RVICANOPY_SYN  0.279333333333329991710059
							6518          RVICANUND_DD  0.035000000000000003330669
							6518         RVICANUND_RIP  0.512499999999999955591079
							6518         RVICANUND_SYN  0.511166666666659996742794
							6518          RVIGROUND_DD  0.294999999999999984456878
							6518         RVIGROUND_RIP  0.729999999999999982236432
							6518         RVIGROUND_SYN  0.727999999999999980460075
							6518           RVIHERBS_DD  0.299999999999999988897770
							6518          RVIHERBS_RIP  0.612500000000000044408921
							6518          RVIHERBS_SYN  0.611833333333330009473627
							6518        RVITALLWOOD_DD  0.025000000000000001387779
							6518       RVITALLWOOD_RIP  0.467500000000000026645353
							6518       RVITALLWOOD_SYN  0.466833333333329991710059
							6518        RVITOTALVEG_DD  0.330000000000000015543122
							6518       RVITOTALVEG_RIP  1.242499999999999937827511
							6518       RVITOTALVEG_SYN  1.239166666666660088225171
							6518      RVIUNDERSTORY_DD  0.035000000000000003330669
							6518     RVIUNDERSTORY_RIP  0.232500000000000012212453
							6518     RVIUNDERSTORY_SYN  0.231833333333330005032735
							6518           RVIWOODY_DD  0.029999999999999998889777
							6518          RVIWOODY_RIP  0.630000000000000004440892
							6518          RVIWOODY_SYN  0.627333333333329967729242
							6518          RVNCANBIG_DD 10.000000000000000000000000
							6518         RVNCANBIG_RIP 10.000000000000000000000000
							6518         RVNCANBIG_SYN 10.000000000000000000000000
							6518        RVNCANSMALL_DD 10.000000000000000000000000
							6518       RVNCANSMALL_RIP 10.000000000000000000000000
							6518       RVNCANSMALL_SYN 10.000000000000000000000000
							6518         RVNGNDBARE_DD 10.000000000000000000000000
							6518        RVNGNDBARE_RIP 10.000000000000000000000000
							6518        RVNGNDBARE_SYN 10.000000000000000000000000
							6518    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6518   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6518   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6518         RVNGNDNONW_DD 10.000000000000000000000000
							6518        RVNGNDNONW_RIP 10.000000000000000000000000
							6518        RVNGNDNONW_SYN 10.000000000000000000000000
							6518        RVNGNDWOODY_DD 10.000000000000000000000000
							6518       RVNGNDWOODY_RIP 10.000000000000000000000000
							6518       RVNGNDWOODY_SYN 10.000000000000000000000000
							6518     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6518         RVNUNDNONW_DD 10.000000000000000000000000
							6518        RVNUNDNONW_RIP 10.000000000000000000000000
							6518        RVNUNDNONW_SYN 10.000000000000000000000000
							6518        RVNUNDWOODY_DD 10.000000000000000000000000
							6518       RVNUNDWOODY_RIP 10.000000000000000000000000
							6518       RVNUNDWOODY_SYN 10.000000000000000000000000
							6518          RVVCANBIG_DD  0.000000000000000000000000
							6518         RVVCANBIG_RIP  0.169640141999990001764331
							6518         RVVCANBIG_SYN  0.169774360058669998574388
							6518        RVVCANSMALL_DD  0.000000000000000000000000
							6518       RVVCANSMALL_RIP  0.170701168387590002106791
							6518       RVVCANSMALL_SYN  0.170932085278850010023888
							6518         RVVGNDBARE_DD  0.095246122398960006427693
							6518        RVVGNDBARE_RIP  0.122195434577600001158082
							6518        RVVGNDBARE_SYN  0.122472970067400002691649
							6518    RVVGNDINUNDATED_DD  0.000000000000000000000000
							6518   RVVGNDINUNDATED_RIP  0.000000000000000000000000
							6518   RVVGNDINUNDATED_SYN  0.000000000000000000000000
							6518         RVVGNDNONW_DD  0.457637906889599987980688
							6518        RVVGNDNONW_RIP  0.240420634161999990929814
							6518        RVVGNDNONW_SYN  0.241872514584139997051437
							6518        RVVGNDWOODY_DD  0.023424278964209999803758
							6518       RVVGNDWOODY_RIP  0.216161559629449995245665
							6518       RVVGNDWOODY_SYN  0.217805330122460005792817
							6518         RVVUNDNONW_DD  0.021081851067779999037155
							6518        RVVUNDNONW_RIP  0.015811388300840001353009
							6518        RVVUNDNONW_SYN  0.015717411906449998970636
							6518        RVVUNDWOODY_DD  0.079056941504200006765046
							6518       RVVUNDWOODY_RIP  0.180758433029029996763626
							6518       RVVUNDWOODY_SYN  0.180758433029029996763626
							6518   RVFPCANDECIDUOUS_DD  1.000000000000000000000000
							6518  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							6518      RVFPCANMIXED_RIP                           0	# SAS calculates as NA
							6518   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							6518      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6518          RVNCANOPY_DD  1.000000000000000000000000
							6518         RVNCANOPY_RIP 10.000000000000000000000000
							6518      RVNUNDERSTORY_DD  2.000000000000000000000000
							6518        RVFPCANNONE_DD                           0	# SAS calculates as NA
							6518       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6518        RVFPUNDNONE_DD                           0	# SAS calculates as NA
							6518       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6518 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6530         RVFCCANBIG_DD  0.000000000000000000000000
							6530        RVFCCANBIG_RIP  0.125000000000000000000000
							6530        RVFCCANBIG_SYN  0.125000000000000000000000
							6530       RVFCCANSMALL_DD  0.000000000000000000000000
							6530      RVFCCANSMALL_RIP  0.020000000000000000416334
							6530      RVFCCANSMALL_SYN  0.020000000000000000416334
							6530        RVFCGNDBARE_DD  0.000000000000000000000000
							6530       RVFCGNDBARE_RIP  0.283135733135729983356299
							6530       RVFCGNDBARE_SYN  0.283135733135729983356299
							6530   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							6530  RVFCGNDINUNDATED_RIP  0.005405405405400000307115
							6530  RVFCGNDINUNDATED_SYN  0.005405405405400000307115
							6530        RVFCGNDNONW_DD  0.000000000000000000000000
							6530       RVFCGNDNONW_RIP  0.670954928954920037931231
							6530       RVFCGNDNONW_SYN  0.670954928954920037931231
							6530       RVFCGNDWOODY_DD  0.000000000000000000000000
							6530      RVFCGNDWOODY_RIP  0.040503932503930001329806
							6530      RVFCGNDWOODY_SYN  0.040503932503930001329806
							6530        RVFCUNDNONW_DD  0.000000000000000000000000
							6530       RVFCUNDNONW_RIP  0.040000000000000000832667
							6530       RVFCUNDNONW_SYN  0.040000000000000000832667
							6530       RVFCUNDWOODY_DD  0.000000000000000000000000
							6530      RVFCUNDWOODY_RIP  0.025000000000000001387779
							6530      RVFCUNDWOODY_SYN  0.025000000000000001387779
							6530         RVFPCANBIG_DD  0.000000000000000000000000
							6530        RVFPCANBIG_RIP  0.900000000000000022204460
							6530        RVFPCANBIG_SYN  0.900000000000000022204460
							6530       RVFPCANSMALL_DD  0.000000000000000000000000
							6530      RVFPCANSMALL_RIP  0.400000000000000022204460
							6530      RVFPCANSMALL_SYN  0.400000000000000022204460
							6530        RVFPGNDBARE_DD  0.000000000000000000000000
							6530       RVFPGNDBARE_RIP  0.800000000000000044408921
							6530       RVFPGNDBARE_SYN  0.800000000000000044408921
							6530   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							6530  RVFPGNDINUNDATED_RIP  0.100000000000000005551115
							6530  RVFPGNDINUNDATED_SYN  0.100000000000000005551115
							6530        RVFPGNDNONW_DD  0.000000000000000000000000
							6530       RVFPGNDNONW_RIP  1.000000000000000000000000
							6530       RVFPGNDNONW_SYN  1.000000000000000000000000
							6530       RVFPGNDWOODY_DD  0.000000000000000000000000
							6530      RVFPGNDWOODY_RIP  0.500000000000000000000000
							6530      RVFPGNDWOODY_SYN  0.500000000000000000000000
							6530  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6530        RVFPUNDNONW_DD  0.000000000000000000000000
							6530       RVFPUNDNONW_RIP  0.400000000000000022204460
							6530       RVFPUNDNONW_SYN  0.400000000000000022204460
							6530       RVFPUNDWOODY_DD  0.000000000000000000000000
							6530      RVFPUNDWOODY_RIP  0.500000000000000000000000
							6530      RVFPUNDWOODY_SYN  0.500000000000000000000000
							6530          RVICANOPY_DD  0.000000000000000000000000
							6530         RVICANOPY_RIP  0.144999999999999990007993
							6530         RVICANOPY_SYN  0.144999999999999990007993
							6530          RVICANUND_DD  0.000000000000000000000000
							6530         RVICANUND_RIP  0.209999999999999992228439
							6530         RVICANUND_SYN  0.209999999999999992228439
							6530          RVIGROUND_DD  0.000000000000000000000000
							6530         RVIGROUND_RIP  0.484999999999999986677324
							6530         RVIGROUND_SYN  0.484999999999999986677324
							6530           RVIHERBS_DD  0.000000000000000000000000
							6530          RVIHERBS_RIP  0.494999999999999995559108
							6530          RVIHERBS_SYN  0.494999999999999995559108
							6530        RVITALLWOOD_DD  0.000000000000000000000000
							6530       RVITALLWOOD_RIP  0.170000000000000012212453
							6530       RVITALLWOOD_SYN  0.170000000000000012212453
							6530        RVITOTALVEG_DD  0.000000000000000000000000
							6530       RVITOTALVEG_RIP  0.689999999999999946709295
							6530       RVITOTALVEG_SYN  0.689999999999999946709295
							6530      RVIUNDERSTORY_DD  0.000000000000000000000000
							6530     RVIUNDERSTORY_RIP  0.065000000000000002220446
							6530     RVIUNDERSTORY_SYN  0.065000000000000002220446
							6530           RVIWOODY_DD  0.000000000000000000000000
							6530          RVIWOODY_RIP  0.195000000000000006661338
							6530          RVIWOODY_SYN  0.195000000000000006661338
							6530          RVNCANBIG_DD 10.000000000000000000000000
							6530         RVNCANBIG_RIP 10.000000000000000000000000
							6530         RVNCANBIG_SYN 10.000000000000000000000000
							6530        RVNCANSMALL_DD 10.000000000000000000000000
							6530       RVNCANSMALL_RIP 10.000000000000000000000000
							6530       RVNCANSMALL_SYN 10.000000000000000000000000
							6530         RVNGNDBARE_DD 10.000000000000000000000000
							6530        RVNGNDBARE_RIP 10.000000000000000000000000
							6530        RVNGNDBARE_SYN 10.000000000000000000000000
							6530    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6530   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6530   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6530         RVNGNDNONW_DD 10.000000000000000000000000
							6530        RVNGNDNONW_RIP 10.000000000000000000000000
							6530        RVNGNDNONW_SYN 10.000000000000000000000000
							6530        RVNGNDWOODY_DD 10.000000000000000000000000
							6530       RVNGNDWOODY_RIP 10.000000000000000000000000
							6530       RVNGNDWOODY_SYN 10.000000000000000000000000
							6530     RVNUNDERSTORY_RIP  9.000000000000000000000000
							6530         RVNUNDNONW_DD 10.000000000000000000000000
							6530        RVNUNDNONW_RIP 10.000000000000000000000000
							6530        RVNUNDNONW_SYN 10.000000000000000000000000
							6530        RVNUNDWOODY_DD 10.000000000000000000000000
							6530       RVNUNDWOODY_RIP 10.000000000000000000000000
							6530       RVNUNDWOODY_SYN 10.000000000000000000000000
							6530          RVVCANBIG_DD  0.000000000000000000000000
							6530         RVVCANBIG_RIP  0.108653373420040000318920
							6530         RVVCANBIG_SYN  0.108653373420040000318920
							6530        RVVCANSMALL_DD  0.000000000000000000000000
							6530       RVVCANSMALL_RIP  0.025819888974709999385215
							6530       RVVCANSMALL_SYN  0.025819888974709999385215
							6530         RVVGNDBARE_DD  0.000000000000000000000000
							6530        RVVGNDBARE_RIP  0.235351071611580003883901
							6530        RVVGNDBARE_SYN  0.235351071611580003883901
							6530    RVVGNDINUNDATED_DD  0.000000000000000000000000
							6530   RVVGNDINUNDATED_RIP  0.017093392757659999631192
							6530   RVVGNDINUNDATED_SYN  0.017093392757659999631192
							6530         RVVGNDNONW_DD  0.000000000000000000000000
							6530        RVVGNDNONW_RIP  0.244463064338319990831394
							6530        RVVGNDNONW_SYN  0.244463064338319990831394
							6530        RVVGNDWOODY_DD  0.000000000000000000000000
							6530       RVVGNDWOODY_RIP  0.049140532886990002936489
							6530       RVVGNDWOODY_SYN  0.049140532886990002936489
							6530         RVVUNDNONW_DD  0.000000000000000000000000
							6530        RVVUNDNONW_RIP  0.077459666924139997101761
							6530        RVVUNDNONW_SYN  0.077459666924139997101761
							6530        RVVUNDWOODY_DD  0.000000000000000000000000
							6530       RVVUNDWOODY_RIP  0.026352313834729999136863
							6530       RVVUNDWOODY_SYN  0.026352313834729999136863
							6530   RVFPCANDECIDUOUS_DD                          NA
							6530  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							6530      RVFPCANMIXED_RIP                           0	# SAS calculates as NA
							6530   RVFPUNDDECIDUOUS_DD                          NA
							6530      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6530          RVNCANOPY_DD  0.000000000000000000000000
							6530         RVNCANOPY_RIP 10.000000000000000000000000
							6530      RVNUNDERSTORY_DD  0.000000000000000000000000
							6530        RVFPCANNONE_DD                          NA
							6530       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							6530        RVFPUNDNONE_DD                          NA
							6530       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6530 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6618         RVFCCANBIG_DD  0.000000000000000000000000
							6618        RVFCCANBIG_RIP  0.000000000000000000000000
							6618        RVFCCANBIG_SYN  0.000000000000000000000000
							6618       RVFCCANSMALL_DD  0.000000000000000000000000
							6618      RVFCCANSMALL_RIP  0.000000000000000000000000
							6618      RVFCCANSMALL_SYN  0.000000000000000000000000
							6618        RVFCGNDBARE_DD  0.335073593073590025870345
							6618       RVFCGNDBARE_RIP  0.321797517297509994005367
							6618       RVFCGNDBARE_SYN  0.332855597028400007619808
							6618   RVFCGNDINUNDATED_DD  0.059090909090899999822621
							6618  RVFCGNDINUNDATED_RIP  0.027314814814810000481726
							6618  RVFCGNDINUNDATED_SYN  0.032581204198850001463317
							6618        RVFCGNDNONW_DD  0.301073593073589995672279
							6618       RVFCGNDNONW_RIP  0.470233007732999974681576
							6618       RVFCGNDNONW_SYN  0.473347861849980011683670
							6618       RVFCGNDWOODY_DD  0.104761904761900004334763
							6618      RVFCGNDWOODY_RIP  0.180654660154660001714078
							6618      RVFCGNDWOODY_SYN  0.161215336922759994164878
							6618        RVFCUNDNONW_DD  0.107499999999999998334665
							6618       RVFCUNDNONW_RIP  0.195000000000000006661338
							6618       RVFCUNDNONW_SYN  0.182499999999999995559108
							6618       RVFCUNDWOODY_DD  0.074999999999999997224442
							6618      RVFCUNDWOODY_RIP  0.197500000000000008881784
							6618      RVFCUNDWOODY_SYN  0.189333333333329995040728
							6618         RVFPCANBIG_DD  0.000000000000000000000000
							6618        RVFPCANBIG_RIP  0.000000000000000000000000
							6618        RVFPCANBIG_SYN  0.000000000000000000000000
							6618       RVFPCANSMALL_DD  0.000000000000000000000000
							6618      RVFPCANSMALL_RIP  0.000000000000000000000000
							6618      RVFPCANSMALL_SYN  0.000000000000000000000000
							6618        RVFPGNDBARE_DD  0.599999999999999977795540
							6618       RVFPGNDBARE_RIP  0.900000000000000022204460
							6618       RVFPGNDBARE_SYN  0.900000000000000022204460
							6618   RVFPGNDINUNDATED_DD  0.200000000000000011102230
							6618  RVFPGNDINUNDATED_RIP  0.299999999999999988897770
							6618  RVFPGNDINUNDATED_SYN  0.400000000000000022204460
							6618        RVFPGNDNONW_DD  0.699999999999999955591079
							6618       RVFPGNDNONW_RIP  0.900000000000000022204460
							6618       RVFPGNDNONW_SYN  0.900000000000000022204460
							6618       RVFPGNDWOODY_DD  0.200000000000000011102230
							6618      RVFPGNDWOODY_RIP  0.900000000000000022204460
							6618      RVFPGNDWOODY_SYN  0.900000000000000022204460
							6618  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							6618        RVFPUNDNONW_DD  0.200000000000000011102230
							6618       RVFPUNDNONW_RIP  0.599999999999999977795540
							6618       RVFPUNDNONW_SYN  0.599999999999999977795540
							6618       RVFPUNDWOODY_DD  0.200000000000000011102230
							6618      RVFPUNDWOODY_RIP  0.900000000000000022204460
							6618      RVFPUNDWOODY_SYN  0.900000000000000022204460
							6618          RVICANOPY_DD  0.000000000000000000000000
							6618         RVICANOPY_RIP  0.000000000000000000000000
							6618         RVICANOPY_SYN  0.000000000000000000000000
							6618          RVICANUND_DD  0.197500000000000008881784
							6618         RVICANUND_RIP  0.392500000000000015543122
							6618         RVICANUND_SYN  0.371833333333330018355412
							6618          RVIGROUND_DD  0.207499999999999990007993
							6618         RVIGROUND_RIP  0.392500000000000015543122
							6618         RVIGROUND_SYN  0.358916666666660000295508
							6618           RVIHERBS_DD  0.262500000000000011102230
							6618          RVIHERBS_RIP  0.487499999999999988897770
							6618          RVIHERBS_SYN  0.450583333333330005032735
							6618        RVITALLWOOD_DD  0.082500000000000003885781
							6618       RVITALLWOOD_RIP  0.197500000000000008881784
							6618       RVITALLWOOD_SYN  0.189333333333329995040728
							6618        RVITOTALVEG_DD  0.375000000000000000000000
							6618       RVITOTALVEG_RIP  0.770000000000000017763568
							6618       RVITOTALVEG_SYN  0.712916666666659981643761
							6618      RVIUNDERSTORY_DD  0.197500000000000008881784
							6618     RVIUNDERSTORY_RIP  0.392500000000000015543122
							6618     RVIUNDERSTORY_SYN  0.371833333333330018355412
							6618           RVIWOODY_DD  0.112500000000000002775558
							6618          RVIWOODY_RIP  0.282499999999999973354647
							6618          RVIWOODY_SYN  0.262333333333329976611026
							6618          RVNCANBIG_DD 10.000000000000000000000000
							6618         RVNCANBIG_RIP 10.000000000000000000000000
							6618         RVNCANBIG_SYN 10.000000000000000000000000
							6618        RVNCANSMALL_DD 10.000000000000000000000000
							6618       RVNCANSMALL_RIP 10.000000000000000000000000
							6618       RVNCANSMALL_SYN 10.000000000000000000000000
							6618         RVNGNDBARE_DD 10.000000000000000000000000
							6618        RVNGNDBARE_RIP 10.000000000000000000000000
							6618        RVNGNDBARE_SYN 10.000000000000000000000000
							6618    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6618   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6618   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6618         RVNGNDNONW_DD 10.000000000000000000000000
							6618        RVNGNDNONW_RIP 10.000000000000000000000000
							6618        RVNGNDNONW_SYN 10.000000000000000000000000
							6618        RVNGNDWOODY_DD 10.000000000000000000000000
							6618       RVNGNDWOODY_RIP 10.000000000000000000000000
							6618       RVNGNDWOODY_SYN 10.000000000000000000000000
							6618     RVNUNDERSTORY_RIP  8.000000000000000000000000
							6618         RVNUNDNONW_DD 10.000000000000000000000000
							6618        RVNUNDNONW_RIP 10.000000000000000000000000
							6618        RVNUNDNONW_SYN 10.000000000000000000000000
							6618        RVNUNDWOODY_DD 10.000000000000000000000000
							6618       RVNUNDWOODY_RIP 10.000000000000000000000000
							6618       RVNUNDWOODY_SYN 10.000000000000000000000000
							6618          RVVCANBIG_DD  0.000000000000000000000000
							6618         RVVCANBIG_RIP  0.000000000000000000000000
							6618         RVVCANBIG_SYN  0.000000000000000000000000
							6618        RVVCANSMALL_DD  0.000000000000000000000000
							6618       RVVCANSMALL_RIP  0.000000000000000000000000
							6618       RVVCANSMALL_SYN  0.000000000000000000000000
							6618         RVVGNDBARE_DD  0.381777671902820026073044
							6618        RVVGNDBARE_RIP  0.312233564978860012040229
							6618        RVVGNDBARE_SYN  0.310984634149210015241493
							6618    RVVGNDINUNDATED_DD  0.157532045219029998017746
							6618   RVVGNDINUNDATED_RIP  0.046113321546169996900666
							6618   RVVGNDINUNDATED_SYN  0.052089767450449998842377
							6618         RVVGNDNONW_DD  0.322848048221159977799744
							6618        RVVGNDNONW_RIP  0.331517622638559994463492
							6618        RVVGNDNONW_SYN  0.318702365409779975191640
							6618        RVVGNDWOODY_DD  0.238412487054200011860061
							6618       RVVGNDWOODY_RIP  0.168914058542550010733052
							6618       RVVGNDWOODY_SYN  0.151549390459159993849170
							6618         RVVUNDNONW_DD  0.227318303510980007642317
							6618        RVVUNDNONW_RIP  0.228764799156970011217993
							6618        RVVUNDNONW_SYN  0.226944614443019998528683
							6618        RVVUNDWOODY_DD  0.168737139427629995180880
							6618       RVVUNDWOODY_RIP  0.170151076660970013154639
							6618       RVVUNDWOODY_SYN  0.172321118558440011803157
							6618   RVFPCANDECIDUOUS_DD                          NA
							6618  RVFPCANDECIDUOUS_RIP                          NA
							6618      RVFPCANMIXED_RIP                          NA
							6618   RVFPUNDDECIDUOUS_DD                          NA
							6618      RVFPUNDMIXED_RIP                           0	# SAS calculates as NA
							6618          RVNCANOPY_DD  0.000000000000000000000000
							6618         RVNCANOPY_RIP  0.000000000000000000000000
							6618      RVNUNDERSTORY_DD  0.000000000000000000000000
							6618        RVFPCANNONE_DD                          NA
							6618       RVFPCANNONE_RIP                          NA
							6618        RVFPUNDNONE_DD                          NA
							6618       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							6618 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							6647         RVFCCANBIG_DD  0.005000000000000000104083
							6647        RVFCCANBIG_RIP  0.035000000000000003330669
							6647        RVFCCANBIG_SYN  0.029999999999999998889777
							6647       RVFCCANSMALL_DD  0.010000000000000000208167
							6647      RVFCCANSMALL_RIP  0.170000000000000012212453
							6647      RVFCCANSMALL_SYN  0.100583333333329999481620
							6647        RVFCGNDBARE_DD  0.296261261261260011590224
							6647       RVFCGNDBARE_RIP  0.185939893439890008286497
							6647       RVFCGNDBARE_SYN  0.252852125750349987498566
							6647   RVFCGNDINUNDATED_DD  0.008333333333330000813888
							6647  RVFCGNDINUNDATED_RIP  0.246199633699629993888180
							6647  RVFCGNDINUNDATED_SYN  0.246789604201099993519009
							6647        RVFCGNDNONW_DD  0.136595881595879986569742
							6647       RVFCGNDNONW_RIP  0.495149872649869982055293
							6647       RVFCGNDNONW_SYN  0.429362342377380024061040
							6647       RVFCGNDWOODY_DD  0.058809523809520002080031
							6647      RVFCGNDWOODY_RIP  0.072710600210599996007232
							6647      RVFCGNDWOODY_SYN  0.070995927671140005021932
							6647        RVFCUNDNONW_DD  0.020000000000000000416334
							6647       RVFCUNDNONW_RIP  0.172499999999999986677324
							6647       RVFCUNDNONW_SYN  0.158083333333329995040728
							6647       RVFCUNDWOODY_DD  0.040000000000000000832667
							6647      RVFCUNDWOODY_RIP  0.172499999999999986677324
							6647      RVFCUNDWOODY_SYN  0.112333333333329996039929
							6647         RVFPCANBIG_DD  0.100000000000000005551115
							6647        RVFPCANBIG_RIP  0.299999999999999988897770
							6647        RVFPCANBIG_SYN  0.200000000000000011102230
							6647       RVFPCANSMALL_DD  0.200000000000000011102230
							6647      RVFPCANSMALL_RIP  0.500000000000000000000000
							6647      RVFPCANSMALL_SYN  0.400000000000000022204460
							6647        RVFPGNDBARE_DD  0.500000000000000000000000
							6647       RVFPGNDBARE_RIP  0.599999999999999977795540
							6647       RVFPGNDBARE_SYN  0.699999999999999955591079
							6647   RVFPGNDINUNDATED_DD  0.100000000000000005551115
							6647  RVFPGNDINUNDATED_RIP  0.500000000000000000000000
							6647  RVFPGNDINUNDATED_SYN  0.599999999999999977795540
							6647        RVFPGNDNONW_DD  0.500000000000000000000000
							6647       RVFPGNDNONW_RIP  0.800000000000000044408921
							6647       RVFPGNDNONW_SYN  0.800000000000000044408921
							6647       RVFPGNDWOODY_DD  0.400000000000000022204460
							6647      RVFPGNDWOODY_RIP  0.699999999999999955591079
							6647      RVFPGNDWOODY_SYN  0.699999999999999955591079
							6647  RVFPUNDDECIDUOUS_RIP  0.500000000000000000000000
							6647        RVFPUNDNONW_DD  0.400000000000000022204460
							6647       RVFPUNDNONW_RIP  0.800000000000000044408921
							6647       RVFPUNDNONW_SYN  0.800000000000000044408921
							6647       RVFPUNDWOODY_DD  0.400000000000000022204460
							6647      RVFPUNDWOODY_RIP  0.800000000000000044408921
							6647      RVFPUNDWOODY_SYN  0.800000000000000044408921
							6647          RVICANOPY_DD  0.014999999999999999444888
							6647         RVICANOPY_RIP  0.204999999999999987787547
							6647         RVICANOPY_SYN  0.130583333333329998371397
							6647          RVICANUND_DD  0.074999999999999997224442
							6647         RVICANUND_RIP  0.550000000000000044408921
							6647         RVICANUND_SYN  0.401000000000000023092639
							6647          RVIGROUND_DD  0.162500000000000005551115
							6647         RVIGROUND_RIP  0.667499999999999982236432
							6647         RVIGROUND_SYN  0.614666666666660033158109
							6647           RVIHERBS_DD  0.137500000000000011102230
							6647          RVIHERBS_RIP  0.574999999999999955591079
							6647          RVIHERBS_SYN  0.508249999999999979571896
							6647        RVITALLWOOD_DD  0.055000000000000000277556
							6647       RVITALLWOOD_RIP  0.377500000000000002220446
							6647       RVITALLWOOD_SYN  0.242916666666660008289114
							6647        RVITOTALVEG_DD  0.232500000000000012212453
							6647       RVITOTALVEG_RIP  1.007500000000000062172489
							6647       RVITOTALVEG_SYN  0.805166666666660035822645
							6647      RVIUNDERSTORY_DD  0.059999999999999997779554
							6647     RVIUNDERSTORY_RIP  0.344999999999999973354647
							6647     RVIUNDERSTORY_SYN  0.270416666666659977202869
							6647           RVIWOODY_DD  0.095000000000000001110223
							6647          RVIWOODY_RIP  0.432499999999999995559108
							6647          RVIWOODY_SYN  0.296916666666660000739597
							6647          RVNCANBIG_DD 10.000000000000000000000000
							6647         RVNCANBIG_RIP 10.000000000000000000000000
							6647         RVNCANBIG_SYN 10.000000000000000000000000
							6647        RVNCANSMALL_DD 10.000000000000000000000000
							6647       RVNCANSMALL_RIP 10.000000000000000000000000
							6647       RVNCANSMALL_SYN 10.000000000000000000000000
							6647         RVNGNDBARE_DD 10.000000000000000000000000
							6647        RVNGNDBARE_RIP 10.000000000000000000000000
							6647        RVNGNDBARE_SYN 10.000000000000000000000000
							6647    RVNGNDINUNDATED_DD 10.000000000000000000000000
							6647   RVNGNDINUNDATED_RIP 10.000000000000000000000000
							6647   RVNGNDINUNDATED_SYN 10.000000000000000000000000
							6647         RVNGNDNONW_DD 10.000000000000000000000000
							6647        RVNGNDNONW_RIP 10.000000000000000000000000
							6647        RVNGNDNONW_SYN 10.000000000000000000000000
							6647        RVNGNDWOODY_DD 10.000000000000000000000000
							6647       RVNGNDWOODY_RIP 10.000000000000000000000000
							6647       RVNGNDWOODY_SYN 10.000000000000000000000000
							6647     RVNUNDERSTORY_RIP 10.000000000000000000000000
							6647         RVNUNDNONW_DD 10.000000000000000000000000
							6647        RVNUNDNONW_RIP 10.000000000000000000000000
							6647        RVNUNDNONW_SYN 10.000000000000000000000000
							6647        RVNUNDWOODY_DD 10.000000000000000000000000
							6647       RVNUNDWOODY_RIP 10.000000000000000000000000
							6647       RVNUNDWOODY_SYN 10.000000000000000000000000
							6647          RVVCANBIG_DD  0.015811388300840001353009
							6647         RVVCANBIG_RIP  0.078351061823620005153401
							6647         RVVCANBIG_SYN  0.078881063774660004073169
							6647        RVVCANSMALL_DD  0.021081851067779999037155
							6647       RVVCANSMALL_RIP  0.235643327462870005861717
							6647       RVVCANSMALL_SYN  0.173348490256109988294497
							6647         RVVGNDBARE_DD  0.355281362204739992183278
							6647        RVVGNDBARE_RIP  0.229548353686959999420125
							6647        RVVGNDBARE_SYN  0.288728346211290010092654
							6647    RVVGNDINUNDATED_DD  0.026352313834729999136863
							6647   RVVGNDINUNDATED_RIP  0.407396190663819990351868
							6647   RVVGNDINUNDATED_SYN  0.407004125142449990626403
							6647         RVVGNDNONW_DD  0.215608693220660008194045
							6647        RVVGNDNONW_RIP  0.356601713596290004204548
							6647        RVVGNDNONW_SYN  0.345894406432920020755262
							6647        RVVGNDWOODY_DD  0.128591268310539991182040
							6647       RVVGNDWOODY_RIP  0.085226209563009996950100
							6647       RVVGNDWOODY_SYN  0.087625596002209996115973
							6647         RVVUNDNONW_DD  0.025819888974709999385215
							6647        RVVUNDNONW_RIP  0.179679498614119997901284
							6647        RVVUNDNONW_SYN  0.158276817398900010447349
							6647        RVVUNDWOODY_DD  0.077459666924139997101761
							6647       RVVUNDWOODY_RIP  0.179679498614119997901284
							6647       RVVUNDWOODY_SYN  0.107508828233530001616813
							6647   RVFPCANDECIDUOUS_DD  0.400000000000000022204460
							6647  RVFPCANDECIDUOUS_RIP  0.400000000000000022204460
							6647      RVFPCANMIXED_RIP  0.100000000000000005551115
							6647   RVFPUNDDECIDUOUS_DD  0.800000000000000044408921
							6647      RVFPUNDMIXED_RIP  0.299999999999999988897770
							6647          RVNCANOPY_DD  5.000000000000000000000000
							6647         RVNCANOPY_RIP 10.000000000000000000000000
							6647      RVNUNDERSTORY_DD  5.000000000000000000000000
							6647        RVFPCANNONE_DD  0.599999999999999977795540
							6647       RVFPCANNONE_RIP  0.500000000000000000000000
							6647        RVFPUNDNONE_DD  0.200000000000000011102230
							6647       RVFPUNDNONE_RIP  0.200000000000000011102230
							6647 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates as NA
							7431         RVFCCANBIG_DD  0.000000000000000000000000
							7431        RVFCCANBIG_RIP  0.074999999999999997224442
							7431        RVFCCANBIG_SYN  0.074999999999999997224442
							7431       RVFCCANSMALL_DD  0.000000000000000000000000
							7431      RVFCCANSMALL_RIP  0.243749999999999994448885
							7431      RVFCCANSMALL_SYN  0.243749999999999994448885
							7431        RVFCGNDBARE_DD  0.000000000000000000000000
							7431       RVFCGNDBARE_RIP  0.267482517482510007145180
							7431       RVFCGNDBARE_SYN  0.267482517482510007145180
							7431   RVFCGNDINUNDATED_DD  0.000000000000000000000000
							7431  RVFCGNDINUNDATED_RIP  0.118355481727570002736805
							7431  RVFCGNDINUNDATED_SYN  0.118355481727570002736805
							7431        RVFCGNDNONW_DD  0.000000000000000000000000
							7431       RVFCGNDNONW_RIP  0.263476349232160012014958
							7431       RVFCGNDNONW_SYN  0.263476349232160012014958
							7431       RVFCGNDWOODY_DD  0.000000000000000000000000
							7431      RVFCGNDWOODY_RIP  0.350685651557740007966402
							7431      RVFCGNDWOODY_SYN  0.350685651557740007966402
							7431        RVFCUNDNONW_DD  0.000000000000000000000000
							7431       RVFCUNDNONW_RIP  0.050000000000000002775558
							7431       RVFCUNDNONW_SYN  0.050000000000000002775558
							7431       RVFCUNDWOODY_DD  0.000000000000000000000000
							7431      RVFCUNDWOODY_RIP  0.168750000000000011102230
							7431      RVFCUNDWOODY_SYN  0.168750000000000011102230
							7431         RVFPCANBIG_DD  0.000000000000000000000000
							7431        RVFPCANBIG_RIP  0.500000000000000000000000
							7431        RVFPCANBIG_SYN  0.500000000000000000000000
							7431       RVFPCANSMALL_DD  0.000000000000000000000000
							7431      RVFPCANSMALL_RIP  0.750000000000000000000000
							7431      RVFPCANSMALL_SYN  0.750000000000000000000000
							7431        RVFPGNDBARE_DD  0.000000000000000000000000
							7431       RVFPGNDBARE_RIP  0.500000000000000000000000
							7431       RVFPGNDBARE_SYN  0.500000000000000000000000
							7431   RVFPGNDINUNDATED_DD  0.000000000000000000000000
							7431  RVFPGNDINUNDATED_RIP  0.250000000000000000000000
							7431  RVFPGNDINUNDATED_SYN  0.250000000000000000000000
							7431        RVFPGNDNONW_DD  0.000000000000000000000000
							7431       RVFPGNDNONW_RIP  0.875000000000000000000000
							7431       RVFPGNDNONW_SYN  0.875000000000000000000000
							7431       RVFPGNDWOODY_DD  0.000000000000000000000000
							7431      RVFPGNDWOODY_RIP  1.000000000000000000000000
							7431      RVFPGNDWOODY_SYN  1.000000000000000000000000
							7431  RVFPUNDDECIDUOUS_RIP  0.250000000000000000000000
							7431        RVFPUNDNONW_DD  0.000000000000000000000000
							7431       RVFPUNDNONW_RIP  0.500000000000000000000000
							7431       RVFPUNDNONW_SYN  0.500000000000000000000000
							7431       RVFPUNDWOODY_DD  0.000000000000000000000000
							7431      RVFPUNDWOODY_RIP  0.875000000000000000000000
							7431      RVFPUNDWOODY_SYN  0.875000000000000000000000
							7431          RVICANOPY_DD  0.000000000000000000000000
							7431         RVICANOPY_RIP  0.318749999999999977795540
							7431         RVICANOPY_SYN  0.318749999999999977795540
							7431          RVICANUND_DD  0.000000000000000000000000
							7431         RVICANUND_RIP  0.537499999999999977795540
							7431         RVICANUND_SYN  0.537499999999999977795540
							7431          RVIGROUND_DD  0.000000000000000000000000
							7431         RVIGROUND_RIP  0.396874999999999977795540
							7431         RVIGROUND_SYN  0.396874999999999977795540
							7431           RVIHERBS_DD  0.000000000000000000000000
							7431          RVIHERBS_RIP  0.234375000000000000000000
							7431          RVIHERBS_SYN  0.234375000000000000000000
							7431        RVITALLWOOD_DD  0.000000000000000000000000
							7431       RVITALLWOOD_RIP  0.487499999999999988897770
							7431       RVITALLWOOD_SYN  0.487499999999999988897770
							7431        RVITOTALVEG_DD  0.000000000000000000000000
							7431       RVITOTALVEG_RIP  0.871874999999999955591079
							7431       RVITOTALVEG_SYN  0.871874999999999955591079
							7431      RVIUNDERSTORY_DD  0.000000000000000000000000
							7431     RVIUNDERSTORY_RIP  0.218750000000000000000000
							7431     RVIUNDERSTORY_SYN  0.218750000000000000000000
							7431           RVIWOODY_DD  0.000000000000000000000000
							7431          RVIWOODY_RIP  0.637499999999999955591079
							7431          RVIWOODY_SYN  0.637499999999999955591079
							7431          RVNCANBIG_DD  8.000000000000000000000000
							7431         RVNCANBIG_RIP  8.000000000000000000000000
							7431         RVNCANBIG_SYN  8.000000000000000000000000
							7431        RVNCANSMALL_DD  8.000000000000000000000000
							7431       RVNCANSMALL_RIP  8.000000000000000000000000
							7431       RVNCANSMALL_SYN  8.000000000000000000000000
							7431         RVNGNDBARE_DD  8.000000000000000000000000
							7431        RVNGNDBARE_RIP  8.000000000000000000000000
							7431        RVNGNDBARE_SYN  8.000000000000000000000000
							7431    RVNGNDINUNDATED_DD  8.000000000000000000000000
							7431   RVNGNDINUNDATED_RIP  8.000000000000000000000000
							7431   RVNGNDINUNDATED_SYN  8.000000000000000000000000
							7431         RVNGNDNONW_DD  8.000000000000000000000000
							7431        RVNGNDNONW_RIP  8.000000000000000000000000
							7431        RVNGNDNONW_SYN  8.000000000000000000000000
							7431        RVNGNDWOODY_DD  8.000000000000000000000000
							7431       RVNGNDWOODY_RIP  8.000000000000000000000000
							7431       RVNGNDWOODY_SYN  8.000000000000000000000000
							7431     RVNUNDERSTORY_RIP  8.000000000000000000000000
							7431         RVNUNDNONW_DD  8.000000000000000000000000
							7431        RVNUNDNONW_RIP  8.000000000000000000000000
							7431        RVNUNDNONW_SYN  8.000000000000000000000000
							7431        RVNUNDWOODY_DD  8.000000000000000000000000
							7431       RVNUNDWOODY_RIP  8.000000000000000000000000
							7431       RVNUNDWOODY_SYN  8.000000000000000000000000
							7431          RVVCANBIG_DD  0.000000000000000000000000
							7431         RVVCANBIG_RIP  0.110194633003859995823426
							7431         RVVCANBIG_SYN  0.110194633003859995823426
							7431        RVVCANSMALL_DD  0.000000000000000000000000
							7431       RVVCANSMALL_RIP  0.231744163125749996767766
							7431       RVVCANSMALL_SYN  0.231744163125749996767766
							7431         RVVGNDBARE_DD  0.000000000000000000000000
							7431        RVVGNDBARE_RIP  0.328926883830099991978813
							7431        RVVGNDBARE_SYN  0.328926883830099991978813
							7431    RVVGNDINUNDATED_DD  0.000000000000000000000000
							7431   RVVGNDINUNDATED_RIP  0.254171823058120027738482
							7431   RVVGNDINUNDATED_SYN  0.254171823058120027738482
							7431         RVVGNDNONW_DD  0.000000000000000000000000
							7431        RVVGNDNONW_RIP  0.217047545251019990963925
							7431        RVVGNDNONW_SYN  0.217047545251019990963925
							7431        RVVGNDWOODY_DD  0.000000000000000000000000
							7431       RVVGNDWOODY_RIP  0.309207520425549997611370
							7431       RVVGNDWOODY_SYN  0.309207520425549997611370
							7431         RVVUNDNONW_DD  0.000000000000000000000000
							7431        RVVUNDNONW_RIP  0.084515425472850000399028
							7431        RVVUNDNONW_SYN  0.084515425472850000399028
							7431        RVVUNDWOODY_DD  0.000000000000000000000000
							7431       RVVUNDWOODY_RIP  0.113192314226710000202303
							7431       RVVUNDWOODY_SYN  0.113192314226710000202303
							7431   RVFPCANDECIDUOUS_DD                          NA
							7431  RVFPCANDECIDUOUS_RIP                           0	# SAS calculates as NA
							7431      RVFPCANMIXED_RIP  1.000000000000000000000000
							7431   RVFPUNDDECIDUOUS_DD                          NA
							7431      RVFPUNDMIXED_RIP  0.625000000000000000000000
							7431          RVNCANOPY_DD  0.000000000000000000000000
							7431         RVNCANOPY_RIP  6.000000000000000000000000
							7431      RVNUNDERSTORY_DD  0.000000000000000000000000
							7431        RVFPCANNONE_DD                          NA
							7431       RVFPCANNONE_RIP                           0	# SAS calculates as NA
							7431        RVFPUNDNONE_DD                          NA
							7431       RVFPUNDNONE_RIP                           0	# SAS calculates as NA
							7431 RVFPUNDCONIFEROUS_RIP  0.125000000000000000000000
							1000222         RVFCCANBIG_DD  0.200000000000000011102230
							1000222        RVFCCANBIG_RIP  0.275000000000000022204460
							1000222        RVFCCANBIG_SYN  0.265166666666660000295508
							1000222       RVFCCANSMALL_DD  0.000000000000000000000000
							1000222      RVFCCANSMALL_RIP  0.080000000000000001665335
							1000222      RVFCCANSMALL_SYN  0.056000000000000001165734
							1000222        RVFCGNDBARE_DD  0.461778583173930001759544
							1000222       RVFCGNDBARE_RIP  0.036363636363629998904390
							1000222       RVFCGNDBARE_SYN  0.219565593281730003782926
							1000222   RVFCGNDINUNDATED_DD  0.046511627906969998935693
							1000222  RVFCGNDINUNDATED_RIP  0.066666666666660004403333
							1000222  RVFCGNDINUNDATED_SYN  0.034099616858230000215890
							1000222        RVFCGNDNONW_DD  0.480281217490510003997883
							1000222       RVFCGNDNONW_RIP  0.581818181818180013564756
							1000222       RVFCGNDNONW_SYN  0.575428225324620012948174
							1000222       RVFCGNDWOODY_DD  0.011428571428569999149372
							1000222      RVFCGNDWOODY_RIP  0.315151515151510019929759
							1000222      RVFCGNDWOODY_SYN  0.170906564535389993153558
							1000222        RVFCUNDNONW_DD  0.050000000000000002775558
							1000222       RVFCUNDNONW_RIP  0.610555555555549966939566
							1000222       RVFCUNDNONW_SYN  0.457166666666660004292311
							1000222       RVFCUNDWOODY_DD  0.040000000000000000832667
							1000222      RVFCUNDWOODY_RIP  0.229444444444440009966968
							1000222      RVFCUNDWOODY_SYN  0.172666666666660001405731
							1000222         RVFPCANBIG_DD  0.800000000000000044408921
							1000222        RVFPCANBIG_RIP  1.000000000000000000000000
							1000222        RVFPCANBIG_SYN  1.000000000000000000000000
							1000222       RVFPCANSMALL_DD  0.000000000000000000000000
							1000222      RVFPCANSMALL_RIP  0.800000000000000044408921
							1000222      RVFPCANSMALL_SYN  0.800000000000000044408921
							1000222        RVFPGNDBARE_DD  1.000000000000000000000000
							1000222       RVFPGNDBARE_RIP  0.400000000000000022204460
							1000222       RVFPGNDBARE_SYN  1.000000000000000000000000
							1000222   RVFPGNDINUNDATED_DD  0.200000000000000011102230
							1000222  RVFPGNDINUNDATED_RIP  0.200000000000000011102230
							1000222  RVFPGNDINUNDATED_SYN  0.400000000000000022204460
							1000222        RVFPGNDNONW_DD  1.000000000000000000000000
							1000222       RVFPGNDNONW_RIP  1.000000000000000000000000
							1000222       RVFPGNDNONW_SYN  1.000000000000000000000000
							1000222       RVFPGNDWOODY_DD  0.200000000000000011102230
							1000222      RVFPGNDWOODY_RIP  1.000000000000000000000000
							1000222      RVFPGNDWOODY_SYN  1.000000000000000000000000
							1000222  RVFPUNDDECIDUOUS_RIP  1.000000000000000000000000
							1000222        RVFPUNDNONW_DD  1.000000000000000000000000
							1000222       RVFPUNDNONW_RIP  1.000000000000000000000000
							1000222       RVFPUNDNONW_SYN  1.000000000000000000000000
							1000222       RVFPUNDWOODY_DD  0.800000000000000044408921
							1000222      RVFPUNDWOODY_RIP  1.000000000000000000000000
							1000222      RVFPUNDWOODY_SYN  1.000000000000000000000000
							1000222          RVICANOPY_DD  0.200000000000000011102230
							1000222         RVICANOPY_RIP  0.354999999999999982236432
							1000222         RVICANOPY_SYN  0.321166666666659994522348
							1000222          RVICANUND_DD  0.289999999999999980015986
							1000222         RVICANUND_RIP  1.219999999999999973354647
							1000222         RVICANUND_SYN  0.950999999999999956479257
							1000222          RVIGROUND_DD  0.500000000000000000000000
							1000222         RVIGROUND_RIP  0.349999999999999977795540
							1000222         RVIGROUND_SYN  0.419666666666660026496771
							1000222           RVIHERBS_DD  0.489999999999999991118216
							1000222          RVIHERBS_RIP  0.839999999999999968913755
							1000222          RVIHERBS_SYN  0.754833333333330025460839
							1000222        RVITALLWOOD_DD  0.239999999999999991118216
							1000222       RVITALLWOOD_RIP  0.589999999999999968913755
							1000222       RVITALLWOOD_SYN  0.493833333333330015690876
							1000222        RVITOTALVEG_DD  0.739999999999999991118216
							1000222       RVITOTALVEG_RIP  1.560000000000000053290705
							1000222       RVITOTALVEG_SYN  1.348000000000000087041485
							1000222      RVIUNDERSTORY_DD  0.089999999999999996669331
							1000222     RVIUNDERSTORY_RIP  0.864999999999999991118216
							1000222     RVIUNDERSTORY_SYN  0.629833333333330025460839
							1000222           RVIWOODY_DD  0.250000000000000000000000
							1000222          RVIWOODY_RIP  0.719999999999999973354647
							1000222          RVIWOODY_SYN  0.593166666666659958551122
							1000222          RVNCANBIG_DD  5.000000000000000000000000
							1000222         RVNCANBIG_RIP  5.000000000000000000000000
							1000222         RVNCANBIG_SYN  5.000000000000000000000000
							1000222        RVNCANSMALL_DD  5.000000000000000000000000
							1000222       RVNCANSMALL_RIP  5.000000000000000000000000
							1000222       RVNCANSMALL_SYN  5.000000000000000000000000
							1000222         RVNGNDBARE_DD  5.000000000000000000000000
							1000222        RVNGNDBARE_RIP  5.000000000000000000000000
							1000222        RVNGNDBARE_SYN  5.000000000000000000000000
							1000222    RVNGNDINUNDATED_DD  5.000000000000000000000000
							1000222   RVNGNDINUNDATED_RIP  5.000000000000000000000000
							1000222   RVNGNDINUNDATED_SYN  5.000000000000000000000000
							1000222         RVNGNDNONW_DD  5.000000000000000000000000
							1000222        RVNGNDNONW_RIP  5.000000000000000000000000
							1000222        RVNGNDNONW_SYN  5.000000000000000000000000
							1000222        RVNGNDWOODY_DD  5.000000000000000000000000
							1000222       RVNGNDWOODY_RIP  5.000000000000000000000000
							1000222       RVNGNDWOODY_SYN  5.000000000000000000000000
							1000222     RVNUNDERSTORY_RIP  5.000000000000000000000000
							1000222         RVNUNDNONW_DD  5.000000000000000000000000
							1000222        RVNUNDNONW_RIP  5.000000000000000000000000
							1000222        RVNUNDNONW_SYN  5.000000000000000000000000
							1000222        RVNUNDWOODY_DD  5.000000000000000000000000
							1000222       RVNUNDWOODY_RIP  5.000000000000000000000000
							1000222       RVNUNDWOODY_SYN  5.000000000000000000000000
							1000222          RVVCANBIG_DD  0.111803398874980003396828
							1000222         RVVCANBIG_RIP  0.188745860881760013638697
							1000222         RVVCANBIG_SYN  0.181211585967579996703236
							1000222        RVVCANSMALL_DD  0.000000000000000000000000
							1000222       RVVCANSMALL_RIP  0.097467943448080004986700
							1000222       RVVCANSMALL_SYN  0.072548834128000005971870
							1000222         RVVGNDBARE_DD  0.226314050663340010638436
							1000222        RVVGNDBARE_RIP  0.049792959773190002825682
							1000222        RVVGNDBARE_SYN  0.076160849525050006159077
							1000222    RVVGNDINUNDATED_DD  0.104003161744170000146958
							1000222   RVVGNDINUNDATED_RIP  0.149071198499979989948727
							1000222   RVVGNDINUNDATED_SYN  0.051196399570270001433414
							1000222         RVVGNDNONW_DD  0.200716619121389994173654
							1000222        RVVGNDNONW_RIP  0.234872921504789999858076
							1000222        RVVGNDNONW_SYN  0.118112850811099998060705
							1000222        RVVGNDWOODY_DD  0.025555062599989998572969
							1000222       RVVGNDWOODY_RIP  0.144297799403739990253825
							1000222       RVVGNDWOODY_SYN  0.142305613633199989909173
							1000222         RVVUNDNONW_DD  0.000000000000000000000000
							1000222        RVVUNDNONW_RIP  0.240011895281340009722371
							1000222        RVVUNDNONW_SYN  0.164210654005420009227123
							1000222        RVVUNDWOODY_DD  0.022360679774990000617807
							1000222       RVVUNDWOODY_RIP  0.214641532920169991749404
							1000222       RVVUNDWOODY_SYN  0.143515388257379999936703
							1000222   RVFPCANDECIDUOUS_DD  0.800000000000000044408921
							1000222  RVFPCANDECIDUOUS_RIP  1.000000000000000000000000
							1000222      RVFPCANMIXED_RIP                           0	# SAS calculates NA
							1000222   RVFPUNDDECIDUOUS_DD  1.000000000000000000000000
							1000222      RVFPUNDMIXED_RIP                           0	# SAS calculates NA
							1000222          RVNCANOPY_DD  5.000000000000000000000000
							1000222         RVNCANOPY_RIP  5.000000000000000000000000
							1000222      RVNUNDERSTORY_DD  3.000000000000000000000000
							1000222        RVFPCANNONE_DD  0.200000000000000011102230
							1000222       RVFPCANNONE_RIP                           0	# SAS calculates NA
							1000222        RVFPUNDNONE_DD                           0	# SAS calculates NA
							1000222       RVFPUNDNONE_RIP                           0	# SAS calculates NA
							1000222 RVFPUNDCONIFEROUS_RIP                           0	# SAS calculates NA
							6362   RVFPCANBROADLEAF_DD     NA			# These next 81 rows should be but aren't calculated by SAS.
							6362  RVFPCANBROADLEAF_RIP     NA
							6362  RVFPCANCONIFEROUS_DD     NA
							6362 RVFPCANCONIFEROUS_RIP     NA
							6362       RVFPCANMIXED_DD     NA
							6362   RVFPUNDBROADLEAF_DD     NA
							6362  RVFPUNDBROADLEAF_RIP      0
							6362  RVFPUNDCONIFEROUS_DD     NA
							6362       RVFPUNDMIXED_DD     NA
							6396   RVFPCANBROADLEAF_DD      0
							6396  RVFPCANBROADLEAF_RIP      0
							6396  RVFPCANCONIFEROUS_DD      0
							6396 RVFPCANCONIFEROUS_RIP      0
							6396       RVFPCANMIXED_DD      0
							6396   RVFPUNDBROADLEAF_DD      0
							6396  RVFPUNDBROADLEAF_RIP      0
							6396  RVFPUNDCONIFEROUS_DD      0
							6396       RVFPUNDMIXED_DD      0
							6449   RVFPCANBROADLEAF_DD      0
							6449  RVFPCANBROADLEAF_RIP      0
							6449  RVFPCANCONIFEROUS_DD      0
							6449 RVFPCANCONIFEROUS_RIP      0
							6449       RVFPCANMIXED_DD      0
							6449   RVFPUNDBROADLEAF_DD      0
							6449  RVFPUNDBROADLEAF_RIP      0
							6449  RVFPUNDCONIFEROUS_DD      0
							6449       RVFPUNDMIXED_DD      0
							6518   RVFPCANBROADLEAF_DD      0
							6518  RVFPCANBROADLEAF_RIP      0
							6518  RVFPCANCONIFEROUS_DD      0
							6518 RVFPCANCONIFEROUS_RIP      0
							6518       RVFPCANMIXED_DD      0
							6518   RVFPUNDBROADLEAF_DD      0
							6518  RVFPUNDBROADLEAF_RIP      0
							6518  RVFPUNDCONIFEROUS_DD      0
							6518       RVFPUNDMIXED_DD      0
							6530   RVFPCANBROADLEAF_DD     NA
							6530  RVFPCANBROADLEAF_RIP      0
							6530  RVFPCANCONIFEROUS_DD     NA
							6530 RVFPCANCONIFEROUS_RIP      0
							6530       RVFPCANMIXED_DD     NA
							6530   RVFPUNDBROADLEAF_DD     NA
							6530  RVFPUNDBROADLEAF_RIP      0
							6530  RVFPUNDCONIFEROUS_DD     NA
							6530       RVFPUNDMIXED_DD     NA
							6618   RVFPCANBROADLEAF_DD     NA
							6618  RVFPCANBROADLEAF_RIP     NA
							6618  RVFPCANCONIFEROUS_DD     NA
							6618 RVFPCANCONIFEROUS_RIP     NA
							6618       RVFPCANMIXED_DD     NA
							6618   RVFPUNDBROADLEAF_DD     NA
							6618  RVFPUNDBROADLEAF_RIP      0
							6618  RVFPUNDCONIFEROUS_DD     NA
							6618       RVFPUNDMIXED_DD     NA
							6647   RVFPCANBROADLEAF_DD      0
							6647  RVFPCANBROADLEAF_RIP      0
							6647  RVFPCANCONIFEROUS_DD      0
							6647 RVFPCANCONIFEROUS_RIP      0
							6647       RVFPCANMIXED_DD      0
							6647   RVFPUNDBROADLEAF_DD      0
							6647  RVFPUNDBROADLEAF_RIP      0
							6647  RVFPUNDCONIFEROUS_DD      0
							6647       RVFPUNDMIXED_DD      0
							7431   RVFPCANBROADLEAF_DD     NA
							7431  RVFPCANBROADLEAF_RIP      0
							7431  RVFPCANCONIFEROUS_DD     NA
							7431 RVFPCANCONIFEROUS_RIP      0
							7431       RVFPCANMIXED_DD     NA
							7431   RVFPUNDBROADLEAF_DD     NA
							7431  RVFPUNDBROADLEAF_RIP      0
							7431  RVFPUNDCONIFEROUS_DD     NA
							7431       RVFPUNDMIXED_DD     NA
							1000222   RVFPCANBROADLEAF_DD      0
							1000222  RVFPCANBROADLEAF_RIP      0
							1000222  RVFPCANCONIFEROUS_DD      0
							1000222 RVFPCANCONIFEROUS_RIP      0
							1000222       RVFPCANMIXED_DD      0
							1000222   RVFPUNDBROADLEAF_DD      0
							1000222  RVFPUNDBROADLEAF_RIP      0
							1000222  RVFPUNDCONIFEROUS_DD      0
							1000222       RVFPUNDMIXED_DD      0
						 ")		
					
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(rc)
	
}

# end of file