# nlaShorelineSubstrate.r
# RUnit tests
#
#    7/17/17 cws Renamed from metsShorelineSubstrate and updated to new calling
#            interface, changing UID, and RESULT to SITE and VALUE, and changing
#            output column PARAMETER to METRIC.
#

nlaShorelineSubstrateTest <- function()
# Unit test for nlaShorelineSubstrate
{
	nlaShorelineSubstrateTest.2007()
	
}


nlaShorelineSubstrateTest.2007 <- function()
# Unit test for nlaShorelineSubstrate using 2007 data.
{
	testData <- nlaShorelineSubstrateTest.createTestData2007()
	expected <- nlaShorelineSubstrateTest.expectedResults2007()
	actual <- nlaShorelineSubstrate(bedrock = testData %>% subset(PARAMETER == 'SS_BEDROCK') %>% select(SITE, STATION, VALUE)
	                               ,boulder = testData %>% subset(PARAMETER == 'SS_BOULDERS') %>% select(SITE, STATION, VALUE)
	                               ,cobble = testData %>% subset(PARAMETER == 'SS_COBBLE') %>% select(SITE, STATION, VALUE)
	                               ,gravel = testData %>% subset(PARAMETER == 'SS_GRAVEL') %>% select(SITE, STATION, VALUE)
	                               ,organic = testData %>% subset(PARAMETER == 'SS_ORGANIC') %>% select(SITE, STATION, VALUE)
	                               ,other = testData %>% subset(PARAMETER == 'SS_OTHER') %>% select(SITE, STATION, VALUE)
	                               ,sand = testData %>% subset(PARAMETER == 'SS_SAND') %>% select(SITE, STATION, VALUE)
	                               ,silt = testData %>% subset(PARAMETER == 'SS_SILT') %>% select(SITE, STATION, VALUE)
	                               ,wood = testData %>% subset(PARAMETER == 'SS_WOOD') %>% select(SITE, STATION, VALUE)
	                               )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
#	return(diff)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
	
}


nlaShorelineSubstrateTest.createTestData2007 <- function()
# Creates data from 2007 study for nlaShorelineSubstrate unit test.
#
#	SITE		Description
#	7469	All values recorded at all stations
#	7533	Some values recorded at all stations, some values totally absent
#	7611	All values recorded at some stations
#	7682	Some values recorded at most stations, missing one station and one SS_ORGANIC
#	7684	Some values recorded at most stations
#	7771	All values recorded at more stations than expected
#	7784	All values recorded only at one station
#	7961	All values recorded at all stations, using one moved station
{
	tc <- textConnection("   SITE    STATION   PARAMETER VALUE UNITS
							7469          A  SS_BEDROCK      0 X         
							7469          A SS_BOULDERS      0 X         
							7469          A   SS_COBBLE      0 X         
							7469          A   SS_GRAVEL      0 X         
							7469          A  SS_ORGANIC      3 X         
							7469          A    SS_OTHER      3 X         
							7469          A     SS_SAND      0 X         
							7469          A     SS_SILT      1 X         
							7469          A     SS_WOOD      0 X         
							7469          B  SS_BEDROCK      0 X         
							7469          B SS_BOULDERS      0 X         
							7469          B   SS_COBBLE      0 X         
							7469          B   SS_GRAVEL      0 X         
							7469          B  SS_ORGANIC      3 X         
							7469          B    SS_OTHER      3 X         
							7469          B     SS_SAND      0 X         
							7469          B     SS_SILT      1 X         
							7469          B     SS_WOOD      0 X         
							7469          C  SS_BEDROCK      0 X         
							7469          C SS_BOULDERS      0 X         
							7469          C   SS_COBBLE      0 X         
							7469          C   SS_GRAVEL      0 X         
							7469          C  SS_ORGANIC      3 X         
							7469          C    SS_OTHER      3 X         
							7469          C     SS_SAND      0 X         
							7469          C     SS_SILT      1 X         
							7469          C     SS_WOOD      0 X         
							7469          D  SS_BEDROCK      0 X         
							7469          D SS_BOULDERS      0 X         
							7469          D   SS_COBBLE      0 X         
							7469          D   SS_GRAVEL      0 X         
							7469          D  SS_ORGANIC      3 X         
							7469          D    SS_OTHER      3 X         
							7469          D     SS_SAND      0 X         
							7469          D     SS_SILT      1 X         
							7469          D     SS_WOOD      0 X         
							7469          E  SS_BEDROCK      0 X         
							7469          E SS_BOULDERS      0 X         
							7469          E   SS_COBBLE      0 X         
							7469          E   SS_GRAVEL      0 X         
							7469          E  SS_ORGANIC      3 X         
							7469          E    SS_OTHER      3 X         
							7469          E     SS_SAND      0 X         
							7469          E     SS_SILT      1 X         
							7469          E     SS_WOOD      0 X         
							7469          F  SS_BEDROCK      0 X         
							7469          F SS_BOULDERS      0 X         
							7469          F   SS_COBBLE      0 X         
							7469          F   SS_GRAVEL      0 X         
							7469          F  SS_ORGANIC      2 X         
							7469          F    SS_OTHER      3 X         
							7469          F     SS_SAND      0 X         
							7469          F     SS_SILT      1 X         
							7469          F     SS_WOOD      0 X         
							7469          G  SS_BEDROCK      0 X         
							7469          G SS_BOULDERS      0 X         
							7469          G   SS_COBBLE      0 X         
							7469          G   SS_GRAVEL      0 X         
							7469          G  SS_ORGANIC      0 X         
							7469          G    SS_OTHER      4 X         
							7469          G     SS_SAND      0 X         
							7469          G     SS_SILT      1 X         
							7469          G     SS_WOOD      0 X         
							7469          H  SS_BEDROCK      0 X         
							7469          H SS_BOULDERS      0 X         
							7469          H   SS_COBBLE      0 X         
							7469          H   SS_GRAVEL      0 X         
							7469          H  SS_ORGANIC      0 X         
							7469          H    SS_OTHER      4 X         
							7469          H     SS_SAND      0 X         
							7469          H     SS_SILT      0 X         
							7469          H     SS_WOOD      0 X         
							7469          I  SS_BEDROCK      0 X         
							7469          I SS_BOULDERS      0 X         
							7469          I   SS_COBBLE      0 X         
							7469          I   SS_GRAVEL      0 X         
							7469          I  SS_ORGANIC      0 X         
							7469          I    SS_OTHER      4 X         
							7469          I     SS_SAND      0 X         
							7469          I     SS_SILT      1 X         
							7469          I     SS_WOOD      0 X         
							7469          J  SS_BEDROCK      0 X         
							7469          J SS_BOULDERS      0 X         
							7469          J   SS_COBBLE      0 X         
							7469          J   SS_GRAVEL      0 X         
							7469          J  SS_ORGANIC      1 X         
							7469          J    SS_OTHER      4 X         
							7469          J     SS_SAND      0 X         
							7469          J     SS_SILT      1 X         
							7469          J     SS_WOOD      0 X         
							7533          A  SS_BEDROCK      0 X         
							7533          A SS_BOULDERS      1 X         
							7533          A   SS_COBBLE      0 X         
							7533          A    SS_OTHER      3 X         
							7533          A     SS_SAND      0 X         
							7533          A     SS_SILT      2 X         
							7533          B  SS_BEDROCK      0 X         
							7533          B SS_BOULDERS      2 X         
							7533          B   SS_COBBLE      2 X         
							7533          B    SS_OTHER      0 X         
							7533          B     SS_SAND      2 X         
							7533          B     SS_SILT      2 X         
							7533          C  SS_BEDROCK      0 X         
							7533          C SS_BOULDERS      0 X         
							7533          C   SS_COBBLE      0 X         
							7533          C    SS_OTHER      3 X         
							7533          C     SS_SAND      0 X         
							7533          C     SS_SILT      2 X         
							7533          D  SS_BEDROCK      0 X         
							7533          D SS_BOULDERS      0 X         
							7533          D   SS_COBBLE      0 X         
							7533          D    SS_OTHER      4 X         
							7533          D     SS_SAND      0 X         
							7533          D     SS_SILT      1 X         
							7533          E  SS_BEDROCK      0 X         
							7533          E SS_BOULDERS      0 X         
							7533          E   SS_COBBLE      0 X         
							7533          E    SS_OTHER      3 X         
							7533          E     SS_SAND      1 X         
							7533          E     SS_SILT      2 X         
							7533          F  SS_BEDROCK      0 X         
							7533          F SS_BOULDERS      0 X         
							7533          F   SS_COBBLE      0 X         
							7533          F    SS_OTHER      4 X         
							7533          F     SS_SAND      0 X         
							7533          F     SS_SILT      1 X         
							7533          G  SS_BEDROCK      0 X         
							7533          G SS_BOULDERS      0 X         
							7533          G   SS_COBBLE      0 X         
							7533          G    SS_OTHER      4 X         
							7533          G     SS_SAND      0 X         
							7533          G     SS_SILT      1 X         
							7533          H  SS_BEDROCK      0 X         
							7533          H SS_BOULDERS      0 X         
							7533          H   SS_COBBLE      0 X         
							7533          H    SS_OTHER      4 X         
							7533          H     SS_SAND      0 X         
							7533          H     SS_SILT      1 X         
							7533          I  SS_BEDROCK      0 X         
							7533          I SS_BOULDERS      1 X         
							7533          I   SS_COBBLE      0 X         
							7533          I    SS_OTHER      2 X         
							7533          I     SS_SAND      0 X         
							7533          I     SS_SILT      2 X         
							7533          J  SS_BEDROCK      0 X         
							7533          J SS_BOULDERS      2 X         
							7533          J   SS_COBBLE      2 X         
							7533          J    SS_OTHER      2 X         
							7533          J     SS_SAND      0 X         
							7533          J     SS_SILT      0 X         
							7611          A  SS_BEDROCK      0 X         
							7611          A SS_BOULDERS      1 X         
							7611          A   SS_COBBLE      4 X         
							7611          A   SS_GRAVEL      1 X         
							7611          A  SS_ORGANIC      0 X         
							7611          A    SS_OTHER      1 X         
							7611          A     SS_SAND      0 X         
							7611          A     SS_SILT      0 X         
							7611          A     SS_WOOD      0 X         
							7611          B  SS_BEDROCK      0 X         
							7611          B SS_BOULDERS      1 X         
							7611          B   SS_COBBLE      4 X         
							7611          B   SS_GRAVEL      1 X         
							7611          B  SS_ORGANIC      0 X         
							7611          B    SS_OTHER      1 X         
							7611          B     SS_SAND      0 X         
							7611          B     SS_SILT      0 X         
							7611          B     SS_WOOD      1 X         
							7611          C  SS_BEDROCK      0 X         
							7611          C SS_BOULDERS      3 X         
							7611          C   SS_COBBLE      3 X         
							7611          C   SS_GRAVEL      0 X         
							7611          C  SS_ORGANIC      0 X         
							7611          C    SS_OTHER      0 X         
							7611          C     SS_SAND      0 X         
							7611          C     SS_SILT      0 X         
							7611          C     SS_WOOD      2 X         
							7611          D  SS_BEDROCK      1 X         
							7611          D SS_BOULDERS      2 X         
							7611          D   SS_COBBLE      4 X         
							7611          D   SS_GRAVEL      1 X         
							7611          D  SS_ORGANIC      1 X         
							7611          D    SS_OTHER      1 X         
							7611          D     SS_SAND      0 X         
							7611          D     SS_SILT      0 X         
							7611          D     SS_WOOD      1 X         
							7611          J  SS_BEDROCK      0 X         
							7611          J SS_BOULDERS      2 X         
							7611          J   SS_COBBLE      4 X         
							7611          J   SS_GRAVEL      1 X         
							7611          J  SS_ORGANIC      0 X         
							7611          J    SS_OTHER      2 X         
							7611          J     SS_SAND      0 X         
							7611          J     SS_SILT      0 X         
							7611          J     SS_WOOD      1 X         
							7682          A  SS_BEDROCK      0 X         
							7682          A SS_BOULDERS      0 X         
							7682          A   SS_COBBLE      2 X         
							7682          A    SS_OTHER      3 X         
							7682          A     SS_SAND      2 X         
							7682          A     SS_SILT      1 X         
							7682          A     SS_WOOD      1 X         
							7682          B  SS_BEDROCK      0 X         
							7682          B SS_BOULDERS      0 X         
							7682          B   SS_COBBLE      1 X         
							7682          B    SS_OTHER      3 X         
							7682          C  SS_BEDROCK      0 X         
							7682          C SS_BOULDERS      1 X         
							7682          C   SS_COBBLE      2 X         
							7682          C   SS_GRAVEL      3 X         
							7682          C    SS_OTHER      2 X         
							7682          C     SS_SAND      2 X         
							7682          C     SS_SILT      1 X         
							7682          D  SS_BEDROCK      0 X         
							7682          D SS_BOULDERS      0 X         
							7682          D   SS_COBBLE      2 X         
							7682          D   SS_GRAVEL      2 X         
							7682          D    SS_OTHER      3 X         
							7682          D     SS_SAND      1 X         
							7682          D     SS_SILT      2 X         
							7682          E  SS_BEDROCK      0 X         
							7682          E SS_BOULDERS      1 X         
							7682          E   SS_COBBLE      2 X         
							7682          E    SS_OTHER      3 X         
							7682          E     SS_SAND      2 X         
							7682          E     SS_SILT      1 X         
							7682          F  SS_BEDROCK      0 X         
							7682          F SS_BOULDERS      1 X         
							7682          F   SS_COBBLE      1 X         
							7682          F    SS_OTHER      3 X         
							7682          F     SS_SAND      0 X         
							7682          F     SS_SILT      3 X         
							7682          H  SS_BEDROCK      0 X         
							7682          H SS_BOULDERS      0 X         
							7682          H   SS_COBBLE      0 X         
							7682          H    SS_OTHER      3 X         
							7682          H     SS_SAND      1 X         
							7682          H     SS_SILT      3 X         
							7682          I  SS_BEDROCK      0 X         
							7682          I SS_BOULDERS      0 X         
							7682          I   SS_COBBLE      2 X         
							7682          I    SS_OTHER      3 X         
							7682          I     SS_SAND      2 X         
							7682          I     SS_SILT      2 X         
							7682          J  SS_BEDROCK      0 X         
							7682          J SS_BOULDERS      1 X         
							7682          J   SS_COBBLE      2 X         
							7682          J    SS_OTHER      4 X         
							7682          J     SS_SAND      2 X         
							7682          J     SS_SILT      2 X         
							7684          A  SS_BEDROCK      0 X         
							7684          A SS_BOULDERS      3 X         
							7684          A   SS_COBBLE      0 X         
							7684          A   SS_GRAVEL      0 X         
							7684          A  SS_ORGANIC      0 X         
							7684          A    SS_OTHER      0 X         
							7684          A     SS_SAND      3 X         
							7684          A     SS_SILT      0 X         
							7684          A     SS_WOOD      0 X         
							7684          B  SS_BEDROCK      0 X         
							7684          B SS_BOULDERS      3 X         
							7684          B   SS_COBBLE      0 X         
							7684          B   SS_GRAVEL      2 X         
							7684          B  SS_ORGANIC      0 X         
							7684          B    SS_OTHER      0 X         
							7684          B     SS_SAND      2 X         
							7684          B     SS_SILT      0 X         
							7684          B     SS_WOOD      0 X         
							7684          C  SS_BEDROCK      0 X         
							7684          C SS_BOULDERS      0 X         
							7684          C   SS_COBBLE      0 X         
							7684          C   SS_GRAVEL      0 X         
							7684          C  SS_ORGANIC      0 X         
							7684          C    SS_OTHER      0 X         
							7684          C     SS_SAND      4 X         
							7684          C     SS_SILT      0 X         
							7684          C     SS_WOOD      1 X         
							7684          D  SS_BEDROCK      0 X         
							7684          D SS_BOULDERS      0 X         
							7684          D   SS_COBBLE      3 X         
							7684          D   SS_GRAVEL      3 X         
							7684          D  SS_ORGANIC      0 X         
							7684          D    SS_OTHER      0 X         
							7684          D     SS_SAND      3 X         
							7684          D     SS_SILT      0 X         
							7684          D     SS_WOOD      0 X         
							7684          E  SS_BEDROCK      0 X         
							7684          E SS_BOULDERS      0 X         
							7684          E   SS_COBBLE      0 X         
							7684          E   SS_GRAVEL      0 X         
							7684          E  SS_ORGANIC      0 X         
							7684          E    SS_OTHER      0 X         
							7684          E     SS_SAND      4 X         
							7684          E     SS_SILT      1 X         
							7684          E     SS_WOOD      1 X         
							7684          F     SS_SAND      4 X         
							7684          I  SS_BEDROCK      0 X         
							7684          I SS_BOULDERS      0 X         
							7684          I   SS_COBBLE      0 X         
							7684          I   SS_GRAVEL      0 X         
							7684          I  SS_ORGANIC      0 X         
							7684          I    SS_OTHER      4 X         
							7684          I     SS_SAND      0 X         
							7684          I     SS_SILT      3 X         
							7684          I     SS_WOOD      0 X         
							7771          A  SS_BEDROCK      0 X         
							7771          A SS_BOULDERS      1 X         
							7771          A   SS_COBBLE      1 X         
							7771          A   SS_GRAVEL      2 X         
							7771          A  SS_ORGANIC      0 X         
							7771          A    SS_OTHER      0 X         
							7771          A     SS_SAND      0 X         
							7771          A     SS_SILT      0 X         
							7771          A     SS_WOOD      0 X         
							7771          B  SS_BEDROCK      0 X         
							7771          B SS_BOULDERS      1 X         
							7771          B   SS_COBBLE      1 X         
							7771          B   SS_GRAVEL      1 X         
							7771          B  SS_ORGANIC      0 X         
							7771          B    SS_OTHER      0 X         
							7771          B     SS_SAND      0 X         
							7771          B     SS_SILT      1 X         
							7771          B     SS_WOOD      0 X         
							7771          C  SS_BEDROCK      0 X         
							7771          C SS_BOULDERS      1 X         
							7771          C   SS_COBBLE      1 X         
							7771          C   SS_GRAVEL      1 X         
							7771          C  SS_ORGANIC      0 X         
							7771          C    SS_OTHER      0 X         
							7771          C     SS_SAND      0 X         
							7771          C     SS_SILT      3 X         
							7771          C     SS_WOOD      0 X         
							7771          D  SS_BEDROCK      0 X         
							7771          D SS_BOULDERS      1 X         
							7771          D   SS_COBBLE      2 X         
							7771          D   SS_GRAVEL      2 X         
							7771          D  SS_ORGANIC      0 X         
							7771          D    SS_OTHER      0 X         
							7771          D     SS_SAND      0 X         
							7771          D     SS_SILT      1 X         
							7771          D     SS_WOOD      0 X         
							7771          E  SS_BEDROCK      0 X         
							7771          E SS_BOULDERS      0 X         
							7771          E   SS_COBBLE      0 X         
							7771          E   SS_GRAVEL      0 X         
							7771          E  SS_ORGANIC      0 X         
							7771          E    SS_OTHER      4 X         
							7771          E     SS_SAND      0 X         
							7771          E     SS_SILT      0 X         
							7771          E     SS_WOOD      0 X         
							7771          F  SS_BEDROCK      0 X         
							7771          F SS_BOULDERS      0 X         
							7771          F   SS_COBBLE      0 X         
							7771          F   SS_GRAVEL      3 X         
							7771          F  SS_ORGANIC      0 X         
							7771          F    SS_OTHER      2 X         
							7771          F     SS_SAND      0 X         
							7771          F     SS_SILT      0 X         
							7771          F     SS_WOOD      0 X         
							7771          G  SS_BEDROCK      0 X         
							7771          G SS_BOULDERS      0 X         
							7771          G   SS_COBBLE      0 X         
							7771          G   SS_GRAVEL      4 X         
							7771          G  SS_ORGANIC      0 X         
							7771          G    SS_OTHER      1 X         
							7771          G     SS_SAND      0 X         
							7771          G     SS_SILT      0 X         
							7771          G     SS_WOOD      0 X         
							7771          H  SS_BEDROCK      0 X         
							7771          H SS_BOULDERS      2 X         
							7771          H   SS_COBBLE      3 X         
							7771          H   SS_GRAVEL      1 X         
							7771          H  SS_ORGANIC      0 X         
							7771          H    SS_OTHER      0 X         
							7771          H     SS_SAND      0 X         
							7771          H     SS_SILT      0 X         
							7771          H     SS_WOOD      0 X         
							7771          I  SS_BEDROCK      0 X         
							7771          I SS_BOULDERS      2 X         
							7771          I   SS_COBBLE      2 X         
							7771          I   SS_GRAVEL      1 X         
							7771          I  SS_ORGANIC      0 X         
							7771          I    SS_OTHER      0 X         
							7771          I     SS_SAND      0 X         
							7771          I     SS_SILT      0 X         
							7771          I     SS_WOOD      0 X         
							7771          J  SS_BEDROCK      0 X         
							7771          J SS_BOULDERS      4 X         
							7771          J   SS_COBBLE      0 X         
							7771          J   SS_GRAVEL      0 X         
							7771          J  SS_ORGANIC      0 X         
							7771          J    SS_OTHER      0 X         
							7771          J     SS_SAND      0 X         
							7771          J     SS_SILT      0 X         
							7771          J     SS_WOOD      0 X         
							7771          K  SS_BEDROCK      0 X         
							7771          K SS_BOULDERS      0 X         
							7771          K   SS_COBBLE      2 X         
							7771          K   SS_GRAVEL      3 X         
							7771          K  SS_ORGANIC      0 X         
							7771          K    SS_OTHER      0 X         
							7771          K     SS_SAND      0 X         
							7771          K     SS_SILT      0 X         
							7771          K     SS_WOOD      0 X         
							7771          L  SS_BEDROCK      0 X         
							7771          L SS_BOULDERS      0 X         
							7771          L   SS_COBBLE      0 X         
							7771          L   SS_GRAVEL      2 X         
							7771          L  SS_ORGANIC      0 X         
							7771          L    SS_OTHER      0 X         
							7771          L     SS_SAND      0 X         
							7771          L     SS_SILT      2 X         
							7771          L     SS_WOOD      0 X         
							7784          J  SS_BEDROCK      0 X         
							7784          J SS_BOULDERS      0 X         
							7784          J   SS_COBBLE      0 X         
							7784          J   SS_GRAVEL      0 X         
							7784          J  SS_ORGANIC      0 X         
							7784          J    SS_OTHER      1 X         
							7784          J     SS_SAND      4 X         
							7784          J     SS_SILT      0 X         
							7784          J     SS_WOOD      1 X         
							7961          A  SS_BEDROCK      0 X         
							7961          A SS_BOULDERS      0 X         
							7961          A   SS_COBBLE      2 X         
							7961          A    SS_OTHER      2 X         
							7961          A     SS_SAND      0 X         
							7961          A     SS_SILT      2 X         
							7961          B  SS_BEDROCK      0 X         
							7961          B SS_BOULDERS      3 X         
							7961          B   SS_COBBLE      0 X         
							7961          B    SS_OTHER      3 X         
							7961          B     SS_SAND      0 X         
							7961          B     SS_SILT      2 X         
							7961          C  SS_BEDROCK      0 X         
							7961          C SS_BOULDERS      2 X         
							7961          C   SS_COBBLE      2 X         
							7961          C    SS_OTHER      4 X         
							7961          C     SS_SAND      0 X         
							7961          C     SS_SILT      2 X         
							7961          E  SS_BEDROCK      0 X         
							7961          E SS_BOULDERS      3 X         
							7961          E   SS_COBBLE      0 X         
							7961          E    SS_OTHER      3 X         
							7961          E     SS_SAND      0 X         
							7961          E     SS_SILT      2 X         
							7961          F  SS_BEDROCK      0 X         
							7961          F SS_BOULDERS      3 X         
							7961          F   SS_COBBLE      0 X         
							7961          F    SS_OTHER      3 X         
							7961          F     SS_SAND      0 X         
							7961          F     SS_SILT      2 X         
							7961          G  SS_BEDROCK      3 X         
							7961          G SS_BOULDERS      2 X         
							7961          G   SS_COBBLE      0 X         
							7961          G    SS_OTHER      4 X         
							7961          G     SS_SAND      0 X         
							7961          G     SS_SILT      1 X         
							7961          H  SS_BEDROCK      0 X         
							7961          H SS_BOULDERS      0 X         
							7961          H   SS_COBBLE      0 X         
							7961          H    SS_OTHER      0 X         
							7961          H     SS_SAND      4 X         
							7961          H     SS_SILT      0 X         
							7961          I  SS_BEDROCK      3 X         
							7961          I SS_BOULDERS      1 X         
							7961          I   SS_COBBLE      0 X         
							7961          I    SS_OTHER      3 X         
							7961          I     SS_SAND      0 X         
							7961          I     SS_SILT      2 X         
							7961          J  SS_BEDROCK      2 X         
							7961          J SS_BOULDERS      2 X         
							7961          J   SS_COBBLE      2 X         
							7961          J    SS_OTHER      3 X         
							7961          J     SS_SAND      0 X         
							7961          J     SS_SILT      1 X         
							7961          Z  SS_BEDROCK      0 X         
							7961          Z SS_BOULDERS      1 X         
							7961          Z   SS_COBBLE      1 X         
							7961          Z    SS_OTHER      3 X         
							7961          Z     SS_SAND      1 X         
							7961          Z     SS_SILT      1 X"
		)
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaShorelineSubstrateTest.expectedResults2007 <- function()
# Creates expected VALUEs from the calculation with 2007 data.  The values
# are identical to the 2007 VALUEs except for:
#	removed trailing zeros
# 	changed ssfcSilt at 7684 from 0.074638962569997 to 0.0746389625699971
#   changed ssiSiteVariety at 7533, 7682, 7961 from NA to 4, 6 and 5 respectively.   
#
{
	tc <- textConnection("   SITE      METRIC                       VALUE
							7469       SS16LDIA        -0.351820729134696
							7469       SS25LDIA        -0.351820729134696
							7469       SS50LDIA        -0.351820729134696
							7469       SS75LDIA        -0.351820729134696
							7469       SS84LDIA        -0.351820729134696
							7469    SSFCBEDROCK                         0
							7469   SSFCBOULDERS                         0
							7469     SSFCCOBBLE                         0
							7469     SSFCGRAVEL                         0
							7469    SSFCORGANIC         0.273282967032967
							7469      SSFCOTHER         0.684230397980398
							7469       SSFCSAND                         0
							7469       SSFCSILT         0.042486634986635
							7469       SSFCWOOD                         0
							7469    SSFPBEDROCK                         0
							7469   SSFPBOULDERS                         0
							7469     SSFPCOBBLE                         0
							7469     SSFPGRAVEL                         0
							7469    SSFPORGANIC                       0.7
							7469      SSFPOTHER                         1
							7469       SSFPSAND                         0
							7469       SSFPSILT                       0.9
							7469       SSFPWOOD                         0
							7469 SSISITEVARIETY                         2
							7469  SSISTAVARIETY                       2.6
							7469     SSNBEDROCK                        10
							7469    SSNBOULDERS                        10
							7469      SSNCOBBLE                        10
							7469      SSNGRAVEL                        10
							7469     SSNORGANIC                        10
							7469       SSNOTHER                        10
							7469        SSNSAND                        10
							7469        SSNSILT                        10
							7469        SSNWOOD                        10
							7469      SSOFCLASS                     Other
							7469      SSOPCLASS                     Other
							7469     SSVBEDROCK                         0
							7469    SSVBOULDERS                         0
							7469      SSVCOBBLE                         0
							7469      SSVGRAVEL                         0
							7469        SSVLDIA          0.11125548321268
							7469     SSVORGANIC         0.232242730627468
							7469       SSVOTHER         0.234081298085771
							7469        SSVSAND                         0
							7469        SSVSILT        0.0162215578652508
							7469        SSVWOOD                         0
							7469        SSXLDIA        -0.316638656221227
							7533       SS16LDIA        -0.422184874961636
							7533       SS25LDIA        -0.422184874961636
							7533       SS50LDIA        -0.394676291664436
							7533       SS75LDIA        -0.251820729134696
							7533       SS84LDIA          0.12653631197718
							7533    SSFCBEDROCK                         0
							7533   SSFCBOULDERS        0.0731385281385281
							7533     SSFCCOBBLE        0.0583333333333333
							#7533     SSFCGRAVEL                        NA
							#7533    SSFCORGANIC                        NA
							7533      SSFCOTHER         0.658291798291798
							7533       SSFCSAND        0.0307142857142857
							7533       SSFCSILT         0.179522054522055
							#7533       SSFCWOOD                        NA
							7533    SSFPBEDROCK                         0
							7533   SSFPBOULDERS                       0.4
							7533     SSFPCOBBLE                       0.2
							#7533     SSFPGRAVEL                        NA
							#7533    SSFPORGANIC                        NA
							7533      SSFPOTHER                       0.9
							7533       SSFPSAND                       0.2
							7533       SSFPSILT                       0.9
							#7533       SSFPWOOD                        NA
							7533 SSISITEVARIETY                         4	# SAS VALUE IS NA
							7533  SSISTAVARIETY                       2.6
							7533     SSNBEDROCK                        10
							7533    SSNBOULDERS                        10
							7533      SSNCOBBLE                        10
							7533      SSNGRAVEL                         0
							7533     SSNORGANIC                         0
							7533       SSNOTHER                        10
							7533        SSNSAND                        10
							7533        SSNSILT                        10
							7533        SSNWOOD                         0
							7533      SSOFCLASS                     Other
							7533      SSOPCLASS             'Silt, Other'
							7533     SSVBEDROCK                         0
							7533    SSVBOULDERS         0.120944632447512
							7533      SSVCOBBLE         0.124536176508111
							#7533      SSVGRAVEL                        NA
							7533        SSVLDIA          0.31256342312997
							#7533     SSVORGANIC                        NA
							7533       SSVOTHER         0.318492442412448
							7533        SSVSAND        0.0791142862875257
							7533        SSVSILT         0.154059845285864
							#7533        SSVWOOD                        NA
							7533        SSXLDIA        -0.234499123033483
							7611       SS16LDIA         0.349056878915071
							7611       SS25LDIA         0.349056878915071
							7611       SS50LDIA         0.374749325057186
							7611       SS75LDIA         0.384981155253219
							7611       SS84LDIA         0.425171665943997
							7611    SSFCBEDROCK       0.00727272727272727
							7611   SSFCBOULDERS          0.17146322173361
							7611     SSFCCOBBLE         0.661582057203686
							7611     SSFCGRAVEL        0.0331108114320474
							7611    SSFCORGANIC       0.00727272727272727
							7611      SSFCOTHER         0.060229455499844
							7611       SSFCSAND                         0
							7611       SSFCSILT                         0
							7611       SSFCWOOD        0.0590689995853575
							7611    SSFPBEDROCK                       0.2
							7611   SSFPBOULDERS                         1
							7611     SSFPCOBBLE                         1
							7611     SSFPGRAVEL                       0.8
							7611    SSFPORGANIC                       0.2
							7611      SSFPOTHER                       0.8
							7611       SSFPSAND                         0
							7611       SSFPSILT                         0
							7611       SSFPWOOD                       0.8
							7611 SSISITEVARIETY                         6
							7611  SSISTAVARIETY                       4.8
							7611     SSNBEDROCK                         5
							7611    SSNBOULDERS                         5
							7611      SSNCOBBLE                         5
							7611      SSNGRAVEL                         5
							7611     SSNORGANIC                         5
							7611       SSNOTHER                         5
							7611        SSNSAND                         5
							7611        SSNSILT                         5
							7611        SSNWOOD                         5
							7611      SSOFCLASS                    Cobble
							7611      SSOPCLASS        'Boulders, Cobble'
							7611     SSVBEDROCK        0.0162623125636348
							7611    SSVBOULDERS         0.148337569756132
							7611      SSVCOBBLE         0.179132882944318
							7611      SSVGRAVEL        0.0195745210018099
							7611        SSVLDIA         0.031424999925648
							7611     SSVORGANIC        0.0162623125636348
							7611       SSVOTHER        0.0641377856884102
							7611        SSVSAND                         0
							7611        SSVSILT                         0
							7611        SSVWOOD        0.0690593263247773
							7611        SSXLDIA         0.376603180816909
							7682       SS16LDIA        -0.284052893614312
							7682       SS25LDIA       -0.0312849173637602
							7682       SS50LDIA        0.0496176419764834
							7682       SS75LDIA         0.150452383137773
							7682       SS84LDIA         0.151622144949178
							7682    SSFCBEDROCK                         0
							7682   SSFCBOULDERS        0.0163879618951405
							7682     SSFCCOBBLE          0.13785796331437
							7682     SSFCGRAVEL         0.292663476874003
							#7682    SSFCORGANIC                        NA
							7682      SSFCOTHER         0.487540089938608
							7682       SSFCSAND         0.127116723880181
							7682       SSFCSILT         0.197388990923515
							7682       SSFCWOOD        0.0425531914893617
							7682    SSFPBEDROCK                         0
							7682   SSFPBOULDERS         0.444444444444444
							7682     SSFPCOBBLE         0.888888888888889
							7682     SSFPGRAVEL                         1
							#7682    SSFPORGANIC                        NA
							7682      SSFPOTHER                         1
							7682       SSFPSAND                     0.875
							7682       SSFPSILT                         1
							7682       SSFPWOOD                         1
							7682 SSISITEVARIETY                         6	# SAS VALUE IS NA
							7682  SSISTAVARIETY          4.33333333333333
							7682     SSNBEDROCK                         9
							7682    SSNBOULDERS                         9
							7682      SSNCOBBLE                         9
							7682      SSNGRAVEL                         2
							7682     SSNORGANIC                         0
							7682       SSNOTHER                         9
							7682        SSNSAND                         8
							7682        SSNSILT                         8
							7682        SSNWOOD                         1
							7682      SSOFCLASS                     Other
							7682      SSOPCLASS 'Gravel, Silt, Wood, Other'
							7682     SSVBEDROCK                         0
							7682    SSVBOULDERS        0.0197354425735443
							7682      SSVCOBBLE        0.0784286532084917
							7682      SSVGRAVEL          0.15675891959319
							7682        SSVLDIA         0.30946896985605
							#7682     SSVORGANIC                        NA
							7682       SSVOTHER          0.19175494048696
							7682        SSVSAND        0.0870000604238252
							7682        SSVSILT         0.179399182176652
							7682        SSVWOOD                        NA
							7682        SSXLDIA        0.0511439221776934
							7684       SS16LDIA        -0.351820729134696
							7684       SS25LDIA        -0.351820729134696
							7684       SS50LDIA       -0.0767348961626979
							7684       SS75LDIA         0.211632551918651
							7684       SS84LDIA         0.211632551918651
							7684    SSFCBEDROCK                         0
							7684   SSFCBOULDERS         0.172480620155039
							7684     SSFCCOBBLE        0.0555555555555556
							7684     SSFCGRAVEL        0.0943152454780362
							7684    SSFCORGANIC                         0
							7684      SSFCOTHER         0.100574712643678
							7684       SSFCSAND         0.558467616607151
							7684       SSFCSILT        0.0746389625699971
							7684       SSFCWOOD        0.0175560175560176
							7684    SSFPBEDROCK                         0
							7684   SSFPBOULDERS         0.333333333333333
							7684     SSFPCOBBLE         0.166666666666667
							7684     SSFPGRAVEL         0.333333333333333
							7684    SSFPORGANIC                         0
							7684      SSFPOTHER         0.166666666666667
							7684       SSFPSAND         0.857142857142857
							7684       SSFPSILT         0.333333333333333
							7684       SSFPWOOD         0.333333333333333
							7684 SSISITEVARIETY                         6
							7684  SSISTAVARIETY          2.28571428571429
							7684     SSNBEDROCK                         6
							7684    SSNBOULDERS                         6
							7684      SSNCOBBLE                         6
							7684      SSNGRAVEL                         6
							7684     SSNORGANIC                         6
							7684       SSNOTHER                         6
							7684        SSNSAND                         7
							7684        SSNSILT                         6
							7684        SSNWOOD                         6
							7684      SSOFCLASS                      Sand
							7684      SSOPCLASS                      Sand
							7684     SSVBEDROCK                         0
							7684    SSVBOULDERS         0.267433434312829
							7684      SSVCOBBLE         0.136082763487954
							7684      SSVGRAVEL          0.14954745480786
							7684        SSVLDIA         0.284650188281221
							7684     SSVORGANIC                         0
							7684       SSVOTHER         0.246356727004055
							7684        SSVSAND         0.394165102979508
							7684        SSVSILT         0.159032871004273
							7684        SSVWOOD        0.0272117879927445
							7684        SSXLDIA       -0.0469666415053573
							7771       SS16LDIA       -0.0881099491653537
							7771       SS25LDIA        0.0842654291946608
							7771       SS50LDIA         0.202076967331869
							7771       SS75LDIA          0.31501193892245
							7771       SS84LDIA         0.402483408203996
							7771    SSFCBEDROCK                         0
							7771   SSFCBOULDERS         0.190451311140966
							7771     SSFCCOBBLE         0.191100661790317
							7771     SSFCGRAVEL         0.369821265510921
							7771    SSFCORGANIC                         0
							7771      SSFCOTHER         0.113090363090363
							7771       SSFCSAND                         0
							7771       SSFCSILT         0.135536398467433
							7771       SSFCWOOD                         0
							7771    SSFPBEDROCK                         0
							7771   SSFPBOULDERS         0.583333333333333
							7771     SSFPCOBBLE         0.583333333333333
							7771     SSFPGRAVEL         0.833333333333333
							7771    SSFPORGANIC                         0
							7771      SSFPOTHER                      0.25
							7771       SSFPSAND                         0
							7771       SSFPSILT         0.333333333333333
							7771       SSFPWOOD                         0
							7771 SSISITEVARIETY                         4
							7771  SSISTAVARIETY          2.58333333333333
							7771     SSNBEDROCK                        12
							7771    SSNBOULDERS                        12
							7771      SSNCOBBLE                        12
							7771      SSNGRAVEL                        12
							7771     SSNORGANIC                        12
							7771       SSNOTHER                        12
							7771        SSNSAND                        12
							7771        SSNSILT                        12
							7771        SSNWOOD                        12
							7771      SSOFCLASS                    Gravel
							7771      SSOPCLASS                    Gravel
							7771     SSVBEDROCK                         0
							7771    SSVBOULDERS         0.293504320953716
							7771      SSVCOBBLE         0.224280188238825
							7771      SSVGRAVEL         0.335442073687158
							7771        SSVLDIA         0.203958857010393
							7771     SSVORGANIC                         0
							7771       SSVOTHER         0.292524442277501
							7771        SSVSAND                         0
							7771        SSVSILT         0.257261123162163
							7771        SSVWOOD                         0
							7771        SSXLDIA         0.184658264235294
							7784       SS16LDIA       -0.0767348961626979
							7784       SS25LDIA       -0.0767348961626979
							7784       SS50LDIA       -0.0767348961626979
							7784       SS75LDIA       -0.0767348961626979
							7784       SS84LDIA       -0.0767348961626979
							7784    SSFCBEDROCK                         0
							7784   SSFCBOULDERS                         0
							7784     SSFCCOBBLE                         0
							7784     SSFCGRAVEL                         0
							7784    SSFCORGANIC                         0
							7784      SSFCOTHER        0.0512820512820513
							7784       SSFCSAND         0.897435897435898
							7784       SSFCSILT                         0
							7784       SSFCWOOD        0.0512820512820513
							7784    SSFPBEDROCK                         0
							7784   SSFPBOULDERS                         0
							7784     SSFPCOBBLE                         0
							7784     SSFPGRAVEL                         0
							7784    SSFPORGANIC                         0
							7784      SSFPOTHER                         1
							7784       SSFPSAND                         1
							7784       SSFPSILT                         0
							7784       SSFPWOOD                         1
							7784 SSISITEVARIETY                         2
							7784  SSISTAVARIETY                         3
							7784     SSNBEDROCK                         1
							7784    SSNBOULDERS                         1
							7784      SSNCOBBLE                         1
							7784      SSNGRAVEL                         1
							7784     SSNORGANIC                         1
							7784       SSNOTHER                         1
							7784        SSNSAND                         1
							7784        SSNSILT                         1
							7784        SSNWOOD                         1
							7784      SSOFCLASS                      Sand
							7784      SSOPCLASS       'Sand, Wood, Other'
							7784     SSVBEDROCK                        NA
							7784    SSVBOULDERS                        NA
							7784      SSVCOBBLE                        NA
							7784      SSVGRAVEL                        NA
							7784        SSVLDIA                        NA
							7784     SSVORGANIC                        NA
							7784       SSVOTHER                        NA
							7784        SSVSAND                        NA
							7784        SSVSILT                        NA
							7784        SSVWOOD                        NA
							7784        SSXLDIA       -0.0767348961626979
							7961       SS16LDIA     -0.000886438348021557
							7961       SS25LDIA          0.12653631197718
							7961       SS50LDIA         0.290247007587383
							7961       SS75LDIA         0.406857034300555
							7961       SS84LDIA         0.527028131595393
							7961    SSFCBEDROCK        0.0906941334527541
							7961   SSFCBOULDERS         0.180966322331728
							7961     SSFCCOBBLE        0.0733513798029927
							#7961     SSFCGRAVEL                        NA
							#7961    SSFCORGANIC                        NA
							7961      SSFCOTHER         0.416060675512845
							7961       SSFCSAND         0.106451612903226
							7961       SSFCSILT         0.132475875996454
							#7961       SSFCWOOD                        NA
							7961    SSFPBEDROCK                       0.3
							7961   SSFPBOULDERS                       0.8
							7961     SSFPCOBBLE                       0.4
							#7961     SSFPGRAVEL                        NA
							#7961    SSFPORGANIC                        NA
							7961      SSFPOTHER                       0.9
							7961       SSFPSAND                       0.2
							7961       SSFPSILT                       0.9
							#7961       SSFPWOOD                        NA
							7961 SSISITEVARIETY                         5	# SAS VALUE IS NA
							7961  SSISTAVARIETY                       3.5
							7961     SSNBEDROCK                        10
							7961    SSNBOULDERS                        10
							7961      SSNCOBBLE                        10
							7961      SSNGRAVEL                         0
							7961     SSNORGANIC                         0
							7961       SSNOTHER                        10
							7961        SSNSAND                        10
							7961        SSNSILT                        10
							7961        SSNWOOD                         0
							7961      SSOFCLASS                     Other
							7961      SSOPCLASS             'Silt, Other'
							7961     SSVBEDROCK         0.154925088134895
							7961    SSVBOULDERS         0.170312334123812
							7961      SSVCOBBLE         0.114598663517179
							#7961      SSVGRAVEL                        NA
							7961        SSVLDIA         0.224423351784493
							#7961     SSVORGANIC                        NA
							7961       SSVOTHER         0.185185588922838
							7961        SSVSAND         0.314614909586717
							7961        SSVSILT          0.10053850881795
							#7961        SSVWOOD                        NA
							7961        SSXLDIA         0.267810251942798"
			)
			
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
			
	return(fake)			
}


# end of file