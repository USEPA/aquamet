# nlaLittoralZone.r
# RUnit tests
#
#    7/14/17 cws Renamed metsLittoralZone to nlaLittoralZone.
#

nlaLittoralZoneTest <- function()
# unit test for nlaLittoralZone
{
	nlaLittoralZoneTest.2007()
	nlaLittoralZoneTest.2012()
}


nlaLittoralZoneTest.2007 <- function()
# unit test for nlaLittoralZone using 2007 data
{
	testData <- nlaLittoralZoneTest.createTestData2007()
	expected <- nlaLittoralZoneTest.createExpectedResults2007()
	actual <- nlaLittoralZone(testData %>% select(SITE, STATION, VALUE)
	                         ,data2007=TRUE
	                         )

	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
	
}


nlaLittoralZoneTest.2012 <- function()
# unit test for nlaLittoralZone using 2012 data
{
	testData <- nlaLittoralZoneTest.createTestData2012()
	expected <- nlaLittoralZoneTest.createExpectedResults2012()
	actual <- nlaLittoralZone(testData %>% select(SITE, STATION, VALUE), data2007=FALSE)
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
	
}


nlaLittoralZoneTest.createTestData2007 <- function()
# Selected data from 2007 study:
# 8020	Has 2 extra stations of data.  All are N
# 8188	No N, all 10 stations are O
# 8602	Great variety: 4A, 3N, 2O, 1S
# 8637	No N, all 10 stations are S
# 8656	6 S, 4N
# 8665	Some variety: 1A, 2O, 7N
# 8682	No N: 9A, 1S
# 8859	8N, 2O
# 8884	5N, 5 absent stations
{
	tc <- textConnection("   SITE    STATION   PARAMETER  VALUE
							8020          A SURFACE_FILM      N
							8020          B SURFACE_FILM      N
							8020          C SURFACE_FILM      N
							8020          D SURFACE_FILM      N
							8020          E SURFACE_FILM      N
							8020          F SURFACE_FILM      N
							8020          G SURFACE_FILM      N
							8020          H SURFACE_FILM      N
							8020          I SURFACE_FILM      N
							8020          J SURFACE_FILM      N
							8020          K SURFACE_FILM      N
							8020          L SURFACE_FILM      N
							8188          A SURFACE_FILM      N
							8188          B SURFACE_FILM      N
							8188          C SURFACE_FILM      N
							8188          D SURFACE_FILM      N
							8188          E SURFACE_FILM      N
							8188          F SURFACE_FILM      N
							8188          G SURFACE_FILM      N
							8188          H SURFACE_FILM      N
							8188          I SURFACE_FILM      N
							8188          J SURFACE_FILM      P
							8602          A SURFACE_FILM      A
							8602          B SURFACE_FILM      S
							8602          C SURFACE_FILM      A
							8602          D SURFACE_FILM      A
							8602          E SURFACE_FILM      N
							8602          F SURFACE_FILM      A
							8602          G SURFACE_FILM      N
							8602          H SURFACE_FILM      N
							8602          I SURFACE_FILM      O
							8602          J SURFACE_FILM      O
							8637          A SURFACE_FILM      S
							8637          B SURFACE_FILM      S
							8637          C SURFACE_FILM      S
							8637          D SURFACE_FILM      S
							8637          E SURFACE_FILM      S
							8637          F SURFACE_FILM      S
							8637          G SURFACE_FILM      S
							8637          H SURFACE_FILM      S
							8637          I SURFACE_FILM      S
							8637          J SURFACE_FILM      S
							8656          A SURFACE_FILM      S
							8656          B SURFACE_FILM      S
							8656          C SURFACE_FILM      S
							8656          D SURFACE_FILM      N
							8656          E SURFACE_FILM      S
							8656          F SURFACE_FILM      S
							8656          G SURFACE_FILM      N
							8656          H SURFACE_FILM      N
							8656          I SURFACE_FILM      S
							8656          J SURFACE_FILM      N
							8665          A SURFACE_FILM      N
							8665          B SURFACE_FILM      N
							8665          C SURFACE_FILM      N
							8665          D SURFACE_FILM      O
							8665          E SURFACE_FILM      O
							8665          F SURFACE_FILM      N
							8665          G SURFACE_FILM      N
							8665          H SURFACE_FILM      A
							8665          I SURFACE_FILM      N
							8665          J SURFACE_FILM      N
							8682          A SURFACE_FILM      A
							8682          B SURFACE_FILM      A
							8682          C SURFACE_FILM      A
							8682          D SURFACE_FILM      A
							8682          E SURFACE_FILM      S
							8682          F SURFACE_FILM      A
							8682          G SURFACE_FILM      A
							8682          H SURFACE_FILM      A
							8682          I SURFACE_FILM      A
							8682          J SURFACE_FILM      A
							8859          A SURFACE_FILM      N
							8859          B SURFACE_FILM      N
							8859          C SURFACE_FILM      N
							8859          D SURFACE_FILM      N
							8859          E SURFACE_FILM      N
							8859          F SURFACE_FILM      N
							8859          G SURFACE_FILM      N
							8859          H SURFACE_FILM      N
							8859          I SURFACE_FILM      O
							8859          J SURFACE_FILM      O
							8884          A SURFACE_FILM      N
							8884          B SURFACE_FILM      N
							8884          C SURFACE_FILM      N
							8884          D SURFACE_FILM      N
							8884          E SURFACE_FILM      N
						")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaLittoralZoneTest.createExpectedResults2007 <- function()
#
{
	
	tc <- textConnection("   SITE	  METRIC VALUE
							8020       LZFPFILM      0
							8020 LZIFILMVARIETY      0
							8020        LZOFILM      N
							8188       LZFPFILM    0.1
							8188 LZIFILMVARIETY    0.1
							8188        LZOFILM      N
							8602       LZFPFILM    0.7
							8602 LZIFILMVARIETY    0.7
							8602        LZOFILM      A
							8637       LZFPFILM      1
							8637 LZIFILMVARIETY      1
							8637        LZOFILM      S
							8656       LZFPFILM    0.6
							8656 LZIFILMVARIETY    0.6
							8656        LZOFILM      S
							8665       LZFPFILM    0.3
							8665 LZIFILMVARIETY    0.3
							8665        LZOFILM      N
							8682       LZFPFILM      1
							8682 LZIFILMVARIETY      1
							8682        LZOFILM      A
							8859       LZFPFILM    0.2
							8859 LZIFILMVARIETY    0.2
							8859        LZOFILM      N
							8884       LZFPFILM      0
							8884 LZIFILMVARIETY      0
							8884        LZOFILM      N
						")
		
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
			
	return(fake)		
}

nlaLittoralZoneTest.createTestData2012 <- function()
# Selected data from 2012 study:
# 6160		all NONE
# 6233		1 Algal, 9 NONE
# 6249		8 Algal, 2 NONE
# 6321		0 NONE
# 6412		Mostly not NONE
# 7802		only 2 values recorded
# 7943		Tied Mode
# 8324		2 Algal and 1 OTHER_ALGAL and 6 NONE
# 1000118	10 OTHER_FLOATING
{
	tc <- textConnection("   SITE STATION    PARAMETER                       VALUE
							6160       A SURFACE_FILM                         NONE
							6160       B SURFACE_FILM                         NONE
							6160       C SURFACE_FILM                         NONE
							6160       D SURFACE_FILM                         NONE
							6160       E SURFACE_FILM                         NONE
							6160       F SURFACE_FILM                         NONE
							6160       G SURFACE_FILM                         NONE
							6160       H SURFACE_FILM                         NONE
							6160       I SURFACE_FILM                         NONE
							6160       J SURFACE_FILM                         NONE
							6233       A SURFACE_FILM                         NONE
							6233       B SURFACE_FILM                         NONE
							6233       C SURFACE_FILM                         NONE
							6233       D SURFACE_FILM                         NONE
							6233       E SURFACE_FILM                    ALGAL_MAT
							6233       F SURFACE_FILM                         NONE
							6233       G SURFACE_FILM                         NONE
							6233       H SURFACE_FILM                         NONE
							6233       I SURFACE_FILM                         NONE
							6233       J SURFACE_FILM                         NONE
							6249       A SURFACE_FILM                    ALGAL_MAT
							6249       B SURFACE_FILM                    ALGAL_MAT
							6249       C SURFACE_FILM                    ALGAL_MAT
							6249       D SURFACE_FILM                         NONE
							6249       E SURFACE_FILM                         NONE
							6249       F SURFACE_FILM                    ALGAL_MAT
							6249       G SURFACE_FILM                    ALGAL_MAT
							6249       H SURFACE_FILM                    ALGAL_MAT
							6249       I SURFACE_FILM                    ALGAL_MAT
							6249       J SURFACE_FILM                    ALGAL_MAT
							6321       A SURFACE_FILM                         SCUM
							6321       B SURFACE_FILM                         SCUM
							6321       C SURFACE_FILM                         SCUM
							6321       D SURFACE_FILM                         SCUM
							6321       E SURFACE_FILM                         SCUM
							6321       F SURFACE_FILM                         SCUM
							6321       G SURFACE_FILM                         SCUM
							6321       H SURFACE_FILM                         SCUM
							6321       I SURFACE_FILM                         SCUM
							6321       J SURFACE_FILM                    ALGAL_MAT
							6412       A SURFACE_FILM                         OILY
							6412       B SURFACE_FILM                         NONE
							6412       C SURFACE_FILM                         NONE
							6412       D SURFACE_FILM                         OILY
							6412       E SURFACE_FILM                         OILY
							6412       F SURFACE_FILM                         SCUM
							6412       G SURFACE_FILM                         SCUM
							6412       H SURFACE_FILM                         SCUM
							6412       I SURFACE_FILM                         OILY
							6412       J SURFACE_FILM                         OILY
							7802       G SURFACE_FILM                         OILY
							7802       J SURFACE_FILM                         NONE
							7943       A SURFACE_FILM                         NONE
							7943       B SURFACE_FILM                         NONE
							7943       C SURFACE_FILM                    ALGAL_MAT
							7943       D SURFACE_FILM                         SCUM
							7943       E SURFACE_FILM                         NONE
							7943       F SURFACE_FILM                         NONE
							7943       G SURFACE_FILM                         NONE
							7943       H SURFACE_FILM                         NONE
							7943       I SURFACE_FILM                         NONE
							7943       J SURFACE_FILM                         NONE
							8324       A SURFACE_FILM                         NONE
							8324       B SURFACE_FILM                         NONE
							8324       C SURFACE_FILM                         NONE
							8324       D SURFACE_FILM                         NONE
							8324       E SURFACE_FILM                         NONE
							8324       F SURFACE_FILM                    ALGAL_MAT
							8324       G SURFACE_FILM 'OTHER_ALGAL MAT AND DUCKWEED'
							8324       H SURFACE_FILM                    ALGAL_MAT
							8324       I SURFACE_FILM                         NONE
							8324       J SURFACE_FILM                         NONE
							1000118    A SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    B SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    C SURFACE_FILM  'OTHER_MACROPHYTE FLOATING'
							1000118    D SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    E SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    F SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    G SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    H SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    I SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
							1000118    J SURFACE_FILM 'OTHER_FLOATING MACROPHYTES'
						")
		
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
			
	return(fake)		
}


nlaLittoralZoneTest.createExpectedResults2012 <- function()
#
{
	
	tc <- textConnection("   SITE    METRIC	VALUE
							6160 	LZFPALGAE        0
							6160  	LZFPFILM         0
							6160  	LZFPNONE         1
							6160  	LZFPOILY         0
							6160 	LZFPOTHER        0
							6160  	LZFPSCUM         0
							6160   	LZOFILM       NONE
							6233 	LZFPALGAE      0.1
							6233  	LZFPFILM       0.1
							6233  	LZFPNONE       0.9
							6233  	LZFPOILY         0
							6233 	LZFPOTHER        0
							6233  	LZFPSCUM         0
							6233   	LZOFILM       NONE
							6249 	LZFPALGAE      0.8
							6249  	LZFPFILM       0.8
							6249  	LZFPNONE       0.2
							6249  	LZFPOILY         0
							6249 	LZFPOTHER        0
							6249  	LZFPSCUM         0
							6249   	LZOFILM  ALGAL_MAT
							6321 	LZFPALGAE      0.1
							6321  	LZFPFILM         1
							6321  	LZFPNONE         0
							6321  	LZFPOILY         0
							6321 	LZFPOTHER        0
							6321  	LZFPSCUM       0.9
							6321   	LZOFILM       SCUM
							6412 	LZFPALGAE        0
							6412  	LZFPFILM       0.8
							6412  	LZFPNONE       0.2
							6412  	LZFPOILY       0.5
							6412 	LZFPOTHER        0
							6412  	LZFPSCUM       0.3
							6412   	LZOFILM       OILY
							7802 	LZFPALGAE        0
							7802  	LZFPFILM       0.5
							7802  	LZFPNONE       0.5
							7802  	LZFPOILY       0.5
							7802 	LZFPOTHER        0
							7802  	LZFPSCUM         0
							7802   	LZOFILM       NONE&OILY
							7943 	LZFPALGAE      0.1
							7943  	LZFPFILM       0.2
							7943  	LZFPNONE       0.8
							7943  	LZFPOILY         0
							7943 	LZFPOTHER        0
							7943  	LZFPSCUM       0.1
							7943   	LZOFILM       NONE
							8324 	LZFPALGAE      0.2
							8324  	LZFPFILM       0.3
							8324  	LZFPNONE       0.7
							8324  	LZFPOILY         0
							8324 	LZFPOTHER      0.1
							8324  	LZFPSCUM         0
							8324   	LZOFILM       NONE
							1000118 LZFPALGAE        0
							1000118  LZFPFILM        1
							1000118  LZFPNONE        0
							1000118  LZFPOILY        0
							1000118 LZFPOTHER        1
							1000118  LZFPSCUM        0
							1000118   LZOFILM    OTHER
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}

# end of file