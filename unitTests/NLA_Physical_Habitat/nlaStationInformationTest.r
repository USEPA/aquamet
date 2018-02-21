# nlaStationInformation.r
# RUnit tests
#
#  7/18/17 cws Created from metsStationInformation.r
#  7/19/17 cws Modified test for 2007 data to update input for isIsland argument
#          reflecting changes in the metrics calculation. This means making
#          false values of ISLAND explicitly present in the test with 2007 data.
#          It also results in a change in values for 2012 data for sites 6683 
#          and 6794 for which there is no ISLAND data; previously these resolved 
#          to 0 but are now absent.
#


nlaStationInformationTest <- function()
# unit test for nlaStationInformation
{
	nlaStationInformationTest.2007()
	nlaStationInformationTest.2012()
}


nlaStationInformationTest.2007 <- function()
# Unit test for nlaLittoralMacrohabitat using 2007 data. This differs from 2012
# data in having depths in both feet and meters, and ISLAND is either X or absent.
{
	testData <- nlaStationInformationTest.createTestData2007()
	expected <- nlaStationInformationTest.createExpectedResults2007()
	# actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER == 'ISLAND') %>% select(SITE,STATION,VALUE)
	#                                ,stationDepth = testData %>% 
	#                                                subset(PARAMETER == 'DEPTH_AT_STATION' & !is.na(UNITS)) %>% 
	#                                                mutate(VALUE = ifelse(toupper(UNITS) %in% 'FT', as.numeric(VALUE) * 0.3048, VALUE)) %>% 
	#                                                select(SITE,STATION,VALUE)
	#                                )
	actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER == 'ISLAND') %>% 
	                                           merge(testData %>% select(SITE, STATION) %>% unique() 
	                                                ,by=c('SITE','STATION'), all=TRUE
	                                                ) %>%
	                                           mutate(VALUE = ifelse(is.na(VALUE), 'NO', 'YES')) %>%
	                                           select(SITE,STATION,VALUE)
	                               ,stationDepth = testData %>% 
	                                               subset(PARAMETER == 'DEPTH_AT_STATION' & !is.na(UNITS)) %>% 
	                                               mutate(VALUE = ifelse(toupper(UNITS) %in% 'FT', as.numeric(VALUE) * 0.3048, VALUE)) %>% 
	                                               select(SITE,STATION,VALUE)
	                               )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
#return(actual)	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
#return(diff)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}


nlaStationInformationTest.2012 <- function()
# Unit test for nlaLittoralMacrohabitat using 2012 data.  All depths are in meters,
# and ISLAND is N, NO or YES.
{
	testData <- nlaStationInformationTest.createTestData2012()
	
	# Test case with normal data in both arguments
	expected <- nlaStationInformationTest.createExpectedResults2012()
	actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER=='ISLAND') %>% select(SITE,STATION,VALUE)
	                               ,stationDepth = testData %>% subset(PARAMETER=='DEPTH_AT_STATION') %>% select(SITE,STATION,VALUE)
	                               )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-9)
#return(diff)
	checkTrue(is.null(diff), "Incorrect calculation of metrics when both arguments have normal data")
	
	
	# Test case with zero rows in one argument
	expected <- nlaStationInformationTest.createExpectedResults2012() %>%
	            subset(METRIC != 'SIFPISLAND')
	actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER=='ISLAND') %>% select(SITE,STATION,VALUE) %>% subset(FALSE)
	                               ,stationDepth = testData %>% subset(PARAMETER=='DEPTH_AT_STATION') %>% select(SITE,STATION,VALUE)
	                               )
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics when isIsland has zero rows")

	expected <- nlaStationInformationTest.createExpectedResults2012() %>%
	            subset(METRIC == 'SIFPISLAND')
	actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER=='ISLAND') %>% select(SITE,STATION,VALUE)
	                               ,stationDepth = testData %>% subset(PARAMETER=='DEPTH_AT_STATION') %>% select(SITE,STATION,VALUE) %>% subset(FALSE)
	                               )
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics when stationDepth has zero rows")

	
	# Test case with one argument is NULL
	expected <- nlaStationInformationTest.createExpectedResults2012() %>%
	            subset(METRIC != 'SIFPISLAND')
	actual <- nlaStationInformation(isIsland = NULL
	                               ,stationDepth = testData %>% subset(PARAMETER=='DEPTH_AT_STATION') %>% select(SITE,STATION,VALUE)
	                               )
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics when isIsland is NULL")

	expected <- nlaStationInformationTest.createExpectedResults2012() %>%
	            subset(METRIC == 'SIFPISLAND')
	actual <- nlaStationInformation(isIsland = testData %>% subset(PARAMETER=='ISLAND') %>% select(SITE,STATION,VALUE)
	                               ,stationDepth = NULL
	                               )
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics when stationDepth is NULL")

	
	# Test cases when both arguments are NULL
	expected <- nlaStationInformationTest.createExpectedResults2012() %>%
	            subset(FALSE)
	actual <- nlaStationInformation(isIsland = NULL
	                               ,stationDepth = NULL
	                               )
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics when both arguments are NULL")

}


nlaStationInformationTest.createTestData2007 <- function()
# Test data from 2007 data.  UNITS is used to allow depth to be expressed in ft or m
# and island presence is coded as X, and absence is coded by the absence of a record. 
#
# SITE	Description
# 7468	9 stations with data; UNITS has 1 M, 8 m; zero ISLAND
# 7474	10 stations with data; UNITS has 3 ft, 1 FT, 6 m; zero ISLAND
# 7481	10 stations with data; UNITS all ft; zero ISLAND
# 7493	10 stations with data; UNITS has 1 FT, 9 m; zero ISLAND
# 7516	9 stations with data; UNITS 4 ft, 5 NA; zero ISLAND
# 7518	11 stations with data; UNITS has 10 m, 1 M; 1 ISLAND
# 7648	10 stations with data; UNITS has 2 ft, 7 m, 1 NA; zero ISLAND
# 7797	12 stations with data; UNITS has 13 m; 3 ISLAND
# 8022	10 stations with data; UNITS has 1 ft, 2 FT, 4 m, 2 M, 1 NA; zero ISLAND
#
{
	tc <- textConnection("   SITE STATION        PARAMETER VALUE UNITS
							7468       A DEPTH_AT_STATION     15 M         
							7474       A DEPTH_AT_STATION     12 FT        
							7481       A DEPTH_AT_STATION      6 ft        
							7493       A DEPTH_AT_STATION    0.8 m         
							7516       A DEPTH_AT_STATION     25 ft        
							7518       A DEPTH_AT_STATION    0.5 m         
							7648       A DEPTH_AT_STATION    2.5 NA
							7797       A DEPTH_AT_STATION    0.9 m         
							8022       A DEPTH_AT_STATION     10 FT        
							7468       B DEPTH_AT_STATION      1 m         
							7474       B DEPTH_AT_STATION      3 ft        
							7481       B DEPTH_AT_STATION      7 ft        
							7493       B DEPTH_AT_STATION     18 FT        
							7516       B DEPTH_AT_STATION     17 ft        
							7518       B DEPTH_AT_STATION    4.5 M         
							7648       B DEPTH_AT_STATION      1 m         
							7797       B DEPTH_AT_STATION    0.5 m         
							8022       B DEPTH_AT_STATION      8 ft        
							7468       C DEPTH_AT_STATION    1.5 m         
							7474       C DEPTH_AT_STATION    1.2 m         
							7481       C DEPTH_AT_STATION      5 ft        
							7493       C DEPTH_AT_STATION    0.7 m         
							7516       C DEPTH_AT_STATION     10 NA
							7518       C DEPTH_AT_STATION      1 m         
							7648       C DEPTH_AT_STATION    0.6 m         
							7797       C DEPTH_AT_STATION    0.4 m         
							8022       C DEPTH_AT_STATION    0.9 m         
							7468       D DEPTH_AT_STATION      1 m         
							7474       D DEPTH_AT_STATION    0.3 m         
							7481       D DEPTH_AT_STATION      8 ft        
							7493       D DEPTH_AT_STATION    0.1 m         
							7516       D DEPTH_AT_STATION     14 ft        
							7518       D DEPTH_AT_STATION    1.1 m         
							7648       D DEPTH_AT_STATION    0.6 m         
							7797       D DEPTH_AT_STATION    0.4 m         
							8022       D DEPTH_AT_STATION    0.8 M         
							7468       E DEPTH_AT_STATION    0.8 m         
							7474       E DEPTH_AT_STATION      3 ft        
							7481       E DEPTH_AT_STATION      5 ft        
							7493       E DEPTH_AT_STATION    0.7 m         
							7516       E DEPTH_AT_STATION     13 NA
							7518       E DEPTH_AT_STATION      1 m         
							7648       E DEPTH_AT_STATION      1 m         
							7797       E DEPTH_AT_STATION    0.5 m         
							8022       E DEPTH_AT_STATION    0.8 m         
							7468       F DEPTH_AT_STATION    1.2 m         
							7474       F DEPTH_AT_STATION      6 ft        
							7481       F DEPTH_AT_STATION      3 ft        
							7493       F DEPTH_AT_STATION    0.5 m         
							7518       F DEPTH_AT_STATION      3 m         
							7648       F DEPTH_AT_STATION    1.1 m         
							7797       F DEPTH_AT_STATION    1.2 m         
							8022       F DEPTH_AT_STATION    0.9 m         
							7468       G DEPTH_AT_STATION    1.4 m         
							7474       G DEPTH_AT_STATION    0.3 m         
							7481       G DEPTH_AT_STATION      9 ft        
							7493       G DEPTH_AT_STATION    0.6 m         
							7516       G DEPTH_AT_STATION      4 NA
							7518       G DEPTH_AT_STATION      2 m         
							7648       G DEPTH_AT_STATION    1.2 m         
							7797       G DEPTH_AT_STATION    0.6 m         
							8022       G DEPTH_AT_STATION    4.8 NA
							7468       H DEPTH_AT_STATION    2.3 m         
							7474       H DEPTH_AT_STATION      1 m         
							7481       H DEPTH_AT_STATION      7 ft        
							7493       H DEPTH_AT_STATION    0.4 m         
							7516       H DEPTH_AT_STATION      4 NA
							7518       H DEPTH_AT_STATION    0.8 m         
							7648       H DEPTH_AT_STATION    6.3 ft        
							7797       H DEPTH_AT_STATION    0.9 m         
							8022       H DEPTH_AT_STATION    1.5 M         
							7468       I DEPTH_AT_STATION    3.1 m         
							7474       I DEPTH_AT_STATION    0.7 m         
							7481       I DEPTH_AT_STATION      8 ft        
							7493       I DEPTH_AT_STATION    0.7 m         
							7516       I DEPTH_AT_STATION     32 NA
							7518       I DEPTH_AT_STATION    0.8 m         
							7648       I DEPTH_AT_STATION    4.4 ft        
							7797       I DEPTH_AT_STATION    0.4 m         
							8022       I DEPTH_AT_STATION    0.6 m         
							7474       J DEPTH_AT_STATION    0.6 m         
							7481       J DEPTH_AT_STATION      5 ft        
							7493       J DEPTH_AT_STATION    0.9 m         
							7516       J DEPTH_AT_STATION    5.5 ft        
							7518       J DEPTH_AT_STATION    1.4 m         
							7648       J DEPTH_AT_STATION      1 m         
							7797       J DEPTH_AT_STATION    1.1 m         
							8022       J DEPTH_AT_STATION     11 FT        
							7518       K DEPTH_AT_STATION    1.2 m         
							7518       K           ISLAND      X X         
							7797       K DEPTH_AT_STATION    0.4 m         
							7797       K           ISLAND      X X         
							7797       L DEPTH_AT_STATION    0.5 m         
							7797       L           ISLAND      X X         
							7797       M DEPTH_AT_STATION    0.6 m         
							7797       M           ISLAND      X X   
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaStationInformationTest.createExpectedResults2007 <- function()
#
{
	tc <- textConnection("   SITE  METRIC             VALUE
							7468 SIFPISLAND                  0
							7468   SINDEPTH                  9
							7468   SIVDEPTH    4.5467021015237
							7468   SIXDEPTH   3.03333333333333
							7474 SIFPISLAND                  0
							7474   SINDEPTH                 10
							7474   SIVDEPTH  0.990741743229676
							7474   SIXDEPTH            1.14152
							7481 SIFPISLAND                  0
							7481   SINDEPTH                 10
							7481   SIVDEPTH  0.557412823677389
							7481   SIXDEPTH            1.92024
							7493 SIFPISLAND                  0
							7493   SINDEPTH                 10
							7493   SIVDEPTH   1.56166629185339
							7493   SIXDEPTH            1.08864
							7516 SIFPISLAND                  0
							7516   SINDEPTH                  4
							7516   SIVDEPTH   2.45540632075427
							7516   SIXDEPTH             4.6863
							7518 SIFPISLAND 0.0909090909090909 # is 0.0833333333333333 with entire 2007 data
							7518   SINDEPTH                 11
							7518   SIVDEPTH   1.19087439227730
							7518   SIXDEPTH   1.57272727272727
							7648 SIFPISLAND                  0
							7648   SINDEPTH                  9
							7648   SIVDEPTH  0.398245659082152
							7648   SIXDEPTH   1.08459555555556
							7797 SIFPISLAND  0.230769230769231 # is 0.214285714285714 with entire 2007 data
							7797   SINDEPTH                 13
							7797   SIVDEPTH  0.281707249390168
							7797   SIXDEPTH  0.646153846153846
							8022 SIFPISLAND                  0
							8022   SINDEPTH                  9
							8022   SIVDEPTH   1.06896653725820
							8022   SIXDEPTH   1.59324444444444
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	fake$VALUE <- as.character(fake$VALUE)
	return(fake)		
	
}


nlaStationInformationTest.createTestData2012 <- function()
# Test data from 2012 data.  UNITS is absent and thus depths are assumed to be 
# in meters, and island presence is coded as N, NO, Y, YES. 
#
# SITE		Description
# 6160		10 stations of data, ISLAND 10 NO, DEPTH_AT_STATION has 10 values
# 6293		10 stations of data, ISLAND 2 NO, DEPTH_AT_STATION has 10 values
# 6319		10 stations of data, ISLAND 8 NO, DEPTH_AT_STATION has 10 values
# 6362		1 station of data (J), ISLAND 1 NO, DEPTH_AT_STATION has 1 value
# 1000158	10 stations of data, ISLAND all N, DEPTH_AT_STATION all absent
# 6683		1 stations of data, ISLAND all absent, DEPTH_AT_STATION has 1 value
# 6794		10 stations of data, ISLAND all absent, DEPTH_AT_STATION has 10 value0
# 7830		7 stations of data, ISLAND 6 NO, 1 YES, DEPTH_AT_STATION has 7 values
# 8762		10 stations of data, ISLAND 7 NO, 3 YES, DEPTH_AT_STATION has 10 values
# 1         7 stations of data, ISLAND is NA for all
# 2         7 stations of data, ISLAND is NA for 5
{
	tc <- textConnection("   SITE STATION        PARAMETER VALUE
							6160       A DEPTH_AT_STATION    3.6
							6160       A           ISLAND     NO
							6160       B DEPTH_AT_STATION    2.3
							6160       B           ISLAND     NO
							6160       C DEPTH_AT_STATION    1.0
							6160       C           ISLAND     NO
							6160       D DEPTH_AT_STATION    3.1
							6160       D           ISLAND     NO
							6160       E DEPTH_AT_STATION    3.3
							6160       E           ISLAND     NO
							6160       F DEPTH_AT_STATION    2.9
							6160       F           ISLAND     NO
							6160       G DEPTH_AT_STATION    2.1
							6160       G           ISLAND     NO
							6160       H DEPTH_AT_STATION    2.6
							6160       H           ISLAND     NO
							6160       I DEPTH_AT_STATION    3.5
							6160       I           ISLAND     NO
							6160       J DEPTH_AT_STATION    4.7
							6160       J           ISLAND     NO
							6293       A DEPTH_AT_STATION    2.1
							6293       A           ISLAND     NO
							6293       B DEPTH_AT_STATION    2.3
							6293       C DEPTH_AT_STATION    2.2
							6293       D DEPTH_AT_STATION    2.0
							6293       E DEPTH_AT_STATION    2.2
							6293       F DEPTH_AT_STATION    1.8
							6293       G DEPTH_AT_STATION    2.1
							6293       H DEPTH_AT_STATION    1.7
							6293       H           ISLAND     NO
							6293       I DEPTH_AT_STATION    2.1
							6293       J DEPTH_AT_STATION    1.9
							6319       A DEPTH_AT_STATION    0.5
							6319       A           ISLAND     NO
							6319       B DEPTH_AT_STATION    1.0
							6319       B           ISLAND     NO
							6319       C DEPTH_AT_STATION    0.5
							6319       C           ISLAND     NO
							6319       D DEPTH_AT_STATION    1.0
							6319       D           ISLAND     NO
							6319       E DEPTH_AT_STATION    5.0
							6319       F DEPTH_AT_STATION    5.0
							6319       F           ISLAND     NO
							6319       G DEPTH_AT_STATION    5.5
							6319       G           ISLAND     NO
							6319       H DEPTH_AT_STATION    1.0
							6319       H           ISLAND     NO
							6319       I DEPTH_AT_STATION    0.5
							6319       J DEPTH_AT_STATION    0.8
							6319       J           ISLAND     NO
							6362       J DEPTH_AT_STATION    1.0
							6362       J           ISLAND     NO
							6683       J DEPTH_AT_STATION    1.1
							6794       A DEPTH_AT_STATION    0.3
							6794       B DEPTH_AT_STATION    0.3
							6794       C DEPTH_AT_STATION    0.2
							6794       D DEPTH_AT_STATION    0.3
							6794       E DEPTH_AT_STATION    1.0
							6794       F DEPTH_AT_STATION    1.0
							6794       G DEPTH_AT_STATION    0.2
							6794       H DEPTH_AT_STATION    1.4
							6794       I DEPTH_AT_STATION    0.5
							6794       J DEPTH_AT_STATION    0.3
							7830       A DEPTH_AT_STATION    0.6
							7830       A           ISLAND     NO
							7830       B DEPTH_AT_STATION    1.2
							7830       B           ISLAND     NO
							7830       C DEPTH_AT_STATION    0.9
							7830       C           ISLAND    YES
							7830       D DEPTH_AT_STATION    0.9
							7830       D           ISLAND     NO
							7830       E DEPTH_AT_STATION    1.3
							7830       E           ISLAND     NO
							7830       F DEPTH_AT_STATION    1.0
							7830       F           ISLAND     NO
							7830       G DEPTH_AT_STATION    0.7
							7830       G           ISLAND     NO
							8762       A DEPTH_AT_STATION    0.9
							8762       A           ISLAND     NO
							8762       B DEPTH_AT_STATION    1.2
							8762       B           ISLAND     NO
							8762       C DEPTH_AT_STATION    1.2
							8762       C           ISLAND     NO
							8762       D DEPTH_AT_STATION    0.8
							8762       D           ISLAND     NO
							8762       E DEPTH_AT_STATION    0.9
							8762       E           ISLAND     NO
							8762       F DEPTH_AT_STATION    1.2
							8762       F           ISLAND    YES
							8762       G DEPTH_AT_STATION    1.2
							8762       G           ISLAND    YES
							8762       H DEPTH_AT_STATION    1.2
							8762       H           ISLAND    YES
							8762       I DEPTH_AT_STATION    1.1
							8762       I           ISLAND     NO
							8762       J DEPTH_AT_STATION    0.5
							8762       J           ISLAND     NO
						 1000158       A           ISLAND      N
						 1000158       B           ISLAND      N
						 1000158       C           ISLAND      N
						 1000158       D           ISLAND      N
						 1000158       E           ISLAND      N
						 1000158       F           ISLAND      N
						 1000158       G           ISLAND      N
						 1000158       H           ISLAND      N
						 1000158       I           ISLAND      N
						 1000158       J           ISLAND      N
                               1       A           ISLAND     NA
                               1       B           ISLAND     NA
                               1       C           ISLAND     NA
                               1       D           ISLAND     NA
                               1       E           ISLAND     NA
                               1       F           ISLAND     NA
                               1       G           ISLAND     NA
                               2       A           ISLAND     NA
                               2       B           ISLAND     NA
                               2       C           ISLAND     NA
                               2       D           ISLAND     NA
                               2       E           ISLAND     NA
                               2       F           ISLAND     YES
                               2       G           ISLAND     NO

					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaStationInformationTest.createExpectedResults2012 <- function()
# Based on SAS calculations and modified to reduce floating point precision and
# to add SIFPISLAND values of 0 to sites where ISLAND is totally absent (6683,
# 6794).  These actually 'should' be missing values, but this is consistent  
# with 2007 calculation method.
{
	tc <- textConnection("   SITE  METRIC  VALUE
							6160 SIFPISLAND  0
							6160   SINDEPTH  10
							6160   SIVDEPTH  0.999388702046295	# was 0.9993887019999999621334
							6160   SIXDEPTH  2.91				# was 2.9100000000000001421085
							6293 SIFPISLAND  0
							6293   SINDEPTH  10
							6293   SIVDEPTH  0.189736659610103	# was 0.1897366595999999960842
							6293   SIXDEPTH  2.04				# was 2.0400000000000000355271
							6319 SIFPISLAND  0
							6319   SINDEPTH  10
							6319   SIVDEPTH  2.14413929895735	# was 2.1441392989999998874850
							6319   SIXDEPTH  2.08				# was 2.0800000000000000710543
							6362 SIFPISLAND  0
							6362   SINDEPTH  1
							6362   SIVDEPTH  NA
							6362   SIXDEPTH  1
						#	6683 SIFPISLAND  0					# added to SAS results, then removed from expected results after updating calling interface, see history comments.
							6683   SINDEPTH  1
							6683   SIVDEPTH  NA
							6683   SIXDEPTH  1.1				# was 1.1000000000000000888178
						#	6794 SIFPISLAND  0					# added to SAS results, then removed from expected results after updating calling interface, see history comments.
							6794   SINDEPTH  10
							6794   SIVDEPTH  0.424918292799399	# was 0.4249182927999999859203
							6794   SIXDEPTH  0.55				# was 0.5500000000000000444089
							7830 SIFPISLAND  0.142857142857143	# was 0.1428571429000000114762
							7830   SINDEPTH  7
							7830   SIVDEPTH  0.250713268211203	# was 0.2507132682000000056810
							7830   SIXDEPTH  0.942857142857143	# was 0.9428571429000000003740
							8762 SIFPISLAND  0.3				# was 0.2999999999999999888978
							8762   SINDEPTH  10
							8762   SIVDEPTH  0.239443799947573	# was 0.2394437999000000039107
							8762   SIXDEPTH  1.02				# was 1.0200000000000000177636
						 1000158 SIFPISLAND  0
						       1 SIFPISLAND  NA
						       2 SIFPISLAND  0.5
					")
	
	fake <- read.table(tc, header=TRUE, colClasses=c('integer','character','character')
					  ,stringsAsFactors=FALSE, row.names=NULL
			  		  )
	close(tc)
	
	return(fake)		
}


# end of file