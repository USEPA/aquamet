# nlaLittoralMacrohabitat
# RUnit tests
#   07/14/17 cws Renamed metsLittoralMacrohabitat to nlaLittoralMacrohabitat.
#            Changed UID to SITE, RESULT to VALUE, and output uses METRIC instead
#            of PARAMETER. Removed stubs for 2012 data, as that year did not
#            include this data.  Extended test to include cases where some data
#            are absent.
#

nlaLittoralMacrohabitatTest <- function()
# unit test for nlaLittoralMacrohabitat
{
	nlaLittoralMacrohabitatTest.2007()
}



nlaLittoralMacrohabitatTest.2007 <- function()
# Unit test for nlaLittoralMacrohabitat using 2007 data
{
	testData <- nlaLittoralMacrohabitatTest.createTestData2007()
	
	# Test with full data
	expected <- nlaLittoralMacrohabitatTest.createExpectedResults2007()
	actual <- nlaLittoralMacrohabitat(artificial = testData %>% subset(PARAMETER=='COVER_ARTIFICIAL') %>% select(SITE, STATION, VALUE)
                                     ,boulders = testData %>% subset(PARAMETER=='COVER_BOULDERS') %>% select(SITE, STATION, VALUE)
                                     ,coverExtent = testData %>% subset(PARAMETER=='COVER_CLASS') %>% select(SITE, STATION, VALUE)
                                     ,humanDisturbance = testData %>% subset(PARAMETER=='HUMAN_DISTURBANCE') %>% select(SITE, STATION, VALUE)
                                     ,noCover = testData %>% subset(PARAMETER=='COVER_NONE') %>% select(SITE, STATION, VALUE)
                                     ,substrate = testData %>% subset(PARAMETER=='DOM_SUBSTRATE') %>% select(SITE, STATION, VALUE)
                                     ,vegetation = testData %>% subset(PARAMETER=='COVER_VEG') %>% select(SITE, STATION, VALUE)
                                     ,woody = testData %>% subset(PARAMETER=='COVER_WOODY') %>% select(SITE, STATION, VALUE)
                                     )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")	
	
	# Test with absent cover type ARTIFICIAL
	actual <- nlaLittoralMacrohabitat(artificial = NULL #testData %>% subset(PARAMETER=='COVER_ARTIFICIAL') %>% select(SITE, STATION, VALUE)
                                     ,boulders = testData %>% subset(PARAMETER=='COVER_BOULDERS') %>% select(SITE, STATION, VALUE)
                                     ,coverExtent = testData %>% subset(PARAMETER=='COVER_CLASS') %>% select(SITE, STATION, VALUE)
                                     ,humanDisturbance = testData %>% subset(PARAMETER=='HUMAN_DISTURBANCE') %>% select(SITE, STATION, VALUE)
                                     ,noCover = testData %>% subset(PARAMETER=='COVER_NONE') %>% select(SITE, STATION, VALUE)
                                     ,substrate = testData %>% subset(PARAMETER=='DOM_SUBSTRATE') %>% select(SITE, STATION, VALUE)
                                     ,vegetation = testData %>% subset(PARAMETER=='COVER_VEG') %>% select(SITE, STATION, VALUE)
                                     ,woody = testData %>% subset(PARAMETER=='COVER_WOODY') %>% select(SITE, STATION, VALUE)
                                     )
	expected <- nlaLittoralMacrohabitatTest.createExpectedResults2007() %>%
	            subset(METRIC %nin% 'LMFPARTIFICIAL') %>%
	            mutate(VALUE = ifelse(SITE==7494,
	                                  ifelse(METRIC=='LMFPBOULDERS',  3/6       # Now there are only 6 stations with cover data rather than 10
	                                 ,ifelse(METRIC=='LMFPVEG',       3/6
	                                 ,ifelse(METRIC=='LMFPWOODY',     3/6
	                                 ,ifelse(METRIC=='LMNCOVERTYPES', 6, VALUE
	                                  ))))
	                          ,ifelse(SITE==7508,
	                                  ifelse(METRIC=='LMFPBOULDERS',  1/8       # Now there are only 8 stations with cover data rather than 10
	                                 ,ifelse(METRIC=='LMFPVEG',       7/8
	                                 ,ifelse(METRIC=='LMFPWOODY',     3/8
	                                 ,ifelse(METRIC=='LMNCOVERTYPES', 8, VALUE
	                                  ))))
	                          ,ifelse(SITE==8645,
	                                  ifelse(METRIC=='LMFPBOULDERS',  1/8       # Now there are only 8 stations with cover data rather than 10
	                                 ,ifelse(METRIC=='LMFPVEG',       8/8
	                                 ,ifelse(METRIC=='LMNCOVERTYPES', 8, VALUE
	                                  )))
	                          ,VALUE    
	                           )))
	                  )
	diff <- dfCompare(expected
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics")	
	
	# Test with absent substrate
	actual <- nlaLittoralMacrohabitat(artificial = testData %>% subset(PARAMETER=='COVER_ARTIFICIAL') %>% select(SITE, STATION, VALUE)
                                     ,boulders = testData %>% subset(PARAMETER=='COVER_BOULDERS') %>% select(SITE, STATION, VALUE)
                                     ,coverExtent = testData %>% subset(PARAMETER=='COVER_CLASS') %>% select(SITE, STATION, VALUE)
                                     ,humanDisturbance = testData %>% subset(PARAMETER=='HUMAN_DISTURBANCE') %>% select(SITE, STATION, VALUE)
                                     ,noCover = testData %>% subset(PARAMETER=='COVER_NONE') %>% select(SITE, STATION, VALUE)
                                     ,substrate = NULL #testData %>% subset(PARAMETER=='DOM_SUBSTRATE') %>% select(SITE, STATION, VALUE)
                                     ,vegetation = testData %>% subset(PARAMETER=='COVER_VEG') %>% select(SITE, STATION, VALUE)
                                     ,woody = testData %>% subset(PARAMETER=='COVER_WOODY') %>% select(SITE, STATION, VALUE)
                                     )
	expected <- nlaLittoralMacrohabitatTest.createExpectedResults2007() %>%
	            subset(METRIC %nin% c('LMFPBEDROCK','LMFPCOBBLE','LMFPMUD','LMFPSAND','LMNSUBSTRATE','LMOSUBSTRATE'))
	diff <- dfCompare(expected
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics")	
	
	# Test with absent humanDisturbance
	actual <- nlaLittoralMacrohabitat(artificial = testData %>% subset(PARAMETER=='COVER_ARTIFICIAL') %>% select(SITE, STATION, VALUE)
                                     ,boulders = testData %>% subset(PARAMETER=='COVER_BOULDERS') %>% select(SITE, STATION, VALUE)
                                     ,coverExtent = testData %>% subset(PARAMETER=='COVER_CLASS') %>% select(SITE, STATION, VALUE)
                                     ,humanDisturbance = NULL # testData %>% subset(PARAMETER=='HUMAN_DISTURBANCE') %>% select(SITE, STATION, VALUE)
                                     ,noCover = testData %>% subset(PARAMETER=='COVER_NONE') %>% select(SITE, STATION, VALUE)
                                     ,substrate = testData %>% subset(PARAMETER=='DOM_SUBSTRATE') %>% select(SITE, STATION, VALUE)
                                     ,vegetation = testData %>% subset(PARAMETER=='COVER_VEG') %>% select(SITE, STATION, VALUE)
                                     ,woody = testData %>% subset(PARAMETER=='COVER_WOODY') %>% select(SITE, STATION, VALUE)
                                     )
	expected <- nlaLittoralMacrohabitatTest.createExpectedResults2007() %>%
	            subset(METRIC %nin% c('LMNHUMAN','LMPWHUMAN'))
	diff <- dfCompare(expected
	                 ,actual
	                 ,c('SITE','METRIC'), zeroFudge=1e-14
	                 )
	checkTrue(is.null(diff), "Incorrect calculation of metrics")	
	
}


nlaLittoralMacrohabitatTest.createTestData2007 <- function()
# Unit test data based on 2007 data
#	SITE		Description
#	7469 - 10 DOM_SUBSTRATE='M', no B, C or S 
#	7474 - All 3 COVER_CLASS values present in 10 stations
#	7478 - 10 HUMAN_DISTURBANCE='MODERATE'
#	7490 - 10 COVER_CLASS = 'NO COVER'
#	7494 - All 4 DOM_SUBSTRATE values present in 10 stations
#          10 HUMAN_DISTURBANCE='HEAVY'
#	7503 - 10 COVER_CLASS = 'CONTINUOUS'
#	7508 - All 3 HUMAN_DISTURBANCE values present in 10 stations
#	7521 - DOM_SUBSTRATE has 3 modes
#	7525 - HUMAN_DISTURBANCE has 3 modes
#	7533 - Has 0 COVER_* PARAMETER values
#	7542 - 10 HUMAN_DISTURBANCE = 'LOW'
#	7784 - depauperate data, 1 value of HUMAN_DISTURBANCE, DOM_SUBSTRATE and COVER_CLASS
#	8322 - depauperate data, 4 values of HUMAN_DISTURBANCE, DOM_SUBSTRATE and COVER_CLASS and COVER_*
#	8645 - COVER_CLASS has 2 modes and one other value
#	8806 - COVER_CLASS has 2 modes and no other values
#	8860 - 10 COVER_CLASS = 'PATCHY COVER'
{
	tc <- textConnection("   SITE STATION         PARAMETER             VALUE
					7469       A       COVER_CLASS 'CONTINUOUS COVER'
					7469       A         COVER_VEG                  X
					7469       A     DOM_SUBSTRATE                  M
					7469       A HUMAN_DISTURBANCE               NONE
					7469       B       COVER_CLASS 'CONTINUOUS COVER'
					7469       B         COVER_VEG                  X
					7469       B     DOM_SUBSTRATE                  M
					7469       B HUMAN_DISTURBANCE               NONE
					7469       C       COVER_CLASS 'CONTINUOUS COVER'
					7469       C         COVER_VEG                  X
					7469       C     DOM_SUBSTRATE                  M
					7469       C HUMAN_DISTURBANCE               NONE
					7469       D       COVER_CLASS 'CONTINUOUS COVER'
					7469       D         COVER_VEG                  X
					7469       D     DOM_SUBSTRATE                  M
					7469       D HUMAN_DISTURBANCE               NONE
					7469       E       COVER_CLASS 'CONTINUOUS COVER'
					7469       E         COVER_VEG                  X
					7469       E     DOM_SUBSTRATE                  M
					7469       E HUMAN_DISTURBANCE               NONE
					7469       F       COVER_CLASS 'CONTINUOUS COVER'
					7469       F         COVER_VEG                  X
					7469       F     DOM_SUBSTRATE                  M
					7469       F HUMAN_DISTURBANCE               NONE
					7469       G       COVER_CLASS 'CONTINUOUS COVER'
					7469       G         COVER_VEG                  X
					7469       G     DOM_SUBSTRATE                  M
					7469       H       COVER_CLASS     'PATCHY COVER'
					7469       H         COVER_VEG                  X
					7469       H     DOM_SUBSTRATE                  M
					7469       H HUMAN_DISTURBANCE               NONE
					7469       I       COVER_CLASS 'CONTINUOUS COVER'
					7469       I         COVER_VEG                  X
					7469       I     DOM_SUBSTRATE                  M
					7469       I HUMAN_DISTURBANCE               NONE
					7469       J       COVER_CLASS 'CONTINUOUS COVER'
					7469       J         COVER_VEG                  X
					7469       J     DOM_SUBSTRATE                  M
					7469       J HUMAN_DISTURBANCE               NONE
					7474       A    COVER_BOULDERS                  X
					7474       A       COVER_CLASS         'NO COVER'
					7474       A     DOM_SUBSTRATE                  C
					7474       A HUMAN_DISTURBANCE              HEAVY
					7474       B       COVER_CLASS 'CONTINUOUS COVER'
					7474       B         COVER_VEG                  X
					7474       B     DOM_SUBSTRATE                  M
					7474       B HUMAN_DISTURBANCE              HEAVY
					7474       C       COVER_CLASS     'PATCHY COVER'
					7474       C         COVER_VEG                  X
					7474       C     DOM_SUBSTRATE                  M
					7474       C HUMAN_DISTURBANCE              HEAVY
					7474       D       COVER_CLASS         'NO COVER'
					7474       D         COVER_VEG                  X
					7474       D     DOM_SUBSTRATE                  M
					7474       D HUMAN_DISTURBANCE              HEAVY
					7474       E       COVER_CLASS 'CONTINUOUS COVER'
					7474       E         COVER_VEG                  X
					7474       E     DOM_SUBSTRATE                  M
					7474       E HUMAN_DISTURBANCE              HEAVY
					7474       F       COVER_CLASS     'PATCHY COVER'
					7474       F         COVER_VEG                  X
					7474       F     DOM_SUBSTRATE                  M
					7474       F HUMAN_DISTURBANCE              HEAVY
					7474       G       COVER_CLASS     'PATCHY COVER'
					7474       G         COVER_VEG                  X
					7474       G     DOM_SUBSTRATE                  M
					7474       G HUMAN_DISTURBANCE              HEAVY
					7474       H       COVER_CLASS         'NO COVER'
					7474       H         COVER_VEG                  X
					7474       H     DOM_SUBSTRATE                  M
					7474       H HUMAN_DISTURBANCE              HEAVY
					7474       I       COVER_CLASS         'NO COVER'
					7474       I         COVER_VEG                  X
					7474       I     DOM_SUBSTRATE                  M
					7474       I HUMAN_DISTURBANCE              HEAVY
					7474       J       COVER_CLASS         'NO COVER'
					7474       J         COVER_VEG                  X
					7474       J     DOM_SUBSTRATE                  M
					7474       J HUMAN_DISTURBANCE              HEAVY
					7478       A       COVER_CLASS         'NO COVER'
					7478       A         COVER_VEG                  X
					7478       A     DOM_SUBSTRATE                  M
					7478       A HUMAN_DISTURBANCE           MODERATE
					7478       B    COVER_BOULDERS                  X
					7478       B       COVER_CLASS     'PATCHY COVER'
					7478       B     DOM_SUBSTRATE                  C
					7478       B HUMAN_DISTURBANCE           MODERATE
					7478       C    COVER_BOULDERS                  X
					7478       C       COVER_CLASS     'PATCHY COVER'
					7478       C     DOM_SUBSTRATE                  C
					7478       C HUMAN_DISTURBANCE           MODERATE
					7478       D       COVER_CLASS 'CONTINUOUS COVER'
					7478       D       COVER_WOODY                  X
					7478       D HUMAN_DISTURBANCE           MODERATE
					7478       E    COVER_BOULDERS                  X
					7478       E       COVER_CLASS     'PATCHY COVER'
					7478       E     DOM_SUBSTRATE                  C
					7478       E HUMAN_DISTURBANCE           MODERATE
					7478       F    COVER_BOULDERS                  X
					7478       F       COVER_CLASS     'PATCHY COVER'
					7478       F     DOM_SUBSTRATE                  B
					7478       F HUMAN_DISTURBANCE           MODERATE
					7478       G    COVER_BOULDERS                  X
					7478       G       COVER_CLASS     'PATCHY COVER'
					7478       G     DOM_SUBSTRATE                  C
					7478       G HUMAN_DISTURBANCE           MODERATE
					7478       H    COVER_BOULDERS                  X
					7478       H       COVER_CLASS     'PATCHY COVER'
					7478       H       COVER_WOODY                  X
					7478       H     DOM_SUBSTRATE                  C
					7478       H HUMAN_DISTURBANCE           MODERATE
					7478       I    COVER_BOULDERS                  X
					7478       I       COVER_CLASS     'PATCHY COVER'
					7478       I     DOM_SUBSTRATE                  S
					7478       I HUMAN_DISTURBANCE           MODERATE
					7478       J       COVER_CLASS     'PATCHY COVER'
					7478       J         COVER_VEG                  X
					7478       J     DOM_SUBSTRATE                  M
					7478       J HUMAN_DISTURBANCE           MODERATE
					7490       A       COVER_CLASS         'NO COVER'
					7490       A         COVER_VEG                  X
					7490       A     DOM_SUBSTRATE                  M
					7490       A HUMAN_DISTURBANCE               NONE
					7490       B       COVER_CLASS         'NO COVER'
					7490       B         COVER_VEG                  X
					7490       B     DOM_SUBSTRATE                  M
					7490       B HUMAN_DISTURBANCE               NONE
					7490       C       COVER_CLASS         'NO COVER'
					7490       C         COVER_VEG                  X
					7490       C     DOM_SUBSTRATE                  M
					7490       C HUMAN_DISTURBANCE               NONE
					7490       D       COVER_CLASS         'NO COVER'
					7490       D         COVER_VEG                  X
					7490       D     DOM_SUBSTRATE                  M
					7490       D HUMAN_DISTURBANCE               NONE
					7490       E       COVER_CLASS         'NO COVER'
					7490       E         COVER_VEG                  X
					7490       E     DOM_SUBSTRATE                  M
					7490       E HUMAN_DISTURBANCE               NONE
					7490       F       COVER_CLASS         'NO COVER'
					7490       F         COVER_VEG                  X
					7490       F     DOM_SUBSTRATE                  M
					7490       F HUMAN_DISTURBANCE               NONE
					7490       G       COVER_CLASS         'NO COVER'
					7490       G         COVER_VEG                  X
					7490       G     DOM_SUBSTRATE                  M
					7490       G HUMAN_DISTURBANCE               NONE
					7490       H       COVER_CLASS         'NO COVER'
					7490       H         COVER_VEG                  X
					7490       H     DOM_SUBSTRATE                  M
					7490       H HUMAN_DISTURBANCE               NONE
					7490       I    COVER_BOULDERS                  X
					7490       I       COVER_CLASS         'NO COVER'
					7490       I         COVER_VEG                  X
					7490       I     DOM_SUBSTRATE                  C
					7490       I HUMAN_DISTURBANCE               NONE
					7490       J       COVER_CLASS         'NO COVER'
					7490       J         COVER_VEG                  X
					7490       J     DOM_SUBSTRATE                  M
					7490       J HUMAN_DISTURBANCE               NONE
					7494       A  COVER_ARTIFICIAL                  X
					7494       A       COVER_CLASS         'NO COVER'
					7494       A     DOM_SUBSTRATE                  C
					7494       A HUMAN_DISTURBANCE              HEAVY
					7494       B  COVER_ARTIFICIAL                  X
					7494       B       COVER_CLASS         'NO COVER'
					7494       B     DOM_SUBSTRATE                  B
					7494       B HUMAN_DISTURBANCE              HEAVY
					7494       C    COVER_BOULDERS                  X
					7494       C       COVER_CLASS         'NO COVER'
					7494       C     DOM_SUBSTRATE                  C
					7494       C HUMAN_DISTURBANCE              HEAVY
					7494       D  COVER_ARTIFICIAL                  X
					7494       D       COVER_CLASS         'NO COVER'
					7494       D     DOM_SUBSTRATE                  S
					7494       D HUMAN_DISTURBANCE              HEAVY
					7494       E  COVER_ARTIFICIAL                  X
					7494       E    COVER_BOULDERS                  X
					7494       E       COVER_CLASS         'NO COVER'
					7494       E     DOM_SUBSTRATE                  M
					7494       E HUMAN_DISTURBANCE              HEAVY
					7494       F       COVER_CLASS         'NO COVER'
					7494       F       COVER_WOODY                  X
					7494       F     DOM_SUBSTRATE                  S
					7494       F HUMAN_DISTURBANCE              HEAVY
					7494       G  COVER_ARTIFICIAL                  X
					7494       G    COVER_BOULDERS                  X
					7494       G       COVER_CLASS         'NO COVER'
					7494       G         COVER_VEG                  X
					7494       G     DOM_SUBSTRATE                  S
					7494       G HUMAN_DISTURBANCE              HEAVY
					7494       H  COVER_ARTIFICIAL                  X
					7494       H       COVER_CLASS         'NO COVER'
					7494       H     DOM_SUBSTRATE                  S
					7494       H HUMAN_DISTURBANCE              HEAVY
					7494       I  COVER_ARTIFICIAL                  X
					7494       I       COVER_CLASS     'PATCHY COVER'
					7494       I         COVER_VEG                  X
					7494       I       COVER_WOODY                  X
					7494       I     DOM_SUBSTRATE                  M
					7494       I HUMAN_DISTURBANCE              HEAVY
					7494       J  COVER_ARTIFICIAL                  X
					7494       J       COVER_CLASS         'NO COVER'
					7494       J         COVER_VEG                  X
					7494       J       COVER_WOODY                  X
					7494       J     DOM_SUBSTRATE                  S
					7494       J HUMAN_DISTURBANCE              HEAVY
					7503       A       COVER_CLASS 'CONTINUOUS COVER'
					7503       A         COVER_VEG                  X
					7503       A     DOM_SUBSTRATE                  M
					7503       A HUMAN_DISTURBANCE                LOW
					7503       B    COVER_BOULDERS                  X
					7503       B       COVER_CLASS 'CONTINUOUS COVER'
					7503       B         COVER_VEG                  X
					7503       B     DOM_SUBSTRATE                  M
					7503       B HUMAN_DISTURBANCE                LOW
					7503       C       COVER_CLASS 'CONTINUOUS COVER'
					7503       C         COVER_VEG                  X
					7503       C     DOM_SUBSTRATE                  M
					7503       C HUMAN_DISTURBANCE                LOW
					7503       D       COVER_CLASS 'CONTINUOUS COVER'
					7503       D         COVER_VEG                  X
					7503       D     DOM_SUBSTRATE                  M
					7503       D HUMAN_DISTURBANCE                LOW
					7503       E       COVER_CLASS 'CONTINUOUS COVER'
					7503       E         COVER_VEG                  X
					7503       E     DOM_SUBSTRATE                  M
					7503       E HUMAN_DISTURBANCE           MODERATE
					7503       F       COVER_CLASS 'CONTINUOUS COVER'
					7503       F         COVER_VEG                  X
					7503       F     DOM_SUBSTRATE                  M
					7503       F HUMAN_DISTURBANCE           MODERATE
					7503       G       COVER_CLASS 'CONTINUOUS COVER'
					7503       G         COVER_VEG                  X
					7503       G     DOM_SUBSTRATE                  M
					7503       G HUMAN_DISTURBANCE           MODERATE
					7503       H       COVER_CLASS 'CONTINUOUS COVER'
					7503       H         COVER_VEG                  X
					7503       H     DOM_SUBSTRATE                  M
					7503       H HUMAN_DISTURBANCE              HEAVY
					7503       I       COVER_CLASS 'CONTINUOUS COVER'
					7503       I         COVER_VEG                  X
					7503       I     DOM_SUBSTRATE                  M
					7503       I HUMAN_DISTURBANCE                LOW
					7503       J       COVER_CLASS 'CONTINUOUS COVER'
					7503       J         COVER_VEG                  X
					7503       J     DOM_SUBSTRATE                  M
					7503       J HUMAN_DISTURBANCE                LOW
					7508       A  COVER_ARTIFICIAL                  X
					7508       A       COVER_CLASS     'PATCHY COVER'
					7508       A         COVER_VEG                  X
					7508       A     DOM_SUBSTRATE                  S
					7508       A HUMAN_DISTURBANCE                LOW
					7508       B       COVER_CLASS     'PATCHY COVER'
					7508       B         COVER_VEG                  X
					7508       B       COVER_WOODY                  X
					7508       B     DOM_SUBSTRATE                  S
					7508       B HUMAN_DISTURBANCE               NONE
					7508       C       COVER_CLASS     'PATCHY COVER'
					7508       C         COVER_VEG                  X
					7508       C     DOM_SUBSTRATE                  M
					7508       C HUMAN_DISTURBANCE                LOW
					7508       D  COVER_ARTIFICIAL                  X
					7508       D       COVER_CLASS         'NO COVER'
					7508       D     DOM_SUBSTRATE                  S
					7508       D HUMAN_DISTURBANCE                LOW
					7508       E       COVER_CLASS 'CONTINUOUS COVER'
					7508       E         COVER_VEG                  X
					7508       E     DOM_SUBSTRATE                  M
					7508       E HUMAN_DISTURBANCE               NONE
					7508       F       COVER_CLASS     'PATCHY COVER'
					7508       F         COVER_VEG                  X
					7508       F       COVER_WOODY                  X
					7508       F     DOM_SUBSTRATE                  S
					7508       F HUMAN_DISTURBANCE               NONE
					7508       G  COVER_ARTIFICIAL                  X
					7508       G       COVER_CLASS     'PATCHY COVER'
					7508       G         COVER_VEG                  X
					7508       G HUMAN_DISTURBANCE                LOW
					7508       H  COVER_ARTIFICIAL                  X
					7508       H    COVER_BOULDERS                  X
					7508       H       COVER_CLASS     'PATCHY COVER'
					7508       H     DOM_SUBSTRATE                  M
					7508       H HUMAN_DISTURBANCE              HEAVY
					7508       I  COVER_ARTIFICIAL                  X
					7508       I       COVER_CLASS     'PATCHY COVER'
					7508       I         COVER_VEG                  X
					7508       I       COVER_WOODY                  X
					7508       I     DOM_SUBSTRATE                  S
					7508       I HUMAN_DISTURBANCE                LOW
					7508       J  COVER_ARTIFICIAL                  X
					7508       J       COVER_CLASS         'NO COVER'
					7508       J     DOM_SUBSTRATE                  S
					7508       J HUMAN_DISTURBANCE           MODERATE
					7521       A       COVER_CLASS     'PATCHY COVER'
					7521       A         COVER_VEG                  X
					7521       A       COVER_WOODY                  X
					7521       A     DOM_SUBSTRATE                  S
					7521       A HUMAN_DISTURBANCE               NONE
					7521       B    COVER_BOULDERS                  X
					7521       B       COVER_CLASS     'PATCHY COVER'
					7521       B       COVER_WOODY                  X
					7521       B     DOM_SUBSTRATE                  S
					7521       B HUMAN_DISTURBANCE               NONE
					7521       C       COVER_CLASS     'PATCHY COVER'
					7521       C         COVER_VEG                  X
					7521       C       COVER_WOODY                  X
					7521       C     DOM_SUBSTRATE                  M
					7521       C HUMAN_DISTURBANCE               NONE
					7521       D       COVER_CLASS     'PATCHY COVER'
					7521       D         COVER_VEG                  X
					7521       D       COVER_WOODY                  X
					7521       D     DOM_SUBSTRATE                  M
					7521       D HUMAN_DISTURBANCE               NONE
					7521       E    COVER_BOULDERS                  X
					7521       E       COVER_CLASS         'NO COVER'
					7521       E         COVER_VEG                  X
					7521       E       COVER_WOODY                  X
					7521       E     DOM_SUBSTRATE                  M
					7521       E HUMAN_DISTURBANCE               NONE
					7521       F    COVER_BOULDERS                  X
					7521       F       COVER_CLASS     'PATCHY COVER'
					7521       F     DOM_SUBSTRATE                  B
					7521       F HUMAN_DISTURBANCE               NONE
					7521       G    COVER_BOULDERS                  X
					7521       G       COVER_CLASS 'CONTINUOUS COVER'
					7521       G         COVER_VEG                  X
					7521       G       COVER_WOODY                  X
					7521       G     DOM_SUBSTRATE                  C
					7521       G HUMAN_DISTURBANCE               NONE
					7521       H    COVER_BOULDERS                  X
					7521       H       COVER_CLASS     'PATCHY COVER'
					7521       H         COVER_VEG                  X
					7521       H       COVER_WOODY                  X
					7521       H     DOM_SUBSTRATE                  C
					7521       H HUMAN_DISTURBANCE               NONE
					7521       I    COVER_BOULDERS                  X
					7521       I       COVER_CLASS     'PATCHY COVER'
					7521       I         COVER_VEG                  X
					7521       I       COVER_WOODY                  X
					7521       I     DOM_SUBSTRATE                  C
					7521       I HUMAN_DISTURBANCE               NONE
					7521       J       COVER_CLASS     'PATCHY COVER'
					7521       J       COVER_WOODY                  X
					7521       J     DOM_SUBSTRATE                  S
					7521       J HUMAN_DISTURBANCE               NONE
					7525       A       COVER_CLASS         'NO COVER'
					7525       A         COVER_VEG                  X
					7525       A     DOM_SUBSTRATE                  S
					7525       A HUMAN_DISTURBANCE              HEAVY
					7525       B  COVER_ARTIFICIAL                  X
					7525       B       COVER_CLASS         'NO COVER'
					7525       B         COVER_VEG                  X
					7525       B       COVER_WOODY                  X
					7525       B     DOM_SUBSTRATE                  S
					7525       B HUMAN_DISTURBANCE              HEAVY
					7525       C  COVER_ARTIFICIAL                  X
					7525       C       COVER_CLASS     'PATCHY COVER'
					7525       C         COVER_VEG                  X
					7525       C       COVER_WOODY                  X
					7525       C     DOM_SUBSTRATE                  S
					7525       C HUMAN_DISTURBANCE           MODERATE
					7525       D  COVER_ARTIFICIAL                  X
					7525       D       COVER_CLASS     'PATCHY COVER'
					7525       D         COVER_VEG                  X
					7525       D       COVER_WOODY                  X
					7525       D     DOM_SUBSTRATE                  M
					7525       D HUMAN_DISTURBANCE              HEAVY
					7525       E       COVER_CLASS     'PATCHY COVER'
					7525       E         COVER_VEG                  X
					7525       E       COVER_WOODY                  X
					7525       E     DOM_SUBSTRATE                  M
					7525       E HUMAN_DISTURBANCE                LOW
					7525       F  COVER_ARTIFICIAL                  X
					7525       F       COVER_CLASS 'CONTINUOUS COVER'
					7525       F         COVER_VEG                  X
					7525       F       COVER_WOODY                  X
					7525       F     DOM_SUBSTRATE                  M
					7525       F HUMAN_DISTURBANCE           MODERATE
					7525       G       COVER_CLASS 'CONTINUOUS COVER'
					7525       G         COVER_VEG                  X
					7525       G       COVER_WOODY                  X
					7525       G     DOM_SUBSTRATE                  M
					7525       G HUMAN_DISTURBANCE                LOW
					7525       H       COVER_CLASS     'PATCHY COVER'
					7525       H         COVER_VEG                  X
					7525       H       COVER_WOODY                  X
					7525       H     DOM_SUBSTRATE                  M
					7525       H HUMAN_DISTURBANCE               NONE
					7525       I       COVER_CLASS     'PATCHY COVER'
					7525       I         COVER_VEG                  X
					7525       I     DOM_SUBSTRATE                  M
					7525       I HUMAN_DISTURBANCE                LOW
					7525       J       COVER_CLASS 'CONTINUOUS COVER'
					7525       J         COVER_VEG                  X
					7525       J     DOM_SUBSTRATE                  M
					7525       J HUMAN_DISTURBANCE           MODERATE
					7533       A     DOM_SUBSTRATE                  M
					7533       B     DOM_SUBSTRATE                  S
					7533       C     DOM_SUBSTRATE                  S
					7533       D     DOM_SUBSTRATE                  C
					7533       E     DOM_SUBSTRATE                  M
					7533       F     DOM_SUBSTRATE                  M
					7533       G     DOM_SUBSTRATE                  M
					7533       H     DOM_SUBSTRATE                  M
					7533       I     DOM_SUBSTRATE                  M
					7533       J     DOM_SUBSTRATE                  M
					7533       Z     DOM_SUBSTRATE                  M
					7542       A       COVER_CLASS         'NO COVER'
					7542       A       COVER_WOODY                  X
					7542       A     DOM_SUBSTRATE                  S
					7542       A HUMAN_DISTURBANCE                LOW
					7542       B       COVER_CLASS         'NO COVER'
					7542       B         COVER_VEG                  X
					7542       B     DOM_SUBSTRATE                  S
					7542       B HUMAN_DISTURBANCE                LOW
					7542       C       COVER_CLASS         'NO COVER'
					7542       C       COVER_WOODY                  X
					7542       C     DOM_SUBSTRATE                  S
					7542       C HUMAN_DISTURBANCE                LOW
					7542       D    COVER_BOULDERS                  X
					7542       D       COVER_CLASS         'NO COVER'
					7542       D       COVER_WOODY                  X
					7542       D     DOM_SUBSTRATE                  C
					7542       D HUMAN_DISTURBANCE                LOW
					7542       E       COVER_CLASS         'NO COVER'
					7542       E        COVER_NONE                  X
					7542       E     DOM_SUBSTRATE                  B
					7542       E HUMAN_DISTURBANCE                LOW
					7542       F       COVER_CLASS         'NO COVER'
					7542       F        COVER_NONE                  X
					7542       F     DOM_SUBSTRATE                  S
					7542       F HUMAN_DISTURBANCE                LOW
					7542       G       COVER_CLASS         'NO COVER'
					7542       G       COVER_WOODY                  X
					7542       G     DOM_SUBSTRATE                  S
					7542       G HUMAN_DISTURBANCE                LOW
					7542       H    COVER_BOULDERS                  X
					7542       H       COVER_CLASS         'NO COVER'
					7542       H       COVER_WOODY                  X
					7542       H     DOM_SUBSTRATE                  C
					7542       H HUMAN_DISTURBANCE                LOW
					7542       I       COVER_CLASS     'PATCHY COVER'
					7542       I       COVER_WOODY                  X
					7542       I     DOM_SUBSTRATE                  S
					7542       I HUMAN_DISTURBANCE                LOW
					7542       J       COVER_CLASS         'NO COVER'
					7542       J       COVER_WOODY                  X
					7542       J     DOM_SUBSTRATE                  M
					7542       J HUMAN_DISTURBANCE                LOW
					7784       J       COVER_CLASS 'CONTINUOUS COVER'
					7784       J         COVER_VEG                  X
					7784       J     DOM_SUBSTRATE                  S
					7784       J HUMAN_DISTURBANCE               NONE
					8322       A       COVER_CLASS         'NO COVER'
					8322       A        COVER_NONE                  X
					8322       A     DOM_SUBSTRATE                  M
					8322       A HUMAN_DISTURBANCE                LOW
					8322       B       COVER_CLASS         'NO COVER'
					8322       B        COVER_NONE                  X
					8322       B     DOM_SUBSTRATE                  S
					8322       B HUMAN_DISTURBANCE                LOW
					8322       C       COVER_CLASS         'NO COVER'
					8322       C        COVER_NONE                  X
					8322       C     DOM_SUBSTRATE                  S
					8322       C HUMAN_DISTURBANCE                LOW
					8322       J       COVER_CLASS         'NO COVER'
					8322       J        COVER_NONE                  X
					8322       J     DOM_SUBSTRATE                  S
					8322       J HUMAN_DISTURBANCE                LOW
					8645       A    COVER_BOULDERS                  X
					8645       A       COVER_CLASS         'NO COVER'
					8645       A         COVER_VEG                  X
					8645       A     DOM_SUBSTRATE                  S
					8645       A HUMAN_DISTURBANCE                LOW
					8645       B  COVER_ARTIFICIAL                  X
					8645       B       COVER_CLASS         'NO COVER'
					8645       B     DOM_SUBSTRATE                  S
					8645       B HUMAN_DISTURBANCE           MODERATE
					8645       C  COVER_ARTIFICIAL                  X
					8645       C       COVER_CLASS     'PATCHY COVER'
					8645       C     DOM_SUBSTRATE                  S
					8645       C HUMAN_DISTURBANCE           MODERATE
					8645       D       COVER_CLASS     'PATCHY COVER'
					8645       D         COVER_VEG                  X
					8645       D     DOM_SUBSTRATE                  M
					8645       D HUMAN_DISTURBANCE                LOW
					8645       E       COVER_CLASS     'PATCHY COVER'
					8645       E         COVER_VEG                  X
					8645       E     DOM_SUBSTRATE                  M
					8645       E HUMAN_DISTURBANCE               NONE
					8645       F  COVER_ARTIFICIAL                  X
					8645       F       COVER_CLASS 'CONTINUOUS COVER'
					8645       F         COVER_VEG                  X
					8645       F     DOM_SUBSTRATE                  M
					8645       F HUMAN_DISTURBANCE                LOW
					8645       G  COVER_ARTIFICIAL                  X
					8645       G       COVER_CLASS     'PATCHY COVER'
					8645       G         COVER_VEG                  X
					8645       G     DOM_SUBSTRATE                  M
					8645       G HUMAN_DISTURBANCE                LOW
					8645       H       COVER_CLASS 'CONTINUOUS COVER'
					8645       H         COVER_VEG                  X
					8645       H     DOM_SUBSTRATE                  M
					8645       H HUMAN_DISTURBANCE               NONE
					8645       I       COVER_CLASS 'CONTINUOUS COVER'
					8645       I         COVER_VEG                  X
					8645       I     DOM_SUBSTRATE                  M
					8645       I HUMAN_DISTURBANCE               NONE
					8645       J       COVER_CLASS 'CONTINUOUS COVER'
					8645       J         COVER_VEG                  X
					8645       J     DOM_SUBSTRATE                  M
					8645       J HUMAN_DISTURBANCE               NONE
					8806       A    COVER_BOULDERS                  X
					8806       A       COVER_CLASS         'NO COVER'
					8806       A     DOM_SUBSTRATE                  C
					8806       A HUMAN_DISTURBANCE               NONE
					8806       B       COVER_CLASS         'NO COVER'
					8806       B        COVER_NONE                  X
					8806       B     DOM_SUBSTRATE                  S
					8806       B HUMAN_DISTURBANCE               NONE
					8806       C       COVER_CLASS     'PATCHY COVER'
					8806       C       COVER_WOODY                  X
					8806       C     DOM_SUBSTRATE                  M
					8806       C HUMAN_DISTURBANCE               NONE
					8806       D       COVER_CLASS         'NO COVER'
					8806       D       COVER_WOODY                  X
					8806       D     DOM_SUBSTRATE                  S
					8806       D HUMAN_DISTURBANCE               NONE
					8806       E       COVER_CLASS     'PATCHY COVER'
					8806       E       COVER_WOODY                  X
					8806       E     DOM_SUBSTRATE                  S
					8806       E HUMAN_DISTURBANCE               NONE
					8806       F       COVER_CLASS     'PATCHY COVER'
					8806       F       COVER_WOODY                  X
					8806       F     DOM_SUBSTRATE                  S
					8806       F HUMAN_DISTURBANCE               NONE
					8806       G    COVER_BOULDERS                  X
					8806       G       COVER_CLASS     'PATCHY COVER'
					8806       G       COVER_WOODY                  X
					8806       G     DOM_SUBSTRATE                  S
					8806       G HUMAN_DISTURBANCE               NONE
					8806       H       COVER_CLASS         'NO COVER'
					8806       H       COVER_WOODY                  X
					8806       H     DOM_SUBSTRATE                  S
					8806       H HUMAN_DISTURBANCE               NONE
					8806       I       COVER_CLASS     'PATCHY COVER'
					8806       I       COVER_WOODY                  X
					8806       I     DOM_SUBSTRATE                  S
					8806       I HUMAN_DISTURBANCE               NONE
					8806       J    COVER_BOULDERS                  X
					8806       J       COVER_CLASS         'NO COVER'
					8806       J     DOM_SUBSTRATE                  C
					8806       J HUMAN_DISTURBANCE               NONE
					8860       A    COVER_BOULDERS                  X
					8860       A       COVER_CLASS     'PATCHY COVER'
					8860       A     DOM_SUBSTRATE                  C
					8860       A HUMAN_DISTURBANCE               NONE
					8860       B    COVER_BOULDERS                  X
					8860       B       COVER_CLASS     'PATCHY COVER'
					8860       B       COVER_WOODY                  X
					8860       B     DOM_SUBSTRATE                  C
					8860       B HUMAN_DISTURBANCE               NONE
					8860       C    COVER_BOULDERS                  X
					8860       C       COVER_CLASS     'PATCHY COVER'
					8860       C     DOM_SUBSTRATE                  C
					8860       C HUMAN_DISTURBANCE               NONE
					8860       D    COVER_BOULDERS                  X
					8860       D       COVER_CLASS     'PATCHY COVER'
					8860       D     DOM_SUBSTRATE                  C
					8860       D HUMAN_DISTURBANCE               NONE
					8860       E    COVER_BOULDERS                  X
					8860       E       COVER_CLASS     'PATCHY COVER'
					8860       E       COVER_WOODY                  X
					8860       E     DOM_SUBSTRATE                  C
					8860       E HUMAN_DISTURBANCE               NONE
					8860       F    COVER_BOULDERS                  X
					8860       F       COVER_CLASS     'PATCHY COVER'
					8860       F         COVER_VEG                  X
					8860       F       COVER_WOODY                  X
					8860       F     DOM_SUBSTRATE                  C
					8860       F HUMAN_DISTURBANCE               NONE
					8860       G    COVER_BOULDERS                  X
					8860       G       COVER_CLASS     'PATCHY COVER'
					8860       G         COVER_VEG                  X
					8860       G     DOM_SUBSTRATE                  S
					8860       G HUMAN_DISTURBANCE               NONE
					8860       H    COVER_BOULDERS                  X
					8860       H       COVER_CLASS     'PATCHY COVER'
					8860       H     DOM_SUBSTRATE                  C
					8860       H HUMAN_DISTURBANCE               NONE
					8860       I    COVER_BOULDERS                  X
					8860       I       COVER_CLASS     'PATCHY COVER'
					8860       I     DOM_SUBSTRATE                  C
					8860       I HUMAN_DISTURBANCE               NONE
					8860       J    COVER_BOULDERS                  X
					8860       J       COVER_CLASS     'PATCHY COVER'
					8860       J     DOM_SUBSTRATE                  C
					8860       J HUMAN_DISTURBANCE               NONE
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaLittoralMacrohabitatTest.createExpectedResults2007 <- function()
#
{
	tc <- textConnection("   SITE      METRIC                           VALUE
					7469 LMFPARTIFICIAL                               NA
					7469    LMFPBEDROCK                                0
					7469   LMFPBOULDERS                               NA
					7469     LMFPCOBBLE                                0
					7469 LMFPCONTINUOUS                              0.9
					7469       LMFPFILL                               NA
					7469     LMFPLITTLE                                0
					7469        LMFPMUD                                1
					7469       LMFPNONE                               NA
					7469     LMFPPATCHY                              0.1
					7469       LMFPSAND                                0
					7469        LMFPVEG                                1
					7469      LMFPWOODY                               NA
					7469       LMNCOVER                               10
					7469  LMNCOVERTYPES                               10
					7469       LMNHUMAN                                9
					7469   LMNSUBSTRATE                               10
					7469       LMOCOVER               'CONTINUOUS COVER'
					7469   LMOSUBSTRATE                                M
					7469      LMPWHUMAN                                0
					7474 LMFPARTIFICIAL                               NA
					7474    LMFPBEDROCK                                0
					7474   LMFPBOULDERS                              0.1
					7474     LMFPCOBBLE                              0.1
					7474 LMFPCONTINUOUS                              0.2
					7474       LMFPFILL                               NA
					7474     LMFPLITTLE                              0.5
					7474        LMFPMUD                              0.9
					7474       LMFPNONE                               NA
					7474     LMFPPATCHY                              0.3
					7474       LMFPSAND                                0
					7474        LMFPVEG                              0.9
					7474      LMFPWOODY                               NA
					7474       LMNCOVER                               10
					7474  LMNCOVERTYPES                               10
					7474       LMNHUMAN                               10
					7474   LMNSUBSTRATE                               10
					7474       LMOCOVER                       'NO COVER'
					7474   LMOSUBSTRATE                                M
					7474      LMPWHUMAN                                1
					7478 LMFPARTIFICIAL                               NA
					7478    LMFPBEDROCK                0.111111111111111
					7478   LMFPBOULDERS                              0.7
					7478     LMFPCOBBLE                0.555555555555556
					7478 LMFPCONTINUOUS                              0.1
					7478       LMFPFILL                               NA
					7478     LMFPLITTLE                              0.1
					7478        LMFPMUD                0.222222222222222
					7478       LMFPNONE                               NA
					7478     LMFPPATCHY                              0.8
					7478       LMFPSAND                0.111111111111111
					7478        LMFPVEG                              0.2
					7478      LMFPWOODY                              0.2
					7478       LMNCOVER                               10
					7478  LMNCOVERTYPES                               10
					7478       LMNHUMAN                               10
					7478   LMNSUBSTRATE                                9
					7478       LMOCOVER                   'PATCHY COVER'
					7478   LMOSUBSTRATE                                C
					7478      LMPWHUMAN                              0.5
					7490 LMFPARTIFICIAL                               NA
					7490    LMFPBEDROCK                                0
					7490   LMFPBOULDERS                              0.1
					7490     LMFPCOBBLE                              0.1
					7490 LMFPCONTINUOUS                                0
					7490       LMFPFILL                               NA
					7490     LMFPLITTLE                                1
					7490        LMFPMUD                              0.9
					7490       LMFPNONE                               NA
					7490     LMFPPATCHY                                0
					7490       LMFPSAND                                0
					7490        LMFPVEG                                1
					7490      LMFPWOODY                               NA
					7490       LMNCOVER                               10
					7490  LMNCOVERTYPES                               10
					7490       LMNHUMAN                               10
					7490   LMNSUBSTRATE                               10
					7490       LMOCOVER                       'NO COVER'
					7490   LMOSUBSTRATE                                M
					7490      LMPWHUMAN                                0
					7494 LMFPARTIFICIAL                              0.8
					7494    LMFPBEDROCK                              0.1
					7494   LMFPBOULDERS                              0.3
					7494     LMFPCOBBLE                              0.2
					7494 LMFPCONTINUOUS                                0
					7494       LMFPFILL                               NA
					7494     LMFPLITTLE                              0.9
					7494        LMFPMUD                              0.2
					7494       LMFPNONE                               NA
					7494     LMFPPATCHY                              0.1
					7494       LMFPSAND                              0.5
					7494        LMFPVEG                              0.3
					7494      LMFPWOODY                              0.3
					7494       LMNCOVER                               10
					7494  LMNCOVERTYPES                               10
					7494       LMNHUMAN                               10
					7494   LMNSUBSTRATE                               10
					7494       LMOCOVER                       'NO COVER'
					7494   LMOSUBSTRATE                                S
					7494      LMPWHUMAN                                1
					7503 LMFPARTIFICIAL                               NA
					7503    LMFPBEDROCK                                0
					7503   LMFPBOULDERS                              0.1
					7503     LMFPCOBBLE                                0
					7503 LMFPCONTINUOUS                                1
					7503       LMFPFILL                               NA
					7503     LMFPLITTLE                                0
					7503        LMFPMUD                                1
					7503       LMFPNONE                               NA
					7503     LMFPPATCHY                                0
					7503       LMFPSAND                                0
					7503        LMFPVEG                                1
					7503      LMFPWOODY                               NA
					7503       LMNCOVER                               10
					7503  LMNCOVERTYPES                               10
					7503       LMNHUMAN                               10
					7503   LMNSUBSTRATE                               10
					7503       LMOCOVER               'CONTINUOUS COVER'
					7503   LMOSUBSTRATE                                M
					7503      LMPWHUMAN                             0.37
					7508 LMFPARTIFICIAL                              0.6
					7508    LMFPBEDROCK                                0
					7508   LMFPBOULDERS                              0.1
					7508     LMFPCOBBLE                                0
					7508 LMFPCONTINUOUS                              0.1
					7508       LMFPFILL                               NA
					7508     LMFPLITTLE                              0.2
					7508        LMFPMUD                0.333333333333333
					7508       LMFPNONE                               NA
					7508     LMFPPATCHY                              0.7
					7508       LMFPSAND                0.666666666666667
					7508        LMFPVEG                              0.7
					7508      LMFPWOODY                              0.3
					7508       LMNCOVER                               10
					7508  LMNCOVERTYPES                               10
					7508       LMNHUMAN                               10
					7508   LMNSUBSTRATE                                9
					7508       LMOCOVER                   'PATCHY COVER'
					7508   LMOSUBSTRATE                                S
					7508      LMPWHUMAN                             0.25
					7521 LMFPARTIFICIAL                               NA
					7521    LMFPBEDROCK                              0.1
					7521   LMFPBOULDERS                              0.6
					7521     LMFPCOBBLE                              0.3
					7521 LMFPCONTINUOUS                              0.1
					7521       LMFPFILL                               NA
					7521     LMFPLITTLE                              0.1
					7521        LMFPMUD                              0.3
					7521       LMFPNONE                               NA
					7521     LMFPPATCHY                              0.8
					7521       LMFPSAND                              0.3
					7521        LMFPVEG                              0.7
					7521      LMFPWOODY                              0.9
					7521       LMNCOVER                               10
					7521  LMNCOVERTYPES                               10
					7521       LMNHUMAN                               10
					7521   LMNSUBSTRATE                               10
					7521       LMOCOVER                   'PATCHY COVER'
					7521   LMOSUBSTRATE                        'C, M, S'
					7521      LMPWHUMAN                                0
					7525 LMFPARTIFICIAL                              0.4
					7525    LMFPBEDROCK                                0
					7525   LMFPBOULDERS                               NA
					7525     LMFPCOBBLE                                0
					7525 LMFPCONTINUOUS                              0.3
					7525       LMFPFILL                               NA
					7525     LMFPLITTLE                              0.2
					7525        LMFPMUD                              0.7
					7525       LMFPNONE                               NA
					7525     LMFPPATCHY                              0.5
					7525       LMFPSAND                              0.3
					7525        LMFPVEG                                1
					7525      LMFPWOODY                              0.7
					7525       LMNCOVER                               10
					7525  LMNCOVERTYPES                               10
					7525       LMNHUMAN                               10
					7525   LMNSUBSTRATE                               10
					7525       LMOCOVER                   'PATCHY COVER'
					7525   LMOSUBSTRATE                                M
					7525      LMPWHUMAN                             0.51
					#7533 LMFPARTIFICIAL                               NA	# not calculated for lack of values
					7533    LMFPBEDROCK                                0
					#7533   LMFPBOULDERS                               NA	# not calculated for lack of values
					7533     LMFPCOBBLE               0.0909090909090909	# was 0.090909090909091
					#7533 LMFPCONTINUOUS                               NA	# not calculated for lack of values
					7533       LMFPFILL                               NA
					#7533     LMFPLITTLE                               NA	# not calculated for lack of values
					7533        LMFPMUD                0.727272727272727
					#7533       LMFPNONE                               NA	# not calculated for lack of values
					#7533     LMFPPATCHY                               NA	# not calculated for lack of values
					7533       LMFPSAND                0.181818181818182
					#7533        LMFPVEG                               NA	# not calculated for lack of values
					#7533      LMFPWOODY                               NA	# not calculated for lack of values
					#7533       LMNCOVER                                0	# not calculated for lack of values
					#7533  LMNCOVERTYPES                                0	# not calculated for lack of values
					#7533       LMNHUMAN                                0	# not calculated for lack of values
					7533   LMNSUBSTRATE                               11
					#7533       LMOCOVER                               NA	# not calculated for lack of values
					7533   LMOSUBSTRATE                                M
					#7533      LMPWHUMAN                               NA	# not calculated for lack of values
					7542 LMFPARTIFICIAL                               NA
					7542    LMFPBEDROCK                              0.1
					7542   LMFPBOULDERS                              0.2
					7542     LMFPCOBBLE                              0.2
					7542 LMFPCONTINUOUS                                0
					7542       LMFPFILL                               NA
					7542     LMFPLITTLE                              0.9
					7542        LMFPMUD                              0.1
					7542       LMFPNONE                              0.2
					7542     LMFPPATCHY                              0.1
					7542       LMFPSAND                              0.6
					7542        LMFPVEG                              0.1
					7542      LMFPWOODY                              0.7
					7542       LMNCOVER                               10
					7542  LMNCOVERTYPES                               10
					7542       LMNHUMAN                               10
					7542   LMNSUBSTRATE                               10
					7542       LMOCOVER                       'NO COVER'
					7542   LMOSUBSTRATE                                S
					7542      LMPWHUMAN                              0.2
					7784 LMFPARTIFICIAL                               NA
					7784    LMFPBEDROCK                                0
					7784   LMFPBOULDERS                               NA
					7784     LMFPCOBBLE                                0
					7784 LMFPCONTINUOUS                                1
					7784       LMFPFILL                               NA
					7784     LMFPLITTLE                                0
					7784        LMFPMUD                                0
					7784       LMFPNONE                               NA
					7784     LMFPPATCHY                                0
					7784       LMFPSAND                                1
					7784        LMFPVEG                                1
					7784      LMFPWOODY                               NA
					7784       LMNCOVER                                1
					7784  LMNCOVERTYPES                                1
					7784       LMNHUMAN                                1
					7784   LMNSUBSTRATE                                1
					7784       LMOCOVER               'CONTINUOUS COVER'
					7784   LMOSUBSTRATE                                S
					7784      LMPWHUMAN                                0
					8322 LMFPARTIFICIAL                               NA
					8322    LMFPBEDROCK                                0
					8322   LMFPBOULDERS                               NA
					8322     LMFPCOBBLE                                0
					8322 LMFPCONTINUOUS                                0
					8322       LMFPFILL                               NA
					8322     LMFPLITTLE                                1
					8322        LMFPMUD                             0.25
					8322       LMFPNONE                                1
					8322     LMFPPATCHY                                0
					8322       LMFPSAND                             0.75
					8322        LMFPVEG                               NA
					8322      LMFPWOODY                               NA
					8322       LMNCOVER                                4
					8322  LMNCOVERTYPES                                4
					8322       LMNHUMAN                                4
					8322   LMNSUBSTRATE                                4
					8322       LMOCOVER                       'NO COVER'
					8322   LMOSUBSTRATE                                S
					8322      LMPWHUMAN                              0.2
					8645 LMFPARTIFICIAL                              0.4
					8645    LMFPBEDROCK                                0
					8645   LMFPBOULDERS                              0.1
					8645     LMFPCOBBLE                                0
					8645 LMFPCONTINUOUS                              0.4
					8645       LMFPFILL                               NA
					8645     LMFPLITTLE                              0.2
					8645        LMFPMUD                              0.7
					8645       LMFPNONE                               NA
					8645     LMFPPATCHY                              0.4
					8645       LMFPSAND                              0.3
					8645        LMFPVEG                              0.8
					8645      LMFPWOODY                               NA
					8645       LMNCOVER                               10
					8645  LMNCOVERTYPES                               10
					8645       LMNHUMAN                               10
					8645   LMNSUBSTRATE                               10
					8645       LMOCOVER 'CONTINUOUS COVER, PATCHY COVER' # was 'PATCHY COVER, CONTINUOUS COVER'
					8645   LMOSUBSTRATE                                M
					8645      LMPWHUMAN                             0.18
					8806 LMFPARTIFICIAL                               NA
					8806    LMFPBEDROCK                                0
					8806   LMFPBOULDERS                              0.3
					8806     LMFPCOBBLE                              0.2
					8806 LMFPCONTINUOUS                                0
					8806       LMFPFILL                               NA
					8806     LMFPLITTLE                              0.5
					8806        LMFPMUD                              0.1
					8806       LMFPNONE                              0.1
					8806     LMFPPATCHY                              0.5
					8806       LMFPSAND                              0.7
					8806        LMFPVEG                               NA
					8806      LMFPWOODY                              0.7
					8806       LMNCOVER                               10
					8806  LMNCOVERTYPES                               10
					8806       LMNHUMAN                               10
					8806   LMNSUBSTRATE                               10
					8806       LMOCOVER         'NO COVER, PATCHY COVER'
					8806   LMOSUBSTRATE                                S
					8806      LMPWHUMAN                                0
					8860 LMFPARTIFICIAL                               NA
					8860    LMFPBEDROCK                                0
					8860   LMFPBOULDERS                                1
					8860     LMFPCOBBLE                              0.9
					8860 LMFPCONTINUOUS                                0
					8860       LMFPFILL                               NA
					8860     LMFPLITTLE                                0
					8860        LMFPMUD                                0
					8860       LMFPNONE                               NA
					8860     LMFPPATCHY                                1
					8860       LMFPSAND                              0.1
					8860        LMFPVEG                              0.2
					8860      LMFPWOODY                              0.3
					8860       LMNCOVER                               10
					8860  LMNCOVERTYPES                               10
					8860       LMNHUMAN                               10
					8860   LMNSUBSTRATE                               10
					8860       LMOCOVER                   'PATCHY COVER'
					8860   LMOSUBSTRATE                                C
					8860      LMPWHUMAN                                0
					")
	
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	fake <- within(subset(fake, METRIC != 'LMFPFILL')
			,{VALUE <- ifelse(grepl('^LMN.+', METRIC) & VALUE==0, NA, VALUE)		#### TEMPORARY WHILE DEVELOPING UNIT TEST
			}
	)
	return(fake)		
	
}

# end of file