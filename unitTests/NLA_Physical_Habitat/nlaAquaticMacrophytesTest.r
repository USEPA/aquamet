# nlasAquaticMacrophytesTest.r
# RUnit tests
#
#  5/20/15 cws Modified expected value in unit test for 8417 AMVEMERGENT
#          from 0.0783510618236211 to 0.078351061823621; the extra digit
#          had previously been added to 'avoid truncation by SAS', but
#          it seems to cause dfCompare to choke on it, and the easiest
#          thing to do now is just remove it.
#  6/29/17 cws Renamed from metsAquaticMacrophytesTest to nlasAquaticMacrophytesTest.
#          Using unit test with 2007 data as 2012 data test was not functioning.
#          Reverted change to single AMVEMERGENT.
#

nlaAquaticMacrophytesTest <- function()
# unit test for metsAquaticMacrophytes
# No need to create separate test with 2012 data as it has not changed.
{
	testData <- nlaAquaticMacrophytesTest.createTestData2007() 
	
    # Test with good data for all arguments
	expected <- nlaAquaticMacrophytesTest.createExpectedResults2007()
	actual <- nlaAquaticMacrophytes(emergent = testData %>% subset(PARAMETER=='AM_EMERGENT') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                               ,floating = testData %>% subset(PARAMETER=='AM_FLOATING') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                               ,submergent = testData %>% subset(PARAMETER=='AM_SUBMERGENT') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                               ,totalCover = testData %>% subset(PARAMETER=='AM_TOTALCOVER') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                               )

	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-16)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
	
	# Test cases when no data is sent
	expected <- NULL
	actual <- nlaAquaticMacrophytes()
	checkEquals(expected, actual, "Incorrect response when args are NULL")
	
	actual <- try(nlaAquaticMacrophytes(emergent = testData %>% subset(PARAMETER=='AM_EMERGENT') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                                   ,floating = testData %>% subset(PARAMETER=='AM_FLOATING') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                                   ,submergent = testData %>% subset(PARAMETER=='AM_SUBMERGENT') %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% select(SITE, STATION, VALUE)
	                                   ,totalCover = 'foo'
	                                   )
	             ,silent=TRUE
	             ) 
	checkTrue(class(actual) == 'try-error', "Incorrect response when an argument is unexpected class")
	
}


nlaAquaticMacrophytesTest.createTestData2007 <- function()
# Test data based on 2007 values
#	UID		Description
#	8258	Has 2 extra stations 
#	8417 	Has full data with a mix of cover values
# 	8435 	Has full data with zeros for all covers 
# 	8451	Has full data with zeros for AM_FLOATING and AM_EMERGENT
# 	8453 	Some stations completely absent 
{
	tc <- textConnection("   UID STATION     PARAMETER RESULT
							8258       A   AM_EMERGENT      2
							8258       A   AM_FLOATING      0
							8258       A AM_SUBMERGENT      0
							8258       A AM_TOTALCOVER      2
							8258       B   AM_EMERGENT      0
							8258       B   AM_FLOATING      0
							8258       B AM_SUBMERGENT      0
							8258       B AM_TOTALCOVER      0
							8258       C   AM_EMERGENT      4
							8258       C   AM_FLOATING      4
							8258       C AM_SUBMERGENT      4
							8258       C AM_TOTALCOVER      4
							8258       D   AM_EMERGENT      1
							8258       D   AM_FLOATING      0
							8258       D AM_SUBMERGENT      1
							8258       D AM_TOTALCOVER      1
							8258       E   AM_EMERGENT      0
							8258       E   AM_FLOATING      0
							8258       E AM_SUBMERGENT      2
							8258       E AM_TOTALCOVER      2
							8258       F   AM_EMERGENT      0
							8258       F   AM_FLOATING      0
							8258       F AM_SUBMERGENT      0
							8258       F AM_TOTALCOVER      0
							8258       G   AM_EMERGENT      1
							8258       G   AM_FLOATING      0
							8258       G AM_SUBMERGENT      0
							8258       G AM_TOTALCOVER      1
							8258       H   AM_EMERGENT      3
							8258       H   AM_FLOATING      0
							8258       H AM_SUBMERGENT      0
							8258       H AM_TOTALCOVER      2
							8258       I   AM_EMERGENT      3
							8258       I   AM_FLOATING      3
							8258       I AM_SUBMERGENT      3
							8258       I AM_TOTALCOVER      3
							8258       J   AM_EMERGENT      0
							8258       J   AM_FLOATING      0
							8258       J AM_SUBMERGENT      0
							8258       J AM_TOTALCOVER      0
							8258       K   AM_EMERGENT      2
							8258       K   AM_FLOATING      0
							8258       K AM_SUBMERGENT      0
							8258       K AM_TOTALCOVER      2
							8258       L   AM_EMERGENT      2
							8258       L   AM_FLOATING      0
							8258       L AM_SUBMERGENT      0
							8258       L AM_TOTALCOVER      2
							8417       A   AM_EMERGENT      1
							8417       A   AM_FLOATING      0
							8417       A AM_SUBMERGENT      3
							8417       A AM_TOTALCOVER      4
							8417       B   AM_EMERGENT      0
							8417       B   AM_FLOATING      0
							8417       B AM_SUBMERGENT      2
							8417       B AM_TOTALCOVER      2
							8417       C   AM_EMERGENT      0
							8417       C   AM_FLOATING      0
							8417       C AM_SUBMERGENT      1
							8417       C AM_TOTALCOVER      1
							8417       D   AM_EMERGENT      0
							8417       D   AM_FLOATING      0
							8417       D AM_SUBMERGENT      1
							8417       D AM_TOTALCOVER      1
							8417       E   AM_EMERGENT      0
							8417       E   AM_FLOATING      0
							8417       E AM_SUBMERGENT      0
							8417       E AM_TOTALCOVER      0
							8417       F   AM_EMERGENT      0
							8417       F   AM_FLOATING      0
							8417       F AM_SUBMERGENT      2
							8417       F AM_TOTALCOVER      2
							8417       G   AM_EMERGENT      0
							8417       G   AM_FLOATING      0
							8417       G AM_SUBMERGENT      1
							8417       G AM_TOTALCOVER      1
							8417       H   AM_EMERGENT      1
							8417       H   AM_FLOATING      0
							8417       H AM_SUBMERGENT      3
							8417       H AM_TOTALCOVER      4
							8417       I   AM_EMERGENT      0
							8417       I   AM_FLOATING      0
							8417       I AM_SUBMERGENT      2
							8417       I AM_TOTALCOVER      2
							8417       J   AM_EMERGENT      2
							8417       J   AM_FLOATING      0
							8417       J AM_SUBMERGENT      2
							8417       J AM_TOTALCOVER      4
							8435       A   AM_EMERGENT      0
							8435       A   AM_FLOATING      0
							8435       A AM_SUBMERGENT      0
							8435       A AM_TOTALCOVER      0
							8435       B   AM_EMERGENT      0
							8435       B   AM_FLOATING      0
							8435       B AM_SUBMERGENT      0
							8435       B AM_TOTALCOVER      0
							8435       C   AM_EMERGENT      0
							8435       C   AM_FLOATING      0
							8435       C AM_SUBMERGENT      0
							8435       C AM_TOTALCOVER      0
							8435       D   AM_EMERGENT      0
							8435       D   AM_FLOATING      0
							8435       D AM_SUBMERGENT      0
							8435       D AM_TOTALCOVER      0
							8435       E   AM_EMERGENT      0
							8435       E   AM_FLOATING      0
							8435       E AM_SUBMERGENT      0
							8435       E AM_TOTALCOVER      0
							8435       F   AM_EMERGENT      0
							8435       F   AM_FLOATING      0
							8435       F AM_SUBMERGENT      0
							8435       F AM_TOTALCOVER      0
							8435       G   AM_EMERGENT      0
							8435       G   AM_FLOATING      0
							8435       G AM_SUBMERGENT      0
							8435       G AM_TOTALCOVER      0
							8435       H   AM_EMERGENT      0
							8435       H   AM_FLOATING      0
							8435       H AM_SUBMERGENT      0
							8435       H AM_TOTALCOVER      0
							8435       I   AM_EMERGENT      0
							8435       I   AM_FLOATING      0
							8435       I AM_SUBMERGENT      0
							8435       I AM_TOTALCOVER      0
							8435       J   AM_EMERGENT      0
							8435       J   AM_FLOATING      0
							8435       J AM_SUBMERGENT      0
							8435       J AM_TOTALCOVER      0
							8451       A   AM_EMERGENT      0
							8451       A   AM_FLOATING      0
							8451       A AM_SUBMERGENT      1
							8451       A AM_TOTALCOVER      1
							8451       B   AM_EMERGENT      0
							8451       B   AM_FLOATING      0
							8451       B AM_SUBMERGENT      1
							8451       B AM_TOTALCOVER      1
							8451       C   AM_EMERGENT      0
							8451       C   AM_FLOATING      0
							8451       C AM_SUBMERGENT      0
							8451       C AM_TOTALCOVER      0
							8451       D   AM_EMERGENT      0
							8451       D   AM_FLOATING      0
							8451       D AM_SUBMERGENT      0
							8451       D AM_TOTALCOVER      0
							8451       E   AM_EMERGENT      0
							8451       E   AM_FLOATING      0
							8451       E AM_SUBMERGENT      2
							8451       E AM_TOTALCOVER      2
							8451       F   AM_EMERGENT      0
							8451       F   AM_FLOATING      0
							8451       F AM_SUBMERGENT      3
							8451       F AM_TOTALCOVER      3
							8451       G   AM_EMERGENT      0
							8451       G   AM_FLOATING      0
							8451       G AM_SUBMERGENT      0
							8451       G AM_TOTALCOVER      0
							8451       H   AM_EMERGENT      0
							8451       H   AM_FLOATING      0
							8451       H AM_SUBMERGENT      0
							8451       H AM_TOTALCOVER      0
							8451       I   AM_EMERGENT      0
							8451       I   AM_FLOATING      0
							8451       I AM_SUBMERGENT      0
							8451       I AM_TOTALCOVER      0
							8451       J   AM_EMERGENT      0
							8451       J   AM_FLOATING      0
							8451       J AM_SUBMERGENT      2
							8451       J AM_TOTALCOVER      0
							8453       A   AM_EMERGENT      2
							8453       A   AM_FLOATING      3
							8453       A AM_SUBMERGENT      3
							8453       A AM_TOTALCOVER      4
							8453       B   AM_EMERGENT      3
							8453       B   AM_FLOATING      2
							8453       B AM_SUBMERGENT      3
							8453       B AM_TOTALCOVER      4
							8453       H   AM_EMERGENT      3
							8453       H   AM_FLOATING      2
							8453       H AM_SUBMERGENT      3
							8453       H AM_TOTALCOVER      4
							8453       I   AM_EMERGENT      3
							8453       I   AM_FLOATING      3
							8453       I AM_SUBMERGENT      2
							8453       I AM_TOTALCOVER      4
							8453       J   AM_EMERGENT      3
							8453       J   AM_FLOATING      2
							8453       J AM_SUBMERGENT      3
							8453       J AM_TOTALCOVER      4
						")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL) %>%
	        mutate(RESULT = as.character(RESULT))
	close(tc)
	
	return(fake)		
					
}



nlaAquaticMacrophytesTest.createExpectedResults2007 <- function()
#
{
	tc <- textConnection("  SITE         METRIC             VALUE
							8258        AMFCALL 0.233333333333333
							8258   AMFCEMERGENT 0.239583333333333
							8258   AMFCFLOATING 0.120833333333333
							8258 AMFCSUBMERGENT 0.145833333333333
							8258        AMFPALL              0.75
							8258   AMFPEMERGENT 0.666666666666667
							8258   AMFPFLOATING 0.166666666666667
							8258 AMFPSUBMERGENT 0.333333333333333
							8258        AMIDALL             0.575
							8258        AMIQALL             0.225
							8258       AMITOTAL           0.50625
							8258         AMNALL                12
							8258    AMNEMERGENT                12
							8258    AMNFLOATING                12
							8258  AMNSUBMERGENT                12
							8258         AMVALL 0.262923539986034
							8258    AMVEMERGENT 0.290856558940636
							8258    AMVFLOATING 0.289363198430728
							8258  AMVSUBMERGENT 0.286997254764613
							8417        AMFCALL            0.3525
							8417   AMFCEMERGENT             0.035
							8417   AMFCFLOATING                 0
							8417 AMFCSUBMERGENT              0.23
							8417        AMFPALL               0.9
							8417   AMFPEMERGENT               0.3
							8417   AMFPFLOATING                 0
							8417 AMFPSUBMERGENT               0.9
							8417        AMIDALL              0.85
							8417        AMIQALL             0.825
							8417       AMITOTAL             0.265
							8417         AMNALL                10
							8417    AMNEMERGENT                10
							8417    AMNFLOATING                10
							8417  AMNSUBMERGENT                10
							8417         AMVALL 0.372575495705233
							8417    AMVEMERGENT 0.078351061823621 # was added one last digit '1' on 6/29 so character comparison would match, not on 7/3
							8417    AMVFLOATING                 0
							8417  AMVSUBMERGENT 0.208099869186781
							8435        AMFCALL                 0
							8435   AMFCEMERGENT                 0
							8435   AMFCFLOATING                 0
							8435 AMFCSUBMERGENT                 0
							8435        AMFPALL                 0
							8435   AMFPEMERGENT                 0
							8435   AMFPFLOATING                 0
							8435 AMFPSUBMERGENT                 0
							8435        AMIDALL                 0
							8435        AMIQALL                 0
							8435       AMITOTAL                 0
							8435         AMNALL                10
							8435    AMNEMERGENT                10
							8435    AMNFLOATING                10
							8435  AMNSUBMERGENT                10
							8435         AMVALL                 0
							8435    AMVEMERGENT                 0
							8435    AMVFLOATING                 0
							8435  AMVSUBMERGENT                 0
							8451        AMFCALL            0.0925
							8451   AMFCEMERGENT                 0
							8451   AMFCFLOATING                 0
							8451 AMFCSUBMERGENT            0.1175
							8451        AMFPALL               0.4
							8451   AMFPEMERGENT                 0
							8451   AMFPFLOATING                 0
							8451 AMFPSUBMERGENT               0.5
							8451        AMIDALL            0.4125
							8451        AMIQALL              0.05
							8451       AMITOTAL            0.1175
							8451         AMNALL                10
							8451    AMNEMERGENT                10
							8451    AMNFLOATING                10
							8451  AMNSUBMERGENT                10
							8451         AMVALL 0.186357631093193
							8451    AMVEMERGENT                 0
							8451    AMVFLOATING                 0
							8451  AMVSUBMERGENT 0.189315286698612
							8453        AMFCALL             0.875
							8453   AMFCEMERGENT              0.51
							8453   AMFCFLOATING              0.38
							8453 AMFCSUBMERGENT              0.51
							8453        AMFPALL                 1
							8453   AMFPEMERGENT                 1
							8453   AMFPFLOATING                 1
							8453 AMFPSUBMERGENT                 1
							8453        AMIDALL                 0
							8453        AMIQALL                 0
							8453       AMITOTAL               1.4
							8453         AMNALL                 5
							8453    AMNEMERGENT                 5
							8453    AMNFLOATING                 5
							8453  AMNSUBMERGENT                 5
							8453         AMVALL                 0
							8453    AMVEMERGENT 0.145344418537486
							8453    AMVFLOATING 0.178009831189179
							8453  AMVSUBMERGENT 0.145344418537486
						")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)

	fake$VALUE <- as.character(fake$VALUE)
	return(fake)		

}

# end of file
