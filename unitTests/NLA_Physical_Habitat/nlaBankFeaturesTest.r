# nlaBankFeaturesTest.r
# RUnit tests
#  7/18/17 cws Corrected to expect metrics values in METRICS column
#


nlaBankFeatures.fillinDistancesTest <- function()
# unit test for nlaBankFeatures.fillinDistances
#
# SITE distsPresent tests case where distance is always present
# SITE distsNA tests case where distances rows are present and recorded as NA
# SITE distsAbsent tests case where distances rows are not present in the data
#   and not all stations were sampled.
# SITE drawdownAbsent tests case where DRAWDOWN and HORIZ_DIST_DD is absent
#   in the data and not all stations were sampled. In this case, no changes
#   are made to the data for this site.
#
{
	testData <- data.frame(SITE=c(rep('distsPresent',4*10)
	                            ,rep('distsNA', 4*10)
				                ,rep('distsAbsent', 2*8)
				                ,rep('drawdownAbsent', 2*8)
								)
						  ,STATION=c(rep(LETTERS[1:10], each=4)
				  					,rep(LETTERS[1:10], each=4)
									,rep(LETTERS[1:8], each=2)
									,rep(LETTERS[1:8], each=2)
									)
						  ,PARAMETER=c(rep(c('DRAWDOWN','HORIZ_DIST_DD','VERT_HEIGHT_DD','other'), times=10)
								  	  ,rep(c('DRAWDOWN','HORIZ_DIST_DD','VERT_HEIGHT_DD','other'), times=10)
									  ,rep(c('DRAWDOWN','other'), times=8)
									  ,rep(c('VERT_HEIGHT_DD','other'), times=8)
									  )
						  ,VALUE=c(rep(c('YES','1','2','x','NO','2','3','y',NA,'3','4','z'), length=10*4)
				                   ,rep(c('YES', NA, NA,'x','NO', NA, NA,'y',NA, NA, NA,'z'), length=10*4)
								   ,rep(c('YES','x','NO','y',NA,'z'), length=8*2)
								   ,rep(c('1','x','2','y',NA,'z'), length=8*2)
								   )
						  ,stringsAsFactors=FALSE
						  )

	expected <- data.frame(SITE=c(rep('distsPresent',4*10)
	                           	,rep('distsNA', 4*10)
			                	,rep('distsAbsent', 4*8)
								,rep('drawdownAbsent', 2*8)
								)
					  	  ,STATION=c(rep(LETTERS[1:10], each=4)
			  						,rep(LETTERS[1:10], each=4)
									,rep(LETTERS[1:8], each=4)
									,rep(LETTERS[1:8], each=2)
									)
					  	  ,PARAMETER=c(rep(c('DRAWDOWN','HORIZ_DIST_DD','VERT_HEIGHT_DD','other'), times=10)
							  	  	  ,rep(c('DRAWDOWN','HORIZ_DIST_DD','VERT_HEIGHT_DD','other'), times=10)
								  	  ,rep(c('DRAWDOWN','HORIZ_DIST_DD','VERT_HEIGHT_DD','other'), times=8)
									  ,rep(c('VERT_HEIGHT_DD','other'), times=8)
								  	  )
					  	  ,VALUE=c(rep(c('YES','1','2','x','NO','2','3','y',NA,'3','4','z'), length=10*4)
			                   	   ,rep(c('YES', NA, NA,'x','NO','0','0','y',NA, NA, NA,'z'), length=10*4)
							   	   ,rep(c('YES', NA, NA,'x','NO','0','0','y',NA, NA, NA,'z'), length=8*4)
								   ,rep(c('1','x','2','y',NA,'z'), length=8*2)
				   				   )
					 	  ,stringsAsFactors=FALSE
						  )
	expected <- subset(expected, PARAMETER != 'other')
	
	
	actual <- nlaBankFeatures.fillinDistances(testData)

	diff <- dfCompare(expected, actual, c('SITE','STATION','PARAMETER'))
	checkEquals(NULL, diff, "Incorrect filling in of drawdown distances")
}


nlaBankFeaturesTest <- function()
# unit test for nlaBankFeatures
{
	nlaBankFeaturesTest.2007()
	nlaBankFeaturesTest.2012withDrawdown()
	nlaBankFeaturesTest.2012noDrawdown()
}


nlaBankFeaturesTest.2007 <- function()
#
{
	testData <- nlaBankFeaturesTest.createTestData2007()
	expected <- nlaBankFeaturesTest.createExpectedResults2007()
	actual <- nlaBankFeatures(angle = testData %>% subset(PARAMETER=='ANGLE') %>% select(SITE,STATION,VALUE)
                             ,drawdown = testData %>% subset(PARAMETER=='DRAWDOWN') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistance = testData %>% subset(PARAMETER=='HORIZ_DIST') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistanceDrawdown = testData %>% subset(PARAMETER=='HORIZ_DIST_DD') %>% select(SITE,STATION,VALUE)
                             ,verticalHeight = testData %>% subset(PARAMETER=='VERT_HEIGHT') %>% select(SITE,STATION,VALUE)
                             ,verticalHeightDrawdown = testData %>% subset(PARAMETER=='VERT_HEIGHT_DD') %>% select(SITE,STATION,VALUE)
                             )
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}


nlaBankFeaturesTest.2012withDrawdown <- function()
# Test with 2012 data with DRAWDOWN values
{
	testData <- nlaBankFeaturesTest.createTestData2012()
	expected <- nlaBankFeaturesTest.createExpectedResults2012withDrawdown()
	actual <- nlaBankFeatures(angle = testData %>% subset(PARAMETER=='ANGLE') %>% select(SITE,STATION,VALUE)
                             ,drawdown = testData %>% subset(PARAMETER=='DRAWDOWN') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistance = testData %>% subset(PARAMETER=='HORIZ_DIST') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistanceDrawdown = testData %>% subset(PARAMETER=='HORIZ_DIST_DD') %>% select(SITE,STATION,VALUE)
                             ,verticalHeight = testData %>% subset(PARAMETER=='VERT_HEIGHT') %>% select(SITE,STATION,VALUE)
                             ,verticalHeightDrawdown = testData %>% subset(PARAMETER=='VERT_HEIGHT_DD') %>% select(SITE,STATION,VALUE)
                             )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}



nlaBankFeaturesTest.2012noDrawdown <- function()
# Test with 2012 data lacking DRAWDOWN values
{
	testData <- subset(nlaBankFeaturesTest.createTestData2012(), PARAMETER != 'DRAWDOWN')
	expected <- nlaBankFeaturesTest.createExpectedResults2012noDrawdown()
	actual <- nlaBankFeatures(angle = testData %>% subset(PARAMETER=='ANGLE') %>% select(SITE,STATION,VALUE)
                             ,drawdown = testData %>% subset(PARAMETER=='DRAWDOWN') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistance = testData %>% subset(PARAMETER=='HORIZ_DIST') %>% select(SITE,STATION,VALUE)
                             ,horizontalDistanceDrawdown = testData %>% subset(PARAMETER=='HORIZ_DIST_DD') %>% select(SITE,STATION,VALUE)
                             ,verticalHeight = testData %>% subset(PARAMETER=='VERT_HEIGHT') %>% select(SITE,STATION,VALUE)
                             ,verticalHeightDrawdown = testData %>% subset(PARAMETER=='VERT_HEIGHT_DD') %>% select(SITE,STATION,VALUE)
                             )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of metrics")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-14)
	checkTrue(is.null(diff), "Incorrect calculation of metrics")
}



nlaBankFeaturesTest.createTestData2007 <- function()
#
{
	tc <- textConnection("   SITE STATION   PARAMETER        VALUE
							7468       A       ANGLE         STEEP
							7468       B       ANGLE       GRADUAL
							7468       C       ANGLE         STEEP
							7468       D       ANGLE          FLAT
							7468       E       ANGLE       GRADUAL
							7468       F       ANGLE       GRADUAL
							7468       G       ANGLE         STEEP
							7468       H       ANGLE 'NEAR VERTICAL'
							7468       I       ANGLE 'NEAR VERTICAL'
							7468       A  HORIZ_DIST             0
							7468       B  HORIZ_DIST           0.4
							7468       C  HORIZ_DIST             0
							7468       D  HORIZ_DIST             0
							7468       E  HORIZ_DIST             0
							7468       F  HORIZ_DIST             0
							7468       G  HORIZ_DIST             0
							7468       H  HORIZ_DIST             0
							7468       I  HORIZ_DIST             0
							7468       A VERT_HEIGHT             0
							7468       B VERT_HEIGHT           0.2
							7468       C VERT_HEIGHT             0
							7468       D VERT_HEIGHT             0
							7468       E VERT_HEIGHT             0
							7468       F VERT_HEIGHT             0
							7468       G VERT_HEIGHT             0
							7468       H VERT_HEIGHT             0
							7468       I VERT_HEIGHT             0
							7492       A       ANGLE          FLAT
							7492       B       ANGLE       GRADUAL
							7492       C       ANGLE          FLAT
							7492       D       ANGLE          FLAT
							7492       F       ANGLE          FLAT
							7492       H       ANGLE          FLAT
							7492       I       ANGLE       GRADUAL
							7492       J       ANGLE       GRADUAL
							7492       A  HORIZ_DIST            95
							7492       B  HORIZ_DIST            30
							7492       C  HORIZ_DIST            50
							7492       D  HORIZ_DIST            38
							7492       F  HORIZ_DIST            75
							7492       H  HORIZ_DIST            22
							7492       I  HORIZ_DIST            54
							7492       J  HORIZ_DIST            23
							7492       A VERT_HEIGHT             1
							7492       B VERT_HEIGHT           1.5
							7492       C VERT_HEIGHT           0.8
							7492       D VERT_HEIGHT           0.8
							7492       F VERT_HEIGHT           0.5
							7492       H VERT_HEIGHT           0.8
							7492       I VERT_HEIGHT           1.2
							7492       J VERT_HEIGHT             2
							7515       A       ANGLE       GRADUAL
							7515       B       ANGLE       GRADUAL
							7515       C       ANGLE       GRADUAL
							7515       D       ANGLE       GRADUAL
							7515       E       ANGLE       GRADUAL
							7515       F       ANGLE       GRADUAL
							7515       G       ANGLE       GRADUAL
							7515       H       ANGLE       GRADUAL
							7515       I       ANGLE       GRADUAL
							7515       J       ANGLE       GRADUAL
							7515       A  HORIZ_DIST           0.1
							7515       B  HORIZ_DIST             0
							7515       C  HORIZ_DIST             0
							7515       D  HORIZ_DIST           0.1
							7515       E  HORIZ_DIST           0.1
							7515       F  HORIZ_DIST             0
							7515       G  HORIZ_DIST           0.1
							7515       H  HORIZ_DIST             0
							7515       I  HORIZ_DIST           0.1
							7515       J  HORIZ_DIST           0.1
							7515       A VERT_HEIGHT             0
							7515       B VERT_HEIGHT             0
							7515       C VERT_HEIGHT             0
							7515       D VERT_HEIGHT             0
							7515       E VERT_HEIGHT           0.1
							7515       F VERT_HEIGHT             0
							7515       G VERT_HEIGHT           0.1
							7515       H VERT_HEIGHT           0.1
							7515       I VERT_HEIGHT             0
							7515       J VERT_HEIGHT           0.1
							7529       A       ANGLE          FLAT
							7529       B       ANGLE          FLAT
							7529       C       ANGLE          FLAT
							7529       D       ANGLE          FLAT
							7529       E       ANGLE          FLAT
							7529       F       ANGLE          FLAT
							7529       G       ANGLE          FLAT
							7529       H       ANGLE          FLAT
							7529       I       ANGLE          FLAT
							7529       J       ANGLE          FLAT
							7529       A  HORIZ_DIST           0.2
							7529       B  HORIZ_DIST           0.2
							7529       C  HORIZ_DIST           0.2
							7529       D  HORIZ_DIST           0.2
							7529       E  HORIZ_DIST           0.2
							7529       F  HORIZ_DIST           0.2
							7529       G  HORIZ_DIST           0.2
							7529       H  HORIZ_DIST           0.2
							7529       I  HORIZ_DIST           0.2
							7529       J  HORIZ_DIST           0.2
							7529       A VERT_HEIGHT           0.2
							7529       B VERT_HEIGHT           0.2
							7529       C VERT_HEIGHT           0.2
							7529       D VERT_HEIGHT           0.2
							7529       E VERT_HEIGHT           0.2
							7529       F VERT_HEIGHT           0.2
							7529       G VERT_HEIGHT           0.2
							7529       H VERT_HEIGHT           0.2
							7529       I VERT_HEIGHT           0.2
							7529       J VERT_HEIGHT           0.2
							7533       A       ANGLE       GRADUAL
							7533       B       ANGLE       GRADUAL
							7533       C       ANGLE       GRADUAL
							7533       D       ANGLE       GRADUAL
							7533       E       ANGLE       GRADUAL
							7533       F       ANGLE       GRADUAL
							7533       G       ANGLE       GRADUAL
							7533       I       ANGLE       GRADUAL
							7533       J       ANGLE       GRADUAL
							7533       Z       ANGLE       GRADUAL
							7533       A  HORIZ_DIST           0.2
							7533       B  HORIZ_DIST           0.2
							7533       C  HORIZ_DIST           0.3
							7533       D  HORIZ_DIST           0.3
							7533       E  HORIZ_DIST           0.3
							7533       F  HORIZ_DIST           0.3
							7533       G  HORIZ_DIST           0.3
							7533       I  HORIZ_DIST           0.3
							7533       J  HORIZ_DIST           0.2
							7533       Z  HORIZ_DIST           0.1
							7533       A VERT_HEIGHT           0.3
							7533       B VERT_HEIGHT           0.3
							7533       C VERT_HEIGHT           0.3
							7533       D VERT_HEIGHT           0.2
							7533       E VERT_HEIGHT           0.2
							7533       F VERT_HEIGHT           0.2
							7533       G VERT_HEIGHT           0.2
							7533       I VERT_HEIGHT           0.2
							7533       J VERT_HEIGHT           0.1
							7533       Z VERT_HEIGHT           0.2
							7721       A       ANGLE          FLAT
							7721       B       ANGLE          FLAT
							7721       C       ANGLE          FLAT
							7721       D       ANGLE          FLAT
							7721       E       ANGLE          FLAT
							7721       F       ANGLE          FLAT
							7721       G       ANGLE          FLAT
							7721       H       ANGLE         STEEP
							7721       I       ANGLE       GRADUAL
							7721       J       ANGLE          FLAT
							7721       A  HORIZ_DIST            50
							7721       B  HORIZ_DIST            25
							7721       C  HORIZ_DIST            50
							7721       D  HORIZ_DIST           100
							7721       E  HORIZ_DIST           200
							7721       F  HORIZ_DIST            50
							7721       G  HORIZ_DIST            30
							7721       H  HORIZ_DIST             5
							7721       I  HORIZ_DIST            20
							7721       J  HORIZ_DIST           300
							7721       A VERT_HEIGHT             5
							7721       B VERT_HEIGHT             5
							7721       C VERT_HEIGHT             5
							7721       D VERT_HEIGHT             2
							7721       E VERT_HEIGHT             1
							7721       F VERT_HEIGHT             3
							7721       G VERT_HEIGHT             5
							7721       H VERT_HEIGHT             5
							7721       I VERT_HEIGHT             5
							7721       J VERT_HEIGHT             2
							7784       J       ANGLE          FLAT
							7784       J  HORIZ_DIST             1
							7784       J VERT_HEIGHT           0.1
							7797       A       ANGLE         STEEP
							7797       B       ANGLE          FLAT
							7797       C       ANGLE       GRADUAL
							7797       D       ANGLE       GRADUAL
							7797       E       ANGLE          FLAT
							7797       F       ANGLE          FLAT
							7797       G       ANGLE       GRADUAL
							7797       H       ANGLE       GRADUAL
							7797       I       ANGLE         STEEP
							7797       J       ANGLE          FLAT
							7797       K       ANGLE       GRADUAL
							7797       L       ANGLE       GRADUAL
							7797       M       ANGLE       GRADUAL
							7797       A  HORIZ_DIST             0
							7797       B  HORIZ_DIST             0
							7797       C  HORIZ_DIST             0
							7797       D  HORIZ_DIST             0
							7797       E  HORIZ_DIST             0
							7797       F  HORIZ_DIST             0
							7797       G  HORIZ_DIST             0
							7797       H  HORIZ_DIST             0
							7797       I  HORIZ_DIST             0
							7797       J  HORIZ_DIST             0
							7797       K  HORIZ_DIST             0
							7797       L  HORIZ_DIST             0
							7797       M  HORIZ_DIST             0
							7797       A VERT_HEIGHT           0.1
							7797       B VERT_HEIGHT             0
							7797       C VERT_HEIGHT             0
							7797       D VERT_HEIGHT             0
							7797       E VERT_HEIGHT             0
							7797       F VERT_HEIGHT             0
							7797       G VERT_HEIGHT             0
							7797       H VERT_HEIGHT             0
							7797       I VERT_HEIGHT             0
							7797       J VERT_HEIGHT             0
							7797       K VERT_HEIGHT             0
							7797       L VERT_HEIGHT             0
							7797       M VERT_HEIGHT             0
							8125       A       ANGLE       GRADUAL
							8125       B       ANGLE       GRADUAL
							8125       C       ANGLE          FLAT
							8125       D       ANGLE          FLAT
							8125       E       ANGLE         STEEP
							8125       F       ANGLE          FLAT
							8125       G       ANGLE          FLAT
							8125       H       ANGLE          FLAT
							8125       I       ANGLE       GRADUAL
							8125       J       ANGLE       GRADUAL
							8125       A  HORIZ_DIST             1
							8125       B  HORIZ_DIST             2
							8125       C  HORIZ_DIST             1
							8125       D  HORIZ_DIST             1
							8125       E  HORIZ_DIST           0.3
							8125       F  HORIZ_DIST             3
							8125       G  HORIZ_DIST             3
							8125       H  HORIZ_DIST             3
							8125       I  HORIZ_DIST             2
							8125       J  HORIZ_DIST             2
							8125       A VERT_HEIGHT           0.3
							8125       B VERT_HEIGHT           0.3
							8125       C VERT_HEIGHT           0.3
							8125       D VERT_HEIGHT           0.3
							8125       E VERT_HEIGHT           0.3
							8125       F VERT_HEIGHT           0.3
							8125       G VERT_HEIGHT           0.3
							8125       H VERT_HEIGHT           0.3
							8125       I VERT_HEIGHT           0.3
							8125       J VERT_HEIGHT           0.3
						")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
					
}


nlaBankFeaturesTest.createExpectedResults2007 <- function()
#
{
	tc <- textConnection("   SITE       METRIC              VALUE
							7468       BFFFLAT   0.111111111111111
							7468    BFFGRADUAL   0.333333333333333
							7468      BFFSTEEP   0.333333333333333
							7468   BFFVERTICAL   0.222222222222222
							7468      BFNANGLE                   9
							7468  BFNHORIZDIST                   9
							7468 BFNVERTHEIGHT                   9
							7468      BFOANGLE      'GRADUAL, STEEP'
							7468  BFXHORIZDIST  0.0444444444444444
							7468 BFXVERTHEIGHT  0.0222222222222222
							7492       BFFFLAT               0.625
							7492    BFFGRADUAL               0.375
							7492      BFFSTEEP                   0
							7492   BFFVERTICAL                   0
							7492      BFNANGLE                   8
							7492  BFNHORIZDIST                   8
							7492 BFNVERTHEIGHT                   8
							7492      BFOANGLE                FLAT
							7492  BFXHORIZDIST              48.375
							7492 BFXVERTHEIGHT               1.075
							7515       BFFFLAT                   0
							7515    BFFGRADUAL                   1
							7515      BFFSTEEP                   0
							7515   BFFVERTICAL                   0
							7515      BFNANGLE                  10
							7515  BFNHORIZDIST                  10
							7515 BFNVERTHEIGHT                  10
							7515      BFOANGLE             GRADUAL
							7515  BFXHORIZDIST                0.06
							7515 BFXVERTHEIGHT                0.04
							7529       BFFFLAT                   1
							7529    BFFGRADUAL                   0
							7529      BFFSTEEP                   0
							7529   BFFVERTICAL                   0
							7529      BFNANGLE                  10
							7529  BFNHORIZDIST                  10
							7529 BFNVERTHEIGHT                  10
							7529      BFOANGLE                FLAT
							7529  BFXHORIZDIST                 0.2
							7529 BFXVERTHEIGHT                 0.2
							7533       BFFFLAT                   0
							7533    BFFGRADUAL                   1
							7533      BFFSTEEP                   0
							7533   BFFVERTICAL                   0
							7533      BFNANGLE                  10
							7533  BFNHORIZDIST                  10
							7533 BFNVERTHEIGHT                  10
							7533      BFOANGLE             GRADUAL
							7533  BFXHORIZDIST                0.25
							7533 BFXVERTHEIGHT                0.22
							7721       BFFFLAT                 0.8
							7721    BFFGRADUAL                 0.1
							7721      BFFSTEEP                 0.1
							7721   BFFVERTICAL                   0
							7721      BFNANGLE                  10
							7721  BFNHORIZDIST                  10
							7721 BFNVERTHEIGHT                  10
							7721      BFOANGLE                FLAT
							7721  BFXHORIZDIST                  83
							7721 BFXVERTHEIGHT                 3.8
							7784       BFFFLAT                   1
							7784    BFFGRADUAL                   0
							7784      BFFSTEEP                   0
							7784   BFFVERTICAL                   0
							7784      BFNANGLE                   1
							7784  BFNHORIZDIST                   1
							7784 BFNVERTHEIGHT                   1
							7784      BFOANGLE                FLAT
							7784  BFXHORIZDIST                   1
							7784 BFXVERTHEIGHT                 0.1
							7797       BFFFLAT   0.307692307692308
							7797    BFFGRADUAL   0.538461538461538
							7797      BFFSTEEP   0.153846153846154
							7797   BFFVERTICAL                   0
							7797      BFNANGLE                  13
							7797  BFNHORIZDIST                  13
							7797 BFNVERTHEIGHT                  13
							7797      BFOANGLE             GRADUAL
							7797  BFXHORIZDIST                   0
							7797 BFXVERTHEIGHT 0.00769230769230769
							8125       BFFFLAT                 0.5
							8125    BFFGRADUAL                 0.4
							8125      BFFSTEEP                 0.1
							8125   BFFVERTICAL                   0
							8125      BFNANGLE                  10
							8125  BFNHORIZDIST                  10
							8125 BFNVERTHEIGHT                  10
							8125      BFOANGLE                FLAT
							8125  BFXHORIZDIST                1.83
							8125 BFXVERTHEIGHT                 0.3
					")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaBankFeaturesTest.createTestData2012 <- function()
# Returns dataframe of test data using 2012 data.
#
# 6160	Has 10 HORIZ_DIST_DD and 10 VERT_HEIGHT_DD, no non-DD values
# 6192	Has 1 ANGLE: NEAR_VERTICAL_UNDERCUT
# 6194	Has 10 HORIZ_DIST and 10 VERT_HEIGHT, no DD values
# 6227	Has 5 HORIZ_DIST_DD and 5 VERT_HEIGHT_DD, no non-DD values
# 6231	Has 10 ANGLE, all GRADUAL 
# 6237	Has 10 ANGLE, all FLAT
# 6279	Has 1 VERTICAL_HEIGHT only
# 6321	Has 1 HORIZ_DIST_DD only
# 6326	10 HORIZ_DIST and 1 HORIZ_DIST_DD (one overlap) and 1 VERT_HEIGHT and 6 VERT_HEIGHT_DD (one overlap)
# 6362	Has 1 ANGLE: GRADUAL
# 6374	Has all 4 ANGLE values
# 6397	Has 1 HORIZ_DIST only
# 6404	Has 2 missing ANGLE
# 6427	has 7 HORIZ_DIST and 4 HORIZ_DIST_DD with some overlap; Has 7 VERT_HEIGHT and 4 HORIZ_DIST_DD with 4 overlap
# 6444	Has 2 ANGLE modes
# 6449	Has 12 stations with no missing values; ANGLE always GRADUAL
# 6659	Has 9 HORIZ_DIST and VERT_HEIGHT and 1 HORIZ_DIST_DD and VERT_HEIGHT_DD with no overlap
# 6735	Has 10 values of everything, all distances are 0.
# 
{
	tc <- textConnection("   SITE STATION      PARAMETER                 VALUE
							6160       A          ANGLE                  STEEP
							6160       A  HORIZ_DIST_DD                    6.2
							6160       A VERT_HEIGHT_DD                    2.0
							6160       B          ANGLE                  STEEP
							6160       B  HORIZ_DIST_DD                    5.0
							6160       B VERT_HEIGHT_DD                    2.0
							6160       C          ANGLE                  STEEP
							6160       C  HORIZ_DIST_DD                    4.9
							6160       C VERT_HEIGHT_DD                    2.0
							6160       D          ANGLE                  STEEP
							6160       D  HORIZ_DIST_DD                    4.0
							6160       D VERT_HEIGHT_DD                    2.0
							6160       E          ANGLE                  STEEP
							6160       E  HORIZ_DIST_DD                    4.0
							6160       E VERT_HEIGHT_DD                    2.0
							6160       F          ANGLE                  STEEP
							6160       F  HORIZ_DIST_DD                    3.5
							6160       F VERT_HEIGHT_DD                    2.0
							6160       G          ANGLE                  STEEP
							6160       G  HORIZ_DIST_DD                    7.0
							6160       G VERT_HEIGHT_DD                    2.0
							6160       H          ANGLE                GRADUAL
							6160       H  HORIZ_DIST_DD                    23.
							6160       H VERT_HEIGHT_DD                    2.0
							6160       I          ANGLE                  STEEP
							6160       I  HORIZ_DIST_DD                    4.3
							6160       I VERT_HEIGHT_DD                    2.0
							6160       J          ANGLE                  STEEP
							6160       J  HORIZ_DIST_DD                    4.5
							6160       J VERT_HEIGHT_DD                    2.0
							6192       D          ANGLE NEAR_VERTICAL_UNDERCUT
							6194       A          ANGLE                  STEEP
							6194       A     HORIZ_DIST                    8.0
							6194       A    VERT_HEIGHT                    2.0
							6194       B          ANGLE                GRADUAL
							6194       B     HORIZ_DIST                    5.0
							6194       B    VERT_HEIGHT                    1.7
							6194       C          ANGLE                  STEEP
							6194       C     HORIZ_DIST                    4.0
							6194       C    VERT_HEIGHT                    2.0
							6194       D          ANGLE                  STEEP
							6194       D     HORIZ_DIST                    3.0
							6194       D    VERT_HEIGHT                    1.0
							6194       E          ANGLE NEAR_VERTICAL_UNDERCUT
							6194       E     HORIZ_DIST                    4.0
							6194       E    VERT_HEIGHT                    1.3
							6194       F          ANGLE                  STEEP
							6194       F     HORIZ_DIST                    1.2
							6194       F    VERT_HEIGHT                    1.7
							6194       G          ANGLE                  STEEP
							6194       G     HORIZ_DIST                    0.5
							6194       G    VERT_HEIGHT                    0.4
							6194       H          ANGLE                GRADUAL
							6194       H     HORIZ_DIST                    8.0
							6194       H    VERT_HEIGHT                    1.0
							6194       I          ANGLE                  STEEP
							6194       I     HORIZ_DIST                    1.0
							6194       I    VERT_HEIGHT                    1.5
							6194       J          ANGLE                  STEEP
							6194       J     HORIZ_DIST                    1.0
							6194       J    VERT_HEIGHT                    1.3
							6227       A          ANGLE                GRADUAL
							6227       A  HORIZ_DIST_DD                    2.4
							6227       A VERT_HEIGHT_DD                    0.6
							6227       B          ANGLE                GRADUAL
							6227       B  HORIZ_DIST_DD                    1.0
							6227       B VERT_HEIGHT_DD                    0.4
							6227       C          ANGLE                   FLAT
							6227       D          ANGLE                   FLAT
							6227       E          ANGLE                   FLAT
							6227       F          ANGLE                   FLAT
							6227       G          ANGLE                   FLAT
							6227       H          ANGLE                  STEEP
							6227       H  HORIZ_DIST_DD                    0.8
							6227       H VERT_HEIGHT_DD                    1.0
							6227       I          ANGLE                  STEEP
							6227       I  HORIZ_DIST_DD                    2.0
							6227       I VERT_HEIGHT_DD                    0.2
							6227       J          ANGLE                GRADUAL
							6227       J  HORIZ_DIST_DD                    2.9
							6227       J VERT_HEIGHT_DD                    0.2
							6231       A          ANGLE                GRADUAL
							6231       A  HORIZ_DIST_DD                    2.8
							6231       A VERT_HEIGHT_DD                    0.8
							6231       B          ANGLE                GRADUAL
							6231       B  HORIZ_DIST_DD                    3.0
							6231       B VERT_HEIGHT_DD                    0.8
							6231       C          ANGLE                GRADUAL
							6231       C  HORIZ_DIST_DD                    3.2
							6231       C VERT_HEIGHT_DD                    0.8
							6231       D          ANGLE                GRADUAL
							6231       D  HORIZ_DIST_DD                    3.0
							6231       D VERT_HEIGHT_DD                    0.8
							6231       E          ANGLE                GRADUAL
							6231       E  HORIZ_DIST_DD                    4.2
							6231       E VERT_HEIGHT_DD                    0.8
							6231       F          ANGLE                GRADUAL
							6231       F  HORIZ_DIST_DD                    5.0
							6231       F VERT_HEIGHT_DD                    0.8
							6231       G          ANGLE                GRADUAL
							6231       G  HORIZ_DIST_DD                    2.9
							6231       G VERT_HEIGHT_DD                    0.8
							6231       H          ANGLE                GRADUAL
							6231       H  HORIZ_DIST_DD                    2.2
							6231       H VERT_HEIGHT_DD                    0.8
							6231       I          ANGLE                GRADUAL
							6231       I  HORIZ_DIST_DD                    4.1
							6231       I VERT_HEIGHT_DD                    0.8
							6231       J          ANGLE                GRADUAL
							6231       J  HORIZ_DIST_DD                    2.4
							6231       J VERT_HEIGHT_DD                    0.8
							6237       A          ANGLE                   FLAT
							6237       B          ANGLE                   FLAT
							6237       C          ANGLE                   FLAT
							6237       D          ANGLE                   FLAT
							6237       E          ANGLE                   FLAT
							6237       F          ANGLE                   FLAT
							6237       G          ANGLE                   FLAT
							6237       H          ANGLE                   FLAT
							6237       I          ANGLE                   FLAT
							6237       J          ANGLE                   FLAT
							6279       A          ANGLE                GRADUAL
							6279       A  HORIZ_DIST_DD                    2.0
							6279       B          ANGLE                GRADUAL
							6279       B  HORIZ_DIST_DD                    2.0
							6279       C          ANGLE                  STEEP
							6279       C  HORIZ_DIST_DD                    1.8
							6279       D          ANGLE                  STEEP
							6279       D  HORIZ_DIST_DD                    1.0
							6279       E          ANGLE                GRADUAL
							6279       F          ANGLE                GRADUAL
							6279       F  HORIZ_DIST_DD                    2.5
							6279       G          ANGLE                  STEEP
							6279       G  HORIZ_DIST_DD                    2.4
							6279       H          ANGLE                  STEEP
							6279       H  HORIZ_DIST_DD                    2.0
							6279       I          ANGLE                  STEEP
							6279       I  HORIZ_DIST_DD                    3.0
							6279       I    VERT_HEIGHT                    4.6
							6279       J  HORIZ_DIST_DD                    2.5
							6321       A          ANGLE NEAR_VERTICAL_UNDERCUT
							6321       B          ANGLE NEAR_VERTICAL_UNDERCUT
							6321       C          ANGLE                GRADUAL
							6321       D          ANGLE                  STEEP
							6321       E          ANGLE                  STEEP
							6321       F          ANGLE                GRADUAL
							6321       G          ANGLE                GRADUAL
							6321       H          ANGLE                GRADUAL
							6321       H  HORIZ_DIST_DD                    1.0
							6321       H    VERT_HEIGHT                    0.8
							6321       H VERT_HEIGHT_DD                    0.1
							6321       I          ANGLE NEAR_VERTICAL_UNDERCUT
							6321       J          ANGLE                  STEEP
							6326       A          ANGLE                GRADUAL
							6326       A  HORIZ_DIST_DD                    4.0
							6326       B          ANGLE                   FLAT
							6326       B  HORIZ_DIST_DD                    6.5
							6326       B VERT_HEIGHT_DD                    0.5
							6326       C          ANGLE                   FLAT
							6326       C  HORIZ_DIST_DD                    4.0
							6326       D          ANGLE                   FLAT
							6326       D     HORIZ_DIST                    2.5
							6326       D  HORIZ_DIST_DD                    1.0
							6326       D    VERT_HEIGHT                    1.4
							6326       D VERT_HEIGHT_DD                    0.2
							6326       E          ANGLE                GRADUAL
							6326       E  HORIZ_DIST_DD                    1.5
							6326       F          ANGLE                GRADUAL
							6326       F  HORIZ_DIST_DD                    1.0
							6326       F VERT_HEIGHT_DD                    0.5
							6326       G          ANGLE                GRADUAL
							6326       G  HORIZ_DIST_DD                    2.0
							6326       H          ANGLE                  STEEP
							6326       H  HORIZ_DIST_DD                    2.0
							6326       H VERT_HEIGHT_DD                    0.3
							6326       I          ANGLE                   FLAT
							6326       I  HORIZ_DIST_DD                    3.0
							6326       I VERT_HEIGHT_DD                    0.3
							6326       J          ANGLE                GRADUAL
							6326       J  HORIZ_DIST_DD                    2.0
							6326       J VERT_HEIGHT_DD                    0.5
							6362       J          ANGLE                GRADUAL
							6362       J  HORIZ_DIST_DD                   20.0
							6362       J VERT_HEIGHT_DD                    2.0
							6374       A          ANGLE                   FLAT
							6374       B          ANGLE                GRADUAL
							6374       C          ANGLE                  STEEP
							6374       D          ANGLE NEAR_VERTICAL_UNDERCUT
							6374       E          ANGLE                GRADUAL
							6374       F          ANGLE NEAR_VERTICAL_UNDERCUT
							6374       G          ANGLE NEAR_VERTICAL_UNDERCUT
							6374       H          ANGLE                  STEEP
							6374       I          ANGLE NEAR_VERTICAL_UNDERCUT
							6374       J          ANGLE NEAR_VERTICAL_UNDERCUT
							6397       A          ANGLE                GRADUAL
							6397       B          ANGLE                   FLAT
							6397       C          ANGLE                GRADUAL
							6397       D          ANGLE                  STEEP
							6397       E          ANGLE                GRADUAL
							6397       F          ANGLE                GRADUAL
							6397       G          ANGLE                GRADUAL
							6397       H          ANGLE                GRADUAL
							6397       I          ANGLE                GRADUAL
							6397       I     HORIZ_DIST                    1.0
							6397       I    VERT_HEIGHT                    0.5
							6397       J          ANGLE                GRADUAL
							6404       A          ANGLE                   FLAT
							6404       B          ANGLE                   FLAT
							6404       C          ANGLE                GRADUAL
							6404       D          ANGLE                   FLAT
							6404       G          ANGLE                   FLAT
							6404       H          ANGLE                GRADUAL
							6404       I          ANGLE                GRADUAL
							6404       J          ANGLE                   FLAT
							6427       A          ANGLE                GRADUAL
							6427       A     HORIZ_DIST                    0.0
							6427       A  HORIZ_DIST_DD                    0.0
							6427       A    VERT_HEIGHT                    0.0
							6427       A VERT_HEIGHT_DD                    0.0
							6427       B          ANGLE                  STEEP
							6427       B     HORIZ_DIST                   12.0
							6427       B  HORIZ_DIST_DD                    0.0
							6427       B    VERT_HEIGHT                    0.3
							6427       B VERT_HEIGHT_DD                    0.0
							6427       C          ANGLE                  STEEP
							6427       C     HORIZ_DIST                    0.0
							6427       C  HORIZ_DIST_DD                    0.0
							6427       C    VERT_HEIGHT                    0.0
							6427       C VERT_HEIGHT_DD                    0.0
							6427       D          ANGLE                GRADUAL
							6427       D     HORIZ_DIST                    0.0
							6427       D  HORIZ_DIST_DD                    0.0
							6427       D    VERT_HEIGHT                    0.0
							6427       D VERT_HEIGHT_DD                    0.0
							6427       E          ANGLE                GRADUAL
							6427       F          ANGLE                GRADUAL
							6427       G          ANGLE                GRADUAL
							6427       H          ANGLE                GRADUAL
							6427       H     HORIZ_DIST                   15.0
							6427       H    VERT_HEIGHT                    0.4
							6427       I          ANGLE NEAR_VERTICAL_UNDERCUT
							6427       I     HORIZ_DIST                    2.0
							6427       I    VERT_HEIGHT                    0.1
							6427       J          ANGLE NEAR_VERTICAL_UNDERCUT
							6427       J     HORIZ_DIST                    4.0
							6427       J    VERT_HEIGHT                    0.2
							6444       A          ANGLE NEAR_VERTICAL_UNDERCUT
							6444       B          ANGLE NEAR_VERTICAL_UNDERCUT
							6444       C          ANGLE                  STEEP
							6444       D          ANGLE                  STEEP
							6444       E          ANGLE                GRADUAL
							6444       F          ANGLE                GRADUAL
							6444       G          ANGLE                  STEEP
							6444       H          ANGLE NEAR_VERTICAL_UNDERCUT
							6444       I          ANGLE NEAR_VERTICAL_UNDERCUT
							6444       J          ANGLE                  STEEP
							6449       A          ANGLE                GRADUAL
							6449       A  HORIZ_DIST_DD                    0.9
							6449       A VERT_HEIGHT_DD                    0.2
							6449       B          ANGLE                GRADUAL
							6449       B  HORIZ_DIST_DD                    1.1
							6449       B VERT_HEIGHT_DD                    0.2
							6449       C          ANGLE                GRADUAL
							6449       C  HORIZ_DIST_DD                    0.2
							6449       C VERT_HEIGHT_DD                    0.2
							6449       D          ANGLE                GRADUAL
							6449       D  HORIZ_DIST_DD                    0.6
							6449       D VERT_HEIGHT_DD                    0.2
							6449       E          ANGLE                GRADUAL
							6449       E  HORIZ_DIST_DD                    0.1
							6449       E VERT_HEIGHT_DD                    0.2
							6449       F          ANGLE                GRADUAL
							6449       F  HORIZ_DIST_DD                    0.1
							6449       F VERT_HEIGHT_DD                    0.3
							6449       G          ANGLE                GRADUAL
							6449       G  HORIZ_DIST_DD                    0.3
							6449       G VERT_HEIGHT_DD                    0.5
							6449       H          ANGLE                GRADUAL
							6449       H  HORIZ_DIST_DD                    0.2
							6449       H VERT_HEIGHT_DD                    0.2
							6449       I          ANGLE                GRADUAL
							6449       I  HORIZ_DIST_DD                    0.3
							6449       I VERT_HEIGHT_DD                    0.2
							6449       J          ANGLE                GRADUAL
							6449       J  HORIZ_DIST_DD                    0.5
							6449       J VERT_HEIGHT_DD                    0.2
							6449       K          ANGLE                GRADUAL
							6449       K  HORIZ_DIST_DD                    0.1
							6449       K VERT_HEIGHT_DD                    0.3
							6449       L          ANGLE                GRADUAL
							6449       L  HORIZ_DIST_DD                    0.3
							6449       L VERT_HEIGHT_DD                    0.2
							6659       A          ANGLE                  STEEP
							6659       A     HORIZ_DIST                    1.8
							6659       A    VERT_HEIGHT                    2.0
							6659       B          ANGLE                  STEEP
							6659       B     HORIZ_DIST                    2.0
							6659       B    VERT_HEIGHT                    2.0
							6659       C          ANGLE                  STEEP
							6659       C     HORIZ_DIST                    2.2
							6659       C    VERT_HEIGHT                    2.0
							6659       D          ANGLE                  STEEP
							6659       D     HORIZ_DIST                    1.8
							6659       D    VERT_HEIGHT                    2.0
							6659       E          ANGLE                   FLAT
							6659       E  HORIZ_DIST_DD                   10.0
							6659       E VERT_HEIGHT_DD                    2.0
							6659       F          ANGLE                   FLAT
							6659       F     HORIZ_DIST                   13.0
							6659       F    VERT_HEIGHT                    2.0
							6659       G          ANGLE                GRADUAL
							6659       G     HORIZ_DIST                   12.0
							6659       G    VERT_HEIGHT                    2.0
							6659       H          ANGLE                   FLAT
							6659       H     HORIZ_DIST                   15.0
							6659       H    VERT_HEIGHT                    2.0
							6659       I          ANGLE                   FLAT
							6659       I     HORIZ_DIST                   10.0
							6659       I    VERT_HEIGHT                    2.0
							6659       J          ANGLE                GRADUAL
							6659       J     HORIZ_DIST                    1.0
							6659       J    VERT_HEIGHT                    2.0
							6735       A          ANGLE NEAR_VERTICAL_UNDERCUT
							6735       A     HORIZ_DIST                  000.0
							6735       A  HORIZ_DIST_DD                  000.0
							6735       A    VERT_HEIGHT                  000.0
							6735       A VERT_HEIGHT_DD                  000.0
							6735       B          ANGLE                GRADUAL
							6735       B     HORIZ_DIST                    0.0
							6735       B  HORIZ_DIST_DD                    0.0
							6735       B    VERT_HEIGHT                    0.0
							6735       B VERT_HEIGHT_DD                    0.0
							6735       C          ANGLE                   FLAT
							6735       C     HORIZ_DIST                  000.0
							6735       C  HORIZ_DIST_DD                    0.0
							6735       C    VERT_HEIGHT                  000.0
							6735       C VERT_HEIGHT_DD                    0.0
							6735       D          ANGLE                   FLAT
							6735       D     HORIZ_DIST                    0.0
							6735       D  HORIZ_DIST_DD                    0.0
							6735       D    VERT_HEIGHT                  000.0
							6735       D VERT_HEIGHT_DD                    0.0
							6735       E          ANGLE                GRADUAL
							6735       E     HORIZ_DIST                    0.0
							6735       E  HORIZ_DIST_DD                    0.0
							6735       E    VERT_HEIGHT                    0.0
							6735       E VERT_HEIGHT_DD                    0.0
							6735       F          ANGLE                  STEEP
							6735       F     HORIZ_DIST                    0.0
							6735       F  HORIZ_DIST_DD                    0.0
							6735       F    VERT_HEIGHT                    0.0
							6735       F VERT_HEIGHT_DD                    0.0
							6735       G          ANGLE                   FLAT
							6735       G     HORIZ_DIST                    0.0
							6735       G  HORIZ_DIST_DD                    0.0
							6735       G    VERT_HEIGHT                     0.
							6735       G VERT_HEIGHT_DD                    0.0
							6735       H          ANGLE                GRADUAL
							6735       H     HORIZ_DIST                    0.0
							6735       H  HORIZ_DIST_DD                    0.0
							6735       H    VERT_HEIGHT                    0.0
							6735       H VERT_HEIGHT_DD                    0.0
							6735       I          ANGLE                GRADUAL
							6735       I     HORIZ_DIST                    0.0
							6735       I  HORIZ_DIST_DD                    0.0
							6735       I    VERT_HEIGHT                    0.0
							6735       I VERT_HEIGHT_DD                    0.0
							6735       J          ANGLE                GRADUAL
							6735       J     HORIZ_DIST                    0.0
							6735       J  HORIZ_DIST_DD                    0.0
							6735       J    VERT_HEIGHT                    0.0
							6735       J VERT_HEIGHT_DD                    0.0
							6160       A       DRAWDOWN                    YES
							6160       B       DRAWDOWN                    YES
							6160       C       DRAWDOWN                    YES
							6160       D       DRAWDOWN                    YES
							6160       E       DRAWDOWN                    YES
							6160       F       DRAWDOWN                    YES
							6160       G       DRAWDOWN                    YES
							6160       H       DRAWDOWN                    YES
							6160       I       DRAWDOWN                    YES
							6160       J       DRAWDOWN                    YES
							6192       A       DRAWDOWN                     NO
							6192       B       DRAWDOWN                     NO
							6192       C       DRAWDOWN                     NO
							6192       D       DRAWDOWN                     NO
							6192       E       DRAWDOWN                     NO
							6192       F       DRAWDOWN                     NO
							6192       G       DRAWDOWN                     NO
							6192       H       DRAWDOWN                     NO
							6192       I       DRAWDOWN                     NO
							6192       J       DRAWDOWN                     NO
							6194       A       DRAWDOWN                     NO
							6194       B       DRAWDOWN                     NO
							6194       C       DRAWDOWN                     NO
							6194       D       DRAWDOWN                     NO
							6194       E       DRAWDOWN                     NO
							6194       F       DRAWDOWN                     NO
							6194       G       DRAWDOWN                     NO
							6194       H       DRAWDOWN                     NO
							6194       I       DRAWDOWN                     NO
							6194       J       DRAWDOWN                     NO
							6227       A       DRAWDOWN                    YES
							6227       B       DRAWDOWN                    YES
							6227       C       DRAWDOWN                     NO
							6227       D       DRAWDOWN                     NO
							6227       E       DRAWDOWN                     NO
							6227       F       DRAWDOWN                     NO
							6227       G       DRAWDOWN                     NO
							6227       H       DRAWDOWN                    YES
							6227       I       DRAWDOWN                    YES
							6227       J       DRAWDOWN                    YES
							6231       A       DRAWDOWN                    YES
							6231       B       DRAWDOWN                    YES
							6231       C       DRAWDOWN                    YES
							6231       D       DRAWDOWN                    YES
							6231       E       DRAWDOWN                    YES
							6231       F       DRAWDOWN                    YES
							6231       G       DRAWDOWN                    YES
							6231       H       DRAWDOWN                    YES
							6231       I       DRAWDOWN                    YES
							6231       J       DRAWDOWN                    YES
							6237       A       DRAWDOWN                     NO
							6237       B       DRAWDOWN                     NO
							6237       C       DRAWDOWN                     NO
							6237       D       DRAWDOWN                     NO
							6237       E       DRAWDOWN                     NO
							6237       F       DRAWDOWN                     NO
							6237       G       DRAWDOWN                     NO
							6237       H       DRAWDOWN                     NO
							6237       I       DRAWDOWN                     NO
							6237       J       DRAWDOWN                     NO
							6279       A       DRAWDOWN                    YES
							6279       B       DRAWDOWN                    YES
							6279       C       DRAWDOWN                    YES
							6279       E       DRAWDOWN                    YES
							6279       F       DRAWDOWN                    YES
							6279       G       DRAWDOWN                    YES
							6279       H       DRAWDOWN                    YES
							6279       I       DRAWDOWN                    YES
							6279       J       DRAWDOWN                    YES
							6321       A       DRAWDOWN                     NO
							6321       B       DRAWDOWN                     NO
							6321       C       DRAWDOWN                     NO
							6321       D       DRAWDOWN                     NO
							6321       E       DRAWDOWN                     NO
							6321       F       DRAWDOWN                     NO
							6321       G       DRAWDOWN                     NO
							6321       H       DRAWDOWN                    YES
							6321       I       DRAWDOWN                     NO
							6321       J       DRAWDOWN                     NO
							6326       A       DRAWDOWN                    YES
							6326       B       DRAWDOWN                    YES
							6326       C       DRAWDOWN                    YES
							6326       D       DRAWDOWN                    YES
							6326       E       DRAWDOWN                    YES
							6326       F       DRAWDOWN                    YES
							6326       G       DRAWDOWN                    YES
							6326       H       DRAWDOWN                    YES
							6326       I       DRAWDOWN                    YES
							6326       J       DRAWDOWN                    YES
							6362       J       DRAWDOWN                    YES
							6374       A       DRAWDOWN                     NO
							6374       B       DRAWDOWN                     NO
							6374       C       DRAWDOWN                     NO
							6374       D       DRAWDOWN                     NO
							6374       E       DRAWDOWN                     NO
							6374       F       DRAWDOWN                     NO
							6374       G       DRAWDOWN                     NO
							6374       H       DRAWDOWN                     NO
							6374       I       DRAWDOWN                     NO
							6374       J       DRAWDOWN                     NO
							6397       A       DRAWDOWN                     NO
							6397       B       DRAWDOWN                     NO
							6397       C       DRAWDOWN                     NO
							6397       D       DRAWDOWN                     NO
							6397       E       DRAWDOWN                     NO
							6397       F       DRAWDOWN                     NO
							6397       G       DRAWDOWN                     NO
							6397       H       DRAWDOWN                     NO
							6397       I       DRAWDOWN                     NO
							6397       J       DRAWDOWN                     NO
							6404       A       DRAWDOWN                     NO
							6404       B       DRAWDOWN                     NO
							6404       C       DRAWDOWN                     NO
							6404       D       DRAWDOWN                     NO
							6404       E       DRAWDOWN                     NO
							6404       F       DRAWDOWN                     NO
							6404       G       DRAWDOWN                     NO
							6404       H       DRAWDOWN                     NO
							6404       I       DRAWDOWN                     NO
							6404       J       DRAWDOWN                     NO
							6427       A       DRAWDOWN                     NO
							6427       B       DRAWDOWN                     NO
							6427       C       DRAWDOWN                     NO
							6427       D       DRAWDOWN                     NO
							6427       E       DRAWDOWN                     NO
							6427       F       DRAWDOWN                     NO
							6427       G       DRAWDOWN                     NO
							6427       H       DRAWDOWN                     NO
							6427       I       DRAWDOWN                     NO
							6427       J       DRAWDOWN                     NO
							6444       A       DRAWDOWN                     NO
							6444       B       DRAWDOWN                     NO
							6444       C       DRAWDOWN                     NO
							6444       D       DRAWDOWN                     NO
							6444       E       DRAWDOWN                     NO
							6444       F       DRAWDOWN                     NO
							6444       G       DRAWDOWN                     NO
							6444       H       DRAWDOWN                     NO
							6444       I       DRAWDOWN                     NO
							6444       J       DRAWDOWN                     NO
							6449       A       DRAWDOWN                    YES
							6449       B       DRAWDOWN                    YES
							6449       C       DRAWDOWN                    YES
							6449       D       DRAWDOWN                    YES
							6449       E       DRAWDOWN                    YES
							6449       F       DRAWDOWN                    YES
							6449       G       DRAWDOWN                    YES
							6449       H       DRAWDOWN                    YES
							6449       I       DRAWDOWN                    YES
							6449       J       DRAWDOWN                    YES
							6449       K       DRAWDOWN                    YES
							6449       L       DRAWDOWN                    YES
							6659       A       DRAWDOWN                     NO
							6659       B       DRAWDOWN                     NO
							6659       C       DRAWDOWN                     NO
							6659       D       DRAWDOWN                     NO
							6659       E       DRAWDOWN                     NO
							6659       F       DRAWDOWN                     NO
							6659       G       DRAWDOWN                     NO
							6659       H       DRAWDOWN                     NO
							6659       I       DRAWDOWN                     NO
							6659       J       DRAWDOWN                     NO
							6735       A       DRAWDOWN                     NO
							6735       B       DRAWDOWN                     NO
							6735       D       DRAWDOWN                     NO
							6735       E       DRAWDOWN                     NO
							6735       F       DRAWDOWN                     NO
							6735       G       DRAWDOWN                     NO
							6735       H       DRAWDOWN                     NO
							6735       I       DRAWDOWN                     NO
							6735       J       DRAWDOWN                     NO
						 ")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaBankFeaturesTest.createExpectedResults2012noDrawdown <- function()
#
{
	tc <- textConnection("  SITE           METRIC                  VALUE
							6160          BFFFLAT                      0
							6160       BFFGRADUAL                    0.1
							6160         BFFSTEEP                    0.9
							6160      BFFVERTICAL                      0
							6160         BFNANGLE                     10
							6160  BFNHORIZDIST_DD                     10
							6160 BFNVERTHEIGHT_DD                     10
							6160         BFOANGLE                  STEEP
							6160  BFXHORIZDIST_DD                   6.64
							6160 BFXVERTHEIGHT_DD                      2
							6192          BFFFLAT                      0
							6192       BFFGRADUAL                      0
							6192         BFFSTEEP                      0
							6192      BFFVERTICAL                      1
							6192         BFNANGLE                      1
							6192         BFOANGLE          NEAR_VERTICAL
							6194          BFFFLAT                      0
							6194       BFFGRADUAL                    0.2
							6194         BFFSTEEP                    0.7
							6194      BFFVERTICAL                    0.1
							6194         BFNANGLE                     10
							6194     BFNHORIZDIST                     10
							6194    BFNVERTHEIGHT                     10
							6194         BFOANGLE                  STEEP
							6194     BFXHORIZDIST                   3.57
							6194    BFXVERTHEIGHT                   1.39
							6227          BFFFLAT                    0.5
							6227       BFFGRADUAL                    0.3
							6227         BFFSTEEP                    0.2
							6227      BFFVERTICAL                      0
							6227         BFNANGLE                     10
							6227  BFNHORIZDIST_DD                      5
							6227 BFNVERTHEIGHT_DD                      5
							6227         BFOANGLE                   FLAT
							6227  BFXHORIZDIST_DD                   1.82
							6227 BFXVERTHEIGHT_DD                   0.48
							6231          BFFFLAT                      0
							6231       BFFGRADUAL                      1
							6231         BFFSTEEP                      0
							6231      BFFVERTICAL                      0
							6231         BFNANGLE                     10
							6231  BFNHORIZDIST_DD                     10
							6231 BFNVERTHEIGHT_DD                     10
							6231         BFOANGLE                GRADUAL
							6231  BFXHORIZDIST_DD                   3.28
							6231 BFXVERTHEIGHT_DD                    0.8
							6237          BFFFLAT                      1
							6237       BFFGRADUAL                      0
							6237         BFFSTEEP                      0
							6237      BFFVERTICAL                      0
							6237         BFNANGLE                     10
							6237         BFOANGLE                   FLAT
							6279          BFFFLAT                      0
							6279       BFFGRADUAL       0.444444444444444	# added digit to avoid truncation error
							6279         BFFSTEEP       0.555555555555556	# added digit to avoid truncation error
							6279      BFFVERTICAL                      0
							6279         BFNANGLE                      9
							6279  BFNHORIZDIST_DD                      9
							6279    BFNVERTHEIGHT                      1
							6279         BFOANGLE                  STEEP
							6279  BFXHORIZDIST_DD       2.13333333333333
							6279    BFXVERTHEIGHT                    4.6
							6321          BFFFLAT                      0
							6321       BFFGRADUAL                    0.4
							6321         BFFSTEEP                    0.3
							6321      BFFVERTICAL                    0.3
							6321         BFNANGLE                     10
							6321  BFNHORIZDIST_DD                      1
							6321    BFNVERTHEIGHT                      1
							6321 BFNVERTHEIGHT_DD                      1
							6321         BFOANGLE                GRADUAL
							6321  BFXHORIZDIST_DD                      1
							6321    BFXVERTHEIGHT                    0.8
							6321 BFXVERTHEIGHT_DD                    0.1
							6326          BFFFLAT                    0.4
							6326       BFFGRADUAL                    0.5
							6326         BFFSTEEP                    0.1
							6326      BFFVERTICAL                      0
							6326         BFNANGLE                     10
							6326     BFNHORIZDIST                      1
							6326  BFNHORIZDIST_DD                     10
							6326    BFNVERTHEIGHT                      1
							6326 BFNVERTHEIGHT_DD                      6
							6326         BFOANGLE                GRADUAL
							6326     BFXHORIZDIST                    2.5
							6326  BFXHORIZDIST_DD                    2.7
							6326    BFXVERTHEIGHT                    1.4
							6326 BFXVERTHEIGHT_DD       0.383333333333333	# added digit to avoid truncation error
							6362          BFFFLAT                      0
							6362       BFFGRADUAL                      1
							6362         BFFSTEEP                      0
							6362      BFFVERTICAL                      0
							6362         BFNANGLE                      1
							6362  BFNHORIZDIST_DD                      1
							6362 BFNVERTHEIGHT_DD                      1
							6362         BFOANGLE                GRADUAL
							6362  BFXHORIZDIST_DD                     20
							6362 BFXVERTHEIGHT_DD                      2
							6374          BFFFLAT                    0.1
							6374       BFFGRADUAL                    0.2
							6374         BFFSTEEP                    0.2
							6374      BFFVERTICAL                    0.5
							6374         BFNANGLE                     10
							6374         BFOANGLE          NEAR_VERTICAL
							6397          BFFFLAT                    0.1
							6397       BFFGRADUAL                    0.8
							6397         BFFSTEEP                    0.1
							6397      BFFVERTICAL                      0
							6397         BFNANGLE                     10
							6397     BFNHORIZDIST                      1
							6397    BFNVERTHEIGHT                      1
							6397         BFOANGLE                GRADUAL
							6397     BFXHORIZDIST                      1
							6397    BFXVERTHEIGHT                    0.5
							6404          BFFFLAT                  0.625
							6404       BFFGRADUAL                  0.375
							6404         BFFSTEEP                      0
							6404      BFFVERTICAL                      0
							6404         BFNANGLE                      8
							6404         BFOANGLE                   FLAT
							6427          BFFFLAT                      0
							6427       BFFGRADUAL                    0.6
							6427         BFFSTEEP                    0.2
							6427      BFFVERTICAL                    0.2
							6427         BFNANGLE                     10
							6427     BFNHORIZDIST                      7
							6427  BFNHORIZDIST_DD                      4
							6427    BFNVERTHEIGHT                      7
							6427 BFNVERTHEIGHT_DD                      4
							6427         BFOANGLE                GRADUAL
							6427     BFXHORIZDIST       4.71428571428571
							6427  BFXHORIZDIST_DD                      0
							6427    BFXVERTHEIGHT       0.142857142857143	# added last digit to avoid truncation error
							6427 BFXVERTHEIGHT_DD                      0
							6444          BFFFLAT                      0
							6444       BFFGRADUAL                    0.2
							6444         BFFSTEEP                    0.4
							6444      BFFVERTICAL                    0.4
							6444         BFNANGLE                     10
							6444         BFOANGLE 'NEAR_VERTICAL, STEEP'	# corrected from SAS value
							6449          BFFFLAT                      0
							6449       BFFGRADUAL                      1
							6449         BFFSTEEP                      0
							6449      BFFVERTICAL                      0
							6449         BFNANGLE                     12
							6449  BFNHORIZDIST_DD                     12
							6449 BFNVERTHEIGHT_DD                     12
							6449         BFOANGLE                GRADUAL
							6449  BFXHORIZDIST_DD       0.391666666666667	# added digit to avoid truncation error
							6449 BFXVERTHEIGHT_DD       0.241666666666667	# added digit to avoid truncation error
							6659          BFFFLAT                    0.4
							6659       BFFGRADUAL                    0.2
							6659         BFFSTEEP                    0.4
							6659      BFFVERTICAL                      0
							6659         BFNANGLE                     10
							6659     BFNHORIZDIST                      9
							6659  BFNHORIZDIST_DD                      1
							6659    BFNVERTHEIGHT                      9
							6659 BFNVERTHEIGHT_DD                      1
							6659         BFOANGLE          'FLAT, STEEP'	# corrected from SAS value
							6659     BFXHORIZDIST       6.53333333333333
							6659  BFXHORIZDIST_DD                     10
							6659    BFXVERTHEIGHT                      2
							6659 BFXVERTHEIGHT_DD                      2
							6735          BFFFLAT                    0.3
							6735       BFFGRADUAL                    0.5
							6735         BFFSTEEP                    0.1
							6735      BFFVERTICAL                    0.1
							6735         BFNANGLE                     10
							6735     BFNHORIZDIST                     10
							6735  BFNHORIZDIST_DD                     10
							6735    BFNVERTHEIGHT                     10
							6735 BFNVERTHEIGHT_DD                     10
							6735         BFOANGLE                GRADUAL
							6735     BFXHORIZDIST                      0
							6735  BFXHORIZDIST_DD                      0
							6735    BFXVERTHEIGHT                      0
							6735 BFXVERTHEIGHT_DD                      0
						 ")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}


nlaBankFeaturesTest.createExpectedResults2012withDrawdown <- function()
#
{
	tc <- textConnection("   SITE     METRIC                    VALUE
					6160          BFFFLAT                      0
					6160       BFFGRADUAL                    0.1
					6160         BFFSTEEP                    0.9
					6160      BFFVERTICAL                      0
					6160         BFNANGLE                     10
					6160  BFNHORIZDIST_DD                     10
					6160 BFNVERTHEIGHT_DD                     10
					6160         BFOANGLE                  STEEP
					6160  BFXHORIZDIST_DD                   6.64
					6160 BFXVERTHEIGHT_DD                      2
					6192          BFFFLAT                      0
					6192       BFFGRADUAL                      0
					6192         BFFSTEEP                      0
					6192      BFFVERTICAL                      1
					6192         BFNANGLE                      1
					6192         BFOANGLE          NEAR_VERTICAL
					6194          BFFFLAT                      0
					6194       BFFGRADUAL                    0.2
					6194         BFFSTEEP                    0.7
					6194      BFFVERTICAL                    0.1
					6194         BFNANGLE                     10
					6194     BFNHORIZDIST                     10
					6194    BFNVERTHEIGHT                     10
					6194         BFOANGLE                  STEEP
					6194     BFXHORIZDIST                   3.57
					6194    BFXVERTHEIGHT                   1.39
					6227          BFFFLAT                    0.5
					6227       BFFGRADUAL                    0.3
					6227         BFFSTEEP                    0.2
					6227      BFFVERTICAL                      0
					6227         BFNANGLE                     10
					6227  BFNHORIZDIST_DD                     10 #5 without DRAWDOWN assumptions
					6227 BFNVERTHEIGHT_DD                     10 #5 without DRAWDOWN assumptions
					6227         BFOANGLE                   FLAT
					6227  BFXHORIZDIST_DD                   0.91 #1.82 without DRAWDOWN assumptions
					6227 BFXVERTHEIGHT_DD                   0.24 #0.48 without DRAWDOWN assumptions
					6231          BFFFLAT                      0
					6231       BFFGRADUAL                      1
					6231         BFFSTEEP                      0
					6231      BFFVERTICAL                      0
					6231         BFNANGLE                     10
					6231  BFNHORIZDIST_DD                     10
					6231 BFNVERTHEIGHT_DD                     10
					6231         BFOANGLE                GRADUAL
					6231  BFXHORIZDIST_DD                   3.28
					6231 BFXVERTHEIGHT_DD                    0.8
					6237          BFFFLAT                      1
					6237       BFFGRADUAL                      0
					6237         BFFSTEEP                      0
					6237      BFFVERTICAL                      0
					6237         BFNANGLE                     10
					6237         BFOANGLE                   FLAT
					6279          BFFFLAT                      0
					6279       BFFGRADUAL       0.444444444444444	# added digit to avoid truncation error
					6279         BFFSTEEP       0.555555555555556	# added digit to avoid truncation error
					6279      BFFVERTICAL                      0
					6279         BFNANGLE                      9
					6279  BFNHORIZDIST_DD                      9
					6279    BFNVERTHEIGHT                      1
					6279         BFOANGLE                  STEEP
					6279  BFXHORIZDIST_DD       2.13333333333333
					6279    BFXVERTHEIGHT                    4.6
					6321          BFFFLAT                      0
					6321       BFFGRADUAL                    0.4
					6321         BFFSTEEP                    0.3
					6321      BFFVERTICAL                    0.3
					6321         BFNANGLE                     10
					6321  BFNHORIZDIST_DD                     10 #1 without DRAWDOWN assumptions
					6321    BFNVERTHEIGHT                      1
					6321 BFNVERTHEIGHT_DD                     10 #1 without DRAWDOWN assumptions
					6321         BFOANGLE                GRADUAL
					6321  BFXHORIZDIST_DD                    0.1 #1 without DRAWDOWN assumptions
					6321    BFXVERTHEIGHT                    0.8
					6321 BFXVERTHEIGHT_DD                   0.01 #0.1 without DRAWDOWN assumptions
					6326          BFFFLAT                    0.4
					6326       BFFGRADUAL                    0.5
					6326         BFFSTEEP                    0.1
					6326      BFFVERTICAL                      0
					6326         BFNANGLE                     10
					6326     BFNHORIZDIST                      1
					6326  BFNHORIZDIST_DD                     10
					6326    BFNVERTHEIGHT                      1
					6326 BFNVERTHEIGHT_DD                      6
					6326         BFOANGLE                GRADUAL
					6326     BFXHORIZDIST                    2.5
					6326  BFXHORIZDIST_DD                    2.7
					6326    BFXVERTHEIGHT                    1.4
					6326 BFXVERTHEIGHT_DD       0.383333333333333	# added digit to avoid truncation error
					6362          BFFFLAT                      0
					6362       BFFGRADUAL                      1
					6362         BFFSTEEP                      0
					6362      BFFVERTICAL                      0
					6362         BFNANGLE                      1
					6362  BFNHORIZDIST_DD                      1
					6362 BFNVERTHEIGHT_DD                      1
					6362         BFOANGLE                GRADUAL
					6362  BFXHORIZDIST_DD                     20
					6362 BFXVERTHEIGHT_DD                      2
					6374          BFFFLAT                    0.1
					6374       BFFGRADUAL                    0.2
					6374         BFFSTEEP                    0.2
					6374      BFFVERTICAL                    0.5
					6374         BFNANGLE                     10
					6374         BFOANGLE          NEAR_VERTICAL
					6397          BFFFLAT                    0.1
					6397       BFFGRADUAL                    0.8
					6397         BFFSTEEP                    0.1
					6397      BFFVERTICAL                      0
					6397         BFNANGLE                     10
					6397     BFNHORIZDIST                      1
					6397    BFNVERTHEIGHT                      1
					6397         BFOANGLE                GRADUAL
					6397     BFXHORIZDIST                      1
					6397    BFXVERTHEIGHT                    0.5
					6404          BFFFLAT                  0.625
					6404       BFFGRADUAL                  0.375
					6404         BFFSTEEP                      0
					6404      BFFVERTICAL                      0
					6404         BFNANGLE                      8
					6404         BFOANGLE                   FLAT
					6427          BFFFLAT                      0
					6427       BFFGRADUAL                    0.6
					6427         BFFSTEEP                    0.2
					6427      BFFVERTICAL                    0.2
					6427         BFNANGLE                     10
					6427     BFNHORIZDIST                      7
					6427  BFNHORIZDIST_DD                     10 #4 without DRAWDOWN assumptions
					6427    BFNVERTHEIGHT                      7
					6427 BFNVERTHEIGHT_DD                     10 #4 without DRAWDOWN assumptions
					6427         BFOANGLE                GRADUAL
					6427     BFXHORIZDIST       4.71428571428571
					6427  BFXHORIZDIST_DD                      0
					6427    BFXVERTHEIGHT       0.142857142857143	# added last digit to avoid truncation error
					6427 BFXVERTHEIGHT_DD                      0
					6444          BFFFLAT                      0
					6444       BFFGRADUAL                    0.2
					6444         BFFSTEEP                    0.4
					6444      BFFVERTICAL                    0.4
					6444         BFNANGLE                     10
					6444         BFOANGLE 'NEAR_VERTICAL, STEEP'	# corrected from SAS value
					6449          BFFFLAT                      0
					6449       BFFGRADUAL                      1
					6449         BFFSTEEP                      0
					6449      BFFVERTICAL                      0
					6449         BFNANGLE                     12
					6449  BFNHORIZDIST_DD                     12
					6449 BFNVERTHEIGHT_DD                     12
					6449         BFOANGLE                GRADUAL
					6449  BFXHORIZDIST_DD       0.391666666666667	# added digit to avoid truncation error
					6449 BFXVERTHEIGHT_DD       0.241666666666667	# added digit to avoid truncation error
					6659          BFFFLAT                    0.4
					6659       BFFGRADUAL                    0.2
					6659         BFFSTEEP                    0.4
					6659      BFFVERTICAL                      0
					6659         BFNANGLE                     10
					6659     BFNHORIZDIST                      9
					6659  BFNHORIZDIST_DD                     10 #1 without DRAWDOWN assumptions
					6659    BFNVERTHEIGHT                      9
					6659 BFNVERTHEIGHT_DD                     10 #1 without DRAWDOWN assumptions
					6659         BFOANGLE          'FLAT, STEEP'	# corrected from SAS value
					6659     BFXHORIZDIST       6.53333333333333
					6659  BFXHORIZDIST_DD                      1 #10 without DRAWDOWN assumptions
					6659    BFXVERTHEIGHT                      2
					6659 BFXVERTHEIGHT_DD                    0.2 #2 without DRAWDOWN assumptions
					6735          BFFFLAT                    0.3
					6735       BFFGRADUAL                    0.5
					6735         BFFSTEEP                    0.1
					6735      BFFVERTICAL                    0.1
					6735         BFNANGLE                     10
					6735     BFNHORIZDIST                     10
					6735  BFNHORIZDIST_DD                     10
					6735    BFNVERTHEIGHT                     10
					6735 BFNVERTHEIGHT_DD                     10
					6735         BFOANGLE                GRADUAL
					6735     BFXHORIZDIST                      0
					6735  BFXHORIZDIST_DD                      0
					6735    BFXVERTHEIGHT                      0
					6735 BFXVERTHEIGHT_DD                      0
					6192  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6192 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6192  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6192 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6194  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6194 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6194  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6194 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6237  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6237 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6237  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6237 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6279 BFNVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6279 BFXVERTHEIGHT_DD					  NA # absent without DRAWDOWN assumptions
					6374  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6374 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6374  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6374 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6397  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6397 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6397  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6397 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6404  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6404 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6404  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6404 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6427  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6427 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6427  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6427 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					6444  BFNHORIZDIST_DD                     10 # absent without DRAWDOWN assumptions
					6444 BFNVERTHEIGHT_DD                     10 # absent without DRAWDOWN assumptions
				    6444  BFXHORIZDIST_DD					   0 # absent without DRAWDOWN assumptions
				    6444 BFXVERTHEIGHT_DD					   0 # absent without DRAWDOWN assumptions
					")
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	close(tc)
	
	return(fake)		
}

# end of file
