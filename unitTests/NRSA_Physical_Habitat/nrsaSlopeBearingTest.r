# nrsaSlopeBearingTest.r
# RUnit tests
#
# 10/29/15 cws Updated to match changes in nrsaSlopeBearing: renamed columns
#          to SITE and VALUE; changed arguments from 0809 database-related
#          to generalized arguments.
#  1/07/16 cws Updated to match additional changes in nrsaSlopeBearing arguments.
#          Also now expecting values from gisSinuosity and gisSlope for sites
#          that do not have field data (rows in b* or w* arguments).
#  1/21/16 cws Changed input data to use LINE=0 instead of LINE=999 to indicate
#          at-transect location in boatable reaches.
#  1/22/16 cws Removed old commented out code and prettyprinted test code a bit.
#  3/28/17 cws Modified to add data for wadeable site "2003 WWYP99-0659 1 no subsightings"
#          which is based on "2003 WWYP99-0659 1" but modified with hand calculations
#          to not contain subsightings but result in the same final metrics.
#  5/19/17 cws Rewrote code creating thalweg and channel geometry testdata, as
#          well as nrsaSlopeBearingTest.makeExpectedResultsWithGIS,  
#          nrsaSlopeBearingTest.makeGisCalcs, nrsaSlopeBearingTest.makeProtocols 
#          and nrsaSlopeBearingTest.makeExpectedResults in preparation for 
#          expanding unit test (Changes to test data resulted in loss of zeros 
#          in text values, but does not change the test calculation results). 
#  5/23/17 cws Adding test cases using simplified arguments rather than munged 
#          data typical of actual use. This will make what each case tests much
#          clearer. Or so I hope.

nrsaSlopeBearingTest <- function()
{
    nrsaSlopeBearingTest.simplifiedArguments()
    nrsaSlopeBearingTest.typicalDataMungedArguments() # early, 'classical' tests
}


nrsaSlopeBearingTest.simplifiedArguments <- function()
# Tests using simple, artificial data as arguments
{
    # nrsaSlopeBearingTest.simplifiedArguments.nullArgs()
    nrsaSlopeBearingTest.simplifiedArguments.crazyBearings()
    nrsaSlopeBearingTest.simplifiedArguments.missingUNITS()
    nrsaSlopeBearingTest.simplifiedArguments.missingMETHOD()
    # nrsaSlopeBearingTest.simplifiedArguments.missingUNITSandMETHOD()
    nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine0_A()
    nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine0_B()
    nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine012()
    nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithPercentsOnLine0()
    nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithPercentsOnLine012()
}

nrsaSlopeBearingTest.simplifiedArguments.nullArgs <- function()                 # Requires modification of nrsaSlopeBearing() to handle lack of data.
{
    expected <- data.frame()
    actual <- nrsaSlopeBearing(bBearing = NULL
                              ,bDistance = NULL
                              ,bSlope = NULL
                              ,wBearing = NULL
                              ,wTransectSpacing = NULL
                              ,wProportion = NULL
                              ,wSlope = NULL
                              ,gisSinuosity = NULL
                              ,gisSlope = NULL
                              )
    checkEquals(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), "Incorrect handling of missing units")
}

nrsaSlopeBearingTest.simplifiedArguments.crazyBearings <- function()   
# NOTE: This test points out the absurdity of the xbearing definition in some 
# unrealistic cases, which suggests that the calculation should be redefined in
# some way.
{
    base <- data.frame(SITE=1L, TRANSECT=c('A','B','C','D'), LINE=0, stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=0, xslope_field=0
                                ,pctClinometer=100, vslope=0, nslp=4
                                ,transpc=1, xbearing=mean(c(90,180,270,360))
                                ,sinu=23106851772068704 # This would be infinite but for floating point errors. It's close enough.
                                )
                     ,data.frame(SITE=2, xslope=0, xslope_field=0
                                ,pctClinometer=100, vslope=0, nslp=4
                                ,transpc=1, xbearing=mean(c(359,180,180,1))
                                ,sinu=  4/sqrt(0^2 + (cos(1*2*pi/360) + -1 + -1 + cos(359*2*pi/360))^2) # 13131.558738459949
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))    
    actual <- nrsaSlopeBearing(bBearing = base %>% mutate(VALUE=c(90,180,270,360))
                              ,bDistance = base %>% mutate(VALUE=1)
                              ,bSlope = base %>% mutate(VALUE=0, METHOD='CL', UNITS='PERCENT')
                              ,wBearing = base %>% mutate(SITE=2, VALUE=c(359,180,180,1))
                              ,wTransectSpacing = base %>% mutate(SITE=2, VALUE=1)
                              ,wProportion = base %>% mutate(SITE=2, VALUE=100)
                              ,wSlope = base %>% mutate(SITE=2, VALUE=0, METHOD='CL', UNITS='PERCENT')
                              ,gisSinuosity = NULL
                              ,gisSlope = NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-10)  # needed for sinu value at site 2.
    checkEquals(NULL, dd, "Incorrect handling of crazy bearings")
}

nrsaSlopeBearingTest.simplifiedArguments.missingUNITS <- function()
{
    base <- data.frame(SITE=1L, TRANSECT=c('A','B','C','D'), LINE=0, stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=0.15, xslope_field=0.15
                                ,pctClinometer=0, vslope=1/(10*sqrt(2)), nslp=2
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=1.5, xslope_field=1.5
                                ,pctClinometer=100, vslope=1/sqrt(2), nslp=2
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(VALUE=c(100,150,200,150))
                              ,bDistance =        base %>% mutate(VALUE=10)
                              ,bSlope =           base %>% mutate(VALUE=c(1,2,3,4), METHOD='TR', UNITS=c('CM','CM','',NA))
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=c(100,150,200,150))
                              ,wTransectSpacing = base %>% mutate(SITE=2L, VALUE=10)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=100)
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=c(1,2,3,4), METHOD='CL', UNITS=c('PERCENT','PERCENT','',NA))
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    checkEquals(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), "Incorrect handling of missing units")
}

nrsaSlopeBearingTest.simplifiedArguments.missingMETHOD <- function()
{
    base <- data.frame(SITE=1L, TRANSECT=c('A','B','C','D'), LINE=0, stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=0.25, xslope_field=0.25
                                ,pctClinometer=0, vslope=sqrt(sum((c(.1,.2,.3,.4)-.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=2.5, xslope_field=2.5
                                ,pctClinometer=100*2/3, vslope=sqrt(sum((c(1,2,3,4)-2.5)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(VALUE=c(100,150,200,150))
                              ,bDistance =        base %>% mutate(VALUE=10)
                              ,bSlope =           base %>% mutate(VALUE=c(1,2,3,4), METHOD=c('TR','TR',NA,''), UNITS='CM')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=c(100,150,200,150))
                              ,wTransectSpacing = base %>% mutate(SITE=2L, VALUE=10)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=100)
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=c(1,2,3,4), METHOD=c('CL','CL',NA,''), UNITS='PERCENT')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    checkEquals(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), "Incorrect handling of missing methods")
}

nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine0_A <- function()
# Tests simplest case in which all transects have identical numbers of backsightings
{
    base <- data.frame(TRANSECT=rep(c('A','B','C','D'), each=3), LINE=rep(0:2, times=4), stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=0, vslope=sqrt(sum((c(.1,.2,.3,.4)-.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=100, vslope=sqrt(sum((c(.1,.2,.3,.4)-0.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,bDistance =        base %>% mutate(SITE=1L, VALUE=ifelse(LINE==0, 3.3, ifelse(LINE==1, 3.3, 3.4)))
                              ,bSlope =           base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', c(1,NA,NA), ifelse(TRANSECT=='B', c(2,NA,NA), ifelse(TRANSECT=='C', c(3,NA,NA),c(4,NA,NA)))), METHOD='TR', UNITS='CM')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,wTransectSpacing = base %>% subset(LINE==0) %>% mutate(SITE=2L, VALUE=10, LINE=NULL)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=ifelse(LINE==0, 33, ifelse(LINE==1, 33, 34)))
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', c(1,NA,NA), ifelse(TRANSECT=='B', c(2,NA,NA), ifelse(TRANSECT=='C', c(3,NA,NA),c(4,NA,NA)))), METHOD='CL', UNITS='CM')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-14)
    checkEquals(NULL, dd, "Incorrect handling of backsighted elevation slopes only provided at LINE 0")
}

nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine0_B <- function()
# Tests case in which transects have differing numbers of backsightings
{
    #base <- data.frame(TRANSECT=rep(c('A','B','C','D'), each=3), LINE=rep(0:2, times=4), stringsAsFactors=FALSE)
    base <- data.frame(TRANSECT=c('A','A','B','B','C','C','C','D','D','D'), LINE=c(0:1,0:1,0:2,0:2), stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=0, vslope=sqrt(sum((c(.1,.2,.3,.4)-.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=100, vslope=sqrt(sum((c(.1,.2,.3,.4)-0.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', c(100,''), ifelse(TRANSECT=='B', c(150,NA), ifelse(TRANSECT=='C', 200, 150))))
                              ,bDistance =        base %>% mutate(SITE=1L, VALUE=c(10,'', 10,NA, 3.3,3.3,3.4, 5,5,0))
                              ,bSlope =           base %>% mutate(SITE=1L, VALUE=c(1,'', 2,NA, 3,NA,NA, 4,NA,''), METHOD='TR', UNITS='CM')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', c(100,''), ifelse(TRANSECT=='B', c(150,NA), ifelse(TRANSECT=='C', 200,150))))
                              ,wTransectSpacing = base %>% subset(LINE==0) %>% mutate(SITE=2L, VALUE=10, LINE=NULL)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=c(100,'', 100,NA, 33,33,34, 50,50,0))
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=c(1,'', 2,NA, 3,NA,NA, 4,NA,''), METHOD='CL', UNITS='CM')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-14)
    checkEquals(NULL, dd, "Incorrect handling of backsighted elevation slopes only provided at LINE 0, transects having differing numbers of suplemental backsightings")
}

nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithElevationsOnLine012 <- function()
{
    base <- data.frame(TRANSECT=rep(c('A','B','C','D'), each=3), LINE=rep(0:2, times=4), stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=0, vslope=sqrt(sum((c(.1,.2,.3,.4)-.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=100*(1+2+3+4)/(40*100), xslope_field=100*(1+2+3+4)/(40*100)
                                ,pctClinometer=100, vslope=sqrt(sum((c(.1,.2,.3,.4)-0.25)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,bDistance =        base %>% mutate(SITE=1L, VALUE=ifelse(LINE==0, 3.3, ifelse(LINE==1, 3.3, 3.4)))
                              ,bSlope =           base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', c(1*.33,1*.33,1*.34), ifelse(TRANSECT=='B', c(2*.33,2*.33,2*.34), ifelse(TRANSECT=='C', c(3*.33,3*.33,3*.34),c(4*.33,4*.33,4*.34)))), METHOD='TR', UNITS='CM')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,wTransectSpacing = base %>% subset(LINE==0) %>% mutate(SITE=2L, VALUE=10, LINE=NULL)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=ifelse(LINE==0, 33, ifelse(LINE==1, 33, 34)))
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', c(1*.33,1*.33,1*.34), ifelse(TRANSECT=='B', c(2*.33,2*.33,2*.34), ifelse(TRANSECT=='C', c(3*.33,3*.33,3*.34),c(4*.33,4*.33,4*.34)))), METHOD='CL', UNITS='CM')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-14)
    checkEquals(NULL, dd, "Incorrect handling of backsighted elevation slopes only provided at LINEs 0,1,2")
}

nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithPercentsOnLine0 <- function()
# Note: Currently (25 May 2017) only wadeable slopes recorded in CM are allowed
# to be recorded only at LINE 0, with the phab code filling in the elevation on 
# other LINEs based on PROPORTION values. This test tests the case when PERCENT
# slopes are recorded only on LINE 0.
#
{
    base <- data.frame(TRANSECT=rep(c('A','B','C','D'), each=3), LINE=rep(0:2, times=4), stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=(1+2+3+4)/4, xslope_field=(1+2+3+4)/4                                 # values when PERCENTs are filled in for absent subsightings
                                ,pctClinometer=0, vslope=sqrt(sum((c(1,2,3,4)-2.5)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=(1+2+3+4)/4, xslope_field=(1+2+3+4)/4
                                ,pctClinometer=100, vslope=sqrt(sum((c(1,2,3,4)-2.5)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    expected <- expected %>%                                                                                         # values when PERCENTs are NOT filled in for absent subsightings
                mutate(VALUE = ifelse(METRIC %in% c('xslope','xslope_field'), VALUE*0.33
                              ,ifelse(METRIC == 'vslope', sqrt(sum((c(1,2,3,4)*0.33-2.5*0.33)^2)/3)
                                     ,VALUE
                               ))
                      )
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,bDistance =        base %>% mutate(SITE=1L, VALUE=ifelse(LINE==0, 3.3, ifelse(LINE==1, 3.3, 3.4)))
                              ,bSlope =           base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', c(1,NA,NA), ifelse(TRANSECT=='B', c(2,NA,NA), ifelse(TRANSECT=='C', c(3,NA,NA),c(4,NA,NA)))), METHOD='TR', UNITS='PERCENT')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,wTransectSpacing = base %>% subset(LINE==0) %>% mutate(SITE=2L, VALUE=10, LINE=NULL)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=ifelse(LINE==0, 33, ifelse(LINE==1, 33, 34)))
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', c(1,NA,NA), ifelse(TRANSECT=='B', c(2,NA,NA), ifelse(TRANSECT=='C', c(3,NA,NA),c(4,NA,NA)))), METHOD='CL', UNITS='PERCENT')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-14)
    checkEquals(NULL, dd, "Incorrect handling of backsighted PERCENT slopes only provided at LINE 0")
}

nrsaSlopeBearingTest.simplifiedArguments.backsightingsWithPercentsOnLine012 <- function()
{
    base <- data.frame(TRANSECT=rep(c('A','B','C','D'), each=3), LINE=rep(0:2, times=4), stringsAsFactors=FALSE)
    expected <- rbind(data.frame(SITE=1, xslope=(1+2+3+4)/4, xslope_field=(1+2+3+4)/4
                                ,pctClinometer=0, vslope=sqrt(sum((c(1,2,3,4)-2.5)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ,data.frame(SITE=2, xslope=(1+2+3+4)/4, xslope_field=(1+2+3+4)/4
                                ,pctClinometer=100, vslope=sqrt(sum((c(1,2,3,4)-2.5)^2)/3), nslp=4
                                ,transpc=10, xbearing=150
                                ,sinu= 40/sqrt(sum(c(10*cos(c(10,60,60)*(2*pi/360)), -10*sin(20*(2*pi/360))))^2 + 
                                               sum(c(10*sin(c(10,60,60)*(2*pi/360)), 10*cos(20*(2*pi/360))))^2
                                              )     # 1.217442832054
                                )
                     ) %>%
                melt('SITE', variable.name='METRIC', value.name='VALUE') %>%
                mutate(METRIC=as.character(METRIC))
    actual <- nrsaSlopeBearing(bBearing =         base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,bDistance =        base %>% mutate(SITE=1L, VALUE=ifelse(LINE==0, 3.3, ifelse(LINE==1, 3.3, 3.4)))
                              ,bSlope =           base %>% mutate(SITE=1L, VALUE=ifelse(TRANSECT=='A', 1, ifelse(TRANSECT=='B', 2, ifelse(TRANSECT=='C', 3, 4))), METHOD='TR', UNITS='PERCENT')
                              ,wBearing =         base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', 100, ifelse(TRANSECT=='B', 150, ifelse(TRANSECT=='C', 200,150))))
                              ,wTransectSpacing = base %>% subset(LINE==0) %>% mutate(SITE=2L, VALUE=10, LINE=NULL)
                              ,wProportion =      base %>% mutate(SITE=2L, VALUE=ifelse(LINE==0, 33, ifelse(LINE==1, 33, 34)))
                              ,wSlope =           base %>% mutate(SITE=2L, VALUE=ifelse(TRANSECT=='A', 1, ifelse(TRANSECT=='B', 2, ifelse(TRANSECT=='C', 3, 4))), METHOD='CL', UNITS='PERCENT')
                              ,gisSinuosity =     NULL
                              ,gisSlope =         NULL
                              )
    dd <- dfCompare(expected, actual %>% mutate(VALUE=as.numeric(VALUE)), c('SITE','METRIC'), zeroFudge=1e-14)
    checkEquals(NULL, dd, "Incorrect handling of backsighted PERCENT slopes provided at LINE 0,1,2")
}


nrsaSlopeBearingTest.typicalDataMungedArguments <- function()
# Unit test for nrsaSlopeBearing. Test data taken from WEMAP data on 2-Feb-2010,
# and is then transformed into the expected organization for NRSA data.
# 2000 WAZP99-0505 1                 Stream with no supplemental readings
# 2000 WAZP99-0569 1                 Stream with many supplemental readings
# 2003 WWYP99-0659 1                 Stream with slopes in cm
# 2003 WWYP99-0659 1 no subsightings Stream with slopes in cm with subsights that have been collapsed to have same results.
# 2003 WWYP99-0659 1 missing CM subsightings   Stream with slopes in cm
# 2003 WWYP99-0659 1 slope unit NONE Stream with slopes in cm, but UNITS=NONE
# 2000 WAZP99-0569 1 no incremnt     Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes       Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes   Stream with insufficient slope information
# 2014 CARO-1043 1                   Stream with mixed slope units, no subsightings
# 2015 AKBB-018 1                    Stream with pct slope units, 2 blank units
#
# 2000 WIDP99-0556 1                 River with some supplemental readings
# 2000 WIDP99-0556 1 with slopes in cm River with some supplemental readings with slopes expressed as elevations that have same results.
# 2000 WIDP99-0556 1 with NA slopes  River with all slope values missing
# 2000 WIDP99-0556 1 with absent slopes  River with no slope values at all
# 2000 WIDP99-0556 1 slope unit NONE River with some supplemental readings and
#                                      slope units are NONE
# 2000 WSDP99-0531 1                 River with lots of supplemental readings
#
# The expected metrics are obtained for these reaches as well, and modified as
# follows:
#   '2000 WAZP99-0569 1' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 only 2 slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no incremnt' xbearing changed from 42.435 to NA and
#       sinu changed from 1.1251 to NA since lack of distance between stations
#       means the distances on which these metrics rely on are unavailable.
#   '2003 WWYP99-0659 1' xslope changed from 0.86733 to 0.588307961 and vslope
#       changed from 0.73286 to 0.371371587 to account for error in previous
#       calculations of slopes at transects with supplemental slopes measured as
#       elevation change.  These should be calculated as elev/(transpc * prop)
#       but instead were calculated as (elev/(transpc * prop))*prop, which were
#       then multiplied by the proportion again and summed to determine the
#       slope of the transect.  This lead to diminished mean slopes at each
#       transect.
#
{
    # Create fake input data.  Thalweg data initially has 1 incremnt at
    # each station, instead of once per site as expected by the calculation code,
    # for testing purposes.
    fakeThal_IncremntsAtEachSITE <- nrsaSlopeBearingTest.makeThalweg()
    fakeThal <- subset(fakeThal_IncremntsAtEachSITE
                      ,!(PARAMETER=='INCREMNT' & TRANSECT !='A' & STATION !=0)
                      )

    fakeChanGeom <- nrsaSlopeBearingTest.makeChannelGeometry()

    fakeGisCalcs <- nrsaSlopeBearingTest.makeGisCalcs()
  
    fakeProtocol <- nrsaSlopeBearingTest.makeProtocols()

    # Create expected results.
    expected <- nrsaSlopeBearingTest.makeExpectedResults()
  
                           
    # Test calculations with both wadeable and boatable data AND with extra incremnt values.
    results <- nrsaSlopeBearing(bBearing = fakeChanGeom %>% 
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>%
                                           mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bDistance = fakeChanGeom %>% 
                                            subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE') %>%
                                            mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bSlope = fakeChanGeom %>% 
                                         subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE') %>%
                                         mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,wBearing = fakeChanGeom %>%
                                           subset(SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                        ,ifelse(PARAMETER == 'BEARING2', 1
                                                        ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                         )))
                                                  ,PARAMETER = NULL
                                                  )                                         
                               ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                         mutate(VALUE=as.numeric(VALUE))
                                                        ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                        ,'SITE'
                                                        ) %>%
                                                   mutate(VALUE = VALUE * nSta
                                                         ,nSta = NULL
                                                         )
                               ,wProportion = fakeChanGeom %>%
                                              subset(SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                         mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                      ,ifelse(PARAMETER == 'PROP2', 1
                                                      ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                       )))
                                                ,PARAMETER = NULL
                                                )
                               ,wSlope = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                         mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                      ,ifelse(PARAMETER == 'SLOPE2', 1
                                                      ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                      )))
                                                ,PARAMETER = NULL
                                                ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                                )                                         
                               ,gisSinuosity = NULL
                               ,gisSlope = NULL
                               )
    results <- results[order(results$SITE, results$METRIC),]
    results$VALUE <- as.numeric(results$VALUE)

    # Compare expected and actual results
    expected <- expected[order(expected$SITE, expected$METRIC),]
    expected$VALUE <- as.numeric(expected$VALUE)
    errs <- dfCompare(expected, results, c('SITE','METRIC'), zeroFudge=1e-4)
#    return(errs)
    checkEquals(NULL, errs
               ,"Error: Slope and bearing metrics are broken with both protocols and extra incremnt values in thalweg table"
               )

             
    # Test calculations with both wadeable and boatable data AND GIS values
    # Fold GIS results into expected values, removing unexpected site and overwriting
    # sinuosity values based on field values with those based on map calculations.
    expectedWithGIS <- nrsaSlopeBearingTest.makeExpectedResultsWithGIS()
  
    results <- nrsaSlopeBearing(bBearing = fakeChanGeom %>%
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>%
                                           mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bDistance = fakeChanGeom %>%
                                            subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE') %>%
                                            mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bSlope = fakeChanGeom %>%
                                         subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE') %>%
                                         mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,wBearing = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                        ,ifelse(PARAMETER == 'BEARING2', 1
                                                        ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                         )))
                                                 ,PARAMETER = NULL
                                                 )                                    
                               ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                         mutate(VALUE=as.numeric(VALUE))
                                                        ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                        ,'SITE'
                                                        ) %>%
                                                   mutate(VALUE = VALUE * nSta
                                                         ,nSta = NULL
                                                         )
                               ,wProportion = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                              mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                           ,ifelse(PARAMETER == 'PROP2', 1
                                                           ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                            )))
                                                    ,PARAMETER = NULL
                                                    )                                         
                               ,wSlope = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                         mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                      ,ifelse(PARAMETER == 'SLOPE2', 1
                                                      ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                      )))
                                               ,PARAMETER = NULL
                                               ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                               )                                         
                               ,gisSlope = subset(fakeGisCalcs, METRIC=='xslope')
                               ,gisSinuosity = subset(fakeGisCalcs, METRIC=='sinu')
                               )
    results <- results[order(results$SITE, results$METRIC),]
    results$VALUE <- as.numeric(results$VALUE)

    expectedWithGIS <- expectedWithGIS[order(expectedWithGIS$SITE, expectedWithGIS$METRIC),]
    expectedWithGIS$VALUE <- as.numeric(expectedWithGIS$VALUE)
    errs <- dfCompare(expectedWithGIS, results, c('SITE','METRIC'), zeroFudge=1e-4)
#    return(errs)
    checkEquals(NULL, errs
               ,"Error: Slope and bearing metrics are broken with GIS calculations, both protocols"
               )


    # Test calculations with both wadeable and boatable data AND with extra incremnt 
    # values AND GIS values
    results <- nrsaSlopeBearing(bBearing = fakeChanGeom %>%
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>%
                                           mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bDistance = fakeChanGeom %>%
                                            subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE') %>%
                                            mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bSlope = fakeChanGeom %>%
                                         subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE') %>%
                                         mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,wBearing = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                        ,ifelse(PARAMETER == 'BEARING2', 1
                                                        ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                         )))
                                                  ,PARAMETER = NULL
                                                  )                                         
                               ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                         mutate(VALUE=as.numeric(VALUE))
                                                        ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                        ,'SITE'
                                                        ) %>%
                                                   mutate(VALUE = VALUE * nSta
                                                         ,nSta = NULL
                                                         )
                               ,wProportion = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                              mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                           ,ifelse(PARAMETER == 'PROP2', 1
                                                           ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                            )))
                                                    ,PARAMETER = NULL
                                                    )                                         
                               ,wSlope = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                         mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                      ,ifelse(PARAMETER == 'SLOPE2', 1
                                                      ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                       )))
                                                ,PARAMETER = NULL
                                                ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                                )                                         
                               ,gisSinuosity = subset(fakeGisCalcs, METRIC=='sinu')
                               ,gisSlope = subset(fakeGisCalcs, METRIC=='xslope')
                               )
    results <- results[order(results$SITE, results$METRIC),]
    results$VALUE <- as.numeric(results$VALUE)
    
    expectedWithGIS <- expectedWithGIS[order(expectedWithGIS$SITE, expectedWithGIS$METRIC),]
    expectedWithGIS$VALUE <- as.numeric(expectedWithGIS$VALUE)
    errs <- dfCompare(expectedWithGIS, results, c('SITE','METRIC'), zeroFudge=1e-4)
#    return(errs)
    checkEquals(NULL, errs
               ,"Error: Slope and bearing metrics are broken with GIS calculations, both protocols and extra incremnt values in thalweg table"
               )


    # Test calculation with both wadeable and boatable reach data and no GIS 
    # based calculations or extra incremnt values.
    results <- nrsaSlopeBearing(bBearing = fakeChanGeom %>%
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>%
                                           mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bDistance = fakeChanGeom %>%
                                            subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE') %>%
                                            mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,bSlope = fakeChanGeom %>%
                                         subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE') %>%
                                         mutate(LINE=ifelse(LINE==999, 0, LINE))
                               ,wBearing = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                        ,ifelse(PARAMETER == 'BEARING2', 1
                                                        ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                         )))
                                                  ,PARAMETER = NULL
                                                  )                                         
                               ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                         mutate(VALUE=as.numeric(VALUE))
                                                        ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                        ,'SITE'
                                                        ) %>%
                                                   mutate(VALUE = VALUE * nSta
                                                         ,nSta = NULL
                                                         )
                               ,wProportion = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                              mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                           ,ifelse(PARAMETER == 'PROP2', 1
                                                           ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                            )))
                                                    ,PARAMETER = NULL
                                                    )                                         
                               ,wSlope = subset(fakeChanGeom, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                         mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                      ,ifelse(PARAMETER == 'SLOPE2', 1
                                                      ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                      )))
                                               ,PARAMETER = NULL
                                               ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                               )                                         
                               ,gisSinuosity = NULL
                               ,gisSlope = NULL
                               )

    expected <- expected[order(expected$SITE, expected$METRIC),]
    results <- results[order(results$SITE, results$METRIC),]
    expected$VALUE <- as.numeric(expected$VALUE)
    results$VALUE <- as.numeric(results$VALUE)
    errs <- dfCompare(expected, results, c('SITE','METRIC'), zeroFudge=1e-3)
#    return(errs)
    checkEquals(NULL, errs
               ,"Error: Slope and bearing metrics are broken with both protocols using normal thalweg data"
               )
             
             
    # Test calculation restricted to wadeable reaches
    fakeProtocol.s <- subset(fakeProtocol, PROTOCOL=='WADEABLE')
    fakeThal.s <- subset(fakeThal, SITE %in% fakeProtocol.s$SITE)
    fakeChanGeom.s <- subset(fakeChanGeom, SITE %in% fakeProtocol.s$SITE)
    expected.s <- subset(expected, SITE %in% fakeProtocol.s$SITE)

    # TODO: the boatable args have zero rows, and would normally be allowed to be NULL -- that case should be tested as well
    results.s <- nrsaSlopeBearing(bBearing = fakeChanGeom.s %>%
                                             subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR')
                                 ,bDistance = fakeChanGeom.s %>%
                                              subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE')
                                 ,bSlope = fakeChanGeom.s %>%
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE')
                                 ,wBearing = subset(fakeChanGeom.s, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                             mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                          ,ifelse(PARAMETER == 'BEARING2', 1
                                                          ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                           )))
                                                   ,PARAMETER = NULL
                                                   )                                         
                                 ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                           mutate(VALUE=as.numeric(VALUE))
                                                          ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                          ,'SITE'
                                                          ) %>%
                                                     mutate(VALUE = VALUE * nSta
                                                           ,nSta = NULL
                                                           )
                                 ,wProportion = subset(fakeChanGeom.s, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                                mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                             ,ifelse(PARAMETER == 'PROP2', 1
                                                             ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                              )))
                                                      ,PARAMETER = NULL
                                                      )                                         
                                 ,wSlope = subset(fakeChanGeom.s, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                        ,ifelse(PARAMETER == 'SLOPE2', 1
                                                        ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                         )))
                                                 ,PARAMETER = NULL
                                                 ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                                 )                                         
                                 ,gisSinuosity = NULL
                                 ,gisSlope = NULL
                                 )
    results.s$VALUE <- as.numeric(results.s$VALUE)
    errs.s <- dfCompare(expected.s, results.s, c('SITE','METRIC'), zeroFudge=1e-3)
#    return(errs.s)
    checkEquals(NULL, errs.s
               ,"Error: Slope and bearing metrics are broken with wadeable data"
               )

             
    # Test calculation restricted to boatable reaches
    fakeProtocol.r <- subset(fakeProtocol, PROTOCOL=='BOATABLE')
    fakeThal.r <- subset(fakeThal, SITE %in% fakeProtocol.r$SITE)
    fakeChanGeom.r <- subset(fakeChanGeom, SITE %in% fakeProtocol.r$SITE)
    expected.r <- subset(expected, SITE %in% fakeProtocol.r$SITE)
  
    # TODO: the wadetable args have zero rows, and would normally be allowed to be NULL -- that case should be tested as well
    results.r <- nrsaSlopeBearing(bBearing = fakeChanGeom.r %>%
                                             subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>%
                                             mutate(LINE=ifelse(LINE==999, 0, LINE))
                                 ,bDistance = fakeChanGeom.r %>%
                                              subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE') %>%
                                              mutate(LINE=ifelse(LINE==999, 0, LINE))
                                 ,bSlope = fakeChanGeom.r %>%
                                           subset(SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE') %>%
                                           mutate(LINE=ifelse(LINE==999, 0, LINE))
                                 ,wBearing = subset(fakeChanGeom.r, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER)) %>%
                                             mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                                                          ,ifelse(PARAMETER == 'BEARING2', 1
                                                          ,ifelse(PARAMETER == 'BEARING3', 2, NA
                                                           )))
                                                   ,PARAMETER = NULL
                                                   )                                         
                                 ,wTransectSpacing = merge(subset(fakeThal_IncremntsAtEachSITE, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')] %>%
                                                           mutate(VALUE=as.numeric(VALUE))
                                                          ,nWadeableStationsPerTransect(fakeThal_IncremntsAtEachSITE[c('SITE','TRANSECT','STATION')])
                                                          ,'SITE'
                                                          ) %>%
                                                     mutate(VALUE = VALUE * nSta
                                                           ,nSta = NULL
                                                           )
                                 ,wProportion = subset(fakeChanGeom.r, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER)) %>%
                                                mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                                                             ,ifelse(PARAMETER == 'PROP2', 1
                                                             ,ifelse(PARAMETER == 'PROP3', 2, NA
                                                              )))
                                                      ,PARAMETER = NULL
                                                      )                                         
                                 ,wSlope = subset(fakeChanGeom.r, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER)) %>%
                                           mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                                                        ,ifelse(PARAMETER == 'SLOPE2', 1
                                                        ,ifelse(PARAMETER == 'SLOPE3', 2, NA
                                                         )))
                                                 ,PARAMETER = NULL
                                                 ,UNITS = ifelse(UNITS %in% c('', NA), 'CM', UNITS)
                                                 )                                         
                                 ,gisSinuosity = NULL
                                 ,gisSlope = NULL
                                 )
    results.r$VALUE <- as.numeric(results.r$VALUE)
    errs.r <- dfCompare(expected.r, results.r, c('SITE','METRIC'), zeroFudge=1e-3)
#    return(errs.r)
    checkEquals(NULL, errs.r
               ,"Error: Slope and bearing metrics are broken with boatable data"
               )

}


nrsaSlopeBearingTest.makeTestDataDEPRECATED <- function()
# Creates thalweg and channel geometry data used in testing nrsaSlopeBearing().
# Returns a list of dataframes, element 1 is thalweg, element 2 is chanGeom.
#
# Test data taken from WEMAP data on 2-Feb-2010, and is then transformed into
# the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes taken in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
{
#234567890123456789012345678901234567890
  wemapThal <- data.frame(matrix(
                  c('2000 WAZP99-0505 1', 'A', 1.5, 7, 354, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'B', 1.5, 6, 357, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'C', 1.5, 4, 11, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'D', 1.5, 7.5, 3, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'E', 1.5, 12.5, 9, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'F', 1.5, 5, 17, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'G', 1.5, 3.5, 5, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'H', 1.5, 1.5, 57, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'I', 1.5, 4, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'J', 1.5, 6.5, 53, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1', 'A', 1.5, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'C', 1.5, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'D', 1.5, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'E', 1.5, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'F', 1.5, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'G', 1.5, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'H', 1.5, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'I', 1.5, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'J', 1.5, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2003 WWYP99-0659 1', 'A', 1.5, 36, 161, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'B', 1.5, 12, 110, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'C', 1.5, 12, 193,  20
                   ,4,  75, 30,  0, 124, 50, 'CM'
                   ,'2003 WWYP99-0659 1', 'D', 1.5, 26, 230, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'E', 1.5,  8, 193, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'F', 1.5, 18, 120, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'G', 1.5,  9, 210,  50
                   ,2, 108, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'H', 1.5, 14, 246, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'I', 1.5,  2, 157,  50
                   ,10, 238, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'J', 1.5,  1, 100, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'

                   ,'2000 WAZP99-0569 1 no incremnt', 'A', NA, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'B', NA, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'C', NA, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'D', NA, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'E', NA, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'F', NA, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'G', NA, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'H', NA, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'I', NA, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'J', NA, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 no slopes', 'A', 1.5, NA, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'B', 1.5, NA, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 only 2 slopes', 'A', 1.5, 6, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WIDP99-0556 1', 'A', 30, 0.2, 150, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'B', 30, 0.2, 50, 40
                   ,0.2, 140, 60, NA, NA, NA , 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'C', 33.3333333, 0.2, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'D', 30, 0.2, 50, 28.5714286
                   ,0.2, 40, 28.5714286, 0.2, 20, 42.8571429, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'E', 30, 0.2, 70, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'F', 30, 0.2, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'G', 30, 0.1, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'H', 30, 0.2, 50, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'I', 30, 0.1, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'J', 30, 0.2, 30, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WSDP99-0531 1', 'A', 20, 0.1, 80, 45.7142857
                   ,0.1, 340, 40, 0.1, 20, 14.2857143, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'B', 20, 0.1, 50, 50
                   ,0.1, 10, 25, 0.1, 100, 25, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'C', 20, 0.1, 360, 22.5
                   ,0.1, 350, 42.5, 0.1, 60, 35, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'D', 20, 0.1, 40, 25
                   ,0.1, 40, 12.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'E', 20, 0.1, 330, 12.5
                   ,0.1, 20, 87.5, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'F', 20, 0.1, 120, 15
                   ,0.1, 330, 22.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'G', 20, 0.1, 50, 75
                   ,0.1, 140, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'H', 20, 0.1, 90, 40.7407407
                   ,0.1, 340, 59.2592593, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'I', 20, 0.1, 200, 30
                   ,0.1, 200, 32.5, 0.1, 220, 37.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'J', 20, 0.1, 180, 12.5
                   ,0.1, 160, 52.5, 0.1, 240, 35, 'PERCENT'
                   )
                   ,ncol=13, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapThal) <- c('SITE','TRANSECT','incremnt', 'slopet','beart','proportt'
                        ,'slope1','bear1','proport1','slope2','bear2','proport2'
                        ,'units'
                        )

  wemapRiverStations <- data.frame('SITE'=c(rep('2000 WIDP99-0556 1', 10)
                                          ,rep('2000 WSDP99-0531 1', 10)
                                          )
                                  ,'TRANSECT'=rep(LETTERS[1:10], 2)
                                  ,'nsta'=c(20,20,18,20,20, 20,20,20,20,20
                                           ,20,20,20,20,20, 20,20,20,20,20
                                           )
                                  )

  # Create fakeThal from transposed wemap thalweg data.  This holds the wadeable
  # reach incremnt values; everything else is in the channel geometry.  Transect
  # C of WAZP99-0505 ended at station 8, so that is removed explicitly.  The
  # UNITS information is handled separately.
  # To allow proper counting of stations per transect during metrics calculation,
  # DEPTH is added at each station, though these values will not be used.
  units <- rename(wemapThal[c('SITE','TRANSECT','units')], 'units', 'UNITS')
  
  ss <- subset(wemapThal, select=-units)
  tt <- dfLengthen(ss
                  ,c('SITE','TRANSECT')
                  ,'PARAMETER'
                  ,'VALUE'
                  ,names(ss)[!(names(ss) %in%
                                       c('SITE','TRANSECT')
                                     )]
                        )

  fakeThal <- subset(tt
                    ,SITE %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               ) &
                     PARAMETER == 'incremnt'
                    )
  fakeThal <- merge(fakeThal, list('STATION'=0:9), all=TRUE)
  fakeThal <- subset(fakeThal
                    ,!(SITE!='2000 WAZP99-0505 1' & TRANSECT=='C' & STATION=='9')
                    )
  fakeThal$PARAMETER <- 'INCREMNT'
  fakeThal$SAMPLE_TYPE <- 'PHAB_THALW'
  fakeThal$FLAG <- NA
  fakeThal$UNITS <- 'M'

  addDepths<-fakeThal
  addDepths$PARAMETER='DEPTH'
  addDepths$UNITS<-'CM'
  fakeThal<-rbind(fakeThal,addDepths)

  # Create fakeChanGeom from transposed wemap thalweg data.  River and stream
  # data are slightly different, and thus are assembled separately.  Rows with
  # missing values are removed.
  wadeable <- subset(tt
                    ,SITE %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               )
                     & PARAMETER != 'incremnt'
                    )
  wadeable$LINE <- NA
  wadeable$PARAMETER <- ifelse(wadeable$PARAMETER == 'slopet', 'SLOPE'
                       ,ifelse(wadeable$PARAMETER == 'beart', 'BEARING'
                       ,ifelse(wadeable$PARAMETER == 'proportt', 'PROP'
                       ,ifelse(wadeable$PARAMETER == 'slope1', 'SLOPE2'
                       ,ifelse(wadeable$PARAMETER == 'bear1', 'BEARING2'
                       ,ifelse(wadeable$PARAMETER == 'proport1', 'PROP2'
                       ,ifelse(wadeable$PARAMETER == 'slope2', 'SLOPE3'
                       ,ifelse(wadeable$PARAMETER == 'bear2', 'BEARING3'
                       ,ifelse(wadeable$PARAMETER == 'proport2', 'PROP3', NA
                       )))))))))
  wadeable <- merge(wadeable, units, by=c('SITE','TRANSECT'))
  wadeable$UNITS <- ifelse(wadeable$PARAMETER %in% c('SLOPE','SLOPE2','SLOPE3')
                          ,wadeable$UNITS
                          ,'NONE'
                          )

  bb <- subset(wemapThal
              ,!(SITE %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           )
                )
              )
  bb <- merge(bb, wemapRiverStations, by=c('SITE','TRANSECT'), all.x=TRUE)
  bb$distancet <- (as.numeric(bb$proportt)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance1 <- (as.numeric(bb$proport1)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance2 <- (as.numeric(bb$proport2)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$incremnt <- NULL
  bb$proportt <- NULL
  bb$proport1 <- NULL
  bb$proport2 <- NULL
  bb$nsta <- NULL

  boatable <- dfLengthen(bb, c('SITE','TRANSECT'), 'PARAMETER', 'VALUE'
                        ,names(bb)[!(names(bb) %in% c('SITE','TRANSECT'))]
                        )

  boatable$LINE <- ifelse(boatable$PARAMETER %in% c('slopet','beart','distancet'), 999
                  ,ifelse(boatable$PARAMETER %in% c('slope1','bear1','distance1'), 1
                  ,ifelse(boatable$PARAMETER %in% c('slope2','bear2','distance2'), 2, NA
                  )))

  boatable$PARAMETER <-
        ifelse(boatable$PARAMETER %in% c('slopet','slope1','slope2'), 'SLOPE'
       ,ifelse(boatable$PARAMETER %in% c('beart','bear1','bear2'), 'BEAR'
       ,ifelse(boatable$PARAMETER %in% c('distancet','distance1','distance2'), 'DISTANCE', NA
       )))
  boatable$UNITS <- 'NONE'

  fakeChanGeom <- subset(rbind(boatable, wadeable), !is.na(VALUE))
  fakeChanGeom$TRANLINE <- 'NONE'
  fakeChanGeom$BANK <- 'NONE'
  fakeChanGeom$SAMPLE_TYPE <- ifelse(fakeChanGeom$SITE %in%
                                       c('2000 WAZP99-0505 1'
                                        ,'2000 WAZP99-0569 1'
                                        ,'2003 WWYP99-0659 1'
                                        ,'2000 WAZP99-0569 1 no incremnt'
                                        ,'2000 WAZP99-0569 1 no slopes'
                                        ,'2000 WAZP99-0569 1 only 2 slopes'
                                        )
                                    ,'PHAB_SLOPE'
                                    ,'PHAB_CHANBFRONT'
                                    )
  fakeChanGeom$FLAG <- as.character(NA)

  return(list(fakeThal, fakeChanGeom))
}


nrsaSlopeBearingTest.makeThalwegOLD <- function()
# Create dataframe of thalweg data for unit test
{
  tc <-textConnection("
          SITE TRANSECT VALUE PARAMETER STATION SAMPLE_TYPE FLAG UNITS
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 no subsightings' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 9 PHAB_THALW NA CM
       ")
  tt <- read.table(tc, header=TRUE,stringsAsFactors=FALSE)
  close(tc)
  tt$VALUE <- as.character(tt$VALUE)

  return(tt)
}

nrsaSlopeBearingTest.makeThalweg <- function()
# Create dataframe of thalweg data for unit test
{
    tc <- textConnection('"SITE" "SAMPLE_TYPE" "TRANSECT" "STATION" "PARAMETER" "VALUE" "FLAG" "UNITS"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "C" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0505 1 with low slope" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 0 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 1 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 2 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 3 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 4 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 5 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 6 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 7 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 8 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 9 "DEPTH" NA NA "CM"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "A" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "B" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "C" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "D" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "E" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "F" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "G" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "H" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "I" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 0 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 1 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 2 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 3 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 4 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 5 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 6 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 7 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 8 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_THALW" "J" 9 "INCREMNT" NA NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 no slopes" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 missing PERCENT subsightings" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "A" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "B" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "C" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "D" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "E" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "F" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "G" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "H" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "I" 9 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 0 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 1 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 2 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 3 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 4 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 5 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 6 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 7 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 8 "INCREMNT" "1.5" NA "M"
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_THALW" "J" 9 "INCREMNT" "1.5" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 0 "INCREMNT" "1.6" NA "M"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "A" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "B" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "C" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "D" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "E" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "F" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "G" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "H" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "I" 9 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 0 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 1 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 2 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 3 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 4 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 5 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 6 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 7 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 8 "DEPTH" "1.5" NA "CM"
                          "2014 CARO-1043 1" "PHAB_THALW" "J" 9 "DEPTH" "1.5" NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 0 "INCREMNT" "1.0" NA "M"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 0 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 1 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 2 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 3 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 4 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 5 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 6 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 7 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 8 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 9 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 10 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 11 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 12 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 13 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "A" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "B" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "C" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "D" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "E" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "F" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "G" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "H" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "I" 14 "DEPTH" 1.5 NA "CM"
                          "2015 AKBB-018 1" "PHAB_THALW" "J" 14 "DEPTH" 1.5 NA "CM"
                          #"2015 AKBB-018 1" "PHAB_THALW" "D" 15 "DEPTH" 1.5 NA "CM" # actual data has this extra station.
                        ')
    thaldata <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(thaldata)
}


nrsaSlopeBearingTest.makeChannelGeometry <- function()
# Create dataframe of channel geometry data for unit test
{
    tc <- textConnection('"SITE" "SAMPLE_TYPE" "TRANSECT" "LINE" "BANK" "TRANLINE" "PARAMETER" "VALUE" "UNITS" "METHOD" "FLAG"
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "354" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "357" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "11" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "3" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "9" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "17" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "5" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "57" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "53" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "7" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "4" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "7.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "12.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "3.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "1.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "4" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "6.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "354" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "357" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "11" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "3" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "9" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "17" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "5" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "57" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "23" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "53" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "7" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "4" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "7.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "12.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "3.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "1.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "4" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "6.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "354" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "357" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "11" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "3" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "9" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "17" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "5" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "57" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "23" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "53" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "0.7" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "0.6" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "0.4" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "0.75" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "1.25" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "0.5" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "0.35" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "0.15" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "0.4" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with clinometer and low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "0.65" "PERCENT" "CL" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "354" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "357" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "11" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "3" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "9" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "17" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "5" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "57" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "53" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "0.7" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "0.6" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "0.4" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "0.75" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "1.25" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "0.5" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "0.35" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "0.15" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "0.4" "PERCENT" "TR" NA
                          "2000 WAZP99-0505 1 with low slope" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "0.65" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "40" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "4" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "65" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "37" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "10" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "107" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "76" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING2" "58" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "39" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING2" "53" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING2" "19" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "73" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "45" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING3" "0" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "30" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP2" "34" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP2" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "70" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP3" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "11" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "22" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "12.2" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE2" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE2" "13" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE2" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" "14" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE3" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "40" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "4" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "65" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "37" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "10" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "107" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "76" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING2" "58" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "39" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING2" "53" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING2" "19" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "73" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "45" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING3" "0" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "30" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP2" "34" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP2" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "70" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP3" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "11" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "22" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "12.2" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 missing PERCENT subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE3" NA "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "40" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "4" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "65" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "37" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "10" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "107" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "76" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING2" "58" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "39" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING2" "53" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING2" "19" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "73" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "45" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING3" "0" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "30" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP2" "34" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP2" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "70" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP3" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "11" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "22" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "12.2" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE2" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE2" "13" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE2" "8" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" "10" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" "14" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE3" "12" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "40" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "4" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "65" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "37" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "10" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "107" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "76" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING2" "58" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "39" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING2" "53" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING2" "19" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "73" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "45" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING3" "0" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "30" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP2" "34" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP2" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "70" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 no slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP3" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "40" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "4" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "65" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "37" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "10" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "107" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "76" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "23" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING2" "58" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "39" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING2" "53" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING2" "19" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "73" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "45" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING3" "0" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "80" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "30" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP2" "34" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "75" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP2" "25" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "20" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "70" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP3" "33" "NONE" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "6" "PERCENT" "TR" NA
                          "2000 WAZP99-0569 1 only 2 slopes" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "10" "PERCENT" "TR" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "150" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "70" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "30" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "360" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "240" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "599.9999994" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "257.1428574" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "150" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "70" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "30" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "360" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "240" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "599.9999994" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "257.1428574" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "SLOPE" "0.2" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "SLOPE" "0.1" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "SLOPE" "0.1" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 slope unit PCT" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "SLOPE" "0.2" "PERCENT" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "150" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "70" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "30" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "360" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "240" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "599.9999994" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "257.1428574" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with absent slopes" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "150" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "70" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "30" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "360" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "240" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "599.9999994" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "257.1428574" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with NA slopes" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "SLOPE" NA "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "150" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "70" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "30" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "360" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "240" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "599.9999994" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "257.1428574" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "171.4285716" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "600" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "CM" "NONE" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "SLOPE" "72" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "SLOPE" "48" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "SLOPE" "34.2857143" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "SLOPE" "51.4285713" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "SLOPE" "34.2857143" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "SLOPE" "60" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "SLOPE" "60" "CM" "NONE" NA
                          "2000 WIDP99-0556 1 with slopes in cm" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "SLOPE" "120" "CM" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 1 "NONE" "NONE" "BEAR" "340" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 2 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "BEAR" "80" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "BEAR" "10" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 2 "NONE" "NONE" "BEAR" "100" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 1 "NONE" "NONE" "BEAR" "350" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 2 "NONE" "NONE" "BEAR" "60" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "BEAR" "360" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "BEAR" "80" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "BEAR" "40" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 1 "NONE" "NONE" "BEAR" "20" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "BEAR" "330" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 1 "NONE" "NONE" "BEAR" "330" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 2 "NONE" "NONE" "BEAR" "80" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "BEAR" "120" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 1 "NONE" "NONE" "BEAR" "140" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "BEAR" "50" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 1 "NONE" "NONE" "BEAR" "340" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "BEAR" "90" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 1 "NONE" "NONE" "BEAR" "200" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 2 "NONE" "NONE" "BEAR" "220" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "BEAR" "200" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 1 "NONE" "NONE" "BEAR" "160" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 2 "NONE" "NONE" "BEAR" "240" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "BEAR" "180" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 1 "NONE" "NONE" "DISTANCE" "160" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 2 "NONE" "NONE" "DISTANCE" "57.1428572" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "DISTANCE" "182.8571428" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "DISTANCE" "100" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 2 "NONE" "NONE" "DISTANCE" "100" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "DISTANCE" "200" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 1 "NONE" "NONE" "DISTANCE" "170" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 2 "NONE" "NONE" "DISTANCE" "140" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "DISTANCE" "90" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "DISTANCE" "50" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "DISTANCE" "250" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "DISTANCE" "100" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 1 "NONE" "NONE" "DISTANCE" "350" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "DISTANCE" "50" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 1 "NONE" "NONE" "DISTANCE" "90" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 2 "NONE" "NONE" "DISTANCE" "250" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "DISTANCE" "60" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 1 "NONE" "NONE" "DISTANCE" "100" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "DISTANCE" "300" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 1 "NONE" "NONE" "DISTANCE" "237.0370372" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "DISTANCE" "162.9629628" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 1 "NONE" "NONE" "DISTANCE" "130" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 2 "NONE" "NONE" "DISTANCE" "150" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "DISTANCE" "120" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 1 "NONE" "NONE" "DISTANCE" "210" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 2 "NONE" "NONE" "DISTANCE" "140" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "DISTANCE" "50" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" NA "NONE" "NONE" "NA" "PERCENT" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "A" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "B" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "C" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "D" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "E" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "F" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "G" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "H" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "I" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 1 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 2 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2000 WSDP99-0531 1" "PHAB_CHANBFRONT" "J" 999 "NONE" "NONE" "SLOPE" "0.1" "NONE" "NONE" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "161" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "110" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "230" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "120" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "210" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "246" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "157" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "75" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "108" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "238" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING3" "124" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "20" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "30" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP3" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "36" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "26" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "8" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "18" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "9" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "14" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "2" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "1" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" "4" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" "2" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" "10" "CM" "TR" NA
                          "2003 WWYP99-0659 1" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE3" "0" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "161" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "110" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "230" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "120" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "210" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "246" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "157" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "75" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "108" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "238" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING3" "124" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "20" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "30" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP3" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "36" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "16" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "26" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "8" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "18" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "11" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "14" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "1" "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" NA "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" NA "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" NA "CM" "TR" NA
                          "2003 WWYP99-0659 1 missing CM subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE3" NA "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "161" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "110" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "121.0429871" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "230" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "120" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "159.0" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "246" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "197.5" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "76.95159" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "62.93204" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "76.0406" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "36" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "16" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "26" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "8" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "18" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "11" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "14" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "12" "CM" "TR" NA
                          "2003 WWYP99-0659 1 no subsightings" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "1" "CM" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "161" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "B" NA "NONE" "NONE" "BEARING" "110" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "D" NA "NONE" "NONE" "BEARING" "230" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "E" NA "NONE" "NONE" "BEARING" "193" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "F" NA "NONE" "NONE" "BEARING" "120" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING" "210" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "H" NA "NONE" "NONE" "BEARING" "246" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING" "157" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "J" NA "NONE" "NONE" "BEARING" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING2" "75" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "BEARING2" "108" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "BEARING2" "238" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "BEARING3" "124" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "A" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "B" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP" "20" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "D" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "E" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "F" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "H" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "J" NA "NONE" "NONE" "PROP" "100" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP2" "30" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "PROP2" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "PROP3" "50" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "A" NA "NONE" "NONE" "SLOPE" "36" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "B" NA "NONE" "NONE" "SLOPE" "12" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE" "12" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "D" NA "NONE" "NONE" "SLOPE" "26" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "E" NA "NONE" "NONE" "SLOPE" "8" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "F" NA "NONE" "NONE" "SLOPE" "18" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE" "9" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "H" NA "NONE" "NONE" "SLOPE" "14" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE" "2" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "J" NA "NONE" "NONE" "SLOPE" "1" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE2" "4" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "G" NA "NONE" "NONE" "SLOPE2" "2" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "I" NA "NONE" "NONE" "SLOPE2" "10" "NONE" "TR" NA
                          "2003 WWYP99-0659 1 slope unit NONE" "PHAB_SLOPE" "C" NA "NONE" "NONE" "SLOPE3" "0" "NONE" "TR" NA
#                         "SITE" "SAMPLE_TYPE" "TRANSECT" "LINE" "BANK" "TRANLINE" "PARAMETER" "VALUE" "UNITS" "METHOD" "FLAG"
#                         "2000 WAZP99-0569 1 no incremnt" "PHAB_SLOPE" "A" NA "NONE" "NONE" "BEARING" "50" "NONE" "TR" NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        A   NA NONE     NONE     BEARING     320   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        B   NA NONE     NONE     BEARING     320   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        C   NA NONE     NONE     BEARING     320   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        D   NA NONE     NONE     BEARING     347   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        E   NA NONE     NONE     BEARING     347   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        F   NA NONE     NONE     BEARING     331   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        G   NA NONE     NONE     BEARING     331   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        H   NA NONE     NONE     BEARING     331   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        I   NA NONE     NONE     BEARING     320   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        J   NA NONE     NONE     BEARING     320   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        A   NA NONE     NONE        PROP     100   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        B   NA NONE     NONE        PROP     100   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        C   NA NONE     NONE        PROP     100   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        D   NA NONE     NONE        PROP     100   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        E   NA NONE     NONE        PROP     100   NONE    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        F   NA NONE     NONE        PROP     100   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        G   NA NONE     NONE        PROP     100   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        H   NA NONE     NONE        PROP     100   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        I   NA NONE     NONE        PROP     100   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        J   NA NONE     NONE        PROP     100   NONE    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        A   NA NONE     NONE       SLOPE       0   PERCENT    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        B   NA NONE     NONE       SLOPE       0   PERCENT    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        C   NA NONE     NONE       SLOPE       0   PERCENT    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        D   NA NONE     NONE       SLOPE       4   PERCENT    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        E   NA NONE     NONE       SLOPE       1   PERCENT    CL   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        F   NA NONE     NONE       SLOPE       0   CM    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        G   NA NONE     NONE       SLOPE       0   CM    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        H   NA NONE     NONE       SLOPE       0   CM    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        I   NA NONE     NONE       SLOPE       0   CM    TR   NA
                          "2014 CARO-1043 1"  PHAB_SLOPE        J   NA NONE     NONE       SLOPE       1   CM    TR   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        A   NA NONE     NONE   BEARING   138 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        B   NA NONE     NONE   BEARING   120 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        C   NA NONE     NONE   BEARING   120 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        D   NA NONE     NONE   BEARING   116 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        E   NA NONE     NONE   BEARING   130 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        F   NA NONE     NONE   BEARING   138 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        G   NA NONE     NONE   BEARING   148 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        H   NA NONE     NONE   BEARING   148 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        I   NA NONE     NONE   BEARING   126 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        J   NA NONE     NONE   BEARING   110 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        A   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        B   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        C   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        D   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        E   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        F   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        G   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        H   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        I   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        J   NA NONE     NONE      PROP   100 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        A   NA NONE     NONE     SLOPE   4.0 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        B   NA NONE     NONE     SLOPE   5.5 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        C   NA NONE     NONE     SLOPE   5.0 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        D   NA NONE     NONE     SLOPE   5.0 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        E   NA NONE     NONE     SLOPE   4.5 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        F   NA NONE     NONE     SLOPE   5.5 ""          ""   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        G   NA NONE     NONE     SLOPE   6.0 ""          ""   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        H   NA NONE     NONE     SLOPE   4.0 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        I   NA NONE     NONE     SLOPE   4.0 PERCENT     CL   NA
                          "2015 AKBB-018 1"  PHAB_SLOPE        J   NA NONE     NONE     SLOPE   4.5 PERCENT     CL   NA
                         ')
    rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(rc)
}

nrsaSlopeBearingTest.makeChannelGeometryOLD2 <- function()
# Create dataframe of channel geometry data for unit test
{
    cg <- rbind(data.frame(SITE = '2000 WAZP99-0505 1'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,VALUE = as.character(c('7','100','354','100','6'
                                                  ,'357','100','11','4','7.5'
                                                  ,'3','100','100','9','12.5'
                                                  ,'100','17','5','3.5','5'
                                                  ,'100','57','100','1.5','100'
                                                  ,'23','4','100','53','6.5')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'TR'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(SITE = '2000 WAZP99-0505 1 with clinometer'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,VALUE = as.character(c('7','100','354','100','6'
                                                  ,'357','100','11','4','7.5'
                                                  ,'3','100','100','9','12.5'
                                                  ,'100','17','5','3.5','5'
                                                  ,'100','57','100','1.5','100'
                                                  ,'23','4','100','53','6.5')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'CL'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(SITE = '2000 WAZP99-0505 1 with low slope'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,VALUE = as.character(c('0.7','100','354','100','0.6'
                                                  ,'357','100','11','0.4','0.75'
                                                  ,'3','100','100','9','1.25'
                                                  ,'100','17','0.5','0.35','5'
                                                  ,'100','57','100','0.15','100'
                                                  ,'23','0.4','100','53','0.65')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'TR'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(SITE = '2000 WAZP99-0505 1 with clinometer and low slope'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,VALUE = as.character(c('0.7','100','354','100','0.6'
                                                  ,'357','100','11','0.4','0.75'
                                                  ,'3','100','100','9','1.25'
                                                  ,'100','17','0.5','0.35','5'
                                                  ,'100','57','100','0.15','100'
                                                  ,'23','0.4','100','53','0.65')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'CL'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(SITE = '2000 WAZP99-0569 1'
			              ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
                                       ,'B','B','B'
                                       ,'C','C','C','C','C','C'
                				       ,'D','D','D','D','D','D'
			                           ,'E','E','E','E','E','E'
            			               ,'F','F','F'
			                	       ,'G','G','G','G','G','G'
            			               ,'H','H','H','I','I','I','I','I','I'
			            	           ,'J','J','J'
  				                       )
			              ,VALUE = c('34','58','33','6','0'
                				     ,'12','10','33','50','100'
			                         ,'40','10','4','75','25'
            			             ,'39','12','12','20','53'
            			             ,'11','80','13','25','19'
			                         ,'25','8','75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80','10','100'
                                     ,'107','6','12.2','45','70'
			                         ,'30','76','14','8','100'
            			             ,'23')
			              ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
			                            ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
			                            ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
			                            ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
			                            ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
			                            ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
			                            ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
			                            ,'BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
			              ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
                                    ,'PERCENT','PERCENT','NONE','NONE','NONE'
			                        ,'NONE','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
            			            ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE'
                				    )
            			  ,TRANLINE = 'NONE'
			              ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
                          )
    	       ,data.frame(SITE = '2000 WAZP99-0569 1 missing PERCENT subsightings'
	            		  ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
            			               ,'B','B','B'
			                           ,'C','C','C','C','C','C'
            			               ,'D','D','D','D','D','D'
            			               ,'E','E','E','E','E','E'
			                           ,'F','F','F'
            			               ,'G','G','G','G','G','G'
            			               ,'H','H','H'
            			               ,'I','I','I','I','I','I'
            			               ,'J','J','J'
            			               )
            			  ,VALUE = c('34','58','33','6','0'
            			             ,NA,NA,'33','50','100'
            			             ,'40','10','4','75','25'
            			             ,'39','12',NA,'20','53'
            			             ,'11','80',NA,'25','19'
            			             ,'25',NA,'75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80',NA,'100'
            			             ,'107','6','12.2','45','70'
            			             ,'30','76',NA,'8','100'
            			             ,'23'
                				     )
            			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
            			                ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
            			                ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
            			                ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
            			                ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
            			                ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
            			                ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
            			                ,'BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
            			  ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
            			            ,'PERCENT','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
			                        ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
			                        ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE'
                				    )
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
               ,data.frame(SITE ='2000 WAZP99-0569 1 no incremnt'
            		   	  ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
                				       ,'B','B','B'
	                			       ,'C','C','C','C','C','C'
                				       ,'D','D','D','D','D','D'
                				       ,'E','E','E','E','E','E'
                				       ,'F','F','F'
                				       ,'G','G','G','G','G','G'
                				       ,'H','H','H'
                				       ,'I','I','I','I','I','I'
                				       ,'J','J','J'
                				       )
            			  ,VALUE = c('34','58','33','6','0'
            			             ,'12','10','33','50','100'
            			             ,'40','10','4','75','25'
            			             ,'39','12','12','20','53'
            			             ,'11','80','13','25','19'
            			             ,'25','8','75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80','10','100'
            			             ,'107','6','12.2','45','70'
            			             ,'30','76','14','8','100'
            			             ,'23'
                				     )
             			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
            			                ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
            			                ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
            			                ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
            			                ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
            			                ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
            			                ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
            			                ,'BEARING')
            			  ,LINE = as.numeric(NA)
            			  ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
            			            ,'PERCENT','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','NONE','NONE'
			                        ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
            			            ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE')
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
     	       ,data.frame(SITE = '2000 WAZP99-0569 1 no slopes'
            			  ,TRANSECT = c('A','A','A','A','A','A'
                				       ,'B','B'
	                			       ,'C','C','C','C'
                				       ,'D','D','D','D'
                				       ,'E','E','E','E'
                     	               ,'F','F'
            			               ,'G','G','G','G'
                                       ,'H','H'
                                       ,'I','I','I','I'
                                       ,'J','J'
                				       )
            			  ,VALUE = c('0','33','34','58','50'
            			             ,'33','100','40','75','39'
            			             ,'4','25','20','53','25'
			                         ,'80','19','25','75','65'
            			             ,'100','37','20','73','10'
            			             ,'80','107','100','30','45'
            			             ,'70','76','100','23')
            			  ,PARAMETER = c('BEARING3','PROP3','PROP2','BEARING2','BEARING'
            			                ,'PROP','PROP','BEARING','PROP2','BEARING2'
            			                ,'BEARING','PROP','PROP2','BEARING2','BEARING'
            			                ,'PROP','BEARING2','PROP2','PROP','BEARING'
            			                ,'PROP','BEARING','PROP2','BEARING2','BEARING'
            			                ,'PROP','BEARING','PROP','PROP','BEARING2'
            			                ,'PROP2','BEARING','PROP','BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
			              ,UNITS = 'NONE'
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
    	       ,data.frame(SITE='2000 WAZP99-0569 1 only 2 slopes'
	            		  ,TRANSECT = c('A','A','A','A','A','A','A'
                				       ,'B','B','B'
                 				       ,'C','C','C','C'
                                       ,'D','D','D','D'
	                                   ,'E','E','E','E'
                                       ,'F','F'
                                       ,'G','G','G','G'
                                       ,'H','H'
                                       ,'I','I','I','I'
                                       ,'J','J'
                                       )
            			  ,VALUE = c('34','58','33','6','0'
			                         ,'33','50','100','40','10'
            			             ,'4','75','25','39','20'
			                         ,'53','80','25','19','25'
			                         ,'75','65','100','37','20'
            			             ,'73','10','80','107','100'
			                         ,'76','30','70','45','100'
			                         ,'23')
            			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
			                            ,'PROP','BEARING','PROP','BEARING','SLOPE'
            			                ,'BEARING','PROP2','PROP','BEARING2','PROP2'
			                            ,'BEARING2','PROP','BEARING','BEARING2','PROP2'
			                            ,'PROP','BEARING','PROP','BEARING','PROP2'
            				            ,'BEARING2','BEARING','PROP','BEARING','PROP'
			                            ,'BEARING','PROP','PROP2','BEARING2','PROP'
			                            ,'BEARING')
            			  ,LINE = as.numeric(NA)
			              ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
                				    ,'NONE','NONE','NONE','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE'
				                    )
            			  ,TRANLINE = 'NONE'
			              ,BANK = 'NONE'
			              ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
			              ,METHOD = 'TR'
			              ,stringsAsFactors=FALSE
            			  ) 
	           ,data.frame(SITE = '2000 WIDP99-0556 1'
            			  ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'B','D','B','D','D','D','A','B','C','D'
			                           ,'E','F','G','H','I','J','A','B','C','D'
			                           ,'E','F','G','H','I','J','B','D','D'
                				       )        
			              ,VALUE = c('0.2','0.2','0.2','0.2','0.2'
			                         ,'0.2','0.1','0.2','0.1','0.2'
             			             ,'150','50','40','50','70'
			                         ,'60','60','50','40','30'
                     	             ,'0.2','0.2','140','40','0.2'
             			             ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
             			             ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                         ,'PERCENT','600','240','599.9999994','171.4285716'
			                         ,'600','600','600','600','600'
			                         ,'600','360','171.4285716','257.1428574'
				                     )
			              ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
            			                ,'BEAR','NA','NA','NA','NA'
            			                ,'NA','NA','NA','NA','NA'
			                            ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			                ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
            			  ,LINE = c(999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,1, 1, 1, 1, 2
			                       ,2, NA, NA, NA, NA
            			           ,NA, NA, NA, NA, NA
			                       ,NA, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 1, 1, 2)
            			  ,UNITS = 'NONE'
			              ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
			              ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
            			  ,FLAG = as.character(NA)
			              ,METHOD = 'NONE'
            			  ,stringsAsFactors=FALSE
			              )
	           ,data.frame(SITE = '2000 WIDP99-0556 1 with slopes in cm'
            			  ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'B','D','B','D','D','D','A','B','C','D'
			                           ,'E','F','G','H','I','J','A','B','C','D'
			                           ,'E','F','G','H','I','J','B','D','D'
                				       )        
			              ,VALUE = c('120','48','120','34.2857143','120'        # slopeElev = slopePct * distInCM/100
			                         ,'120','60','120','60','120'
             			             ,'150','50','40','50','70'
			                         ,'60','60','50','40','30'
                     	             ,'72','34.2857143','140','40','51.4285713'
             			             ,'20','CM','CM','CM','CM'                     # unit changed from PERCENT to CM
             			             ,'CM','CM','CM','CM','CM'
			                         ,'CM','600','240','599.9999994','171.4285716'
			                         ,'600','600','600','600','600'
			                         ,'600','360','171.4285716','257.1428574'
				                     )
			              ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
            			                ,'BEAR','NA','NA','NA','NA'
            			                ,'NA','NA','NA','NA','NA'
			                            ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			                ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
            			  ,LINE = c(999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,1, 1, 1, 1, 2
			                       ,2, NA, NA, NA, NA
            			           ,NA, NA, NA, NA, NA
			                       ,NA, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 1, 1, 2)
            			  ,UNITS = c('CM','CM','CM','CM','CM'            # was all 'NONE', now changed for elevation
			                        ,'CM','CM','CM','CM','CM'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'CM','CM','NONE','NONE','CM'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE')
			              ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
			              ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
            			  ,FLAG = as.character(NA)
			              ,METHOD = 'NONE'
            			  ,stringsAsFactors=FALSE
			              )
               ,data.frame(SITE = '2000 WIDP99-0556 1 with NA slopes'
                          ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
                                       ,'A','B','C','D','E','F','G','H','I','J'
                                       ,'B','D','B','D','D','D','A','B','C','D'
                                       ,'E','F','G','H','I','J','A','B','C','D'
                                       ,'E','F','G','H','I','J','B','D','D'
                                       )        
                          ,VALUE = c(NA, NA, NA, NA, NA
                                     ,NA, NA, NA, NA, NA
                                     ,'150','50','40','50','70'
                                     ,'60','60','50','40','30'
                                     ,NA, NA, '140','40', NA
                                     ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','600','240','599.9999994','171.4285716'
                                     ,'600','600','600','600','600'
                                     ,'600','360','171.4285716','257.1428574'
                                     )
                          ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
                                        ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
                                        ,'BEAR','NA','NA','NA','NA'
                                        ,'NA','NA','NA','NA','NA'
                                        ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
                          ,LINE = c(999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,1, 1, 1, 1, 2
                                   ,2, NA, NA, NA, NA
                                   ,NA, NA, NA, NA, NA
                                   ,NA, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 1, 1, 2)
                          ,UNITS = 'NONE'
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'NONE'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(SITE = '2000 WIDP99-0556 1 with absent slopes'
                          ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
                                       ,'B','D','D','A','B','C','D'
                                       ,'E','F','G','H','I','J','A','B','C','D'
                                       ,'E','F','G','H','I','J','B','D','D'
                                       )        
                          ,VALUE = c('150','50','40','50','70'
                                     ,'60','60','50','40','30'
                                     ,'140','40'
                                     ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','600','240','599.9999994','171.4285716'
                                     ,'600','600','600','600','600'
                                     ,'600','360','171.4285716','257.1428574'
                                     )
                          ,PARAMETER = c('BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR'
                                        ,'BEAR','NA','NA','NA','NA'
                                        ,'NA','NA','NA','NA','NA'
                                        ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
                          ,LINE = c(999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,1, 1
                                   ,2, NA, NA, NA, NA
                                   ,NA, NA, NA, NA, NA
                                   ,NA, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 1, 1, 2)
                          ,UNITS = 'NONE'
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'NONE'
                          ,stringsAsFactors=FALSE
                          )
        	   ,data.frame(SITE = '2000 WIDP99-0556 1 slope unit PCT'
            			  ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
			                           ,'B','D','B','D','D','D','A','B','C','D'
            			               ,'E','F','G','H','I','J','A','B','C','D'
			                           ,'E','F','G','H','I','J','B','D','D'
                 				       )        
            			  ,VALUE = c('0.2','0.2','0.2','0.2','0.2'
		                             ,'0.2','0.1','0.2','0.1','0.2'
			                         ,'150','50','40','50','70'
            			             ,'60','60','50','40','30'
			                         ,'0.2','0.2','140','40','0.2'
			                         ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
            			             ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                         ,'PERCENT','600','240','599.9999994','171.4285716'
			                         ,'600','600','600','600','600'
            			             ,'600','360','171.4285716','257.1428574'
			                	     )
            			  ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
			                            ,'BEAR','NA','NA','NA','NA'
			                            ,'NA','NA','NA','NA','NA'
            			                ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
            			  ,LINE = c(999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,1, 1, 1, 1, 2
            			           ,2, NA, NA, NA, NA
			                       ,NA, NA, NA, NA, NA
            			           ,NA, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 1, 1, 2
			                	   )
            			  ,UNITS = c('PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                        ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE')
			              ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
			              ,FLAG = as.character(NA)
			              ,METHOD = 'NONE'
            			  ,stringsAsFactors=FALSE
			              )
	           ,data.frame(SITE = '2000 WSDP99-0531 1'
			              ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','F','I','J'
                				       ,'A','B','C','D','F','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','F','I','J')
            			  ,VALUE = c('0.1','0.1','0.1','0.1','0.1'
			                         ,'0.1','0.1','0.1','0.1','0.1'
			                         ,'80','50','360','40','330'
            			             ,'120','50','90','200','180'
			                         ,'0.1','0.1','0.1','0.1','0.1'
			                         ,'0.1','0.1','0.1','0.1','0.1'
            			             ,'340','10','350','40','20'
			                         ,'330','140','340','200','160'
			                         ,'0.1','0.1','0.1','0.1','0.1'
            			             ,'0.1','0.1','20','100','60'
			                         ,'80','80','220','240','PERCENT'
			                         ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
            			             ,'PERCENT','PERCENT','PERCENT','PERCENT','182.8571428'
			                         ,'200','90','100','50','60'
			                         ,'300','162.9629628','120','50','160'
            			             ,'100','170','50','350','90'
			                         ,'100','237.0370372','130','210','57.1428572'
			                         ,'100','140','250','250','150'
            			             ,'140')
            			,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			              ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			              ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			              ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'SLOPE','SLOPE','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','NA'
            			              ,'NA','NA','NA','NA','NA'
			                          ,'NA','NA','NA','NA','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			              ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			              ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE'
                				      )
            			,LINE = c(999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 999
            			         ,999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 999
         	                     ,1, 1, 1, 1, 1
            			         ,1, 1, 1, 1, 1
			                     ,1, 1, 1, 1, 1
            			         ,1, 1, 1, 1, 1
			                     ,2, 2, 2, 2, 2
            			         ,2, 2, 2, 2, 2
			                     ,2, 2, 2, 2, NA
            			         ,NA, NA, NA, NA, NA
			                     ,NA, NA, NA, NA, 999
            			         ,999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 1
            			         ,1, 1, 1, 1, 1
			                     ,1, 1, 1, 1, 2
            			         ,2, 2, 2, 2, 2
            			         ,2)
            			,UNITS = 'NONE'
			            ,TRANLINE = 'NONE'
            			,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
            			,FLAG = as.character(NA)
			            ,METHOD = 'NONE'
            			,stringsAsFactors=FALSE
			)
	       ,data.frame(SITE = '2003 WWYP99-0659 1'
        			  ,TRANSECT = c('A','A','A','B','B'
		        	               ,'B','C','C','C','C'
			                       ,'C','C','C','C','C'
			                       ,'D','D','D','E','E'
        			               ,'E','F','F','F','G'
		        	               ,'G','G','G','G','G'
			                       ,'H','H','H','I','I'
			                       ,'I','I','I','I','J'
        			               ,'J','J'
		        		       )
        			  ,VALUE = c('161','36','100','12','100'
		        	             ,'110','50','30','75','193'
			                     ,'4','12','124','0','20'
			                     ,'26','230','100','100','193'
        			             ,'8','100','120','18','50'
		        	             ,'9','108','210','2','50'
			                     ,'246','100','14','10','50'
			                     ,'238','50','157','2','100'
        			             ,'100','1'
		        		     )
        			  ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
		        	                ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                        ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                        ,'SLOPE','BEARING','PROP','PROP','BEARING'
        			                ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
		        	                ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                        ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                        ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
        			                ,'BEARING','SLOPE'
		        		 	)
        			  ,LINE = as.numeric(NA)
		        	  ,UNITS = c('NONE','CM','NONE','CM','NONE'
			                    ,'NONE','NONE','NONE','NONE','NONE'
			                    ,'CM','CM','NONE','CM','NONE'
        			            ,'CM','NONE','NONE','NONE','NONE'
		        	            ,'CM','NONE','NONE','CM','NONE'
			                    ,'CM','NONE','NONE','CM','NONE'
			                    ,'NONE','NONE','CM','CM','NONE'
        			            ,'NONE','NONE','NONE','CM','NONE'
		        	            ,'NONE','CM'
				                )
        			  ,TRANLINE = 'NONE'
		        	  ,BANK = 'NONE'
        			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
		        	  ,FLAG = as.character(NA)
        			  ,METHOD = 'TR'
		        	  ,stringsAsFactors=FALSE
			  )
	       ,data.frame(SITE = '2003 WWYP99-0659 1 no subsightings'
        			  ,TRANSECT = c('A','A','A'
        			               ,'B','B','B'
        			               ,'C','C','C'
			                       ,'D','D','D'
        			               ,'E','E','E'
        			               ,'F','F','F'
        			               ,'G','G','G'
			                       ,'H','H','H'
        			               ,'I','I','I'
        			               ,'J','J','J'
		        		           )
        			  ,VALUE = c('161','36','100'
        			            ,'12','100','110'
        			            ,'121.0429871','16','76.95159'   # condensed transect uses hand calculated bearing; proportion hand calculated to reflect shorter resulting crows distance
			                    ,'26','230','100'
        			            ,'100','193','8'
        			            ,'100','120','18'
        			            ,'159.0','11', '62.93204'        # condensed transect uses hand calculated bearing; proportion hand calculated to reflect shorter resulting crows distance
			                    ,'246','100','14'
        			            ,'197.5','12','76.0406'          # condensed transect uses hand calculated bearing; proportion hand calculated to reflect shorter resulting crows distance
        			            ,'100','100','1'
		        		        )
        			  ,PARAMETER = c('BEARING','SLOPE','PROP'
        			                ,'SLOPE','PROP','BEARING'
        			                ,'BEARING','SLOPE','PROP' # condensed transect
			                        ,'SLOPE','BEARING','PROP'
        			                ,'PROP','BEARING','SLOPE'
        			                ,'PROP','BEARING','SLOPE'
        			                ,'BEARING','SLOPE','PROP' # condensed transect
			                        ,'BEARING','PROP','SLOPE'
        			                ,'BEARING','SLOPE','PROP' # condensed transect
        			                ,'PROP','BEARING','SLOPE'
		        		 	        )
        			  ,LINE = as.numeric(NA)
		        	  ,UNITS = c('NONE','CM','NONE'
		        	            ,'CM','NONE','NONE'
		        	            ,'NONE','CM','NONE' # condensed transect
        			            ,'CM','NONE','NONE'
		        	            ,'NONE','NONE','CM'
		        	            ,'NONE','NONE','CM'
		        	            ,'NONE','CM','NONE' # condensed transect
		        	            ,'NONE','NONE','CM'
		        	            ,'NONE','CM','NONE' # condensed transect
		        	            ,'NONE','NONE','CM'
				                )
        			  ,TRANLINE = 'NONE'
		        	  ,BANK = 'NONE'
        			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
		        	  ,FLAG = as.character(NA)
        			  ,METHOD = 'TR'
		        	  ,stringsAsFactors=FALSE
			  )
	          ,data.frame(SITE ='2003 WWYP99-0659 1 missing CM subsightings'
			             ,TRANSECT = c('A','A','A','B','B'
            			              ,'B','C','C','C','C'
                                      ,'C','C','C','C','C'
			                          ,'D','D','D','E','E'
			                          ,'E','F','F','F','G'
			                          ,'G','G','G','G','G'
			                          ,'H','H','H','I','I'
			                          ,'I','I','I','I','J'
			                          ,'J','J'
				                      )
			             ,VALUE = c('161','36','100','12','100'
			                        ,'110','50','30','75','193'
                                    ,NA,'16','124',NA,'20'
			                        ,'26','230','100','100','193'
			                        ,'8','100','120','18','50'
			                        ,'11','108','210',NA,'50'
			                        ,'246','100','14',NA,'50'
			                        ,'238','50','157','12','100'
			                        ,'100','1')
			             ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
			                           ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                           ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                           ,'SLOPE','BEARING','PROP','PROP','BEARING'
			                           ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
			                           ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                           ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                           ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
			                           ,'BEARING','SLOPE')
            			 ,LINE = as.numeric(NA)
                         ,UNITS = c('NONE','CM','NONE','CM','NONE'
            			           ,'NONE','NONE','NONE','NONE','NONE'
			                       ,'CM','CM','NONE','CM','NONE'
                                   ,'CM','NONE','NONE','NONE','NONE'
                                   ,'CM','NONE','NONE','CM','NONE'
			                       ,'CM','NONE','NONE','CM','NONE'
			                       ,'NONE','NONE','CM','CM','NONE'
			                       ,'NONE','NONE','NONE','CM','NONE'
			                       ,'NONE','CM')
			            ,TRANLINE = 'NONE'
			            ,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_SLOPE'
			            ,FLAG = as.character(NA)
			            ,METHOD = 'TR'
			            ,stringsAsFactors=FALSE
			            )
	         ,data.frame(SITE = '2003 WWYP99-0659 1 slope unit NONE'
			            ,TRANSECT = c('A','A','A','B','B'
                                      ,'B','C','C','C','C'
			                          ,'C','C','C','C','C'
			                          ,'D','D','D','E','E'
			                          ,'E','F','F','F','G'
			                          ,'G','G','G','G','G'
			                          ,'H','H','H','I','I'
			                          ,'I','I','I','I','J'
			                          ,'J','J')
			            ,VALUE = c('161','36','100','12','100'
			                       ,'110','50','30','75','193'
			                       ,'4','12','124','0','20'
                                   ,'26','230','100','100','193'
			                       ,'8','100','120','18','50'
			                       ,'9','108','210','2','50'
			                       ,'246','100','14','10','50'
			                       ,'238','50','157','2','100'
			                       ,'100','1')
			            ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
                                      ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                          ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                          ,'SLOPE','BEARING','PROP','PROP','BEARING'
			                          ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
			                          ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                          ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                          ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
			                          ,'BEARING','SLOPE'
					                  )
			            ,LINE = as.numeric(NA)
			            ,UNITS = 'NONE'
			            ,TRANLINE = 'NONE'
			            ,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_SLOPE'
			            ,FLAG = as.character(NA)
			            ,METHOD = 'TR'
			            ,stringsAsFactors=FALSE
			            ) 
              )

  return(cg)
}

nrsaSlopeBearingTest.makeChannelGeometryOLD <- function()
# Create dataframe of channel geometry data for unit test
{
  tc <-textConnection("
         SITE TRANSECT VALUE PARAMETER LINE UNITS TRANLINE BANK SAMPLE_TYPE FLAG
        '2000 WIDP99-0556 1' A 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 0.1 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 0.1 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A 150 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 70 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 30 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 150 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 70 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 30 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 80 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 360 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 330 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 120 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 90 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 200 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 180 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 340 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 10 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 350 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 20 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 330 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 340 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 200 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 160 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 60 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 80 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 80 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 220 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 240 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 240 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 599.9999994 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 171.4285716 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 240 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 599.9999994 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 171.4285716 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 182.8571428 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 200 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 90 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 100 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 50 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 60 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 300 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 162.9629628 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 120 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 50 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 360 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 171.4285716 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 360 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 171.4285716 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 160 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 170 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 50 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 350 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 90 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 100 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 237.0370372 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 130 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 210 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 257.1428574 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 257.1428574 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 57.1428572 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 140 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 250 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 250 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 150 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 140 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WAZP99-0505 1' A 7 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' A 354 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 357 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 11 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 4 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 7.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 3 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 9 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 12.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 17 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 3.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 5 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 57 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 1.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 4 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 53 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 6.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 12 SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 12 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 13 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 8 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 14 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 12 SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 12 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 13 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 8 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 14 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A NA SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 36 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 4 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 0 SLOPE3 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 26 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 8 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 18 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 9 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 2 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 14 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 10 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 2 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 1 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 36 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 12 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 4 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 12 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 0 SLOPE3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 26 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 8 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 18 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 9 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 2 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 14 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 10 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 2 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 1 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 36 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 16 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C NA SLOPE3 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 26 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 8 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 18 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 11 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 14 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 1 SLOPE NA CM NONE NONE PHAB_SLOPE NA
       ")
  tt <- read.table(tc, header=TRUE,stringsAsFactors=FALSE)
  close(tc)
  tt$LINE <- as.numeric(tt$LINE)
  tt$FLAG <- as.logical(tt$FLAG)
  tt$METHOD <- 'TR'

  return(tt)
}


nrsaSlopeBearingTest.makeProtocols <- function()
# Create dataframe of protocol information for unit test
{
  # return(data.frame('SITE'=c('2000 WAZP99-0505 1'
  #                          ,'2000 WAZP99-0505 1 with clinometer'
  #                          ,'2000 WAZP99-0505 1 with low slope'
  #                          ,'2000 WAZP99-0505 1 with clinometer and low slope'
  #                          ,'2000 WAZP99-0569 1'
  #                          ,'2003 WWYP99-0659 1'
  #                          ,'2003 WWYP99-0659 1 no subsightings'
  #                          ,'2003 WWYP99-0659 1 slope unit NONE'
  #                          ,'2003 WWYP99-0659 1 missing CM subsightings'
  #                          ,'2000 WAZP99-0569 1 no incremnt'
  #                          ,'2000 WAZP99-0569 1 no slopes'
  #                          ,'2000 WAZP99-0569 1 only 2 slopes'
  #                          ,'2000 WAZP99-0569 1 missing PERCENT subsightings'
  # 
  #                          ,'2000 WIDP99-0556 1'
  #                          ,'2000 WIDP99-0556 1 with slopes in cm'
  #                          ,'2000 WIDP99-0556 1 with NA slopes'
  #                          ,'2000 WIDP99-0556 1 with absent slopes'
  #                          ,'2000 WIDP99-0556 1 slope unit PCT'
  #                          ,'2000 WSDP99-0531 1'
  #                          )
  #                  ,'PROTOCOL'=c('WADEABLE','WADEABLE','WADEABLE','WADEABLE'
  #                               ,'WADEABLE','WADEABLE','WADEABLE','WADEABLE','WADEABLE'
  #                               ,'WADEABLE','WADEABLE','WADEABLE','WADEABLE'
  #                               ,'BOATABLE','BOATABLE','BOATABLE','BOATABLE'
  #                               ,'BOATABLE','BOATABLE'
  #                               )
  #                  ,stringsAsFactors=FALSE
  #                  )
  #       )
    tc <- textConnection("  SITE PROTOCOL
                            '2000 WAZP99-0505 1' WADEABLE
                            '2000 WAZP99-0505 1 with clinometer' WADEABLE
                            '2000 WAZP99-0505 1 with low slope' WADEABLE
                            '2000 WAZP99-0505 1 with clinometer and low slope' WADEABLE
                            '2000 WAZP99-0569 1' WADEABLE
                            '2003 WWYP99-0659 1' WADEABLE
                            '2003 WWYP99-0659 1 no subsightings' WADEABLE
                            '2003 WWYP99-0659 1 slope unit NONE' WADEABLE
                            '2003 WWYP99-0659 1 missing CM subsightings' WADEABLE
                            '2000 WAZP99-0569 1 no incremnt' WADEABLE
                            '2000 WAZP99-0569 1 no slopes' WADEABLE
                            '2000 WAZP99-0569 1 only 2 slopes' WADEABLE
                            '2000 WAZP99-0569 1 missing PERCENT subsightings' WADEABLE
                            '2000 WIDP99-0556 1' BOATABLE
                            '2000 WIDP99-0556 1 with slopes in cm' BOATABLE
                            '2000 WIDP99-0556 1 with NA slopes' BOATABLE
                            '2000 WIDP99-0556 1 with absent slopes' BOATABLE
                            '2000 WIDP99-0556 1 slope unit PCT' BOATABLE
                            '2000 WSDP99-0531 1' BOATABLE
                            '2014 CARO-1043 1' WADEABLE
                            '2015 AKBB-018 1' WADEABLE
                        ")
    rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(rc)
}


nrsaSlopeBearingTest.makeExpectedResults <- function()
# Create dataframe of calculation results for unit test using field values.
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable SITEs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
    tc <- textConnection("                   SITE        METRIC            VALUE
                             '2000 WAZP99-0505 1'  xslope_field             5.75
                             '2000 WAZP99-0505 1'        xslope             5.75
                             '2000 WAZP99-0505 1'        vslope           2.9930
                             '2000 WAZP99-0505 1'          nslp               10
                             '2000 WAZP99-0505 1'       transpc             15.0
                             '2000 WAZP99-0505 1'      xbearing           16.442
                             '2000 WAZP99-0505 1'          sinu           1.0680
                             '2000 WAZP99-0505 1' pctClinometer                0
             '2000 WAZP99-0505 1 with clinometer'  xslope_field             5.75
             '2000 WAZP99-0505 1 with clinometer'        xslope             5.75
             '2000 WAZP99-0505 1 with clinometer'        vslope           2.9930
             '2000 WAZP99-0505 1 with clinometer'          nslp               10
             '2000 WAZP99-0505 1 with clinometer'       transpc             15.0
             '2000 WAZP99-0505 1 with clinometer'      xbearing           16.442
             '2000 WAZP99-0505 1 with clinometer'          sinu           1.0680
             '2000 WAZP99-0505 1 with clinometer' pctClinometer              100
              '2000 WAZP99-0505 1 with low slope'  xslope_field            0.575
              '2000 WAZP99-0505 1 with low slope'        xslope            0.575
              '2000 WAZP99-0505 1 with low slope'        vslope          0.29930
              '2000 WAZP99-0505 1 with low slope'          nslp               10
              '2000 WAZP99-0505 1 with low slope'       transpc             15.0
              '2000 WAZP99-0505 1 with low slope'      xbearing           16.442
              '2000 WAZP99-0505 1 with low slope'          sinu           1.0680
              '2000 WAZP99-0505 1 with low slope' pctClinometer                0
'2000 WAZP99-0505 1 with clinometer and low slope' xslope_field            0.575
'2000 WAZP99-0505 1 with clinometer and low slope'       xslope            0.575
'2000 WAZP99-0505 1 with clinometer and low slope'       vslope          0.29930
'2000 WAZP99-0505 1 with clinometer and low slope'         nslp               10
'2000 WAZP99-0505 1 with clinometer and low slope'      transpc             15.0
'2000 WAZP99-0505 1 with clinometer and low slope'    xbearing           16.442
'2000 WAZP99-0505 1 with clinometer and low slope'         sinu           1.0680
'2000 WAZP99-0505 1 with clinometer and low slope' pctClinometer             100
                             '2000 WAZP99-0569 1'  xslope_field          10.8300
                             '2000 WAZP99-0569 1'        xslope          10.8300
                             '2000 WAZP99-0569 1'        vslope           3.5006
                             '2000 WAZP99-0569 1'          nslp               10
                             '2000 WAZP99-0569 1'       transpc           15.000
                             '2000 WAZP99-0569 1'      xbearing         42.43476
                             '2000 WAZP99-0569 1'          sinu           1.1251
                             '2000 WAZP99-0569 1' pctClinometer                0
                             '2003 WWYP99-0659 1'  xslope_field      1.026666667
                             '2003 WWYP99-0659 1'        xslope      1.026666667
                             '2003 WWYP99-0659 1'        vslope      0.648035969
                             '2003 WWYP99-0659 1'          nslp               10
                             '2003 WWYP99-0659 1'       transpc           15.000
                             '2003 WWYP99-0659 1'      xbearing         161.8415
                             '2003 WWYP99-0659 1'          sinu          1.66858
                             '2003 WWYP99-0659 1' pctClinometer                0
             '2003 WWYP99-0659 1 no subsightings'  xslope_field      1.026666667
             '2003 WWYP99-0659 1 no subsightings'        xslope      1.026666667
             '2003 WWYP99-0659 1 no subsightings'        vslope      0.648035969
             '2003 WWYP99-0659 1 no subsightings'          nslp               10
             '2003 WWYP99-0659 1 no subsightings'       transpc           15.000
             '2003 WWYP99-0659 1 no subsightings'      xbearing         161.8415
             '2003 WWYP99-0659 1 no subsightings'          sinu          1.66858
             '2003 WWYP99-0659 1 no subsightings' pctClinometer                0
             '2003 WWYP99-0659 1 slope unit NONE'  xslope_field      1.026666667
             '2003 WWYP99-0659 1 slope unit NONE'        xslope      1.026666667
             '2003 WWYP99-0659 1 slope unit NONE'        vslope      0.648035969
             '2003 WWYP99-0659 1 slope unit NONE'          nslp               10
             '2003 WWYP99-0659 1 slope unit NONE'       transpc           15.000
             '2003 WWYP99-0659 1 slope unit NONE'      xbearing         161.8415
             '2003 WWYP99-0659 1 slope unit NONE'          sinu          1.66858
             '2003 WWYP99-0659 1 slope unit NONE' pctClinometer                0
     '2003 WWYP99-0659 1 missing CM subsightings'  xslope_field      1.026666667
     '2003 WWYP99-0659 1 missing CM subsightings'        xslope      1.026666667
     '2003 WWYP99-0659 1 missing CM subsightings'        vslope      0.648035969
     '2003 WWYP99-0659 1 missing CM subsightings'          nslp               10
     '2003 WWYP99-0659 1 missing CM subsightings'       transpc           15.000
     '2003 WWYP99-0659 1 missing CM subsightings'      xbearing         161.8415
     '2003 WWYP99-0659 1 missing CM subsightings'          sinu          1.66858
     '2003 WWYP99-0659 1 missing CM subsightings' pctClinometer                0
                 '2000 WAZP99-0569 1 no incremnt'  xslope_field          10.8300
                 '2000 WAZP99-0569 1 no incremnt'        xslope          10.8300
                 '2000 WAZP99-0569 1 no incremnt'        vslope           3.5006
                 '2000 WAZP99-0569 1 no incremnt'          nslp               10
                 '2000 WAZP99-0569 1 no incremnt'       transpc               NA
                 '2000 WAZP99-0569 1 no incremnt'      xbearing               NA
                 '2000 WAZP99-0569 1 no incremnt'          sinu               NA
                 '2000 WAZP99-0569 1 no incremnt' pctClinometer                0
                   '2000 WAZP99-0569 1 no slopes'  xslope_field               NA
                   '2000 WAZP99-0569 1 no slopes'        xslope               NA
                   '2000 WAZP99-0569 1 no slopes'        vslope               NA
                   '2000 WAZP99-0569 1 no slopes'          nslp                0
                   '2000 WAZP99-0569 1 no slopes'       transpc           15.000
                   '2000 WAZP99-0569 1 no slopes'      xbearing         42.43476
                   '2000 WAZP99-0569 1 no slopes'          sinu           1.1251
                   '2000 WAZP99-0569 1 no slopes' pctClinometer                0
               '2000 WAZP99-0569 1 only 2 slopes'  xslope_field             5.99
               '2000 WAZP99-0569 1 only 2 slopes'        xslope             5.99
               '2000 WAZP99-0569 1 only 2 slopes'        vslope 5.67099638511611
               '2000 WAZP99-0569 1 only 2 slopes'          nslp                2
               '2000 WAZP99-0569 1 only 2 slopes'       transpc           15.000
               '2000 WAZP99-0569 1 only 2 slopes'      xbearing         42.43476
               '2000 WAZP99-0569 1 only 2 slopes'          sinu           1.1251
               '2000 WAZP99-0569 1 only 2 slopes' pctClinometer                0
'2000 WAZP99-0569 1 missing PERCENT subsightings'  xslope_field            7.554
'2000 WAZP99-0569 1 missing PERCENT subsightings'        xslope            7.554
'2000 WAZP99-0569 1 missing PERCENT subsightings'        vslope      4.236571465
'2000 WAZP99-0569 1 missing PERCENT subsightings'          nslp               10
'2000 WAZP99-0569 1 missing PERCENT subsightings'       transpc           15.000
'2000 WAZP99-0569 1 missing PERCENT subsightings'      xbearing         42.43476
'2000 WAZP99-0569 1 missing PERCENT subsightings'          sinu           1.1251
'2000 WAZP99-0569 1 missing PERCENT subsightings' pctClinometer                0
                             '2000 WIDP99-0556 1'  xslope_field          0.18000
                             '2000 WIDP99-0556 1'        xslope          0.18000
                             '2000 WIDP99-0556 1'        vslope          0.04216
                             '2000 WIDP99-0556 1'          nslp               10
                             '2000 WIDP99-0556 1'       transpc           600.00
                             '2000 WIDP99-0556 1'      xbearing           59.395
                             '2000 WIDP99-0556 1'          sinu          1.23583
                             '2000 WIDP99-0556 1' pctClinometer                0
           '2000 WIDP99-0556 1 with slopes in cm'  xslope_field          0.18000
           '2000 WIDP99-0556 1 with slopes in cm'        xslope          0.18000
           '2000 WIDP99-0556 1 with slopes in cm'        vslope          0.04216
           '2000 WIDP99-0556 1 with slopes in cm'          nslp               10
           '2000 WIDP99-0556 1 with slopes in cm'       transpc           600.00
           '2000 WIDP99-0556 1 with slopes in cm'      xbearing           59.395
           '2000 WIDP99-0556 1 with slopes in cm'          sinu          1.23583
           '2000 WIDP99-0556 1 with slopes in cm' pctClinometer                0
              '2000 WIDP99-0556 1 with NA slopes'  xslope_field               NA
              '2000 WIDP99-0556 1 with NA slopes'        xslope               NA
              '2000 WIDP99-0556 1 with NA slopes'        vslope               NA
              '2000 WIDP99-0556 1 with NA slopes'          nslp                0
              '2000 WIDP99-0556 1 with NA slopes'       transpc           600.00
              '2000 WIDP99-0556 1 with NA slopes'      xbearing           59.395
              '2000 WIDP99-0556 1 with NA slopes'          sinu          1.23583
              '2000 WIDP99-0556 1 with NA slopes' pctClinometer                0
          '2000 WIDP99-0556 1 with absent slopes'  xslope_field               NA
          '2000 WIDP99-0556 1 with absent slopes'        xslope               NA
          '2000 WIDP99-0556 1 with absent slopes'        vslope               NA
          '2000 WIDP99-0556 1 with absent slopes'          nslp                0
          '2000 WIDP99-0556 1 with absent slopes'       transpc           600.00
          '2000 WIDP99-0556 1 with absent slopes'      xbearing           59.395
          '2000 WIDP99-0556 1 with absent slopes'          sinu          1.23583
          '2000 WIDP99-0556 1 with absent slopes' pctClinometer                0
              '2000 WIDP99-0556 1 slope unit PCT'  xslope_field          0.18000
              '2000 WIDP99-0556 1 slope unit PCT'        xslope          0.18000
              '2000 WIDP99-0556 1 slope unit PCT'        vslope          0.04216
              '2000 WIDP99-0556 1 slope unit PCT'          nslp               10
              '2000 WIDP99-0556 1 slope unit PCT'       transpc           600.00
              '2000 WIDP99-0556 1 slope unit PCT'      xbearing           59.395
              '2000 WIDP99-0556 1 slope unit PCT'          sinu          1.23583
              '2000 WIDP99-0556 1 slope unit PCT' pctClinometer                0
                             '2000 WSDP99-0531 1'  xslope_field          0.10000
                             '2000 WSDP99-0531 1'        xslope          0.10000
                             '2000 WSDP99-0531 1'        vslope          0.00000
                             '2000 WSDP99-0531 1'          nslp               10
                             '2000 WSDP99-0531 1'       transpc           400.00
                             '2000 WSDP99-0531 1'      xbearing           51.499
                             '2000 WSDP99-0531 1'          sinu          2.33470
                             '2000 WSDP99-0531 1' pctClinometer                0
                               '2014 CARO-1043 1'          nslp               10
                               '2014 CARO-1043 1' pctClinometer               50
                               '2014 CARO-1043 1'          sinu 1.01637308274482
                               '2014 CARO-1043 1'       transpc               16
                               '2014 CARO-1043 1'        vslope 1.26671121083958
                               '2014 CARO-1043 1'      xbearing 328.653856558643
                               '2014 CARO-1043 1'        xslope          0.50625
                               '2014 CARO-1043 1'  xslope_field          0.50625
                                '2015 AKBB-018 1'          nslp               10
                                '2015 AKBB-018 1' pctClinometer               80
                                '2015 AKBB-018 1'          sinu  1.0245505113752
                                '2015 AKBB-018 1'       transpc               15
                                '2015 AKBB-018 1'        vslope 1.83072541784822
                                '2015 AKBB-018 1'      xbearing 129.385874766173
                                '2015 AKBB-018 1'        xslope 3.72666666666667
                                '2015 AKBB-018 1'  xslope_field 3.72666666666667
                         ")
    rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(rc)
}

nrsaSlopeBearingTest.makeExpectedResultsOLD <- function()
# Create dataframe of calculation results for unit test using field values.
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable SITEs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
  expected <- rbind(data.frame('SITE'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with clinometer'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with clinometer and low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('10.8300','10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 no subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 slope unit NONE'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 missing CM subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('10.8300','10.8300','3.5006','10',NA
                                         ,NA,NA,'0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c(NA, NA, NA,'0','15.000'
                                         ,'42.43476','1.1251',0
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('5.99','5.99','5.67099638511611','2','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 missing PERCENT subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('7.554','7.554','4.236571465','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with slopes in cm'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with NA slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c(NA,NA,NA,'0','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with absent slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c(NA,NA,NA,'0','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 slope unit PCT'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.10000','0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470','0'
                                         )
                              )
                   )
  expected$SITE <- as.character(expected$SITE)
  expected$METRIC <- as.character(expected$METRIC)
  expected$VALUE <- as.character(expected$VALUE)

  return(expected)
}

nrsaSlopeBearingTest.makeExpectedResultsWithGIS <- function()
# Create dataframe of calculation results for unit test using field values 
# AND map based values
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable SITEs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
    tc <- textConnection("                     SITE        METRIC             VALUE
                               '2000 WAZP99-0505 1'  xslope_field              5.75
                               '2000 WAZP99-0505 1'        xslope              5.75
                               '2000 WAZP99-0505 1'        vslope            2.9930
                               '2000 WAZP99-0505 1'          nslp                10
                               '2000 WAZP99-0505 1'       transpc              15.0
                               '2000 WAZP99-0505 1'      xbearing            16.442
                               '2000 WAZP99-0505 1'          sinu    1.067600000001
                               '2000 WAZP99-0505 1' pctClinometer                 0
                               '2000 WAZP99-0505 1'    xslope_map           12.3456
               '2000 WAZP99-0505 1 with clinometer'  xslope_field              5.75
               '2000 WAZP99-0505 1 with clinometer'        xslope              5.75
               '2000 WAZP99-0505 1 with clinometer'        vslope            2.9930
               '2000 WAZP99-0505 1 with clinometer'          nslp                10
               '2000 WAZP99-0505 1 with clinometer'       transpc              15.0
               '2000 WAZP99-0505 1 with clinometer'      xbearing            16.442
               '2000 WAZP99-0505 1 with clinometer'          sinu            1.0680
               '2000 WAZP99-0505 1 with clinometer' pctClinometer               100
                '2000 WAZP99-0505 1 with low slope'  xslope_field             0.575
                '2000 WAZP99-0505 1 with low slope'        xslope             0.575
                '2000 WAZP99-0505 1 with low slope'        vslope           0.29930
                '2000 WAZP99-0505 1 with low slope'          nslp                10
                '2000 WAZP99-0505 1 with low slope'       transpc              15.0
                '2000 WAZP99-0505 1 with low slope'      xbearing            16.442
                '2000 WAZP99-0505 1 with low slope'          sinu            1.0680
                '2000 WAZP99-0505 1 with low slope' pctClinometer                 0
 '2000 WAZP99-0505 1 with clinometer and low slope'  xslope_field             0.575
 '2000 WAZP99-0505 1 with clinometer and low slope'        xslope             0.575
 '2000 WAZP99-0505 1 with clinometer and low slope'        vslope           0.29930
 '2000 WAZP99-0505 1 with clinometer and low slope'          nslp                10
 '2000 WAZP99-0505 1 with clinometer and low slope'       transpc              15.0
 '2000 WAZP99-0505 1 with clinometer and low slope'      xbearing            16.442
 '2000 WAZP99-0505 1 with clinometer and low slope'          sinu            1.0680
 '2000 WAZP99-0505 1 with clinometer and low slope' pctClinometer               100
                               '2000 WAZP99-0569 1'  xslope_field           10.8300
                               '2000 WAZP99-0569 1'        xslope           10.8300
                               '2000 WAZP99-0569 1'        vslope            3.5006
                               '2000 WAZP99-0569 1'          nslp                10
                               '2000 WAZP99-0569 1'       transpc            15.000
                               '2000 WAZP99-0569 1'      xbearing          42.43476
                               '2000 WAZP99-0569 1'          sinu            1.1251
                               '2000 WAZP99-0569 1' pctClinometer                 0
                               '2000 WAZP99-0569 1'    xslope_map 10.82999999999999
                               '2003 WWYP99-0659 1'  xslope_field       1.026666667
                               '2003 WWYP99-0659 1'        xslope       1.026666667
                               '2003 WWYP99-0659 1'        vslope       0.648035969
                               '2003 WWYP99-0659 1'          nslp                10
                               '2003 WWYP99-0659 1'       transpc            15.000
                               '2003 WWYP99-0659 1'      xbearing          161.8415
                               '2003 WWYP99-0659 1'          sinu           1.66858
                               '2003 WWYP99-0659 1' pctClinometer                 0
               '2003 WWYP99-0659 1 no subsightings'  xslope_field       1.026666667
               '2003 WWYP99-0659 1 no subsightings'        xslope       1.026666667
               '2003 WWYP99-0659 1 no subsightings'        vslope       0.648035969
               '2003 WWYP99-0659 1 no subsightings'          nslp                10
               '2003 WWYP99-0659 1 no subsightings'       transpc            15.000
               '2003 WWYP99-0659 1 no subsightings'      xbearing          161.8415
               '2003 WWYP99-0659 1 no subsightings'          sinu           1.66858
               '2003 WWYP99-0659 1 no subsightings' pctClinometer                 0
               '2003 WWYP99-0659 1 slope unit NONE'  xslope_field       1.026666667
               '2003 WWYP99-0659 1 slope unit NONE'        xslope       1.026666667
               '2003 WWYP99-0659 1 slope unit NONE'        vslope       0.648035969
               '2003 WWYP99-0659 1 slope unit NONE'          nslp                10
               '2003 WWYP99-0659 1 slope unit NONE'       transpc            15.000
               '2003 WWYP99-0659 1 slope unit NONE'      xbearing          161.8415
               '2003 WWYP99-0659 1 slope unit NONE'          sinu           1.66858
               '2003 WWYP99-0659 1 slope unit NONE' pctClinometer                 0
       '2003 WWYP99-0659 1 missing CM subsightings'  xslope_field       1.026666667
       '2003 WWYP99-0659 1 missing CM subsightings'        xslope       1.026666667
       '2003 WWYP99-0659 1 missing CM subsightings'        vslope       0.648035969
       '2003 WWYP99-0659 1 missing CM subsightings'          nslp                10
       '2003 WWYP99-0659 1 missing CM subsightings'       transpc            15.000
       '2003 WWYP99-0659 1 missing CM subsightings'      xbearing          161.8415
       '2003 WWYP99-0659 1 missing CM subsightings'          sinu           1.66858
       '2003 WWYP99-0659 1 missing CM subsightings' pctClinometer                 0
                   '2000 WAZP99-0569 1 no incremnt'  xslope_field           10.8300
                   '2000 WAZP99-0569 1 no incremnt'        xslope           10.8300
                   '2000 WAZP99-0569 1 no incremnt'        vslope            3.5006
                   '2000 WAZP99-0569 1 no incremnt'          nslp                10
                   '2000 WAZP99-0569 1 no incremnt'       transpc                NA
                   '2000 WAZP99-0569 1 no incremnt'      xbearing                NA
                   '2000 WAZP99-0569 1 no incremnt'          sinu                NA
                   '2000 WAZP99-0569 1 no incremnt' pctClinometer                 0
                     '2000 WAZP99-0569 1 no slopes'  xslope_field                NA
                     '2000 WAZP99-0569 1 no slopes'        xslope                NA
                     '2000 WAZP99-0569 1 no slopes'        vslope                NA
                     '2000 WAZP99-0569 1 no slopes'          nslp                 0
                     '2000 WAZP99-0569 1 no slopes'       transpc            15.000
                     '2000 WAZP99-0569 1 no slopes'      xbearing          42.43476
                     '2000 WAZP99-0569 1 no slopes'          sinu            1.1251
                     '2000 WAZP99-0569 1 no slopes' pctClinometer                 0
                 '2000 WAZP99-0569 1 only 2 slopes'  xslope_field              5.99
                 '2000 WAZP99-0569 1 only 2 slopes'        xslope              5.99
                 '2000 WAZP99-0569 1 only 2 slopes'        vslope  5.67099638511611
                 '2000 WAZP99-0569 1 only 2 slopes'          nslp                 2
                 '2000 WAZP99-0569 1 only 2 slopes'       transpc            15.000
                 '2000 WAZP99-0569 1 only 2 slopes'      xbearing          42.43476
                 '2000 WAZP99-0569 1 only 2 slopes'          sinu            1.1251
                 '2000 WAZP99-0569 1 only 2 slopes' pctClinometer                 0
  '2000 WAZP99-0569 1 missing PERCENT subsightings'  xslope_field             7.554
  '2000 WAZP99-0569 1 missing PERCENT subsightings'        xslope             7.554
  '2000 WAZP99-0569 1 missing PERCENT subsightings'        vslope       4.236571465
  '2000 WAZP99-0569 1 missing PERCENT subsightings'          nslp                10
  '2000 WAZP99-0569 1 missing PERCENT subsightings'       transpc            15.000
  '2000 WAZP99-0569 1 missing PERCENT subsightings'      xbearing          42.43476
  '2000 WAZP99-0569 1 missing PERCENT subsightings'          sinu            1.1251
  '2000 WAZP99-0569 1 missing PERCENT subsightings' pctClinometer                 0
                               '2000 WIDP99-0556 1'  xslope_field           0.18000
                               '2000 WIDP99-0556 1'        xslope           0.18000
                               '2000 WIDP99-0556 1'        vslope           0.04216
                               '2000 WIDP99-0556 1'          nslp                10
                               '2000 WIDP99-0556 1'       transpc            600.00
                               '2000 WIDP99-0556 1'      xbearing            59.395
                               '2000 WIDP99-0556 1'          sinu           1.23583
                               '2000 WIDP99-0556 1' pctClinometer                 0
             '2000 WIDP99-0556 1 with slopes in cm'  xslope_field           0.18000
             '2000 WIDP99-0556 1 with slopes in cm'        xslope           0.18000
             '2000 WIDP99-0556 1 with slopes in cm'        vslope           0.04216
             '2000 WIDP99-0556 1 with slopes in cm'          nslp                10
             '2000 WIDP99-0556 1 with slopes in cm'       transpc            600.00
             '2000 WIDP99-0556 1 with slopes in cm'      xbearing            59.395
             '2000 WIDP99-0556 1 with slopes in cm'          sinu           1.23583
             '2000 WIDP99-0556 1 with slopes in cm' pctClinometer                 0
                '2000 WIDP99-0556 1 with NA slopes'  xslope_field                NA
                '2000 WIDP99-0556 1 with NA slopes'        xslope           0.01234
                '2000 WIDP99-0556 1 with NA slopes'        vslope                NA
                '2000 WIDP99-0556 1 with NA slopes'          nslp                 0
                '2000 WIDP99-0556 1 with NA slopes'       transpc            600.00
                '2000 WIDP99-0556 1 with NA slopes'      xbearing            59.395
                '2000 WIDP99-0556 1 with NA slopes'          sinu           1.23583
                '2000 WIDP99-0556 1 with NA slopes' pctClinometer                 0
                '2000 WIDP99-0556 1 with NA slopes'    xslope_map           0.01234
            '2000 WIDP99-0556 1 with absent slopes'  xslope_field                NA
            '2000 WIDP99-0556 1 with absent slopes'        xslope          0.567890
            '2000 WIDP99-0556 1 with absent slopes'        vslope                NA
            '2000 WIDP99-0556 1 with absent slopes'          nslp                 0
            '2000 WIDP99-0556 1 with absent slopes'       transpc            600.00
            '2000 WIDP99-0556 1 with absent slopes'      xbearing            59.395
            '2000 WIDP99-0556 1 with absent slopes'          sinu           1.23583
            '2000 WIDP99-0556 1 with absent slopes' pctClinometer                 0
            '2000 WIDP99-0556 1 with absent slopes'    xslope_map          0.567890
                '2000 WIDP99-0556 1 slope unit PCT'  xslope_field           0.18000
                '2000 WIDP99-0556 1 slope unit PCT'        xslope           0.18000
                '2000 WIDP99-0556 1 slope unit PCT'        vslope           0.04216
                '2000 WIDP99-0556 1 slope unit PCT'          nslp                10
                '2000 WIDP99-0556 1 slope unit PCT'       transpc            600.00
                '2000 WIDP99-0556 1 slope unit PCT'      xbearing            59.395
                '2000 WIDP99-0556 1 slope unit PCT'          sinu           1.23583
                '2000 WIDP99-0556 1 slope unit PCT' pctClinometer                 0
                               '2000 WSDP99-0531 1'  xslope_field           0.10000
                               '2000 WSDP99-0531 1'        xslope           0.10000
                               '2000 WSDP99-0531 1'        vslope           0.00000
                               '2000 WSDP99-0531 1'          nslp                10
                               '2000 WSDP99-0531 1'       transpc            400.00
                               '2000 WSDP99-0531 1'      xbearing            51.499
                               '2000 WSDP99-0531 1'          sinu           2.33470
                               '2000 WSDP99-0531 1' pctClinometer                 0
  'An extra reach not in the main expected results'        xslope   1.5999999999999
  'An extra reach not in the main expected results'    xslope_map   1.5999999999999
  'An extra reach not in the main expected results'          sinu       1.101010101
  'An extra reach not in the main expected results' pctClinometer           0.00000
                                 '2014 CARO-1043 1'          nslp               10
                                 '2014 CARO-1043 1' pctClinometer               50
                                 '2014 CARO-1043 1'          sinu 1.01637308274482
                                 '2014 CARO-1043 1'       transpc               16
                                 '2014 CARO-1043 1'        vslope 1.26671121083958
                                 '2014 CARO-1043 1'      xbearing 328.653856558643
                                 '2014 CARO-1043 1'        xslope          0.50625
                                 '2014 CARO-1043 1'  xslope_field          0.50625
                                  '2015 AKBB-018 1'          nslp               10
                                  '2015 AKBB-018 1' pctClinometer               80
                                  '2015 AKBB-018 1'          sinu  1.0245505113752
                                  '2015 AKBB-018 1'       transpc               15
                                  '2015 AKBB-018 1'        vslope 1.83072541784822
                                  '2015 AKBB-018 1'      xbearing 129.385874766173
                                  '2015 AKBB-018 1'        xslope 3.72666666666667
                                  '2015 AKBB-018 1'  xslope_field 3.72666666666667
                         ")
    rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(rc)
}


nrsaSlopeBearingTest.makeExpectedResultsWithGISOLD <- function()
# Create dataframe of calculation results for unit test using field values 
# AND map based values
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable SITEs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
  expected <- rbind(data.frame('SITE'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'VALUE'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.067600000001','0','12.3456'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with clinometer'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0505 1 with clinometer and low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'VALUE'=c('10.8300','10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251','0','10.82999999999999'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'    # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 no subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 slope unit NONE'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2003 WWYP99-0659 1 missing CM subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('1.026666667','1.026666667','0.648035969','10','15.000'  # Corrected xslope & xslope_field, was 0.588307961; vslope was 0.371371587
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('10.8300','10.8300','3.5006','10',NA
                                         ,NA,NA,'0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c(NA, NA, NA,'0','15.000'
                                         ,'42.43476','1.1251',0
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('5.99','5.99','5.67099638511611','2','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WAZP99-0569 1 missing PERCENT subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('7.554','7.554','4.236571465','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with slopes in cm'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with NA slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'VALUE'=c(NA,'0.01234',NA,'0','600.00'
                                         ,'59.395','1.23583','0','0.01234'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 with absent slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'VALUE'=c(NA,'0.567890',NA,'0','600.00'
                                         ,'59.395','1.23583','0','0.567890'
                                         )
                              )
                   ,data.frame('SITE'='2000 WIDP99-0556 1 slope unit PCT'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('SITE'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'VALUE'=c('0.10000','0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470','0'
                                         )
                              )
                     ,data.frame('SITE'='An extra reach not in the main expected results'
#                                 ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
#                                            ,'xbearing','sinu','pctClinometer'
#                                            )
#                                 ,'VALUE'=c('0.00000','1.5999999999999','1.5999999999999','0.00000','0.00000'
#                                           ,'0.00000','1.101010101','0'
#                                           )
                                ,'METRIC'=c('xslope','xslope_map','sinu','pctClinometer')
                                ,'VALUE'=c('1.5999999999999','1.5999999999999','1.101010101','0.00000')
                                ,stringsAsFactors=FALSE
                                )
                   )
  expected$SITE <- as.character(expected$SITE)
  expected$METRIC <- as.character(expected$METRIC)
  expected$VALUE <- as.character(expected$VALUE)

  return(expected)
}


nrsaSlopeBearingTest.makeGisCalcs <- function()
# Creates a dataframe of fake GIS calculations of XSLOPE and SINU.
{
    # gisCalcs <- rbind(data.frame('SITE'='2000 WAZP99-0505 1'
    #                             ,'METRIC'=c('xslope','sinu')
    #                             ,'VALUE'=c('12.3456','1.067600000001')
    #                             ,stringsAsFactors=FALSE
    #                             )
    #                  ,data.frame('SITE'='2000 WAZP99-0569 1'
    #                             ,'METRIC'=c('xslope')
    #                             ,'VALUE'=c('10.82999999999999')
    #                             ,stringsAsFactors=FALSE
    #                             )
    #                  ,data.frame('SITE'='2000 WIDP99-0556 1 with NA slopes'
    #                             ,'METRIC'='xslope'
    #                             ,VALUE='0.01234'
    #                             ,stringsAsFactors=FALSE
    #                             )
    #                  ,data.frame('SITE'='2000 WIDP99-0556 1 with absent slopes'
    #                             ,'METRIC'='xslope'
    #                             ,VALUE='0.567890'
    #                             ,stringsAsFactors=FALSE
    #                             )
    #                  ,data.frame('SITE'='An extra reach not in the main expected results'
    #                             ,'METRIC'=c('xslope','sinu')
    #                             ,'VALUE'=c('1.5999999999999','1.101010101')
    #                             ,stringsAsFactors=FALSE
    #                             )
    #                  )
    tc <- textConnection("SITE METRIC VALUE
                          '2000 WAZP99-0505 1'                              xslope           12.3456
                          '2000 WAZP99-0505 1'                              sinu      1.067600000001
                          '2000 WAZP99-0569 1'                              xslope 10.82999999999999
                          '2000 WIDP99-0556 1 with NA slopes'               xslope           0.01234
                          '2000 WIDP99-0556 1 with absent slopes'           xslope          0.567890
                          'An extra reach not in the main expected results' xslope   1.5999999999999
                          'An extra reach not in the main expected results'   sinu       1.101010101
                         ")
    gisCalcs <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    close(tc)
    return(gisCalcs)
}

# end of file