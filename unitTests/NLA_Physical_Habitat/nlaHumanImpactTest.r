# nlaHumanImpact.r
# RUnit tests



nlaHumanImpactTest <- function()
# unit test for nlaHumanImpact
# The complex testing here is due to the precision differences between
# calculated and retrieved values, requiring separate tests for presence (names)
# type and value of the metrics.
{
	nlaHumanImpactTest.2007()
	nlaHumanImpactTest.withDrawDown()
	nlaHumanImpactTest.withDrawDownAndFillin()
}



nlaHumanImpactTest.2007 <- function()
# Tests calculation with 2007 data
{
	testData <- nlaHumanImpactTest.createTestData2007()
	expected <- nlaHumanImpactTest.expectedResults2007()
	actual <- nlaHumanImpact(buildings =     testData %>% subset(CLASS=='HI_BUILDINGS') %>% select(SITE, STATION, VALUE)
                            ,buildings_dd =  testData %>% subset(CLASS=='HI_BUILDINGS_DD') %>% select(SITE, STATION, VALUE)
                            ,commercial =    testData %>% subset(CLASS=='HI_COMMERCIAL') %>% select(SITE, STATION, VALUE)
                            ,commercial_dd = testData %>% subset(CLASS=='HI_COMMERCIAL_DD') %>% select(SITE, STATION, VALUE)
                            ,crops =         testData %>% subset(CLASS=='HI_CROPS') %>% select(SITE, STATION, VALUE)
                            ,crops_dd =      testData %>% subset(CLASS=='HI_CROPS_DD') %>% select(SITE, STATION, VALUE)
                            ,docks =         testData %>% subset(CLASS=='HI_DOCKS') %>% select(SITE, STATION, VALUE)
                            ,docks_dd =      testData %>% subset(CLASS=='HI_DOCKS_DD') %>% select(SITE, STATION, VALUE)
                            ,landfill =      testData %>% subset(CLASS=='HI_LANDFILL') %>% select(SITE, STATION, VALUE)
                            ,landfill_dd =   testData %>% subset(CLASS=='HI_LANDFILL_DD') %>% select(SITE, STATION, VALUE)
                            ,lawn =          testData %>% subset(CLASS=='HI_LAWN') %>% select(SITE, STATION, VALUE)
                            ,lawn_dd =       testData %>% subset(CLASS=='HI_LAWN_DD') %>% select(SITE, STATION, VALUE)
                            ,orchard =       testData %>% subset(CLASS=='HI_ORCHARD') %>% select(SITE, STATION, VALUE)
                            ,orchard_dd =    testData %>% subset(CLASS=='HI_ORCHARD_DD') %>% select(SITE, STATION, VALUE)
                            ,other =         testData %>% subset(CLASS=='HI_OTHER') %>% select(SITE, STATION, VALUE)
                            ,other_dd =      testData %>% subset(CLASS=='HI_OTHER_DD') %>% select(SITE, STATION, VALUE)
                            ,park =          testData %>% subset(CLASS=='HI_PARK') %>% select(SITE, STATION, VALUE)
                            ,park_dd =       testData %>% subset(CLASS=='HI_PARK_DD') %>% select(SITE, STATION, VALUE)
                            ,pasture =       testData %>% subset(CLASS=='HI_PASTURE') %>% select(SITE, STATION, VALUE)
                            ,pasture_dd =    testData %>% subset(CLASS=='HI_PASTURE_DD') %>% select(SITE, STATION, VALUE)
                            ,powerlines =    testData %>% subset(CLASS=='HI_POWERLINES') %>% select(SITE, STATION, VALUE)
                            ,powerlines_dd = testData %>% subset(CLASS=='HI_POWERLINES_DD') %>% select(SITE, STATION, VALUE)
                            ,roads =         testData %>% subset(CLASS=='HI_ROADS') %>% select(SITE, STATION, VALUE)
                            ,roads_dd =      testData %>% subset(CLASS=='HI_ROADS_DD') %>% select(SITE, STATION, VALUE)
                            ,walls =         testData %>% subset(CLASS=='HI_WALLS') %>% select(SITE, STATION, VALUE)
                            ,walls_dd =      testData %>% subset(CLASS=='HI_WALLS_DD') %>% select(SITE, STATION, VALUE)
                            ,drawdown =      NULL#testData %>% subset(CLASS=='DRAWDOWN') %>% select(SITE, STATION, VALUE)
                            ,horizontalDistance_dd = NULL#testData %>% subset(CLASS=='HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
	                        ,data2007=TRUE
	                        ,fillinDrawdown=FALSE
	                        )

	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns in 2007")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics in 2007")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics in 2007")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-15)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with 2007-like data")
}



nlaHumanImpactTest.withDrawDown <- function()
# Tests calculation with drawdown data, but do NOT fill in drawdown values
{
	testData <- nlaHumanImpactTest.createTestDataWithDrawDown()
	expected <- nlaHumanImpactTest.expectedResultsWithDrawDownNoFillin()
	expected$VALUE <- as.numeric(expected$VALUE)
	actual <- nlaHumanImpact(buildings =     testData %>% subset(CLASS=='HI_BUILDINGS') %>% select(SITE, STATION, VALUE)
                            ,buildings_dd =  testData %>% subset(CLASS=='HI_BUILDINGS_DD') %>% select(SITE, STATION, VALUE)
                            ,commercial =    testData %>% subset(CLASS=='HI_COMMERCIAL') %>% select(SITE, STATION, VALUE)
                            ,commercial_dd = testData %>% subset(CLASS=='HI_COMMERCIAL_DD') %>% select(SITE, STATION, VALUE)
                            ,crops =         testData %>% subset(CLASS=='HI_CROPS') %>% select(SITE, STATION, VALUE)
                            ,crops_dd =      testData %>% subset(CLASS=='HI_CROPS_DD') %>% select(SITE, STATION, VALUE)
                            ,docks =         testData %>% subset(CLASS=='HI_DOCKS') %>% select(SITE, STATION, VALUE)
                            ,docks_dd =      testData %>% subset(CLASS=='HI_DOCKS_DD') %>% select(SITE, STATION, VALUE)
                            ,landfill =      testData %>% subset(CLASS=='HI_LANDFILL') %>% select(SITE, STATION, VALUE)
                            ,landfill_dd =   testData %>% subset(CLASS=='HI_LANDFILL_DD') %>% select(SITE, STATION, VALUE)
                            ,lawn =          testData %>% subset(CLASS=='HI_LAWN') %>% select(SITE, STATION, VALUE)
                            ,lawn_dd =       testData %>% subset(CLASS=='HI_LAWN_DD') %>% select(SITE, STATION, VALUE)
                            ,orchard =       testData %>% subset(CLASS=='HI_ORCHARD') %>% select(SITE, STATION, VALUE)
                            ,orchard_dd =    testData %>% subset(CLASS=='HI_ORCHARD_DD') %>% select(SITE, STATION, VALUE)
                            ,other =         testData %>% subset(CLASS=='HI_OTHER') %>% select(SITE, STATION, VALUE)
                            ,other_dd =      testData %>% subset(CLASS=='HI_OTHER_DD') %>% select(SITE, STATION, VALUE)
                            ,park =          testData %>% subset(CLASS=='HI_PARK') %>% select(SITE, STATION, VALUE)
                            ,park_dd =       testData %>% subset(CLASS=='HI_PARK_DD') %>% select(SITE, STATION, VALUE)
                            ,pasture =       testData %>% subset(CLASS=='HI_PASTURE') %>% select(SITE, STATION, VALUE)
                            ,pasture_dd =    testData %>% subset(CLASS=='HI_PASTURE_DD') %>% select(SITE, STATION, VALUE)
                            ,powerlines =    testData %>% subset(CLASS=='HI_POWERLINES') %>% select(SITE, STATION, VALUE)
                            ,powerlines_dd = testData %>% subset(CLASS=='HI_POWERLINES_DD') %>% select(SITE, STATION, VALUE)
                            ,roads =         testData %>% subset(CLASS=='HI_ROADS') %>% select(SITE, STATION, VALUE)
                            ,roads_dd =      testData %>% subset(CLASS=='HI_ROADS_DD') %>% select(SITE, STATION, VALUE)
                            ,walls =         testData %>% subset(CLASS=='HI_WALLS') %>% select(SITE, STATION, VALUE)
                            ,walls_dd =      testData %>% subset(CLASS=='HI_WALLS_DD') %>% select(SITE, STATION, VALUE)
                            ,drawdown =      testData %>% subset(CLASS=='DRAWDOWN') %>% select(SITE, STATION, VALUE)
                            ,horizontalDistance_dd = testData %>% subset(CLASS=='HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
	                        ,data2007=FALSE
                            ,fillinDrawdown=FALSE
                            )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawDown")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-10)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawdown")
}


nlaHumanImpactTest.withDrawDownAndFillin <- function()
# Tests calculation with drawdown data, and filling in drawdown values
{
	testData <- nlaHumanImpactTest.createTestDataWithDrawDown()
	expected <- nlaHumanImpactTest.expectedResultsWithDrawDown()
	expected$VALUE <- as.numeric(expected$VALUE)
	actual <- nlaHumanImpact(buildings =     testData %>% subset(CLASS=='HI_BUILDINGS') %>% select(SITE, STATION, VALUE)
                            ,buildings_dd =  testData %>% subset(CLASS=='HI_BUILDINGS_DD') %>% select(SITE, STATION, VALUE)
                            ,commercial =    testData %>% subset(CLASS=='HI_COMMERCIAL') %>% select(SITE, STATION, VALUE)
                            ,commercial_dd = testData %>% subset(CLASS=='HI_COMMERCIAL_DD') %>% select(SITE, STATION, VALUE)
                            ,crops =         testData %>% subset(CLASS=='HI_CROPS') %>% select(SITE, STATION, VALUE)
                            ,crops_dd =      testData %>% subset(CLASS=='HI_CROPS_DD') %>% select(SITE, STATION, VALUE)
                            ,docks =         testData %>% subset(CLASS=='HI_DOCKS') %>% select(SITE, STATION, VALUE)
                            ,docks_dd =      testData %>% subset(CLASS=='HI_DOCKS_DD') %>% select(SITE, STATION, VALUE)
                            ,landfill =      testData %>% subset(CLASS=='HI_LANDFILL') %>% select(SITE, STATION, VALUE)
                            ,landfill_dd =   testData %>% subset(CLASS=='HI_LANDFILL_DD') %>% select(SITE, STATION, VALUE)
                            ,lawn =          testData %>% subset(CLASS=='HI_LAWN') %>% select(SITE, STATION, VALUE)
                            ,lawn_dd =       testData %>% subset(CLASS=='HI_LAWN_DD') %>% select(SITE, STATION, VALUE)
                            ,orchard =       testData %>% subset(CLASS=='HI_ORCHARD') %>% select(SITE, STATION, VALUE)
                            ,orchard_dd =    testData %>% subset(CLASS=='HI_ORCHARD_DD') %>% select(SITE, STATION, VALUE)
                            ,other =         testData %>% subset(CLASS=='HI_OTHER') %>% select(SITE, STATION, VALUE)
                            ,other_dd =      testData %>% subset(CLASS=='HI_OTHER_DD') %>% select(SITE, STATION, VALUE)
                            ,park =          testData %>% subset(CLASS=='HI_PARK') %>% select(SITE, STATION, VALUE)
                            ,park_dd =       testData %>% subset(CLASS=='HI_PARK_DD') %>% select(SITE, STATION, VALUE)
                            ,pasture =       testData %>% subset(CLASS=='HI_PASTURE') %>% select(SITE, STATION, VALUE)
                            ,pasture_dd =    testData %>% subset(CLASS=='HI_PASTURE_DD') %>% select(SITE, STATION, VALUE)
                            ,powerlines =    testData %>% subset(CLASS=='HI_POWERLINES') %>% select(SITE, STATION, VALUE)
                            ,powerlines_dd = testData %>% subset(CLASS=='HI_POWERLINES_DD') %>% select(SITE, STATION, VALUE)
                            ,roads =         testData %>% subset(CLASS=='HI_ROADS') %>% select(SITE, STATION, VALUE)
                            ,roads_dd =      testData %>% subset(CLASS=='HI_ROADS_DD') %>% select(SITE, STATION, VALUE)
                            ,walls =         testData %>% subset(CLASS=='HI_WALLS') %>% select(SITE, STATION, VALUE)
                            ,walls_dd =      testData %>% subset(CLASS=='HI_WALLS_DD') %>% select(SITE, STATION, VALUE)
                            ,drawdown =      testData %>% subset(CLASS=='DRAWDOWN') %>% select(SITE, STATION, VALUE)
                            ,horizontalDistance_dd = testData %>% subset(CLASS=='HORIZ_DIST_DD') %>% select(SITE, STATION, VALUE)
	                        ,data2007=FALSE
                            ,fillinDrawdown=TRUE
                            )
	
	checkEquals(sort(names(expected)), sort(names(actual)), "Incorrect naming of columns with drawDown and DD fill-in")
	checkEquals(sort(unique(expected$METRIC)), sort(unique(actual$METRIC)), "Incorrect naming of metrics with drawDown and DD fill-in")
	
	expectedTypes <- unlist(lapply(expected, typeof))[names(expected)]
	actualTypes <- unlist(lapply(actual, typeof))[names(expected)]
	checkEquals(expectedTypes, actualTypes, "Incorrect typing of metrics with drawDown and DD fill-in")
	
	diff <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-10)
	checkTrue(is.null(diff), "Incorrect calculation of metrics with drawdown and DD fill-in")
}


nlaHumanImpactTest.createTestData2007 <- function()
# This data is based on NLA2007 data for the following VISIT_ID:
#	7784	data occurs only for station J
#	8069	incomplete data at all stations, also using relocated station from E to Z
#	8078	full complement of data using relocated station from I to S
#   8119	full complement of data in stations A-J, plus D2
#	8258	full complement of data in stations A-J, plus K, L
#   8541	incomplete data, partial data at station D
#   8902	full complement of data in stations A-J (as expected)
# 	8903	incomplete data, only at stations B,C,I,J,K
#hi07 <- dbFetch(nla07, 'tblRESULTS', where="PARAMETER in ('HI_BUILDINGS', 'HI_COMMERCIAL','HI_CROPS', 'HI_DOCKS', 'HI_LANDFILL', 'HI_LAWN', 'HI_ORCHARD', 'HI_PARK', 'HI_PASTURE','HI_POWERLINES', 'HI_ROADS', 'HI_WALLS')")
#tt<-subset(hi07, VISIT_ID %in% c(7784,8069,8078,8119,8258,8541,8902,8903), select=c(VISIT_ID,STATION_NO,PARAMETER,RESULT,UNITS))
#tt <- tt[order(tt$VISIT_ID,tt$STATION_NO,tt$PARAMETER),]	
{
	tc <- textConnection("   SITE      STATION     CLASS VALUE UNITS
							7784          J  HI_BUILDINGS      0 X         
							7784          J HI_COMMERCIAL      0 X         
							7784          J      HI_CROPS      0 X         
							7784          J      HI_DOCKS      0 X         
							7784          J   HI_LANDFILL      0 X         
							7784          J       HI_LAWN      0 X         
							7784          J    HI_ORCHARD      0 X         
							7784          J       HI_PARK      0 X         
							7784          J    HI_PASTURE      0 X         
							7784          J HI_POWERLINES      0 X         
							7784          J      HI_ROADS      0 X         
							7784          J      HI_WALLS      0 X         
							8069          A  HI_BUILDINGS      0 X         
							8069          A HI_COMMERCIAL      0 X         
							8069          A      HI_CROPS      0 X         
							8069          A      HI_DOCKS      P X         
							8069          A   HI_LANDFILL      0 X         
							8069          A       HI_LAWN      C X         
							8069          A    HI_ORCHARD      0 X         
							8069          A       HI_PARK      0 X         
							8069          A    HI_PASTURE      0 X         
							8069          A      HI_ROADS      P X         
							8069          A      HI_WALLS      0 X         
							8069          B  HI_BUILDINGS      0 X         
							8069          B HI_COMMERCIAL      0 X         
							8069          B      HI_CROPS      0 X         
							8069          B      HI_DOCKS      0 X         
							8069          B   HI_LANDFILL      0 X         
							8069          B       HI_LAWN      0 X         
							8069          B    HI_ORCHARD      0 X         
							8069          B       HI_PARK      C X         
							8069          B    HI_PASTURE      0 X         
							8069          B      HI_ROADS      P X         
							8069          B      HI_WALLS      0 X         
							8069          C  HI_BUILDINGS      0 X         
							8069          C HI_COMMERCIAL      0 X         
							8069          C      HI_CROPS      0 X         
							8069          C      HI_DOCKS      0 X         
							8069          C   HI_LANDFILL      0 X         
							8069          C       HI_LAWN      0 X         
							8069          C    HI_ORCHARD      0 X         
							8069          C       HI_PARK      C X         
							8069          C    HI_PASTURE      0 X         
							8069          C      HI_ROADS      P X         
							8069          C      HI_WALLS      0 X         
							8069          D  HI_BUILDINGS      P X         
							8069          D HI_COMMERCIAL      0 X         
							8069          D      HI_CROPS      0 X         
							8069          D      HI_DOCKS      P X         
							8069          D   HI_LANDFILL      0 X         
							8069          D       HI_LAWN      C X         
							8069          D    HI_ORCHARD      0 X         
							8069          D       HI_PARK      0 X         
							8069          D    HI_PASTURE      0 X         
							8069          D      HI_ROADS      P X         
							8069          D      HI_WALLS      C X         
							8069          F  HI_BUILDINGS      0 X         
							8069          F HI_COMMERCIAL      0 X         
							8069          F      HI_CROPS      0 X         
							8069          F      HI_DOCKS      0 X         
							8069          F   HI_LANDFILL      0 X         
							8069          F       HI_LAWN      0 X         
							8069          F    HI_ORCHARD      0 X         
							8069          F       HI_PARK      0 X         
							8069          F    HI_PASTURE      0 X         
							8069          F      HI_ROADS      P X         
							8069          F      HI_WALLS      0 X         
							8069          G  HI_BUILDINGS      P X         
							8069          G HI_COMMERCIAL      0 X         
							8069          G      HI_CROPS      0 X         
							8069          G      HI_DOCKS      P X         
							8069          G   HI_LANDFILL      0 X         
							8069          G       HI_LAWN      C X         
							8069          G    HI_ORCHARD      0 X         
							8069          G       HI_PARK      0 X         
							8069          G    HI_PASTURE      0 X         
							8069          G HI_POWERLINES      0 X         
							8069          G      HI_ROADS      P X         
							8069          G      HI_WALLS      0 X         
							8069          H  HI_BUILDINGS      C X         
							8069          H HI_COMMERCIAL      0 X         
							8069          H      HI_CROPS      0 X         
							8069          H      HI_DOCKS      C X         
							8069          H   HI_LANDFILL      0 X         
							8069          H       HI_LAWN      0 X         
							8069          H    HI_ORCHARD      0 X         
							8069          H       HI_PARK      0 X         
							8069          H    HI_PASTURE      0 X         
							8069          H      HI_ROADS      P X         
							8069          H      HI_WALLS      0 X         
							8069          I  HI_BUILDINGS      P X         
							8069          I HI_COMMERCIAL      0 X         
							8069          I      HI_CROPS      0 X         
							8069          I      HI_DOCKS      C X         
							8069          I   HI_LANDFILL      0 X         
							8069          I       HI_LAWN      0 X         
							8069          I    HI_ORCHARD      0 X         
							8069          I       HI_PARK      0 X         
							8069          I    HI_PASTURE      0 X         
							8069          I      HI_ROADS      P X         
							8069          I      HI_WALLS      0 X         
							8069          J  HI_BUILDINGS      P X         
							8069          J HI_COMMERCIAL      0 X         
							8069          J      HI_CROPS      0 X         
							8069          J      HI_DOCKS      P X         
							8069          J   HI_LANDFILL      0 X         
							8069          J       HI_LAWN      0 X         
							8069          J    HI_ORCHARD      0 X         
							8069          J       HI_PARK      0 X         
							8069          J    HI_PASTURE      0 X         
							8069          J      HI_ROADS      P X         
							8069          J      HI_WALLS      0 X         
							8069          Z  HI_BUILDINGS      C X         
							8069          Z HI_COMMERCIAL      P X         
							8069          Z      HI_CROPS      0 X         
							8069          Z      HI_DOCKS      C X         
							8069          Z   HI_LANDFILL      C X         
							8069          Z       HI_LAWN      C X         
							8069          Z    HI_ORCHARD      0 X         
							8069          Z       HI_PARK      0 X         
							8069          Z    HI_PASTURE      0 X         
							8069          Z      HI_ROADS      P X         
							8069          Z      HI_WALLS      C X         
							8078          A  HI_BUILDINGS      0 X         
							8078          A HI_COMMERCIAL      0 X         
							8078          A      HI_CROPS      0 X         
							8078          A      HI_DOCKS      0 X         
							8078          A   HI_LANDFILL      0 X         
							8078          A       HI_LAWN      0 X         
							8078          A    HI_ORCHARD      0 X         
							8078          A       HI_PARK      0 X         
							8078          A    HI_PASTURE      0 X         
							8078          A HI_POWERLINES      0 X         
							8078          A      HI_ROADS      0 X         
							8078          A      HI_WALLS      0 X         
							8078          B  HI_BUILDINGS      0 X         
							8078          B HI_COMMERCIAL      0 X         
							8078          B      HI_CROPS      P X         
							8078          B      HI_DOCKS      0 X         
							8078          B   HI_LANDFILL      0 X         
							8078          B       HI_LAWN      0 X         
							8078          B    HI_ORCHARD      0 X         
							8078          B       HI_PARK      0 X         
							8078          B    HI_PASTURE      0 X         
							8078          B HI_POWERLINES      0 X         
							8078          B      HI_ROADS      0 X         
							8078          B      HI_WALLS      0 X         
							8078          C  HI_BUILDINGS      0 X         
							8078          C HI_COMMERCIAL      0 X         
							8078          C      HI_CROPS      0 X         
							8078          C      HI_DOCKS      0 X         
							8078          C   HI_LANDFILL      0 X         
							8078          C       HI_LAWN      0 X         
							8078          C    HI_ORCHARD      0 X         
							8078          C       HI_PARK      0 X         
							8078          C    HI_PASTURE      0 X         
							8078          C HI_POWERLINES      0 X         
							8078          C      HI_ROADS      0 X         
							8078          C      HI_WALLS      0 X         
							8078          D  HI_BUILDINGS      0 X         
							8078          D HI_COMMERCIAL      0 X         
							8078          D      HI_CROPS      0 X         
							8078          D      HI_DOCKS      0 X         
							8078          D   HI_LANDFILL      0 X         
							8078          D       HI_LAWN      0 X         
							8078          D    HI_ORCHARD      0 X         
							8078          D       HI_PARK      0 X         
							8078          D    HI_PASTURE      0 X         
							8078          D HI_POWERLINES      0 X         
							8078          D      HI_ROADS      P X         
							8078          D      HI_WALLS      0 X         
							8078          E  HI_BUILDINGS      0 X         
							8078          E HI_COMMERCIAL      0 X         
							8078          E      HI_CROPS      0 X         
							8078          E      HI_DOCKS      0 X         
							8078          E   HI_LANDFILL      0 X         
							8078          E       HI_LAWN      0 X         
							8078          E    HI_ORCHARD      0 X         
							8078          E       HI_PARK      0 X         
							8078          E    HI_PASTURE      0 X         
							8078          E HI_POWERLINES      0 X         
							8078          E      HI_ROADS      0 X         
							8078          E      HI_WALLS      0 X         
							8078          F  HI_BUILDINGS      0 X         
							8078          F HI_COMMERCIAL      0 X         
							8078          F      HI_CROPS      0 X         
							8078          F      HI_DOCKS      0 X         
							8078          F   HI_LANDFILL      0 X         
							8078          F       HI_LAWN      0 X         
							8078          F    HI_ORCHARD      0 X         
							8078          F       HI_PARK      0 X         
							8078          F    HI_PASTURE      0 X         
							8078          F HI_POWERLINES      0 X         
							8078          F      HI_ROADS      0 X         
							8078          F      HI_WALLS      0 X         
							8078          G  HI_BUILDINGS      0 X         
							8078          G HI_COMMERCIAL      0 X         
							8078          G      HI_CROPS      0 X         
							8078          G      HI_DOCKS      P X         
							8078          G   HI_LANDFILL      0 X         
							8078          G       HI_LAWN      P X         
							8078          G    HI_ORCHARD      0 X         
							8078          G       HI_PARK      0 X         
							8078          G    HI_PASTURE      0 X         
							8078          G HI_POWERLINES      0 X         
							8078          G      HI_ROADS      0 X         
							8078          G      HI_WALLS      0 X         
							8078          H  HI_BUILDINGS      0 X         
							8078          H HI_COMMERCIAL      0 X         
							8078          H      HI_CROPS      0 X         
							8078          H      HI_DOCKS      0 X         
							8078          H   HI_LANDFILL      0 X         
							8078          H       HI_LAWN      0 X         
							8078          H    HI_ORCHARD      0 X         
							8078          H       HI_PARK      0 X         
							8078          H    HI_PASTURE      0 X         
							8078          H HI_POWERLINES      0 X         
							8078          H      HI_ROADS      0 X         
							8078          H      HI_WALLS      0 X         
							8078          J  HI_BUILDINGS      0 X         
							8078          J HI_COMMERCIAL      0 X         
							8078          J      HI_CROPS      0 X         
							8078          J      HI_DOCKS      0 X         
							8078          J   HI_LANDFILL      0 X         
							8078          J       HI_LAWN      0 X         
							8078          J    HI_ORCHARD      0 X         
							8078          J       HI_PARK      0 X         
							8078          J    HI_PASTURE      0 X         
							8078          J HI_POWERLINES      0 X         
							8078          J      HI_ROADS      P X         
							8078          J      HI_WALLS      0 X         
							8078          S  HI_BUILDINGS      0 X         
							8078          S HI_COMMERCIAL      0 X         
							8078          S      HI_CROPS      0 X         
							8078          S      HI_DOCKS      0 X         
							8078          S   HI_LANDFILL      0 X         
							8078          S       HI_LAWN      0 X         
							8078          S    HI_ORCHARD      0 X         
							8078          S       HI_PARK      0 X         
							8078          S    HI_PASTURE      0 X    							
							8078          S HI_POWERLINES      0 X         
							8078          S      HI_ROADS      0 X         
							8078          S      HI_WALLS      0 X         
							8119          A  HI_BUILDINGS      0 X         
							8119          A HI_COMMERCIAL      0 X         
							8119          A      HI_CROPS      0 X         
							8119          A      HI_DOCKS      0 X         
							8119          A   HI_LANDFILL      0 X         
							8119          A       HI_LAWN      0 X         
							8119          A    HI_ORCHARD      0 X         
							8119          A       HI_PARK      0 X         
							8119          A    HI_PASTURE      0 X         
							8119          A HI_POWERLINES      0 X         
							8119          A      HI_ROADS      0 X         
							8119          A      HI_WALLS      0 X         
							8119          B  HI_BUILDINGS      0 X         
							8119          B HI_COMMERCIAL      0 X         
							8119          B      HI_CROPS      0 X         
							8119          B      HI_DOCKS      0 X         
							8119          B   HI_LANDFILL      0 X         
							8119          B       HI_LAWN      0 X         
							8119          B    HI_ORCHARD      0 X         
							8119          B       HI_PARK      0 X         
							8119          B    HI_PASTURE      0 X         
							8119          B HI_POWERLINES      0 X         
							8119          B      HI_ROADS      0 X         
							8119          B      HI_WALLS      0 X         
							8119          C  HI_BUILDINGS      0 X         
							8119          C HI_COMMERCIAL      0 X         
							8119          C      HI_CROPS      0 X         
							8119          C      HI_DOCKS      0 X         
							8119          C   HI_LANDFILL      0 X         
							8119          C       HI_LAWN      0 X         
							8119          C    HI_ORCHARD      0 X         
							8119          C       HI_PARK      0 X         
							8119          C    HI_PASTURE      0 X         
							8119          C HI_POWERLINES      0 X         
							8119          C      HI_ROADS      0 X         
							8119          C      HI_WALLS      0 X         
							8119          D  HI_BUILDINGS      P X         
							8119          D HI_COMMERCIAL      0 X         
							8119          D      HI_CROPS      0 X         
							8119          D      HI_DOCKS      0 X         
							8119          D   HI_LANDFILL      0 X         
							8119          D       HI_LAWN      0 X         
							8119          D    HI_ORCHARD      0 X         
							8119          D       HI_PARK      0 X         
							8119          D    HI_PASTURE      0 X         
							8119          D HI_POWERLINES      0 X         
							8119          D      HI_ROADS      P X         
							8119          D      HI_WALLS      0 X         
							8119         D2  HI_BUILDINGS      0 X         
							8119         D2 HI_COMMERCIAL      0 X         
							8119         D2      HI_CROPS      0 X         
							8119         D2      HI_DOCKS      0 X         
							8119         D2   HI_LANDFILL      0 X         
							8119         D2       HI_LAWN      0 X         
							8119         D2    HI_ORCHARD      0 X         
							8119         D2       HI_PARK      0 X         
							8119         D2    HI_PASTURE      0 X         
							8119         D2 HI_POWERLINES      0 X         
							8119         D2      HI_ROADS      0 X         
							8119         D2      HI_WALLS      0 X         
							8119          E  HI_BUILDINGS      P X         
							8119          E HI_COMMERCIAL      0 X         
							8119          E      HI_CROPS      0 X         
							8119          E      HI_DOCKS      0 X         
							8119          E   HI_LANDFILL      0 X         
							8119          E       HI_LAWN      P X         
							8119          E    HI_ORCHARD      0 X         
							8119          E       HI_PARK      0 X         
							8119          E    HI_PASTURE      0 X         
							8119          E HI_POWERLINES      0 X         
							8119          E      HI_ROADS      0 X         
							8119          E      HI_WALLS      0 X         
							8119          F  HI_BUILDINGS      0 X         
							8119          F HI_COMMERCIAL      0 X         
							8119          F      HI_CROPS      0 X         
							8119          F      HI_DOCKS      0 X         
							8119          F   HI_LANDFILL      0 X         
							8119          F       HI_LAWN      0 X         
							8119          F    HI_ORCHARD      0 X         
							8119          F       HI_PARK      0 X         
							8119          F    HI_PASTURE      0 X         
							8119          F HI_POWERLINES      0 X         
							8119          F      HI_ROADS      0 X         
							8119          F      HI_WALLS      0 X         
							8119          G  HI_BUILDINGS      0 X         
							8119          G HI_COMMERCIAL      0 X         
							8119          G      HI_CROPS      0 X         
							8119          G      HI_DOCKS      0 X         
							8119          G   HI_LANDFILL      0 X         
							8119          G       HI_LAWN      0 X         
							8119          G    HI_ORCHARD      0 X         
							8119          G       HI_PARK      0 X         
							8119          G    HI_PASTURE      0 X         
							8119          G HI_POWERLINES      0 X         
							8119          G      HI_ROADS      0 X         
							8119          G      HI_WALLS      0 X         
							8119          H  HI_BUILDINGS      0 X         
							8119          H HI_COMMERCIAL      0 X         
							8119          H      HI_CROPS      0 X         
							8119          H      HI_DOCKS      0 X         
							8119          H   HI_LANDFILL      0 X         
							8119          H       HI_LAWN      0 X         
							8119          H    HI_ORCHARD      0 X         
							8119          H       HI_PARK      0 X         
							8119          H    HI_PASTURE      0 X         
							8119          H HI_POWERLINES      0 X         
							8119          H      HI_ROADS      0 X         
							8119          H      HI_WALLS      0 X         
							8119          I  HI_BUILDINGS      0 X         
							8119          I HI_COMMERCIAL      0 X         
							8119          I      HI_CROPS      0 X         
							8119          I      HI_DOCKS      0 X         
							8119          I   HI_LANDFILL      C X         
							8119          I       HI_LAWN      0 X         
							8119          I    HI_ORCHARD      0 X         
							8119          I       HI_PARK      0 X         
							8119          I    HI_PASTURE      0 X         
							8119          I HI_POWERLINES      0 X         
							8119          I      HI_ROADS      0 X         
							8119          I      HI_WALLS      0 X         
							8119          J  HI_BUILDINGS      0 X         
							8119          J HI_COMMERCIAL      0 X         
							8119          J      HI_CROPS      0 X         
							8119          J      HI_DOCKS      0 X         
							8119          J   HI_LANDFILL      0 X         
							8119          J       HI_LAWN      0 X         
							8119          J    HI_ORCHARD      0 X         
							8119          J       HI_PARK      0 X         
							8119          J    HI_PASTURE      0 X         
							8119          J HI_POWERLINES      0 X         
							8119          J      HI_ROADS      0 X         
							8119          J      HI_WALLS      0 X         
							8258          A  HI_BUILDINGS      0 X         
							8258          A HI_COMMERCIAL      0 X         
							8258          A      HI_CROPS      0 X         
							8258          A      HI_DOCKS      0 X         
							8258          A   HI_LANDFILL      0 X         
							8258          A       HI_LAWN      0 X         
							8258          A    HI_ORCHARD      0 X         
							8258          A       HI_PARK      0 X         
							8258          A    HI_PASTURE      0 X         
							8258          A HI_POWERLINES      0 X         
							8258          A      HI_ROADS      0 X         
							8258          A      HI_WALLS      0 X         
							8258          B  HI_BUILDINGS      P X         
							8258          B HI_COMMERCIAL      0 X         
							8258          B      HI_CROPS      0 X         
							8258          B      HI_DOCKS      P X         
							8258          B   HI_LANDFILL      0 X         
							8258          B       HI_LAWN      P X         
							8258          B    HI_ORCHARD      0 X         
							8258          B       HI_PARK      0 X         
							8258          B    HI_PASTURE      0 X         
							8258          B HI_POWERLINES      0 X         
							8258          B      HI_ROADS      0 X         
							8258          B      HI_WALLS      0 X         
							8258          C  HI_BUILDINGS      0 X         
							8258          C HI_COMMERCIAL      0 X         
							8258          C      HI_CROPS      0 X         
							8258          C      HI_DOCKS      0 X         
							8258          C   HI_LANDFILL      0 X         
							8258          C       HI_LAWN      0 X         
							8258          C    HI_ORCHARD      0 X         
							8258          C       HI_PARK      0 X         
							8258          C    HI_PASTURE      0 X         
							8258          C HI_POWERLINES      0 X         
							8258          C      HI_ROADS      0 X         
							8258          C      HI_WALLS      0 X         
							8258          D  HI_BUILDINGS      P X         
							8258          D HI_COMMERCIAL      0 X         
							8258          D      HI_CROPS      0 X         
							8258          D      HI_DOCKS      C X         
							8258          D   HI_LANDFILL      0 X         
							8258          D       HI_LAWN      C X         
							8258          D    HI_ORCHARD      0 X         
							8258          D       HI_PARK      0 X         
							8258          D    HI_PASTURE      0 X         
							8258          D HI_POWERLINES      0 X         
							8258          D      HI_ROADS      0 X         
							8258          D      HI_WALLS      C X         
							8258          E  HI_BUILDINGS      P X         
							8258          E HI_COMMERCIAL      0 X         
							8258          E      HI_CROPS      0 X         
							8258          E      HI_DOCKS      C X         
							8258          E   HI_LANDFILL      0 X         
							8258          E       HI_LAWN      C X         
							8258          E    HI_ORCHARD      0 X         
							8258          E       HI_PARK      0 X         
							8258          E    HI_PASTURE      0 X         
							8258          E HI_POWERLINES      P X         
							8258          E      HI_ROADS      0 X         
							8258          E      HI_WALLS      P X         
							8258          F  HI_BUILDINGS      P X         
							8258          F HI_COMMERCIAL      0 X         
							8258          F      HI_CROPS      0 X         
							8258          F      HI_DOCKS      C X         
							8258          F   HI_LANDFILL      0 X         
							8258          F       HI_LAWN      C X         
							8258          F    HI_ORCHARD      0 X         
							8258          F       HI_PARK      0 X         
							8258          F    HI_PASTURE      0 X         
							8258          F HI_POWERLINES      0 X         
							8258          F      HI_ROADS      0 X         
							8258          F      HI_WALLS      P X         
							8258          G  HI_BUILDINGS      0 X         
							8258          G HI_COMMERCIAL      0 X         
							8258          G      HI_CROPS      0 X         
							8258          G      HI_DOCKS      0 X         
							8258          G   HI_LANDFILL      0 X         
							8258          G       HI_LAWN      0 X         
							8258          G    HI_ORCHARD      0 X         
							8258          G       HI_PARK      0 X         
							8258          G    HI_PASTURE      0 X         
							8258          G HI_POWERLINES      0 X         
							8258          G      HI_ROADS      0 X         
							8258          G      HI_WALLS      0 X         
							8258          H  HI_BUILDINGS      0 X         
							8258          H HI_COMMERCIAL      0 X         
							8258          H      HI_CROPS      0 X         
							8258          H      HI_DOCKS      0 X         
							8258          H   HI_LANDFILL      0 X         
							8258          H       HI_LAWN      0 X         
							8258          H    HI_ORCHARD      0 X         
							8258          H       HI_PARK      0 X         
							8258          H    HI_PASTURE      0 X         
							8258          H HI_POWERLINES      0 X         
							8258          H      HI_ROADS      0 X         
							8258          H      HI_WALLS      0 X         
							8258          I  HI_BUILDINGS      0 X         
							8258          I HI_COMMERCIAL      0 X         
							8258          I      HI_CROPS      0 X         
							8258          I      HI_DOCKS      0 X         
							8258          I   HI_LANDFILL      0 X         
							8258          I       HI_LAWN      0 X         
							8258          I    HI_ORCHARD      0 X         
							8258          I       HI_PARK      0 X         
							8258          I    HI_PASTURE      0 X         
							8258          I HI_POWERLINES      P X         
							8258          I      HI_ROADS      0 X         
							8258          I      HI_WALLS      0 X         
							8258          J  HI_BUILDINGS      0 X         
							8258          J HI_COMMERCIAL      0 X         
							8258          J      HI_CROPS      0 X         
							8258          J      HI_DOCKS      0 X         
							8258          J   HI_LANDFILL      0 X         
							8258          J       HI_LAWN      0 X         
							8258          J    HI_ORCHARD      0 X         
							8258          J       HI_PARK      0 X         
							8258          J    HI_PASTURE      0 X         
							8258          J HI_POWERLINES      0 X         
							8258          J      HI_ROADS      0 X         
							8258          J      HI_WALLS      0 X         
							8258          K  HI_BUILDINGS      0 X         
							8258          K HI_COMMERCIAL      0 X         
							8258          K      HI_CROPS      0 X         
							8258          K      HI_DOCKS      0 X         
							8258          K   HI_LANDFILL      0 X         
							8258          K       HI_LAWN      0 X         
							8258          K    HI_ORCHARD      0 X         
							8258          K       HI_PARK      0 X         
							8258          K    HI_PASTURE      0 X         
							8258          K HI_POWERLINES      0 X         
							8258          K      HI_ROADS      0 X         
							8258          K      HI_WALLS      0 X         
							8258          L  HI_BUILDINGS      P X         
							8258          L HI_COMMERCIAL      0 X         
							8258          L      HI_CROPS      0 X         
							8258          L      HI_DOCKS      P X         
							8258          L   HI_LANDFILL      0 X         
							8258          L       HI_LAWN      P X         
							8258          L    HI_ORCHARD      0 X         
							8258          L       HI_PARK      0 X         
							8258          L    HI_PASTURE      0 X         
							8258          L HI_POWERLINES      0 X         
							8258          L      HI_ROADS      0 X         
							8258          L      HI_WALLS      0 X         
							8541          A  HI_BUILDINGS      0 X         
							8541          A HI_COMMERCIAL      0 X         
							8541          A      HI_CROPS      0 X         
							8541          A      HI_DOCKS      0 X         
							8541          A   HI_LANDFILL      0 X         
							8541          A       HI_LAWN      0 X         
							8541          A    HI_ORCHARD      0 X         
							8541          A       HI_PARK      0 X         
							8541          A    HI_PASTURE      0 X         
							8541          A HI_POWERLINES      0 X         
							8541          A      HI_ROADS      0 X         
							8541          A      HI_WALLS      0 X         
							8541          B  HI_BUILDINGS      0 X         
							8541          B HI_COMMERCIAL      0 X         
							8541          B      HI_CROPS      0 X         
							8541          B      HI_DOCKS      0 X         
							8541          B   HI_LANDFILL      0 X         
							8541          B       HI_LAWN      0 X         
							8541          B    HI_ORCHARD      0 X         
							8541          B       HI_PARK      0 X         
							8541          B    HI_PASTURE      0 X         
							8541          B HI_POWERLINES      0 X         
							8541          B      HI_ROADS      0 X         
							8541          B      HI_WALLS      0 X         
							8541          C  HI_BUILDINGS      0 X         
							8541          C HI_COMMERCIAL      0 X         
							8541          C      HI_CROPS      0 X         
							8541          C      HI_DOCKS      0 X         
							8541          C   HI_LANDFILL      0 X         
							8541          C       HI_LAWN      0 X         
							8541          C    HI_ORCHARD      0 X         
							8541          C       HI_PARK      0 X         
							8541          C    HI_PASTURE      0 X         
							8541          C HI_POWERLINES      0 X         
							8541          C      HI_ROADS      0 X         
							8541          C      HI_WALLS      0 X         
							8541          D  HI_BUILDINGS      0 X         
							8541          D HI_COMMERCIAL      0 X         
							8541          D      HI_CROPS      0 X         
							8541          D      HI_DOCKS      0 X         
							8541          D       HI_LAWN      0 X         
							8541          D    HI_ORCHARD      0 X         
							8541          D       HI_PARK      0 X         
							8541          D    HI_PASTURE      0 X         
							8541          D HI_POWERLINES      0 X         
							8541          D      HI_ROADS      0 X         
							8541          D      HI_WALLS      0 X         
							8541          E  HI_BUILDINGS      0 X         
							8541          E HI_COMMERCIAL      0 X         
							8541          E      HI_CROPS      0 X         
							8541          E      HI_DOCKS      0 X         
							8541          E   HI_LANDFILL      0 X         
							8541          E       HI_LAWN      0 X         
							8541          E    HI_ORCHARD      0 X         
							8541          E       HI_PARK      0 X         
							8541          E    HI_PASTURE      0 X         
							8541          E HI_POWERLINES      0 X         
							8541          E      HI_ROADS      0 X         
							8541          E      HI_WALLS      0 X         
							8541          F  HI_BUILDINGS      0 X         
							8541          F HI_COMMERCIAL      0 X         
							8541          F      HI_CROPS      0 X         
							8541          F      HI_DOCKS      0 X         
							8541          F   HI_LANDFILL      0 X         
							8541          F       HI_LAWN      C X         
							8541          F    HI_ORCHARD      0 X         
							8541          F       HI_PARK      C X         
							8541          F    HI_PASTURE      0 X         
							8541          F HI_POWERLINES      0 X         
							8541          F      HI_ROADS      0 X         
							8541          F      HI_WALLS      C X         
							8541          G  HI_BUILDINGS      0 X         
							8541          G HI_COMMERCIAL      0 X         
							8541          G      HI_CROPS      0 X         
							8541          G      HI_DOCKS      0 X         
							8541          G   HI_LANDFILL      0 X         
							8541          G       HI_LAWN      0 X         
							8541          G    HI_ORCHARD      0 X         
							8541          G       HI_PARK      0 X         
							8541          G    HI_PASTURE      0 X         
							8541          G HI_POWERLINES      0 X         
							8541          G      HI_ROADS      0 X         
							8541          G      HI_WALLS      0 X         
							8541          H  HI_BUILDINGS      0 X         
							8541          H HI_COMMERCIAL      0 X         
							8541          H      HI_CROPS      0 X         
							8541          H      HI_DOCKS      0 X         
							8541          H   HI_LANDFILL      0 X         
							8541          H       HI_LAWN      0 X         
							8541          H    HI_ORCHARD      0 X         
							8541          H       HI_PARK      0 X         
							8541          H    HI_PASTURE      0 X         
							8541          H HI_POWERLINES      0 X         
							8541          H      HI_ROADS      0 X         
							8541          H      HI_WALLS      0 X         
							8541          I  HI_BUILDINGS      0 X         
							8541          I HI_COMMERCIAL      0 X         
							8541          I      HI_CROPS      0 X         
							8541          I      HI_DOCKS      0 X         
							8541          I   HI_LANDFILL      0 X         
							8541          I       HI_LAWN      0 X         
							8541          I    HI_ORCHARD      0 X         
							8541          I       HI_PARK      0 X         
							8541          I    HI_PASTURE      0 X         
							8541          I HI_POWERLINES      0 X         
							8541          I      HI_ROADS      0 X         
							8541          I      HI_WALLS      0 X         
							8541          J  HI_BUILDINGS      0 X         
							8541          J HI_COMMERCIAL      0 X         
							8541          J      HI_CROPS      0 X         
							8541          J      HI_DOCKS      0 X         
							8541          J   HI_LANDFILL      0 X         
							8541          J       HI_LAWN      0 X         
							8541          J    HI_ORCHARD      0 X         
							8541          J       HI_PARK      0 X         
							8541          J    HI_PASTURE      0 X         
							8541          J HI_POWERLINES      0 X         
							8541          J      HI_ROADS      0 X         
							8541          J      HI_WALLS      0 X         
							8902          A  HI_BUILDINGS      0 X         
							8902          A HI_COMMERCIAL      0 X         
							8902          A      HI_CROPS      0 X         
							8902          A      HI_DOCKS      P X         
							8902          A   HI_LANDFILL      0 X         
							8902          A       HI_LAWN      0 X         
							8902          A    HI_ORCHARD      0 X         
							8902          A       HI_PARK      0 X         
							8902          A    HI_PASTURE      0 X         
							8902          A HI_POWERLINES      0 X         
							8902          A      HI_ROADS      P X         
							8902          A      HI_WALLS      0 X         
							8902          B  HI_BUILDINGS      0 X         
							8902          B HI_COMMERCIAL      0 X         
							8902          B      HI_CROPS      0 X         
							8902          B      HI_DOCKS      0 X         
							8902          B   HI_LANDFILL      0 X         
							8902          B       HI_LAWN      0 X         
							8902          B    HI_ORCHARD      0 X         
							8902          B       HI_PARK      0 X         
							8902          B    HI_PASTURE      0 X         
							8902          B HI_POWERLINES      0 X         
							8902          B      HI_ROADS      0 X         
							8902          B      HI_WALLS      0 X         
							8902          C  HI_BUILDINGS      0 X         
							8902          C HI_COMMERCIAL      0 X         
							8902          C      HI_CROPS      0 X         
							8902          C      HI_DOCKS      0 X         
							8902          C   HI_LANDFILL      0 X         
							8902          C       HI_LAWN      0 X         
							8902          C    HI_ORCHARD      0 X         
							8902          C       HI_PARK      0 X         
							8902          C    HI_PASTURE      0 X         
							8902          C HI_POWERLINES      0 X         
							8902          C      HI_ROADS      0 X         
							8902          C      HI_WALLS      0 X         
							8902          D  HI_BUILDINGS      0 X         
							8902          D HI_COMMERCIAL      0 X         
							8902          D      HI_CROPS      0 X         
							8902          D      HI_DOCKS      0 X         
							8902          D   HI_LANDFILL      0 X         
							8902          D       HI_LAWN      0 X         
							8902          D    HI_ORCHARD      0 X         
							8902          D       HI_PARK      0 X         
							8902          D    HI_PASTURE      0 X         
							8902          D HI_POWERLINES      0 X         
							8902          D      HI_ROADS      0 X         
							8902          D      HI_WALLS      0 X         
							8902          E  HI_BUILDINGS      0 X         
							8902          E HI_COMMERCIAL      0 X         
							8902          E      HI_CROPS      0 X         
							8902          E      HI_DOCKS      0 X         
							8902          E   HI_LANDFILL      0 X         
							8902          E       HI_LAWN      0 X         
							8902          E    HI_ORCHARD      0 X         
							8902          E       HI_PARK      0 X         
							8902          E    HI_PASTURE      0 X         
							8902          E HI_POWERLINES      0 X         
							8902          E      HI_ROADS      0 X         
							8902          E      HI_WALLS      0 X         
							8902          F  HI_BUILDINGS      0 X         
							8902          F HI_COMMERCIAL      0 X         
							8902          F      HI_CROPS      0 X         
							8902          F      HI_DOCKS      0 X         
							8902          F   HI_LANDFILL      0 X         
							8902          F       HI_LAWN      0 X         
							8902          F    HI_ORCHARD      0 X         
							8902          F       HI_PARK      0 X         
							8902          F    HI_PASTURE      0 X         
							8902          F HI_POWERLINES      0 X         
							8902          F      HI_ROADS      0 X         
							8902          F      HI_WALLS      0 X         
							8902          G  HI_BUILDINGS      0 X         
							8902          G HI_COMMERCIAL      0 X         
							8902          G      HI_CROPS      0 X         
							8902          G      HI_DOCKS      0 X         
							8902          G   HI_LANDFILL      0 X         
							8902          G       HI_LAWN      0 X         
							8902          G    HI_ORCHARD      0 X         
							8902          G       HI_PARK      0 X         
							8902          G    HI_PASTURE      0 X         
							8902          G HI_POWERLINES      0 X         
							8902          G      HI_ROADS      0 X         
							8902          G      HI_WALLS      0 X         
							8902          H  HI_BUILDINGS      0 X         
							8902          H HI_COMMERCIAL      0 X         
							8902          H      HI_CROPS      0 X         
							8902          H      HI_DOCKS      0 X         
							8902          H   HI_LANDFILL      0 X         
							8902          H       HI_LAWN      0 X         
							8902          H    HI_ORCHARD      0 X         
							8902          H       HI_PARK      0 X         
							8902          H    HI_PASTURE      0 X         
							8902          H HI_POWERLINES      0 X         
							8902          H      HI_ROADS      0 X         
							8902          H      HI_WALLS      0 X         
							8902          I  HI_BUILDINGS      0 X         
							8902          I HI_COMMERCIAL      0 X         
							8902          I      HI_CROPS      0 X         
							8902          I      HI_DOCKS      0 X         
							8902          I   HI_LANDFILL      0 X         
							8902          I       HI_LAWN      0 X         
							8902          I    HI_ORCHARD      0 X         
							8902          I       HI_PARK      0 X         
							8902          I    HI_PASTURE      0 X         
							8902          I HI_POWERLINES      0 X         
							8902          I      HI_ROADS      0 X         
							8902          I      HI_WALLS      0 X         
							8902          J  HI_BUILDINGS      0 X         
							8902          J HI_COMMERCIAL      0 X         
							8902          J      HI_CROPS      0 X         
							8902          J      HI_DOCKS      0 X         
							8902          J   HI_LANDFILL      0 X         
							8902          J       HI_LAWN      0 X         
							8902          J    HI_ORCHARD      0 X         
							8902          J       HI_PARK      0 X         
							8902          J    HI_PASTURE      0 X         
							8902          J HI_POWERLINES      0 X         
							8902          J      HI_ROADS      0 X         
							8902          J      HI_WALLS      0 X         
							8903          A  HI_BUILDINGS      0 X         
							8903          A HI_COMMERCIAL      0 X         
							8903          A      HI_CROPS      0 X         
							8903          A      HI_DOCKS      0 X         
							8903          A   HI_LANDFILL      0 X         
							8903          A       HI_LAWN      0 X         
							8903          A    HI_ORCHARD      0 X         
							8903          A       HI_PARK      0 X         
							8903          A    HI_PASTURE      0 X         
							8903          A HI_POWERLINES      0 X         
							8903          A      HI_ROADS      0 X         
							8903          A      HI_WALLS      0 X         
							8903          B  HI_BUILDINGS      0 X         
							8903          B HI_COMMERCIAL      0 X         
							8903          B      HI_CROPS      0 X         
							8903          B      HI_DOCKS      0 X         
							8903          B   HI_LANDFILL      0 X         
							8903          B       HI_LAWN      0 X         
							8903          B    HI_ORCHARD      0 X         
							8903          B       HI_PARK      0 X         
							8903          B    HI_PASTURE      0 X         
							8903          B HI_POWERLINES      0 X         
							8903          B      HI_ROADS      0 X         
							8903          B      HI_WALLS      0 X         
							8903          C  HI_BUILDINGS      0 X         
							8903          C HI_COMMERCIAL      0 X         
							8903          C      HI_CROPS      0 X         
							8903          C      HI_DOCKS      0 X         
							8903          C   HI_LANDFILL      0 X         
							8903          C       HI_LAWN      0 X         
							8903          C    HI_ORCHARD      0 X         
							8903          C       HI_PARK      0 X         
							8903          C    HI_PASTURE      0 X         
							8903          C HI_POWERLINES      0 X         
							8903          C      HI_ROADS      0 X         
							8903          C      HI_WALLS      0 X         
							8903          D  HI_BUILDINGS      0 X         
							8903          D HI_COMMERCIAL      0 X         
							8903          D      HI_CROPS      0 X         
							8903          D      HI_DOCKS      0 X         
							8903          D   HI_LANDFILL      0 X         
							8903          D       HI_LAWN      0 X         
							8903          D    HI_ORCHARD      0 X         
							8903          D       HI_PARK      0 X         
							8903          D    HI_PASTURE      0 X         
							8903          D HI_POWERLINES      0 X         
							8903          D      HI_ROADS      0 X         
							8903          D      HI_WALLS      0 X         
							8903          J  HI_BUILDINGS      0 X         
							8903          J HI_COMMERCIAL      0 X         
							8903          J      HI_CROPS      0 X         
							8903          J      HI_DOCKS      0 X         
							8903          J   HI_LANDFILL      0 X         
							8903          J       HI_LAWN      0 X         
							8903          J    HI_ORCHARD      0 X         
							8903          J       HI_PARK      0 X         
							8903          J    HI_PASTURE      0 X         
							8903          J HI_POWERLINES      0 X         
							8903          J      HI_ROADS      0 X         
							8903          J      HI_WALLS      0 X"
			)
	fake <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
			
	return(fake)		
}


nlaHumanImpactTest.expectedResults2007 <- function()
# Values expected based on test data.  These were taken directly from tblPHABMET_LONG
# in the 2007 NLA databse.
#phab <- sqlFetch(nla07, 'tblPHABMET_LONG', stringsAsFactors=FALSE) 
#tt<-subset(phab, VISIT_ID %in% c(7784,8069,8078,8119,8258,8541,8902,8903) & grepl('^hi', PARAMETER))
#tt <- tt[order(tt$VISIT_ID,tt$PARAMETER), c('VISIT_ID','PARAMETER','RESULT')]	
{
	tc <- textConnection("  SITE         METRIC               VALUE
							7784        HIFPANY                   0
							7784   HIFPANYCIRCA                   0
							7784          HIIAG                   0
							7784     HIIAGCIRCA                   0
							7784         HIIALL                   0
							7784    HIIALLCIRCA                   0
							7784       HIINONAG                   0
							7784  HIINONAGCIRCA                   0
							7784          HINAG                   3
							7784         HINALL                  12
							7784   HINBUILDINGS                   1
							7784  HINCOMMERCIAL                   1
							7784       HINCROPS                   1
							7784       HINDOCKS                   1
							7784    HINLANDFILL                   1
							7784        HINLAWN                   1
							7784       HINNONAG                   9
							7784     HINORCHARD                   1
							7784        HINPARK                   1
							7784     HINPASTURE                   1
							7784  HINPOWERLINES                   1
							7784       HINROADS                   1
							7784       HINWALLS                   1
							7784         HIPWAG                   0
							7784        HIPWALL                   0
							7784  HIPWBUILDINGS                   0
							7784 HIPWCOMMERCIAL                   0
							7784      HIPWCROPS                   0
							7784      HIPWDOCKS                   0
							7784   HIPWLANDFILL                   0
							7784       HIPWLAWN                   0
							7784      HIPWNONAG                   0
							7784    HIPWORCHARD                   0
							7784       HIPWPARK                   0
							7784    HIPWPASTURE                   0
							7784 HIPWPOWERLINES                   0
							7784      HIPWROADS                   0
							7784      HIPWWALLS                   0
							8069        HIFPANY                   1
							8069   HIFPANYCIRCA                 0.8
							8069          HIIAG                   0
							8069     HIIAGCIRCA                   0
							8069         HIIALL                2.35
							8069    HIIALLCIRCA                 1.4
							8069       HIINONAG                2.35
							8069  HIINONAGCIRCA                 1.4
							8069          HINAG                  30
							8069         HINALL                 111
							8069   HINBUILDINGS                  10
							8069  HINCOMMERCIAL                  10
							8069       HINCROPS                  10
							8069       HINDOCKS                  10
							8069    HINLANDFILL                  10
							8069        HINLAWN                  10
							8069       HINNONAG                  81
							8069     HINORCHARD                  10
							8069        HINPARK                  10
							8069     HINPASTURE                  10
							8069  HINPOWERLINES                   1
							8069       HINROADS                  10
							8069       HINWALLS                  10
							8069         HIPWAG                   0
							8069        HIPWALL   0.211711711711712
							8069  HIPWBUILDINGS                 0.4
							8069 HIPWCOMMERCIAL                0.05
							8069      HIPWCROPS                   0
							8069      HIPWDOCKS                 0.5
							8069   HIPWLANDFILL                 0.1
							8069       HIPWLAWN                 0.4
							8069      HIPWNONAG   0.290123456790123
							8069    HIPWORCHARD                   0
							8069       HIPWPARK                 0.2
							8069    HIPWPASTURE                   0
							8069 HIPWPOWERLINES                   0
							8069      HIPWROADS                 0.5
							8069      HIPWWALLS                 0.2
							8078        HIFPANY                 0.4
							8078   HIFPANYCIRCA                   0
							8078          HIIAG                0.05
							8078     HIIAGCIRCA                   0
							8078         HIIALL                0.25
							8078    HIIALLCIRCA                   0
							8078       HIINONAG                 0.2
							8078  HIINONAGCIRCA                   0
							8078          HINAG                  30
							8078         HINALL                 120
							8078   HINBUILDINGS                  10
							8078  HINCOMMERCIAL                  10
							8078       HINCROPS                  10
							8078       HINDOCKS                  10
							8078    HINLANDFILL                  10
							8078        HINLAWN                  10
							8078       HINNONAG                  90
							8078     HINORCHARD                  10
							8078        HINPARK                  10
							8078     HINPASTURE                  10
							8078  HINPOWERLINES                  10
							8078       HINROADS                  10
							8078       HINWALLS                  10
							8078         HIPWAG  0.0166666666666667
							8078        HIPWALL  0.0208333333333333
							8078  HIPWBUILDINGS                   0
							8078 HIPWCOMMERCIAL                   0
							8078      HIPWCROPS                0.05
							8078      HIPWDOCKS                0.05
							8078   HIPWLANDFILL                   0
							8078       HIPWLAWN                0.05
							8078      HIPWNONAG  0.0222222222222222
							8078    HIPWORCHARD                   0
							8078       HIPWPARK                   0
							8078    HIPWPASTURE                   0
							8078 HIPWPOWERLINES                   0
							8078      HIPWROADS                 0.1
							8078      HIPWWALLS                   0
							8119        HIFPANY   0.272727272727273
							8119   HIFPANYCIRCA   0.090909090909091
							8119          HIIAG                   0
							8119     HIIAGCIRCA                   0
							8119         HIIALL   0.272727272727273
							8119    HIIALLCIRCA   0.090909090909091
							8119       HIINONAG   0.272727272727273
							8119  HIINONAGCIRCA   0.090909090909091
							8119          HINAG                  33
							8119         HINALL                 132
							8119   HINBUILDINGS                  11
							8119  HINCOMMERCIAL                  11
							8119       HINCROPS                  11
							8119       HINDOCKS                  11
							8119    HINLANDFILL                  11
							8119        HINLAWN                  11
							8119       HINNONAG                  99
							8119     HINORCHARD                  11
							8119        HINPARK                  11
							8119     HINPASTURE                  11
							8119  HINPOWERLINES                  11
							8119       HINROADS                  11
							8119       HINWALLS                  11
							8119         HIPWAG                   0
							8119        HIPWALL  0.0227272727272727
							8119  HIPWBUILDINGS   0.090909090909091
							8119 HIPWCOMMERCIAL                   0
							8119      HIPWCROPS                   0
							8119      HIPWDOCKS                   0
							8119   HIPWLANDFILL   0.090909090909091
							8119       HIPWLAWN  0.0454545454545455
							8119      HIPWNONAG  0.0303030303030303
							8119    HIPWORCHARD                   0
							8119       HIPWPARK                   0
							8119    HIPWPASTURE                   0
							8119 HIPWPOWERLINES                   0
							8119      HIPWROADS  0.0454545454545455
							8119      HIPWWALLS                   0
							8258        HIFPANY                 0.5
							8258   HIFPANYCIRCA                0.25
							8258          HIIAG                   0
							8258     HIIAGCIRCA                   0
							8258         HIIALL               1.125
							8258    HIIALLCIRCA   0.583333333333333
							8258       HIINONAG               1.125
							8258  HIINONAGCIRCA   0.583333333333333
							8258          HINAG                  36
							8258         HINALL                 144
							8258   HINBUILDINGS                  12
							8258  HINCOMMERCIAL                  12
							8258       HINCROPS                  12
							8258       HINDOCKS                  12
							8258    HINLANDFILL                  12
							8258        HINLAWN                  12
							8258       HINNONAG                 108
							8258     HINORCHARD                  12
							8258        HINPARK                  12
							8258     HINPASTURE                  12
							8258  HINPOWERLINES                  12
							8258       HINROADS                  12
							8258       HINWALLS                  12
							8258         HIPWAG                   0
							8258        HIPWALL             0.09375
							8258  HIPWBUILDINGS   0.208333333333333
							8258 HIPWCOMMERCIAL                   0
							8258      HIPWCROPS                   0
							8258      HIPWDOCKS   0.333333333333333
							8258   HIPWLANDFILL                   0
							8258       HIPWLAWN   0.333333333333333
							8258      HIPWNONAG               0.125
							8258    HIPWORCHARD                   0
							8258       HIPWPARK                   0
							8258    HIPWPASTURE                   0
							8258 HIPWPOWERLINES  0.0833333333333333
							8258      HIPWROADS                   0
							8258      HIPWWALLS   0.166666666666667
							8541        HIFPANY                 0.1
							8541   HIFPANYCIRCA                 0.1
							8541          HIIAG                   0
							8541     HIIAGCIRCA                   0
							8541         HIIALL                 0.3
							8541    HIIALLCIRCA                 0.3
							8541       HIINONAG                 0.3
							8541  HIINONAGCIRCA                 0.3
							8541          HINAG                  30
							8541         HINALL                 119
							8541   HINBUILDINGS                  10
							8541  HINCOMMERCIAL                  10
							8541       HINCROPS                  10
							8541       HINDOCKS                  10
							8541    HINLANDFILL                   9
							8541        HINLAWN                  10
							8541       HINNONAG                  89
							8541     HINORCHARD                  10
							8541        HINPARK                  10
							8541     HINPASTURE                  10
							8541  HINPOWERLINES                  10
							8541       HINROADS                  10
							8541       HINWALLS                  10
							8541         HIPWAG                   0
							8541        HIPWALL  0.0252100840336134
							8541  HIPWBUILDINGS                   0
							8541 HIPWCOMMERCIAL                   0
							8541      HIPWCROPS                   0
							8541      HIPWDOCKS                   0
							8541   HIPWLANDFILL                   0
							8541       HIPWLAWN                 0.1
							8541      HIPWNONAG  0.0337078651685393
							8541    HIPWORCHARD                   0
							8541       HIPWPARK                 0.1
							8541    HIPWPASTURE                   0
							8541 HIPWPOWERLINES                   0
							8541      HIPWROADS                   0
							8541      HIPWWALLS                 0.1
							8902        HIFPANY                 0.1
							8902   HIFPANYCIRCA                   0
							8902          HIIAG                   0
							8902     HIIAGCIRCA                   0
							8902         HIIALL                 0.1
							8902    HIIALLCIRCA                   0
							8902       HIINONAG                 0.1
							8902  HIINONAGCIRCA                   0
							8902          HINAG                  30
							8902         HINALL                 120
							8902   HINBUILDINGS                  10
							8902  HINCOMMERCIAL                  10
							8902       HINCROPS                  10
							8902       HINDOCKS                  10
							8902    HINLANDFILL                  10
							8902        HINLAWN                  10
							8902       HINNONAG                  90
							8902     HINORCHARD                  10
							8902        HINPARK                  10
							8902     HINPASTURE                  10
							8902  HINPOWERLINES                  10
							8902       HINROADS                  10
							8902       HINWALLS                  10
							8902         HIPWAG                   0
							8902        HIPWALL 0.00833333333333333
							8902  HIPWBUILDINGS                   0
							8902 HIPWCOMMERCIAL                   0
							8902      HIPWCROPS                   0
							8902      HIPWDOCKS                0.05
							8902   HIPWLANDFILL                   0
							8902       HIPWLAWN                   0
							8902      HIPWNONAG  0.0111111111111111
							8902    HIPWORCHARD                   0
							8902       HIPWPARK                   0
							8902    HIPWPASTURE                   0
							8902 HIPWPOWERLINES                   0
							8902      HIPWROADS                0.05
							8902      HIPWWALLS                   0
							8903        HIFPANY                   0
							8903   HIFPANYCIRCA                   0
							8903          HIIAG                   0
							8903     HIIAGCIRCA                   0
							8903         HIIALL                   0
							8903    HIIALLCIRCA                   0
							8903       HIINONAG                   0
							8903  HIINONAGCIRCA                   0
							8903          HINAG                  15
							8903         HINALL                  60
							8903   HINBUILDINGS                   5
							8903  HINCOMMERCIAL                   5
							8903       HINCROPS                   5
							8903       HINDOCKS                   5
							8903    HINLANDFILL                   5
							8903        HINLAWN                   5
							8903       HINNONAG                  45
							8903     HINORCHARD                   5
							8903        HINPARK                   5
							8903     HINPASTURE                   5
							8903  HINPOWERLINES                   5
							8903       HINROADS                   5
							8903       HINWALLS                   5
							8903         HIPWAG                   0
							8903        HIPWALL                   0
							8903  HIPWBUILDINGS                   0
							8903 HIPWCOMMERCIAL                   0
							8903      HIPWCROPS                   0
							8903      HIPWDOCKS                   0
							8903   HIPWLANDFILL                   0
							8903       HIPWLAWN                   0
							8903      HIPWNONAG                   0
							8903    HIPWORCHARD                   0
							8903       HIPWPARK                   0
							8903    HIPWPASTURE                   0
							8903 HIPWPOWERLINES                   0
							8903      HIPWROADS                   0
							8903      HIPWWALLS                   0"
							)
	longMets <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)

	return(longMets)
}


nlaHumanImpactTest.createTestDataWithDrawDown <- function()
# Returns dataframe of test data.  This is a subset of the 2012 study as of 
# 20 August 2013.  It contains data for the following SITEs:
#	6228	Full complement of data A-J, all HORIZ_DIST_DD present
#	6279	Data mostly present in A-J; HORIZ_DIST_DD missing at E
#	6302	Data mostly present, H missing; All HORIZ_DIST_DD present
#	6303	Half complement of data; no HORIZ_DIST_DD present
#	6362	Data only at J; HORIZ_DIST_DD only at J
#	6399	Full complement of data in B, C, else 12-13/station; HORIZ_DIST_DD at B,C only
#	6449	Full complement of data A-J, K, L; HORIZ_DIST_DD at all stations
#	6684	Half complement of data at A,B,C,H,I,J; no HORIZ_DIST_DD present 
#	7504	Depauperate data, few per station; no HORIZ_DIST_DD present
#	7740	Few data missing at each station; all HORIZ_DIST_DD present
#	7808	Depauperate data, few per station; all HORIZ_DIST_DD present
#	1000100	One row of data total; no HORIZ_DIST_DD present
#
{
	tc <- textConnection("   SITE SAMPLE_TYPE STATION        CLASS VALUE FLAG  FORM_TYPE
							6228        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       A  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       A HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       A      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6228        PHAB       A      HI_DOCKS_DD      0   NA  PHAB_BACK
							6228        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       A   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       A          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       A       HI_LAWN_DD      C   NA  PHAB_BACK
							6228        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       A    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       A         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       A      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       A       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       A    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       A HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       A      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       A      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       A    HORIZ_DIST_DD    4.5   NA PHAB_FRONT
							6228        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       B  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       B         HI_DOCKS      C   NA  PHAB_BACK
							6228        PHAB       B      HI_DOCKS_DD      C   NA  PHAB_BACK
							6228        PHAB       B      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       B   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       B          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       B       HI_LAWN_DD      C   NA  PHAB_BACK
							6228        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       B         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       B      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       B    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       B HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       B         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       B      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       B      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       B    HORIZ_DIST_DD    4.0   NA PHAB_FRONT
							6228        PHAB       C     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       C  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       C         HI_DOCKS      C   NA  PHAB_BACK
							6228        PHAB       C      HI_DOCKS_DD      C   NA  PHAB_BACK
							6228        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       C   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       C          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       C       HI_LAWN_DD      C   NA  PHAB_BACK
							6228        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       C         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       C      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       C          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       C       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       C    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       C HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       C         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       C      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       C         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       C      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       C    HORIZ_DIST_DD    3.2   NA PHAB_FRONT
							6228        PHAB       D     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       D  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       D HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       D      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							6228        PHAB       D      HI_DOCKS_DD      0   NA  PHAB_BACK
							6228        PHAB       D      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       D   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       D          HI_LAWN      0   NA  PHAB_BACK
							6228        PHAB       D       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       D    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       D         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       D      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       D       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       D    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       D    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       D HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       D         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       D      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       D         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       D      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       D    HORIZ_DIST_DD    5.0   NA PHAB_FRONT
							6228        PHAB       E     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       E  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       E HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       E      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       E         HI_DOCKS      0   NA  PHAB_BACK
							6228        PHAB       E      HI_DOCKS_DD      0   NA  PHAB_BACK
							6228        PHAB       E      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       E   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       E          HI_LAWN      0   NA  PHAB_BACK
							6228        PHAB       E       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       E    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       E         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       E      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       E          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       E       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       E    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       E    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       E HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       E         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       E      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       E         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       E      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       E    HORIZ_DIST_DD   12.0   NA PHAB_FRONT
							6228        PHAB       F     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       F  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       F HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       F      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       F         HI_DOCKS      C   NA  PHAB_BACK
							6228        PHAB       F      HI_DOCKS_DD      C   NA  PHAB_BACK
							6228        PHAB       F      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       F   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       F          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       F       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       F    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       F         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       F      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       F       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       F    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       F HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       F         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       F      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       F         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       F      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       F    HORIZ_DIST_DD    1.7   NA PHAB_FRONT
							6228        PHAB       G     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       G  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       G HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       G      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       G         HI_DOCKS      C   NA  PHAB_BACK
							6228        PHAB       G      HI_DOCKS_DD      C   NA  PHAB_BACK
							6228        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       G   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       G          HI_LAWN      0   NA  PHAB_BACK
							6228        PHAB       G       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       G    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       G         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       G      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       G          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       G       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       G    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       G    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       G HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       G         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       G      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       G      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       G    HORIZ_DIST_DD    1.8   NA PHAB_FRONT
							6228        PHAB       H     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       H  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       H HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       H      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       H         HI_DOCKS      0   NA  PHAB_BACK
							6228        PHAB       H      HI_DOCKS_DD      0   NA  PHAB_BACK
							6228        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       H   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       H          HI_LAWN      0   NA  PHAB_BACK
							6228        PHAB       H       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       H    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       H         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       H      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       H       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       H    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       H HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       H         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       H      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       H         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       H      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       H    HORIZ_DIST_DD    3.0   NA PHAB_FRONT
							6228        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       I  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       I HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       I      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       I         HI_DOCKS      C   NA  PHAB_BACK
							6228        PHAB       I      HI_DOCKS_DD      C   NA  PHAB_BACK
							6228        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6228        PHAB       I   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6228        PHAB       I          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       I       HI_LAWN_DD      C   NA  PHAB_BACK
							6228        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       I    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       I         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       I      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       I       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       I    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       I    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       I HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       I         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       I      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       I      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       I    HORIZ_DIST_DD    2.5   NA PHAB_FRONT
							6228        PHAB       J     HI_BUILDINGS      0   NA  PHAB_BACK
							6228        PHAB       J  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6228        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6228        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6228        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6228        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							6228        PHAB       J         HI_DOCKS      P   NA  PHAB_BACK
							6228        PHAB       J      HI_DOCKS_DD      P   NA  PHAB_BACK
							6228        PHAB       J      HI_LANDFILL      C   NA  PHAB_BACK
							6228        PHAB       J   HI_LANDFILL_DD      P   NA  PHAB_BACK
							6228        PHAB       J          HI_LAWN      C   NA  PHAB_BACK
							6228        PHAB       J       HI_LAWN_DD      0   NA  PHAB_BACK
							6228        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6228        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6228        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6228        PHAB       J      HI_OTHER_DD      0   NA  PHAB_BACK
							6228        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6228        PHAB       J       HI_PARK_DD      0   NA  PHAB_BACK
							6228        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6228        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							6228        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6228        PHAB       J HI_POWERLINES_DD      0   NA  PHAB_BACK
							6228        PHAB       J         HI_ROADS      0   NA  PHAB_BACK
							6228        PHAB       J      HI_ROADS_DD      0   NA  PHAB_BACK
							6228        PHAB       J         HI_WALLS      0   NA  PHAB_BACK
							6228        PHAB       J      HI_WALLS_DD      0   NA  PHAB_BACK
							6228        PHAB       J    HORIZ_DIST_DD    1.5   NA PHAB_FRONT
							6279        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       A  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       A HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       A      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       A         HI_DOCKS      P   NA  PHAB_BACK
							6279        PHAB       A      HI_DOCKS_DD      P   NA  PHAB_BACK
							6279        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       A   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       A       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       A    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       A          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       A       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       A    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       A    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       A HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       A         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       A      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       A      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       A    HORIZ_DIST_DD    2.0   NA PHAB_FRONT
							6279        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       B  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       B         HI_DOCKS      0   NA  PHAB_BACK
							6279        PHAB       B      HI_DOCKS_DD      0   NA  PHAB_BACK
							6279        PHAB       B      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       B   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       B       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6279        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							6279        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       B    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       B HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       B         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       B      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       B      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       B    HORIZ_DIST_DD    2.0   NA PHAB_FRONT
							6279        PHAB       C     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       C  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       C         HI_DOCKS      0   NA  PHAB_BACK
							6279        PHAB       C      HI_DOCKS_DD      0   NA  PHAB_BACK
							6279        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       C   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       C          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       C       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       C          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       C       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       C    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       C HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       C         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       C      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       C         HI_WALLS      P   NA  PHAB_BACK
							6279        PHAB       C      HI_WALLS_DD      P   NA  PHAB_BACK
							6279        PHAB       C    HORIZ_DIST_DD    1.8   NA PHAB_FRONT
							6279        PHAB       D     HI_BUILDINGS      P   NA  PHAB_BACK
							6279        PHAB       D  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6279        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       D HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       D      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							6279        PHAB       D      HI_DOCKS_DD      0   NA  PHAB_BACK
							6279        PHAB       D      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       D   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       D          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       D       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       D    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       D          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       D       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       D    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       D    HI_POWERLINES      C   NA  PHAB_BACK
							6279        PHAB       D HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       D         HI_ROADS      C   NA  PHAB_BACK
							6279        PHAB       D      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       D         HI_WALLS      C   NA  PHAB_BACK
							6279        PHAB       D      HI_WALLS_DD      P   NA  PHAB_BACK
							6279        PHAB       D    HORIZ_DIST_DD    1.0   NA PHAB_FRONT
							6279        PHAB       E     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       E  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       E HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       E      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       E         HI_DOCKS      0   NA  PHAB_BACK
							6279        PHAB       E      HI_DOCKS_DD      0   NA  PHAB_BACK
							6279        PHAB       E      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       E   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       E          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       E       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       E    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       E          HI_PARK      C   NA  PHAB_BACK
							6279        PHAB       E       HI_PARK_DD      C   NA  PHAB_BACK
							6279        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       E    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       E    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       E HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       E         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       E      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       F     HI_BUILDINGS      C   NA  PHAB_BACK
							6279        PHAB       F  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6279        PHAB       F    HI_COMMERCIAL      P   NA  PHAB_BACK
							6279        PHAB       F HI_COMMERCIAL_DD      P   NA  PHAB_BACK
							6279        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       F      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       F         HI_DOCKS      P   NA  PHAB_BACK
							6279        PHAB       F      HI_DOCKS_DD      P   NA  PHAB_BACK
							6279        PHAB       F      HI_LANDFILL      P   NA  PHAB_BACK
							6279        PHAB       F   HI_LANDFILL_DD      P   NA  PHAB_BACK
							6279        PHAB       F          HI_LAWN      P   NA  PHAB_BACK
							6279        PHAB       F       HI_LAWN_DD      P   NA  PHAB_BACK
							6279        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       F    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       F          HI_PARK      C   NA  PHAB_BACK
							6279        PHAB       F       HI_PARK_DD      C   NA  PHAB_BACK
							6279        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       F    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       F    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       F HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       F         HI_ROADS      C   NA  PHAB_BACK
							6279        PHAB       F      HI_ROADS_DD      C   NA  PHAB_BACK
							6279        PHAB       F         HI_WALLS      P   NA  PHAB_BACK
							6279        PHAB       F      HI_WALLS_DD      P   NA  PHAB_BACK
							6279        PHAB       F    HORIZ_DIST_DD    2.5   NA PHAB_FRONT
							6279        PHAB       G     HI_BUILDINGS      P   NA  PHAB_BACK
							6279        PHAB       G  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6279        PHAB       G    HI_COMMERCIAL      P   NA  PHAB_BACK
							6279        PHAB       G HI_COMMERCIAL_DD      P   NA  PHAB_BACK
							6279        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       G      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       G         HI_DOCKS      P   NA  PHAB_BACK
							6279        PHAB       G      HI_DOCKS_DD      P   NA  PHAB_BACK
							6279        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       G   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       G          HI_LAWN      P   NA  PHAB_BACK
							6279        PHAB       G       HI_LAWN_DD      P   NA  PHAB_BACK
							6279        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       G    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       G          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       G       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       G    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       G    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       G HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       G         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       G      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       G      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       G    HORIZ_DIST_DD    2.4   NA PHAB_FRONT
							6279        PHAB       H     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       H  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       H HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       H      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       H         HI_DOCKS      P   NA  PHAB_BACK
							6279        PHAB       H      HI_DOCKS_DD      P   NA  PHAB_BACK
							6279        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       H   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       H          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       H       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       H    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       H          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       H       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       H    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       H    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       H HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       H         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       H      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       H         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       H      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       H    HORIZ_DIST_DD    2.0   NA PHAB_FRONT
							6279        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       I  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       I HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       I      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       I         HI_DOCKS      0   NA  PHAB_BACK
							6279        PHAB       I      HI_DOCKS_DD      0   NA  PHAB_BACK
							6279        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       I   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       I          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       I       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       I    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       I          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       I       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       I    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       I    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       I HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       I         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       I      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       I      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       I    HORIZ_DIST_DD    3.0   NA PHAB_FRONT
							6279        PHAB       J     HI_BUILDINGS      0   NA  PHAB_BACK
							6279        PHAB       J  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6279        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6279        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6279        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6279        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							6279        PHAB       J         HI_DOCKS      P   NA  PHAB_BACK
							6279        PHAB       J      HI_DOCKS_DD      P   NA  PHAB_BACK
							6279        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							6279        PHAB       J   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6279        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							6279        PHAB       J       HI_LAWN_DD      0   NA  PHAB_BACK
							6279        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6279        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6279        PHAB       J          HI_PARK      P   NA  PHAB_BACK
							6279        PHAB       J       HI_PARK_DD      P   NA  PHAB_BACK
							6279        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6279        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							6279        PHAB       J    HI_POWERLINES      P   NA  PHAB_BACK
							6279        PHAB       J HI_POWERLINES_DD      P   NA  PHAB_BACK
							6279        PHAB       J         HI_ROADS      P   NA  PHAB_BACK
							6279        PHAB       J      HI_ROADS_DD      P   NA  PHAB_BACK
							6279        PHAB       J         HI_WALLS      0   NA  PHAB_BACK
							6279        PHAB       J      HI_WALLS_DD      0   NA  PHAB_BACK
							6279        PHAB       J    HORIZ_DIST_DD    2.5   NA PHAB_FRONT
							6302        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       A  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       A HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       A      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       A      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       A   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       A       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       A    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       A         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       A      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       A       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       A    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       A HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       A      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       A      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       A    HORIZ_DIST_DD   23.3   NA PHAB_FRONT
							6302        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       B  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       B         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       B      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       B      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       B   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       B       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       B         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       B      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       B    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       B HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       B         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       B      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       B      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       B    HORIZ_DIST_DD  011.8   NA PHAB_FRONT
							6302        PHAB       C  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       C         HI_DOCKS      P   NA  PHAB_BACK
							6302        PHAB       C      HI_DOCKS_DD      P   NA  PHAB_BACK
							6302        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       C   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       C          HI_LAWN      C   NA  PHAB_BACK
							6302        PHAB       C       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       C      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       C          HI_PARK      C   NA  PHAB_BACK
							6302        PHAB       C       HI_PARK_DD      P   NA  PHAB_BACK
							6302        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       C    HI_POWERLINES      C   NA  PHAB_BACK
							6302        PHAB       C HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       C         HI_ROADS      C   NA  PHAB_BACK
							6302        PHAB       C      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       C         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       C    HORIZ_DIST_DD   15.6   NA PHAB_FRONT
							6302        PHAB       D     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       D  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       D HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       D      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       D      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       D      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       D   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6302        PHAB       D          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       D       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       D    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       D         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       D      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       D       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       D    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       D    HI_POWERLINES      C   NA  PHAB_BACK
							6302        PHAB       D HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       D         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       D      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       D         HI_WALLS      C   F2  PHAB_BACK
							6302        PHAB       D      HI_WALLS_DD      C   F2  PHAB_BACK
							6302        PHAB       D    HORIZ_DIST_DD    4.5   NA PHAB_FRONT
							6302        PHAB       E     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       E  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       E HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       E      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       E         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       E      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       E      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       E   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       E          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       E       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       E    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       E         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       E      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       E          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       E       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       E    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       E    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       E HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       E         HI_ROADS      P   NA  PHAB_BACK
							6302        PHAB       E      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       E         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       E      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       E    HORIZ_DIST_DD  023.9   NA PHAB_FRONT
							6302        PHAB       F     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       F  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       F HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       F      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       F         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       F      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       F      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       F   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       F          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       F       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       F    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       F         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       F      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       F       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       F    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       F HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       F         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       F      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       F         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       F      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       F    HORIZ_DIST_DD   21.7   NA PHAB_FRONT
							6302        PHAB       G     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       G  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       G HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       G      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       G         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       G      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       G   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       G          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       G       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       G    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       G         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       G      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       G          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       G       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       G    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       G    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       G HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       G         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       G      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       G      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       G    HORIZ_DIST_DD    9.7   NA PHAB_FRONT
							6302        PHAB       H    HORIZ_DIST_DD  036.2   NA PHAB_FRONT
							6302        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							6302        PHAB       I  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       I HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       I      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       I         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       I      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       I   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6302        PHAB       I          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       I       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       I    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       I         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       I      HI_OTHER_DD      0   NA  PHAB_BACK
							6302        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       I       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       I    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       I    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       I HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       I         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       I      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       I      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       I    HORIZ_DIST_DD  017.8   NA PHAB_FRONT
							6302        PHAB       J     HI_BUILDINGS      C   NA  PHAB_BACK
							6302        PHAB       J  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6302        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6302        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6302        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6302        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							6302        PHAB       J         HI_DOCKS      0   NA  PHAB_BACK
							6302        PHAB       J      HI_DOCKS_DD      0   NA  PHAB_BACK
							6302        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							6302        PHAB       J   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6302        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							6302        PHAB       J       HI_LAWN_DD      0   NA  PHAB_BACK
							6302        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6302        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6302        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6302        PHAB       J      HI_OTHER_DD      C   F1  PHAB_BACK
							6302        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6302        PHAB       J       HI_PARK_DD      0   NA  PHAB_BACK
							6302        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6302        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							6302        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6302        PHAB       J HI_POWERLINES_DD      0   NA  PHAB_BACK
							6302        PHAB       J         HI_ROADS      0   NA  PHAB_BACK
							6302        PHAB       J      HI_ROADS_DD      0   NA  PHAB_BACK
							6302        PHAB       J         HI_WALLS      0   NA  PHAB_BACK
							6302        PHAB       J      HI_WALLS_DD      0   NA  PHAB_BACK
							6302        PHAB       J    HORIZ_DIST_DD    2.0   NA PHAB_FRONT
							6303        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       A      HI_LANDFILL      C   NA  PHAB_BACK
							6303        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       A         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       B         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       B      HI_LANDFILL      P   NA  PHAB_BACK
							6303        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       B         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       B    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       B         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       C     HI_BUILDINGS      C   NA  PHAB_BACK
							6303        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       C         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       C      HI_LANDFILL      C   NA  PHAB_BACK
							6303        PHAB       C          HI_LAWN      C   NA  PHAB_BACK
							6303        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       C         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       C          HI_PARK      P   NA  PHAB_BACK
							6303        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       C    HI_POWERLINES      P   NA  PHAB_BACK
							6303        PHAB       C         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       C         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       D     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       D      HI_LANDFILL      C   NA  PHAB_BACK
							6303        PHAB       D          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       D         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       D    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       D         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       D         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       E     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       E         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       E      HI_LANDFILL      P   NA  PHAB_BACK
							6303        PHAB       E          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       E         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       E          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       E    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       E         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       E         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       F     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       F         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       F      HI_LANDFILL      C   NA  PHAB_BACK
							6303        PHAB       F          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       F         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       F         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       F         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       G     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       G         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							6303        PHAB       G          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       G         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       G          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       G    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       G         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       H     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       H         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       H      HI_LANDFILL      P   NA  PHAB_BACK
							6303        PHAB       H          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       H         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       H         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       H         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							6303        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       I         HI_DOCKS      0   NA  PHAB_BACK
							6303        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6303        PHAB       I          HI_LAWN      0   NA  PHAB_BACK
							6303        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       I         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       I    HI_POWERLINES      0   NA  PHAB_BACK
							6303        PHAB       I         HI_ROADS      0   NA  PHAB_BACK
							6303        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6303        PHAB       J     HI_BUILDINGS      P   NA  PHAB_BACK
							6303        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6303        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6303        PHAB       J         HI_DOCKS      P   NA  PHAB_BACK
							6303        PHAB       J      HI_LANDFILL      C   NA  PHAB_BACK
							6303        PHAB       J          HI_LAWN      C   NA  PHAB_BACK
							6303        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6303        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6303        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6303        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6303        PHAB       J    HI_POWERLINES      C   NA  PHAB_BACK
							6303        PHAB       J         HI_ROADS      C   NA  PHAB_BACK
							6303        PHAB       J         HI_WALLS      C   NA  PHAB_BACK
							6362        PHAB       J     HI_BUILDINGS      0   NA  PHAB_BACK
							6362        PHAB       J  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6362        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6362        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6362        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6362        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							6362        PHAB       J         HI_DOCKS      0   NA  PHAB_BACK
							6362        PHAB       J      HI_DOCKS_DD      0   NA  PHAB_BACK
							6362        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							6362        PHAB       J   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6362        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							6362        PHAB       J       HI_LAWN_DD      0   NA  PHAB_BACK
							6362        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6362        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6362        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6362        PHAB       J      HI_OTHER_DD      0   NA  PHAB_BACK
							6362        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6362        PHAB       J       HI_PARK_DD      0   NA  PHAB_BACK
							6362        PHAB       J       HI_PASTURE      C   NA  PHAB_BACK
							6362        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							6362        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6362        PHAB       J HI_POWERLINES_DD      0   NA  PHAB_BACK
							6362        PHAB       J         HI_ROADS      0   NA  PHAB_BACK
							6362        PHAB       J      HI_ROADS_DD      0   NA  PHAB_BACK
							6362        PHAB       J         HI_WALLS      0   NA  PHAB_BACK
							6362        PHAB       J      HI_WALLS_DD      0   NA  PHAB_BACK
							6362        PHAB       J    HORIZ_DIST_DD   20.0   NA PHAB_FRONT
							6399        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6399        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6399        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6399        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       A         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							6399        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6399        PHAB       B     HI_BUILDINGS      C   NA  PHAB_BACK
							6399        PHAB       B  HI_BUILDINGS_DD      C   NA  PHAB_BACK
							6399        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6399        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							6399        PHAB       B         HI_DOCKS      C   NA  PHAB_BACK
							6399        PHAB       B      HI_DOCKS_DD      C   NA  PHAB_BACK
							6399        PHAB       B      HI_LANDFILL      C   NA  PHAB_BACK
							6399        PHAB       B   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6399        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6399        PHAB       B       HI_LAWN_DD      0   NA  PHAB_BACK
							6399        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6399        PHAB       B         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       B      HI_OTHER_DD      0   NA  PHAB_BACK
							6399        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							6399        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							6399        PHAB       B    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       B HI_POWERLINES_DD      P   NA  PHAB_BACK
							6399        PHAB       B         HI_ROADS      P   NA  PHAB_BACK
							6399        PHAB       B      HI_ROADS_DD      P   NA  PHAB_BACK
							6399        PHAB       B         HI_WALLS      C   NA  PHAB_BACK
							6399        PHAB       B      HI_WALLS_DD      C   NA  PHAB_BACK
							6399        PHAB       B    HORIZ_DIST_DD    1.5   NA PHAB_FRONT
							6399        PHAB       C     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       C  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6399        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6399        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							6399        PHAB       C         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       C      HI_DOCKS_DD      P   NA  PHAB_BACK
							6399        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       C   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6399        PHAB       C          HI_LAWN      C   NA  PHAB_BACK
							6399        PHAB       C       HI_LAWN_DD      P   NA  PHAB_BACK
							6399        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6399        PHAB       C         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       C      HI_OTHER_DD      0   NA  PHAB_BACK
							6399        PHAB       C          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       C       HI_PARK_DD      0   NA  PHAB_BACK
							6399        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							6399        PHAB       C    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       C HI_POWERLINES_DD      P   NA  PHAB_BACK
							6399        PHAB       C         HI_ROADS      P   NA  PHAB_BACK
							6399        PHAB       C      HI_ROADS_DD      P   NA  PHAB_BACK
							6399        PHAB       C         HI_WALLS      C   NA  PHAB_BACK
							6399        PHAB       C      HI_WALLS_DD      C   NA  PHAB_BACK
							6399        PHAB       C    HORIZ_DIST_DD    1.0   NA PHAB_FRONT
							6399        PHAB       D     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							6399        PHAB       D      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       D          HI_LAWN      C   NA  PHAB_BACK
							6399        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       D         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       D    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       D         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       D         HI_WALLS      C   NA  PHAB_BACK
							6399        PHAB       E     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       E         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       E      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       E          HI_LAWN      C   NA  PHAB_BACK
							6399        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       E         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       E          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       E    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       E         HI_ROADS      P   NA  PHAB_BACK
							6399        PHAB       E         HI_WALLS      C   NA  PHAB_BACK
							6399        PHAB       F     HI_BUILDINGS      0   NA  PHAB_BACK
							6399        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       F         HI_DOCKS      0   NA  PHAB_BACK
							6399        PHAB       F      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       F          HI_LAWN      0   NA  PHAB_BACK
							6399        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       F         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							6399        PHAB       F         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       F         HI_WALLS      0   NA  PHAB_BACK
							6399        PHAB       G     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       G         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       G          HI_LAWN      P   NA  PHAB_BACK
							6399        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       G         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       G    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       G         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							6399        PHAB       H     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       H         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       H          HI_LAWN      C   NA  PHAB_BACK
							6399        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       H         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							6399        PHAB       H         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       H         HI_WALLS      C   NA  PHAB_BACK
							6399        PHAB       I     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       I         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6399        PHAB       I          HI_LAWN      C   NA  PHAB_BACK
							6399        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       I         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       I    HI_POWERLINES      P   NA  PHAB_BACK
							6399        PHAB       I         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6399        PHAB       J     HI_BUILDINGS      P   NA  PHAB_BACK
							6399        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6399        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6399        PHAB       J         HI_DOCKS      P   NA  PHAB_BACK
							6399        PHAB       J      HI_LANDFILL      C   NA  PHAB_BACK
							6399        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							6399        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6399        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6399        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6399        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6399        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6399        PHAB       J         HI_ROADS      0   NA  PHAB_BACK
							6399        PHAB       J         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6449        PHAB       A  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6449        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       A HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       A      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6449        PHAB       A      HI_DOCKS_DD      0   NA  PHAB_BACK
							6449        PHAB       A      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       A   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6449        PHAB       A       HI_LAWN_DD      0   NA  PHAB_BACK
							6449        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       A    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       A         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       A      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       A       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       A    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       A    HI_POWERLINES      P   NA  PHAB_BACK
							6449        PHAB       A HI_POWERLINES_DD      P   NA  PHAB_BACK
							6449        PHAB       A         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       A      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6449        PHAB       A      HI_WALLS_DD      0   NA  PHAB_BACK
							6449        PHAB       A    HORIZ_DIST_DD    0.9   NA PHAB_FRONT
							6449        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6449        PHAB       B  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							6449        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       B         HI_DOCKS      0   NA  PHAB_BACK
							6449        PHAB       B      HI_DOCKS_DD      0   NA  PHAB_BACK
							6449        PHAB       B      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       B   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6449        PHAB       B       HI_LAWN_DD      0   NA  PHAB_BACK
							6449        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       B         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       B      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       B    HI_POWERLINES      P   NA  PHAB_BACK
							6449        PHAB       B HI_POWERLINES_DD      P   NA  PHAB_BACK
							6449        PHAB       B         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       B      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6449        PHAB       B      HI_WALLS_DD      0   NA  PHAB_BACK
							6449        PHAB       B    HORIZ_DIST_DD    1.1   NA PHAB_FRONT
							6449        PHAB       C     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       C  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       C         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       C      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       C      HI_LANDFILL      P   NA  PHAB_BACK
							6449        PHAB       C   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       C          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       C       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       C         HI_OTHER      C   F1  PHAB_BACK
							6449        PHAB       C      HI_OTHER_DD      P   F1  PHAB_BACK
							6449        PHAB       C          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       C       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       C    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       C HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       C         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       C      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       C         HI_WALLS      P   NA  PHAB_BACK
							6449        PHAB       C      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       C    HORIZ_DIST_DD    0.2   NA PHAB_FRONT
							6449        PHAB       D     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       D  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       D HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       D      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       D         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       D      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       D      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       D   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       D          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       D       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       D    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       D         HI_OTHER      C   F1  PHAB_BACK
							6449        PHAB       D      HI_OTHER_DD      P   F1  PHAB_BACK
							6449        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       D       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       D    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       D    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       D HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       D         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       D      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       D         HI_WALLS      P   NA  PHAB_BACK
							6449        PHAB       D      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       D    HORIZ_DIST_DD    0.6   NA PHAB_FRONT
							6449        PHAB       E     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       E  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       E HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       E      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       E         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       E      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       E      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       E   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       E          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       E       HI_LAWN_DD      C   NA  PHAB_BACK
							6449        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       E    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       E         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       E      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       E          HI_PARK      P   NA  PHAB_BACK
							6449        PHAB       E       HI_PARK_DD      P   NA  PHAB_BACK
							6449        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       E    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       E    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       E HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       E         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       E         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       E      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       E    HORIZ_DIST_DD    0.1   NA PHAB_FRONT
							6449        PHAB       F     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       F  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       F HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       F      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       F         HI_DOCKS      C   NA  PHAB_BACK
							6449        PHAB       F      HI_DOCKS_DD      C   NA  PHAB_BACK
							6449        PHAB       F      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       F   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       F          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       F       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       F    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       F         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       F      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       F       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       F    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       F HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       F         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       F      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       F         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       F      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       F    HORIZ_DIST_DD    0.1   NA PHAB_FRONT
							6449        PHAB       G     HI_BUILDINGS      C   NA  PHAB_BACK
							6449        PHAB       G  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       G HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       G      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       G         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       G      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       G      HI_LANDFILL      C   NA  PHAB_BACK
							6449        PHAB       G   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       G          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       G       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       G    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       G         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       G      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       G          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       G       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       G    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       G    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       G HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       G         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       G      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       G         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       G      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       G    HORIZ_DIST_DD    0.3   NA PHAB_FRONT
							6449        PHAB       H     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       H  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       H HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       H      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       H         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       H      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							6449        PHAB       H   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6449        PHAB       H          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       H       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       H    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       H         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       H      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       H       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       H    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       H HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       H         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       H      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       H         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       H      HI_WALLS_DD      C   NA  PHAB_BACK
							6449        PHAB       H    HORIZ_DIST_DD    0.2   NA PHAB_FRONT
							6449        PHAB       I     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       I  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       I HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       I      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       I         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       I      HI_DOCKS_DD      C   NA  PHAB_BACK
							6449        PHAB       I      HI_LANDFILL      P   NA  PHAB_BACK
							6449        PHAB       I   HI_LANDFILL_DD      C   NA  PHAB_BACK
							6449        PHAB       I          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       I       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       I    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       I         HI_OTHER      C   F1  PHAB_BACK
							6449        PHAB       I      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       I          HI_PARK      C   NA  PHAB_BACK
							6449        PHAB       I       HI_PARK_DD      C   NA  PHAB_BACK
							6449        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       I    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       I    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       I HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       I         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       I      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       I         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       I      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       I    HORIZ_DIST_DD    0.3   NA PHAB_FRONT
							6449        PHAB       J     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       J  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       J         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       J      HI_DOCKS_DD      C   NA  PHAB_BACK
							6449        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							6449        PHAB       J   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6449        PHAB       J          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       J       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       J         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       J      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       J          HI_PARK      P   NA  PHAB_BACK
							6449        PHAB       J       HI_PARK_DD      C   NA  PHAB_BACK
							6449        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       J HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       J         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       J      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       J         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       J      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       J    HORIZ_DIST_DD    0.5   NA PHAB_FRONT
							6449        PHAB       K     HI_BUILDINGS      P   NA  PHAB_BACK
							6449        PHAB       K  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       K    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       K HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       K         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       K      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       K         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       K      HI_DOCKS_DD      P   NA  PHAB_BACK
							6449        PHAB       K      HI_LANDFILL      0   NA  PHAB_BACK
							6449        PHAB       K   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6449        PHAB       K          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       K       HI_LAWN_DD      C   NA  PHAB_BACK
							6449        PHAB       K       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       K    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       K         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       K      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       K          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       K       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       K       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       K    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       K    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       K HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       K         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       K      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       K         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       K      HI_WALLS_DD      C   NA  PHAB_BACK
							6449        PHAB       K    HORIZ_DIST_DD    0.1   NA PHAB_FRONT
							6449        PHAB       L     HI_BUILDINGS      C   NA  PHAB_BACK
							6449        PHAB       L  HI_BUILDINGS_DD      P   NA  PHAB_BACK
							6449        PHAB       L    HI_COMMERCIAL      0   NA  PHAB_BACK
							6449        PHAB       L HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							6449        PHAB       L         HI_CROPS      0   NA  PHAB_BACK
							6449        PHAB       L      HI_CROPS_DD      0   NA  PHAB_BACK
							6449        PHAB       L         HI_DOCKS      P   NA  PHAB_BACK
							6449        PHAB       L      HI_DOCKS_DD      C   NA  PHAB_BACK
							6449        PHAB       L      HI_LANDFILL      0   NA  PHAB_BACK
							6449        PHAB       L   HI_LANDFILL_DD      0   NA  PHAB_BACK
							6449        PHAB       L          HI_LAWN      C   NA  PHAB_BACK
							6449        PHAB       L       HI_LAWN_DD      P   NA  PHAB_BACK
							6449        PHAB       L       HI_ORCHARD      0   NA  PHAB_BACK
							6449        PHAB       L    HI_ORCHARD_DD      0   NA  PHAB_BACK
							6449        PHAB       L         HI_OTHER      0   NA  PHAB_BACK
							6449        PHAB       L      HI_OTHER_DD      0   NA  PHAB_BACK
							6449        PHAB       L          HI_PARK      0   NA  PHAB_BACK
							6449        PHAB       L       HI_PARK_DD      0   NA  PHAB_BACK
							6449        PHAB       L       HI_PASTURE      0   NA  PHAB_BACK
							6449        PHAB       L    HI_PASTURE_DD      0   NA  PHAB_BACK
							6449        PHAB       L    HI_POWERLINES      0   NA  PHAB_BACK
							6449        PHAB       L HI_POWERLINES_DD      0   NA  PHAB_BACK
							6449        PHAB       L         HI_ROADS      P   NA  PHAB_BACK
							6449        PHAB       L      HI_ROADS_DD      P   NA  PHAB_BACK
							6449        PHAB       L         HI_WALLS      C   NA  PHAB_BACK
							6449        PHAB       L      HI_WALLS_DD      P   NA  PHAB_BACK
							6449        PHAB       L    HORIZ_DIST_DD    0.3   NA PHAB_FRONT
							6684        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       A       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							6684        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       B         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       B      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       B          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       B       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       B    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       B         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							6684        PHAB       C     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       C         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       C          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       C          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       C       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       C    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       C         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       C         HI_WALLS      0   NA  PHAB_BACK
							6684        PHAB       H     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       H         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       H          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       H       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       H         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       H         HI_WALLS      0   NA  PHAB_BACK
							6684        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       I         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       I          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       I       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       I    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       I         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							6684        PHAB       J     HI_BUILDINGS      0   NA  PHAB_BACK
							6684        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							6684        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							6684        PHAB       J         HI_DOCKS      0   NA  PHAB_BACK
							6684        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							6684        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							6684        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							6684        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							6684        PHAB       J       HI_PASTURE      C   NA  PHAB_BACK
							6684        PHAB       J    HI_POWERLINES      0   NA  PHAB_BACK
							6684        PHAB       J         HI_ROADS      0   NA  PHAB_BACK
							6684        PHAB       J         HI_WALLS      0   NA  PHAB_BACK
							7504        PHAB       A    HI_COMMERCIAL      P   ''     eForms
							7504        PHAB       A         HI_ROADS      C   ''     eForms
							7504        PHAB       B      HI_LANDFILL      C   ''     eForms
							7504        PHAB       B          HI_LAWN      P   ''     eForms
							7504        PHAB       B         HI_ROADS      C   ''     eForms
							7504        PHAB       C         HI_OTHER      P   ''     eForms
							7504        PHAB       C         HI_ROADS      C   ''     eForms
							7504        PHAB       D         HI_ROADS      C   ''     eForms
							7504        PHAB       E         HI_ROADS      C   ''     eForms
							7504        PHAB       F         HI_ROADS      C   ''     eForms
							7504        PHAB       G     HI_BUILDINGS      0   ''     eForms
							7504        PHAB       G         HI_ROADS      C   ''     eForms
							7504        PHAB       H     HI_BUILDINGS      P   ''     eForms
							7504        PHAB       H    HI_COMMERCIAL      0   ''     eForms
							7504        PHAB       H         HI_CROPS      0   ''     eForms
							7504        PHAB       H         HI_DOCKS      C   ''     eForms
							7504        PHAB       H      HI_LANDFILL      0   ''     eForms
							7504        PHAB       H          HI_LAWN      C   ''     eForms
							7504        PHAB       H       HI_ORCHARD      0   ''     eForms
							7504        PHAB       H         HI_OTHER      0   ''     eForms
							7504        PHAB       H          HI_PARK      0   ''     eForms
							7504        PHAB       H       HI_PASTURE      0   ''     eForms
							7504        PHAB       H    HI_POWERLINES      0   ''     eForms
							7504        PHAB       H         HI_ROADS      P   ''     eForms
							7504        PHAB       H         HI_WALLS      0   ''     eForms
							7504        PHAB       I     HI_BUILDINGS      P   ''     eForms
							7504        PHAB       I          HI_LAWN      C   ''     eForms
							7504        PHAB       I    HI_POWERLINES      C   ''     eForms
							7504        PHAB       I         HI_ROADS      C   ''     eForms
							7504        PHAB       J          HI_LAWN      C   ''     eForms
							7504        PHAB       J    HI_POWERLINES      C   ''     eForms
							7504        PHAB       J         HI_ROADS      C   ''     eForms
							7740        PHAB       A     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       A  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       A    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       A HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       A         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       A      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       A         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       A      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       A      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       A   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       A          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       A       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       A       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       A    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       A          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       A       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       A       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       A    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       A    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       A HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       A         HI_ROADS      0   NA  PHAB_BACK
							7740        PHAB       A      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       A         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       A      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       A    HORIZ_DIST_DD   12.0   NA PHAB_FRONT
							7740        PHAB       B     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       B  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       B    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       B HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       B         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       B      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       B         HI_DOCKS      P   NA  PHAB_BACK
							7740        PHAB       B      HI_DOCKS_DD      P   NA  PHAB_BACK
							7740        PHAB       B      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       B   HI_LANDFILL_DD      C   NA  PHAB_BACK
							7740        PHAB       B          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       B       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       B       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       B    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       B          HI_PARK      P   NA  PHAB_BACK
							7740        PHAB       B       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       B       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       B    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       B    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       B HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       B         HI_ROADS      P   NA  PHAB_BACK
							7740        PHAB       B      HI_ROADS_DD      P   NA  PHAB_BACK
							7740        PHAB       B         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       B      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       B    HORIZ_DIST_DD   15.0   NA PHAB_FRONT
							7740        PHAB       C     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       C  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       C    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       C HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       C         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       C      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       C         HI_DOCKS      C   F1  PHAB_BACK
							7740        PHAB       C      HI_DOCKS_DD      C   F1  PHAB_BACK
							7740        PHAB       C      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       C   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       C          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       C       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       C       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       C    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       C          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       C       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       C    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       C    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       C HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       C         HI_ROADS      P   NA  PHAB_BACK
							7740        PHAB       C      HI_ROADS_DD      P   NA  PHAB_BACK
							7740        PHAB       C         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       C      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       C    HORIZ_DIST_DD   30.0   NA PHAB_FRONT
							7740        PHAB       D     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       D  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       D    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       D HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       D         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       D      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       D         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       D      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       D      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       D   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       D          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       D       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       D       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       D    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       D          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       D       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       D       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       D    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       D    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       D HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       D         HI_ROADS      0   NA  PHAB_BACK
							7740        PHAB       D      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       D         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       D      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       D    HORIZ_DIST_DD   13.0   NA PHAB_FRONT
							7740        PHAB       E     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       E  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       E    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       E HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       E         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       E      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       E         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       E      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       E      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       E   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       E          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       E       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       E       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       E    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       E          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       E       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       E       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       E    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       E    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       E HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       E         HI_ROADS      0   NA  PHAB_BACK
							7740        PHAB       E      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       E         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       E      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       E    HORIZ_DIST_DD   10.0   NA PHAB_FRONT
							7740        PHAB       F     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       F  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       F    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       F HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       F         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       F      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       F         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       F      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       F      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       F   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       F          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       F       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       F       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       F    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       F          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       F       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       F       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       F    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       F    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       F HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       F         HI_ROADS      0   NA  PHAB_BACK
							7740        PHAB       F      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       F         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       F      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       F    HORIZ_DIST_DD   10.0   NA PHAB_FRONT
							7740        PHAB       G     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       G  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       G    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       G HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       G         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       G      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       G         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       G      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       G      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       G   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       G          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       G       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       G       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       G    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       G          HI_PARK      P   NA  PHAB_BACK
							7740        PHAB       G       HI_PARK_DD      P   NA  PHAB_BACK
							7740        PHAB       G       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       G    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       G    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       G HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       G         HI_ROADS      P   NA  PHAB_BACK
							7740        PHAB       G      HI_ROADS_DD      P   NA  PHAB_BACK
							7740        PHAB       G         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       G      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       G    HORIZ_DIST_DD   30.0   NA PHAB_FRONT
							7740        PHAB       H     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       H  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       H    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       H HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       H         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       H      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       H         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       H      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       H      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       H   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       H          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       H       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       H       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       H    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       H          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       H       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       H       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       H    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       H    HI_POWERLINES      0   NA  PHAB_BACK
							7740        PHAB       H HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       H         HI_ROADS      0   NA  PHAB_BACK
							7740        PHAB       H      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       H         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       H      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       H    HORIZ_DIST_DD   20.0   NA PHAB_FRONT
							7740        PHAB       I     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       I  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       I    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       I HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       I         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       I      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       I         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       I      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       I      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       I   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       I          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       I       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       I       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       I    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       I          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       I       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       I       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       I    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       I    HI_POWERLINES      P   NA  PHAB_BACK
							7740        PHAB       I HI_POWERLINES_DD      0   NA  PHAB_BACK
							7740        PHAB       I         HI_ROADS      C   NA  PHAB_BACK
							7740        PHAB       I      HI_ROADS_DD      0   NA  PHAB_BACK
							7740        PHAB       I         HI_WALLS      0   NA  PHAB_BACK
							7740        PHAB       I      HI_WALLS_DD      0   NA  PHAB_BACK
							7740        PHAB       I    HORIZ_DIST_DD   18.0   NA PHAB_FRONT
							7740        PHAB       J     HI_BUILDINGS      0   NA  PHAB_BACK
							7740        PHAB       J  HI_BUILDINGS_DD      0   NA  PHAB_BACK
							7740        PHAB       J    HI_COMMERCIAL      0   NA  PHAB_BACK
							7740        PHAB       J HI_COMMERCIAL_DD      0   NA  PHAB_BACK
							7740        PHAB       J         HI_CROPS      0   NA  PHAB_BACK
							7740        PHAB       J      HI_CROPS_DD      0   NA  PHAB_BACK
							7740        PHAB       J         HI_DOCKS      0   NA  PHAB_BACK
							7740        PHAB       J      HI_DOCKS_DD      0   NA  PHAB_BACK
							7740        PHAB       J      HI_LANDFILL      0   NA  PHAB_BACK
							7740        PHAB       J   HI_LANDFILL_DD      0   NA  PHAB_BACK
							7740        PHAB       J          HI_LAWN      0   NA  PHAB_BACK
							7740        PHAB       J       HI_LAWN_DD      0   NA  PHAB_BACK
							7740        PHAB       J       HI_ORCHARD      0   NA  PHAB_BACK
							7740        PHAB       J    HI_ORCHARD_DD      0   NA  PHAB_BACK
							7740        PHAB       J          HI_PARK      0   NA  PHAB_BACK
							7740        PHAB       J       HI_PARK_DD      0   NA  PHAB_BACK
							7740        PHAB       J       HI_PASTURE      0   NA  PHAB_BACK
							7740        PHAB       J    HI_PASTURE_DD      0   NA  PHAB_BACK
							7740        PHAB       J    HI_POWERLINES      P   NA  PHAB_BACK
							7740        PHAB       J HI_POWERLINES_DD      P   NA  PHAB_BACK
							7740        PHAB       J         HI_ROADS      C   NA  PHAB_BACK
							7740        PHAB       J      HI_ROADS_DD      P   NA  PHAB_BACK
							7740        PHAB       J         HI_WALLS      C   F2  PHAB_BACK
							7740        PHAB       J      HI_WALLS_DD      C   F2  PHAB_BACK
							7740        PHAB       J    HORIZ_DIST_DD   18.0   NA PHAB_FRONT
							7808        PHAB       A         HI_CROPS      P   ''     eForms
							7808        PHAB       A      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       A       HI_PASTURE      P   ''     eForms
							7808        PHAB       A    HI_PASTURE_DD      P   ''     eForms
							7808        PHAB       A    HORIZ_DIST_DD    1.8   ''     eForms
							7808        PHAB       B         HI_CROPS      P   ''     eForms
							7808        PHAB       B      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       B       HI_PASTURE      P   ''     eForms
							7808        PHAB       B    HI_PASTURE_DD      P   ''     eForms
							7808        PHAB       B    HORIZ_DIST_DD    1.2   ''     eForms
							7808        PHAB       C     HI_BUILDINGS      0   ''     eForms
							7808        PHAB       C  HI_BUILDINGS_DD      P   ''     eForms
							7808        PHAB       C    HORIZ_DIST_DD    1.2   ''     eForms
							7808        PHAB       D    HORIZ_DIST_DD    0.7   ''     eForms
							7808        PHAB       E      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       E    HORIZ_DIST_DD    0.7   ''     eForms
							7808        PHAB       F         HI_CROPS      P   ''     eForms
							7808        PHAB       F      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       F    HORIZ_DIST_DD    0.9   ''     eForms
							7808        PHAB       G         HI_CROPS      P   ''     eForms
							7808        PHAB       G      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       G    HORIZ_DIST_DD    1.0   ''     eForms
							7808        PHAB       H     HI_BUILDINGS      P   ''     eForms
							7808        PHAB       H  HI_BUILDINGS_DD      P   ''     eForms
							7808        PHAB       H   HI_LANDFILL_DD      P   ''     eForms
							7808        PHAB       H    HI_POWERLINES      P   ''     eForms
							7808        PHAB       H HI_POWERLINES_DD      P   ''     eForms
							7808        PHAB       H         HI_ROADS      P   ''     eForms
							7808        PHAB       H      HI_ROADS_DD      P   ''     eForms
							7808        PHAB       H    HORIZ_DIST_DD      6   ''     eForms
							7808        PHAB       I         HI_CROPS      P   ''     eForms
							7808        PHAB       I      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       I    HORIZ_DIST_DD    10.   ''     eForms
							7808        PHAB       J     HI_BUILDINGS      P   ''     eForms
							7808        PHAB       J  HI_BUILDINGS_DD      P   ''     eForms
							7808        PHAB       J         HI_CROPS      P   ''     eForms
							7808        PHAB       J      HI_CROPS_DD      P   ''     eForms
							7808        PHAB       J    HORIZ_DIST_DD    1.4   ''     eForms
							1000100     PHAB       A      HI_LANDFILL      C   ''     eForms
							6228        PHAB       A         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       D         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       E         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       F         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       G         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       H         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       I         DRAWDOWN    YES   NA PHAB_FRONT
							6228        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       A         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       E         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       F         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       G         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       H         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       I         DRAWDOWN    YES   NA PHAB_FRONT
							6279        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       A         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       D         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       E         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       F         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       G         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       H         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       I         DRAWDOWN    YES   NA PHAB_FRONT
							6302        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							6303        PHAB       A         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       C         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       D         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       E         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       F         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       G         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       H         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       I         DRAWDOWN     NO   NA PHAB_FRONT
							6303        PHAB       J         DRAWDOWN     NO   NA PHAB_FRONT
							6362        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							6399        PHAB       A         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							6399        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							6399        PHAB       D         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       E         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       F         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       G         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       H         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       I         DRAWDOWN     NO   NA PHAB_FRONT
							6399        PHAB       J         DRAWDOWN     NO   NA PHAB_FRONT
							6449        PHAB       A         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       D         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       E         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       F         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       G         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       H         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       I         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       K         DRAWDOWN    YES   NA PHAB_FRONT
							6449        PHAB       L         DRAWDOWN    YES   NA PHAB_FRONT
							6684        PHAB       A         DRAWDOWN     NO   NA PHAB_FRONT
							6684        PHAB       B         DRAWDOWN     NO   NA PHAB_FRONT
							6684        PHAB       C         DRAWDOWN     NO   NA PHAB_FRONT
							6684        PHAB       H         DRAWDOWN     NO   NA PHAB_FRONT
							6684        PHAB       I         DRAWDOWN     NO   NA PHAB_FRONT
							6684        PHAB       J         DRAWDOWN     NO   NA PHAB_FRONT
							7504        PHAB       A         DRAWDOWN     NO   ''     eForms
							7504        PHAB       B         DRAWDOWN     NO   ''     eForms
							7504        PHAB       C         DRAWDOWN     NO   ''     eForms
							7504        PHAB       D         DRAWDOWN     NO   ''     eForms
							7504        PHAB       E         DRAWDOWN     NO   ''     eForms
							7504        PHAB       F         DRAWDOWN     NO   ''     eForms
							7504        PHAB       G         DRAWDOWN     NO   ''     eForms
							7504        PHAB       H         DRAWDOWN     NO   ''     eForms
							7504        PHAB       I         DRAWDOWN     NO   ''     eForms
							7504        PHAB       J         DRAWDOWN     NO   ''     eForms
							7740        PHAB       A         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       B         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       C         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       D         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       E         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       F         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       G         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       H         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       I         DRAWDOWN    YES   NA PHAB_FRONT
							7740        PHAB       J         DRAWDOWN    YES   NA PHAB_FRONT
							7808        PHAB       A         DRAWDOWN      Y   ''     eForms
							7808        PHAB       B         DRAWDOWN      Y   ''     eForms
							7808        PHAB       C         DRAWDOWN      Y   ''     eForms
							7808        PHAB       D         DRAWDOWN      Y   ''     eForms
							7808        PHAB       E         DRAWDOWN      Y   ''     eForms
							7808        PHAB       F         DRAWDOWN      Y   ''     eForms
							7808        PHAB       G         DRAWDOWN      Y   ''     eForms
							7808        PHAB       H         DRAWDOWN      Y   ''     eForms
							7808        PHAB       I         DRAWDOWN      Y   ''     eForms
							7808        PHAB       J         DRAWDOWN      Y   ''     eForms
							1000100     PHAB       A         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       B         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       C         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       E         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       F         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       G         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       H         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       I         DRAWDOWN     NO   ''     eForms
							1000100     PHAB       J         DRAWDOWN     NO   ''     eForms	
					")
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(rc)
}


nlaHumanImpactTest.expectedResultsWithDrawDownNoFillin <- function()
# Returns dataframe of expected calculations based on the test data.
{
	tc <- textConnection("  SITE             METRIC                       VALUE
							6228    HIFPANYCIRCA_DD   0.59999999999999997779554
							6228   HIFPANYCIRCA_RIP   0.69999999999999995559108
							6228   HIFPANYCIRCA_SYN   0.59999999999999997779554
							6228         HIFPANY_DD   0.69999999999999995559108
							6228        HIFPANY_RIP   0.69999999999999995559108
							6228        HIFPANY_SYN   0.69999999999999995559108
							6228      HIIAGCIRCA_DD   0.00000000000000000000000
							6228     HIIAGCIRCA_RIP   0.00000000000000000000000
							6228     HIIAGCIRCA_SYN   0.00000000000000000000000
							6228           HIIAG_DD   0.00000000000000000000000
							6228          HIIAG_RIP   0.00000000000000000000000
							6228          HIIAG_SYN   0.00000000000000000000000
							6228     HIIALLCIRCA_DD   0.90000000000000002220446
							6228    HIIALLCIRCA_RIP   1.19999999999999995559108
							6228    HIIALLCIRCA_SYN   0.90000000000000002220446
							6228          HIIALL_DD   1.00000000000000000000000
							6228         HIIALL_RIP   1.25000000000000000000000
							6228         HIIALL_SYN   1.22366666670000001637675
							6228   HIINONAGCIRCA_DD   0.90000000000000002220446
							6228  HIINONAGCIRCA_RIP   1.19999999999999995559108
							6228  HIINONAGCIRCA_SYN   0.90000000000000002220446
							6228        HIINONAG_DD   1.00000000000000000000000
							6228       HIINONAG_RIP   1.25000000000000000000000
							6228       HIINONAG_SYN   1.22366666670000001637675
							6228           HINAG_DD  30.00000000000000000000000
							6228          HINAG_RIP  30.00000000000000000000000
							6228          HINAG_SYN  30.00000000000000000000000
							6228          HINALL_DD 130.00000000000000000000000
							6228         HINALL_RIP 130.00000000000000000000000
							6228         HINALL_SYN 130.00000000000000000000000
							6228    HINBUILDINGS_DD  10.00000000000000000000000
							6228   HINBUILDINGS_RIP  10.00000000000000000000000
							6228   HINBUILDINGS_SYN  10.00000000000000000000000
							6228   HINCOMMERCIAL_DD  10.00000000000000000000000
							6228  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6228  HINCOMMERCIAL_SYN  10.00000000000000000000000
							6228        HINCROPS_DD  10.00000000000000000000000
							6228       HINCROPS_RIP  10.00000000000000000000000
							6228       HINCROPS_SYN  10.00000000000000000000000
							6228        HINDOCKS_DD  10.00000000000000000000000
							6228       HINDOCKS_RIP  10.00000000000000000000000
							6228       HINDOCKS_SYN  10.00000000000000000000000
							6228     HINLANDFILL_DD  10.00000000000000000000000
							6228    HINLANDFILL_RIP  10.00000000000000000000000
							6228    HINLANDFILL_SYN  10.00000000000000000000000
							6228         HINLAWN_DD  10.00000000000000000000000
							6228        HINLAWN_RIP  10.00000000000000000000000
							6228        HINLAWN_SYN  10.00000000000000000000000
							6228        HINNONAG_DD 100.00000000000000000000000
							6228       HINNONAG_RIP 100.00000000000000000000000
							6228       HINNONAG_SYN 100.00000000000000000000000
							6228      HINORCHARD_DD  10.00000000000000000000000
							6228     HINORCHARD_RIP  10.00000000000000000000000
							6228     HINORCHARD_SYN  10.00000000000000000000000
							6228        HINOTHER_DD  10.00000000000000000000000
							6228       HINOTHER_RIP  10.00000000000000000000000
							6228       HINOTHER_SYN  10.00000000000000000000000
							6228         HINPARK_DD  10.00000000000000000000000
							6228        HINPARK_RIP  10.00000000000000000000000
							6228        HINPARK_SYN  10.00000000000000000000000
							6228      HINPASTURE_DD  10.00000000000000000000000
							6228     HINPASTURE_RIP  10.00000000000000000000000
							6228     HINPASTURE_SYN  10.00000000000000000000000
							6228   HINPOWERLINES_DD  10.00000000000000000000000
							6228  HINPOWERLINES_RIP  10.00000000000000000000000
							6228  HINPOWERLINES_SYN  10.00000000000000000000000
							6228        HINROADS_DD  10.00000000000000000000000
							6228       HINROADS_RIP  10.00000000000000000000000
							6228       HINROADS_SYN  10.00000000000000000000000
							6228        HINWALLS_DD  10.00000000000000000000000
							6228       HINWALLS_RIP  10.00000000000000000000000
							6228       HINWALLS_SYN  10.00000000000000000000000
							6228          HIPWAG_DD   0.00000000000000000000000
							6228         HIPWAG_RIP   0.00000000000000000000000
							6228         HIPWAG_SYN   0.00000000000000000000000
							6228         HIPWALL_DD   0.07692307689999999809061
							6228        HIPWALL_RIP   0.09615384620000000381879
							6228        HIPWALL_SYN   0.09412820510000000084894
							6228   HIPWBUILDINGS_DD   0.00000000000000000000000
							6228  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6228  HIPWBUILDINGS_SYN   0.00000000000000000000000
							6228  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6228 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6228 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6228       HIPWCROPS_DD   0.00000000000000000000000
							6228      HIPWCROPS_RIP   0.00000000000000000000000
							6228      HIPWCROPS_SYN   0.00000000000000000000000
							6228       HIPWDOCKS_DD   0.55000000000000004440892
							6228      HIPWDOCKS_RIP   0.55000000000000004440892
							6228      HIPWDOCKS_SYN   0.55000000000000004440892
							6228    HIPWLANDFILL_DD   0.05000000000000000277556
							6228   HIPWLANDFILL_RIP   0.10000000000000000555112
							6228   HIPWLANDFILL_SYN   0.09500000000000000111022
							6228        HIPWLAWN_DD   0.40000000000000002220446
							6228       HIPWLAWN_RIP   0.59999999999999997779554
							6228       HIPWLAWN_SYN   0.57866666669999999861318
							6228       HIPWNONAG_DD   0.10000000000000000555112
							6228      HIPWNONAG_RIP   0.12500000000000000000000
							6228      HIPWNONAG_SYN   0.12236666670000000134433
							6228     HIPWORCHARD_DD   0.00000000000000000000000
							6228    HIPWORCHARD_RIP   0.00000000000000000000000
							6228    HIPWORCHARD_SYN   0.00000000000000000000000
							6228       HIPWOTHER_DD   0.00000000000000000000000
							6228      HIPWOTHER_RIP   0.00000000000000000000000
							6228      HIPWOTHER_SYN   0.00000000000000000000000
							6228        HIPWPARK_DD   0.00000000000000000000000
							6228       HIPWPARK_RIP   0.00000000000000000000000
							6228       HIPWPARK_SYN   0.00000000000000000000000
							6228     HIPWPASTURE_DD   0.00000000000000000000000
							6228    HIPWPASTURE_RIP   0.00000000000000000000000
							6228    HIPWPASTURE_SYN   0.00000000000000000000000
							6228  HIPWPOWERLINES_DD   0.00000000000000000000000
							6228 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6228 HIPWPOWERLINES_SYN   0.00000000000000000000000
							6228       HIPWROADS_DD   0.00000000000000000000000
							6228      HIPWROADS_RIP   0.00000000000000000000000
							6228      HIPWROADS_SYN   0.00000000000000000000000
							6228       HIPWWALLS_DD   0.00000000000000000000000
							6228      HIPWWALLS_RIP   0.00000000000000000000000
							6228      HIPWWALLS_SYN   0.00000000000000000000000
							6279    HIFPANYCIRCA_DD   0.20000000000000001110223
							6279   HIFPANYCIRCA_RIP   0.29999999999999998889777
							6279   HIFPANYCIRCA_SYN   0.20000000000000001110223
							6279         HIFPANY_DD   1.00000000000000000000000
							6279        HIFPANY_RIP   1.00000000000000000000000
							6279        HIFPANY_SYN   1.00000000000000000000000
							6279      HIIAGCIRCA_DD   0.00000000000000000000000
							6279     HIIAGCIRCA_RIP   0.00000000000000000000000
							6279     HIIAGCIRCA_SYN   0.00000000000000000000000
							6279           HIIAG_DD   0.00000000000000000000000
							6279          HIIAG_RIP   0.00000000000000000000000
							6279          HIIAG_SYN   0.00000000000000000000000
							6279     HIIALLCIRCA_DD   0.29999999999999998889777
							6279    HIIALLCIRCA_RIP   0.71111111110000002621945
							6279    HIIALLCIRCA_SYN   0.29999999999999998889777
							6279          HIIALL_DD   2.41666666669999985472828
							6279         HIIALL_RIP   2.62222222220000000802997
							6279         HIIALL_SYN   2.60351851850000004517938
							6279   HIINONAGCIRCA_DD   0.29999999999999998889777
							6279  HIINONAGCIRCA_RIP   0.71111111110000002621945
							6279  HIINONAGCIRCA_SYN   0.29999999999999998889777
							6279        HIINONAG_DD   2.41666666669999985472828
							6279       HIINONAG_RIP   2.62222222220000000802997
							6279       HIINONAG_SYN   2.60351851850000004517938
							6279           HINAG_DD  30.00000000000000000000000
							6279          HINAG_RIP  30.00000000000000000000000
							6279          HINAG_SYN  30.00000000000000000000000
							6279          HINALL_DD 119.00000000000000000000000
							6279         HINALL_RIP 119.00000000000000000000000
							6279         HINALL_SYN 119.00000000000000000000000
							6279    HINBUILDINGS_DD  10.00000000000000000000000
							6279   HINBUILDINGS_RIP  10.00000000000000000000000
							6279   HINBUILDINGS_SYN  10.00000000000000000000000
							6279   HINCOMMERCIAL_DD  10.00000000000000000000000
							6279  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6279  HINCOMMERCIAL_SYN  10.00000000000000000000000
							6279        HINCROPS_DD  10.00000000000000000000000
							6279       HINCROPS_RIP  10.00000000000000000000000
							6279       HINCROPS_SYN  10.00000000000000000000000
							6279        HINDOCKS_DD  10.00000000000000000000000
							6279       HINDOCKS_RIP  10.00000000000000000000000
							6279       HINDOCKS_SYN  10.00000000000000000000000
							6279     HINLANDFILL_DD  10.00000000000000000000000
							6279    HINLANDFILL_RIP  10.00000000000000000000000
							6279    HINLANDFILL_SYN  10.00000000000000000000000
							6279         HINLAWN_DD  10.00000000000000000000000
							6279        HINLAWN_RIP  10.00000000000000000000000
							6279        HINLAWN_SYN  10.00000000000000000000000
							6279        HINNONAG_DD  89.00000000000000000000000
							6279       HINNONAG_RIP  89.00000000000000000000000
							6279       HINNONAG_SYN  89.00000000000000000000000
							6279      HINORCHARD_DD  10.00000000000000000000000
							6279     HINORCHARD_RIP  10.00000000000000000000000
							6279     HINORCHARD_SYN  10.00000000000000000000000
							6279        HINOTHER_DD   0.00000000000000000000000
							6279       HINOTHER_RIP   0.00000000000000000000000
							6279       HINOTHER_SYN   0.00000000000000000000000
							6279         HINPARK_DD  10.00000000000000000000000
							6279        HINPARK_RIP  10.00000000000000000000000
							6279        HINPARK_SYN  10.00000000000000000000000
							6279      HINPASTURE_DD  10.00000000000000000000000
							6279     HINPASTURE_RIP  10.00000000000000000000000
							6279     HINPASTURE_SYN  10.00000000000000000000000
							6279   HINPOWERLINES_DD  10.00000000000000000000000
							6279  HINPOWERLINES_RIP  10.00000000000000000000000
							6279  HINPOWERLINES_SYN  10.00000000000000000000000
							6279        HINROADS_DD  10.00000000000000000000000
							6279       HINROADS_RIP  10.00000000000000000000000
							6279       HINROADS_SYN  10.00000000000000000000000
							6279        HINWALLS_DD   9.00000000000000000000000
							6279       HINWALLS_RIP   9.00000000000000000000000
							6279       HINWALLS_SYN   9.00000000000000000000000
							6279          HIPWAG_DD   0.00000000000000000000000
							6279         HIPWAG_RIP   0.00000000000000000000000
							6279         HIPWAG_SYN   0.00000000000000000000000
							6279         HIPWALL_DD   0.20168067230000000233936
							6279        HIPWALL_RIP   0.21848739500000000091084
							6279        HIPWALL_SYN   0.21694677870000000918793
							6279   HIPWBUILDINGS_DD   0.14999999999999999444888
							6279  HIPWBUILDINGS_RIP   0.20000000000000001110223
							6279  HIPWBUILDINGS_SYN   0.19166666669999998795504
							6279  HIPWCOMMERCIAL_DD   0.10000000000000000555112
							6279 HIPWCOMMERCIAL_RIP   0.10000000000000000555112
							6279 HIPWCOMMERCIAL_SYN   0.10000000000000000555112
							6279       HIPWCROPS_DD   0.00000000000000000000000
							6279      HIPWCROPS_RIP   0.00000000000000000000000
							6279      HIPWCROPS_SYN   0.00000000000000000000000
							6279       HIPWDOCKS_DD   0.25000000000000000000000
							6279      HIPWDOCKS_RIP   0.25000000000000000000000
							6279      HIPWDOCKS_SYN   0.25000000000000000000000
							6279    HIPWLANDFILL_DD   0.05000000000000000277556
							6279   HIPWLANDFILL_RIP   0.05000000000000000277556
							6279   HIPWLANDFILL_SYN   0.05000000000000000277556
							6279        HIPWLAWN_DD   0.10000000000000000555112
							6279       HIPWLAWN_RIP   0.10000000000000000555112
							6279       HIPWLAWN_SYN   0.10000000000000000555112
							6279       HIPWNONAG_DD   0.26966292130000002469359
							6279      HIPWNONAG_RIP   0.29213483150000002508406
							6279      HIPWNONAG_SYN   0.29007490639999999881837
							6279     HIPWORCHARD_DD   0.00000000000000000000000
							6279    HIPWORCHARD_RIP   0.00000000000000000000000
							6279    HIPWORCHARD_SYN   0.00000000000000000000000
							6279       HIPWOTHER_DD                          NA
							6279      HIPWOTHER_RIP                          NA
							6279      HIPWOTHER_SYN                          NA
							6279        HIPWPARK_DD   0.55000000000000004440892
							6279       HIPWPARK_RIP   0.55000000000000004440892
							6279       HIPWPARK_SYN   0.55000000000000004440892
							6279     HIPWPASTURE_DD   0.00000000000000000000000
							6279    HIPWPASTURE_RIP   0.00000000000000000000000
							6279    HIPWPASTURE_SYN   0.00000000000000000000000
							6279  HIPWPOWERLINES_DD   0.50000000000000000000000
							6279 HIPWPOWERLINES_RIP   0.55000000000000004440892
							6279 HIPWPOWERLINES_SYN   0.54666666669999997019147
							6279       HIPWROADS_DD   0.55000000000000004440892
							6279      HIPWROADS_RIP   0.59999999999999997779554
							6279      HIPWROADS_SYN   0.59666666670000001460039
							6279       HIPWWALLS_DD   0.16666666669999999350615
							6279      HIPWWALLS_RIP   0.22222222220000001358109
							6279      HIPWWALLS_SYN   0.21851851850000000854202
							6302    HIFPANYCIRCA_DD   1.00000000000000000000000
							6302   HIFPANYCIRCA_RIP   0.33333333329999997873827
							6302   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6302         HIFPANY_DD   1.00000000000000000000000
							6302        HIFPANY_RIP   0.44444444440000002716218
							6302        HIFPANY_SYN   1.00000000000000000000000
							6302      HIIAGCIRCA_DD   0.00000000000000000000000
							6302     HIIAGCIRCA_RIP   0.00000000000000000000000
							6302     HIIAGCIRCA_SYN   0.00000000000000000000000
							6302           HIIAG_DD   0.00000000000000000000000
							6302          HIIAG_RIP   0.00000000000000000000000
							6302          HIIAG_SYN   0.00000000000000000000000
							6302     HIIALLCIRCA_DD   1.01388888889999995157609
							6302    HIIALLCIRCA_RIP   0.79166666669999996575058
							6302    HIIALLCIRCA_SYN   1.01388888889999995157609
							6302          HIIALL_DD   1.12500000000000000000000
							6302         HIIALL_RIP   0.90277777780000001417449
							6302         HIIALL_SYN   1.29907407409999997582872
							6302   HIINONAGCIRCA_DD   1.01388888889999995157609
							6302  HIINONAGCIRCA_RIP   0.79166666669999996575058
							6302  HIINONAGCIRCA_SYN   1.01388888889999995157609
							6302        HIINONAG_DD   1.12500000000000000000000
							6302       HIINONAG_RIP   0.90277777780000001417449
							6302       HIINONAG_SYN   1.29907407409999997582872
							6302           HINAG_DD  27.00000000000000000000000
							6302          HINAG_RIP  27.00000000000000000000000
							6302          HINAG_SYN  27.00000000000000000000000
							6302          HINALL_DD 116.00000000000000000000000
							6302         HINALL_RIP 115.00000000000000000000000
							6302         HINALL_SYN 116.00000000000000000000000
							6302    HINBUILDINGS_DD   9.00000000000000000000000
							6302   HINBUILDINGS_RIP   8.00000000000000000000000
							6302   HINBUILDINGS_SYN   9.00000000000000000000000
							6302   HINCOMMERCIAL_DD   9.00000000000000000000000
							6302  HINCOMMERCIAL_RIP   9.00000000000000000000000
							6302  HINCOMMERCIAL_SYN   9.00000000000000000000000
							6302        HINCROPS_DD   9.00000000000000000000000
							6302       HINCROPS_RIP   9.00000000000000000000000
							6302       HINCROPS_SYN   9.00000000000000000000000
							6302        HINDOCKS_DD   9.00000000000000000000000
							6302       HINDOCKS_RIP   9.00000000000000000000000
							6302       HINDOCKS_SYN   9.00000000000000000000000
							6302     HINLANDFILL_DD   9.00000000000000000000000
							6302    HINLANDFILL_RIP   9.00000000000000000000000
							6302    HINLANDFILL_SYN   9.00000000000000000000000
							6302         HINLAWN_DD   9.00000000000000000000000
							6302        HINLAWN_RIP   9.00000000000000000000000
							6302        HINLAWN_SYN   9.00000000000000000000000
							6302        HINNONAG_DD  89.00000000000000000000000
							6302       HINNONAG_RIP  88.00000000000000000000000
							6302       HINNONAG_SYN  89.00000000000000000000000
							6302      HINORCHARD_DD   9.00000000000000000000000
							6302     HINORCHARD_RIP   9.00000000000000000000000
							6302     HINORCHARD_SYN   9.00000000000000000000000
							6302        HINOTHER_DD   9.00000000000000000000000
							6302       HINOTHER_RIP   8.00000000000000000000000
							6302       HINOTHER_SYN   9.00000000000000000000000
							6302         HINPARK_DD   9.00000000000000000000000
							6302        HINPARK_RIP   9.00000000000000000000000
							6302        HINPARK_SYN   9.00000000000000000000000
							6302      HINPASTURE_DD   9.00000000000000000000000
							6302     HINPASTURE_RIP   9.00000000000000000000000
							6302     HINPASTURE_SYN   9.00000000000000000000000
							6302   HINPOWERLINES_DD   9.00000000000000000000000
							6302  HINPOWERLINES_RIP   9.00000000000000000000000
							6302  HINPOWERLINES_SYN   9.00000000000000000000000
							6302        HINROADS_DD   9.00000000000000000000000
							6302       HINROADS_RIP   9.00000000000000000000000
							6302       HINROADS_SYN   9.00000000000000000000000
							6302        HINWALLS_DD   8.00000000000000000000000
							6302       HINWALLS_RIP   9.00000000000000000000000
							6302       HINWALLS_SYN   8.00000000000000000000000
							6302          HIPWAG_DD   0.00000000000000000000000
							6302         HIPWAG_RIP   0.00000000000000000000000
							6302         HIPWAG_SYN   0.00000000000000000000000
							6302         HIPWALL_DD   0.08620689659999999920892
							6302        HIPWALL_RIP   0.06956521740000000397774
							6302        HIPWALL_SYN   0.09971264370000000598626
							6302   HIPWBUILDINGS_DD   0.00000000000000000000000
							6302  HIPWBUILDINGS_RIP   0.12500000000000000000000
							6302  HIPWBUILDINGS_SYN   0.09629629630000000051204
							6302  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6302 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6302 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6302       HIPWCROPS_DD   0.00000000000000000000000
							6302      HIPWCROPS_RIP   0.00000000000000000000000
							6302      HIPWCROPS_SYN   0.00000000000000000000000
							6302       HIPWDOCKS_DD   0.05555555560000000059340
							6302      HIPWDOCKS_RIP   0.05555555560000000059340
							6302      HIPWDOCKS_SYN   0.05555555560000000059340
							6302    HIPWLANDFILL_DD   0.77777777780000001417449
							6302   HIPWLANDFILL_RIP   0.00000000000000000000000
							6302   HIPWLANDFILL_SYN   0.77777777780000001417449
							6302        HIPWLAWN_DD   0.00000000000000000000000
							6302       HIPWLAWN_RIP   0.11111111110000000679054
							6302       HIPWLAWN_SYN   0.00000000000000000000000
							6302       HIPWNONAG_DD   0.11235955059999999661180
							6302      HIPWNONAG_RIP   0.09090909090000000303267
							6302      HIPWNONAG_SYN   0.12996254679999999503970
							6302     HIPWORCHARD_DD   0.00000000000000000000000
							6302    HIPWORCHARD_RIP   0.00000000000000000000000
							6302    HIPWORCHARD_SYN   0.00000000000000000000000
							6302       HIPWOTHER_DD   0.11111111110000000679054
							6302      HIPWOTHER_RIP   0.00000000000000000000000
							6302      HIPWOTHER_SYN   0.11111111110000000679054
							6302        HIPWPARK_DD   0.05555555560000000059340
							6302       HIPWPARK_RIP   0.11111111110000000679054
							6302       HIPWPARK_SYN   0.05555555560000000059340
							6302     HIPWPASTURE_DD   0.00000000000000000000000
							6302    HIPWPASTURE_RIP   0.00000000000000000000000
							6302    HIPWPASTURE_SYN   0.00000000000000000000000
							6302  HIPWPOWERLINES_DD   0.00000000000000000000000
							6302 HIPWPOWERLINES_RIP   0.22222222220000001358109
							6302 HIPWPOWERLINES_SYN   0.07777777780000000307226
							6302       HIPWROADS_DD   0.00000000000000000000000
							6302      HIPWROADS_RIP   0.16666666669999999350615
							6302      HIPWROADS_SYN   0.00000000000000000000000
							6302       HIPWWALLS_DD   0.12500000000000000000000
							6302      HIPWWALLS_RIP   0.11111111110000000679054
							6302      HIPWWALLS_SYN   0.12500000000000000000000
							6303    HIFPANYCIRCA_DD                          NA
							6303   HIFPANYCIRCA_RIP   0.50000000000000000000000
							6303   HIFPANYCIRCA_SYN                          NA
							6303         HIFPANY_DD                          NA
							6303        HIFPANY_RIP   0.80000000000000004440892
							6303        HIFPANY_SYN                          NA
							6303      HIIAGCIRCA_DD                          NA
							6303     HIIAGCIRCA_RIP   0.00000000000000000000000
							6303     HIIAGCIRCA_SYN                          NA
							6303           HIIAG_DD                          NA
							6303          HIIAG_RIP   0.00000000000000000000000
							6303          HIIAG_SYN                          NA
							6303     HIIALLCIRCA_DD                          NA
							6303    HIIALLCIRCA_RIP   1.10000000000000008881784
							6303    HIIALLCIRCA_SYN                          NA
							6303          HIIALL_DD                          NA
							6303         HIIALL_RIP   1.44999999999999995559108
							6303         HIIALL_SYN                          NA
							6303   HIINONAGCIRCA_DD                          NA
							6303  HIINONAGCIRCA_RIP   1.10000000000000008881784
							6303  HIINONAGCIRCA_SYN                          NA
							6303        HIINONAG_DD                          NA
							6303       HIINONAG_RIP   1.44999999999999995559108
							6303       HIINONAG_SYN                          NA
							6303           HINAG_DD   0.00000000000000000000000
							6303          HINAG_RIP  30.00000000000000000000000
							6303          HINAG_SYN   0.00000000000000000000000
							6303          HINALL_DD   0.00000000000000000000000
							6303         HINALL_RIP 130.00000000000000000000000
							6303         HINALL_SYN   0.00000000000000000000000
							6303    HINBUILDINGS_DD   0.00000000000000000000000
							6303   HINBUILDINGS_RIP  10.00000000000000000000000
							6303   HINBUILDINGS_SYN   0.00000000000000000000000
							6303   HINCOMMERCIAL_DD   0.00000000000000000000000
							6303  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6303  HINCOMMERCIAL_SYN   0.00000000000000000000000
							6303        HINCROPS_DD   0.00000000000000000000000
							6303       HINCROPS_RIP  10.00000000000000000000000
							6303       HINCROPS_SYN   0.00000000000000000000000
							6303        HINDOCKS_DD   0.00000000000000000000000
							6303       HINDOCKS_RIP  10.00000000000000000000000
							6303       HINDOCKS_SYN   0.00000000000000000000000
							6303     HINLANDFILL_DD   0.00000000000000000000000
							6303    HINLANDFILL_RIP  10.00000000000000000000000
							6303    HINLANDFILL_SYN   0.00000000000000000000000
							6303         HINLAWN_DD   0.00000000000000000000000
							6303        HINLAWN_RIP  10.00000000000000000000000
							6303        HINLAWN_SYN   0.00000000000000000000000
							6303        HINNONAG_DD   0.00000000000000000000000
							6303       HINNONAG_RIP 100.00000000000000000000000
							6303       HINNONAG_SYN   0.00000000000000000000000
							6303      HINORCHARD_DD   0.00000000000000000000000
							6303     HINORCHARD_RIP  10.00000000000000000000000
							6303     HINORCHARD_SYN   0.00000000000000000000000
							6303        HINOTHER_DD   0.00000000000000000000000
							6303       HINOTHER_RIP  10.00000000000000000000000
							6303       HINOTHER_SYN   0.00000000000000000000000
							6303         HINPARK_DD   0.00000000000000000000000
							6303        HINPARK_RIP  10.00000000000000000000000
							6303        HINPARK_SYN   0.00000000000000000000000
							6303      HINPASTURE_DD   0.00000000000000000000000
							6303     HINPASTURE_RIP  10.00000000000000000000000
							6303     HINPASTURE_SYN   0.00000000000000000000000
							6303   HINPOWERLINES_DD   0.00000000000000000000000
							6303  HINPOWERLINES_RIP  10.00000000000000000000000
							6303  HINPOWERLINES_SYN   0.00000000000000000000000
							6303        HINROADS_DD   0.00000000000000000000000
							6303       HINROADS_RIP  10.00000000000000000000000
							6303       HINROADS_SYN   0.00000000000000000000000
							6303        HINWALLS_DD   0.00000000000000000000000
							6303       HINWALLS_RIP  10.00000000000000000000000
							6303       HINWALLS_SYN   0.00000000000000000000000
							6303          HIPWAG_DD                          NA
							6303         HIPWAG_RIP   0.00000000000000000000000
							6303         HIPWAG_SYN                          NA
							6303         HIPWALL_DD                          NA
							6303        HIPWALL_RIP   0.11153846150000000514435
							6303        HIPWALL_SYN                          NA
							6303   HIPWBUILDINGS_DD                          NA
							6303  HIPWBUILDINGS_RIP   0.14999999999999999444888
							6303  HIPWBUILDINGS_SYN                          NA
							6303  HIPWCOMMERCIAL_DD                          NA
							6303 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6303 HIPWCOMMERCIAL_SYN                          NA
							6303       HIPWCROPS_DD                          NA
							6303      HIPWCROPS_RIP   0.00000000000000000000000
							6303      HIPWCROPS_SYN                          NA
							6303       HIPWDOCKS_DD                          NA
							6303      HIPWDOCKS_RIP   0.05000000000000000277556
							6303      HIPWDOCKS_SYN                          NA
							6303    HIPWLANDFILL_DD                          NA
							6303   HIPWLANDFILL_RIP   0.65000000000000002220446
							6303   HIPWLANDFILL_SYN                          NA
							6303        HIPWLAWN_DD                          NA
							6303       HIPWLAWN_RIP   0.20000000000000001110223
							6303       HIPWLAWN_SYN                          NA
							6303       HIPWNONAG_DD                          NA
							6303      HIPWNONAG_RIP   0.14499999999999999000799
							6303      HIPWNONAG_SYN                          NA
							6303     HIPWORCHARD_DD                          NA
							6303    HIPWORCHARD_RIP   0.00000000000000000000000
							6303    HIPWORCHARD_SYN                          NA
							6303       HIPWOTHER_DD                          NA
							6303      HIPWOTHER_RIP   0.00000000000000000000000
							6303      HIPWOTHER_SYN                          NA
							6303        HIPWPARK_DD                          NA
							6303       HIPWPARK_RIP   0.05000000000000000277556
							6303       HIPWPARK_SYN                          NA
							6303     HIPWPASTURE_DD                          NA
							6303    HIPWPASTURE_RIP   0.00000000000000000000000
							6303    HIPWPASTURE_SYN                          NA
							6303  HIPWPOWERLINES_DD                          NA
							6303 HIPWPOWERLINES_RIP   0.14999999999999999444888
							6303 HIPWPOWERLINES_SYN                          NA
							6303       HIPWROADS_DD                          NA
							6303      HIPWROADS_RIP   0.10000000000000000555112
							6303      HIPWROADS_SYN                          NA
							6303       HIPWWALLS_DD                          NA
							6303      HIPWWALLS_RIP   0.10000000000000000555112
							6303      HIPWWALLS_SYN                          NA
							6362    HIFPANYCIRCA_DD   0.00000000000000000000000
							6362   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6362   HIFPANYCIRCA_SYN   0.00000000000000000000000
							6362         HIFPANY_DD   0.00000000000000000000000
							6362        HIFPANY_RIP   1.00000000000000000000000
							6362        HIFPANY_SYN   0.00000000000000000000000
							6362      HIIAGCIRCA_DD   0.00000000000000000000000
							6362     HIIAGCIRCA_RIP   1.00000000000000000000000
							6362     HIIAGCIRCA_SYN   0.00000000000000000000000
							6362           HIIAG_DD   0.00000000000000000000000
							6362          HIIAG_RIP   1.00000000000000000000000
							6362          HIIAG_SYN   0.00000000000000000000000
							6362     HIIALLCIRCA_DD   0.00000000000000000000000
							6362    HIIALLCIRCA_RIP   1.00000000000000000000000
							6362    HIIALLCIRCA_SYN   0.00000000000000000000000
							6362          HIIALL_DD   0.00000000000000000000000
							6362         HIIALL_RIP   1.00000000000000000000000
							6362         HIIALL_SYN   0.00000000000000000000000
							6362   HIINONAGCIRCA_DD   0.00000000000000000000000
							6362  HIINONAGCIRCA_RIP   0.00000000000000000000000
							6362  HIINONAGCIRCA_SYN   0.00000000000000000000000
							6362        HIINONAG_DD   0.00000000000000000000000
							6362       HIINONAG_RIP   0.00000000000000000000000
							6362       HIINONAG_SYN   0.00000000000000000000000
							6362           HINAG_DD   3.00000000000000000000000
							6362          HINAG_RIP   3.00000000000000000000000
							6362          HINAG_SYN   3.00000000000000000000000
							6362          HINALL_DD  13.00000000000000000000000
							6362         HINALL_RIP  13.00000000000000000000000
							6362         HINALL_SYN  13.00000000000000000000000
							6362    HINBUILDINGS_DD   1.00000000000000000000000
							6362   HINBUILDINGS_RIP   1.00000000000000000000000
							6362   HINBUILDINGS_SYN   1.00000000000000000000000
							6362   HINCOMMERCIAL_DD   1.00000000000000000000000
							6362  HINCOMMERCIAL_RIP   1.00000000000000000000000
							6362  HINCOMMERCIAL_SYN   1.00000000000000000000000
							6362        HINCROPS_DD   1.00000000000000000000000
							6362       HINCROPS_RIP   1.00000000000000000000000
							6362       HINCROPS_SYN   1.00000000000000000000000
							6362        HINDOCKS_DD   1.00000000000000000000000
							6362       HINDOCKS_RIP   1.00000000000000000000000
							6362       HINDOCKS_SYN   1.00000000000000000000000
							6362     HINLANDFILL_DD   1.00000000000000000000000
							6362    HINLANDFILL_RIP   1.00000000000000000000000
							6362    HINLANDFILL_SYN   1.00000000000000000000000
							6362         HINLAWN_DD   1.00000000000000000000000
							6362        HINLAWN_RIP   1.00000000000000000000000
							6362        HINLAWN_SYN   1.00000000000000000000000
							6362        HINNONAG_DD  10.00000000000000000000000
							6362       HINNONAG_RIP  10.00000000000000000000000
							6362       HINNONAG_SYN  10.00000000000000000000000
							6362      HINORCHARD_DD   1.00000000000000000000000
							6362     HINORCHARD_RIP   1.00000000000000000000000
							6362     HINORCHARD_SYN   1.00000000000000000000000
							6362        HINOTHER_DD   1.00000000000000000000000
							6362       HINOTHER_RIP   1.00000000000000000000000
							6362       HINOTHER_SYN   1.00000000000000000000000
							6362         HINPARK_DD   1.00000000000000000000000
							6362        HINPARK_RIP   1.00000000000000000000000
							6362        HINPARK_SYN   1.00000000000000000000000
							6362      HINPASTURE_DD   1.00000000000000000000000
							6362     HINPASTURE_RIP   1.00000000000000000000000
							6362     HINPASTURE_SYN   1.00000000000000000000000
							6362   HINPOWERLINES_DD   1.00000000000000000000000
							6362  HINPOWERLINES_RIP   1.00000000000000000000000
							6362  HINPOWERLINES_SYN   1.00000000000000000000000
							6362        HINROADS_DD   1.00000000000000000000000
							6362       HINROADS_RIP   1.00000000000000000000000
							6362       HINROADS_SYN   1.00000000000000000000000
							6362        HINWALLS_DD   1.00000000000000000000000
							6362       HINWALLS_RIP   1.00000000000000000000000
							6362       HINWALLS_SYN   1.00000000000000000000000
							6362          HIPWAG_DD   0.00000000000000000000000
							6362         HIPWAG_RIP   0.33333333329999997873827
							6362         HIPWAG_SYN   0.00000000000000000000000
							6362         HIPWALL_DD   0.00000000000000000000000
							6362        HIPWALL_RIP   0.07692307689999999809061
							6362        HIPWALL_SYN   0.00000000000000000000000
							6362   HIPWBUILDINGS_DD   0.00000000000000000000000
							6362  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6362  HIPWBUILDINGS_SYN   0.00000000000000000000000
							6362  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6362 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6362 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6362       HIPWCROPS_DD   0.00000000000000000000000
							6362      HIPWCROPS_RIP   0.00000000000000000000000
							6362      HIPWCROPS_SYN   0.00000000000000000000000
							6362       HIPWDOCKS_DD   0.00000000000000000000000
							6362      HIPWDOCKS_RIP   0.00000000000000000000000
							6362      HIPWDOCKS_SYN   0.00000000000000000000000
							6362    HIPWLANDFILL_DD   0.00000000000000000000000
							6362   HIPWLANDFILL_RIP   0.00000000000000000000000
							6362   HIPWLANDFILL_SYN   0.00000000000000000000000
							6362        HIPWLAWN_DD   0.00000000000000000000000
							6362       HIPWLAWN_RIP   0.00000000000000000000000
							6362       HIPWLAWN_SYN   0.00000000000000000000000
							6362       HIPWNONAG_DD   0.00000000000000000000000
							6362      HIPWNONAG_RIP   0.00000000000000000000000
							6362      HIPWNONAG_SYN   0.00000000000000000000000
							6362     HIPWORCHARD_DD   0.00000000000000000000000
							6362    HIPWORCHARD_RIP   0.00000000000000000000000
							6362    HIPWORCHARD_SYN   0.00000000000000000000000
							6362       HIPWOTHER_DD   0.00000000000000000000000
							6362      HIPWOTHER_RIP   0.00000000000000000000000
							6362      HIPWOTHER_SYN   0.00000000000000000000000
							6362        HIPWPARK_DD   0.00000000000000000000000
							6362       HIPWPARK_RIP   0.00000000000000000000000
							6362       HIPWPARK_SYN   0.00000000000000000000000
							6362     HIPWPASTURE_DD   0.00000000000000000000000
							6362    HIPWPASTURE_RIP   1.00000000000000000000000
							6362    HIPWPASTURE_SYN   0.00000000000000000000000
							6362  HIPWPOWERLINES_DD   0.00000000000000000000000
							6362 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6362 HIPWPOWERLINES_SYN   0.00000000000000000000000
							6362       HIPWROADS_DD   0.00000000000000000000000
							6362      HIPWROADS_RIP   0.00000000000000000000000
							6362      HIPWROADS_SYN   0.00000000000000000000000
							6362       HIPWWALLS_DD   0.00000000000000000000000
							6362      HIPWWALLS_RIP   0.00000000000000000000000
							6362      HIPWWALLS_SYN   0.00000000000000000000000
							6399    HIFPANYCIRCA_DD   1.00000000000000000000000
							6399   HIFPANYCIRCA_RIP   0.69999999999999995559108
							6399   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6399         HIFPANY_DD   1.00000000000000000000000
							6399        HIFPANY_RIP   0.80000000000000004440892
							6399        HIFPANY_SYN   1.00000000000000000000000
							6399      HIIAGCIRCA_DD   0.00000000000000000000000
							6399     HIIAGCIRCA_RIP   0.00000000000000000000000
							6399     HIIAGCIRCA_SYN   0.00000000000000000000000
							6399           HIIAG_DD   0.00000000000000000000000
							6399          HIIAG_RIP   0.00000000000000000000000
							6399          HIIAG_SYN   0.00000000000000000000000
							6399     HIIALLCIRCA_DD   2.50000000000000000000000
							6399    HIIALLCIRCA_RIP   1.50000000000000000000000
							6399    HIIALLCIRCA_SYN   2.50000000000000000000000
							6399          HIIALL_DD   4.25000000000000000000000
							6399         HIIALL_RIP   2.64999999999999991118216
							6399         HIIALL_SYN   4.48333333330000005645388
							6399   HIINONAGCIRCA_DD   2.50000000000000000000000
							6399  HIINONAGCIRCA_RIP   1.50000000000000000000000
							6399  HIINONAGCIRCA_SYN   2.50000000000000000000000
							6399        HIINONAG_DD   4.25000000000000000000000
							6399       HIINONAG_RIP   2.64999999999999991118216
							6399       HIINONAG_SYN   4.48333333330000005645388
							6399           HINAG_DD   6.00000000000000000000000
							6399          HINAG_RIP  30.00000000000000000000000
							6399          HINAG_SYN   6.00000000000000000000000
							6399          HINALL_DD  26.00000000000000000000000
							6399         HINALL_RIP 129.00000000000000000000000
							6399         HINALL_SYN  26.00000000000000000000000
							6399    HINBUILDINGS_DD   2.00000000000000000000000
							6399   HINBUILDINGS_RIP  10.00000000000000000000000
							6399   HINBUILDINGS_SYN   2.00000000000000000000000
							6399   HINCOMMERCIAL_DD   2.00000000000000000000000
							6399  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6399  HINCOMMERCIAL_SYN   2.00000000000000000000000
							6399        HINCROPS_DD   2.00000000000000000000000
							6399       HINCROPS_RIP  10.00000000000000000000000
							6399       HINCROPS_SYN   2.00000000000000000000000
							6399        HINDOCKS_DD   2.00000000000000000000000
							6399       HINDOCKS_RIP  10.00000000000000000000000
							6399       HINDOCKS_SYN   2.00000000000000000000000
							6399     HINLANDFILL_DD   2.00000000000000000000000
							6399    HINLANDFILL_RIP  10.00000000000000000000000
							6399    HINLANDFILL_SYN   2.00000000000000000000000
							6399         HINLAWN_DD   2.00000000000000000000000
							6399        HINLAWN_RIP  10.00000000000000000000000
							6399        HINLAWN_SYN   2.00000000000000000000000
							6399        HINNONAG_DD  20.00000000000000000000000
							6399       HINNONAG_RIP  99.00000000000000000000000
							6399       HINNONAG_SYN  20.00000000000000000000000
							6399      HINORCHARD_DD   2.00000000000000000000000
							6399     HINORCHARD_RIP  10.00000000000000000000000
							6399     HINORCHARD_SYN   2.00000000000000000000000
							6399        HINOTHER_DD   2.00000000000000000000000
							6399       HINOTHER_RIP  10.00000000000000000000000
							6399       HINOTHER_SYN   2.00000000000000000000000
							6399         HINPARK_DD   2.00000000000000000000000
							6399        HINPARK_RIP   9.00000000000000000000000
							6399        HINPARK_SYN   2.00000000000000000000000
							6399      HINPASTURE_DD   2.00000000000000000000000
							6399     HINPASTURE_RIP  10.00000000000000000000000
							6399     HINPASTURE_SYN   2.00000000000000000000000
							6399   HINPOWERLINES_DD   2.00000000000000000000000
							6399  HINPOWERLINES_RIP  10.00000000000000000000000
							6399  HINPOWERLINES_SYN   2.00000000000000000000000
							6399        HINROADS_DD   2.00000000000000000000000
							6399       HINROADS_RIP  10.00000000000000000000000
							6399       HINROADS_SYN   2.00000000000000000000000
							6399        HINWALLS_DD   2.00000000000000000000000
							6399       HINWALLS_RIP  10.00000000000000000000000
							6399       HINWALLS_SYN   2.00000000000000000000000
							6399          HIPWAG_DD   0.00000000000000000000000
							6399         HIPWAG_RIP   0.00000000000000000000000
							6399         HIPWAG_SYN   0.00000000000000000000000
							6399         HIPWALL_DD   0.32692307689999999809061
							6399        HIPWALL_RIP   0.20542635659999999853120
							6399        HIPWALL_SYN   0.34487179489999997272776
							6399   HIPWBUILDINGS_DD   0.75000000000000000000000
							6399  HIPWBUILDINGS_RIP   0.45000000000000001110223
							6399  HIPWBUILDINGS_SYN   0.75000000000000000000000
							6399  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6399 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6399 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6399       HIPWCROPS_DD   0.00000000000000000000000
							6399      HIPWCROPS_RIP   0.00000000000000000000000
							6399      HIPWCROPS_SYN   0.00000000000000000000000
							6399       HIPWDOCKS_DD   0.75000000000000000000000
							6399      HIPWDOCKS_RIP   0.40000000000000002220446
							6399      HIPWDOCKS_SYN   0.75000000000000000000000
							6399    HIPWLANDFILL_DD   0.50000000000000000000000
							6399   HIPWLANDFILL_RIP   0.20000000000000001110223
							6399   HIPWLANDFILL_SYN   0.50000000000000000000000
							6399        HIPWLAWN_DD   0.25000000000000000000000
							6399       HIPWLAWN_RIP   0.55000000000000004440892
							6399       HIPWLAWN_SYN   0.48333333330000000094273
							6399       HIPWNONAG_DD   0.42499999999999998889777
							6399      HIPWNONAG_RIP   0.26767676769999998453997
							6399      HIPWNONAG_SYN   0.44833333330000002536764
							6399     HIPWORCHARD_DD   0.00000000000000000000000
							6399    HIPWORCHARD_RIP   0.00000000000000000000000
							6399    HIPWORCHARD_SYN   0.00000000000000000000000
							6399       HIPWOTHER_DD   0.00000000000000000000000
							6399      HIPWOTHER_RIP   0.00000000000000000000000
							6399      HIPWOTHER_SYN   0.00000000000000000000000
							6399        HIPWPARK_DD   0.00000000000000000000000
							6399       HIPWPARK_RIP   0.00000000000000000000000
							6399       HIPWPARK_SYN   0.00000000000000000000000
							6399     HIPWPASTURE_DD   0.00000000000000000000000
							6399    HIPWPASTURE_RIP   0.00000000000000000000000
							6399    HIPWPASTURE_SYN   0.00000000000000000000000
							6399  HIPWPOWERLINES_DD   0.50000000000000000000000
							6399 HIPWPOWERLINES_RIP   0.29999999999999998889777
							6399 HIPWPOWERLINES_SYN   0.50000000000000000000000
							6399       HIPWROADS_DD   0.50000000000000000000000
							6399      HIPWROADS_RIP   0.14999999999999999444888
							6399      HIPWROADS_SYN   0.50000000000000000000000
							6399       HIPWWALLS_DD   1.00000000000000000000000
							6399      HIPWWALLS_RIP   0.59999999999999997779554
							6399      HIPWWALLS_SYN   1.00000000000000000000000
							6449    HIFPANYCIRCA_DD   1.00000000000000000000000
							6449   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6449   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6449         HIFPANY_DD   1.00000000000000000000000
							6449        HIFPANY_RIP   1.00000000000000000000000
							6449        HIFPANY_SYN   1.00000000000000000000000
							6449      HIIAGCIRCA_DD   0.00000000000000000000000
							6449     HIIAGCIRCA_RIP   0.00000000000000000000000
							6449     HIIAGCIRCA_SYN   0.00000000000000000000000
							6449           HIIAG_DD   0.00000000000000000000000
							6449          HIIAG_RIP   0.00000000000000000000000
							6449          HIIAG_SYN   0.00000000000000000000000
							6449     HIIALLCIRCA_DD   1.50000000000000000000000
							6449    HIIALLCIRCA_RIP   2.58333333330000014527172
							6449    HIIALLCIRCA_SYN   1.50000000000000000000000
							6449          HIIALL_DD   3.54166666669999985472828
							6449         HIIALL_RIP   4.12500000000000000000000
							6449         HIIALL_SYN   4.35805555560000001946719
							6449   HIINONAGCIRCA_DD   1.50000000000000000000000
							6449  HIINONAGCIRCA_RIP   2.58333333330000014527172
							6449  HIINONAGCIRCA_SYN   1.50000000000000000000000
							6449        HIINONAG_DD   3.54166666669999985472828
							6449       HIINONAG_RIP   4.12500000000000000000000
							6449       HIINONAG_SYN   4.35805555560000001946719
							6449           HINAG_DD  36.00000000000000000000000
							6449          HINAG_RIP  36.00000000000000000000000
							6449          HINAG_SYN  36.00000000000000000000000
							6449          HINALL_DD 155.00000000000000000000000
							6449         HINALL_RIP 156.00000000000000000000000
							6449         HINALL_SYN 155.00000000000000000000000
							6449    HINBUILDINGS_DD  12.00000000000000000000000
							6449   HINBUILDINGS_RIP  12.00000000000000000000000
							6449   HINBUILDINGS_SYN  12.00000000000000000000000
							6449   HINCOMMERCIAL_DD  12.00000000000000000000000
							6449  HINCOMMERCIAL_RIP  12.00000000000000000000000
							6449  HINCOMMERCIAL_SYN  12.00000000000000000000000
							6449        HINCROPS_DD  12.00000000000000000000000
							6449       HINCROPS_RIP  12.00000000000000000000000
							6449       HINCROPS_SYN  12.00000000000000000000000
							6449        HINDOCKS_DD  12.00000000000000000000000
							6449       HINDOCKS_RIP  12.00000000000000000000000
							6449       HINDOCKS_SYN  12.00000000000000000000000
							6449     HINLANDFILL_DD  12.00000000000000000000000
							6449    HINLANDFILL_RIP  12.00000000000000000000000
							6449    HINLANDFILL_SYN  12.00000000000000000000000
							6449         HINLAWN_DD  12.00000000000000000000000
							6449        HINLAWN_RIP  12.00000000000000000000000
							6449        HINLAWN_SYN  12.00000000000000000000000
							6449        HINNONAG_DD 119.00000000000000000000000
							6449       HINNONAG_RIP 120.00000000000000000000000
							6449       HINNONAG_SYN 119.00000000000000000000000
							6449      HINORCHARD_DD  12.00000000000000000000000
							6449     HINORCHARD_RIP  12.00000000000000000000000
							6449     HINORCHARD_SYN  12.00000000000000000000000
							6449        HINOTHER_DD  12.00000000000000000000000
							6449       HINOTHER_RIP  12.00000000000000000000000
							6449       HINOTHER_SYN  12.00000000000000000000000
							6449         HINPARK_DD  12.00000000000000000000000
							6449        HINPARK_RIP  12.00000000000000000000000
							6449        HINPARK_SYN  12.00000000000000000000000
							6449      HINPASTURE_DD  12.00000000000000000000000
							6449     HINPASTURE_RIP  12.00000000000000000000000
							6449     HINPASTURE_SYN  12.00000000000000000000000
							6449   HINPOWERLINES_DD  12.00000000000000000000000
							6449  HINPOWERLINES_RIP  12.00000000000000000000000
							6449  HINPOWERLINES_SYN  12.00000000000000000000000
							6449        HINROADS_DD  11.00000000000000000000000
							6449       HINROADS_RIP  12.00000000000000000000000
							6449       HINROADS_SYN  11.00000000000000000000000
							6449        HINWALLS_DD  12.00000000000000000000000
							6449       HINWALLS_RIP  12.00000000000000000000000
							6449       HINWALLS_SYN  12.00000000000000000000000
							6449          HIPWAG_DD   0.00000000000000000000000
							6449         HIPWAG_RIP   0.00000000000000000000000
							6449         HIPWAG_SYN   0.00000000000000000000000
							6449         HIPWALL_DD   0.27096774190000000315237
							6449        HIPWALL_RIP   0.31730769229999999936354
							6449        HIPWALL_SYN   0.33417204299999997418880
							6449   HIPWBUILDINGS_DD   0.41666666670000002126173
							6449  HIPWBUILDINGS_RIP   0.50000000000000000000000
							6449  HIPWBUILDINGS_SYN   0.49833333330000001426541
							6449  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6449 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6449 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6449       HIPWCROPS_DD   0.00000000000000000000000
							6449      HIPWCROPS_RIP   0.00000000000000000000000
							6449      HIPWCROPS_SYN   0.00000000000000000000000
							6449       HIPWDOCKS_DD   0.58333333330000003424942
							6449      HIPWDOCKS_RIP   0.45833333329999997873827
							6449      HIPWDOCKS_SYN   0.58333333330000003424942
							6449    HIPWLANDFILL_DD   0.66666666669999996575058
							6449   HIPWLANDFILL_RIP   0.58333333330000003424942
							6449   HIPWLANDFILL_SYN   0.66666666669999996575058
							6449        HIPWLAWN_DD   0.50000000000000000000000
							6449       HIPWLAWN_RIP   0.83333333330000003424942
							6449       HIPWLAWN_SYN   0.82638888889999995157609
							6449       HIPWNONAG_DD   0.35294117650000000896426
							6449      HIPWNONAG_RIP   0.41249999999999997779554
							6449      HIPWNONAG_SYN   0.43526610640000001906103
							6449     HIPWORCHARD_DD   0.00000000000000000000000
							6449    HIPWORCHARD_RIP   0.00000000000000000000000
							6449    HIPWORCHARD_SYN   0.00000000000000000000000
							6449       HIPWOTHER_DD   0.08333333330000000649385
							6449      HIPWOTHER_RIP   0.25000000000000000000000
							6449      HIPWOTHER_SYN   0.24611111110000000179454
							6449        HIPWPARK_DD   0.20833333330000000649385
							6449       HIPWPARK_RIP   0.16666666669999999350615
							6449       HIPWPARK_SYN   0.20833333330000000649385
							6449     HIPWPASTURE_DD   0.00000000000000000000000
							6449    HIPWPASTURE_RIP   0.00000000000000000000000
							6449    HIPWPASTURE_SYN   0.00000000000000000000000
							6449  HIPWPOWERLINES_DD   0.08333333330000000649385
							6449 HIPWPOWERLINES_RIP   0.08333333330000000649385
							6449 HIPWPOWERLINES_SYN   0.08333333330000000649385
							6449       HIPWROADS_DD   0.50000000000000000000000
							6449      HIPWROADS_RIP   0.50000000000000000000000
							6449      HIPWROADS_SYN   0.50000000000000000000000
							6449       HIPWWALLS_DD   0.50000000000000000000000
							6449      HIPWWALLS_RIP   0.75000000000000000000000
							6449      HIPWWALLS_SYN   0.74555555559999997505827
							6684    HIFPANYCIRCA_DD                          NA
							6684   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6684   HIFPANYCIRCA_SYN                          NA
							6684         HIFPANY_DD                          NA
							6684        HIFPANY_RIP   1.00000000000000000000000
							6684        HIFPANY_SYN                          NA
							6684      HIIAGCIRCA_DD                          NA
							6684     HIIAGCIRCA_RIP   1.00000000000000000000000
							6684     HIIAGCIRCA_SYN                          NA
							6684           HIIAG_DD                          NA
							6684          HIIAG_RIP   1.00000000000000000000000
							6684          HIIAG_SYN                          NA
							6684     HIIALLCIRCA_DD                          NA
							6684    HIIALLCIRCA_RIP   1.00000000000000000000000
							6684    HIIALLCIRCA_SYN                          NA
							6684          HIIALL_DD                          NA
							6684         HIIALL_RIP   1.00000000000000000000000
							6684         HIIALL_SYN                          NA
							6684   HIINONAGCIRCA_DD                          NA
							6684  HIINONAGCIRCA_RIP   0.00000000000000000000000
							6684  HIINONAGCIRCA_SYN                          NA
							6684        HIINONAG_DD                          NA
							6684       HIINONAG_RIP   0.00000000000000000000000
							6684       HIINONAG_SYN                          NA
							6684           HINAG_DD   0.00000000000000000000000
							6684          HINAG_RIP  18.00000000000000000000000
							6684          HINAG_SYN   0.00000000000000000000000
							6684          HINALL_DD   0.00000000000000000000000
							6684         HINALL_RIP  72.00000000000000000000000
							6684         HINALL_SYN   0.00000000000000000000000
							6684    HINBUILDINGS_DD   0.00000000000000000000000
							6684   HINBUILDINGS_RIP   6.00000000000000000000000
							6684   HINBUILDINGS_SYN   0.00000000000000000000000
							6684   HINCOMMERCIAL_DD   0.00000000000000000000000
							6684  HINCOMMERCIAL_RIP   6.00000000000000000000000
							6684  HINCOMMERCIAL_SYN   0.00000000000000000000000
							6684        HINCROPS_DD   0.00000000000000000000000
							6684       HINCROPS_RIP   6.00000000000000000000000
							6684       HINCROPS_SYN   0.00000000000000000000000
							6684        HINDOCKS_DD   0.00000000000000000000000
							6684       HINDOCKS_RIP   6.00000000000000000000000
							6684       HINDOCKS_SYN   0.00000000000000000000000
							6684     HINLANDFILL_DD   0.00000000000000000000000
							6684    HINLANDFILL_RIP   6.00000000000000000000000
							6684    HINLANDFILL_SYN   0.00000000000000000000000
							6684         HINLAWN_DD   0.00000000000000000000000
							6684        HINLAWN_RIP   6.00000000000000000000000
							6684        HINLAWN_SYN   0.00000000000000000000000
							6684        HINNONAG_DD   0.00000000000000000000000
							6684       HINNONAG_RIP  54.00000000000000000000000
							6684       HINNONAG_SYN   0.00000000000000000000000
							6684      HINORCHARD_DD   0.00000000000000000000000
							6684     HINORCHARD_RIP   6.00000000000000000000000
							6684     HINORCHARD_SYN   0.00000000000000000000000
							6684        HINOTHER_DD   0.00000000000000000000000
							6684       HINOTHER_RIP   0.00000000000000000000000
							6684       HINOTHER_SYN   0.00000000000000000000000
							6684         HINPARK_DD   0.00000000000000000000000
							6684        HINPARK_RIP   6.00000000000000000000000
							6684        HINPARK_SYN   0.00000000000000000000000
							6684      HINPASTURE_DD   0.00000000000000000000000
							6684     HINPASTURE_RIP   6.00000000000000000000000
							6684     HINPASTURE_SYN   0.00000000000000000000000
							6684   HINPOWERLINES_DD   0.00000000000000000000000
							6684  HINPOWERLINES_RIP   6.00000000000000000000000
							6684  HINPOWERLINES_SYN   0.00000000000000000000000
							6684        HINROADS_DD   0.00000000000000000000000
							6684       HINROADS_RIP   6.00000000000000000000000
							6684       HINROADS_SYN   0.00000000000000000000000
							6684        HINWALLS_DD   0.00000000000000000000000
							6684       HINWALLS_RIP   6.00000000000000000000000
							6684       HINWALLS_SYN   0.00000000000000000000000
							6684          HIPWAG_DD                          NA
							6684         HIPWAG_RIP   0.33333333329999997873827
							6684         HIPWAG_SYN                          NA
							6684         HIPWALL_DD                          NA
							6684        HIPWALL_RIP   0.08333333330000000649385
							6684        HIPWALL_SYN                          NA
							6684   HIPWBUILDINGS_DD                          NA
							6684  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6684  HIPWBUILDINGS_SYN                          NA
							6684  HIPWCOMMERCIAL_DD                          NA
							6684 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6684 HIPWCOMMERCIAL_SYN                          NA
							6684       HIPWCROPS_DD                          NA
							6684      HIPWCROPS_RIP   0.00000000000000000000000
							6684      HIPWCROPS_SYN                          NA
							6684       HIPWDOCKS_DD                          NA
							6684      HIPWDOCKS_RIP   0.00000000000000000000000
							6684      HIPWDOCKS_SYN                          NA
							6684    HIPWLANDFILL_DD                          NA
							6684   HIPWLANDFILL_RIP   0.00000000000000000000000
							6684   HIPWLANDFILL_SYN                          NA
							6684        HIPWLAWN_DD                          NA
							6684       HIPWLAWN_RIP   0.00000000000000000000000
							6684       HIPWLAWN_SYN                          NA
							6684       HIPWNONAG_DD                          NA
							6684      HIPWNONAG_RIP   0.00000000000000000000000
							6684      HIPWNONAG_SYN                          NA
							6684     HIPWORCHARD_DD                          NA
							6684    HIPWORCHARD_RIP   0.00000000000000000000000
							6684    HIPWORCHARD_SYN                          NA
							6684       HIPWOTHER_DD                          NA
							6684      HIPWOTHER_RIP                          NA
							6684      HIPWOTHER_SYN                          NA
							6684        HIPWPARK_DD                          NA
							6684       HIPWPARK_RIP   0.00000000000000000000000
							6684       HIPWPARK_SYN                          NA
							6684     HIPWPASTURE_DD                          NA
							6684    HIPWPASTURE_RIP   1.00000000000000000000000
							6684    HIPWPASTURE_SYN                          NA
							6684  HIPWPOWERLINES_DD                          NA
							6684 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6684 HIPWPOWERLINES_SYN                          NA
							6684       HIPWROADS_DD                          NA
							6684      HIPWROADS_RIP   0.00000000000000000000000
							6684      HIPWROADS_SYN                          NA
							6684       HIPWWALLS_DD                          NA
							6684      HIPWWALLS_RIP   0.00000000000000000000000
							6684      HIPWWALLS_SYN                          NA
							7504    HIFPANYCIRCA_DD                          NA
							7504   HIFPANYCIRCA_RIP   1.00000000000000000000000
							7504   HIFPANYCIRCA_SYN                          NA
							7504         HIFPANY_DD                          NA
							7504        HIFPANY_RIP   1.00000000000000000000000
							7504        HIFPANY_SYN                          NA
							7504      HIIAGCIRCA_DD                          NA
							7504     HIIAGCIRCA_RIP   0.00000000000000000000000
							7504     HIIAGCIRCA_SYN                          NA
							7504           HIIAG_DD                          NA
							7504          HIIAG_RIP   0.00000000000000000000000
							7504          HIIAG_SYN                          NA
							7504     HIIALLCIRCA_DD                          NA
							7504    HIIALLCIRCA_RIP   3.81666666670000020999964
							7504    HIIALLCIRCA_SYN                          NA
							7504          HIIALL_DD                          NA
							7504         HIIALL_RIP   4.82500000000000017763568
							7504         HIIALL_SYN                          NA
							7504   HIINONAGCIRCA_DD                          NA
							7504  HIINONAGCIRCA_RIP   3.81666666670000020999964
							7504  HIINONAGCIRCA_SYN                          NA
							7504        HIINONAG_DD                          NA
							7504       HIINONAG_RIP   4.82500000000000017763568
							7504       HIINONAG_SYN                          NA
							7504           HINAG_DD   0.00000000000000000000000
							7504          HINAG_RIP   3.00000000000000000000000
							7504          HINAG_SYN   0.00000000000000000000000
							7504          HINALL_DD   0.00000000000000000000000
							7504         HINALL_RIP  32.00000000000000000000000
							7504         HINALL_SYN   0.00000000000000000000000
							7504    HINBUILDINGS_DD   0.00000000000000000000000
							7504   HINBUILDINGS_RIP   3.00000000000000000000000
							7504   HINBUILDINGS_SYN   0.00000000000000000000000
							7504   HINCOMMERCIAL_DD   0.00000000000000000000000
							7504  HINCOMMERCIAL_RIP   2.00000000000000000000000
							7504  HINCOMMERCIAL_SYN   0.00000000000000000000000
							7504        HINCROPS_DD   0.00000000000000000000000
							7504       HINCROPS_RIP   1.00000000000000000000000
							7504       HINCROPS_SYN   0.00000000000000000000000
							7504        HINDOCKS_DD   0.00000000000000000000000
							7504       HINDOCKS_RIP   1.00000000000000000000000
							7504       HINDOCKS_SYN   0.00000000000000000000000
							7504     HINLANDFILL_DD   0.00000000000000000000000
							7504    HINLANDFILL_RIP   2.00000000000000000000000
							7504    HINLANDFILL_SYN   0.00000000000000000000000
							7504         HINLAWN_DD   0.00000000000000000000000
							7504        HINLAWN_RIP   4.00000000000000000000000
							7504        HINLAWN_SYN   0.00000000000000000000000
							7504        HINNONAG_DD   0.00000000000000000000000
							7504       HINNONAG_RIP  29.00000000000000000000000
							7504       HINNONAG_SYN   0.00000000000000000000000
							7504      HINORCHARD_DD   0.00000000000000000000000
							7504     HINORCHARD_RIP   1.00000000000000000000000
							7504     HINORCHARD_SYN   0.00000000000000000000000
							7504        HINOTHER_DD   0.00000000000000000000000
							7504       HINOTHER_RIP   2.00000000000000000000000
							7504       HINOTHER_SYN   0.00000000000000000000000
							7504         HINPARK_DD   0.00000000000000000000000
							7504        HINPARK_RIP   1.00000000000000000000000
							7504        HINPARK_SYN   0.00000000000000000000000
							7504      HINPASTURE_DD   0.00000000000000000000000
							7504     HINPASTURE_RIP   1.00000000000000000000000
							7504     HINPASTURE_SYN   0.00000000000000000000000
							7504   HINPOWERLINES_DD   0.00000000000000000000000
							7504  HINPOWERLINES_RIP   3.00000000000000000000000
							7504  HINPOWERLINES_SYN   0.00000000000000000000000
							7504        HINROADS_DD   0.00000000000000000000000
							7504       HINROADS_RIP  10.00000000000000000000000
							7504       HINROADS_SYN   0.00000000000000000000000
							7504        HINWALLS_DD   0.00000000000000000000000
							7504       HINWALLS_RIP   1.00000000000000000000000
							7504       HINWALLS_SYN   0.00000000000000000000000
							7504          HIPWAG_DD                          NA
							7504         HIPWAG_RIP   0.00000000000000000000000
							7504         HIPWAG_SYN                          NA
							7504         HIPWALL_DD                          NA
							7504        HIPWALL_RIP   0.59375000000000000000000
							7504        HIPWALL_SYN                          NA
							7504   HIPWBUILDINGS_DD                          NA
							7504  HIPWBUILDINGS_RIP   0.33333333329999997873827
							7504  HIPWBUILDINGS_SYN                          NA
							7504  HIPWCOMMERCIAL_DD                          NA
							7504 HIPWCOMMERCIAL_RIP   0.25000000000000000000000
							7504 HIPWCOMMERCIAL_SYN                          NA
							7504       HIPWCROPS_DD                          NA
							7504      HIPWCROPS_RIP   0.00000000000000000000000
							7504      HIPWCROPS_SYN                          NA
							7504       HIPWDOCKS_DD                          NA
							7504      HIPWDOCKS_RIP   1.00000000000000000000000
							7504      HIPWDOCKS_SYN                          NA
							7504    HIPWLANDFILL_DD                          NA
							7504   HIPWLANDFILL_RIP   0.50000000000000000000000
							7504   HIPWLANDFILL_SYN                          NA
							7504        HIPWLAWN_DD                          NA
							7504       HIPWLAWN_RIP   0.87500000000000000000000
							7504       HIPWLAWN_SYN                          NA
							7504       HIPWNONAG_DD                          NA
							7504      HIPWNONAG_RIP   0.65517241380000001971240
							7504      HIPWNONAG_SYN                          NA
							7504     HIPWORCHARD_DD                          NA
							7504    HIPWORCHARD_RIP   0.00000000000000000000000
							7504    HIPWORCHARD_SYN                          NA
							7504       HIPWOTHER_DD                          NA
							7504      HIPWOTHER_RIP   0.25000000000000000000000
							7504      HIPWOTHER_SYN                          NA
							7504        HIPWPARK_DD                          NA
							7504       HIPWPARK_RIP   0.00000000000000000000000
							7504       HIPWPARK_SYN                          NA
							7504     HIPWPASTURE_DD                          NA
							7504    HIPWPASTURE_RIP   0.00000000000000000000000
							7504    HIPWPASTURE_SYN                          NA
							7504  HIPWPOWERLINES_DD                          NA
							7504 HIPWPOWERLINES_RIP   0.66666666669999996575058
							7504 HIPWPOWERLINES_SYN                          NA
							7504       HIPWROADS_DD                          NA
							7504      HIPWROADS_RIP   0.94999999999999995559108
							7504      HIPWROADS_SYN                          NA
							7504       HIPWWALLS_DD                          NA
							7504      HIPWWALLS_RIP   0.00000000000000000000000
							7504      HIPWWALLS_SYN                          NA
							7740    HIFPANYCIRCA_DD   0.29999999999999998889777
							7740   HIFPANYCIRCA_RIP   0.29999999999999998889777
							7740   HIFPANYCIRCA_SYN   0.29999999999999998889777
							7740         HIFPANY_DD   0.40000000000000002220446
							7740        HIFPANY_RIP   0.50000000000000000000000
							7740        HIFPANY_SYN   0.40000000000000002220446
							7740      HIIAGCIRCA_DD   0.00000000000000000000000
							7740     HIIAGCIRCA_RIP   0.00000000000000000000000
							7740     HIIAGCIRCA_SYN   0.00000000000000000000000
							7740           HIIAG_DD   0.00000000000000000000000
							7740          HIIAG_RIP   0.00000000000000000000000
							7740          HIIAG_SYN   0.00000000000000000000000
							7740     HIIALLCIRCA_DD   0.29999999999999998889777
							7740    HIIALLCIRCA_RIP   0.40000000000000002220446
							7740    HIIALLCIRCA_SYN   0.29999999999999998889777
							7740          HIIALL_DD   0.65555555560000000614451
							7740         HIIALL_RIP   0.80000000000000004440892
							7740         HIIALL_SYN   0.65555555560000000614451
							7740   HIINONAGCIRCA_DD   0.29999999999999998889777
							7740  HIINONAGCIRCA_RIP   0.40000000000000002220446
							7740  HIINONAGCIRCA_SYN   0.29999999999999998889777
							7740        HIINONAG_DD   0.65555555560000000614451
							7740       HIINONAG_RIP   0.80000000000000004440892
							7740       HIINONAG_SYN   0.65555555560000000614451
							7740           HINAG_DD  30.00000000000000000000000
							7740          HINAG_RIP  30.00000000000000000000000
							7740          HINAG_SYN  30.00000000000000000000000
							7740          HINALL_DD 119.00000000000000000000000
							7740         HINALL_RIP 120.00000000000000000000000
							7740         HINALL_SYN 119.00000000000000000000000
							7740    HINBUILDINGS_DD  10.00000000000000000000000
							7740   HINBUILDINGS_RIP  10.00000000000000000000000
							7740   HINBUILDINGS_SYN  10.00000000000000000000000
							7740   HINCOMMERCIAL_DD  10.00000000000000000000000
							7740  HINCOMMERCIAL_RIP  10.00000000000000000000000
							7740  HINCOMMERCIAL_SYN  10.00000000000000000000000
							7740        HINCROPS_DD  10.00000000000000000000000
							7740       HINCROPS_RIP  10.00000000000000000000000
							7740       HINCROPS_SYN  10.00000000000000000000000
							7740        HINDOCKS_DD  10.00000000000000000000000
							7740       HINDOCKS_RIP  10.00000000000000000000000
							7740       HINDOCKS_SYN  10.00000000000000000000000
							7740     HINLANDFILL_DD  10.00000000000000000000000
							7740    HINLANDFILL_RIP  10.00000000000000000000000
							7740    HINLANDFILL_SYN  10.00000000000000000000000
							7740         HINLAWN_DD  10.00000000000000000000000
							7740        HINLAWN_RIP  10.00000000000000000000000
							7740        HINLAWN_SYN  10.00000000000000000000000
							7740        HINNONAG_DD  89.00000000000000000000000
							7740       HINNONAG_RIP  90.00000000000000000000000
							7740       HINNONAG_SYN  89.00000000000000000000000
							7740      HINORCHARD_DD  10.00000000000000000000000
							7740     HINORCHARD_RIP  10.00000000000000000000000
							7740     HINORCHARD_SYN  10.00000000000000000000000
							7740        HINOTHER_DD   0.00000000000000000000000
							7740       HINOTHER_RIP   0.00000000000000000000000
							7740       HINOTHER_SYN   0.00000000000000000000000
							7740         HINPARK_DD   9.00000000000000000000000
							7740        HINPARK_RIP  10.00000000000000000000000
							7740        HINPARK_SYN   9.00000000000000000000000
							7740      HINPASTURE_DD  10.00000000000000000000000
							7740     HINPASTURE_RIP  10.00000000000000000000000
							7740     HINPASTURE_SYN  10.00000000000000000000000
							7740   HINPOWERLINES_DD  10.00000000000000000000000
							7740  HINPOWERLINES_RIP  10.00000000000000000000000
							7740  HINPOWERLINES_SYN  10.00000000000000000000000
							7740        HINROADS_DD  10.00000000000000000000000
							7740       HINROADS_RIP  10.00000000000000000000000
							7740       HINROADS_SYN  10.00000000000000000000000
							7740        HINWALLS_DD  10.00000000000000000000000
							7740       HINWALLS_RIP  10.00000000000000000000000
							7740       HINWALLS_SYN  10.00000000000000000000000
							7740          HIPWAG_DD   0.00000000000000000000000
							7740         HIPWAG_RIP   0.00000000000000000000000
							7740         HIPWAG_SYN   0.00000000000000000000000
							7740         HIPWALL_DD   0.05462184870000000302959
							7740        HIPWALL_RIP   0.06666666670000000183283
							7740        HIPWALL_SYN   0.05462184870000000302959
							7740   HIPWBUILDINGS_DD   0.00000000000000000000000
							7740  HIPWBUILDINGS_RIP   0.00000000000000000000000
							7740  HIPWBUILDINGS_SYN   0.00000000000000000000000
							7740  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							7740 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							7740 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							7740       HIPWCROPS_DD   0.00000000000000000000000
							7740      HIPWCROPS_RIP   0.00000000000000000000000
							7740      HIPWCROPS_SYN   0.00000000000000000000000
							7740       HIPWDOCKS_DD   0.14999999999999999444888
							7740      HIPWDOCKS_RIP   0.14999999999999999444888
							7740      HIPWDOCKS_SYN   0.14999999999999999444888
							7740    HIPWLANDFILL_DD   0.10000000000000000555112
							7740   HIPWLANDFILL_RIP   0.00000000000000000000000
							7740   HIPWLANDFILL_SYN   0.10000000000000000555112
							7740        HIPWLAWN_DD   0.00000000000000000000000
							7740       HIPWLAWN_RIP   0.00000000000000000000000
							7740       HIPWLAWN_SYN   0.00000000000000000000000
							7740       HIPWNONAG_DD   0.07303370789999999446174
							7740      HIPWNONAG_RIP   0.08888888890000000431169
							7740      HIPWNONAG_SYN   0.07303370789999999446174
							7740     HIPWORCHARD_DD   0.00000000000000000000000
							7740    HIPWORCHARD_RIP   0.00000000000000000000000
							7740    HIPWORCHARD_SYN   0.00000000000000000000000
							7740       HIPWOTHER_DD                          NA
							7740      HIPWOTHER_RIP                          NA
							7740      HIPWOTHER_SYN                          NA
							7740        HIPWPARK_DD   0.05555555560000000059340
							7740       HIPWPARK_RIP   0.10000000000000000555112
							7740       HIPWPARK_SYN   0.05555555560000000059340
							7740     HIPWPASTURE_DD   0.00000000000000000000000
							7740    HIPWPASTURE_RIP   0.00000000000000000000000
							7740    HIPWPASTURE_SYN   0.00000000000000000000000
							7740  HIPWPOWERLINES_DD   0.05000000000000000277556
							7740 HIPWPOWERLINES_RIP   0.10000000000000000555112
							7740 HIPWPOWERLINES_SYN   0.05000000000000000277556
							7740       HIPWROADS_DD   0.20000000000000001110223
							7740      HIPWROADS_RIP   0.34999999999999997779554
							7740      HIPWROADS_SYN   0.20000000000000001110223
							7740       HIPWWALLS_DD   0.10000000000000000555112
							7740      HIPWWALLS_RIP   0.10000000000000000555112
							7740      HIPWWALLS_SYN   0.10000000000000000555112
							7808    HIFPANYCIRCA_DD   0.00000000000000000000000
							7808   HIFPANYCIRCA_RIP   0.00000000000000000000000
							7808   HIFPANYCIRCA_SYN   0.00000000000000000000000
							7808         HIFPANY_DD   1.00000000000000000000000
							7808        HIFPANY_RIP   0.87500000000000000000000
							7808        HIFPANY_SYN   1.00000000000000000000000
							7808      HIIAGCIRCA_DD   0.00000000000000000000000
							7808     HIIAGCIRCA_RIP   0.00000000000000000000000
							7808     HIIAGCIRCA_SYN   0.00000000000000000000000
							7808           HIIAG_DD   1.00000000000000000000000
							7808          HIIAG_RIP   1.00000000000000000000000
							7808          HIIAG_SYN   1.00000000000000000000000
							7808     HIIALLCIRCA_DD   0.00000000000000000000000
							7808    HIIALLCIRCA_RIP   0.00000000000000000000000
							7808    HIIALLCIRCA_SYN   0.00000000000000000000000
							7808          HIIALL_DD   3.00000000000000000000000
							7808         HIIALL_RIP   2.33333333330000014527172
							7808         HIIALL_SYN   2.50000000000000000000000
							7808   HIINONAGCIRCA_DD   0.00000000000000000000000
							7808  HIINONAGCIRCA_RIP   0.00000000000000000000000
							7808  HIINONAGCIRCA_SYN   0.00000000000000000000000
							7808        HIINONAG_DD   2.00000000000000000000000
							7808       HIINONAG_RIP   1.33333333329999992322712
							7808       HIINONAG_SYN   1.50000000000000000000000
							7808           HINAG_DD   9.00000000000000000000000
							7808          HINAG_RIP   8.00000000000000000000000
							7808          HINAG_SYN   8.00000000000000000000000
							7808          HINALL_DD  15.00000000000000000000000
							7808         HINALL_RIP  13.00000000000000000000000
							7808         HINALL_SYN  13.00000000000000000000000
							7808    HINBUILDINGS_DD   3.00000000000000000000000
							7808   HINBUILDINGS_RIP   3.00000000000000000000000
							7808   HINBUILDINGS_SYN   3.00000000000000000000000
							7808   HINCOMMERCIAL_DD   0.00000000000000000000000
							7808  HINCOMMERCIAL_RIP   0.00000000000000000000000
							7808  HINCOMMERCIAL_SYN   0.00000000000000000000000
							7808        HINCROPS_DD   7.00000000000000000000000
							7808       HINCROPS_RIP   6.00000000000000000000000
							7808       HINCROPS_SYN   6.00000000000000000000000
							7808        HINDOCKS_DD   0.00000000000000000000000
							7808       HINDOCKS_RIP   0.00000000000000000000000
							7808       HINDOCKS_SYN   0.00000000000000000000000
							7808     HINLANDFILL_DD   1.00000000000000000000000
							7808    HINLANDFILL_RIP   0.00000000000000000000000
							7808    HINLANDFILL_SYN   0.00000000000000000000000
							7808         HINLAWN_DD   0.00000000000000000000000
							7808        HINLAWN_RIP   0.00000000000000000000000
							7808        HINLAWN_SYN   0.00000000000000000000000
							7808        HINNONAG_DD   6.00000000000000000000000
							7808       HINNONAG_RIP   5.00000000000000000000000
							7808       HINNONAG_SYN   5.00000000000000000000000
							7808      HINORCHARD_DD   0.00000000000000000000000
							7808     HINORCHARD_RIP   0.00000000000000000000000
							7808     HINORCHARD_SYN   0.00000000000000000000000
							7808        HINOTHER_DD   0.00000000000000000000000
							7808       HINOTHER_RIP   0.00000000000000000000000
							7808       HINOTHER_SYN   0.00000000000000000000000
							7808         HINPARK_DD   0.00000000000000000000000
							7808        HINPARK_RIP   0.00000000000000000000000
							7808        HINPARK_SYN   0.00000000000000000000000
							7808      HINPASTURE_DD   2.00000000000000000000000
							7808     HINPASTURE_RIP   2.00000000000000000000000
							7808     HINPASTURE_SYN   2.00000000000000000000000
							7808   HINPOWERLINES_DD   1.00000000000000000000000
							7808  HINPOWERLINES_RIP   1.00000000000000000000000
							7808  HINPOWERLINES_SYN   1.00000000000000000000000
							7808        HINROADS_DD   1.00000000000000000000000
							7808       HINROADS_RIP   1.00000000000000000000000
							7808       HINROADS_SYN   1.00000000000000000000000
							7808        HINWALLS_DD   0.00000000000000000000000
							7808       HINWALLS_RIP   0.00000000000000000000000
							7808       HINWALLS_SYN   0.00000000000000000000000
							7808          HIPWAG_DD   0.50000000000000000000000
							7808         HIPWAG_RIP   0.50000000000000000000000
							7808         HIPWAG_SYN   0.50000000000000000000000
							7808         HIPWALL_DD   0.50000000000000000000000
							7808        HIPWALL_RIP   0.46153846149999999681768
							7808        HIPWALL_SYN   0.50000000000000000000000
							7808   HIPWBUILDINGS_DD   0.50000000000000000000000
							7808  HIPWBUILDINGS_RIP   0.33333333329999997873827
							7808  HIPWBUILDINGS_SYN   0.50000000000000000000000
							7808  HIPWCOMMERCIAL_DD                          NA
							7808 HIPWCOMMERCIAL_RIP                          NA
							7808 HIPWCOMMERCIAL_SYN                          NA
							7808       HIPWCROPS_DD   0.50000000000000000000000
							7808      HIPWCROPS_RIP   0.50000000000000000000000
							7808      HIPWCROPS_SYN   0.50000000000000000000000
							7808       HIPWDOCKS_DD                          NA
							7808      HIPWDOCKS_RIP                          NA
							7808      HIPWDOCKS_SYN                          NA
							7808    HIPWLANDFILL_DD   0.50000000000000000000000
							7808   HIPWLANDFILL_RIP                          NA
							7808   HIPWLANDFILL_SYN                          NA
							7808        HIPWLAWN_DD                          NA
							7808       HIPWLAWN_RIP                          NA
							7808       HIPWLAWN_SYN                          NA
							7808       HIPWNONAG_DD   0.50000000000000000000000
							7808      HIPWNONAG_RIP   0.40000000000000002220446
							7808      HIPWNONAG_SYN   0.50000000000000000000000
							7808     HIPWORCHARD_DD                          NA
							7808    HIPWORCHARD_RIP                          NA
							7808    HIPWORCHARD_SYN                          NA
							7808       HIPWOTHER_DD                          NA
							7808      HIPWOTHER_RIP                          NA
							7808      HIPWOTHER_SYN                          NA
							7808        HIPWPARK_DD                          NA
							7808       HIPWPARK_RIP                          NA
							7808       HIPWPARK_SYN                          NA
							7808     HIPWPASTURE_DD   0.50000000000000000000000
							7808    HIPWPASTURE_RIP   0.50000000000000000000000
							7808    HIPWPASTURE_SYN   0.50000000000000000000000
							7808  HIPWPOWERLINES_DD   0.50000000000000000000000
							7808 HIPWPOWERLINES_RIP   0.50000000000000000000000
							7808 HIPWPOWERLINES_SYN   0.50000000000000000000000
							7808       HIPWROADS_DD   0.50000000000000000000000
							7808      HIPWROADS_RIP   0.50000000000000000000000
							7808      HIPWROADS_SYN   0.50000000000000000000000
							7808       HIPWWALLS_DD                          NA
							7808      HIPWWALLS_RIP                          NA
							7808      HIPWWALLS_SYN                          NA
							1000100    HIFPANYCIRCA_DD                          NA
							1000100   HIFPANYCIRCA_RIP   1.00000000000000000000000
							1000100   HIFPANYCIRCA_SYN                          NA
							1000100         HIFPANY_DD                          NA
							1000100        HIFPANY_RIP   1.00000000000000000000000
							1000100        HIFPANY_SYN                          NA
							1000100      HIIAGCIRCA_DD                          NA
							1000100     HIIAGCIRCA_RIP                          NA
							1000100     HIIAGCIRCA_SYN                          NA
							1000100           HIIAG_DD                          NA
							1000100          HIIAG_RIP                          NA
							1000100          HIIAG_SYN                          NA
							1000100     HIIALLCIRCA_DD                          NA
							1000100    HIIALLCIRCA_RIP   1.00000000000000000000000
							1000100    HIIALLCIRCA_SYN                          NA
							1000100          HIIALL_DD                          NA
							1000100         HIIALL_RIP   1.00000000000000000000000
							1000100         HIIALL_SYN                          NA
							1000100   HIINONAGCIRCA_DD                          NA
							1000100  HIINONAGCIRCA_RIP   1.00000000000000000000000
							1000100  HIINONAGCIRCA_SYN                          NA
							1000100        HIINONAG_DD                          NA
							1000100       HIINONAG_RIP   1.00000000000000000000000
							1000100       HIINONAG_SYN                          NA
							1000100           HINAG_DD   0.00000000000000000000000
							1000100          HINAG_RIP   0.00000000000000000000000
							1000100          HINAG_SYN   0.00000000000000000000000
							1000100          HINALL_DD   0.00000000000000000000000
							1000100         HINALL_RIP   1.00000000000000000000000
							1000100         HINALL_SYN   0.00000000000000000000000
							1000100    HINBUILDINGS_DD   0.00000000000000000000000
							1000100   HINBUILDINGS_RIP   0.00000000000000000000000
							1000100   HINBUILDINGS_SYN   0.00000000000000000000000
							1000100   HINCOMMERCIAL_DD   0.00000000000000000000000
							1000100  HINCOMMERCIAL_RIP   0.00000000000000000000000
							1000100  HINCOMMERCIAL_SYN   0.00000000000000000000000
							1000100        HINCROPS_DD   0.00000000000000000000000
							1000100       HINCROPS_RIP   0.00000000000000000000000
							1000100       HINCROPS_SYN   0.00000000000000000000000
							1000100        HINDOCKS_DD   0.00000000000000000000000
							1000100       HINDOCKS_RIP   0.00000000000000000000000
							1000100       HINDOCKS_SYN   0.00000000000000000000000
							1000100     HINLANDFILL_DD   0.00000000000000000000000
							1000100    HINLANDFILL_RIP   1.00000000000000000000000
							1000100    HINLANDFILL_SYN   0.00000000000000000000000
							1000100         HINLAWN_DD   0.00000000000000000000000
							1000100        HINLAWN_RIP   0.00000000000000000000000
							1000100        HINLAWN_SYN   0.00000000000000000000000
							1000100        HINNONAG_DD   0.00000000000000000000000
							1000100       HINNONAG_RIP   1.00000000000000000000000
							1000100       HINNONAG_SYN   0.00000000000000000000000
							1000100      HINORCHARD_DD   0.00000000000000000000000
							1000100     HINORCHARD_RIP   0.00000000000000000000000
							1000100     HINORCHARD_SYN   0.00000000000000000000000
							1000100        HINOTHER_DD   0.00000000000000000000000
							1000100       HINOTHER_RIP   0.00000000000000000000000
							1000100       HINOTHER_SYN   0.00000000000000000000000
							1000100         HINPARK_DD   0.00000000000000000000000
							1000100        HINPARK_RIP   0.00000000000000000000000
							1000100        HINPARK_SYN   0.00000000000000000000000
							1000100      HINPASTURE_DD   0.00000000000000000000000
							1000100     HINPASTURE_RIP   0.00000000000000000000000
							1000100     HINPASTURE_SYN   0.00000000000000000000000
							1000100   HINPOWERLINES_DD   0.00000000000000000000000
							1000100  HINPOWERLINES_RIP   0.00000000000000000000000
							1000100  HINPOWERLINES_SYN   0.00000000000000000000000
							1000100        HINROADS_DD   0.00000000000000000000000
							1000100       HINROADS_RIP   0.00000000000000000000000
							1000100       HINROADS_SYN   0.00000000000000000000000
							1000100        HINWALLS_DD   0.00000000000000000000000
							1000100       HINWALLS_RIP   0.00000000000000000000000
							1000100       HINWALLS_SYN   0.00000000000000000000000
							1000100          HIPWAG_DD                          NA
							1000100         HIPWAG_RIP                          NA
							1000100         HIPWAG_SYN                          NA
							1000100         HIPWALL_DD                          NA
							1000100        HIPWALL_RIP   1.00000000000000000000000
							1000100        HIPWALL_SYN                          NA
							1000100   HIPWBUILDINGS_DD                          NA
							1000100  HIPWBUILDINGS_RIP                          NA
							1000100  HIPWBUILDINGS_SYN                          NA
							1000100  HIPWCOMMERCIAL_DD                          NA
							1000100 HIPWCOMMERCIAL_RIP                          NA
							1000100 HIPWCOMMERCIAL_SYN                          NA
							1000100       HIPWCROPS_DD                          NA
							1000100      HIPWCROPS_RIP                          NA
							1000100      HIPWCROPS_SYN                          NA
							1000100       HIPWDOCKS_DD                          NA
							1000100      HIPWDOCKS_RIP                          NA
							1000100      HIPWDOCKS_SYN                          NA
							1000100    HIPWLANDFILL_DD                          NA
							1000100   HIPWLANDFILL_RIP   1.00000000000000000000000
							1000100   HIPWLANDFILL_SYN                          NA
							1000100        HIPWLAWN_DD                          NA
							1000100       HIPWLAWN_RIP                          NA
							1000100       HIPWLAWN_SYN                          NA
							1000100       HIPWNONAG_DD                          NA
							1000100      HIPWNONAG_RIP   1.00000000000000000000000
							1000100      HIPWNONAG_SYN                          NA
							1000100     HIPWORCHARD_DD                          NA
							1000100    HIPWORCHARD_RIP                          NA
							1000100    HIPWORCHARD_SYN                          NA
							1000100       HIPWOTHER_DD                          NA
							1000100      HIPWOTHER_RIP                          NA
							1000100      HIPWOTHER_SYN                          NA
							1000100        HIPWPARK_DD                          NA
							1000100       HIPWPARK_RIP                          NA
							1000100       HIPWPARK_SYN                          NA
							1000100     HIPWPASTURE_DD                          NA
							1000100    HIPWPASTURE_RIP                          NA
							1000100    HIPWPASTURE_SYN                          NA
							1000100  HIPWPOWERLINES_DD                          NA
							1000100 HIPWPOWERLINES_RIP                          NA
							1000100 HIPWPOWERLINES_SYN                          NA
							1000100       HIPWROADS_DD                          NA
							1000100      HIPWROADS_RIP                          NA
							1000100      HIPWROADS_SYN                          NA
							1000100       HIPWWALLS_DD                          NA
							1000100      HIPWWALLS_RIP                          NA
							1000100      HIPWWALLS_SYN                          NA"
					)
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(rc)
}


nlaHumanImpactTest.expectedResultsWithDrawDown <- function()
# Returns dataframe of expected calculations based on the test data.
{
	
	tc <- textConnection("  SITE      	     METRIC                       VALUE
							6228         HIFPANY_DD   0.69999999999999995559108
							6228        HIFPANY_RIP   0.69999999999999995559108
							6228        HIFPANY_SYN   0.69999999999999995559108
							6228    HIFPANYCIRCA_DD   0.59999999999999997779554
							6228   HIFPANYCIRCA_RIP   0.69999999999999995559108
							6228   HIFPANYCIRCA_SYN   0.59999999999999997779554
							6228           HIIAG_DD   0.00000000000000000000000
							6228          HIIAG_RIP   0.00000000000000000000000
							6228          HIIAG_SYN   0.00000000000000000000000
							6228      HIIAGCIRCA_DD   0.00000000000000000000000
							6228     HIIAGCIRCA_RIP   0.00000000000000000000000
							6228     HIIAGCIRCA_SYN   0.00000000000000000000000
							6228          HIIALL_DD   1.00000000000000000000000
							6228         HIIALL_RIP   1.25000000000000000000000
							6228         HIIALL_SYN   1.22366666670000001637675
							6228     HIIALLCIRCA_DD   0.90000000000000002220446
							6228    HIIALLCIRCA_RIP   1.19999999999999995559108
							6228    HIIALLCIRCA_SYN   0.90000000000000002220446
							6228        HIINONAG_DD   1.00000000000000000000000
							6228       HIINONAG_RIP   1.25000000000000000000000
							6228       HIINONAG_SYN   1.22366666670000001637675
							6228   HIINONAGCIRCA_DD   0.90000000000000002220446
							6228  HIINONAGCIRCA_RIP   1.19999999999999995559108
							6228  HIINONAGCIRCA_SYN   0.90000000000000002220446
							6228           HINAG_DD  30.00000000000000000000000
							6228          HINAG_RIP  30.00000000000000000000000
							6228          HINAG_SYN  30.00000000000000000000000
							6228          HINALL_DD 130.00000000000000000000000
							6228         HINALL_RIP 130.00000000000000000000000
							6228         HINALL_SYN 130.00000000000000000000000
							6228    HINBUILDINGS_DD  10.00000000000000000000000
							6228   HINBUILDINGS_RIP  10.00000000000000000000000
							6228   HINBUILDINGS_SYN  10.00000000000000000000000
							6228   HINCOMMERCIAL_DD  10.00000000000000000000000
							6228  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6228  HINCOMMERCIAL_SYN  10.00000000000000000000000
							6228        HINCROPS_DD  10.00000000000000000000000
							6228       HINCROPS_RIP  10.00000000000000000000000
							6228       HINCROPS_SYN  10.00000000000000000000000
							6228        HINDOCKS_DD  10.00000000000000000000000
							6228       HINDOCKS_RIP  10.00000000000000000000000
							6228       HINDOCKS_SYN  10.00000000000000000000000
							6228     HINLANDFILL_DD  10.00000000000000000000000
							6228    HINLANDFILL_RIP  10.00000000000000000000000
							6228    HINLANDFILL_SYN  10.00000000000000000000000
							6228         HINLAWN_DD  10.00000000000000000000000
							6228        HINLAWN_RIP  10.00000000000000000000000
							6228        HINLAWN_SYN  10.00000000000000000000000
							6228        HINNONAG_DD 100.00000000000000000000000
							6228       HINNONAG_RIP 100.00000000000000000000000
							6228       HINNONAG_SYN 100.00000000000000000000000
							6228      HINORCHARD_DD  10.00000000000000000000000
							6228     HINORCHARD_RIP  10.00000000000000000000000
							6228     HINORCHARD_SYN  10.00000000000000000000000
							6228        HINOTHER_DD  10.00000000000000000000000
							6228       HINOTHER_RIP  10.00000000000000000000000
							6228       HINOTHER_SYN  10.00000000000000000000000
							6228         HINPARK_DD  10.00000000000000000000000
							6228        HINPARK_RIP  10.00000000000000000000000
							6228        HINPARK_SYN  10.00000000000000000000000
							6228      HINPASTURE_DD  10.00000000000000000000000
							6228     HINPASTURE_RIP  10.00000000000000000000000
							6228     HINPASTURE_SYN  10.00000000000000000000000
							6228   HINPOWERLINES_DD  10.00000000000000000000000
							6228  HINPOWERLINES_RIP  10.00000000000000000000000
							6228  HINPOWERLINES_SYN  10.00000000000000000000000
							6228        HINROADS_DD  10.00000000000000000000000
							6228       HINROADS_RIP  10.00000000000000000000000
							6228       HINROADS_SYN  10.00000000000000000000000
							6228        HINWALLS_DD  10.00000000000000000000000
							6228       HINWALLS_RIP  10.00000000000000000000000
							6228       HINWALLS_SYN  10.00000000000000000000000
							6228          HIPWAG_DD   0.00000000000000000000000
							6228         HIPWAG_RIP   0.00000000000000000000000
							6228         HIPWAG_SYN   0.00000000000000000000000
							6228         HIPWALL_DD   0.07692307689999999809061
							6228        HIPWALL_RIP   0.09615384620000000381879
							6228        HIPWALL_SYN   0.09412820510000000084894
							6228   HIPWBUILDINGS_DD   0.00000000000000000000000
							6228  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6228  HIPWBUILDINGS_SYN   0.00000000000000000000000
							6228  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6228 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6228 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6228       HIPWCROPS_DD   0.00000000000000000000000
							6228      HIPWCROPS_RIP   0.00000000000000000000000
							6228      HIPWCROPS_SYN   0.00000000000000000000000
							6228       HIPWDOCKS_DD   0.55000000000000004440892
							6228      HIPWDOCKS_RIP   0.55000000000000004440892
							6228      HIPWDOCKS_SYN   0.55000000000000004440892
							6228    HIPWLANDFILL_DD   0.05000000000000000277556
							6228   HIPWLANDFILL_RIP   0.10000000000000000555112
							6228   HIPWLANDFILL_SYN   0.09500000000000000111022
							6228        HIPWLAWN_DD   0.40000000000000002220446
							6228       HIPWLAWN_RIP   0.59999999999999997779554
							6228       HIPWLAWN_SYN   0.57866666669999999861318
							6228       HIPWNONAG_DD   0.10000000000000000555112
							6228      HIPWNONAG_RIP   0.12500000000000000000000
							6228      HIPWNONAG_SYN   0.12236666670000000134433
							6228     HIPWORCHARD_DD   0.00000000000000000000000
							6228    HIPWORCHARD_RIP   0.00000000000000000000000
							6228    HIPWORCHARD_SYN   0.00000000000000000000000
							6228       HIPWOTHER_DD   0.00000000000000000000000
							6228      HIPWOTHER_RIP   0.00000000000000000000000
							6228      HIPWOTHER_SYN   0.00000000000000000000000
							6228        HIPWPARK_DD   0.00000000000000000000000
							6228       HIPWPARK_RIP   0.00000000000000000000000
							6228       HIPWPARK_SYN   0.00000000000000000000000
							6228     HIPWPASTURE_DD   0.00000000000000000000000
							6228    HIPWPASTURE_RIP   0.00000000000000000000000
							6228    HIPWPASTURE_SYN   0.00000000000000000000000
							6228  HIPWPOWERLINES_DD   0.00000000000000000000000
							6228 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6228 HIPWPOWERLINES_SYN   0.00000000000000000000000
							6228       HIPWROADS_DD   0.00000000000000000000000
							6228      HIPWROADS_RIP   0.00000000000000000000000
							6228      HIPWROADS_SYN   0.00000000000000000000000
							6228       HIPWWALLS_DD   0.00000000000000000000000
							6228      HIPWWALLS_RIP   0.00000000000000000000000
							6228      HIPWWALLS_SYN   0.00000000000000000000000
							6279         HIFPANY_DD   1.00000000000000000000000
							6279        HIFPANY_RIP   1.00000000000000000000000
							6279        HIFPANY_SYN   1.00000000000000000000000
							6279    HIFPANYCIRCA_DD   0.20000000000000001110223
							6279   HIFPANYCIRCA_RIP   0.29999999999999998889777
							6279   HIFPANYCIRCA_SYN   0.20000000000000001110223
							6279           HIIAG_DD   0.00000000000000000000000
							6279          HIIAG_RIP   0.00000000000000000000000
							6279          HIIAG_SYN   0.00000000000000000000000
							6279      HIIAGCIRCA_DD   0.00000000000000000000000
							6279     HIIAGCIRCA_RIP   0.00000000000000000000000
							6279     HIIAGCIRCA_SYN   0.00000000000000000000000
							6279          HIIALL_DD   2.41666666669999985472828
							6279         HIIALL_RIP   2.62222222220000000802997
							6279         HIIALL_SYN   2.60351851850000004517938
							6279     HIIALLCIRCA_DD   0.29999999999999998889777
							6279    HIIALLCIRCA_RIP   0.71111111110000002621945
							6279    HIIALLCIRCA_SYN   0.29999999999999998889777
							6279        HIINONAG_DD   2.41666666669999985472828
							6279       HIINONAG_RIP   2.62222222220000000802997
							6279       HIINONAG_SYN   2.60351851850000004517938
							6279   HIINONAGCIRCA_DD   0.29999999999999998889777
							6279  HIINONAGCIRCA_RIP   0.71111111110000002621945
							6279  HIINONAGCIRCA_SYN   0.29999999999999998889777
							6279           HINAG_DD  30.00000000000000000000000
							6279          HINAG_RIP  30.00000000000000000000000
							6279          HINAG_SYN  30.00000000000000000000000
							6279          HINALL_DD 119.00000000000000000000000
							6279         HINALL_RIP 119.00000000000000000000000
							6279         HINALL_SYN 119.00000000000000000000000
							6279    HINBUILDINGS_DD  10.00000000000000000000000
							6279   HINBUILDINGS_RIP  10.00000000000000000000000
							6279   HINBUILDINGS_SYN  10.00000000000000000000000
							6279   HINCOMMERCIAL_DD  10.00000000000000000000000
							6279  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6279  HINCOMMERCIAL_SYN  10.00000000000000000000000
							6279        HINCROPS_DD  10.00000000000000000000000
							6279       HINCROPS_RIP  10.00000000000000000000000
							6279       HINCROPS_SYN  10.00000000000000000000000
							6279        HINDOCKS_DD  10.00000000000000000000000
							6279       HINDOCKS_RIP  10.00000000000000000000000
							6279       HINDOCKS_SYN  10.00000000000000000000000
							6279     HINLANDFILL_DD  10.00000000000000000000000
							6279    HINLANDFILL_RIP  10.00000000000000000000000
							6279    HINLANDFILL_SYN  10.00000000000000000000000
							6279         HINLAWN_DD  10.00000000000000000000000
							6279        HINLAWN_RIP  10.00000000000000000000000
							6279        HINLAWN_SYN  10.00000000000000000000000
							6279        HINNONAG_DD  89.00000000000000000000000
							6279       HINNONAG_RIP  89.00000000000000000000000
							6279       HINNONAG_SYN  89.00000000000000000000000
							6279      HINORCHARD_DD  10.00000000000000000000000
							6279     HINORCHARD_RIP  10.00000000000000000000000
							6279     HINORCHARD_SYN  10.00000000000000000000000
							6279        HINOTHER_DD   0.00000000000000000000000
							6279       HINOTHER_RIP   0.00000000000000000000000
							6279       HINOTHER_SYN   0.00000000000000000000000
							6279         HINPARK_DD  10.00000000000000000000000
							6279        HINPARK_RIP  10.00000000000000000000000
							6279        HINPARK_SYN  10.00000000000000000000000
							6279      HINPASTURE_DD  10.00000000000000000000000
							6279     HINPASTURE_RIP  10.00000000000000000000000
							6279     HINPASTURE_SYN  10.00000000000000000000000
							6279   HINPOWERLINES_DD  10.00000000000000000000000
							6279  HINPOWERLINES_RIP  10.00000000000000000000000
							6279  HINPOWERLINES_SYN  10.00000000000000000000000
							6279        HINROADS_DD  10.00000000000000000000000
							6279       HINROADS_RIP  10.00000000000000000000000
							6279       HINROADS_SYN  10.00000000000000000000000
							6279        HINWALLS_DD   9.00000000000000000000000
							6279       HINWALLS_RIP   9.00000000000000000000000
							6279       HINWALLS_SYN   9.00000000000000000000000
							6279          HIPWAG_DD   0.00000000000000000000000
							6279         HIPWAG_RIP   0.00000000000000000000000
							6279         HIPWAG_SYN   0.00000000000000000000000
							6279         HIPWALL_DD   0.20168067230000000233936
							6279        HIPWALL_RIP   0.21848739500000000091084
							6279        HIPWALL_SYN   0.21694677870000000918793
							6279   HIPWBUILDINGS_DD   0.14999999999999999444888
							6279  HIPWBUILDINGS_RIP   0.20000000000000001110223
							6279  HIPWBUILDINGS_SYN   0.19166666669999998795504
							6279  HIPWCOMMERCIAL_DD   0.10000000000000000555112
							6279 HIPWCOMMERCIAL_RIP   0.10000000000000000555112
							6279 HIPWCOMMERCIAL_SYN   0.10000000000000000555112
							6279       HIPWCROPS_DD   0.00000000000000000000000
							6279      HIPWCROPS_RIP   0.00000000000000000000000
							6279      HIPWCROPS_SYN   0.00000000000000000000000
							6279       HIPWDOCKS_DD   0.25000000000000000000000
							6279      HIPWDOCKS_RIP   0.25000000000000000000000
							6279      HIPWDOCKS_SYN   0.25000000000000000000000
							6279    HIPWLANDFILL_DD   0.05000000000000000277556
							6279   HIPWLANDFILL_RIP   0.05000000000000000277556
							6279   HIPWLANDFILL_SYN   0.05000000000000000277556
							6279        HIPWLAWN_DD   0.10000000000000000555112
							6279       HIPWLAWN_RIP   0.10000000000000000555112
							6279       HIPWLAWN_SYN   0.10000000000000000555112
							6279       HIPWNONAG_DD   0.26966292130000002469359
							6279      HIPWNONAG_RIP   0.29213483150000002508406
							6279      HIPWNONAG_SYN   0.29007490639999999881837
							6279     HIPWORCHARD_DD   0.00000000000000000000000
							6279    HIPWORCHARD_RIP   0.00000000000000000000000
							6279    HIPWORCHARD_SYN   0.00000000000000000000000
							6279       HIPWOTHER_DD                          NA
							6279      HIPWOTHER_RIP                          NA
							6279      HIPWOTHER_SYN                          NA
							6279        HIPWPARK_DD   0.55000000000000004440892
							6279       HIPWPARK_RIP   0.55000000000000004440892
							6279       HIPWPARK_SYN   0.55000000000000004440892
							6279     HIPWPASTURE_DD   0.00000000000000000000000
							6279    HIPWPASTURE_RIP   0.00000000000000000000000
							6279    HIPWPASTURE_SYN   0.00000000000000000000000
							6279  HIPWPOWERLINES_DD   0.50000000000000000000000
							6279 HIPWPOWERLINES_RIP   0.55000000000000004440892
							6279 HIPWPOWERLINES_SYN   0.54666666669999997019147
							6279       HIPWROADS_DD   0.55000000000000004440892
							6279      HIPWROADS_RIP   0.59999999999999997779554
							6279      HIPWROADS_SYN   0.59666666670000001460039
							6279       HIPWWALLS_DD   0.16666666669999999350615
							6279      HIPWWALLS_RIP   0.22222222220000001358109
							6279      HIPWWALLS_SYN   0.21851851850000000854202
							6302         HIFPANY_DD   1.00000000000000000000000
							6302        HIFPANY_RIP   0.44444444440000002716218
							6302        HIFPANY_SYN   1.00000000000000000000000
							6302    HIFPANYCIRCA_DD   1.00000000000000000000000
							6302   HIFPANYCIRCA_RIP   0.33333333329999997873827
							6302   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6302           HIIAG_DD   0.00000000000000000000000
							6302          HIIAG_RIP   0.00000000000000000000000
							6302          HIIAG_SYN   0.00000000000000000000000
							6302      HIIAGCIRCA_DD   0.00000000000000000000000
							6302     HIIAGCIRCA_RIP   0.00000000000000000000000
							6302     HIIAGCIRCA_SYN   0.00000000000000000000000
							6302          HIIALL_DD   1.12500000000000000000000
							6302         HIIALL_RIP   0.90277777780000001417449
							6302         HIIALL_SYN   1.29907407409999997582872
							6302     HIIALLCIRCA_DD   1.01388888889999995157609
							6302    HIIALLCIRCA_RIP   0.79166666669999996575058
							6302    HIIALLCIRCA_SYN   1.01388888889999995157609
							6302        HIINONAG_DD   1.12500000000000000000000
							6302       HIINONAG_RIP   0.90277777780000001417449
							6302       HIINONAG_SYN   1.29907407409999997582872
							6302   HIINONAGCIRCA_DD   1.01388888889999995157609
							6302  HIINONAGCIRCA_RIP   0.79166666669999996575058
							6302  HIINONAGCIRCA_SYN   1.01388888889999995157609
							6302           HINAG_DD  27.00000000000000000000000
							6302          HINAG_RIP  27.00000000000000000000000
							6302          HINAG_SYN  27.00000000000000000000000
							6302          HINALL_DD 116.00000000000000000000000
							6302         HINALL_RIP 115.00000000000000000000000
							6302         HINALL_SYN 116.00000000000000000000000
							6302    HINBUILDINGS_DD   9.00000000000000000000000
							6302   HINBUILDINGS_RIP   8.00000000000000000000000
							6302   HINBUILDINGS_SYN   9.00000000000000000000000
							6302   HINCOMMERCIAL_DD   9.00000000000000000000000
							6302  HINCOMMERCIAL_RIP   9.00000000000000000000000
							6302  HINCOMMERCIAL_SYN   9.00000000000000000000000
							6302        HINCROPS_DD   9.00000000000000000000000
							6302       HINCROPS_RIP   9.00000000000000000000000
							6302       HINCROPS_SYN   9.00000000000000000000000
							6302        HINDOCKS_DD   9.00000000000000000000000
							6302       HINDOCKS_RIP   9.00000000000000000000000
							6302       HINDOCKS_SYN   9.00000000000000000000000
							6302     HINLANDFILL_DD   9.00000000000000000000000
							6302    HINLANDFILL_RIP   9.00000000000000000000000
							6302    HINLANDFILL_SYN   9.00000000000000000000000
							6302         HINLAWN_DD   9.00000000000000000000000
							6302        HINLAWN_RIP   9.00000000000000000000000
							6302        HINLAWN_SYN   9.00000000000000000000000
							6302        HINNONAG_DD  89.00000000000000000000000
							6302       HINNONAG_RIP  88.00000000000000000000000
							6302       HINNONAG_SYN  89.00000000000000000000000
							6302      HINORCHARD_DD   9.00000000000000000000000
							6302     HINORCHARD_RIP   9.00000000000000000000000
							6302     HINORCHARD_SYN   9.00000000000000000000000
							6302        HINOTHER_DD   9.00000000000000000000000
							6302       HINOTHER_RIP   8.00000000000000000000000
							6302       HINOTHER_SYN   9.00000000000000000000000
							6302         HINPARK_DD   9.00000000000000000000000
							6302        HINPARK_RIP   9.00000000000000000000000
							6302        HINPARK_SYN   9.00000000000000000000000
							6302      HINPASTURE_DD   9.00000000000000000000000
							6302     HINPASTURE_RIP   9.00000000000000000000000
							6302     HINPASTURE_SYN   9.00000000000000000000000
							6302   HINPOWERLINES_DD   9.00000000000000000000000
							6302  HINPOWERLINES_RIP   9.00000000000000000000000
							6302  HINPOWERLINES_SYN   9.00000000000000000000000
							6302        HINROADS_DD   9.00000000000000000000000
							6302       HINROADS_RIP   9.00000000000000000000000
							6302       HINROADS_SYN   9.00000000000000000000000
							6302        HINWALLS_DD   8.00000000000000000000000
							6302       HINWALLS_RIP   9.00000000000000000000000
							6302       HINWALLS_SYN   8.00000000000000000000000
							6302          HIPWAG_DD   0.00000000000000000000000
							6302         HIPWAG_RIP   0.00000000000000000000000
							6302         HIPWAG_SYN   0.00000000000000000000000
							6302         HIPWALL_DD   0.08620689659999999920892
							6302        HIPWALL_RIP   0.06956521740000000397774
							6302        HIPWALL_SYN   0.09971264370000000598626
							6302   HIPWBUILDINGS_DD   0.00000000000000000000000
							6302  HIPWBUILDINGS_RIP   0.12500000000000000000000
							6302  HIPWBUILDINGS_SYN   0.09629629630000000051204
							6302  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6302 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6302 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6302       HIPWCROPS_DD   0.00000000000000000000000
							6302      HIPWCROPS_RIP   0.00000000000000000000000
							6302      HIPWCROPS_SYN   0.00000000000000000000000
							6302       HIPWDOCKS_DD   0.05555555560000000059340
							6302      HIPWDOCKS_RIP   0.05555555560000000059340
							6302      HIPWDOCKS_SYN   0.05555555560000000059340
							6302    HIPWLANDFILL_DD   0.77777777780000001417449
							6302   HIPWLANDFILL_RIP   0.00000000000000000000000
							6302   HIPWLANDFILL_SYN   0.77777777780000001417449
							6302        HIPWLAWN_DD   0.00000000000000000000000
							6302       HIPWLAWN_RIP   0.11111111110000000679054
							6302       HIPWLAWN_SYN   0.00000000000000000000000
							6302       HIPWNONAG_DD   0.11235955059999999661180
							6302      HIPWNONAG_RIP   0.09090909090000000303267
							6302      HIPWNONAG_SYN   0.12996254679999999503970
							6302     HIPWORCHARD_DD   0.00000000000000000000000
							6302    HIPWORCHARD_RIP   0.00000000000000000000000
							6302    HIPWORCHARD_SYN   0.00000000000000000000000
							6302       HIPWOTHER_DD   0.11111111110000000679054
							6302      HIPWOTHER_RIP   0.00000000000000000000000
							6302      HIPWOTHER_SYN   0.11111111110000000679054
							6302        HIPWPARK_DD   0.05555555560000000059340
							6302       HIPWPARK_RIP   0.11111111110000000679054
							6302       HIPWPARK_SYN   0.05555555560000000059340
							6302     HIPWPASTURE_DD   0.00000000000000000000000
							6302    HIPWPASTURE_RIP   0.00000000000000000000000
							6302    HIPWPASTURE_SYN   0.00000000000000000000000
							6302  HIPWPOWERLINES_DD   0.00000000000000000000000
							6302 HIPWPOWERLINES_RIP   0.22222222220000001358109
							6302 HIPWPOWERLINES_SYN   0.07777777780000000307226
							6302       HIPWROADS_DD   0.00000000000000000000000
							6302      HIPWROADS_RIP   0.16666666669999999350615
							6302      HIPWROADS_SYN   0.00000000000000000000000
							6302       HIPWWALLS_DD   0.12500000000000000000000
							6302      HIPWWALLS_RIP   0.11111111110000000679054
							6302      HIPWWALLS_SYN   0.12500000000000000000000
							6303         HIFPANY_DD   0.00000000000000000000000
							6303        HIFPANY_RIP   0.80000000000000004440892
							6303        HIFPANY_SYN   0.77777777780000001417449
							6303    HIFPANYCIRCA_DD   0.00000000000000000000000
							6303   HIFPANYCIRCA_RIP   0.50000000000000000000000
							6303   HIFPANYCIRCA_SYN   0.55555555560000002834897
							6303           HIIAG_DD   0.00000000000000000000000
							6303          HIIAG_RIP   0.00000000000000000000000
							6303          HIIAG_SYN   0.00000000000000000000000
							6303      HIIAGCIRCA_DD   0.00000000000000000000000
							6303     HIIAGCIRCA_RIP   0.00000000000000000000000
							6303     HIIAGCIRCA_SYN   0.00000000000000000000000
							6303          HIIALL_DD   0.00000000000000000000000
							6303         HIIALL_RIP   1.44999999999999995559108
							6303         HIIALL_SYN   1.55555555560000002834897
							6303     HIIALLCIRCA_DD   0.00000000000000000000000
							6303    HIIALLCIRCA_RIP   1.10000000000000008881784
							6303    HIIALLCIRCA_SYN   1.22222222220000009684782
							6303        HIINONAG_DD   0.00000000000000000000000
							6303       HIINONAG_RIP   1.44999999999999995559108
							6303       HIINONAG_SYN   1.55555555560000002834897
							6303   HIINONAGCIRCA_DD   0.00000000000000000000000
							6303  HIINONAGCIRCA_RIP   1.10000000000000008881784
							6303  HIINONAGCIRCA_SYN   1.22222222220000009684782
							6303           HINAG_DD  27.00000000000000000000000
							6303          HINAG_RIP  30.00000000000000000000000
							6303          HINAG_SYN  27.00000000000000000000000
							6303          HINALL_DD 117.00000000000000000000000
							6303         HINALL_RIP 130.00000000000000000000000
							6303         HINALL_SYN 117.00000000000000000000000
							6303    HINBUILDINGS_DD   9.00000000000000000000000
							6303   HINBUILDINGS_RIP  10.00000000000000000000000
							6303   HINBUILDINGS_SYN   9.00000000000000000000000
							6303   HINCOMMERCIAL_DD   9.00000000000000000000000
							6303  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6303  HINCOMMERCIAL_SYN   9.00000000000000000000000
							6303        HINCROPS_DD   9.00000000000000000000000
							6303       HINCROPS_RIP  10.00000000000000000000000
							6303       HINCROPS_SYN   9.00000000000000000000000
							6303        HINDOCKS_DD   9.00000000000000000000000
							6303       HINDOCKS_RIP  10.00000000000000000000000
							6303       HINDOCKS_SYN   9.00000000000000000000000
							6303     HINLANDFILL_DD   9.00000000000000000000000
							6303    HINLANDFILL_RIP  10.00000000000000000000000
							6303    HINLANDFILL_SYN   9.00000000000000000000000
							6303         HINLAWN_DD   9.00000000000000000000000
							6303        HINLAWN_RIP  10.00000000000000000000000
							6303        HINLAWN_SYN   9.00000000000000000000000
							6303        HINNONAG_DD  90.00000000000000000000000
							6303       HINNONAG_RIP 100.00000000000000000000000
							6303       HINNONAG_SYN  90.00000000000000000000000
							6303      HINORCHARD_DD   9.00000000000000000000000
							6303     HINORCHARD_RIP  10.00000000000000000000000
							6303     HINORCHARD_SYN   9.00000000000000000000000
							6303        HINOTHER_DD   9.00000000000000000000000
							6303       HINOTHER_RIP  10.00000000000000000000000
							6303       HINOTHER_SYN   9.00000000000000000000000
							6303         HINPARK_DD   9.00000000000000000000000
							6303        HINPARK_RIP  10.00000000000000000000000
							6303        HINPARK_SYN   9.00000000000000000000000
							6303      HINPASTURE_DD   9.00000000000000000000000
							6303     HINPASTURE_RIP  10.00000000000000000000000
							6303     HINPASTURE_SYN   9.00000000000000000000000
							6303   HINPOWERLINES_DD   9.00000000000000000000000
							6303  HINPOWERLINES_RIP  10.00000000000000000000000
							6303  HINPOWERLINES_SYN   9.00000000000000000000000
							6303        HINROADS_DD   9.00000000000000000000000
							6303       HINROADS_RIP  10.00000000000000000000000
							6303       HINROADS_SYN   9.00000000000000000000000
							6303        HINWALLS_DD   9.00000000000000000000000
							6303       HINWALLS_RIP  10.00000000000000000000000
							6303       HINWALLS_SYN   9.00000000000000000000000
							6303          HIPWAG_DD   0.00000000000000000000000
							6303         HIPWAG_RIP   0.00000000000000000000000
							6303         HIPWAG_SYN   0.00000000000000000000000
							6303         HIPWALL_DD   0.00000000000000000000000
							6303        HIPWALL_RIP   0.11153846150000000514435
							6303        HIPWALL_SYN   0.11965811969999999575531
							6303   HIPWBUILDINGS_DD   0.00000000000000000000000
							6303  HIPWBUILDINGS_RIP   0.14999999999999999444888
							6303  HIPWBUILDINGS_SYN   0.16666666669999999350615
							6303  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6303 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6303 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6303       HIPWCROPS_DD   0.00000000000000000000000
							6303      HIPWCROPS_RIP   0.00000000000000000000000
							6303      HIPWCROPS_SYN   0.00000000000000000000000
							6303       HIPWDOCKS_DD   0.00000000000000000000000
							6303      HIPWDOCKS_RIP   0.05000000000000000277556
							6303      HIPWDOCKS_SYN   0.05555555560000000059340
							6303    HIPWLANDFILL_DD   0.00000000000000000000000
							6303   HIPWLANDFILL_RIP   0.65000000000000002220446
							6303   HIPWLANDFILL_SYN   0.66666666669999996575058
							6303        HIPWLAWN_DD   0.00000000000000000000000
							6303       HIPWLAWN_RIP   0.20000000000000001110223
							6303       HIPWLAWN_SYN   0.22222222220000001358109
							6303       HIPWNONAG_DD   0.00000000000000000000000
							6303      HIPWNONAG_RIP   0.14499999999999999000799
							6303      HIPWNONAG_SYN   0.15555555560000000614451
							6303     HIPWORCHARD_DD   0.00000000000000000000000
							6303    HIPWORCHARD_RIP   0.00000000000000000000000
							6303    HIPWORCHARD_SYN   0.00000000000000000000000
							6303       HIPWOTHER_DD   0.00000000000000000000000
							6303      HIPWOTHER_RIP   0.00000000000000000000000
							6303      HIPWOTHER_SYN   0.00000000000000000000000
							6303        HIPWPARK_DD   0.00000000000000000000000
							6303       HIPWPARK_RIP   0.05000000000000000277556
							6303       HIPWPARK_SYN   0.05555555560000000059340
							6303     HIPWPASTURE_DD   0.00000000000000000000000
							6303    HIPWPASTURE_RIP   0.00000000000000000000000
							6303    HIPWPASTURE_SYN   0.00000000000000000000000
							6303  HIPWPOWERLINES_DD   0.00000000000000000000000
							6303 HIPWPOWERLINES_RIP   0.14999999999999999444888
							6303 HIPWPOWERLINES_SYN   0.16666666669999999350615
							6303       HIPWROADS_DD   0.00000000000000000000000
							6303      HIPWROADS_RIP   0.10000000000000000555112
							6303      HIPWROADS_SYN   0.11111111110000000679054
							6303       HIPWWALLS_DD   0.00000000000000000000000
							6303      HIPWWALLS_RIP   0.10000000000000000555112
							6303      HIPWWALLS_SYN   0.11111111110000000679054
							6362         HIFPANY_DD   0.00000000000000000000000
							6362        HIFPANY_RIP   1.00000000000000000000000
							6362        HIFPANY_SYN   0.00000000000000000000000
							6362    HIFPANYCIRCA_DD   0.00000000000000000000000
							6362   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6362   HIFPANYCIRCA_SYN   0.00000000000000000000000
							6362           HIIAG_DD   0.00000000000000000000000
							6362          HIIAG_RIP   1.00000000000000000000000
							6362          HIIAG_SYN   0.00000000000000000000000
							6362      HIIAGCIRCA_DD   0.00000000000000000000000
							6362     HIIAGCIRCA_RIP   1.00000000000000000000000
							6362     HIIAGCIRCA_SYN   0.00000000000000000000000
							6362          HIIALL_DD   0.00000000000000000000000
							6362         HIIALL_RIP   1.00000000000000000000000
							6362         HIIALL_SYN   0.00000000000000000000000
							6362     HIIALLCIRCA_DD   0.00000000000000000000000
							6362    HIIALLCIRCA_RIP   1.00000000000000000000000
							6362    HIIALLCIRCA_SYN   0.00000000000000000000000
							6362        HIINONAG_DD   0.00000000000000000000000
							6362       HIINONAG_RIP   0.00000000000000000000000
							6362       HIINONAG_SYN   0.00000000000000000000000
							6362   HIINONAGCIRCA_DD   0.00000000000000000000000
							6362  HIINONAGCIRCA_RIP   0.00000000000000000000000
							6362  HIINONAGCIRCA_SYN   0.00000000000000000000000
							6362           HINAG_DD   3.00000000000000000000000
							6362          HINAG_RIP   3.00000000000000000000000
							6362          HINAG_SYN   3.00000000000000000000000
							6362          HINALL_DD  13.00000000000000000000000
							6362         HINALL_RIP  13.00000000000000000000000
							6362         HINALL_SYN  13.00000000000000000000000
							6362    HINBUILDINGS_DD   1.00000000000000000000000
							6362   HINBUILDINGS_RIP   1.00000000000000000000000
							6362   HINBUILDINGS_SYN   1.00000000000000000000000
							6362   HINCOMMERCIAL_DD   1.00000000000000000000000
							6362  HINCOMMERCIAL_RIP   1.00000000000000000000000
							6362  HINCOMMERCIAL_SYN   1.00000000000000000000000
							6362        HINCROPS_DD   1.00000000000000000000000
							6362       HINCROPS_RIP   1.00000000000000000000000
							6362       HINCROPS_SYN   1.00000000000000000000000
							6362        HINDOCKS_DD   1.00000000000000000000000
							6362       HINDOCKS_RIP   1.00000000000000000000000
							6362       HINDOCKS_SYN   1.00000000000000000000000
							6362     HINLANDFILL_DD   1.00000000000000000000000
							6362    HINLANDFILL_RIP   1.00000000000000000000000
							6362    HINLANDFILL_SYN   1.00000000000000000000000
							6362         HINLAWN_DD   1.00000000000000000000000
							6362        HINLAWN_RIP   1.00000000000000000000000
							6362        HINLAWN_SYN   1.00000000000000000000000
							6362        HINNONAG_DD  10.00000000000000000000000
							6362       HINNONAG_RIP  10.00000000000000000000000
							6362       HINNONAG_SYN  10.00000000000000000000000
							6362      HINORCHARD_DD   1.00000000000000000000000
							6362     HINORCHARD_RIP   1.00000000000000000000000
							6362     HINORCHARD_SYN   1.00000000000000000000000
							6362        HINOTHER_DD   1.00000000000000000000000
							6362       HINOTHER_RIP   1.00000000000000000000000
							6362       HINOTHER_SYN   1.00000000000000000000000
							6362         HINPARK_DD   1.00000000000000000000000
							6362        HINPARK_RIP   1.00000000000000000000000
							6362        HINPARK_SYN   1.00000000000000000000000
							6362      HINPASTURE_DD   1.00000000000000000000000
							6362     HINPASTURE_RIP   1.00000000000000000000000
							6362     HINPASTURE_SYN   1.00000000000000000000000
							6362   HINPOWERLINES_DD   1.00000000000000000000000
							6362  HINPOWERLINES_RIP   1.00000000000000000000000
							6362  HINPOWERLINES_SYN   1.00000000000000000000000
							6362        HINROADS_DD   1.00000000000000000000000
							6362       HINROADS_RIP   1.00000000000000000000000
							6362       HINROADS_SYN   1.00000000000000000000000
							6362        HINWALLS_DD   1.00000000000000000000000
							6362       HINWALLS_RIP   1.00000000000000000000000
							6362       HINWALLS_SYN   1.00000000000000000000000
							6362          HIPWAG_DD   0.00000000000000000000000
							6362         HIPWAG_RIP   0.33333333329999997873827
							6362         HIPWAG_SYN   0.00000000000000000000000
							6362         HIPWALL_DD   0.00000000000000000000000
							6362        HIPWALL_RIP   0.07692307689999999809061
							6362        HIPWALL_SYN   0.00000000000000000000000
							6362   HIPWBUILDINGS_DD   0.00000000000000000000000
							6362  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6362  HIPWBUILDINGS_SYN   0.00000000000000000000000
							6362  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6362 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6362 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6362       HIPWCROPS_DD   0.00000000000000000000000
							6362      HIPWCROPS_RIP   0.00000000000000000000000
							6362      HIPWCROPS_SYN   0.00000000000000000000000
							6362       HIPWDOCKS_DD   0.00000000000000000000000
							6362      HIPWDOCKS_RIP   0.00000000000000000000000
							6362      HIPWDOCKS_SYN   0.00000000000000000000000
							6362    HIPWLANDFILL_DD   0.00000000000000000000000
							6362   HIPWLANDFILL_RIP   0.00000000000000000000000
							6362   HIPWLANDFILL_SYN   0.00000000000000000000000
							6362        HIPWLAWN_DD   0.00000000000000000000000
							6362       HIPWLAWN_RIP   0.00000000000000000000000
							6362       HIPWLAWN_SYN   0.00000000000000000000000
							6362       HIPWNONAG_DD   0.00000000000000000000000
							6362      HIPWNONAG_RIP   0.00000000000000000000000
							6362      HIPWNONAG_SYN   0.00000000000000000000000
							6362     HIPWORCHARD_DD   0.00000000000000000000000
							6362    HIPWORCHARD_RIP   0.00000000000000000000000
							6362    HIPWORCHARD_SYN   0.00000000000000000000000
							6362       HIPWOTHER_DD   0.00000000000000000000000
							6362      HIPWOTHER_RIP   0.00000000000000000000000
							6362      HIPWOTHER_SYN   0.00000000000000000000000
							6362        HIPWPARK_DD   0.00000000000000000000000
							6362       HIPWPARK_RIP   0.00000000000000000000000
							6362       HIPWPARK_SYN   0.00000000000000000000000
							6362     HIPWPASTURE_DD   0.00000000000000000000000
							6362    HIPWPASTURE_RIP   1.00000000000000000000000
							6362    HIPWPASTURE_SYN   0.00000000000000000000000
							6362  HIPWPOWERLINES_DD   0.00000000000000000000000
							6362 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6362 HIPWPOWERLINES_SYN   0.00000000000000000000000
							6362       HIPWROADS_DD   0.00000000000000000000000
							6362      HIPWROADS_RIP   0.00000000000000000000000
							6362      HIPWROADS_SYN   0.00000000000000000000000
							6362       HIPWWALLS_DD   0.00000000000000000000000
							6362      HIPWWALLS_RIP   0.00000000000000000000000
							6362      HIPWWALLS_SYN   0.00000000000000000000000
							6399         HIFPANY_DD   0.20000000000000001110223
							6399        HIFPANY_RIP   0.80000000000000004440892
							6399        HIFPANY_SYN   0.80000000000000004440892
							6399    HIFPANYCIRCA_DD   0.20000000000000001110223
							6399   HIFPANYCIRCA_RIP   0.69999999999999995559108
							6399   HIFPANYCIRCA_SYN   0.69999999999999995559108
							6399           HIIAG_DD   0.00000000000000000000000
							6399          HIIAG_RIP   0.00000000000000000000000
							6399          HIIAG_SYN   0.00000000000000000000000
							6399      HIIAGCIRCA_DD   0.00000000000000000000000
							6399     HIIAGCIRCA_RIP   0.00000000000000000000000
							6399     HIIAGCIRCA_SYN   0.00000000000000000000000
							6399          HIIALL_DD   0.84999999999999997779554
							6399         HIIALL_RIP   2.64999999999999991118216
							6399         HIIALL_SYN   2.64666666669999983696471
							6399     HIIALLCIRCA_DD   0.50000000000000000000000
							6399    HIIALLCIRCA_RIP   1.50000000000000000000000
							6399    HIIALLCIRCA_SYN   1.39999999999999991118216
							6399        HIINONAG_DD   0.84999999999999997779554
							6399       HIINONAG_RIP   2.64999999999999991118216
							6399       HIINONAG_SYN   2.64666666669999983696471
							6399   HIINONAGCIRCA_DD   0.50000000000000000000000
							6399  HIINONAGCIRCA_RIP   1.50000000000000000000000
							6399  HIINONAGCIRCA_SYN   1.39999999999999991118216
							6399           HINAG_DD  30.00000000000000000000000
							6399          HINAG_RIP  30.00000000000000000000000
							6399          HINAG_SYN  30.00000000000000000000000
							6399          HINALL_DD 130.00000000000000000000000
							6399         HINALL_RIP 129.00000000000000000000000
							6399         HINALL_SYN 129.00000000000000000000000
							6399    HINBUILDINGS_DD  10.00000000000000000000000
							6399   HINBUILDINGS_RIP  10.00000000000000000000000
							6399   HINBUILDINGS_SYN  10.00000000000000000000000
							6399   HINCOMMERCIAL_DD  10.00000000000000000000000
							6399  HINCOMMERCIAL_RIP  10.00000000000000000000000
							6399  HINCOMMERCIAL_SYN  10.00000000000000000000000
							6399        HINCROPS_DD  10.00000000000000000000000
							6399       HINCROPS_RIP  10.00000000000000000000000
							6399       HINCROPS_SYN  10.00000000000000000000000
							6399        HINDOCKS_DD  10.00000000000000000000000
							6399       HINDOCKS_RIP  10.00000000000000000000000
							6399       HINDOCKS_SYN  10.00000000000000000000000
							6399     HINLANDFILL_DD  10.00000000000000000000000
							6399    HINLANDFILL_RIP  10.00000000000000000000000
							6399    HINLANDFILL_SYN  10.00000000000000000000000
							6399         HINLAWN_DD  10.00000000000000000000000
							6399        HINLAWN_RIP  10.00000000000000000000000
							6399        HINLAWN_SYN  10.00000000000000000000000
							6399        HINNONAG_DD 100.00000000000000000000000
							6399       HINNONAG_RIP  99.00000000000000000000000
							6399       HINNONAG_SYN  99.00000000000000000000000
							6399      HINORCHARD_DD  10.00000000000000000000000
							6399     HINORCHARD_RIP  10.00000000000000000000000
							6399     HINORCHARD_SYN  10.00000000000000000000000
							6399        HINOTHER_DD  10.00000000000000000000000
							6399       HINOTHER_RIP  10.00000000000000000000000
							6399       HINOTHER_SYN  10.00000000000000000000000
							6399         HINPARK_DD  10.00000000000000000000000
							6399        HINPARK_RIP   9.00000000000000000000000
							6399        HINPARK_SYN   9.00000000000000000000000
							6399      HINPASTURE_DD  10.00000000000000000000000
							6399     HINPASTURE_RIP  10.00000000000000000000000
							6399     HINPASTURE_SYN  10.00000000000000000000000
							6399   HINPOWERLINES_DD  10.00000000000000000000000
							6399  HINPOWERLINES_RIP  10.00000000000000000000000
							6399  HINPOWERLINES_SYN  10.00000000000000000000000
							6399        HINROADS_DD  10.00000000000000000000000
							6399       HINROADS_RIP  10.00000000000000000000000
							6399       HINROADS_SYN  10.00000000000000000000000
							6399        HINWALLS_DD  10.00000000000000000000000
							6399       HINWALLS_RIP  10.00000000000000000000000
							6399       HINWALLS_SYN  10.00000000000000000000000
							6399          HIPWAG_DD   0.00000000000000000000000
							6399         HIPWAG_RIP   0.00000000000000000000000
							6399         HIPWAG_SYN   0.00000000000000000000000
							6399         HIPWALL_DD   0.06538461539999999849737
							6399        HIPWALL_RIP   0.20542635659999999853120
							6399        HIPWALL_SYN   0.20516795870000001378308
							6399   HIPWBUILDINGS_DD   0.14999999999999999444888
							6399  HIPWBUILDINGS_RIP   0.45000000000000001110223
							6399  HIPWBUILDINGS_SYN   0.45000000000000001110223
							6399  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6399 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6399 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6399       HIPWCROPS_DD   0.00000000000000000000000
							6399      HIPWCROPS_RIP   0.00000000000000000000000
							6399      HIPWCROPS_SYN   0.00000000000000000000000
							6399       HIPWDOCKS_DD   0.14999999999999999444888
							6399      HIPWDOCKS_RIP   0.40000000000000002220446
							6399      HIPWDOCKS_SYN   0.40000000000000002220446
							6399    HIPWLANDFILL_DD   0.10000000000000000555112
							6399   HIPWLANDFILL_RIP   0.20000000000000001110223
							6399   HIPWLANDFILL_SYN   0.20000000000000001110223
							6399        HIPWLAWN_DD   0.05000000000000000277556
							6399       HIPWLAWN_RIP   0.55000000000000004440892
							6399       HIPWLAWN_SYN   0.54666666669999997019147
							6399       HIPWNONAG_DD   0.08500000000000000610623
							6399      HIPWNONAG_RIP   0.26767676769999998453997
							6399      HIPWNONAG_SYN   0.26734006729999998386305
							6399     HIPWORCHARD_DD   0.00000000000000000000000
							6399    HIPWORCHARD_RIP   0.00000000000000000000000
							6399    HIPWORCHARD_SYN   0.00000000000000000000000
							6399       HIPWOTHER_DD   0.00000000000000000000000
							6399      HIPWOTHER_RIP   0.00000000000000000000000
							6399      HIPWOTHER_SYN   0.00000000000000000000000
							6399        HIPWPARK_DD   0.00000000000000000000000
							6399       HIPWPARK_RIP   0.00000000000000000000000
							6399       HIPWPARK_SYN   0.00000000000000000000000
							6399     HIPWPASTURE_DD   0.00000000000000000000000
							6399    HIPWPASTURE_RIP   0.00000000000000000000000
							6399    HIPWPASTURE_SYN   0.00000000000000000000000
							6399  HIPWPOWERLINES_DD   0.10000000000000000555112
							6399 HIPWPOWERLINES_RIP   0.29999999999999998889777
							6399 HIPWPOWERLINES_SYN   0.29999999999999998889777
							6399       HIPWROADS_DD   0.10000000000000000555112
							6399      HIPWROADS_RIP   0.14999999999999999444888
							6399      HIPWROADS_SYN   0.14999999999999999444888
							6399       HIPWWALLS_DD   0.20000000000000001110223
							6399      HIPWWALLS_RIP   0.59999999999999997779554
							6399      HIPWWALLS_SYN   0.59999999999999997779554
							6449         HIFPANY_DD   1.00000000000000000000000
							6449        HIFPANY_RIP   1.00000000000000000000000
							6449        HIFPANY_SYN   1.00000000000000000000000
							6449    HIFPANYCIRCA_DD   1.00000000000000000000000
							6449   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6449   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6449           HIIAG_DD   0.00000000000000000000000
							6449          HIIAG_RIP   0.00000000000000000000000
							6449          HIIAG_SYN   0.00000000000000000000000
							6449      HIIAGCIRCA_DD   0.00000000000000000000000
							6449     HIIAGCIRCA_RIP   0.00000000000000000000000
							6449     HIIAGCIRCA_SYN   0.00000000000000000000000
							6449          HIIALL_DD   3.54166666669999985472828
							6449         HIIALL_RIP   4.12500000000000000000000
							6449         HIIALL_SYN   4.35805555560000001946719
							6449     HIIALLCIRCA_DD   1.50000000000000000000000
							6449    HIIALLCIRCA_RIP   2.58333333330000014527172
							6449    HIIALLCIRCA_SYN   1.50000000000000000000000
							6449        HIINONAG_DD   3.54166666669999985472828
							6449       HIINONAG_RIP   4.12500000000000000000000
							6449       HIINONAG_SYN   4.35805555560000001946719
							6449   HIINONAGCIRCA_DD   1.50000000000000000000000
							6449  HIINONAGCIRCA_RIP   2.58333333330000014527172
							6449  HIINONAGCIRCA_SYN   1.50000000000000000000000
							6449           HINAG_DD  36.00000000000000000000000
							6449          HINAG_RIP  36.00000000000000000000000
							6449          HINAG_SYN  36.00000000000000000000000
							6449          HINALL_DD 155.00000000000000000000000
							6449         HINALL_RIP 156.00000000000000000000000
							6449         HINALL_SYN 155.00000000000000000000000
							6449    HINBUILDINGS_DD  12.00000000000000000000000
							6449   HINBUILDINGS_RIP  12.00000000000000000000000
							6449   HINBUILDINGS_SYN  12.00000000000000000000000
							6449   HINCOMMERCIAL_DD  12.00000000000000000000000
							6449  HINCOMMERCIAL_RIP  12.00000000000000000000000
							6449  HINCOMMERCIAL_SYN  12.00000000000000000000000
							6449        HINCROPS_DD  12.00000000000000000000000
							6449       HINCROPS_RIP  12.00000000000000000000000
							6449       HINCROPS_SYN  12.00000000000000000000000
							6449        HINDOCKS_DD  12.00000000000000000000000
							6449       HINDOCKS_RIP  12.00000000000000000000000
							6449       HINDOCKS_SYN  12.00000000000000000000000
							6449     HINLANDFILL_DD  12.00000000000000000000000
							6449    HINLANDFILL_RIP  12.00000000000000000000000
							6449    HINLANDFILL_SYN  12.00000000000000000000000
							6449         HINLAWN_DD  12.00000000000000000000000
							6449        HINLAWN_RIP  12.00000000000000000000000
							6449        HINLAWN_SYN  12.00000000000000000000000
							6449        HINNONAG_DD 119.00000000000000000000000
							6449       HINNONAG_RIP 120.00000000000000000000000
							6449       HINNONAG_SYN 119.00000000000000000000000
							6449      HINORCHARD_DD  12.00000000000000000000000
							6449     HINORCHARD_RIP  12.00000000000000000000000
							6449     HINORCHARD_SYN  12.00000000000000000000000
							6449        HINOTHER_DD  12.00000000000000000000000
							6449       HINOTHER_RIP  12.00000000000000000000000
							6449       HINOTHER_SYN  12.00000000000000000000000
							6449         HINPARK_DD  12.00000000000000000000000
							6449        HINPARK_RIP  12.00000000000000000000000
							6449        HINPARK_SYN  12.00000000000000000000000
							6449      HINPASTURE_DD  12.00000000000000000000000
							6449     HINPASTURE_RIP  12.00000000000000000000000
							6449     HINPASTURE_SYN  12.00000000000000000000000
							6449   HINPOWERLINES_DD  12.00000000000000000000000
							6449  HINPOWERLINES_RIP  12.00000000000000000000000
							6449  HINPOWERLINES_SYN  12.00000000000000000000000
							6449        HINROADS_DD  11.00000000000000000000000
							6449       HINROADS_RIP  12.00000000000000000000000
							6449       HINROADS_SYN  11.00000000000000000000000
							6449        HINWALLS_DD  12.00000000000000000000000
							6449       HINWALLS_RIP  12.00000000000000000000000
							6449       HINWALLS_SYN  12.00000000000000000000000
							6449          HIPWAG_DD   0.00000000000000000000000
							6449         HIPWAG_RIP   0.00000000000000000000000
							6449         HIPWAG_SYN   0.00000000000000000000000
							6449         HIPWALL_DD   0.27096774190000000315237
							6449        HIPWALL_RIP   0.31730769229999999936354
							6449        HIPWALL_SYN   0.33417204299999997418880
							6449   HIPWBUILDINGS_DD   0.41666666670000002126173
							6449  HIPWBUILDINGS_RIP   0.50000000000000000000000
							6449  HIPWBUILDINGS_SYN   0.49833333330000001426541
							6449  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6449 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6449 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6449       HIPWCROPS_DD   0.00000000000000000000000
							6449      HIPWCROPS_RIP   0.00000000000000000000000
							6449      HIPWCROPS_SYN   0.00000000000000000000000
							6449       HIPWDOCKS_DD   0.58333333330000003424942
							6449      HIPWDOCKS_RIP   0.45833333329999997873827
							6449      HIPWDOCKS_SYN   0.58333333330000003424942
							6449    HIPWLANDFILL_DD   0.66666666669999996575058
							6449   HIPWLANDFILL_RIP   0.58333333330000003424942
							6449   HIPWLANDFILL_SYN   0.66666666669999996575058
							6449        HIPWLAWN_DD   0.50000000000000000000000
							6449       HIPWLAWN_RIP   0.83333333330000003424942
							6449       HIPWLAWN_SYN   0.82638888889999995157609
							6449       HIPWNONAG_DD   0.35294117650000000896426
							6449      HIPWNONAG_RIP   0.41249999999999997779554
							6449      HIPWNONAG_SYN   0.43526610640000001906103
							6449     HIPWORCHARD_DD   0.00000000000000000000000
							6449    HIPWORCHARD_RIP   0.00000000000000000000000
							6449    HIPWORCHARD_SYN   0.00000000000000000000000
							6449       HIPWOTHER_DD   0.08333333330000000649385
							6449      HIPWOTHER_RIP   0.25000000000000000000000
							6449      HIPWOTHER_SYN   0.24611111110000000179454
							6449        HIPWPARK_DD   0.20833333330000000649385
							6449       HIPWPARK_RIP   0.16666666669999999350615
							6449       HIPWPARK_SYN   0.20833333330000000649385
							6449     HIPWPASTURE_DD   0.00000000000000000000000
							6449    HIPWPASTURE_RIP   0.00000000000000000000000
							6449    HIPWPASTURE_SYN   0.00000000000000000000000
							6449  HIPWPOWERLINES_DD   0.08333333330000000649385
							6449 HIPWPOWERLINES_RIP   0.08333333330000000649385
							6449 HIPWPOWERLINES_SYN   0.08333333330000000649385
							6449       HIPWROADS_DD   0.50000000000000000000000
							6449      HIPWROADS_RIP   0.50000000000000000000000
							6449      HIPWROADS_SYN   0.50000000000000000000000
							6449       HIPWWALLS_DD   0.50000000000000000000000
							6449      HIPWWALLS_RIP   0.75000000000000000000000
							6449      HIPWWALLS_SYN   0.74555555559999997505827
							6684         HIFPANY_DD   0.00000000000000000000000
							6684        HIFPANY_RIP   1.00000000000000000000000
							6684        HIFPANY_SYN   1.00000000000000000000000
							6684    HIFPANYCIRCA_DD   0.00000000000000000000000
							6684   HIFPANYCIRCA_RIP   1.00000000000000000000000
							6684   HIFPANYCIRCA_SYN   1.00000000000000000000000
							6684           HIIAG_DD   0.00000000000000000000000
							6684          HIIAG_RIP   1.00000000000000000000000
							6684          HIIAG_SYN   1.00000000000000000000000
							6684      HIIAGCIRCA_DD   0.00000000000000000000000
							6684     HIIAGCIRCA_RIP   1.00000000000000000000000
							6684     HIIAGCIRCA_SYN   1.00000000000000000000000
							6684          HIIALL_DD   0.00000000000000000000000
							6684         HIIALL_RIP   1.00000000000000000000000
							6684         HIIALL_SYN   1.00000000000000000000000
							6684     HIIALLCIRCA_DD   0.00000000000000000000000
							6684    HIIALLCIRCA_RIP   1.00000000000000000000000
							6684    HIIALLCIRCA_SYN   1.00000000000000000000000
							6684        HIINONAG_DD   0.00000000000000000000000
							6684       HIINONAG_RIP   0.00000000000000000000000
							6684       HIINONAG_SYN   0.00000000000000000000000
							6684   HIINONAGCIRCA_DD   0.00000000000000000000000
							6684  HIINONAGCIRCA_RIP   0.00000000000000000000000
							6684  HIINONAGCIRCA_SYN   0.00000000000000000000000
							6684           HINAG_DD  18.00000000000000000000000
							6684          HINAG_RIP  18.00000000000000000000000
							6684          HINAG_SYN  18.00000000000000000000000
							6684          HINALL_DD  78.00000000000000000000000
							6684         HINALL_RIP  72.00000000000000000000000
							6684         HINALL_SYN  72.00000000000000000000000
							6684    HINBUILDINGS_DD   6.00000000000000000000000
							6684   HINBUILDINGS_RIP   6.00000000000000000000000
							6684   HINBUILDINGS_SYN   6.00000000000000000000000
							6684   HINCOMMERCIAL_DD   6.00000000000000000000000
							6684  HINCOMMERCIAL_RIP   6.00000000000000000000000
							6684  HINCOMMERCIAL_SYN   6.00000000000000000000000
							6684        HINCROPS_DD   6.00000000000000000000000
							6684       HINCROPS_RIP   6.00000000000000000000000
							6684       HINCROPS_SYN   6.00000000000000000000000
							6684        HINDOCKS_DD   6.00000000000000000000000
							6684       HINDOCKS_RIP   6.00000000000000000000000
							6684       HINDOCKS_SYN   6.00000000000000000000000
							6684     HINLANDFILL_DD   6.00000000000000000000000
							6684    HINLANDFILL_RIP   6.00000000000000000000000
							6684    HINLANDFILL_SYN   6.00000000000000000000000
							6684         HINLAWN_DD   6.00000000000000000000000
							6684        HINLAWN_RIP   6.00000000000000000000000
							6684        HINLAWN_SYN   6.00000000000000000000000
							6684        HINNONAG_DD  60.00000000000000000000000
							6684       HINNONAG_RIP  54.00000000000000000000000
							6684       HINNONAG_SYN  54.00000000000000000000000
							6684      HINORCHARD_DD   6.00000000000000000000000
							6684     HINORCHARD_RIP   6.00000000000000000000000
							6684     HINORCHARD_SYN   6.00000000000000000000000
							6684        HINOTHER_DD   6.00000000000000000000000
							6684       HINOTHER_RIP   0.00000000000000000000000
							6684       HINOTHER_SYN   0.00000000000000000000000
							6684         HINPARK_DD   6.00000000000000000000000
							6684        HINPARK_RIP   6.00000000000000000000000
							6684        HINPARK_SYN   6.00000000000000000000000
							6684      HINPASTURE_DD   6.00000000000000000000000
							6684     HINPASTURE_RIP   6.00000000000000000000000
							6684     HINPASTURE_SYN   6.00000000000000000000000
							6684   HINPOWERLINES_DD   6.00000000000000000000000
							6684  HINPOWERLINES_RIP   6.00000000000000000000000
							6684  HINPOWERLINES_SYN   6.00000000000000000000000
							6684        HINROADS_DD   6.00000000000000000000000
							6684       HINROADS_RIP   6.00000000000000000000000
							6684       HINROADS_SYN   6.00000000000000000000000
							6684        HINWALLS_DD   6.00000000000000000000000
							6684       HINWALLS_RIP   6.00000000000000000000000
							6684       HINWALLS_SYN   6.00000000000000000000000
							6684          HIPWAG_DD   0.00000000000000000000000
							6684         HIPWAG_RIP   0.33333333329999997873827
							6684         HIPWAG_SYN   0.33333333329999997873827
							6684         HIPWALL_DD   0.00000000000000000000000
							6684        HIPWALL_RIP   0.08333333330000000649385
							6684        HIPWALL_SYN   0.08333333330000000649385
							6684   HIPWBUILDINGS_DD   0.00000000000000000000000
							6684  HIPWBUILDINGS_RIP   0.00000000000000000000000
							6684  HIPWBUILDINGS_SYN   0.00000000000000000000000
							6684  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							6684 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							6684 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							6684       HIPWCROPS_DD   0.00000000000000000000000
							6684      HIPWCROPS_RIP   0.00000000000000000000000
							6684      HIPWCROPS_SYN   0.00000000000000000000000
							6684       HIPWDOCKS_DD   0.00000000000000000000000
							6684      HIPWDOCKS_RIP   0.00000000000000000000000
							6684      HIPWDOCKS_SYN   0.00000000000000000000000
							6684    HIPWLANDFILL_DD   0.00000000000000000000000
							6684   HIPWLANDFILL_RIP   0.00000000000000000000000
							6684   HIPWLANDFILL_SYN   0.00000000000000000000000
							6684        HIPWLAWN_DD   0.00000000000000000000000
							6684       HIPWLAWN_RIP   0.00000000000000000000000
							6684       HIPWLAWN_SYN   0.00000000000000000000000
							6684       HIPWNONAG_DD   0.00000000000000000000000
							6684      HIPWNONAG_RIP   0.00000000000000000000000
							6684      HIPWNONAG_SYN   0.00000000000000000000000
							6684     HIPWORCHARD_DD   0.00000000000000000000000
							6684    HIPWORCHARD_RIP   0.00000000000000000000000
							6684    HIPWORCHARD_SYN   0.00000000000000000000000
							6684       HIPWOTHER_DD   0.00000000000000000000000
							6684      HIPWOTHER_RIP                          NA
							6684      HIPWOTHER_SYN                          NA
							6684        HIPWPARK_DD   0.00000000000000000000000
							6684       HIPWPARK_RIP   0.00000000000000000000000
							6684       HIPWPARK_SYN   0.00000000000000000000000
							6684     HIPWPASTURE_DD   0.00000000000000000000000
							6684    HIPWPASTURE_RIP   1.00000000000000000000000
							6684    HIPWPASTURE_SYN   1.00000000000000000000000
							6684  HIPWPOWERLINES_DD   0.00000000000000000000000
							6684 HIPWPOWERLINES_RIP   0.00000000000000000000000
							6684 HIPWPOWERLINES_SYN   0.00000000000000000000000
							6684       HIPWROADS_DD   0.00000000000000000000000
							6684      HIPWROADS_RIP   0.00000000000000000000000
							6684      HIPWROADS_SYN   0.00000000000000000000000
							6684       HIPWWALLS_DD   0.00000000000000000000000
							6684      HIPWWALLS_RIP   0.00000000000000000000000
							6684      HIPWWALLS_SYN   0.00000000000000000000000
							7504         HIFPANY_DD   0.00000000000000000000000
							7504        HIFPANY_RIP   1.00000000000000000000000
							7504        HIFPANY_SYN   1.00000000000000000000000
							7504    HIFPANYCIRCA_DD   0.00000000000000000000000
							7504   HIFPANYCIRCA_RIP   1.00000000000000000000000
							7504   HIFPANYCIRCA_SYN   1.00000000000000000000000
							7504           HIIAG_DD   0.00000000000000000000000
							7504          HIIAG_RIP   0.00000000000000000000000
							7504          HIIAG_SYN   0.00000000000000000000000
							7504      HIIAGCIRCA_DD   0.00000000000000000000000
							7504     HIIAGCIRCA_RIP   0.00000000000000000000000
							7504     HIIAGCIRCA_SYN   0.00000000000000000000000
							7504          HIIALL_DD   0.00000000000000000000000
							7504         HIIALL_RIP   4.82500000000000017763568
							7504         HIIALL_SYN   4.82500000000000017763568
							7504     HIIALLCIRCA_DD   0.00000000000000000000000
							7504    HIIALLCIRCA_RIP   3.81666666670000020999964
							7504    HIIALLCIRCA_SYN   3.81666666670000020999964
							7504        HIINONAG_DD   0.00000000000000000000000
							7504       HIINONAG_RIP   4.82500000000000017763568
							7504       HIINONAG_SYN   4.82500000000000017763568
							7504   HIINONAGCIRCA_DD   0.00000000000000000000000
							7504  HIINONAGCIRCA_RIP   3.81666666670000020999964
							7504  HIINONAGCIRCA_SYN   3.81666666670000020999964
							7504           HINAG_DD  30.00000000000000000000000
							7504          HINAG_RIP   3.00000000000000000000000
							7504          HINAG_SYN   3.00000000000000000000000
							7504          HINALL_DD 130.00000000000000000000000
							7504         HINALL_RIP  32.00000000000000000000000
							7504         HINALL_SYN  32.00000000000000000000000
							7504    HINBUILDINGS_DD  10.00000000000000000000000
							7504   HINBUILDINGS_RIP   3.00000000000000000000000
							7504   HINBUILDINGS_SYN   3.00000000000000000000000
							7504   HINCOMMERCIAL_DD  10.00000000000000000000000
							7504  HINCOMMERCIAL_RIP   2.00000000000000000000000
							7504  HINCOMMERCIAL_SYN   2.00000000000000000000000
							7504        HINCROPS_DD  10.00000000000000000000000
							7504       HINCROPS_RIP   1.00000000000000000000000
							7504       HINCROPS_SYN   1.00000000000000000000000
							7504        HINDOCKS_DD  10.00000000000000000000000
							7504       HINDOCKS_RIP   1.00000000000000000000000
							7504       HINDOCKS_SYN   1.00000000000000000000000
							7504     HINLANDFILL_DD  10.00000000000000000000000
							7504    HINLANDFILL_RIP   2.00000000000000000000000
							7504    HINLANDFILL_SYN   2.00000000000000000000000
							7504         HINLAWN_DD  10.00000000000000000000000
							7504        HINLAWN_RIP   4.00000000000000000000000
							7504        HINLAWN_SYN   4.00000000000000000000000
							7504        HINNONAG_DD 100.00000000000000000000000
							7504       HINNONAG_RIP  29.00000000000000000000000
							7504       HINNONAG_SYN  29.00000000000000000000000
							7504      HINORCHARD_DD  10.00000000000000000000000
							7504     HINORCHARD_RIP   1.00000000000000000000000
							7504     HINORCHARD_SYN   1.00000000000000000000000
							7504        HINOTHER_DD  10.00000000000000000000000
							7504       HINOTHER_RIP   2.00000000000000000000000
							7504       HINOTHER_SYN   2.00000000000000000000000
							7504         HINPARK_DD  10.00000000000000000000000
							7504        HINPARK_RIP   1.00000000000000000000000
							7504        HINPARK_SYN   1.00000000000000000000000
							7504      HINPASTURE_DD  10.00000000000000000000000
							7504     HINPASTURE_RIP   1.00000000000000000000000
							7504     HINPASTURE_SYN   1.00000000000000000000000
							7504   HINPOWERLINES_DD  10.00000000000000000000000
							7504  HINPOWERLINES_RIP   3.00000000000000000000000
							7504  HINPOWERLINES_SYN   3.00000000000000000000000
							7504        HINROADS_DD  10.00000000000000000000000
							7504       HINROADS_RIP  10.00000000000000000000000
							7504       HINROADS_SYN  10.00000000000000000000000
							7504        HINWALLS_DD  10.00000000000000000000000
							7504       HINWALLS_RIP   1.00000000000000000000000
							7504       HINWALLS_SYN   1.00000000000000000000000
							7504          HIPWAG_DD   0.00000000000000000000000
							7504         HIPWAG_RIP   0.00000000000000000000000
							7504         HIPWAG_SYN   0.00000000000000000000000
							7504         HIPWALL_DD   0.00000000000000000000000
							7504        HIPWALL_RIP   0.59375000000000000000000
							7504        HIPWALL_SYN   0.59375000000000000000000
							7504   HIPWBUILDINGS_DD   0.00000000000000000000000
							7504  HIPWBUILDINGS_RIP   0.33333333329999997873827
							7504  HIPWBUILDINGS_SYN   0.33333333329999997873827
							7504  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							7504 HIPWCOMMERCIAL_RIP   0.25000000000000000000000
							7504 HIPWCOMMERCIAL_SYN   0.25000000000000000000000
							7504       HIPWCROPS_DD   0.00000000000000000000000
							7504      HIPWCROPS_RIP   0.00000000000000000000000
							7504      HIPWCROPS_SYN   0.00000000000000000000000
							7504       HIPWDOCKS_DD   0.00000000000000000000000
							7504      HIPWDOCKS_RIP   1.00000000000000000000000
							7504      HIPWDOCKS_SYN   1.00000000000000000000000
							7504    HIPWLANDFILL_DD   0.00000000000000000000000
							7504   HIPWLANDFILL_RIP   0.50000000000000000000000
							7504   HIPWLANDFILL_SYN   0.50000000000000000000000
							7504        HIPWLAWN_DD   0.00000000000000000000000
							7504       HIPWLAWN_RIP   0.87500000000000000000000
							7504       HIPWLAWN_SYN   0.87500000000000000000000
							7504       HIPWNONAG_DD   0.00000000000000000000000
							7504      HIPWNONAG_RIP   0.65517241380000001971240
							7504      HIPWNONAG_SYN   0.65517241380000001971240
							7504     HIPWORCHARD_DD   0.00000000000000000000000
							7504    HIPWORCHARD_RIP   0.00000000000000000000000
							7504    HIPWORCHARD_SYN   0.00000000000000000000000
							7504       HIPWOTHER_DD   0.00000000000000000000000
							7504      HIPWOTHER_RIP   0.25000000000000000000000
							7504      HIPWOTHER_SYN   0.25000000000000000000000
							7504        HIPWPARK_DD   0.00000000000000000000000
							7504       HIPWPARK_RIP   0.00000000000000000000000
							7504       HIPWPARK_SYN   0.00000000000000000000000
							7504     HIPWPASTURE_DD   0.00000000000000000000000
							7504    HIPWPASTURE_RIP   0.00000000000000000000000
							7504    HIPWPASTURE_SYN   0.00000000000000000000000
							7504  HIPWPOWERLINES_DD   0.00000000000000000000000
							7504 HIPWPOWERLINES_RIP   0.66666666669999996575058
							7504 HIPWPOWERLINES_SYN   0.66666666669999996575058
							7504       HIPWROADS_DD   0.00000000000000000000000
							7504      HIPWROADS_RIP   0.94999999999999995559108
							7504      HIPWROADS_SYN   0.94999999999999995559108
							7504       HIPWWALLS_DD   0.00000000000000000000000
							7504      HIPWWALLS_RIP   0.00000000000000000000000
							7504      HIPWWALLS_SYN   0.00000000000000000000000
							7740         HIFPANY_DD   0.40000000000000002220446
							7740        HIFPANY_RIP   0.50000000000000000000000
							7740        HIFPANY_SYN   0.40000000000000002220446
							7740    HIFPANYCIRCA_DD   0.29999999999999998889777
							7740   HIFPANYCIRCA_RIP   0.29999999999999998889777
							7740   HIFPANYCIRCA_SYN   0.29999999999999998889777
							7740           HIIAG_DD   0.00000000000000000000000
							7740          HIIAG_RIP   0.00000000000000000000000
							7740          HIIAG_SYN   0.00000000000000000000000
							7740      HIIAGCIRCA_DD   0.00000000000000000000000
							7740     HIIAGCIRCA_RIP   0.00000000000000000000000
							7740     HIIAGCIRCA_SYN   0.00000000000000000000000
							7740          HIIALL_DD   0.65555555560000000614451
							7740         HIIALL_RIP   0.80000000000000004440892
							7740         HIIALL_SYN   0.65555555560000000614451
							7740     HIIALLCIRCA_DD   0.29999999999999998889777
							7740    HIIALLCIRCA_RIP   0.40000000000000002220446
							7740    HIIALLCIRCA_SYN   0.29999999999999998889777
							7740        HIINONAG_DD   0.65555555560000000614451
							7740       HIINONAG_RIP   0.80000000000000004440892
							7740       HIINONAG_SYN   0.65555555560000000614451
							7740   HIINONAGCIRCA_DD   0.29999999999999998889777
							7740  HIINONAGCIRCA_RIP   0.40000000000000002220446
							7740  HIINONAGCIRCA_SYN   0.29999999999999998889777
							7740           HINAG_DD  30.00000000000000000000000
							7740          HINAG_RIP  30.00000000000000000000000
							7740          HINAG_SYN  30.00000000000000000000000
							7740          HINALL_DD 119.00000000000000000000000
							7740         HINALL_RIP 120.00000000000000000000000
							7740         HINALL_SYN 119.00000000000000000000000
							7740    HINBUILDINGS_DD  10.00000000000000000000000
							7740   HINBUILDINGS_RIP  10.00000000000000000000000
							7740   HINBUILDINGS_SYN  10.00000000000000000000000
							7740   HINCOMMERCIAL_DD  10.00000000000000000000000
							7740  HINCOMMERCIAL_RIP  10.00000000000000000000000
							7740  HINCOMMERCIAL_SYN  10.00000000000000000000000
							7740        HINCROPS_DD  10.00000000000000000000000
							7740       HINCROPS_RIP  10.00000000000000000000000
							7740       HINCROPS_SYN  10.00000000000000000000000
							7740        HINDOCKS_DD  10.00000000000000000000000
							7740       HINDOCKS_RIP  10.00000000000000000000000
							7740       HINDOCKS_SYN  10.00000000000000000000000
							7740     HINLANDFILL_DD  10.00000000000000000000000
							7740    HINLANDFILL_RIP  10.00000000000000000000000
							7740    HINLANDFILL_SYN  10.00000000000000000000000
							7740         HINLAWN_DD  10.00000000000000000000000
							7740        HINLAWN_RIP  10.00000000000000000000000
							7740        HINLAWN_SYN  10.00000000000000000000000
							7740        HINNONAG_DD  89.00000000000000000000000
							7740       HINNONAG_RIP  90.00000000000000000000000
							7740       HINNONAG_SYN  89.00000000000000000000000
							7740      HINORCHARD_DD  10.00000000000000000000000
							7740     HINORCHARD_RIP  10.00000000000000000000000
							7740     HINORCHARD_SYN  10.00000000000000000000000
							7740        HINOTHER_DD   0.00000000000000000000000
							7740       HINOTHER_RIP   0.00000000000000000000000
							7740       HINOTHER_SYN   0.00000000000000000000000
							7740         HINPARK_DD   9.00000000000000000000000
							7740        HINPARK_RIP  10.00000000000000000000000
							7740        HINPARK_SYN   9.00000000000000000000000
							7740      HINPASTURE_DD  10.00000000000000000000000
							7740     HINPASTURE_RIP  10.00000000000000000000000
							7740     HINPASTURE_SYN  10.00000000000000000000000
							7740   HINPOWERLINES_DD  10.00000000000000000000000
							7740  HINPOWERLINES_RIP  10.00000000000000000000000
							7740  HINPOWERLINES_SYN  10.00000000000000000000000
							7740        HINROADS_DD  10.00000000000000000000000
							7740       HINROADS_RIP  10.00000000000000000000000
							7740       HINROADS_SYN  10.00000000000000000000000
							7740        HINWALLS_DD  10.00000000000000000000000
							7740       HINWALLS_RIP  10.00000000000000000000000
							7740       HINWALLS_SYN  10.00000000000000000000000
							7740          HIPWAG_DD   0.00000000000000000000000
							7740         HIPWAG_RIP   0.00000000000000000000000
							7740         HIPWAG_SYN   0.00000000000000000000000
							7740         HIPWALL_DD   0.05462184870000000302959
							7740        HIPWALL_RIP   0.06666666670000000183283
							7740        HIPWALL_SYN   0.05462184870000000302959
							7740   HIPWBUILDINGS_DD   0.00000000000000000000000
							7740  HIPWBUILDINGS_RIP   0.00000000000000000000000
							7740  HIPWBUILDINGS_SYN   0.00000000000000000000000
							7740  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							7740 HIPWCOMMERCIAL_RIP   0.00000000000000000000000
							7740 HIPWCOMMERCIAL_SYN   0.00000000000000000000000
							7740       HIPWCROPS_DD   0.00000000000000000000000
							7740      HIPWCROPS_RIP   0.00000000000000000000000
							7740      HIPWCROPS_SYN   0.00000000000000000000000
							7740       HIPWDOCKS_DD   0.14999999999999999444888
							7740      HIPWDOCKS_RIP   0.14999999999999999444888
							7740      HIPWDOCKS_SYN   0.14999999999999999444888
							7740    HIPWLANDFILL_DD   0.10000000000000000555112
							7740   HIPWLANDFILL_RIP   0.00000000000000000000000
							7740   HIPWLANDFILL_SYN   0.10000000000000000555112
							7740        HIPWLAWN_DD   0.00000000000000000000000
							7740       HIPWLAWN_RIP   0.00000000000000000000000
							7740       HIPWLAWN_SYN   0.00000000000000000000000
							7740       HIPWNONAG_DD   0.07303370789999999446174
							7740      HIPWNONAG_RIP   0.08888888890000000431169
							7740      HIPWNONAG_SYN   0.07303370789999999446174
							7740     HIPWORCHARD_DD   0.00000000000000000000000
							7740    HIPWORCHARD_RIP   0.00000000000000000000000
							7740    HIPWORCHARD_SYN   0.00000000000000000000000
							7740       HIPWOTHER_DD                          NA
							7740      HIPWOTHER_RIP                          NA
							7740      HIPWOTHER_SYN                          NA
							7740        HIPWPARK_DD   0.05555555560000000059340
							7740       HIPWPARK_RIP   0.10000000000000000555112
							7740       HIPWPARK_SYN   0.05555555560000000059340
							7740     HIPWPASTURE_DD   0.00000000000000000000000
							7740    HIPWPASTURE_RIP   0.00000000000000000000000
							7740    HIPWPASTURE_SYN   0.00000000000000000000000
							7740  HIPWPOWERLINES_DD   0.05000000000000000277556
							7740 HIPWPOWERLINES_RIP   0.10000000000000000555112
							7740 HIPWPOWERLINES_SYN   0.05000000000000000277556
							7740       HIPWROADS_DD   0.20000000000000001110223
							7740      HIPWROADS_RIP   0.34999999999999997779554
							7740      HIPWROADS_SYN   0.20000000000000001110223
							7740       HIPWWALLS_DD   0.10000000000000000555112
							7740      HIPWWALLS_RIP   0.10000000000000000555112
							7740      HIPWWALLS_SYN   0.10000000000000000555112
							7808         HIFPANY_DD   1.00000000000000000000000
							7808        HIFPANY_RIP   0.87500000000000000000000
							7808        HIFPANY_SYN   1.00000000000000000000000
							7808    HIFPANYCIRCA_DD   0.00000000000000000000000
							7808   HIFPANYCIRCA_RIP   0.00000000000000000000000
							7808   HIFPANYCIRCA_SYN   0.00000000000000000000000
							7808           HIIAG_DD   1.00000000000000000000000
							7808          HIIAG_RIP   1.00000000000000000000000
							7808          HIIAG_SYN   1.00000000000000000000000
							7808      HIIAGCIRCA_DD   0.00000000000000000000000
							7808     HIIAGCIRCA_RIP   0.00000000000000000000000
							7808     HIIAGCIRCA_SYN   0.00000000000000000000000
							7808          HIIALL_DD   3.00000000000000000000000
							7808         HIIALL_RIP   2.33333333330000014527172
							7808         HIIALL_SYN   2.50000000000000000000000
							7808     HIIALLCIRCA_DD   0.00000000000000000000000
							7808    HIIALLCIRCA_RIP   0.00000000000000000000000
							7808    HIIALLCIRCA_SYN   0.00000000000000000000000
							7808        HIINONAG_DD   2.00000000000000000000000
							7808       HIINONAG_RIP   1.33333333329999992322712
							7808       HIINONAG_SYN   1.50000000000000000000000
							7808   HIINONAGCIRCA_DD   0.00000000000000000000000
							7808  HIINONAGCIRCA_RIP   0.00000000000000000000000
							7808  HIINONAGCIRCA_SYN   0.00000000000000000000000
							7808           HINAG_DD   9.00000000000000000000000
							7808          HINAG_RIP   8.00000000000000000000000
							7808          HINAG_SYN   8.00000000000000000000000
							7808          HINALL_DD  15.00000000000000000000000
							7808         HINALL_RIP  13.00000000000000000000000
							7808         HINALL_SYN  13.00000000000000000000000
							7808    HINBUILDINGS_DD   3.00000000000000000000000
							7808   HINBUILDINGS_RIP   3.00000000000000000000000
							7808   HINBUILDINGS_SYN   3.00000000000000000000000
							7808   HINCOMMERCIAL_DD   0.00000000000000000000000
							7808  HINCOMMERCIAL_RIP   0.00000000000000000000000
							7808  HINCOMMERCIAL_SYN   0.00000000000000000000000
							7808        HINCROPS_DD   7.00000000000000000000000
							7808       HINCROPS_RIP   6.00000000000000000000000
							7808       HINCROPS_SYN   6.00000000000000000000000
							7808        HINDOCKS_DD   0.00000000000000000000000
							7808       HINDOCKS_RIP   0.00000000000000000000000
							7808       HINDOCKS_SYN   0.00000000000000000000000
							7808     HINLANDFILL_DD   1.00000000000000000000000
							7808    HINLANDFILL_RIP   0.00000000000000000000000
							7808    HINLANDFILL_SYN   0.00000000000000000000000
							7808         HINLAWN_DD   0.00000000000000000000000
							7808        HINLAWN_RIP   0.00000000000000000000000
							7808        HINLAWN_SYN   0.00000000000000000000000
							7808        HINNONAG_DD   6.00000000000000000000000
							7808       HINNONAG_RIP   5.00000000000000000000000
							7808       HINNONAG_SYN   5.00000000000000000000000
							7808      HINORCHARD_DD   0.00000000000000000000000
							7808     HINORCHARD_RIP   0.00000000000000000000000
							7808     HINORCHARD_SYN   0.00000000000000000000000
							7808        HINOTHER_DD   0.00000000000000000000000
							7808       HINOTHER_RIP   0.00000000000000000000000
							7808       HINOTHER_SYN   0.00000000000000000000000
							7808         HINPARK_DD   0.00000000000000000000000
							7808        HINPARK_RIP   0.00000000000000000000000
							7808        HINPARK_SYN   0.00000000000000000000000
							7808      HINPASTURE_DD   2.00000000000000000000000
							7808     HINPASTURE_RIP   2.00000000000000000000000
							7808     HINPASTURE_SYN   2.00000000000000000000000
							7808   HINPOWERLINES_DD   1.00000000000000000000000
							7808  HINPOWERLINES_RIP   1.00000000000000000000000
							7808  HINPOWERLINES_SYN   1.00000000000000000000000
							7808        HINROADS_DD   1.00000000000000000000000
							7808       HINROADS_RIP   1.00000000000000000000000
							7808       HINROADS_SYN   1.00000000000000000000000
							7808        HINWALLS_DD   0.00000000000000000000000
							7808       HINWALLS_RIP   0.00000000000000000000000
							7808       HINWALLS_SYN   0.00000000000000000000000
							7808          HIPWAG_DD   0.50000000000000000000000
							7808         HIPWAG_RIP   0.50000000000000000000000
							7808         HIPWAG_SYN   0.50000000000000000000000
							7808         HIPWALL_DD   0.50000000000000000000000
							7808        HIPWALL_RIP   0.46153846149999999681768
							7808        HIPWALL_SYN   0.50000000000000000000000
							7808   HIPWBUILDINGS_DD   0.50000000000000000000000
							7808  HIPWBUILDINGS_RIP   0.33333333329999997873827
							7808  HIPWBUILDINGS_SYN   0.50000000000000000000000
							7808  HIPWCOMMERCIAL_DD                          NA
							7808 HIPWCOMMERCIAL_RIP                          NA
							7808 HIPWCOMMERCIAL_SYN                          NA
							7808       HIPWCROPS_DD   0.50000000000000000000000
							7808      HIPWCROPS_RIP   0.50000000000000000000000
							7808      HIPWCROPS_SYN   0.50000000000000000000000
							7808       HIPWDOCKS_DD                          NA
							7808      HIPWDOCKS_RIP                          NA
							7808      HIPWDOCKS_SYN                          NA
							7808    HIPWLANDFILL_DD   0.50000000000000000000000
							7808   HIPWLANDFILL_RIP                          NA
							7808   HIPWLANDFILL_SYN                          NA
							7808        HIPWLAWN_DD                          NA
							7808       HIPWLAWN_RIP                          NA
							7808       HIPWLAWN_SYN                          NA
							7808       HIPWNONAG_DD   0.50000000000000000000000
							7808      HIPWNONAG_RIP   0.40000000000000002220446
							7808      HIPWNONAG_SYN   0.50000000000000000000000
							7808     HIPWORCHARD_DD                          NA
							7808    HIPWORCHARD_RIP                          NA
							7808    HIPWORCHARD_SYN                          NA
							7808       HIPWOTHER_DD                          NA
							7808      HIPWOTHER_RIP                          NA
							7808      HIPWOTHER_SYN                          NA
							7808        HIPWPARK_DD                          NA
							7808       HIPWPARK_RIP                          NA
							7808       HIPWPARK_SYN                          NA
							7808     HIPWPASTURE_DD   0.50000000000000000000000
							7808    HIPWPASTURE_RIP   0.50000000000000000000000
							7808    HIPWPASTURE_SYN   0.50000000000000000000000
							7808  HIPWPOWERLINES_DD   0.50000000000000000000000
							7808 HIPWPOWERLINES_RIP   0.50000000000000000000000
							7808 HIPWPOWERLINES_SYN   0.50000000000000000000000
							7808       HIPWROADS_DD   0.50000000000000000000000
							7808      HIPWROADS_RIP   0.50000000000000000000000
							7808      HIPWROADS_SYN   0.50000000000000000000000
							7808       HIPWWALLS_DD                          NA
							7808      HIPWWALLS_RIP                          NA
							7808      HIPWWALLS_SYN                          NA
							1000100         HIFPANY_DD   0.00000000000000000000000
							1000100        HIFPANY_RIP   1.00000000000000000000000
							1000100        HIFPANY_SYN   1.00000000000000000000000
							1000100    HIFPANYCIRCA_DD   0.00000000000000000000000
							1000100   HIFPANYCIRCA_RIP   1.00000000000000000000000
							1000100   HIFPANYCIRCA_SYN   1.00000000000000000000000
							1000100           HIIAG_DD   0.00000000000000000000000
							1000100          HIIAG_RIP                          NA
							1000100          HIIAG_SYN                          NA
							1000100      HIIAGCIRCA_DD   0.00000000000000000000000
							1000100     HIIAGCIRCA_RIP                          NA
							1000100     HIIAGCIRCA_SYN                          NA
							1000100          HIIALL_DD   0.00000000000000000000000
							1000100         HIIALL_RIP   1.00000000000000000000000
							1000100         HIIALL_SYN   1.00000000000000000000000
							1000100     HIIALLCIRCA_DD   0.00000000000000000000000
							1000100    HIIALLCIRCA_RIP   1.00000000000000000000000
							1000100    HIIALLCIRCA_SYN   1.00000000000000000000000
							1000100        HIINONAG_DD   0.00000000000000000000000
							1000100       HIINONAG_RIP   1.00000000000000000000000
							1000100       HIINONAG_SYN   1.00000000000000000000000
							1000100   HIINONAGCIRCA_DD   0.00000000000000000000000
							1000100  HIINONAGCIRCA_RIP   1.00000000000000000000000
							1000100  HIINONAGCIRCA_SYN   1.00000000000000000000000
							1000100           HINAG_DD  27.00000000000000000000000
							1000100          HINAG_RIP   0.00000000000000000000000
							1000100          HINAG_SYN   0.00000000000000000000000
							1000100          HINALL_DD 117.00000000000000000000000
							1000100         HINALL_RIP   1.00000000000000000000000
							1000100         HINALL_SYN   1.00000000000000000000000
							1000100    HINBUILDINGS_DD   9.00000000000000000000000
							1000100   HINBUILDINGS_RIP   0.00000000000000000000000
							1000100   HINBUILDINGS_SYN   0.00000000000000000000000
							1000100   HINCOMMERCIAL_DD   9.00000000000000000000000
							1000100  HINCOMMERCIAL_RIP   0.00000000000000000000000
							1000100  HINCOMMERCIAL_SYN   0.00000000000000000000000
							1000100        HINCROPS_DD   9.00000000000000000000000
							1000100       HINCROPS_RIP   0.00000000000000000000000
							1000100       HINCROPS_SYN   0.00000000000000000000000
							1000100        HINDOCKS_DD   9.00000000000000000000000
							1000100       HINDOCKS_RIP   0.00000000000000000000000
							1000100       HINDOCKS_SYN   0.00000000000000000000000
							1000100     HINLANDFILL_DD   9.00000000000000000000000
							1000100    HINLANDFILL_RIP   1.00000000000000000000000
							1000100    HINLANDFILL_SYN   1.00000000000000000000000
							1000100         HINLAWN_DD   9.00000000000000000000000
							1000100        HINLAWN_RIP   0.00000000000000000000000
							1000100        HINLAWN_SYN   0.00000000000000000000000
							1000100        HINNONAG_DD  90.00000000000000000000000
							1000100       HINNONAG_RIP   1.00000000000000000000000
							1000100       HINNONAG_SYN   1.00000000000000000000000
							1000100      HINORCHARD_DD   9.00000000000000000000000
							1000100     HINORCHARD_RIP   0.00000000000000000000000
							1000100     HINORCHARD_SYN   0.00000000000000000000000
							1000100        HINOTHER_DD   9.00000000000000000000000
							1000100       HINOTHER_RIP   0.00000000000000000000000
							1000100       HINOTHER_SYN   0.00000000000000000000000
							1000100         HINPARK_DD   9.00000000000000000000000
							1000100        HINPARK_RIP   0.00000000000000000000000
							1000100        HINPARK_SYN   0.00000000000000000000000
							1000100      HINPASTURE_DD   9.00000000000000000000000
							1000100     HINPASTURE_RIP   0.00000000000000000000000
							1000100     HINPASTURE_SYN   0.00000000000000000000000
							1000100   HINPOWERLINES_DD   9.00000000000000000000000
							1000100  HINPOWERLINES_RIP   0.00000000000000000000000
							1000100  HINPOWERLINES_SYN   0.00000000000000000000000
							1000100        HINROADS_DD   9.00000000000000000000000
							1000100       HINROADS_RIP   0.00000000000000000000000
							1000100       HINROADS_SYN   0.00000000000000000000000
							1000100        HINWALLS_DD   9.00000000000000000000000
							1000100       HINWALLS_RIP   0.00000000000000000000000
							1000100       HINWALLS_SYN   0.00000000000000000000000
							1000100          HIPWAG_DD   0.00000000000000000000000
							1000100         HIPWAG_RIP                          NA
							1000100         HIPWAG_SYN                          NA
							1000100         HIPWALL_DD   0.00000000000000000000000
							1000100        HIPWALL_RIP   1.00000000000000000000000
							1000100        HIPWALL_SYN   1.00000000000000000000000
							1000100   HIPWBUILDINGS_DD   0.00000000000000000000000
							1000100  HIPWBUILDINGS_RIP                          NA
							1000100  HIPWBUILDINGS_SYN                          NA
							1000100  HIPWCOMMERCIAL_DD   0.00000000000000000000000
							1000100 HIPWCOMMERCIAL_RIP                          NA
							1000100 HIPWCOMMERCIAL_SYN                          NA
							1000100       HIPWCROPS_DD   0.00000000000000000000000
							1000100      HIPWCROPS_RIP                          NA
							1000100      HIPWCROPS_SYN                          NA
							1000100       HIPWDOCKS_DD   0.00000000000000000000000
							1000100      HIPWDOCKS_RIP                          NA
							1000100      HIPWDOCKS_SYN                          NA
							1000100    HIPWLANDFILL_DD   0.00000000000000000000000
							1000100   HIPWLANDFILL_RIP   1.00000000000000000000000
							1000100   HIPWLANDFILL_SYN   1.00000000000000000000000
							1000100        HIPWLAWN_DD   0.00000000000000000000000
							1000100       HIPWLAWN_RIP                          NA
							1000100       HIPWLAWN_SYN                          NA
							1000100       HIPWNONAG_DD   0.00000000000000000000000
							1000100      HIPWNONAG_RIP   1.00000000000000000000000
							1000100      HIPWNONAG_SYN   1.00000000000000000000000
							1000100     HIPWORCHARD_DD   0.00000000000000000000000
							1000100    HIPWORCHARD_RIP                          NA
							1000100    HIPWORCHARD_SYN                          NA
							1000100       HIPWOTHER_DD   0.00000000000000000000000
							1000100      HIPWOTHER_RIP                          NA
							1000100      HIPWOTHER_SYN                          NA
							1000100        HIPWPARK_DD   0.00000000000000000000000
							1000100       HIPWPARK_RIP                          NA
							1000100       HIPWPARK_SYN                          NA
							1000100     HIPWPASTURE_DD   0.00000000000000000000000
							1000100    HIPWPASTURE_RIP                          NA
							1000100    HIPWPASTURE_SYN                          NA
							1000100  HIPWPOWERLINES_DD   0.00000000000000000000000
							1000100 HIPWPOWERLINES_RIP                          NA
							1000100 HIPWPOWERLINES_SYN                          NA
							1000100       HIPWROADS_DD   0.00000000000000000000000
							1000100      HIPWROADS_RIP                          NA
							1000100      HIPWROADS_SYN                          NA
							1000100       HIPWWALLS_DD   0.00000000000000000000000
							1000100      HIPWWALLS_RIP                          NA
							1000100      HIPWWALLS_SYN                          NA"
						)					
	rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
	rm(tc)
	
	return(rc)
	
}

# end of file