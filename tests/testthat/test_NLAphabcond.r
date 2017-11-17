library(testthat)
library(aquamet)

context("Test NLA physical habitat indicator condition assignments")


test_that("NLA riparian disturbance values correct",
          {
            testOut <- nlaRipDistIndicator(x=nlaPhabIndic_test,sampID=c('UID'),hiiAg='HIIAG_SYN'
                                                      ,hiiNonAg='HIINONAG_SYN',hifpAnyCirca='HIFPANYCIRCA_SYN')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nlaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==20)
            
            compCond <- filter(compOut,variable=='RDIS_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- filter(compOut,variable=='RDis_IX') %>% 
              plyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })

test_that("NLA drawdown values correct",
          {
            testOut <- nlaDrawdownIndicator(x=nlaPhabIndic_test,sampID=c('UID'),bfxVertDD = 'BFXVERTHEIGHT_DD'
                                            ,bfxHorizDD = 'BFXHORIZDIST_DD',ecoreg='AGGR_ECO9_2015',lake_origin='LAKE_ORIGIN')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nlaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==10)
            expect_equal(compOut$value.x,compOut$value.y)
          })

test_that("NLA riparian vegetation complexity values correct",
          {
            testOut.rip <- nlaRipVegCompIndicator(x=nlaPhabIndic_test,sampID=c('UID'),lat='INDEX_LAT_DD',lon='INDEX_LON_DD'
                                                      ,lake_origin='LAKE_ORIGIN',area='AREA_KM2',elev='ELEVATION'
                                                      ,ecoreg='AGGR_ECO9_2015',rviWoody='RVIWOODY_SYN'
                                                      ,rvfcGndInundated='RVFCGNDINUNDATED_SYN',rvfcUndWoody='RVFCUNDWOODY_SYN'
                                                      ,rvfcGndWoody='RVFCGNDWOODY_SYN',rvfpCanBig='RVFPCANBIG_SYN'
                                                      ,ssfcBedrock='SSFCBEDROCK',ssfcBoulders='SSFCBOULDERS'
                                                      ,hipwWalls='HIPWWALLS_SYN'
            )
            testOut.long <- reshape2::melt(testOut.rip,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nlaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==20)
            
            compCond <- filter(compOut,variable=='RVEG_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- filter(compOut,variable=='RVegQc3OE') %>% 
              plyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })

test_that("NLA littoral vegetation complexity values correct",
          {
            testOut.lit <- nlaLitVegCompIndicator(x=nlaPhabIndic_test,sampID=c('UID'),lat='INDEX_LAT_DD',lon='INDEX_LON_DD'
                                              ,lake_origin='LAKE_ORIGIN',area='AREA_KM2',elev='ELEVATION'
                                              ,ecoreg='AGGR_ECO9_2015',fciNatural='FCINATURAL_LIT'
                                              ,fcfcSnag='FCFCSNAGS_LIT',amfcFloating='AMFCFLOATING',amfcEmergent='AMFCEMERGENT'
                                              ,fcfcBoulders='FCFCBOULDERS_LIT',fcfcBrush='FCFCBRUSH_LIT',fcfcLedges='FCFCLEDGES_LIT'
                                              ,fcfcLiveTrees='FCFCLIVETREES_LIT',fcfcOverhang='FCFCOVERHANG_LIT')
            
            testOut.long <- reshape2::melt(testOut.lit,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nlaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==20)
            
            compCond <- filter(compOut,variable=='LITCVR_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- filter(compOut,variable=='LitCvrQc3OE') %>% 
              plyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })

# This one requires mixing the outputs from the two functions above 
test_that("NLA littoral and riparian vegetation complexity values correct",
          {
            testOut.rip <- nlaRipVegCompIndicator(x=nlaPhabIndic_test,sampID=c('UID'),lat='INDEX_LAT_DD',lon='INDEX_LON_DD'
                                                  ,lake_origin='LAKE_ORIGIN',area='AREA_KM2',elev='ELEVATION'
                                                  ,ecoreg='AGGR_ECO9_2015',rviWoody='RVIWOODY_SYN'
                                                  ,rvfcGndInundated='RVFCGNDINUNDATED_SYN',rvfcUndWoody='RVFCUNDWOODY_SYN'
                                                  ,rvfcGndWoody='RVFCGNDWOODY_SYN',rvfpCanBig='RVFPCANBIG_SYN'
                                                  ,ssfcBedrock='SSFCBEDROCK',ssfcBoulders='SSFCBOULDERS'
                                                  ,hipwWalls='HIPWWALLS_SYN') %>%
              dplyr::select(UID,RVegQ)

            testOut.lit <- nlaLitVegCompIndicator(x=nlaPhabIndic_test,sampID=c('UID'),lat='INDEX_LAT_DD',lon='INDEX_LON_DD'
                                                  ,lake_origin='LAKE_ORIGIN',area='AREA_KM2',elev='ELEVATION'
                                                  ,ecoreg='AGGR_ECO9_2015',fciNatural='FCINATURAL_LIT'
                                                  ,fcfcSnag='FCFCSNAGS_LIT',amfcFloating='AMFCFLOATING',amfcEmergent='AMFCEMERGENT'
                                                  ,fcfcBoulders='FCFCBOULDERS_LIT',fcfcBrush='FCFCBRUSH_LIT',fcfcLedges='FCFCLEDGES_LIT'
                                                  ,fcfcLiveTrees='FCFCLIVETREES_LIT',fcfcOverhang='FCFCOVERHANG_LIT') %>%
              dplyr::select(UID,LitCvrQ)
            nlaPhabIndic_test.riplit <- merge(nlaPhabIndic_test,testOut.rip,by='UID') %>%
              merge(testOut.lit,by='UID')
            
            testOut <- nlaLitRipVegCompIndicator(x=nlaPhabIndic_test.riplit,sampID=c('UID'),lat='INDEX_LAT_DD',lon='INDEX_LON_DD'
                                                 ,lake_origin='LAKE_ORIGIN',area='AREA_KM2',elev='ELEVATION'
                                                 ,ecoreg='AGGR_ECO9_2015',rvegq='RVegQ',litcvrq='LitCvrQ')
            
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nlaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==20)
            
            compCond <- filter(compOut,variable=='LITRIPCVR_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- filter(compOut,variable=='LitRipCvrQc3OE') %>% 
              plyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })