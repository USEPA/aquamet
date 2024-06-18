library(testthat)
library(aquamet)

context("Test NLA physical habitat indicator condition assignments")


test_that("NLA riparian disturbance values correct",
          {
            testOut <- nlaRipDistIndicator(x=nlaPhabIndic_test,sampID=c('UID'),hiiAg='HIIAG_SYN'
                                                      ,hiiNonAg='HIINONAG_SYN',hifpAnyCirca='HIFPANYCIRCA_SYN')
            testOut.long <- mutate(testOut, RDis_IX = as.character(RDis_IX)) %>%
              pivot_longer(cols = c('RDis_IX', 'RDIS_COND')) 
            testCond.long <- mutate(nlaPhabIndicCond_test, RDis_IX = as.character(RDis_IX)) %>%
              pivot_longer(cols = c('RDis_IX', 'RDIS_COND'))
            compOut <- merge(testCond.long,testOut.long,by=c('UID','name'))
            expect_true(nrow(compOut)==20)
            
            compCond <- subset(compOut,name=='RDIS_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- subset(compOut,name=='RDis_IX') %>% 
              dplyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })

test_that("NLA drawdown values correct",
          {
            testOut <- nlaDrawdownIndicator(x=nlaPhabIndic_test,sampID=c('UID'),
                                            bfxVertDD = 'BFXVERTHEIGHT_DD',
                                            bfxHorizDD = 'BFXHORIZDIST_DD',
                                            ecoreg='AGGR_ECO9_2015',
                                            lake_origin='LAKE_ORIGIN')
            testOut.long <- pivot_longer(testOut, 
                                         cols = c('horizDD_cond', 
                                                  'vertDD_cond', 
                                                  'DRAWDOWN_COND')) 
            testCond.long <- pivot_longer(nlaPhabIndicCond_test, 
                                          cols = c('DRAWDOWN_COND')) 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','name'))
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
            testOut.long <- mutate(testOut.rip,
                                   RVegQc3OE = as.character(RVegQc3OE)) %>%
                            pivot_longer(cols = c("RVegQc3OE", "RVEG_COND"))
            testCond.long <- mutate(nlaPhabIndicCond_test,
                                    RVegQc3OE = as.character(RVegQc3OE)) %>%
              pivot_longer(cols = c("RVegQc3OE", "RVEG_COND")) 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','name'))
            expect_true(nrow(compOut)==20)
            
            compCond <- subset(compOut, name=='RVEG_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- subset(compOut, name=='RVegQc3OE') %>% 
              dplyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
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
            
            testOut.long <- mutate(testOut.lit, LitCvrQc3OE = as.character(LitCvrQc3OE)) %>%
              pivot_longer(cols = c('LitCvrQc3OE', 'LITCVR_COND')) 
            testCond.long <- mutate(nlaPhabIndicCond_test, 
                                    LitCvrQc3OE = as.character(LitCvrQc3OE)) %>%
              pivot_longer(cols = c('LitCvrQc3OE', 'LITCVR_COND')) 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','name'))
            expect_true(nrow(compOut)==20)
            
            compCond <- subset(compOut, name=='LITCVR_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- subset(compOut, name=='LitCvrQc3OE') %>% 
              dplyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.00001)
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
            
            testOut.long <- mutate(testOut, LitRipCvrQc3OE = as.character(LitRipCvrQc3OE)) %>%
              pivot_longer(cols = c("LitRipCvrQc3OE", "LITRIPCVR_COND")) 
            testCond.long <- mutate(nlaPhabIndicCond_test, LitRipCvrQc3OE = as.character(LitRipCvrQc3OE)) %>%
              pivot_longer(cols = c("LitRipCvrQc3OE", "LITRIPCVR_COND")) 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','name'))
            expect_true(nrow(compOut)==20)
            
            compCond <- subset(compOut, name=='LITRIPCVR_COND')
            expect_equal(compCond$value.x,compCond$value.y)
            
            compNum <- subset(compOut, name=='LitRipCvrQc3OE') %>% 
              dplyr::mutate(value.x=as.numeric(value.x),value.y=as.numeric(value.y))
            expect_equal(compNum$value.x,compNum$value.y,tolerance=0.0001)
          })

