library(testthat)
library(aquamet)

context("Test NRSA physical habitat indicator condition assignments")


test_that("NRSA riparian disturbance values correct",
          {
            testOut <- nrsaRipDistIndicator(nrsaPhabIndic_test,sampID='UID',w1_hall='W1_HALL')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nrsaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==11)
            expect_equal(compOut$value.x,compOut$value.y)
            
          })


test_that("NRSA relative bed stability values correct",
          {
            testOut <- nrsaRelBedStabilityIndicator(nrsaPhabIndic_test,sampID='UID', ecoreg='AG_ECO9' 
                                                    , protocol='REALM', lrbs='LRBS_USE', lat='LAT_DD83'
                                                    , lon='LON_DD83', area='WSAREASQKM', elev='ELEV_PT', slope='XSLOPE', xwidth='XWIDTH')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nrsaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==11)
            expect_equal(compOut$value.x,compOut$value.y)
            
          })

test_that("NRSA instream cover values correct",
          {
            testOut <- nrsaInstrmCoverIndicator(nrsaPhabIndic_test,sampID='UID', ecoreg='AG_ECO9', protocol='REALM'
                                                , xfc_nat='XFC_NAT', lat='LAT_DD83'
                                                , lon='LON_DD83', area='WSAREASQKM'
                                                , elev='ELEV_PT', slope='XSLOPE', xwidth='XWIDTH')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nrsaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==11)
            expect_equal(compOut$value.x,compOut$value.y)
            
          })


test_that("NRSA riparian vegetation values correct",
          {
            testOut <- nrsaRiparianVegIndicator(nrsaPhabIndic_test,sampID='UID', ecoreg='AG_ECO9'
                                                , protocol='REALM', xcmgw='XCMGW', lat='LAT_DD83'
                                                , lon='LON_DD83', area='WSAREASQKM', elev='ELEV_PT'
                                                , slope='XSLOPE', xwidth='XWIDTH')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID')) %>%
              plyr::mutate(variable=as.character(variable))
            testCond.long <- reshape2::melt(nrsaPhabIndicCond_test,id.vars='UID') 
            compOut <- merge(testCond.long,testOut.long,by=c('UID','variable'))
            expect_true(nrow(compOut)==11)
            expect_equal(compOut$value.x,compOut$value.y)
          })