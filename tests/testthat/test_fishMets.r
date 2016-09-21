library(testthat)
library(aquametAlt)

context("Test fish metric and MMI calculations")

test_that("Fish tolerance metric values correct",
{
  testOut <- calcFishTolMets(fishCts_test,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                             ,taxa_id='TAXA_ID',tol='TOLERANCE_NRSA',tolval='TOL_VAL_EMAPW'
                             ,vel='VEL_NRSA',habitat='HABITAT_NRSA',trophic='TROPHIC_NRSA'
                             ,migr='MIGR_NRSA',nonnat='NON_NATIVE')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==740)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

# Run again with missing traits
test_that("Fish tolerance metric values without native or habitat correct",
{
  testOut <- calcFishTolMets(fishCts_test,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                             ,taxa_id='TAXA_ID',tol='TOLERANCE_NRSA',tolval='TOL_VAL_EMAPW'
                             ,vel='VEL_NRSA',trophic='TROPHIC_NRSA'
                             ,migr='MIGR_NRSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==330)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Fish taxonomic metric values correct",
{
  testOut <- calcFishTaxMets(fishCts_test,fishTaxa,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                             ,taxa_id='TAXA_ID',nonnat='NON_NATIVE',family='FAMILY'
                             ,genus='GENUS',comname='FINAL_NAME')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==420)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Fish taxonomic metric values correct",
{
  testOut <- calcFishTrophicMets(fishCts_test,fishTaxa,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                             ,taxa_id='TAXA_ID',nonnat='NON_NATIVE',trophic='TROPHIC_NRSA'
                             ,habitat='HABITAT_NRSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==360)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Fish other metric values correct",
{
  testOut <- calcFishOtherMets(fishCts_test,fishTaxa,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                               ,taxa_id='TAXA_ID',vel='VEL_NRSA'
                               ,migr='MIGR_NRSA', reprod='REPROD_NRSA', temp='TEMP_NRSA'
                               ,nonnat='NON_NATIVE')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'),all.y=T)
  expect_true(nrow(compOut)==360)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Fish native/alien metric values correct",
{
  testOut <- calcFishNativeMets(fishCts_test,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                               ,taxa_id='TAXA_ID',nonnat='NON_NATIVE')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'),all.y=T)
  expect_true(nrow(compOut)==90)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Fish anomaly metric values correct",
{
  testOut <- calcFishAnomMets(fishCts_test,sampID=c('UID'),ct='FINAL_CT'
                                ,anomct='ANOM_CT')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==10)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("All fish metric values correct",
{
  testOut <- calcAllFishMets(fishCts_test,fishTaxa,sampID=c('UID'),dist='IS_DISTINCT',
                             ct='FINAL_CT',anomct='ANOM_CT',taxa_id='TAXA_ID',
                             tol='TOLERANCE_NRSA',tolval='TOL_VAL_EMAPW',vel='VEL_NRSA',
                             habitat='HABITAT_NRSA',trophic='TROPHIC_NRSA',
                             migr='MIGR_NRSA',nonnat='NON_NATIVE',
                             reprod='REPROD_NRSA', temp='TEMP_NRSA',
                             family='FAM_OR_CLS',genus='GENUS',comname='FINAL_NAME')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==1740)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
})


ecoTest <- data.frame(UID=c(11222, 11703, 11711, 12384, 14080, 14275, 14315, 15194, 15196, 15198)
                      ,AGGR_ECO9_2015=c('UMW','CPL','CPL','CPL','CPL','SAP','TPL','XER','XER','WMT'),stringsAsFactors=F)
fishCts.eco <- merge(fishCts_test,ecoTest,by='UID')

test_that("MMI fish metric values correct",
{
  testOut <- calcNRSA_FishMMImets(fishCts.eco,fishTaxa,sampID=c('UID'),dist='IS_DISTINCT',
                             ct='FINAL_CT',anomct='ANOM_CT',taxa_id='TAXA_ID',
                             tol='TOLERANCE_NRSA',vel='VEL_NRSA',
                             habitat='HABITAT_NRSA',trophic='TROPHIC_NRSA',
                             migr='MIGR_NRSA',nonnat='NON_NATIVE',
                             reprod='REPROD_NRSA', temp='TEMP_NRSA',
                             family='FAMILY',genus='GENUS',comname='FINAL_NAME',
                             ecoreg='AGGR_ECO9_2015')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','AGGR_ECO9_2015')
                                 ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==80)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
})

# Create data frame of log10(watershed area) values

lwsarea <- data.frame(UID=c(11222, 11703, 11711, 12384, 14080, 14275, 14315, 15194, 15196, 15198)
                      ,LWSAREA=c(3.88120,6.37310,6.37530,3.81080,0.55011,4.41280,2.80430,2.28930,2.45630,2.02670))
fishws_test <- merge(fishMet_test,lwsarea,by='UID') %>% merge(ecoTest,by='UID')
# Put fish metrics in wide format
fishws_test.1 <- reshape2::dcast(fishws_test,UID+SAMPLE_TYPE+LWSAREA+AGGR_ECO9_2015~PARAMETER,value.var='RESULT')

test_that("Fish MMI scores correct",
          {
            testOut <- calcFishMMI(fishws_test.1,sampID=c('UID','SAMPLE_TYPE'),ecoreg='AGGR_ECO9_2015'
                                   ,lwsarea='LWSAREA')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','AGGR_ECO9_2015','SAMPLE_TYPE')
                                           ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            fishMMI_test.long <- reshape2::melt(fishMMI_test,id.vars=c('UID'),measure.vars='MMI_FISH'
                                                ,variable.name='PARAMETER',value.name='RESULT')
            compOut <- merge(fishMMI_test.long,testOut.long,by=c('UID','PARAMETER'))
            expect_true(nrow(compOut)==10)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
            
          })