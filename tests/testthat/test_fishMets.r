library(testthat)
library(aquametAlt)

context("Test fish metric and MMI calculations")

test_that("Fish tolerance metric values correct",
{
  testOut <- calcFishTolMets(fishCts_test,sampID=c('UID'),dist='IS_DISTINCT',ct='FINAL_CT'
                             ,taxa_id='TAXA_ID',tol='TOLERANCE_NRSA',tolval='TOL_VAL'
                             ,vel='VEL_GLEC',habitat='HABITAT_GLEC',trophic='TROPHIC_GLEC'
                             ,migr='MIGR_GLEC',nonnat='NON_NATIVE')
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
                             ,taxa_id='TAXA_ID',tol='TOLERANCE_NRSA',tolval='TOL_VAL'
                             ,vel='VEL_GLEC',trophic='TROPHIC_GLEC'
                             ,migr='MIGR_GLEC')
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
                             ,taxa_id='TAXA_ID',nonnat='NON_NATIVE',family='FAM_OR_CLS'
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
                             ,taxa_id='TAXA_ID',nonnat='NON_NATIVE',trophic='TROPHIC_GLEC'
                             ,habitat='HABITAT_GLEC')
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
                               ,taxa_id='TAXA_ID',vel='VEL_GLEC'
                               ,migr='MIGR_GLEC', reprod='REPROD_GLEC', temp='TEMP_GLEC'
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
                             tol='TOLERANCE_NRSA',tolval='TOL_VAL',vel='VEL_GLEC',
                             habitat='HABITAT_GLEC',trophic='TROPHIC_GLEC',
                             migr='MIGR_GLEC',nonnat='NON_NATIVE',
                             reprod='REPROD_GLEC', temp='TEMP_GLEC',
                             family='FAM_OR_CLS',genus='GENUS',comname='FINAL_NAME')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(fishMet_test,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==1740)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
})

