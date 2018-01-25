library(testthat)
library(aquamet)

context("Test NRSA benthic metric and MMI calculations")


test_that("Data prep correct",
{
  testOut <- prepBentCts_WSA(inCts=bentCts_test,inTaxa=bentTaxa_nrsa
                             ,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                             ,ct='TOTAL300'
                             ,taxa_id='TAXA_ID')
  expect_equal(nrow(testOut),nrow(indf_test))
  expect_equal(names(testOut),names(indf_test))
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'))
  indf.long <- reshape2::melt(indf_test,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'))
  compOut <- merge(testOut.long,indf.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID','variable'))
  expect_equal(compOut$value.x,compOut$value.y)
})


test_that("Benthic Taxonomy metric values correct",
          {
            testOut <- calcBentTaxMets(indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                        ,dist='IS_DISTINCT',ct='TOTAL')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==550)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })

test_that("Benthic FFG metric values correct",
{
  testOut <- calcBentFFGmets(indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                              ,dist='IS_DISTINCT',ct='TOTAL',ffg='FFG_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==180)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic Habit metric values correct",
{
  testOut <- calcBentHabitMets(indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                         ,dist='IS_DISTINCT',ct='TOTAL',habit='HABIT_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==150)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Benthic tolerance metric values correct",
{
  testOut <- calcBentTolMets(indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                           ,dist='IS_DISTINCT',ct='TOTAL',ptv='PTV_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==280)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic dominance and diversity metric values correct",
{
  testOut <- calcBentDominMets(indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                         ,dist='IS_DISTINCT',ct='TOTAL')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==70)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  })

test_that("All benthic metric values correct",
          {
            testOut <- calcAllBentMets(indf=indf_test,inTaxa=bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==1250)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
          })


ecoTest <- data.frame(UID=c(11245,11703,11821,12483,12571,12999,13971,14822,15073,15803)
                      ,AGGR_ECO9_2015=c('TPL','CPL','NAP','SAP','SAP','UMW','CPL','NPL'
                                        ,'UMW','XER'),stringsAsFactors=F)
indf.eco <- merge(indf_test,ecoTest,by='UID')
test_that("MMI metrics correct",
          {
            testOut <- calcNRSA_BentMMImets(inCts=indf.eco,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA',ecoreg='AGGR_ECO9_2015')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','AGGR_ECO9_2015')
                                           ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==70)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })


testIn <- merge(bentMet_test,ecoTest,by='UID') %>%
  reshape2::dcast(UID+SAMPLE_TYPE+SAMPLE_CAT+AGGR_ECO9_2015~PARAMETER,value.var='RESULT')

test_that("NRSA Benthic MMI scores correct",
{
  testOut <- calcNRSA_BenthicMMI(testIn,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,ecoreg='AGGR_ECO9_2015',totlnind='TOTLNIND')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','AGGR_ECO9_2015','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  bentMMI_test.long <- reshape2::melt(bentMMI_test,id.vars=c('UID')
                                      ,variable.name='PARAMETER',value.name='RESULT')
  compOut <- merge(bentMMI_test.long,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==80)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
  
})