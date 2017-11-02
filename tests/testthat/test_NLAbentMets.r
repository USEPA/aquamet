library(testthat)
library(aquamet)

context("Test NLA benthic metric and MMI calculations")


test_that("Benthic Taxonomy metric values correct",
          {
            testOut <- calcBentTaxMets(bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                                        ,dist='IS_DISTINCT',ct='TOTAL')
            testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
            expect_true(nrow(compOut)==550)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })

test_that("Benthic FFG metric values correct",
{
  testOut <- calcBentFFGmets(bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                              ,dist='IS_DISTINCT',ct='TOTAL',ffg='FFG')
  testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
  expect_true(nrow(compOut)==180)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic Habit metric values correct",
{
  testOut <- calcBentHabitMets(bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                         ,dist='IS_DISTINCT',ct='TOTAL',habit='HABIT')
  testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
  expect_true(nrow(compOut)==150)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Benthic tolerance metric values correct",
{
  testOut <- calcBentTolMets(bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                           ,dist='IS_DISTINCT',ct='TOTAL',ptv='PTV')
  testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
  expect_true(nrow(compOut)==280)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic dominance and diversity metric values correct",
{
  testOut <- calcBentDominMets(bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                         ,dist='IS_DISTINCT',ct='TOTAL')
  testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
  expect_true(nrow(compOut)==70)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  })

test_that("All benthic metric values correct",
          {
            testOut <- calcAllBentMets(indf=bentctsNLA_test,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG'
                                       ,habit='HABIT',ptv='PTV')
            testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
            expect_true(nrow(compOut)==1250)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
          })

indf.eco <- subset(bentMetsNLA_test,select=c('SITE_ID','ECO_BIO')) %>%
  unique() %>% merge(bentctsNLA_test,by='SITE_ID')

test_that("MMI metrics correct",
          {
            testOut <- calcNLA_BentMMImets(inCts=indf.eco,inTaxa=bentTaxa_nla,sampID=c('SITE_ID')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG'
                                       ,habit='HABIT',ptv='PTV',ecoreg='ECO_BIO')
            testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID','ECO_BIO')
                                           ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMetsNLA_test,testOut.long,by=c('SITE_ID','PARAMETER'))
            expect_true(nrow(compOut)==70)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })


testIn <- reshape2::dcast(bentMetsNLA_test,SITE_ID+ECO_BIO~PARAMETER,value.var='RESULT')

test_that("NRSA Benthic MMI scores correct",
{
  testOut <- calcNLA_BenthicMMI(testIn,sampID=c('SITE_ID')
                                 ,ecoreg='ECO_BIO',totlnind='TOTLNIND')
  testOut.long <- reshape2::melt(testOut,id.vars=c('SITE_ID','ECO_BIO')
                                 ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER)) %>%
    mutate(PARAMETER=plyr::revalue(PARAMETER,c('MMI_BENT'='MMI_BENT_NLA12','BENT_MMI_COND'='BENT_COND')))
  bentMMI_NLA_test.long <- reshape2::melt(bentMMI_NLA_test,id.vars=c('SITE_ID')
                                      ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    filter(PARAMETER %nin% c('ECO_BIO','TOTLNIND'))
  compOut <- merge(bentMMI_NLA_test.long,testOut.long,by=c('SITE_ID','PARAMETER'))
  expect_true(nrow(compOut)==73)
  compOut.cond <- subset(compOut, PARAMETER=='BENT_COND')
  expect_equal(compOut.cond$RESULT.x,compOut.cond$RESULT.y)
  compOut.res <- subset(compOut,PARAMETER!='BENT_COND') %>%
    plyr::mutate(RESULT.x=as.numeric(RESULT.x),RESULT.y=as.numeric(RESULT.y))
  expect_equal(compOut.res$RESULT.x,compOut.res$RESULT.y,tolerance=0.01)  
  
})