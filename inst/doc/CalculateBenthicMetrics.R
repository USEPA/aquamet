## ----allBent.1-----------------------------------------------------------
library(aquamet)

head(bentEx)

## ----allBent.2-----------------------------------------------------------
head(bentTaxa_nrsa)

## ----allBent.3-----------------------------------------------------------
outdf <- calcAllBentMets(bentEx,bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA')

names(outdf)

## ----subMets.1-----------------------------------------------------------
outTax <- calcBentTaxMets(bentEx,bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                        ,dist='IS_DISTINCT',ct='TOTAL')
names(outTax)
head(outTax)

## ----subMets.2-----------------------------------------------------------
outTol <- calcBentTolMets(bentEx,bentTaxa_nrsa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                           ,dist='IS_DISTINCT',ct='TOTAL',ptv='PTV_WSA')
names(outTol)
head(outTol)


## ----mmiMets.1-----------------------------------------------------------
bentWSA <- prepBentCts_WSA(bentEx,bentTaxa_nrsa
                             ,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                             ,ct='TOTAL',taxa_id='TAXA_ID')

eco9 <- data.frame(UID=c(10000,10001), ECO9=c('CPL','WMT'),stringsAsFactors=F)

bentEx.1 <- merge(bentWSA, eco9, by='UID')

outMets <- calcNRSA_BentMMImets(inCts=bentEx.1,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA',ecoreg='ECO9')

head(outMets)


## ----mmiMets.3-----------------------------------------------------------

outMMI <- calcNRSA_BenthicMMI(outMets, sampID='UID', ecoreg='ECO9')

head(outMMI)


