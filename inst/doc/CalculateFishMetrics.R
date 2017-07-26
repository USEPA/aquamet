## ----allFish.1-----------------------------------------------------------
library(aquamet)

head(fishEx)

## ----allFish.2-----------------------------------------------------------
head(fishTaxa)

## ----allFish.3-----------------------------------------------------------
outdf <- calcAllFishMets(fishEx,fishTaxa,sampID=c("UID"), dist="IS_DISTINCT",
                            ct="FINAL_CT",anomct='ANOM_CT',taxa_id='TAXA_ID',tol='TOLERANCE_NRSA'
                            , tolval='TOL_VAL_EMAPW', vel='VEL_NRSA', habitat='HABITAT_NRSA'
                            , trophic='TROPHIC_NRSA', migr='MIGR_NRSA', nonnat='NON_NATIVE'
                            , reprod='REPROD_NRSA', temp='TEMP_NRSA', family='FAMILY', genus='GENUS'
                            , comname='FINAL_NAME')

names(outdf)

## ----subMets.1-----------------------------------------------------------
outTax <- calcFishTaxMets(fishEx, fishTaxa, sampID='UID', dist='IS_DISTINCT',
                          ct='FINAL_CT', taxa_id='TAXA_ID', family='FAMILY', genus='GENUS',
                          comname='FINAL_NAME')
names(outTax)
head(outTax)

## ----subMets.2-----------------------------------------------------------
outTol <- calcFishTolMets(fishEx, fishTaxa, sampID='UID', dist='IS_DISTINCT',
                          ct='FINAL_CT', taxa_id='TAXA_ID', tol='TOLERANCE_NRSA',
                          tolval='TOL_VAL_EMAPW', nonnat='NON_NATIVE', 
                          habitat='HABITAT_NRSA')
names(outTol)
head(outTol)


## ----mmiMets.1-----------------------------------------------------------
eco9 <- data.frame(UID=c(10000,10001), ECO9=c('CPL','WMT'),stringsAsFactors=F)

fishEx.1 <- merge(fishEx, eco9, by='UID')

## ----mmiMets.2-----------------------------------------------------------
outMets <- calcNRSA_FishMMImets(fishEx.1,fishTaxa, "UID", ecoreg='ECO9'
                                 ,dist="IS_DISTINCT",ct="FINAL_CT"
                                 ,taxa_id='TAXA_ID',tol='TOLERANCE_NRSA'
                                 ,vel='VEL_NRSA', habitat='HABITAT_NRSA'
                                 ,trophic='TROPHIC_NRSA', migr='MIGR_NRSA', nonnat='NON_NATIVE'
                                 ,reprod='REPROD_NRSA', family='FAMILY', genus='GENUS'
                                 ,comname='FINAL_NAME')

head(outMets)


## ----mmiMets.3-----------------------------------------------------------
wsarea <- data.frame(UID=c(10000,10001), LWSAREA=c(2.1, 3.5), stringsAsFactors=F)

outMets.1 <- merge(outMets, wsarea, by='UID')

outMMI <- calcFishMMI(outMets.1, sampID='UID', ecoreg='ECO9', lwsarea='LWSAREA')

head(outMMI)


## ----fish condition------------------------------------------------------
outCond <- assignFishCondition(outMMI, sampID='UID', ecoreg='ECO9', mmi='MMI_FISH')

outCond


