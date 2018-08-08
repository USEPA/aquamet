## ----chanmorph-----------------------------------------------------------
library(aquamet)
library(stringr)
library(plyr)

head(bankgeomEx)
head(thalwegEx)

bBH <- subset(bankgeomEx,PARAMETER=='BANKHT' & SAMPLE_TYPE=='PHAB_CHANBFRONT')
bBW <- subset(bankgeomEx,PARAMETER=='BANKWID' & SAMPLE_TYPE=='PHAB_CHANBFRONT')
bD <- subset(thalwegEx,(PARAMETER=='DEP_SONR'|PARAMETER=='DEP_POLE') & SAMPLE_TYPE=='PHAB_THAL')
bWW <- subset(bankgeomEx,PARAMETER=='WETWID' & SAMPLE_TYPE=='PHAB_CHANBFRONT')
bInc <- subset(bankgeomEx,PARAMETER=='INCISED' & SAMPLE_TYPE=='PHAB_CHANBFRONT')
wBH <- subset(bankgeomEx,PARAMETER=='BANKHGT' & SAMPLE_TYPE=='PHAB_CHANW')
wBW <- subset(bankgeomEx,PARAMETER=='BANKWID' & SAMPLE_TYPE=='PHAB_CHANW')
wD <- subset(thalwegEx,PARAMETER=='DEPTH' & SAMPLE_TYPE=='PHAB_THALW')
wWW <- subset(thalwegEx,PARAMETER=='WETWIDTH')
wInc <- subset(bankgeomEx,PARAMETER=='INCISHGT' & SAMPLE_TYPE=='PHAB_CHANW')

chanmorphOut <- nrsaChannelMorphology(bBankHeight=bBH, bBankWidth=bBW,
    bDepth=bD, bIncisedHeight=bInc, bWettedWidth=bWW, wBankHeight=wBH,
    wBankWidth=wBW, wDepth=wD, wIncisedHeight=wInc, wWettedWidth=wWW)

head(chanmorphOut)
unique(chanmorphOut$METRIC)


## ----fishcvr-------------------------------------------------------------
head(fishcoverEx)
 
fishCvrOut <- nrsaFishCover(algae=subset(fishcoverEx,PARAMETER=='ALGAE'),
  boulder=subset(fishcoverEx,PARAMETER=='BOULDR'),
  brush=subset(fishcoverEx,PARAMETER=='BRUSH'),
  liveTree=subset(fishcoverEx,PARAMETER=='LVTREE'),
  macrophytes=subset(fishcoverEx,PARAMETER=='MACPHY'),
  overhang=subset(fishcoverEx,PARAMETER=='OVRHNG'),
  structures=subset(fishcoverEx,PARAMETER=='STRUCT'),
  undercut=subset(fishcoverEx,PARAMETER=='UNDCUT'),
  woodyDebris=subset(fishcoverEx,PARAMETER=='WOODY'))

head(fishCvrOut)
unique(fishCvrOut$METRIC)


## ----lwd.1---------------------------------------------------------------

# Run nrsaGeneral() to obtain reach length metric
wTr <- subset(thalwegEx, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0 & SAMPLE_TYPE=='PHAB_THALW'
                 ,select=c('SITE','VALUE')) %>%
  plyr::mutate(VALUE=as.numeric(VALUE)) %>%
  merge(plyr::ddply(unique(subset(thalwegEx, SAMPLE_TYPE=='PHAB_THALW', select=c('SITE','TRANSECT','STATION'))),
                    c('SITE','TRANSECT'), summarise, nSta = length(STATION)), by = 'SITE') %>%
  plyr::ddply(c('SITE'), mutate, lastTran=max(TRANSECT)) %>%
  plyr::mutate(wTr, VALUE = ifelse(TRANSECT!=lastTran, nSta*VALUE, (nSta-1)*VALUE)) %>%
  dplyr::select(-nSta,-lastTran)


bTr <- subset(changeomEx,PARAMETER=='ACTRANSP',select=c('SITE','TRANSECT','VALUE'))
tranSpace <- rbind(wTr,bTr) %>% plyr::mutate(VALUE=as.numeric(VALUE))

genOut <- nrsaGeneral(sampledTransects=unique(thalwegEx[,c('SITE','TRANSECT')]),
                        sideChannels=subset(thalwegEx,PARAMETER %in% c('SIDCHN','OFF_CHAN')),
                        transectSpacing=tranSpace)


## ----lwd.2---------------------------------------------------------------

boatCnt <- subset(woodEx,SITE %in% subset(visitsEx,VALXSITE=='BOATABLE',select='SITE')$SITE) %>%
  plyr::rename(c('PARAMETER'='CLASS')) %>% 
  plyr::mutate(VALUE=as.numeric(VALUE))

wadeCnt <- subset(woodEx,SITE %in% subset(visitsEx,VALXSITE=='WADEABLE',select='SITE')$SITE)  %>%
  plyr::rename(c('PARAMETER'='CLASS')) %>%
  plyr::mutate(VALUE=as.numeric(VALUE))

meanBankfull <- subset(chanmorphOut,METRIC=='xbkf_w')

  
# Now feed these data frames into the function nrsaLargeWoody()
lwdOut <- nrsaLargeWoody(bCounts=boatCnt,wCounts=wadeCnt,reachlength=subset(genOut,METRIC=='reachlen'),
                         meanBankfullWidth=meanBankfull)

unique(lwdOut$METRIC)

## ----subchar-------------------------------------------------------------
head(littoralEx)
head(thalwegEx)
head(channelxsectEx)

subcharOut <- nrsaSubstrateCharacterization (bBottomDom = subset(littoralEx, PARAMETER == 'BOTTOMDOM')
                                    ,bBottomSec = subset(littoralEx, PARAMETER == 'BOTTOMSEC')
                                    ,bShoreDom = subset(littoralEx, PARAMETER == 'SHOREDOM')
                                    ,bShoreSec = subset(littoralEx, PARAMETER == 'SHORESEC')
                                    ,bSizeClass = subset(thalwegEx, PARAMETER == 'SIZE_CLS' & 
                                                           SAMPLE_TYPE=='PHAB_THAL')
                                    ,wSizeClass = subset(channelxsectEx, PARAMETER == 'SIZE_CLS')
                                    ,wMezzoSizeClass = subset(channelxsectEx, PARAMETER == 'XSIZE_CLS'))


unique(subcharOut$METRIC)

## ----slope---------------------------------------------------------------
head(changeomEx)
# separate changeomEx boatable and wadeable sites
changeom.b <- subset(changeomEx, SAMPLE_TYPE=='PHAB_CHANBFRONT')


head(thalwegEx)

bBear <- subset(changeom.b, PARAMETER=='BEAR') %>%
   plyr::mutate(LINE=ifelse(LINE==999, 0, LINE))
bDist <- subset(changeom.b, PARAMETER=='DISTANCE') %>%
   plyr::mutate(LINE=ifelse(LINE==999, 0, LINE))

gis.bSlp <- data.frame(SITE=c(13799,13904),VALUE=c(0.002178,0.051732))

wBear <- subset(changeomEx, grepl('BEARING', PARAMETER) & SAMPLE_TYPE=='PHAB_SLOPE') %>%
   plyr::mutate(LINE = ifelse(PARAMETER == 'BEARING', 0
                ,ifelse(PARAMETER == 'BEARING2', 1
                ,ifelse(PARAMETER == 'BEARING3', 2, NA
                 ))) ,PARAMETER = NULL) 

wTr.alt <- subset(thalwegEx, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0 & SAMPLE_TYPE=='PHAB_THALW'
                 ,select=c('SITE','VALUE')) %>%
  plyr::mutate(VALUE=as.numeric(VALUE)) %>%
  merge(plyr::ddply(unique(subset(thalwegEx,SAMPLE_TYPE=='PHAB_THALW',select=c('SITE','TRANSECT','STATION'))),
                    c('SITE','TRANSECT'),summarise,nSta = length(STATION)), by = 'SITE') %>%
  plyr::mutate(VALUE = nSta*VALUE) %>%
  dplyr::select(-nSta)

wProp <- subset(changeomEx, grepl('PROP', PARAMETER) & SAMPLE_TYPE=='PHAB_SLOPE') %>%
  plyr::mutate(LINE = ifelse(PARAMETER == 'PROP', 0
                       ,ifelse(PARAMETER == 'PROP2', 1
                               ,ifelse(PARAMETER == 'PROP3', 2, NA))),PARAMETER = NULL
               , VALUE=as.numeric(VALUE))

wSlp <- subset(changeomEx, grepl('SLOPE', PARAMETER) & SAMPLE_TYPE=='PHAB_SLOPE') %>%
   plyr::mutate(LINE = ifelse(PARAMETER == 'SLOPE', 0
                ,ifelse(PARAMETER == 'SLOPE2', 1
                ,ifelse(PARAMETER == 'SLOPE3', 2, NA)))
          ,PARAMETER = NULL)                 

slopeOut <- nrsaSlopeBearing(bBearing = bBear
                              ,bDistance = bDist
                              ,wBearing = wBear                            
                              ,wTransectSpacing = wTr.alt                               
                              ,wProportion = wProp
                              ,wSlope = wSlp                        
                              ,gisSinuosity = NULL
                              ,gisSlope = gis.bSlp)

unique(slopeOut$METRIC)


## ----respools------------------------------------------------------------
meanslope <- subset(slopeOut, METRIC=='xslope') %>%
  plyr::mutate(VALUE=as.numeric(VALUE))

bDep <- subset(thalwegEx,(PARAMETER=='DEP_SONR'|PARAMETER=='DEP_POLE') & SAMPLE_TYPE=='PHAB_THAL') %>%
  plyr::mutate(VALUE=as.numeric(VALUE),STATION=as.numeric(STATION))
wDep <- subset(thalwegEx,PARAMETER=='DEPTH' & SAMPLE_TYPE=='PHAB_THALW') %>%
  plyr::mutate(VALUE=as.numeric(VALUE),STATION=as.numeric(STATION))

respoolOut <- nrsaResidualPools(bDepth = bDep     
                           ,wDepth = wDep     
                           ,siteSlopes = meanslope
                           ,transectSpacing = tranSpace)

unique(respoolOut$METRIC)

## ----bedstab-------------------------------------------------------------
boat <- data.frame(SITE=unique(bD$SITE),stringsAsFactors=F)
wade <- data.frame(SITE=unique(wD$SITE),stringsAsFactors=F)

bedstabOut <- nrsaBedStability(bXdepth =  subset(chanmorphOut, METRIC == 'xdepth' & SITE %in% unique(bD$SITE)
                                                 ,select=-METRIC)
                        ,bSddepth = subset(chanmorphOut, METRIC == 'sddepth' & SITE %in% unique(bD$SITE)
                                           ,select=-METRIC)
                        ,wXdepth =  subset(chanmorphOut, METRIC == 'xdepth' & SITE %in% unique(wD$SITE)
                                           ,select=-METRIC)
                        ,wSddepth = subset(chanmorphOut, METRIC == 'sddepth' & SITE %in% unique(wD$SITE)
                                           ,select=-METRIC)
                        ,lsub_dmm = subset(subcharOut, METRIC == 'lsub_dmm',select=-METRIC)
                        ,lsub2dmm = subset(subcharOut, METRIC == 'lsub2dmm',select=-METRIC)
                        ,rp100 =    subset(respoolOut, METRIC == 'rp100',select=-METRIC)
                        ,v1w_msq =  subset(lwdOut, METRIC == 'v1w_msq',select=-METRIC)
                        ,xbkf_h =   subset(chanmorphOut, METRIC == 'xbkf_h',select=-METRIC)
                        ,xbkf_w =   subset(chanmorphOut, METRIC == 'xbkf_w',select=-METRIC)
                        ,xfc_lwd =  subset(fishCvrOut, METRIC == 'xfc_lwd',select=-METRIC)
                        ,xslope =   subset(slopeOut, METRIC == 'xslope',select=-METRIC) %>%
                          plyr::mutate(VALUE=as.numeric(VALUE))
                        ,xwidth =   subset(chanmorphOut, METRIC == 'xwidth',select=-METRIC)
                        )

unique(bedstabOut$METRIC)
subset(bedstabOut, METRIC=='lrbs_g08')

