#' @export
#' @title Calculate taxonomy metrics for benthic macroinvertebrates 
#' 
#' @description This function calculates all taxonomy related 
#' benthic metrics.
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @return A data frame containing the variables in sampID and 
#' the benthic macroinvertebrate metrics as additional variables.
#'  These metrics are named  AMPHNTAX, AMPHPIND, AMPHPTAX, CHIRNTAX, 
#'  CHIRPIND, CHIRPTAX, CRUSNTAX, CRUSPIND, CRUSPTAX, DIPTNTAX,
#'  DIPTPIND, DIPTPTAX, EPHENTAX, EPHEPIND, EPHEPTAX, EPOTNTAX, 
#'  EPOTPIND, EPOTPTAX, EPT_NTAX, EPT_PIND, EPT_PTAX, HEMINTAX, 
#'  HEMIPIND, HEMIPTAX, MITENTAX, MITEPIND, MITEPTAX, MOLLNTAX, 
#'  MOLLPIND, MOLLPTAX, NOINNTAX, NOINPIND, NOINPTAX, ODONNTAX, 
#'  ODONPIND, ODONPTAX, OLLENTAX, OLLEPIND, OLLEPTAX, ORTHNTAX, 
#'  ORTHPIND, ORTHPTAX, PLECNTAX, PLECPIND, PLECPTAX, TANYNTAX, 
#'  TANYPIND, TANYPTAX, TRICNTAX, TRICPIND, TRICPTAX, TUBINAIDNTAX, 
#'  TUBINAIDPIND, TUBINAIDPTAX, and ORTHCHIRPIND. 
#' 
#' Metric descriptions are included in \emph{NRSA_Invertebrate_Metric_Descriptions.pdf},
#' included in this package.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}


calcBentTaxMets <- function(inCts, inTaxa, sampID="UID", dist="IS_DISTINCT",
                             ct="TOTAL",taxa_id='TAXA_ID'){

  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  

  inCts <- subset(inCts,select=c(sampID,ct,dist,taxa_id))
  # Rename ct and dist to FINAL_CT and IS_DISTINCT
  names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
  names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  
  for(i in 1:length(sampID)){
    if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
    else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
  }  
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(inCts,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  # if(is.null(inTaxa)) {
  #   inTaxa <- bentTaxa
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }
  # }

  ## This code assumes that the following are columns in the taxa file: PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, TRIBE, HABIT, FFG, PTV, 
  ##    	TARGET_TAXON, 
  ## The two-letter codes for HABIT and FFG are assumed to match those used for NRSA. All names are assumed to be uppercase
  necTraits <- c('PHYLUM','CLASS','ORDER','FAMILY','TRIBE','SUBFAMILY','GENUS')
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
  }

  samples <- unique(subset(inCts,select=c(sampID,'SAMPID')))
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') %>%
    dplyr::select(SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0)
  
  inTaxa.1 <- mutate(inTaxa, EPT_=ifelse(ORDER %in% c('PLECOPTERA','EPHEMEROPTERA','TRICHOPTERA'),1,NA)
                     ,EPHE=ifelse(ORDER %in% c('EPHEMEROPTERA'),1,NA)
                     ,PLEC=ifelse(ORDER %in% c('PLECOPTERA'),1,NA)
                     ,TRIC=ifelse(ORDER %in% c('TRICHOPTERA'),1,NA)
                     ,CHIR=ifelse(FAMILY %in% c('CHIRONOMIDAE'),1,NA)
                     ,CRUS=ifelse(CLASS %in% c('MALACOSTRACA','MAXILLOPODA','BRANCHIOPODA'
                                               ,'CEPHALOCARIDA','OSTRACODA','REMIPEDIA'),1,NA)
                     ,NOIN=ifelse(CLASS %nin% c('INSECTA'),1,NA)
                     ,DIPT=ifelse(ORDER %in% c('DIPTERA'),1,NA)
                     ,MOLL=ifelse(PHYLUM %in% c('MOLLUSCA'),1,NA)
                     ,AMPH=ifelse(ORDER %in% c('AMPHIPODA'),1,NA)
                     ,EPOT=ifelse(ORDER %in% c('ODONATA','PLECOPTERA','EPHEMEROPTERA','TRICHOPTERA'),1,NA)
                     ,HEMI=ifelse(ORDER %in% c('HEMIPTERA'),1,NA)
                     ,MITE=ifelse(ORDER %in% c('TROMBIDIFORMES','SARCOPTIFORMES'),1,NA)
                     ,ODON=ifelse(ORDER %in% c('ODONATA'),1,NA)
                     ,OLLE=ifelse(CLASS %in% c('OLIGOCHAETA','HIRUDINEA','CLITELLATA'),1,NA)
                     ,ORTH=ifelse(SUBFAMILY %in% c('ORTHOCLADIINAE'),1,NA)
                     ,TANY=ifelse(TRIBE %in% c('TANYTARSINI'),1,NA)
                     ,TUBINAID=ifelse(FAMILY %in% c('TUBIFICIDAE','NAIDIDAE'),1,NA)
                     )
  
  # Drop non-target taxa if included in taxalist
  if(length(grep('NON_TARGET',names(inTaxa.1)))>0) {
    inTaxa.1 <- subset(inTaxa.1,is.na(NON_TARGET)|NON_TARGET=='')
  }
  
  params<-c('EPT_','EPHE','PLEC','TRIC','CHIR','CRUS','DIPT','MOLL','NOIN'
            ,'TUBINAID','AMPH','EPOT','HEMI','MITE','ODON','OLLE','ORTH','TANY')
  
  
  taxalong <- reshape2::melt(inTaxa.1[,c('TAXA_ID',params)],id.vars=c('TAXA_ID'),variable.name='TRAIT',na.rm=TRUE)
  taxalong$TRAIT <- as.character(taxalong$TRAIT)
  
  inCts.1 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                  TOTLNTAX=sum(IS_DISTINCT))
    
  # Merge the count data with the taxalist containing only the traits of
  # interest	
  traitDF <- merge(inCts.1, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT","TOTLNTAX"), summarise,
                  NIND=sum(FINAL_CT), NTAX=sum(IS_DISTINCT),
                  PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                  PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')  
  
  # Melt df to create metric names, then recast into wide format with metric
  # names
  print("Done calculating taxonomy metrics.")
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TOTLNTAX','TRAIT'))
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='') 
  outWide <-reshape2::dcast(outLong,SAMPID+TOTLNTAX~variable,value.var='value')  %>%
    merge(samples,by='SAMPID')
  
  # ORTHCHIRPIND are % of Chironomidae individuals in ORTHOCLADIINAE (not in
  # total indiv in sample)
  outWide <- plyr::ddply(outWide, c(sampID,'SAMPID','TOTLNTAX'), mutate,
                  ORTHCHIRPIND=round(ORTHNIND/CHIRNIND*100,2)) %>%
    mutate(ORTHCHIRPIND=ifelse(is.na(ORTHCHIRPIND)|is.nan(ORTHCHIRPIND),0,ORTHCHIRPIND))
  
  empty_tax <- data.frame(t(rep(NA,56)),stringsAsFactors=F)
  names(empty_tax) <- c('TOTLNTAX','AMPHNTAX','AMPHPIND'
                        ,'AMPHPTAX','CHIRNTAX','CHIRPIND'
                        ,'CHIRPTAX'
                        ,'CRUSNTAX','CRUSPIND','CRUSPTAX'
                        ,'DIPTNTAX','DIPTPIND','DIPTPTAX','EPHENTAX'
                        ,'EPHEPIND','EPHEPTAX','EPOTNTAX','EPOTPIND','EPOTPTAX','EPT_NTAX','EPT_PIND'
                        ,'EPT_PTAX','HEMINTAX','HEMIPIND','HEMIPTAX'
                        ,'MITENTAX','MITEPIND','MITEPTAX'
                        ,'MOLLNTAX','MOLLPIND','MOLLPTAX','NOINNTAX','NOINPIND','NOINPTAX'
                        ,'ODONNTAX','ODONPIND','ODONPTAX','OLLENTAX','OLLEPIND'
                        ,'OLLEPTAX','ORTHNTAX','ORTHPIND','ORTHPTAX','PLECNTAX','PLECPIND','PLECPTAX'
                        ,'TANYNTAX','TANYPIND'
                        ,'TANYPTAX','TRICNTAX','TRICPIND','TRICPTAX'
                        ,'TUBINAIDNTAX','TUBINAIDPIND','TUBINAIDPTAX','ORTHCHIRPIND')
  
  
  outWide.all <- gtools::smartbind(outWide, empty_tax) %>% filter(!is.na(SAMPID))
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide.all,id.vars=c(sampID,'SAMPID')) %>%
    mutate(value=ifelse(is.na(value),0,value)) 
#  outLong.1 <- outLong.1[grep('NIND',outLong.1$variable,invert=T),]  
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.fin <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  outWide.fin <- dplyr::select(outWide.fin, -SAMPID)

  outWide.fin <- outWide.fin[, c(sampID,'AMPHNTAX','AMPHPIND'
                                 ,'AMPHPTAX','CHIRNTAX','CHIRPIND'
                                 ,'CHIRPTAX'
                                 ,'CRUSNTAX','CRUSPIND','CRUSPTAX'
                                 ,'DIPTNTAX','DIPTPIND','DIPTPTAX','EPHENTAX'
                                 ,'EPHEPIND','EPHEPTAX','EPOTNTAX','EPOTPIND','EPOTPTAX','EPT_NTAX','EPT_PIND'
                                 ,'EPT_PTAX','HEMINTAX','HEMIPIND','HEMIPTAX'
                                 ,'MITENTAX','MITEPIND','MITEPTAX'
                                 ,'MOLLNTAX','MOLLPIND','MOLLPTAX','NOINNTAX','NOINPIND','NOINPTAX'
                                 ,'ODONNTAX','ODONPIND','ODONPTAX','OLLENTAX','OLLEPIND'
                                 ,'OLLEPTAX','ORTHNTAX','ORTHPIND','ORTHPTAX','PLECNTAX','PLECPIND','PLECPTAX'
                                 ,'TANYNTAX','TANYPIND'
                                 ,'TANYPTAX','TRICNTAX','TRICPIND','TRICPTAX'
                                 ,'TUBINAIDNTAX','TUBINAIDPIND','TUBINAIDPTAX','ORTHCHIRPIND')]

  return(outWide.fin)
  
}


#' @export
#' @title Calculate functional feeding group benthic metrics 
#' 
#' @description This function calculates 
#' all functional feeding group benthic metrics using trait 
#' information supplied.
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param ffg A string with the name of the functional feeding group 
#' variable in inTaxa. The default value is \emph{FFG}. Values used
#' in calculations include CF, CG, PR, SH, Sc, representing 
#' collector-filterer, collector-gatherer, predator, shredder, and
#' scraper, respectively. Each taxon may have more than 
#' one ffg value.
#' @return A data frame containing the variables in sampID and 
#' the benthic macroinvertebrate metrics as additional variables. 
#' Metrics included are named COFINTAX, COFIPIND, COFIPTAX,
#' COFITRICNTAX, COFITRICPIND, COFITRICPTAX, COGANTAX, COGAPIND,
#' COGAPTAX, PREDNTAX, PREDPIND, PREDPTAX, SCRPNTAX, SCRPPIND,
#' SCRPPTAX, SHRDNTAX, SHRDPIND, and SHRDPTAX.
#' 
#' Metric descriptions are included in \emph{NRSA_Invertebrate_Metric_Descriptions.pdf},
#' included in this package.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}

calcBentFFGmets <- function(inCts, inTaxa, sampID="UID", dist="IS_DISTINCT",
                        ct="TOTAL",taxa_id='TAXA_ID',ffg='FFG'){
  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  inCts <- subset(inCts,select=c(sampID,ct,dist,taxa_id))
  # Rename ct and dist to FINAL_CT and IS_DISTINCT
  names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
  names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  
  for(i in 1:length(sampID)){
    if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
    else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
  }  
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(inCts,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  # if(is.null(inTaxa)) {
  #   inTaxa <- bentTaxa
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }
  # }
  
  if(ffg %nin% names(inTaxa)){
    return(paste("Input taxa list does not contain variable",ffg,"."))
  } 
  
  if('ORDER' %nin% names(inTaxa)){
    print("Missing variable ORDER from input taxa list. Will not calculate Collector-filterer Trichoptera metrics.")
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c('TAXA_ID',ffg,'ORDER')) 
  names(inTaxa)[names(inTaxa)==ffg] <- 'FFG'
  
  samples <- unique(subset(inCts,select=c(sampID,'SAMPID')))
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') %>%
    dplyr::select(SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0)
  
  inTaxa.1 <- mutate(inTaxa
                     ,COFI=ifelse(str_detect(FFG,'CF'), 1, NA)
                     ,COGA=ifelse(str_detect(FFG,'CG'), 1, NA)
                     ,PRED=ifelse(str_detect(FFG,'PR'), 1, NA)
                     ,SHRD=ifelse(str_detect(FFG,'SH'), 1, NA)
                     ,SCRP=ifelse(str_detect(FFG,'SC'), 1, NA))

  if('ORDER' %in% names(inTaxa.1)){
    inTaxa.1 <- mutate(inTaxa.1, COFITRIC=ifelse(str_detect(FFG,'CF') & ORDER=='TRICHOPTERA', 1, NA))
  }
  
  # Drop non-target taxa if included in taxalist
  if(length(grep('NON_TARGET',names(inTaxa.1)))>0) {
    inTaxa.1 <- subset(inTaxa.1,is.na(NON_TARGET)|NON_TARGET=='')
  }
  
  params<-c('COFI','COGA','PRED','SHRD','SCRP','COFITRIC')  
  
  taxalong <- reshape2::melt(inTaxa.1[,c('TAXA_ID',params)],id.vars=c('TAXA_ID'),variable.name='TRAIT',na.rm=TRUE)
  taxalong$TRAIT <- as.character(taxalong$TRAIT)
  
  inCts.1 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                         TOTLNTAX=sum(IS_DISTINCT))
  
  # Merge the count data with the taxalist containing only the traits of
  # interest  
  traitDF <- merge(inCts.1, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT","TOTLNTAX"), summarise,
                        NTAX=sum(IS_DISTINCT),
                        PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                        PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')  
  
  # Melt df to create metric names, then recast into wide format with metric
  # names
  print("Done calculating functional feeding group metrics.")
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TOTLNTAX','TRAIT'))
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='') 
  outWide <-reshape2::dcast(outLong,SAMPID+TOTLNTAX~variable,value.var='value')  %>%
    merge(samples,by='SAMPID')
    
  empty_tax <- data.frame(t(rep(NA,18)),stringsAsFactors=F)
  names(empty_tax) <- c('COFINTAX','COFIPIND','COFIPTAX'
                        ,'COFITRICNTAX','COFITRICPIND','COFITRICPTAX'
                        ,'COGANTAX','COGAPIND','COGAPTAX'
                        ,'PREDNTAX','PREDPIND','PREDPTAX'
                        ,'SCRPNTAX','SCRPPIND','SCRPPTAX'
                        ,'SHRDNTAX','SHRDPIND','SHRDPTAX')
  
  outWide.all <- gtools::smartbind(outWide, empty_tax) %>% filter(!is.na(SAMPID))
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide.all,id.vars=c(sampID,'SAMPID')) %>%
    mutate(value=ifelse(is.na(value),0,value)) 
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.fin <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  outWide.fin <- dplyr::select(outWide.fin, -SAMPID)
  
  outWide.fin <- outWide.fin[, c(sampID,'COFINTAX','COFIPIND','COFIPTAX'
                                 ,'COFITRICNTAX','COFITRICPIND','COFITRICPTAX'
                                 ,'COGANTAX','COGAPIND','COGAPTAX'
                                 ,'PREDNTAX','PREDPIND','PREDPTAX'
                                 ,'SCRPNTAX','SCRPPIND','SCRPPTAX'
                                 ,'SHRDNTAX','SHRDPIND','SHRDPTAX')]
  
  return(outWide.fin)
  
}

#' @export
#' @title Calculate habit benthic metrics 
#' 
#' @description This function calculates habit 
#' benthic metrics.
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param habit A string with the name of the habit variable in inTaxa. 
#' The default value is \emph{HABIT}. Values for habit that are used in
#' calculations include BU, CB, CN, SP, SW, representing burrower, 
#' climber, clinger, sprawler, and swimmer, respectively. Each taxon 
#' may have more than one value for habit. 
#' @return A data frame containing the variables in sampID and 
#' the benthic macroinvertebrate metrics as additional variables.
#'  The metric names include  BURRNTAX, BURRPIND, BURRPTAX, CLMBNTAX, 
#'  CLMBPIND, CLMBPTAX, CLNGNTAX, CLNGPIND, CLNGPTAX, SPWLNTAX, 
#'  SPWLPIND, SPWLPTAX, SWIMNTAX, SWIMPIND, and SWIMPTAX. 
#'  
#' Metric descriptions are included in \emph{NRSA_Invertebrate_Metric_Descriptions.pdf},
#' included in this package.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}

calcBentHabitMets <- function(inCts, inTaxa, sampID="UID", dist="IS_DISTINCT",
                          ct="TOTAL",taxa_id='TAXA_ID',habit='HABIT'){

  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  inCts <- subset(inCts,select=c(sampID,ct,dist,taxa_id))
  # Rename ct and dist to FINAL_CT and IS_DISTINCT
  names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
  names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  
  for(i in 1:length(sampID)){
    if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
    else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
  }  
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(inCts,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  # if(is.null(inTaxa)) {
  #   inTaxa <- bentTaxa
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }
  # }
  
  if(habit %nin% names(inTaxa)){
    return(paste("Input taxa list does not contain variable",habit,"."))
  } 
   
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c('TAXA_ID',habit)) 
  names(inTaxa)[names(inTaxa)==habit] <- 'HABIT'
    
  samples <- unique(subset(inCts,select=c(sampID,'SAMPID')))
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') %>%
    dplyr::select(SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0)
  
  inTaxa.1 <- mutate(inTaxa
                     ,BURR=ifelse(stringr::str_detect(HABIT,'BU'), 1, NA)
                     ,CLMB=ifelse(stringr::str_detect(HABIT,'CB'), 1, NA)
                     ,CLNG=ifelse(stringr::str_detect(HABIT,'CN'), 1, NA)
                     ,SPWL=ifelse(stringr::str_detect(HABIT,'SP'), 1, NA)
                     ,SWIM=ifelse(stringr::str_detect(HABIT,'SW'), 1, NA))
  
  # Drop non-target taxa if included in taxalist
  if(length(grep('NON_TARGET',names(inTaxa.1)))>0) {
    inTaxa.1 <- subset(inTaxa.1,is.na(NON_TARGET)|NON_TARGET=='')
  }
  
  params<-c('BURR','CLMB','CLNG','SPWL','SWIM')  
  
  taxalong <- reshape2::melt(inTaxa.1[,c('TAXA_ID',params)],id.vars=c('TAXA_ID'),variable.name='TRAIT',na.rm=TRUE)
  taxalong$TRAIT <- as.character(taxalong$TRAIT)
  
  inCts.1 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                         TOTLNTAX=sum(IS_DISTINCT))
  
  # Merge the count data with the taxalist containing only the traits of
  # interest  
  traitDF <- merge(inCts.1, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT","TOTLNTAX"), summarise,
                        NTAX=sum(IS_DISTINCT),
                        PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                        PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')  
  
  # Melt df to create metric names, then recast into wide format with metric
  # names
  print("Done calculating habit metrics.")
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TOTLNTAX','TRAIT'))
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='') 
  outWide <-reshape2::dcast(outLong,SAMPID+TOTLNTAX~variable,value.var='value')  %>%
    merge(samples,by='SAMPID')
  
  empty_tax <- data.frame(t(rep(NA,15)),stringsAsFactors=F)
  names(empty_tax) <- c('BURRNTAX','BURRPIND','BURRPTAX'
                        ,'CLMBNTAX','CLMBPIND','CLMBPTAX'
                        ,'CLNGNTAX','CLNGPIND','CLNGPTAX'
                        ,'SPWLNTAX','SPWLPIND','SPWLPTAX'
                        ,'SWIMNTAX','SWIMPIND','SWIMPTAX')
  
  outWide.all <- gtools::smartbind(outWide, empty_tax) %>% filter(!is.na(SAMPID))
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide.all,id.vars=c(sampID,'SAMPID')) %>%
    mutate(value=ifelse(is.na(value),0,value)) 
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.fin <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  outWide.fin <- dplyr::select(outWide.fin, -SAMPID)
  
  outWide.fin <- outWide.fin[, c(sampID,'BURRNTAX','BURRPIND','BURRPTAX'
                                 ,'CLMBNTAX','CLMBPIND','CLMBPTAX'
                                 ,'CLNGNTAX','CLNGPIND','CLNGPTAX'
                                 ,'SPWLNTAX','SPWLPIND','SPWLPTAX'
                                 ,'SWIMNTAX','SWIMPIND','SWIMPTAX')]
  
  return(outWide.fin)
  
}

#' @export
#' @title Calculate tolerance benthic metrics 
#' 
#' @description This function calculates tolerance 
#' benthic metrics.
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param ptv A string with the name of the pollution tolerance value
#' variable in inTaxa. The default value is \emph{PTV}. Valid values
#' for ptv range from 0 to 10. 
#' @return A data frame containing the variables in sampID and  
#' the benthic macroinvertebrate metrics as additional variables. 
#'  The names of metrics are  FACLNTAX, FACLPIND, FACLPTAX, INTLNTAX, 
#'  INTLPIND, INTLPTAX, NTOLNTAX, NTOLPIND, NTOLPTAX, STOLNTAX, 
#'  STOLPIND, STOLPTAX, TL01NTAX, TL01PIND, TL01PTAX, TL23NTAX, 
#'  TL23PIND, TL23PTAX, TL45NTAX, TL45PIND, TL45PTAX, TL67NTAX, 
#'  TL67PIND, TL67PTAX, TOLRNTAX, TOLRPIND, TOLRPTAX, and WTD_TV.
#'  
#' Metric descriptions are included in \emph{NRSA_Invertebrate_Metric_Descriptions.pdf},
#' included in this package.  
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}

calcBentTolMets <- function(inCts, inTaxa, sampID="UID", dist="IS_DISTINCT",
                          ct="TOTAL",taxa_id='TAXA_ID',ptv='PTV'){
  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  inCts <- subset(inCts,select=c(sampID,ct,dist,taxa_id))
  # Rename ct and dist to FINAL_CT and IS_DISTINCT
  names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
  names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  
  for(i in 1:length(sampID)){
    if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
    else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
  }  
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(inCts,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  # if(is.null(inTaxa)) {
  #   inTaxa <- bentTaxa
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }
  # }
  
  if(ptv %nin% names(inTaxa)){
    return(paste("Input taxa list does not contain variable",ptv,"."))
  } 
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c('TAXA_ID',ptv))
  names(inTaxa)[names(inTaxa)==ptv] <- 'PTV'
  inTaxa <- mutate(inTaxa,PTV=as.numeric(PTV))
  
  samples <- unique(subset(inCts,select=c(sampID,'SAMPID')))
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') %>%
    dplyr::select(SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0)
  
  inTaxa.1 <- mutate(inTaxa
                     ,TOLR=ifelse(PTV >= 7, 1, NA)
                     ,FACL=ifelse(PTV >3 & PTV < 7, 1, NA)
                     ,INTL=ifelse(PTV <= 3, 1, NA)
                     ,NTOL=ifelse(PTV < 6, 1, NA)
                     ,STOL=ifelse(PTV >= 8, 1, NA)
                     ,TL01=ifelse(PTV < 2, 1, NA)
                     ,TL23=ifelse(PTV >= 2 & PTV < 4, 1, NA)
                     ,TL45=ifelse(PTV >= 4 & PTV < 6, 1, NA)
                     ,TL67=ifelse(PTV >= 6 & PTV < 8, 1, NA)
                     )
  
  # Drop non-target taxa if included in taxalist
  if(length(grep('NON_TARGET',names(inTaxa.1)))>0) {
    inTaxa.1 <- subset(inTaxa.1,is.na(NON_TARGET)|NON_TARGET=='')
  }
  
  params<-c('TOLR','FACL','INTL','NTOL','STOL','TL01','TL23','TL45','TL67','PTV')  
  
  taxalong <- reshape2::melt(inTaxa.1[,c('TAXA_ID',params)],id.vars=c('TAXA_ID'),variable.name='TRAIT',na.rm=TRUE)
  taxalong$TRAIT <- as.character(taxalong$TRAIT)
  
  inCts.1 <- plyr::ddply(inCts.1, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                         TOTLNTAX=sum(IS_DISTINCT))
  
  # Merge the count data with the taxalist containing only the traits of
  # interest  
  traitDF <- merge(inCts.1, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT","TOTLNTAX"), summarise,
                        NTAX=sum(IS_DISTINCT),
                        PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                        PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')  
   
  # Melt df to create metric names, then recast into wide format with metric
  # names
  print("Done calculating tolerance metrics.")
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TOTLNTAX','TRAIT'))
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='') 
  outWide <-reshape2::dcast(outLong,SAMPID+TOTLNTAX~variable,value.var='value')  %>%
    merge(samples,by='SAMPID')
  
  # Calculate HBI (WTD_TV)
  if(nrow(subset(taxalong,TRAIT %in% c('PTV')))>0){
    TVI <- tolindex(inCts.1,taxalong)  
    outWide <- merge(outWide,TVI,by="SAMPID",all.x=TRUE)
    print("Calculated tolerance index.")
  }  
  
  empty_tax <- data.frame(t(rep(NA,28)),stringsAsFactors=F)
  names(empty_tax) <- c('FACLNTAX','FACLPIND','FACLPTAX'
                        ,'INTLNTAX','INTLPIND','INTLPTAX'
                        ,'NTOLNTAX','NTOLPIND','NTOLPTAX'
                        ,'STOLNTAX','STOLPIND','STOLPTAX'
                        ,'TL01NTAX','TL01PIND','TL01PTAX'
                        ,'TL23NTAX','TL23PIND','TL23PTAX'
                        ,'TL45NTAX','TL45PIND','TL45PTAX'
                        ,'TL67NTAX','TL67PIND','TL67PTAX'
                        ,'TOLRNTAX','TOLRPIND','TOLRPTAX'
                        ,'WTD_TV')
  
  outWide.all <- gtools::smartbind(outWide, empty_tax) %>% filter(!is.na(SAMPID))
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide.all,id.vars=c(sampID,'SAMPID')) %>%
    mutate(value=ifelse(is.na(value) & variable!='WTD_TV',0,value)) 
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.fin <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  outWide.fin <- dplyr::select(outWide.fin, -SAMPID)
  
  outWide.fin <- outWide.fin[, c(sampID,'FACLNTAX','FACLPIND','FACLPTAX'
                                 ,'INTLNTAX','INTLPIND','INTLPTAX'
                                 ,'NTOLNTAX','NTOLPIND','NTOLPTAX'
                                 ,'STOLNTAX','STOLPIND','STOLPTAX'
                                 ,'TL01NTAX','TL01PIND','TL01PTAX'
                                 ,'TL23NTAX','TL23PIND','TL23PTAX'
                                 ,'TL45NTAX','TL45PIND','TL45PTAX'
                                 ,'TL67NTAX','TL67PIND','TL67PTAX'
                                 ,'TOLRNTAX','TOLRPIND','TOLRPTAX'
                                 ,'WTD_TV')]
  
  return(outWide.fin)
  
}


#' @export
#' @title Calculate dominance and diversity benthic metrics 
#' 
#' @description This function calculates overall dominance, 
#' chironomid dominance, and Shannon diversity 
#' benthic metrics.
#' 
#' @param inCts A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' and TRIBE, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{inCts} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @return A data frame containing the variables in sampID and  
#' the benthic macroinvertebrate metrics as additional variables.
#' The names of metrics include HPRIME, DOM1PIND, DOM3PIND, 
#' DOM5PIND, CHIRDOM1PIND, CHIRDOM3PIND, and CHIRDOM5PIND.
#' 
#' Metric descriptions are included in \emph{NRSA_Invertebrate_Metric_Descriptions.pdf},
#' included in this package.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}

calcBentDominMets <- function(inCts, inTaxa, sampID="UID", dist="IS_DISTINCT",
                          ct="TOTAL",taxa_id="TAXA_ID"){

  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
  
  inCts <- subset(inCts,select=c(sampID,ct,dist,taxa_id))
  # Rename ct and dist to FINAL_CT and IS_DISTINCT
  names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
  names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
  names(inCts)[names(inCts)==taxa_id] <- 'TAXA_ID'
  
  for(i in 1:length(sampID)){
    if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
    else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
  }  
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(inCts,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  # If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
  # NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
  # them from our calculations.
  # if(is.null(inTaxa)) {
  #   inTaxa <- bentTaxa
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }
  # }
  
  if('FAMILY' %nin% names(inTaxa)){
    print("Missing variable ORDER from input taxa list. Will not calculate chironomid dominance metrics.")
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c('TAXA_ID','FAMILY')) 
  
  samples <- unique(subset(inCts,select=c(sampID,'SAMPID')))
  inCts.1 <- dplyr::semi_join(inCts,subset(inTaxa,select='TAXA_ID'),by='TAXA_ID') %>%
    dplyr::select(SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT) %>%
    subset(!is.na(FINAL_CT) & FINAL_CT>0) 
   
  # Calculate Shannon Diversity
  outMet <- ShanDiversity(inCts.1)
  print("Shannon Diversity calculated")
  
  # Calculate % dominant individuals in top N taxa
  outMet <- merge(outMet,Dominance(inCts.1,1),by="SAMPID",all.x=TRUE)
  outMet <- merge(outMet,Dominance(inCts.1,3),by="SAMPID",all.x=TRUE)
  outMet <- merge(outMet,Dominance(inCts.1,5),by="SAMPID",all.x=TRUE)
  
  # In cases where there are fewer taxa than the topN number, we need to fix NAs
  # by setting to 100%
  outMet <- plyr::mutate(outMet, DOM3PIND=ifelse(is.na(DOM3PIND) & !is.nan(DOM1PIND), 100, DOM3PIND)
                   , DOM5PIND=ifelse(is.na(DOM5PIND) & !is.nan(DOM1PIND), 100, DOM5PIND))
  print("Dominance calculated")
  
  if('FAMILY' %in% names(inTaxa)){
    # Now calculate % dominant individuals in the top N taxa, but using totlnind
    # in sample as base of calculation
    chiroIn <- merge(inCts,inTaxa[,c('TAXA_ID','FAMILY')],by="TAXA_ID") %>%
      subset(FAMILY=='CHIRONOMIDAE', select=c('SAMPID','TAXA_ID','FINAL_CT','IS_DISTINCT')) %>%
      plyr::ddply('SAMPID', mutate, TOTLDIST=sum(IS_DISTINCT*FINAL_CT))        

    outMet <- merge(outMet, plyr::rename(Dominance(chiroIn,topN=1),
                                         c("DOM1PIND"="CHIRDOM1PIND")), by="SAMPID", all.x=TRUE)
    outMet <- merge(outMet,plyr::rename(Dominance(chiroIn, topN=3),
                                        c("DOM3PIND"="CHIRDOM3PIND")), by="SAMPID",all.x=TRUE)
    outMet <- merge(outMet,plyr::rename(Dominance(chiroIn, topN=5),
                                        c("DOM5PIND"="CHIRDOM5PIND")), by="SAMPID", all.x=TRUE)
    

    outMet <- plyr::mutate(outMet, CHIRDOM1PIND=ifelse(is.na(CHIRDOM1PIND),0,CHIRDOM1PIND)
                           ,CHIRDOM3PIND=ifelse(is.na(CHIRDOM3PIND) & CHIRDOM1PIND>0
                                                ,100,ifelse(is.na(CHIRDOM3PIND) & CHIRDOM1PIND==0,0,CHIRDOM3PIND))
                           ,CHIRDOM5PIND=ifelse(is.na(CHIRDOM5PIND) & CHIRDOM3PIND>0, 100
                                                ,ifelse(is.na(CHIRDOM5PIND) & CHIRDOM3PIND==0, 0, CHIRDOM5PIND)))
    
    
    print("Chironomid dominance calculated")
  
  }
  
  empty_tax <- data.frame(t(rep(NA,7)),stringsAsFactors=F)
  names(empty_tax) <- c('HPRIME','DOM1PIND','DOM3PIND','DOM5PIND'
                        ,'CHIRDOM1PIND','CHIRDOM3PIND','CHIRDOM5PIND')
  
  outWide <- gtools::smartbind(outMet, empty_tax) %>% filter(!is.na(SAMPID)) %>%
    merge(samples, by='SAMPID')
  
  # If we re-melt df now, we have missing values where the metric should be a
  # zero, so we can set NAs to 0 now
  outLong.1 <- reshape2::melt(outWide,id.vars=c(sampID,'SAMPID')) %>%
    mutate(value=ifelse(is.na(value),0,value)) 
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),'SAMPID',sep='+')
  formula <- paste(lside,'~variable',sep='')
  outWide.fin <- reshape2::dcast(outLong.1,eval(formula),value.var='value') 
  
  outWide.fin <- dplyr::select(outWide.fin, -SAMPID)
  
  outWide.fin <- outWide.fin[, c(sampID,'HPRIME','DOM1PIND','DOM3PIND','DOM5PIND'
                                 ,'CHIRDOM1PIND','CHIRDOM3PIND','CHIRDOM5PIND')]
  
  return(outWide.fin)  
}