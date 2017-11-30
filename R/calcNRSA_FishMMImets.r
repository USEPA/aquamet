#' @export
#' @title Calculate only fish metrics necessary for NRSA MMI
#' 
#' @description This is a function that calculates 
#' only the fish metrics used in the National Rivers and Streams
#' Assessment fish MMI.
#' 
#' @param indata A data frame containing, at minimum, the variables 
#' specified in the arguments for \emph{sampID}, \emph{dist}, 
#' \emph{ct}, and \emph{taxa_id}, as well as the optional variable 
#' for non-native status in \emph{nonnat}.
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables that match the arguments for (\emph{family}), 
#' (\emph{genus}), and (\emph{comname}), as well as autecology traits 
#' with names that match those in the arguments \emph{tol}, \emph{vel}, 
#' \emph{habitat}, \emph{trophic}, \emph{migr}, and \emph{reprod}. 
#' In addition, there should be a variable with the 
#' name in argument \emph{taxa_id} that matches 
#' with all of those in the indata data frame
#' @param sampID A character vector containing the names of all 
#' variables in indata that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param ecoreg A string with the name of the ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, sPL, TPL, UMW, WMT, and XER.
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{FINAL_CT}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{indata} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param tol A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing tolerance categories. Valid
#' values include S (sensitive), I (intermediate), and T (tolerant).
#' The default value is \emph{TOLERANCE}.
#' @param vel A strings with the name of the variable in the 
#' \emph{inTaxa} taxalist containing velocity preference values.
#' Valid values include R (Rheophil), P (Pool), O (Other), or 
#' blank, if unknown. The default value is \emph{VELOCITY}.
#' @param habitat A string with the name of the variable in 
#' \emph{inTaxa} containing habitat preference values. Valid
#' values include B (benthic), W (water column), E (edge), or
#' blank, if unknown. The default value is \emph{HABITAT}.
#' @param trophic A string with the name of the variable in
#' \emph{inTaxa} containing trophic category values. Valid
#' values include I (invertivore), C (carnivore), O (omnivore),
#' H (herbivore), or blank if unknown. The default value is
#' \emph{inTaxa} is TROPHIC.
#' @param migr A string with the name of the variable in 
#' \emph{inTaxa} containing migratory status value. Valid 
#' values include N (No), Y (Yes), and blank if unknown. The
#' default value is \emph{MIGRATORY}.
#' @param nonnat A string with the name of the optional variable in 
#' \emph{inCts} containing non-native status. Valid values are 'Y' for 
#' non-native and 'N' for native. The default name 
#' is \emph{NONNATIVE}.
#' @param reprod A string with the name of the variable in 
#' \emph{inTaxa} containing reproductive trait values. Valid 
#' values include C (Clean, coarse lithophil), D (Drifter),
#' G (Guarder), O (Other), or blank if unknown. The default
#' value is \emph{REPROD}.
#' @param family A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing family name. The default value 
#' is \emph{FAMILY}.
#' @param genus A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the genus name. The default
#' value is \emph{GENUS}
#' @param comname A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the common name. The 
#' default value is \emph{NAME}.
#' @return A data frame containing the variables in sampID, 
#' the total number of individuals in the sample as TOTLNIND, and 
#' all fish metrics used in MMIs as additional variables. All metrics
#' will be separate columns, but values will only be provided for 
#' the metrics in the MMI for the specific region of each site,
#' with all other metrics being missing for that site. The metrics, 
#' by aggregated ecoregion, are:
#' 
#'  CPL: ALIENPIND, RBCATONTAX, LOTPIND, INTLMIGRPTAX, LITHPIND, NAT_TOTLNTAX, 
#'      TOLRNTAX, INVPTAX
#'      
#'  NAP: ALIENNTAX, SALMNTAX, NAT_RHEOPIND, INTLMIGRPIND, LITHPTAX,
#'      NTOLPTAX, TOLRNTAX, INVNTAX
#'      
#'  NPL: ALIENNTAX, NAT_CYPRPIND, LOTNTAX, MIGRNTAX, LITHPIND, NTOLPTAX,
#'      NAT_INTLPIND, NAT_CARNNTAX 
#'      
#'  SAP: NAT_PTAX, NAT_CENTNTAX, NAT_NTOLBENTPTAX, NAT_MIGRNTAX,
#'      NAT_LITHPIND, NTOLPTAX, TOLRPTAX, INVPIND
#'      
#'  SPL: NAT_PIND, CYPRPTAX, RHEOPIND, NAT_MIGRPTAX, LITHNTAX,
#'      NAT_NTOLNTAX, TOLRNTAX, HERBPTAX
#'  
#'  TPL: ALIENNTAX, NAT_ICTAPIND, RHEONTAX, INTLMIGRNTAX, LITHPIND,
#'      NAT_NTOLNTAX, INTLPTAX, CARNNTAX
#'      
#'  UMW: NAT_PTAX, CYPRNTAX, INTLLOTNTAX, INTLMIGRPTAX, LITHPIND,
#'      NTOLNTAX, TOLRNTAX, INTLINVPTAX
#'      
#'  WMT: NAT_PIND, NAT_CATOPIND, INTLLOTPTAX, NAT_MIGRPTAX, LITHPTAX,
#'      NAT_TOTLNTAX, TOLRNTAX, NAT_HERBPTAX
#'      
#'  XER: NAT_PIND, CENTPTAX, RHEOPIND, MIGRPTAX, LITHNTAX, NTOLPTAX,
#'      TOLRNTAX, BENTINVPTAX
#'
#' Metric descriptions are included in \emph{NRSA_Fish_Metric_Descriptions.pdf},
#' included in this package.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
calcNRSA_FishMMImets <- function(indata,inTaxa=NULL, sampID="UID", ecoreg=NULL
                                 ,dist="IS_DISTINCT",ct="TOTAL"
                                 ,taxa_id='TAXA_ID',tol='TOLERANCE'
                                 ,vel='VELOCITY', habitat='HABITAT'
                                 ,trophic='TROPHIC', migr='MIGRATORY', nonnat='NONNATIVE'
                                 ,reprod='REPROD', family='FAMILY', genus='GENUS'
                                 ,comname='NAME'){
  
  if(is.null(inTaxa)) {
    inTaxa <- fishTaxa
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "")
  }
  
  ctVars <- c(sampID, dist, ct, taxa_id, nonnat,ecoreg)
  if(any(ctVars %nin% names(indata))){
    msgTraits <- which(ctVars %nin% names(indata))
    print(paste("Missing variables in input count data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
  
  ecoCk <- unique(indata[,ecoreg])
  ecos <- c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
  if(any(ecoCk %nin% ecos)){
    msgEco <- which(ecoCk %nin% ecos)
    print(paste("These ecoregions are not valid: "
                ,paste(ecoCk[msgEco],collapse=',')))
    return(NULL)
  }

  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) indata$SAMPID <- indata[,sampID[i]]
    else indata$SAMPID <- paste(indata$SAMPID,indata[,sampID[i]],sep='.')
  }
  # Keep data frame with crosswalk info between sampID and SAMPID
  samples <- unique(subset(indata,select=c(sampID,ecoreg,'SAMPID')))
  
  
  metnames <- data.frame(ECO=c(rep('CPL',8),rep('NAP',8),rep('NPL',8),rep('SAP',8),rep('SPL',8)
                                          ,rep('TPL',8),rep('UMW',8),rep('WMT',8),rep('XER',8))
                         ,METRIC=c(c('ALIENPIND','RBCATONTAX','LOTPIND','INTLMIGRPTAX','LITHPIND','NAT_TOTLNTAX','TOLRNTAX'
                                        ,'INVPTAX')
                                      ,c('ALIENNTAX','SALMNTAX','NAT_RHEOPIND','INTLMIGRPIND','LITHPTAX','NTOLPTAX','TOLRNTAX'
                                         ,'INVNTAX')
                                      ,c('ALIENNTAX','NAT_CYPRPIND','LOTNTAX','MIGRNTAX','LITHPIND','NTOLPTAX','NAT_INTLPIND'
                                         ,'NAT_CARNNTAX')
                                      ,c('NAT_PTAX','NAT_CENTNTAX','NAT_NTOLBENTPTAX','NAT_MIGRNTAX','NAT_LITHPIND','NTOLPTAX'
                                         ,'TOLRPTAX','INVPIND')
                                      ,c('NAT_PIND','CYPRPTAX','RHEOPIND','NAT_MIGRPTAX','LITHNTAX','NAT_NTOLNTAX','TOLRNTAX'
                                         ,'HERBPTAX')
                                      ,c('ALIENNTAX','NAT_ICTAPIND','RHEONTAX','INTLMIGRNTAX','LITHPIND','NAT_NTOLNTAX','INTLPTAX'
                                         ,'CARNNTAX')
                                      ,c('NAT_PTAX','CYPRNTAX','INTLLOTNTAX','INTLMIGRPTAX','LITHPIND','NTOLNTAX','TOLRNTAX'
                                         ,'INTLINVPTAX')
                                      ,c('NAT_PIND','NAT_CATOPIND','INTLLOTPTAX','NAT_MIGRPTAX','LITHPTAX','NAT_TOTLNTAX'
                                         ,'TOLRNTAX','NAT_HERBPTAX')
                                      ,c('NAT_PIND','CENTPTAX','RHEOPIND','MIGRPTAX','LITHNTAX','NTOLPTAX','TOLRNTAX'
                                         ,'BENTINVPTAX'))
                         ,stringsAsFactors=F)
  
  empty_base <- data.frame(t(rep(NA,length(unique(metnames$METRIC)))),stringsAsFactors=F) 
  names(empty_base) <- unique(metnames$METRIC)
  
  # Taxonomy and traits checks
  necTraits <- c(taxa_id,tol, vel, habitat, trophic, migr, 
                 reprod, family, genus, comname)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    print(paste("Some of the traits are missing from the taxa list. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% necTraits) 
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(indata)[names(indata)==nonnat] <- 'NONNATIVE'
  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
  
  names(inTaxa)[names(inTaxa)==tol] <- 'TOLERANCE'
  names(inTaxa)[names(inTaxa)==vel] <- 'VELOCITY'  
  names(inTaxa)[names(inTaxa)==habitat] <- 'HABITAT'
  names(inTaxa)[names(inTaxa)==trophic] <- 'TROPHIC'
  names(inTaxa)[names(inTaxa)==migr] <- 'MIGRATORY'
  names(inTaxa)[names(inTaxa)==family] <- 'FAMILY'
  names(inTaxa)[names(inTaxa)==genus] <- 'GENUS'
  names(inTaxa)[names(inTaxa)==comname] <- 'NAME'
  names(inTaxa)[names(inTaxa)==reprod] <- 'REPROD'  
  
  indata[,c('FINAL_CT','IS_DISTINCT')] <- lapply(indata[,c('FINAL_CT','IS_DISTINCT')],as.numeric)
  indata$TAXA_ID <- as.character(indata$TAXA_ID)
  inTaxa$TAXA_ID <- as.character(inTaxa$TAXA_ID)
  
  ## for inCts1, keep only observations without missing or zero FINAL_CT values or TAXA_ID and TAXA_ID!=99999
  indata.1 <- subset(indata,!is.na(TAXA_ID) & !is.na(FINAL_CT) & FINAL_CT!=0)
  
  indata.2 <- dplyr::select(indata.1,SAMPID, TAXA_ID, FINAL_CT, IS_DISTINCT, NONNATIVE)
  
  inTaxa.1 <- plyr::mutate(inTaxa, BENTINV=ifelse(HABITAT=='B' & TROPHIC=='I',1,NA)
                           ,CARN=ifelse(TROPHIC=='C',1,NA)
                           ,CENT=ifelse(toupper(FAMILY)=='CENTRARCHIDAE' & toupper(GENUS)!='MICROPTERUS',1,NA)
                           ,CYPR=ifelse(toupper(FAMILY)=='CYPRINIDAE' & toupper(NAME) %nin% 
                                          c('COMMON CARP','GOLDFISH','BIGHEAD CARP','GRASS CARP','MIRROR CARP'),1,NA)
                           ,HERB=ifelse(TROPHIC=='H',1,NA)
                           ,INTL=ifelse(TOLERANCE=='S',1,NA)
                           ,INTLINV=ifelse(INTL==1 & TROPHIC=='I',1,NA)
                           ,INTLLOT=ifelse(INTL==1 & VELOCITY %in% c('R','O'),1,NA)
                           ,INTLMIGR=ifelse(INTL==1 & MIGRATORY=='Y',1,NA)
                           ,INV=ifelse(TROPHIC %in% c('I'),1,NA)
                           ,LITH=ifelse(REPROD=='C',1,NA)
                           ,LOT=ifelse(VELOCITY %in% c('R','O'),1,NA)
                           ,MIGR=ifelse(MIGRATORY=='Y',1,NA)
                           ,CATO=ifelse(toupper(FAMILY)=='CATOSTOMIDAE',1,NA)
                           ,ICTA=ifelse(toupper(FAMILY)=='ICTALURIDAE',1,NA)
                           ,NTOL=ifelse(TOLERANCE %in% c('S','I'),1,NA)
                           ,NTOLBENT=ifelse(NTOL==1 & HABITAT=='B',1,NA)
                           ,RHEO=ifelse(VELOCITY=='R',1,NA)
                           ,RBCATO=ifelse(toupper(GENUS) %in% c('MOXOSTOMA', 'HYPENTELIUM'
                                                                , 'MINYTREMA', 'ERIMYZON' , 'CATOSTOMUS', 'CYCLEPTUS'
                                                                , 'PANTOSTEUS' , 'THOBURNIA'),1,NA)
                           ,SALM=ifelse(toupper(FAMILY)=='SALMONIDAE',1,NA)
                           ,TOLR=ifelse(TOLERANCE=='T',1,NA)
                           )
            
  params<-c('BENTINV','CARN','CENT','CYPR','HERB','INV','INTLINV','INTLLOT','INTLMIGR','INTL','LITH','LOT','MIGR','CATO'
          ,'ICTA','NTOLBENT','NTOL','RHEO','RBCATO','SALM','TOLR')
  
  inTaxa.2 <- subset(inTaxa.1,select=names(inTaxa.1) %in% c('TAXA_ID',params))
  
  taxalong <- reshape2::melt(inTaxa.2,id.vars='TAXA_ID',variable.name='TRAIT',na.rm=TRUE) %>%
    plyr::mutate(TRAIT=as.character(TRAIT))
  
  indata.3 <- plyr::ddply(indata.2, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
                         TOTLNTAX=sum(IS_DISTINCT)) 
  
  # Merge the count data with the taxalist containing only the traits of
  # interest  
  traitDF <- merge(indata.3, taxalong, by='TAXA_ID')
  
  # Calculate no. individuals, % individuals, no. taxa, and % taxa for each
  # trait in taxalist
  outMet <- plyr::ddply(traitDF, c("SAMPID", "TRAIT"), summarise,
                        NTAX=sum(IS_DISTINCT),
                        PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
                        PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')
  
  # Melt df to create metric names, then recast into wide format with metric
  # names
  outLong <- reshape2::melt(outMet,id.vars=c('SAMPID','TRAIT')) 
  outLong$variable <- paste(outLong$TRAIT,outLong$variable,sep='')     
  outWide <- reshape2::dcast(outLong,SAMPID~variable,value.var='value') 
  
  # Calculate metrics based on native status and other traits
  if(any(unique(indata.3$NON_NATIVE) %nin% c('Y','N'))){
    return(print("No native and alien datasets were created because NON_NATIVE must only be 'Y' or 'N' values"))     
  }else{
    inNative <- subset(indata.3,NONNATIVE=='N') 
    if(length(inNative)>0){
      inNative.tot <- plyr::ddply(inNative,c('SAMPID'),mutate,NAT_TOTLNIND=sum(FINAL_CT),
                                  NAT_TOTLNTAX=sum(IS_DISTINCT))
      totals.nat <- unique(inNative.tot[,c('SAMPID','NAT_TOTLNIND','NAT_TOTLNTAX')])
      
      natMets <- merge(inNative.tot, taxalong, by='TAXA_ID') %>%
        plyr::ddply(c('SAMPID','TRAIT','NAT_TOTLNTAX','NAT_TOTLNIND'),summarise,
                    NTAX=sum(IS_DISTINCT),
                    PIND=round(sum(FINAL_CT/NAT_TOTLNIND)*100,2),
                    PTAX=round(sum(IS_DISTINCT/NAT_TOTLNTAX)*100,2), .progress='tk')      
      
      natMets.long <- reshape2::melt(natMets,id.vars=c('SAMPID','TRAIT','NAT_TOTLNTAX','NAT_TOTLNIND')) %>% 
        mutate(variable=paste('NAT_',TRAIT,variable,sep=''))
      
      natMets.1 <- reshape2::dcast(natMets.long,SAMPID~variable,value.var='value')       
      
      outWide.1 <- merge(outWide,natMets.1,all=T)
            
    }else{
      outWide.1 <- outWide
    }
  }
  
  
  # Metrics relying only on native status
  inNat <- plyr::ddply(indata.3, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),TOTLNTAX=sum(IS_DISTINCT)) %>%
    mutate(ALIEN=ifelse(NONNATIVE=='Y',1,NA),NAT=ifelse(NONNATIVE=='N',1,NA))
  
  outNat <- plyr::ddply(inNat, c("SAMPID"), summarise,
                        ALIENNTAX=sum(IS_DISTINCT*ALIEN,na.rm=T),
                        ALIENPIND=round(sum(FINAL_CT*ALIEN/TOTLNIND,na.rm=T)*100,2),
                        NAT_TOTLNTAX=sum(IS_DISTINCT*NAT,na.rm=T),
                        NAT_PIND=round(sum(FINAL_CT*NAT/TOTLNIND,na.rm=T)*100,2),
                        NAT_PTAX=round(sum(IS_DISTINCT*NAT/TOTLNTAX,na.rm=T)*100,2),.progress='tk') 
  
  outAll <- merge(outWide.1,outNat,by='SAMPID',all=T) %>%
    gtools::smartbind(empty_base) %>% 
    filter(!is.na(SAMPID)) %>%
    merge(samples,by='SAMPID',all.y=T)

  outLong <- reshape2::melt(outAll,id.vars=c(sampID,ecoreg,'SAMPID')) %>%
    plyr::mutate(value=ifelse(is.na(value),0,value)) %>%
    merge(metnames,by.x=c(ecoreg,'variable'),by.y=c('ECO','METRIC')) 
  
  ckMetnum <- as.data.frame(table(SAMPID=outLong$SAMPID)) %>% 
    dplyr::filter(Freq!=8)
  if(nrow(ckMetnum)>0){
    print("Error in output! Wrong number of metrics per site!")
    print(ckMetnum)
  }
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste('SAMPID',paste(sampID,collapse='+'),ecoreg,sep='+')
  formula <- paste(lside,'~variable',sep='')
  metOut <- reshape2::dcast(outLong,eval(formula),value.var='value') %>%
    merge(unique(indata.3[,c('SAMPID','TOTLNIND')]),by='SAMPID') %>%
    dplyr::select(-SAMPID)
  
  return(metOut)
}




