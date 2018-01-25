#' @export
#' 
#' @title Calculate all benthic metrics 
#' 
#' @description This is a wrapper function that calculates 
#' all benthic metrics as used in the National Aquatic 
#' Resource Surveys.
#' 
#' @param indf A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, and taxa_id
#' @param inTaxa a data frame containing taxonomic information, 
#' including variables for PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, 
#' TRIBE, and GENUS, as well as autecology traits with names that match those 
#' in the arguments ffg, habit, and ptv. In addition, there
#' should be a variable with the name in argument taxa_id that matches 
#' with all of those in the indf data frame. 
#' @param sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{indf} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param ffg A string with the name of the functional feeding group 
#' variable in inTaxa. The default value is \emph{FFG}. Values used
#' in calculations include CF, CG, PR, SH, Sc, representing 
#' collector-filterer, collector-gatherer, predator, shredder, and
#' scraper, respectively. Each taxon may have more than 
#' one FFG value.
#' @param habit A string with the name of the habit variable in inTaxa. 
#' The default value is \emph{HABIT}. Values for habit that are used in
#' calculations include BU, CB, CN, SP, SW, representing burrower, 
#' climber, clinger, sprawler, and swimmer, respectively. Each taxon 
#' may have more than one value for HABIT. 
#' @param ptv A string with the name of the pollution tolerance value 
#' variable in inTaxa. The default is \emph{PTV}.
#' @return A data frame containing the variables in sampID and all of 
#' the benthic macroinvertebrate metrics as additional variables.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @examples 
#'   data(bentEx)
#'   head(bentEx)
#'   head(bentTaxa_nrsa)
#'   # Calculate metrics for bentIn, using the taxonomy in the count file as is
#'   bentMetrics <- calcAllBentMets(indf=bentEx, inTaxa=bentTaxa_nrsa,
#'                      sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT'), 
#'                      dist='IS_DISTINCT',
#'                      ct='TOTAL',taxa_id='TAXA_ID',
#'                      ffg='FFG',habit='HABIT',ptv='PTV')
#'   head(bentMetrics)
#' @keywords survey
calcAllBentMets <- function(indf,inTaxa, sampID="UID", dist="IS_DISTINCT",
                        ct="TOTAL",taxa_id='TAXA_ID',ffg='FFG',habit='HABIT',ptv='PTV'){
  
  # Make sure all taxa match to taxalist and send error if not
  checkTaxa <- dplyr::anti_join(indf,inTaxa,by='TAXA_ID') 
  if(nrow(checkTaxa)>0){
    return(print('Taxa in counts that do not have matches in taxalist! Cannot continue.'))
  }
  
  if('NON_TARGET' %in% names(inTaxa)){
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "" |NON_TARGET=='N')
  }

  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(indf))){
    msgTraits <- which(ctVars %nin% names(indf))
    print(paste("Missing variables in input count data frame:",paste(names(indf)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  # Taxonomy and traits checks
  necTraits <- c('PHYLUM','CLASS','ORDER','FAMILY','TRIBE','SUBFAMILY','GENUS'
                 ,ffg,habit,ptv)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c('TAXA_ID','PHYLUM','CLASS','ORDER','FAMILY'
                                                      ,'TRIBE','SUBFAMILY','GENUS',ffg,habit,ptv)) 
  
  indf[,c(ct,taxa_id,dist)] <- lapply(indf[,c(ct,taxa_id,dist)],as.numeric)
  inTaxa[,c(ptv,taxa_id)] <- lapply(inTaxa[,c(ptv,taxa_id)],as.numeric)

  
  taxMet <- calcBentTaxMets(indf,inTaxa,sampID,dist,ct,taxa_id)
  tax.1 <- reshape2::melt(taxMet,id.vars=sampID)
  
  ffgMet <- calcBentFFGmets(indf,inTaxa,sampID,dist,ct,taxa_id,ffg)
  ffg.1 <- reshape2::melt(ffgMet,id.vars=sampID)
  
  habitMet <- calcBentHabitMets(indf,inTaxa,sampID,dist,ct,taxa_id,habit)
  habit.1 <- reshape2::melt(habitMet,id.vars=sampID)
  
  tolMet <- calcBentTolMets(indf,inTaxa,sampID,dist,ct,taxa_id,ptv)
  tol.1 <- reshape2::melt(tolMet,id.vars=sampID)
  
  domMet <- calcBentDominMets(indf,inTaxa,sampID,dist,ct,taxa_id)
  dom.1 <- reshape2::melt(domMet,id.vars=sampID)
  
  names(indf)[names(indf)==ct] <- 'FINAL_CT'
  names(indf)[names(indf)==dist] <- 'IS_DISTINCT'
  
  totals <- plyr::ddply(indf, sampID, summarise, TOTLNIND=sum(FINAL_CT),
                                   TOTLNTAX=sum(IS_DISTINCT)) %>%
    melt(id.vars=sampID)
  
  mets <- rbind(tax.1, ffg.1, habit.1, tol.1, dom.1,totals)
  
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),sep='+')
  formula <- paste(lside,'~variable',sep='')
  metOut <- reshape2::dcast(mets,eval(formula),value.var='value') 
  
  return(metOut)
}