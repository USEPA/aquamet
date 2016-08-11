#' @export
#' 
#' @title Calculate benthic MMI metrics 
#' @description This function calculates only the benthic
#' metrics in the corresponding MMI used in the National
#' Rivers and Streams Assessment (NRSA), based on the 
#' aggregated Omernik ecoregions included in the input 
#' data frame. 
#' @param indf A data frame containing, at minimum, the variables 
#' specified in the arguments for sampID, dist, ct, taxa_id, and
#' ecoreg
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
#' @param ecoreg A string with the name of the ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, sPL, TPL, UMW, WMT, and XER.
#' @param taxa_id A string with the name of the taxon ID variable 
#' that matches that in inTaxa. The default value is \emph{TAXA_ID}.
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
#' @keywords survey
calcNRSA_MMImets <- function(indf,inTaxa=NULL, sampID="UID",ecoreg=NULL
                  ,dist="IS_DISTINCT",ct="TOTAL",taxa_id='TAXA_ID'
                  ,ffg='FFG',habit='HABIT',ptv='PTV'){
  if(is.null(inTaxa)) {
    inTaxa <- bentTaxa
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "")
  }
  
  ctVars <- c(sampID,dist,ct,taxa_id,ffg,habit,ptv,ecoreg)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:"
                ,paste(names(inCts)[msgTraits],collapse=',')))
    return(NULL)
  }
  
  ecoCk <- unique(indf[,ecoreg])
  ecos <- c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
  if(any(ecoCk %nin% ecos)){
    msgEco <- which(ecoCk %nin% ecos)
    print(paste("These ecoregions are not valid: "
                ,paste(ecoCk[msgEco],collapse=',')))
    return(NULL)
  }
  
  
  
}