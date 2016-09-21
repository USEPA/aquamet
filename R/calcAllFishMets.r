#' @export
#' @title Calculate all fish metrics
#' 
#' @description This is a wrapper function that calculates 
#' all fish metrics as used in the National Aquatic 
#' Resource Surveys.
#' 
#' @param indf A data frame containing, at minimum, the variables 
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
#' specified, the default is \emph{FINAL_CT}.
#' @param anomct A string with the name of the count variable. If not 
#' specified, the default is \emph{ANOM_CT}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{indf} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param tol A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing tolerance categories. Valid
#' values include S (sensitive), I (intermediate), and T (tolerant).
#' The default value is \emph{TOLERANCE}.
#' @param tolval A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing numeric tolerance values.
#' Valid values range from 0 to 10. The default value is TOL_VAL.
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
#' @param temp A string with the name of the variable in 
#' \emph{inTaxa} containing the temperature preference values.
#' Valid values include WM (warm), CD (cold water), CL (cool water), 
#' or blank if unknow. The default value is \emph{TEMP}.
#' @param family A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing family name. The default value 
#' is \emph{FAMILY}.
#' @param genus A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the genus name. The default
#' value is \emph{GENUS}
#' @param comname A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing the common name. The 
#' default value is \emph{NAME}.
#' @return A data frame containing the variables in sampID and 
#' all fish metrics as additional variables. These are listed in
#' the document \emph{NRSA_Fish_Metric_Descriptions.pdf} included
#' in this package, which contains metric descriptions.
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @examples 
#'   \dontrun{
#'   data(fishEx)
#'   head(fishEx)
#'   head(fishTaxa)
#'   # Calculate metrics for bentIn, using the taxonomy in the count file as is
#'   fishMetrics <- calcAllFishMets(indf=fishEx, inTaxa=fishTaxa,
#'                      sampID='UID',dist='IS_DISTINCT',
#'                      ct='FINAL_CT',anomct='ANOM_CT',taxa_id='TAXA_ID',
#'                      tol='TOLERANCE_NRSA',tolval='TOL_VAL_EMAPW',vel='VEL_NRSA',
#'                      habitat='HABITAT_NRSA',trophic='TROPHIC_NRSA',
#'                      migr='MIGR_NRSA',nonnat='NON_NATIVE',reprod='REPROD_NRSA',
#'                      temp='TEMP_NRSA',family='FAMILY',genus='GENUS',
#'                      comname='FINAL_NAME')
#'                      head(fishMetrics)}
#' @keywords survey
calcAllFishMets <- function(indf,inTaxa=NULL, sampID="UID", dist="IS_DISTINCT",
                            ct="TOTAL",anomct='ANOM_CT',taxa_id='TAXA_ID',tol='TOLERANCE'
                            , tolval='TOL_VAL', vel='VELOCITY', habitat='HABITAT'
                            , trophic='TROPHIC', migr='MIGRATORY', nonnat='NONNATIVE'
                            , reprod='REPROD', temp='TEMP', family='FAMILY', genus='GENUS'
                            , comname='NAME'){
  
  if(is.null(inTaxa)) {
    inTaxa <- fishTaxa
    inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "")
  }
  
  ctVars <- c(sampID, dist, ct, taxa_id, anomct, nonnat)
  if(any(ctVars %nin% names(indf))){
    msgTraits <- which(ctVars %nin% names(indf))
    print(paste("Missing variables in input count data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
  
  # Taxonomy and traits checks
  necTraits <- c(taxa_id,tol, tolval, vel, habitat, trophic, migr, 
                 reprod, temp, family, genus, comname)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    print(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:", necTraits[msgTraits]))
    return(NULL)
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% necTraits) 
  
  taxMet <- calcFishTaxMets(indf,inTaxa,sampID,dist,ct,taxa_id,nonnat,family,genus,comname)
  tax.1 <- reshape2::melt(taxMet,id.vars=sampID) 
  print("Done calculating taxonomic metrics.")
  
  tolMet <- calcFishTolMets(indf,inTaxa,sampID,dist,ct,taxa_id,tol,tolval,
                            vel,habitat,trophic,migr,nonnat)
  tol.1 <- reshape2::melt(tolMet,id.vars=sampID) 
  print("Done calculating tolerance metrics.")
  
  tropMet <- calcFishTrophicMets(indf,inTaxa,sampID,dist,ct,taxa_id,habitat
                                 ,trophic,nonnat)
  trop.1 <- reshape2::melt(tropMet,id.vars=sampID) 
  print("Done calculating trophic metrics.")
  
  othMet <- calcFishOtherMets(indf,inTaxa,sampID,dist,ct,taxa_id,vel,migr
                              ,reprod,temp,nonnat)
  oth.1 <- reshape2::melt(othMet,id.vars=sampID) 
  print("Done calculating other metrics.")
  
  natMet <- calcFishNativeMets(indf,sampID,dist,ct,taxa_id,nonnat)
  nat.1 <- reshape2::melt(natMet,id.vars=sampID)
  print("Done calculating native metrics.")
  
  anomMet <- calcFishAnomMets(indf,sampID,ct,anomct)
  anom.1 <- reshape2::melt(anomMet,id.vars=sampID)
  print("Done calculating anomaly metrics.")
  
  mets <- rbind(tax.1, tol.1, trop.1, oth.1, nat.1, anom.1) %>%
    unique()
  # Finally, we can recast the metrics df into wide format for output
  lside <- paste(paste(sampID,collapse='+'),sep='+')
  formula <- paste(lside,'~variable',sep='')
  metOut <- reshape2::dcast(mets,eval(formula),value.var='value') 
  
  return(metOut)
}