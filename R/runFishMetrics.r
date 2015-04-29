runFishMetrics <- function(indata, parlist, prefix, taxalist) {

################################################################################
# Function: runFishMetrics
# Title: Perform Metric Calculations
# Programmer: Karen Blocksom
# Date: May 11, 2011
# Description:
#   This function take a data frame and a list of parameters and sends data to
#   the ntaxcalc and propcalc functions.
# Function Revisions:
#   05/11/11 kab: Created.
# Arguments:
#   indata = name of input dataset in long format (one count record per row),
#     FINAL_CT as count variable, and UID as the sample identifier.  All missing
#     values and zeroes should be removed.
#   parlist = vector of categorical parameter names as strings to use in
#     calculating metrics.
#   prefix = a string to indicate the dataset, to be added to the beginning of
#    each metric name.
#   taxalist = list of taxa in long format, which must contain PARAMETER = trait
#     name and RESULT = trait value.
# Output:
#   A data frame containing metrics.
################################################################################

	# Calculate and add columns to indata for total number of individuals and taxa
	indata <- ddply(indata, 'SAMPID', mutate, TOTLNTAX=sum(IS_DISTINCT),
	  TOTLNIND=sum(FINAL_CT))

	# Merge the count data with the taxalist containing only the traits of
	# interest	
	traitDF <- merge(subset(indata, select=c('SAMPID', 'FINAL_CT', 'IS_DISTINCT',
	  'TAXA_ID', 'TOTLNTAX', 'TOTLNIND')), subset(taxalist, PARAMETER %in%
	  parlist), by='TAXA_ID')

	# Calculate no. individuals, % individuals, no. taxa, and % taxa for each
	# trait in taxalist
	outMet <- ddply(traitDF, c("SAMPID", "PARAMETER"), summarise,
	  NIND=sum(FINAL_CT), NTAX=sum(IS_DISTINCT),
	  PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
	  PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')

	# Melt df to create metric names, then recast into wide format with metric
	# names
	outLong <- melt(outMet,id.vars=c('SAMPID','PARAMETER'))
	outLong$variable <- paste(outLong$PARAMETER,outLong$variable,sep='') 
	outWide <-dcast(outLong,SAMPID~variable,value.var='value') 

	# If we re-melt df now, we have missing values where the metric should be a
	# zero, so we can set NAs to 0 now
	outLong1 <- melt(outWide,id.vars='SAMPID')
	outLong1$value[is.na(outLong1$value)] <- 0

	# Finally, we can recast the metrics df into wide format for output
	outWide1 <- dcast(outLong1,SAMPID~variable,value.var='value')
	
	# If PTV or TOL_VAL are in the parameter list (of traits), calculate a
	# weighted tolerance value (equivalent to HBI for bugs)
	# Otherwise, ignore this step
	if(nrow(subset(taxalist,PARAMETER %in% c('PTV','TOL_VAL')))>0){
		TVI <- tolindexFish(indata,taxalist)	
		allmet <- merge(outWide1,TVI,by="SAMPID",all.x=TRUE)
	}else{
		allmet <- outWide1
	}
	
	# Merge metrics with the original indata so that those without metrics because
	# no sample was collected are still output with missing values
	outMet <- merge(unique(indata[, c('SAMPID', 'TOTLNIND', 'TOTLNTAX')]), allmet,
	  by='SAMPID', all.x=TRUE)

	# If specified, add prefix to metric names - mainly applies to native metrics
	# currently
	names(outMet)[2:length(outMet)] <- paste(prefix,
	  names(outMet[2:length(outMet)]), sep='')

	return(outMet)
}
