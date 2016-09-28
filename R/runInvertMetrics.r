runInvertMetrics <- function(indata, parlist, taxalist) {

################################################################################
# Function: runInvertMetrics
# Title: Perform Metric Calculations
# Programmer: Karen Blocksom
# Date: April 16, 2012
# Description:
#   This function take a data frame and a list of parameters and sends data to
#   the ntaxcalc and propcalc functions.
# Function Revisions:
#   06/16/12 kab: Created.
#   12/04/12 kab: Updated to remove unnecessary components.
# Arguments:
#   indata = name of input dataset in long format (one count record per row),
#     FINAL_CT as count variable, and UID as the sample identifier.  All missing
#     values and zeroes should be removed.
#   parlist = vector of categorical parameter names as strings to use in
#     calculating metrics.
#   taxalist = list of taxa in long format, which must contain PARAMETER = trait
#     name and RESULT = trait value.
# Output:
#   A data frame containing metrics.
################################################################################

	# Calculate and add columns to indata for total number of individuals and taxa
	indata <- ddply(indata, "SAMPID", mutate, TOTLNIND=sum(FINAL_CT),
	   TOTLNTAX=sum(IS_DISTINCT))
	print("Calculated total taxa and individuals.")

	# Merge the count data with the taxalist containing only the traits of
	# interest	
	traitDF <- merge(subset(indata, select=c('SAMPID', 'FINAL_CT', 'IS_DISTINCT',
	   'TAXA_ID', 'TOTLNTAX', 'TOTLNIND')), subset(taxalist, PARAMETER %in%
	   parlist), by='TAXA_ID')

	# Calculate no. individuals, % individuals, no. taxa, and % taxa for each
	# trait in taxalist
	print("Ready to start calculating metrics by trait.")
	outMet <- ddply(traitDF, c("SAMPID", "PARAMETER"), summarise,
	   NIND=sum(FINAL_CT), NTAX=sum(IS_DISTINCT),
	   PIND=round(sum(FINAL_CT/TOTLNIND)*100,2),
	   PTAX=round(sum(IS_DISTINCT/TOTLNTAX)*100,2), .progress='tk')

	# Melt df to create metric names, then recast into wide format with metric
	# names
	print("Done calculating trait-based metrics.")
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
	# weighted tolerance value (equivalent to HBI for bugs).  Otherwise, ignore
	# this step.
	if(nrow(subset(taxalist,PARAMETER %in% c('PTV','TOL_VAL')))>0){
		TVI <- tolindex(indata,taxalist)	
		allmet <- merge(outWide1,TVI,by="SAMPID",all.x=TRUE)
		print("Calculated tolerance index.")
	}else{
		allmet <- outWide1
	}

	# Merge metrics with the original indata so that those without metrics because
	# no sample was collected are still output
	#	with missing values
	outMet <- merge(unique(indata[,c('SAMPID', 'TOTLNIND', 'TOTLNTAX')]), allmet,
	  by='SAMPID', all.x=TRUE)
	
	# ORTHCHIRPIND are % of Chironomidae individuals in ORTHOCLADIINAE (not in
	# total indiv in sample)
	print("Ready to calculate ORTHCHIRPIND.")
	outMet <- ddply(outMet, "SAMPID", mutate,
	  ORTHCHIRPIND=round(ORTHNIND/CHIRNIND*100,2))
		outMet$ORTHCHIRPIND <- with(outMet, ifelse(is.na(ORTHCHIRPIND) |
		  is.nan(ORTHCHIRPIND), 0, ORTHCHIRPIND))
	
	# Calculate Shannon Diversity
	outMet <- merge(outMet,ShanDiversity(indata),by="SAMPID",all.x=TRUE)
	print("Shannon Diversity calculated")
	
	# Calculate % dominant individuals in top N taxa
	outMet <- merge(outMet,Dominance(indata,1),by="SAMPID",all.x=TRUE)
	outMet <- merge(outMet,Dominance(indata,3),by="SAMPID",all.x=TRUE)
	outMet <- merge(outMet,Dominance(indata,5),by="SAMPID",all.x=TRUE)

	# In cases where there are fewer taxa than the topN number, we need to fix NAs
	# by setting to 100%
	which3 <- with(outMet,which(is.na(DOM3PIND) & !is.nan(DOM1PIND)))
	outMet$DOM3PIND[which3] <- 100
	which5 <- with(outMet,which(is.na(DOM5PIND) & !is.nan(DOM1PIND)))
	outMet$DOM5PIND[which5] <- 100
	print("Dominance calculated")
	
	# Now calculate % dominant individuals in the top N taxa, but using totlnind
	# in sample as base of calculation
	chiroIn <- merge(indata,taxalist,by="TAXA_ID")
	chiroIn <- subset(chiroIn, PARAMETER=='CHIR' & RESULT=='1',
	  select=c('SAMPID','TAXA_ID','FINAL_CT','IS_DISTINCT','TOTLNIND'))	
	outMet <- merge(outMet, plyr::rename(Dominance(chiroIn,topN=1),
	  c("DOM1PIND"="CHIRDOM1PIND")), by="SAMPID", all.x=TRUE)
	outMet <- merge(outMet,plyr::rename(Dominance(chiroIn, topN=3),
	  c("DOM3PIND"="CHIRDOM3PIND")), by="SAMPID",all.x=TRUE)
	outMet <- merge(outMet,plyr::rename(Dominance(chiroIn, topN=5),
	  c("DOM5PIND"="CHIRDOM5PIND")), by="SAMPID", all.x=TRUE)
	
	# For cases where there are fewer than topN chironomid taxa, set the value of
	# CHIRDOMNPIND to CHIRPIND
	which1 <- which(is.na(outMet$CHIRDOM1PIND))
	outMet$CHIRDOM1PIND[which1] <- outMet$CHIRPIND[which1]
	which3 <- which(is.na(outMet$CHIRDOM3PIND))
	outMet$CHIRDOM3PIND[which3] <- outMet$CHIRPIND[which3]
	which5 <- which(is.na(outMet$CHIRDOM5PIND))
	outMet$CHIRDOM5PIND[which5] <- outMet$CHIRPIND[which5]
	print("Chironomid dominance calculated")
	
	return(outMet)
}
