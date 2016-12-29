###############################################################################
# metCalcFxns.r
#
# Contains functions used in metric calculation programs for fish and bugs
# 
# Programmers: Karen Blocksom
#              Tom Kincaid
# Date: June 5, 2013
# Function Revisions:
#   06/05/13 kab: Created based on MetricCalcFxns.R.
#   08/09/13 kab: Updated to use only distinct taxa in Dominance() calculations,
#            following A. Herlihy calculations.
#   03/04/14 kab: Updated to keep old version of tolerance index for fish as
#            tolindexFish()
#   03/05/14 tmk: Removed call to the require() function.
###############################################################################


if(getRversion() >= "3.0")
   utils::globalVariables(c("ALIENNIND", "ALIENNTAX", "ANOM_CT", "ANOMNIND",
      "bentTaxa", "CHIRNIND", "diam", "domN", "FINAL_CT", "first.group",
      "first.UID", "firstRow", "fishTaxa", "FLAG", "group", "HERP",
      "IS_DISTINCT", "isBig", "isNatural", "keep", "lastStation", "lDiam",
      "LINE", "lmax", "lmin", "LOC", "lowerPct", "lsub2dmm", "maxSizen",
      "medianSize", "METHOD", "METRIC", "NAT_TOTLNIND", "NAT_TOTLNTAX", "nCls",
      "nFam", "NON_NATIVE", "NON_TARGET", "nOrd", "nPhy", "nSta", "ORTHNIND",
      "parameter", "PARAMETER", "poolID", "PROTOCOL", "RESULT", "RESULT.fn",
      "RESULT.sa", "rp100", "SAMPID", "SAMPLE_TYPE", "sddepth", "sizen",
      "SLOPE", "SPECIES", "startLOC", "STATION", "SUMCT", "summarise",
      "TAXA_ID", "taxCat", "TOTLNIND", "TOTLNTAX", "TOTSUM", "TRANSDIR",
      "TRANSECT", "TRANSPC", "uid", "UID", "UNITS", "UNITS.SLOPE", "upperPct",
      "value", "variable", "x", "xbkf_h", "xbkf_w", "xdepth", "xfc_lwd",
      "xslope", "xwidth"))


# This function calculates % dominant organisms metric
# df <-- input data frame, containing UID as variable identifying unique samples
# topN <-- number specifying the top number of species to include in calculation

Dominance<-function(df, topN=1){
 rr <- subset(df,IS_DISTINCT==1)
 rr <- ddply(rr,"SAMPID",mutate,TOTSUM=sum(FINAL_CT))
  
	
	tt <- aggregate(list(domN=rr$FINAL_CT)
			,list(SAMPID=rr$SAMPID)
			,function(x){
				sum(x[order(x,decreasing=TRUE)[1:topN]]
				)
			}
	)
	uu <- merge(tt,unique(rr[,c('SAMPID','TOTSUM')]),by="SAMPID")
	uu <- mutate(uu,dompind=round(domN/TOTSUM*100,2))
	uu <- subset(uu,select=c('SAMPID','dompind'))
	uu <- plyr::rename(uu,c('dompind'=paste("DOM",topN,"PIND",sep='')))

	return(uu)
}


# Function calculates Shannon Diversity metric
# indata <-- data frame containing unique sample identifier (UID), IS_DISTINCT numeric variable indicating taxonomic distinctness as 0 or 1,
# 		and FINAL_CT as the count variable

ShanDiversity <- function(indata){
	rr <- subset(indata,IS_DISTINCT==1)
	rr <- ddply(rr,"SAMPID",mutate,TOTSUM=sum(FINAL_CT))
	tt <- ddply(rr,"SAMPID",summarise,HPRIME=round(-1*sum((FINAL_CT/TOTSUM)*(log(FINAL_CT/TOTSUM))),2))
#	sumrr <- with(rr,aggregate(x=list(TOTSUM=FINAL_CT),by=list(SAMPID=SAMPID),"sum"))
#	samps <- data.frame(UID=unique(indata[,c('SAMPID')]))
#	tt <- merge(rr,sumrr,by=c('SAMPID'),all.x=TRUE)
#	tt$PROP <- tt$FINAL_CT/tt$TOTSUM
#	tt$PROP1 <- tt$PROP*log(tt$PROP)
#	uu <- with(tt,aggregate(x=list(HPRIME=PROP1),by=list(SAMPID=SAMPID),"sum"))
#	uu$HPRIME <- (-1)*uu$HPRIME
#	uu$HPRIME <- round(uu$HPRIME,2)
#	outdata <- merge(samps,uu,by=c('SAMPID'),all.x=TRUE)
	
	return(tt)
}


# Function to calculate weighted tolerance value index (e.g., HBI) as the sum of the proportion of a taxon multiplied by the tolerance value
#		for that taxon. All taxa included in proportion calculations, not just those with PTVs.
# indata <-- input data frame with TAXA_ID, FINAL_CT, and UID variables
# taxalist <-- taxalist with TAXA_ID and tolerance values (TVs) as RESULT and PARAMETER as either PTV or TOL_VAL

tolindex<-function(indata, taxalist){
	tv_taxa <- taxalist[taxalist$TRAIT %in% c('PTV','TOL_VAL'),]
	indata1 <- ddply(indata,"SAMPID",mutate,SUMCT=sum(FINAL_CT))
	#This allows us to sum across only those taxa with TVs
	tv_cts <- merge(indata1,tv_taxa,by="TAXA_ID")
	outTV <- ddply(tv_cts,"SAMPID",summarise,WTD_TV=round(sum(FINAL_CT*as.numeric(value))/unique(SUMCT),2))
	return(outTV)
}


# Original version: Only taxa with TVs are included in the proportion
# calculations.
tolindexFish<-function(indata, taxalist){
	tv_taxa <- taxalist[,c('TAXA_ID','TOL_VAL')]
	# This allows us to sum across only those taxa with TVs
	tv_cts <- merge(indata,tv_taxa,by="TAXA_ID")
	outTV <- filter(tv_cts,!is.na(FINAL_CT) & !is.na(TOL_VAL)) %>%
    ddply("SAMPID", summarise,
	   WTD_TV=round(sum(FINAL_CT*as.numeric(TOL_VAL,na.rm=T))/sum(FINAL_CT),2))
	return(outTV)
}
