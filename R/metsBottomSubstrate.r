metsBottomSubstrate <- function(df) {

################################################################################
# Function: metsBottomSubstrate
# Title: Calculate NLA Bottom Substrate Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: September 19, 2008
# Description:
#   This function calculates the bottom substrate portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.
# Function Revisions:
#   09/19/08 cws: Changed comments for how modalClass() is used. Removed
#            nonmineral substrate covers from particle size means, stdevs and
#            percentile calculations.
#   10/07/08 cws: quantile calculations using type 3 algorithm to mimic SAS 
#            results, according to R documentation.
#   10/14/08 cws: Cover values normalized at each station/subid prior to
#            metrics calculation.
#   10/16/08 cws: Adding calculation bsiVariety, previously missed.
#   10/17/08 cws: changed to use type 2 quantile calculation method, as this
#            actually matches SAS output using proc univariate.
#   10/21/08 cws: Correcting counts of missing parameters.  These are now zero.
#   10/30/08 cws: Renamed bsiVariety to bsiStaVariety and added bsiSiteVariety.
#   11/05/08 cws: Correcting normalization of mineral substrate covers on which
#            the percentiles are based; previous percentiles were based on
#            normalized values that were mixed with earlier values that included
#            nonmineral covers.
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   02/27/09 cws: calculation of bsiSiteVariety corrected.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   11/22/13 cws: Temporarily upcased column names when calling normalizeCover()
#            and then lowcasing them to allow calculations.
#   12/24/13 cws: Created unit test using 2007 data.
#   12/26/13 cws: Changed input data column names, upcased metric names, changed
#            output from wide to long.
#   12/30/13 cws: Regression test to 2007 results shows 666 total differences,
#            642 of which are due to 57 sites with no Bottom Substrate data and
#            another 19 sites with Refactored.
#   01/21/13 cws: Completed unit test.  Regression test to 2007 results shows
#            1506 differences due to floating point issues, and 3530 rows in
#            2007 values which are due to absent data and thus not in the
#            current output (BSV*, BSF*, etc.) or counts (BSN*) that were set to
#            zero.
#   02/20/14 cws: Changing expected coding of substrate color and odor (BS_COLOR 
#            and BS_ODOR) from 2007 to 2012 standard.  Unit tests modified 
#            accordingly using code which can be used as an example when 
#            processing 2007 data. 
#   06/12/14 tmk: Removed calls to the require() function.
# Arguments:
#   df = a data frame containing bottom substrate data.  The data frame must
#     include columns that are named as follows:
#       UID - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single UID.
#               For NLA, transect is used for this purpose.
#       PARAMETER - parameter name, which identifies the variables used in
#                   calculations. In wide data frame format, this argument
#                   would be used to name columns.  It is assumed that this
#                   argument has the following values: BS_BEDROCK, BS_BOULDERS,
#                   BS_COBBLE, BS_GRAVEL, BS_SAND, BS_SILT, BS_ORGANIC, BS_WOOD,
#                   BS_COLOR, and BS_ODOR.
#       RESULT - parameter values, which are the values used in calculations.
# Output:
#   A data frame that contains the following columns:
#     UID - universal ID value
#     PARAMETER - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

  # Print initial messages
  cat('Bottom Substrate calculations:\n')
  intermediateMessage('Bottom substrate mets', loc='start')
  
  
  	bsData <- metsBottomSubstrate.setupForParticleCalculations(df)
  	intermediateMessage('.1')
	
  	indivPresence <- metsBottomSubstrate.indivPresence(bsData)
  	intermediateMessage('.2')
  	variety <- metsBottomSubstrate.variety(bsData)
  	intermediateMessage('.3')
  	indivCover <- metsBottomSubstrate.indivCover(bsData)
  	intermediateMessage('.4')
  	populationEstimates <- metsBottomSubstrate.populationEstimates(bsData)
  	intermediateMessage('.5')
  	modeCover <- metsBottomSubstrate.modeCover(indivPresence, indivCover)
  	intermediateMessage('.6')
	
	
  	indivColor <- metsBottomSubstrate.indivColor(df)
  	intermediateMessage('.7')
  	modeColor <- metsBottomSubstrate.modeColor(indivColor)
  	intermediateMessage('.8')
  
	
	indivOdor <- metsBottomSubstrate.indivOdor(df)
  	intermediateMessage('.9')
  	modeOdor <- metsBottomSubstrate.modeOdor(indivOdor)
  	intermediateMessage('.10')
  
	
	mets <- rbind(indivPresence, variety, indivCover, populationEstimates, modeCover
			   	 ,indivColor, modeColor, indivOdor, modeOdor
			   	 )
	intermediateMessage(' Done.', loc='end')
	
	return(mets)
}


metsBottomSubstrate.setupForParticleCalculations <- function(df)
# Prepares the input data for subsequent particle calculations, adding
# information for each substrate class.  
# Returns prepared data in a dataframe.
#
{
  	bsData <- subset(df
                  	,PARAMETER %in% c('BS_BEDROCK', 'BS_BOULDERS', 'BS_COBBLE'
                                   	 ,'BS_GRAVEL', 'BS_SAND', 'BS_SILT'
                                   	 ,'BS_ORGANIC', 'BS_WOOD'
                                   	 )
                  	)

    intermediateMessage('.a')

  	# Set up recodings for presence, cover and characteristic diameter (both
  	# actual and logged)
  	coverInfo<-data.frame(RESULT	=c(NA, '0', '1', '2', '3', '4')
                       	 ,cover		=c(NA, 0, 0.05, 0.25, 0.575, 0.875)
					   	 ,presence	=c(NA, 0, 1, 1, 1, 1)
                       	 ,stringsAsFactors=FALSE
                       	 )

  	tt<-data.frame(class=c('BS_BEDROCK',       'BS_BOULDERS'
                          ,'BS_COBBLE',        'BS_GRAVEL'
                          ,'BS_SAND',          'BS_SILT'
                          ,'BS_ORGANIC',       'BS_WOOD'
                          )
                  ,diam=c(gmean(c(4000,8000)), gmean(c(250,4000))
                       	 ,gmean(c(64,250)),    gmean(c(2,64))
                       	 ,gmean(c(0.06,2)),    gmean(c(0.001,0.06))
                       	 ,NA,                  NA
                       	 )
                  ,stringsAsFactors=FALSE
                  )

	diameters <- within(tt, lDiam <- log10(diam))
	
	preppedData <- merge(merge(bsData, coverInfo, by='RESULT', all.x=TRUE)
						,diameters
						,by.x='PARAMETER', by.y='class'
						,all.x=TRUE
						)

    intermediateMessage('.b')

	return(preppedData)	
}


metsBottomSubstrate.indivPresence <- function(bsPresence)
# Calculate fractional presence of each substrate class
{
  	tt <- aggregate(list(RESULT = bsPresence$presence)
                   ,list(UID=bsPresence$UID
                   		,PARAMETER=bsPresence$PARAMETER
                   		) 
                   ,mean, na.rm=TRUE
                   )
  	meanPresence <- within(tt, PARAMETER <- gsub('^BS_(.+)$', 'BSFP\\1', PARAMETER))

	return(meanPresence)
}


metsBottomSubstrate.variety <- function(bsData)	
# Calculate variety metrics: 
#     bsiStaVariety = mean number of substrate classes at each station.
#     bsiSiteVariety = total number of substrate classes present in site
# The number of substrate classes at a station is the sum of their presences.
# If any of these presences are missing, the sum will also be missing
{

	tt <- aggregate(bsData$presence
                   ,list(UID=bsData$UID
                      	,STATION=bsData$STATION
                      	)
                   ,sum, na.rm=TRUE
                   )
	bsiStaVariety <- aggregate(list(RESULT = tt$x)
                           	  ,list(UID=tt$UID)
                              ,mean, na.rm=TRUE
                           	  )
  	bsiStaVariety$PARAMETER <- 'BSISTAVARIETY'

	
	tt <- subset(bsData, presence %in% 1)
	bsiSiteVariety <- aggregate(list(RESULT = tt$PARAMETER)
							   ,list(UID=tt$UID)
					   		   ,function(x) {
								   rc <- count(unique(x))
								   return(rc)
							    }
							   )
	bsiSiteVariety$PARAMETER <- 'BSISITEVARIETY'
							   
	rc <- rbind(bsiStaVariety, bsiSiteVariety)
	
	return(rc)
}
  

metsBottomSubstrate.indivCover <- function(bsData)
# calculate mean, stdev and counts of normalized characteristic cover values 
# of each substrate class.
{
  	bscover <- normalizedCover(bsData, 'cover', 'normCover')
  
  	tt <- aggregate(list(RESULT = bscover$normCover)
                   ,list(UID=bscover$UID
                      	,PARAMETER=bscover$PARAMETER
                      	) 
                   ,mean, na.rm=TRUE
                   )
	meanCover <- within(tt, PARAMETER <- gsub('^BS_(.+)$', 'BSFC\\1', PARAMETER))
	intermediateMessage('.b')
	
	tt <- aggregate(list(RESULT = bscover$normCover)
                   ,list(UID=bscover$UID
                      	,PARAMETER=bscover$PARAMETER
                      	) 
                   ,sd, na.rm=TRUE
                   )
	sdCover <- within(tt, PARAMETER <- gsub('^BS_(.+)$', 'BSV\\1', PARAMETER))
  	intermediateMessage('.c')


  	tt <- aggregate(list(RESULT = bscover$normCover)
                   ,list(UID=bscover$UID
                      	,PARAMETER=bscover$PARAMETER
                      	) 
                   ,count
                   )
	nCover <- within(tt, PARAMETER <- gsub('^BS_(.+)$', 'BSN\\1', PARAMETER))
  	intermediateMessage('.d')

	rc <- rbind(meanCover, sdCover, nCover)
	return(rc)
}


metsBottomSubstrate.populationEstimates <- function(bsData)
# Calculations using characteristic diameters of the substrate are based
# on mean diameter*cover values at each transect.  Cover values are 
# normalized prior to their use as weights for these means.
#
{
	# Determine normalized coversof mineral substrates, and use those to weight
	# the characteristic diameters for determining diameter percentiles at each 
	# site.
	mineralCover <- subset(bsData
                          ,PARAMETER %in% c('BS_BEDROCK', 'BS_BOULDERS'
                                           ,'BS_COBBLE','BS_GRAVEL', 'BS_SAND'
                                           ,'BS_SILT'
                                           )
                          ,select=names(bsData)[names(bsData) != 'normCover']
                          )
  	mineralCover <- normalizedCover(mineralCover, 'cover', 'normCover')
	intermediateMessage('a')
	
	mineralCover$wtLDiam <- with(mineralCover, lDiam * normCover)
  	diamSubstrate <- aggregate(list(meanLDiam = mineralCover$wtLDiam)
                              ,list(UID=mineralCover$UID
                                   ,STATION=mineralCover$STATION
                                   ) 
                              ,mean, na.rm=TRUE
                              )
    intermediateMessage('b')


	# Estimate measures of logged diameter populations: mean, sd, percentiles.
	# Percentiles use the type 2 algorithm because it matches what was used
	# in SAS by default.
  	tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,mean, na.rm=TRUE
                   )
  	meanLDia <- within(tt, PARAMETER <- 'BSXLDIA')

    tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,sd, na.rm=TRUE
                   )
    sdLDia <- within(tt, PARAMETER <- 'BSVLDIA')

	intermediateMessage('c')
	
	tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                   )
    p16LDia <- within(tt, PARAMETER <- 'BS16LDIA')

    tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                   )
    p25LDia <- within(tt, PARAMETER <- 'BS25LDIA')

    tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                   )
    p50LDia <- within(tt, PARAMETER <- 'BS50LDIA')

    tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                   )
    p75LDia <- within(tt, PARAMETER <- 'BS75LDIA')

    tt <- aggregate(list(RESULT = diamSubstrate$meanLDiam)
                   ,list(UID=diamSubstrate$UID)
                   ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                   )
    p84LDia <- within(tt, PARAMETER <- 'BS84LDIA')

  	intermediateMessage('d')
	
	rc <- rbind(meanLDia, sdLDia, p16LDia, p25LDia, p50LDia, p75LDia, p84LDia)

	return(rc)
}


metsBottomSubstrate.modeCover <- function(presences, covers)
# Determine most common substrate class by presence and by cover
# (requires same site visits in meanPresence and meanCover and in same order)
#
# ARGUMENTS:
# presences	dataframe containing substrate fractional presences for each site,
#            with columns UID, PARAMETER, RESULT and PARAMETER is BSFPx.
# covers	dataframe containing substrate fractional presences for each site,
#            with columns UID, PARAMETER, RESULT and PARAMETER is BSFCx.
#
{
	# mode by presence
	tt <- within(subset(presences, grepl('^BSFP', PARAMETER))
				,PARAMETER <- gsub('^BSFP(.+)$', '\\1', PARAMETER)
				)
	tt <- within(tt, PARAMETER <- capitalize(tolower(PARAMETER)))	# TEMPORARY WHILE REFACTORING
	
	presMode <- modalClasses(tt, "PARAMETER", "RESULT")
	presMode <- within(presMode
					  ,{PARAMETER <- 'BSOPCLASS'
						RESULT <- modalClasses
						modalClasses <- NULL
					   }
					  )

		
	# mode by cover class
	tt <- within(subset(covers, grepl('^BSFC', PARAMETER))
				,PARAMETER <- gsub('^BSFC(.+)$', '\\1', PARAMETER)
				)
	tt <- within(tt, PARAMETER <- capitalize(tolower(PARAMETER)))	# TEMPORARY WHILE REFACTORING
	
	coverMode <- modalClasses(tt, "PARAMETER", "RESULT")
	coverMode <- within(coverMode
					   ,{PARAMETER <- 'BSOFCLASS'
						 RESULT <- modalClasses
						 modalClasses <- NULL
					 	}
					   )
	
	rc <- rbind(presMode, coverMode)
  	return(rc)
}


metsBottomSubstrate.indivColor <- function(df)
# Determine largest fractional presence of each substrate color and sample size.
{
	color <- subset(df, PARAMETER=='BS_COLOR' & 
					    grepl('(BLACK|BROWN|GRAY|OTHER.*|RED)', RESULT)
	               )
	if(nrow(color) == 0) {
		intermediateMessage(" IMPROPER CODING OF BS_COLOR! (You might be using 2007 values) ")
	}
	
	black <- aggregate(list(RESULT = color$RESULT=='BLACK')
    		       	  ,list(UID=color$UID)
                 	  ,mean, na.rm=TRUE
                 	  )
	black$PARAMETER <- 'BSFBLACK'

	brown <- aggregate(list(RESULT = color$RESULT=='BROWN')
                 	  ,list(UID=color$UID)
                 	  ,mean, na.rm=TRUE
                 	  )
	brown$PARAMETER <- 'BSFBROWN'
					  
  	grey <- aggregate(list(RESULT = color$RESULT=='GRAY')
                   	 ,list(UID=color$UID) 
                   	 ,mean, na.rm=TRUE
                   	 )
	grey$PARAMETER <- 'BSFGRAY'
					 
  	red <- aggregate(list(RESULT = color$RESULT=='RED')
                  	,list(UID=color$UID) 
                   	,mean, na.rm=TRUE
                  	)
	red$PARAMETER <- 'BSFRED'
					
  	other <- aggregate(list(RESULT = grepl('^OTHER_', color$RESULT))
                 	  ,list(UID=color$UID)
                 	  ,mean, na.rm=TRUE
                 	  )
	other$PARAMETER <- 'BSFOTHERCOLOR'
					  
  	count <- aggregate(list(RESULT = I(color$RESULT))
                 	  ,list(UID=color$UID)
                 	  ,count
                 	  )
	count$PARAMETER <- 'BSNCOLOR'
					  
  	rc <- rbind(black, brown, grey, red, other, count)
  	return(rc)
}


metsBottomSubstrate.modeColor <- function(df)
# Determine color mode (most common color(s)) at each site.  Returns
# dataframe with mode of substrate color, alphabetizing ties.
#
# ARGUMENTS
# df		dataframe with fractional presences of individual colors
#             (i.e. BSFBLACK, BSFGREY, etc.)
#
{
	indivFracs <- within(subset(df, PARAMETER != 'BSNCOLOR')
						,PARAMETER <- gsub('^BSF(.+)$', '\\1', PARAMETER)
						)
	colorMode <- modalClasses(indivFracs, "PARAMETER", "RESULT")	
	colorMode <- within(colorMode
					   ,{PARAMETER <- 'BSOCOLOR'
						 RESULT <- modalClasses
						 modalClasses <- NULL
						}
					   )

  	return(colorMode)
}


metsBottomSubstrate.indivOdor <- function(df)
# Determine fractional odor presences
{
	odor <- subset(df, PARAMETER=='ODOR' & 
					   grepl('^(ANOXIC|CHEMICAL|H2S|NONE|OTHER.*|OIL)$', RESULT)
				  )
	if(nrow(odor) == 0) {
		intermediateMessage(" IMPROPER CODING OF ODOR! (You might be using 2007 values) ")
    }
				   
  	tt <- aggregate(list(RESULT = odor$RESULT=='ANOXIC')
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
	anoxic <- within(tt, PARAMETER <- 'BSFANOXIC')

  	tt <- aggregate(list(RESULT = odor$RESULT=='CHEMICAL')
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
  	chem <- within(tt, PARAMETER <- 'BSFCHEMICAL')

  	tt <- aggregate(list(RESULT = odor$RESULT=='H2S')
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
    h2s <- within(tt, PARAMETER <- 'BSFH2S')

  	tt <- aggregate(list(RESULT = odor$RESULT=='NONE')
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
  	none <- within(tt, PARAMETER <- 'BSFNONEODOR')

  	tt <- aggregate(list(RESULT = grepl('^OTHER', odor$RESULT))
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
    other <- within(tt, PARAMETER <- 'BSFOTHERODOR')

    tt <- aggregate(list(RESULT = odor$RESULT=='OIL')
                   ,list(UID=odor$UID)
                   ,mean, na.rm=TRUE
                   )
    oil <- within(tt, PARAMETER <- 'BSFOIL')

    tt <- aggregate(list(RESULT = I(odor$RESULT))
                   ,list(UID=odor$UID)
                   ,count
                   )
  count <- within(tt, PARAMETER <- 'BSNODOR')

  rc <- rbind(anoxic, chem, h2s, none, other, oil, count)
  return(rc)
}


metsBottomSubstrate.modeOdor <- function(df)
# Determine most common substrate odor
{
	indivFracs <- within(subset(df, PARAMETER !='BSNODOR')
		  			  	,PARAMETER <- ifelse(PARAMETER == 'BSFNONEODOR', 'NONE'
					   			   	 ,ifelse(PARAMETER == 'BSFOTHERODOR', 'OTHER'
							  	  		  	,gsub('^BSF(.+)$', '\\1', PARAMETER)
					 			      ))
  					  	)
	tt <- modalClasses(indivFracs, "PARAMETER", "RESULT")	
	odorMode <- within(tt
					  ,{PARAMETER <- 'BSOODOR'
						RESULT <- modalClasses
						modalClasses <- NULL
					   }
					  )
					  
	return(odorMode)
}



# end of file
