metsRiparianVegetationNLA <- function(df, createSyntheticCovers=TRUE,
   fillinDrawdown=TRUE) {

################################################################################
# Function: metsRiparianVegetationNLA
# Title: Calculate NLA Riparian Zone and Vegetation Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 20, 2008
# Description:
#   This function calculates the riparian zone and vegetation portion of the
#   physical habitat metrics for National Lakes Assessment (NLA) data.  The
#   function requires a data frame containing validated physical habitat data
#   collected using the NLA protocol.  NOTE: composite indices use calc instead
#   of normCover values, which 2007 mets also do.  Check with Phil to see if
#   this is a bug.
# Function Revisions:
#   10/20/08 cws: Normalizing cover classes within each layer, and basing
#            cover metrics on those values.  Canopy and understory layers
#            are allowed to be less than 100% cover, so they are only 
#            normalized when they exceed 100%; ground cover is expected to
#            sum to exactly 100%.
#   10/21/08 cws: Correcting counts of missing parameters.  These are now zero.
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   07/18/13 cws: Adding unit test based on 2007 data, results assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).  The metrics function was changed
#            to set those counts to integer 0 within the function.
#   07/22/13 cws: Upcased column names, changed subid to STATION. Required
#            update of normalizedCover() definition.  Unit test works.
#   07/26/13 cws: Upcased metric names; filtering NA RESULT values from input
#            data;  returning metrics in long instead of wide organization. 
#   08/06/13 cws: Changed names of metrics to add _RIP suffix to riparian
#            metrics when createSyntheticCovers is TRUE
#   08/07/13 Changed _RIP suffix to _LIT.
#   08/13/13 cws: Standardized calculations to use coverSuffix as a by-variable. 
#   08/20/13 cws: Added unit test for calculations with drawdown data after
#            checking results against those obtained through SAS. Changed _LIT
#            suffix back to _RIP.
#   08/28/13 cws: Updating to provide maxDrawdown argument to calcSynCovers().
#   09/04/13 cws: Using DRAWDOWN parameter to fill in unrecorded drawdown covers
#            and HORIZ_DIST_DD values.  Added DRAWDOWN to unit test data.  Unit
#            test NOT YET modified accordingly, as Phil wants these values
#            quickly.
#   11/20/13 cws: Removed *SYN0 calculations. Normalized covers were calculated
#            with zones (rip, syn, dd) mixed before, this is now fixed.  Removed
#            mets for synthesized veg types (RVNCANOPY_SYN, RVNUNDERSTORY_SYN,
#            RVFPCANBROADLEAF_SYN, RVFPCANCONIFEROUS_SYN, RVFPCANDECIDUOUS_SYN,
#            RVFPCANMIXED_SYN, RVFPCANNONE_SYN, RVFPUNDBROADLEAF_SYN,
#            RVFPUNDCONIFEROUS_SYN, RVFPUNDDECIDUOUS_SYN, RVFPUNDMIXED_SYN,
#            RVFPUNDNONE_SYN) because they are undefined and are artifacts of
#            the expansion process used in metsRiparianVegetation.calculateMets
#            to include zeros for values that are absent due to lack of data.
#            Corrected expected values in unit tests using output of
#            metsRiparianVegetationTest.sas.
#   12/13/13 cws: Using protectedmean() and protectedSum() instead of anonymous
#            functions to avoid NaN results when all values are NA.
#   12/16/13 cws: Correcting unit test without drawdown fillin
#            (metsRiparianVegetationTest.withDrawDown) because of above changes.
#            These corrected values were replicated in SAS, which had previously
#            set missing intermediate values of stationSum to 0 to mimic the
#            behaviour protectSum() guards against.  Regression test with entire
#            2007 data passed with expected 570 differences in RVN* values (was
#            0, now NA when data is absent).
#   06/12/14 tmk: Removed calls to the require() function.
# Arguments:
#   df = a data frame containing riparian zone and vegetation data.  The data
#     frame must include columns that are named as follows:
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
#                   argument has the following values: CANOPY, C_BIGTREES, 
#                   C_SMALLTREES, UNDERSTORY, U_NONWOODY, U_WOODY, GC_BARE,
#                   GC_INUNDATED, GC_NONWOODY, GC_WOODY, HORIZ_DIST_DD,
#                   and DRAWDOWN.
#       RESULT - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
#   createSyntheticCovers = a logical value, which specifies whether to create
#     synthetic cover values as proportions of drawdown and riparian cover.
#     This argument should be set to FALSE when the data follows the 2007 NLA
#     protocol or do not contain drawdown cover data.  The default value is
#     TRUE.
#   fillinDrawdown = a logical value, which specifies whether to use the
#     DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
#     The default value is TRUE.
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
  cat('Riparian Zone and Vegetation calculations:\n')
  intermediateMessage('Riparian zone and vegetation mets', loc='start')

  	vegParams <- c('CANOPY', 'C_BIGTREES', 'C_SMALLTREES'
		  		  ,'UNDERSTORY', 'U_NONWOODY', 'U_WOODY'
		  		  ,'GC_BARE', 'GC_INUNDATED', 'GC_NONWOODY'
		  		  ,'GC_WOODY'
  				  ,'CANOPY_DD', 'C_BIGTREES_DD', 'C_SMALLTREES_DD'
				  ,'UNDERSTORY_DD', 'U_NONWOODY_DD', 'U_WOODY_DD'
				  ,'GC_BARE_DD', 'GC_INUNDATED_DD', 'GC_NONWOODY_DD'
				  ,'GC_WOODY_DD'
				  )
	typeParams <- c('CANOPY','CANOPY_DD','UNDERSTORY','UNDERSTORY_DD')
	coverParams <- setdiff(vegParams, typeParams)			  
	
	# Fill in unrecorded cover amounts and HORIZ_DIST_DD based on DRAWDOWN, but 
	# don't fill in cover types
	if(fillinDrawdown) {
		intermediateMessage('.fill')
		tt <- subset(df, PARAMETER %in% c(coverParams,'HORIZ_DIST_DD','DRAWDOWN'))
		tt <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
		dfStart <- rbind(tt, subset(df, PARAMETER %in% typeParams))
	} else {
		dfStart <- df
	}
	
	
  	# Create table for converting field values to calculation values
  	coverClassInfo<-data.frame(RESULT = c(NA,'0','1','2','3','4')
						  	  ,characteristicCover = c(NA,0,0.05,0.25,0.575,0.875)
							  ,presence = c(NA,0,1,1,1,1)
							  ,stringsAsFactors=FALSE
							  )
				
  	rvData <- subset(dfStart, PARAMETER %in% vegParams & !is.na(RESULT))

  	rvData <- merge(rvData, coverClassInfo, by='RESULT', all.x=TRUE)


  	# Synthesize 2007esque cover values from riparian and drawdown cover values
  	# if requested.  Values of HORIZ_DIST_DD are used for these calculations
  	if(createSyntheticCovers) {
		intermediateMessage('.synth')
		
	  	horizDist <- within(subset(dfStart, PARAMETER %in% 'HORIZ_DIST_DD' & !is.na(RESULT))
			  			   ,{characteristicCover <- NA
						     presence <- NA
			   			    }
	                       )


		synValues <- calcSynCovers(rbind(rvData, horizDist), 15, assumptions=FALSE)
								   
		# Make the resulting dataframe look like the data we're calculating metrics with.
		newValues <- within(synValues[c('UID','STATION','PARAMETER','RESULT','characteristicCover')]
						   ,{SAMPLE_TYPE<-'PHAB'
							 FORM_TYPE <- ''
							 FLAG <- as.character(NA)
							 presence <- ifelse(is.na(characteristicCover),	NA
										,ifelse(characteristicCover > 0, 	1, 	0
										 ))
							}
						   )
						   
	  	rvData <- rbind(rvData, newValues)
  	}
	
	intermediateMessage('.calcs')
	
	# Classify PARAMETERs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later on
	# For 2007, all locations will be riparian
	splitParams <- metsFishCover.splitParameterNames(rvData$PARAMETER)
	rvData <- within(rvData
					,{coverSuffix <- ifelse(splitParams$suffix=='', '_RIP', splitParams$suffix)
					  PARAMETER <- splitParams$base		#ifelse(splitParams$suffix == '', paste0(PARAMETER,'_RIP'), PARAMETER)
					 }
				  	)
#return(rvData)
#print(dcast(subset(rvData, UID==1000222), PARAMETER~coverSuffix+STATION, value.var='RESULT'))
#print(dcast(subset(rvData, UID==1000222), PARAMETER~coverSuffix+STATION, value.var='characteristicCover'))

	rvMets <- metsRiparianVegetation.calculateMets(rvData)
	
  	# combine results into a dataframe.  When not calculating synthetic cover values,
  	# rename the _RIP metrics back to 2007 names (no suffix). 
	rvMets <- within(rvMets
			  		,{PARAMETER <- paste0(PARAMETER, coverSuffix)
				  	  coverSuffix <- NULL
		  			 }
  			 		)
  
  	if(!createSyntheticCovers) {
		rvMets <- within(rvMets
			  			,PARAMETER <- ifelse(grepl('_RIP$', PARAMETER), substr(PARAMETER, 1, nchar(PARAMETER)-4), PARAMETER)
	  					)
  	}
  
  	return(rvMets)
}


metsRiparianVegetation.calculateMets <- function(rvData)
# Do all the calculationy work.  
#
{
  	intermediateMessage('.1')
	
	# Metrics about the canopy & understory types are currently undefined
	# for synthetic data, so these metrics make sense for drawdown and littoral 
	# data only.
	noSyn <- subset(rvData, coverSuffix %in% c('_DD','_RIP'))
  	canPresence <- metsRiparianVegetation.canopyTypePresence(noSyn)
  	intermediateMessage('.2')

  	undPresence <- metsRiparianVegetation.understoryTypePresence(noSyn)
  	intermediateMessage('.3')

	
	# Presences of individual layer components
  	componentPresence <- metsRiparianVegetation.componentPresence(rvData)
  	intermediateMessage('.4')


  	# Convert classes to values to fractional values for calculations.  
  	# Normalize fractional cover within each layer and zone(drawdown, riparian, synthetic).
	# Ground cover must add to exactly 100 percent cover since one of the 'layers' 
	# is bare ground.
	# Note that this code would be simpler if normalizedCover were modified to accept
	# grouping variables as an argument.
	ripData <- subset(rvData, coverSuffix == '_RIP')
	if(nrow(ripData) > 0) {
  		canFrac_RIP <- normalizedCover(subset(ripData, PARAMETER %in% c('C_BIGTREES','C_SMALLTREES'))
									  ,'characteristicCover', 'normCover'
    	                          	  ,allowTotalBelow100=TRUE
								  	  )
		undFrac_RIP <- normalizedCover(subset(ripData, PARAMETER %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
								  	  ,allowTotalBelow100=TRUE
								  	  )
		gndFrac_RIP <- normalizedCover(subset(ripData, PARAMETER %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
								  	  ,'characteristicCover', 'normCover'
								  	  )
							  
	} else {
		canFrac_RIP <- NULL
		undFrac_RIP <- NULL
		gndFrac_RIP <- NULL
	}

	ddData <- subset(rvData, coverSuffix == '_DD')
	if(nrow(ddData) > 0) {
		canFrac_DD <- normalizedCover(subset(ddData, PARAMETER %in% c('C_BIGTREES','C_SMALLTREES'))
									 ,'characteristicCover', 'normCover'
									 ,allowTotalBelow100=TRUE
									 )
		undFrac_DD <- normalizedCover(subset(ddData, PARAMETER %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		gndFrac_DD <- normalizedCover(subset(ddData, PARAMETER %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
									 ,'characteristicCover', 'normCover'
									 )
	} else {
		canFrac_DD <- NULL
		undFrac_DD <- NULL
		gndFrac_DD <- NULL
	}
	
	synData <- subset(rvData, coverSuffix == '_SYN')
	if(nrow(synData) > 0) {
		canFrac_SYN <- normalizedCover(subset(synData, PARAMETER %in% c('C_BIGTREES','C_SMALLTREES'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		undFrac_SYN <- normalizedCover(subset(synData, PARAMETER %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		gndFrac_SYN <- normalizedCover(subset(synData, PARAMETER %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
									  ,'characteristicCover', 'normCover'
									  )	
	} else {
		canFrac_SYN <- NULL
		undFrac_SYN <- NULL
		gndFrac_SYN <- NULL
	}
	
  
  	allFractions <- rbind(canFrac_RIP, canFrac_DD, canFrac_SYN
						 ,undFrac_RIP, undFrac_DD, undFrac_SYN
						 ,gndFrac_RIP, gndFrac_DD, gndFrac_SYN
						 )
  	rm(canFrac_RIP, canFrac_DD, canFrac_SYN
	  ,undFrac_RIP, undFrac_DD, undFrac_SYN
	  ,gndFrac_RIP, gndFrac_DD, gndFrac_SYN
	  )

#	allFractions <- within(subset(rvData, PARAMETER %nin% c('CANOPY','UNDERSTORY')), normCover <- characteristicCover)	########## NO NORMALIZATION #############################
  	intermediateMessage('.5')
  
  	componentCovers <- metsRiparianVegetation.componentCovers(allFractions)
  	intermediateMessage('.6')

	
	tt <- subset(allFractions, PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES'))
	iCan <- metsRiparianVegetation.compositeIndex(tt, 'RVICANOPY')
	
	tt <- subset(allFractions, PARAMETER %in% c('U_NONWOODY', 'U_WOODY'))
	iUnd <- metsRiparianVegetation.compositeIndex(tt, 'RVIUNDERSTORY')
	
	tt <- subset(allFractions, PARAMETER %in% c('GC_INUNDATED', 'GC_NONWOODY', 'GC_WOODY'))
	iGnd <- metsRiparianVegetation.compositeIndex(tt, 'RVIGROUND')
	
	tt <- subset(allFractions, PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_WOODY'))
	iTallw <- metsRiparianVegetation.compositeIndex(tt, 'RVITALLWOOD')
	
	tt <- subset(allFractions, PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_WOODY', 'GC_WOODY'))
	iWoody <- metsRiparianVegetation.compositeIndex(tt, 'RVIWOODY')
	
	tt <- subset(allFractions, PARAMETER %in% c('U_NONWOODY', 'GC_NONWOODY'))
	iHerbs <- metsRiparianVegetation.compositeIndex(tt, 'RVIHERBS')
	
	tt <- subset(allFractions, PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_NONWOODY', 'U_WOODY'))
	iCanUnd <- metsRiparianVegetation.compositeIndex(tt, 'RVICANUND')
	
	tt <- subset(allFractions
				,PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES'
								 ,'U_NONWOODY', 'U_WOODY'
								 ,'GC_NONWOODY', 'GC_WOODY'
								 )
				)
	itotVeg <- metsRiparianVegetation.compositeIndex(tt, 'RVITOTALVEG')
	intermediateMessage('.7')
	

  	# Collect all metrics calculations into single dataframe, expand the results
  	# so sites without data for metrics have those metrics with NA results,
  	# and make NA counts zero.  The results of this expansion should not include 
	# synthetic cover types, which don't/shouldn't exist.
  	rv <- rbind(canPresence, undPresence, componentPresence, componentCovers
			   ,iCan, iUnd, iGnd, iTallw, iWoody, iHerbs, iCanUnd, itotVeg
			   )
  	rv <- within(expand.data.frame(rv, c('UID','PARAMETER','coverSuffix'))
                ,RESULT <- ifelse(grepl('^RVN.+', PARAMETER) & is.na(RESULT), 0, RESULT)
	            )
	rv <- subset(rv
				,!(PARAMETER %in% c('RVNUNDERSTORY','RVNCANOPY'
								   ,"RVFPCANBROADLEAF", "RVFPCANCONIFEROUS"
								   ,"RVFPCANDECIDUOUS", "RVFPCANMIXED"
								   ,"RVFPCANNONE", "RVFPUNDBROADLEAF"
								   ,"RVFPUNDCONIFEROUS", "RVFPUNDDECIDUOUS"
								   ,"RVFPUNDMIXED", "RVFPUNDNONE"
								   ) 
				   & coverSuffix=='_SYN'
				  )
		        )
	
  	intermediateMessage(' Done.', loc='end')

  	return(rv)
}


metsRiparianVegetation.canopyTypePresence <- function(rvData)
# Calculate mean presence (ranges 0-1) of each canopy type
{
	cantype <- subset(rvData, PARAMETER=='CANOPY')

	tt <- aggregate(list(RESULT = cantype$RESULT=='B')
				   ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canB <- within(tt, PARAMETER <- 'RVFPCANBROADLEAF')
	
	tt <- aggregate(list(RESULT = cantype$RESULT=='C')
			       ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canC <- within(tt, PARAMETER <- 'RVFPCANCONIFEROUS')
	
	tt <- aggregate(list(RESULT = cantype$RESULT=='D')
				   ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canD <- within(tt, PARAMETER <- 'RVFPCANDECIDUOUS')
	
	tt <- aggregate(list(RESULT = cantype$RESULT=='M')
				   ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canM <- within(tt, PARAMETER <- 'RVFPCANMIXED')
	
	tt <- aggregate(list(RESULT = cantype$RESULT=='N')
				   ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canN <- within(tt, PARAMETER <- 'RVFPCANNONE')
	
	tt <- aggregate(list(RESULT = I(cantype$RESULT))
				   ,list('UID'=cantype$UID, coverSuffix = cantype$coverSuffix)
				   ,count
				   )
	canCount <- within(tt
					  ,{PARAMETER <- 'RVNCANOPY'
						RESULT <- ifelse(is.na(RESULT), 0L, RESULT)
					   }
			          )
	
	rc <- rbind(canB, canC, canD, canM, canN, canCount)
	return(rc)
}


metsRiparianVegetation.understoryTypePresence <- function(rvData)
# Calculate mean presence (ranges 0-1) of each understory type
{
	undtype <- subset(rvData, PARAMETER=='UNDERSTORY')
	tt <- aggregate(list(RESULT = undtype$RESULT=='B')
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undB <- within(tt, PARAMETER <- 'RVFPUNDBROADLEAF')
	
	tt <- aggregate(list(RESULT = undtype$RESULT=='C')
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undC <- within(tt, PARAMETER <- 'RVFPUNDCONIFEROUS')
	
	tt <- aggregate(list(RESULT = undtype$RESULT=='D')
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undD <- within(tt, PARAMETER <- 'RVFPUNDDECIDUOUS')
	
	tt <- aggregate(list(RESULT = undtype$RESULT=='M')
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
		   		   )
	undM <- within(tt, PARAMETER <- 'RVFPUNDMIXED')
	
	tt <- aggregate(list(RESULT = undtype$RESULT=='N')
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
		   		   )
	undN <- within(tt, PARAMETER <- 'RVFPUNDNONE')
	
	tt <- aggregate(list(RESULT = I(undtype$RESULT))
				   ,list('UID'=undtype$UID, coverSuffix = undtype$coverSuffix)
				   ,count
				   )
	undCount <- within(tt, {PARAMETER <- 'RVNUNDERSTORY'; RESULT <- ifelse(is.na(RESULT), 0L, RESULT)})
	
	rc <- rbind(undB, undC, undD, undM, undN, undCount)
	return(rc)
}


metsRiparianVegetation.componentPresence <- function(rvData)
# Calculate mean presences for all layer components 
{
	paData <- subset(rvData, PARAMETER %in% c('C_BIGTREES', 'C_SMALLTREES'
											 ,'U_NONWOODY', 'U_WOODY'
											 ,'GC_BARE', 'GC_INUNDATED'
											 ,'GC_NONWOODY', 'GC_WOODY'
											 )
					)
	
	tt <- aggregate(list(RESULT = paData$presence)
				   ,list('UID'=paData$UID, 'PARAMETER'=paData$PARAMETER, coverSuffix = paData$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
				   
	paMets <- within(tt, PARAMETER <- ifelse(PARAMETER == 'C_BIGTREES',		'RVFPCANBIG'
									 ,ifelse(PARAMETER == 'C_SMALLTREES', 	'RVFPCANSMALL'
									 ,ifelse(PARAMETER == 'U_NONWOODY', 	'RVFPUNDNONW'
									 ,ifelse(PARAMETER == 'U_WOODY', 		'RVFPUNDWOODY'
									 ,ifelse(PARAMETER == 'GC_BARE', 		'RVFPGNDBARE'
									 ,ifelse(PARAMETER == 'GC_INUNDATED', 	'RVFPGNDINUNDATED'
									 ,ifelse(PARAMETER == 'GC_NONWOODY',  	'RVFPGNDNONW'
									 ,ifelse(PARAMETER == 'GC_WOODY', 		'RVFPGNDWOODY'
																		   ,'UnkRVPAClass'
									  ))))))))
	)

	return(paMets)
}


metsRiparianVegetation.componentCovers <- function(allFractions)
# Calculate cover mean, stdev and count for individual components
{
	# Means
	tt <- aggregate(list(RESULT = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("UID" = allFractions$UID
				   		,"PARAMETER" = allFractions$PARAMETER
						,coverSuffix = allFractions$coverSuffix
				   		)
			  	   ,protectedMean, na.rm=TRUE
				   )
	meanCover <- within(tt, PARAMETER <- ifelse(PARAMETER == 'C_BIGTREES',	'RVFCCANBIG'
										,ifelse(PARAMETER == 'C_SMALLTREES','RVFCCANSMALL'
										,ifelse(PARAMETER == 'U_NONWOODY', 	'RVFCUNDNONW'
										,ifelse(PARAMETER == 'U_WOODY', 	'RVFCUNDWOODY'
										,ifelse(PARAMETER == 'GC_BARE', 	'RVFCGNDBARE'
										,ifelse(PARAMETER == 'GC_INUNDATED','RVFCGNDINUNDATED'
										,ifelse(PARAMETER == 'GC_NONWOODY', 'RVFCGNDNONW'
										,ifelse(PARAMETER == 'GC_WOODY', 	'RVFCGNDWOODY'
																			,'UnkRVCoverClass'
										 ))))))))
	)
	
	
	# Stdev
	tt <- aggregate(list(RESULT = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("UID"=allFractions$UID
				   		,"PARAMETER"=allFractions$PARAMETER
						,coverSuffix = allFractions$coverSuffix
						)
				   ,sd, na.rm=TRUE
				   )
	sdCover <- within(tt, PARAMETER <- ifelse(PARAMETER == 'C_BIGTREES',	'RVVCANBIG'
									  ,ifelse(PARAMETER == 'C_SMALLTREES',	'RVVCANSMALL'
									  ,ifelse(PARAMETER == 'U_NONWOODY', 	'RVVUNDNONW'
									  ,ifelse(PARAMETER == 'U_WOODY', 		'RVVUNDWOODY'
									  ,ifelse(PARAMETER == 'GC_BARE', 		'RVVGNDBARE'
									  ,ifelse(PARAMETER == 'GC_INUNDATED',	'RVVGNDINUNDATED'
									  ,ifelse(PARAMETER == 'GC_NONWOODY', 	'RVVGNDNONW'
									  ,ifelse(PARAMETER == 'GC_WOODY', 		'RVVGNDWOODY'
																		   ,'UnkRVSDClass'
									   ))))))))
	)

	
	# Counts. 
	# Expand data so that all parameters occur in all UID, and thus counts (of 0) 
	# will exist for all UID.  This could be done above when creating allFractions 
	# without introduction of error, but it is done here to localize the change. 
	tt <- aggregate(list(RESULT = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("UID"=allFractions$UID
						,"PARAMETER"=allFractions$PARAMETER
						,coverSuffix = allFractions$coverSuffix
						)
				   ,count
				   )
	countCover <- within(tt
						,{PARAMETER <- ifelse(PARAMETER == 'C_BIGTREES',	'RVNCANBIG'
									  ,ifelse(PARAMETER == 'C_SMALLTREES',	'RVNCANSMALL'
									  ,ifelse(PARAMETER == 'U_NONWOODY', 	'RVNUNDNONW'
									  ,ifelse(PARAMETER == 'U_WOODY', 		'RVNUNDWOODY'
									  ,ifelse(PARAMETER == 'GC_BARE', 		'RVNGNDBARE'
									  ,ifelse(PARAMETER == 'GC_INUNDATED',	'RVNGNDINUNDATED'
									  ,ifelse(PARAMETER == 'GC_NONWOODY', 	'RVNGNDNONW'
									  ,ifelse(PARAMETER == 'GC_WOODY', 		'RVNGNDWOODY'
																		   ,'UnkRVNClass'
									   ))))))))
						  RESULT <- ifelse(is.na(RESULT), 0L, RESULT)
						 }
						)
#print("metsRiparianVegetation.componentCovers allFractions:"); print(dcast(subset(allFractions, UID==1000222), coverSuffix+PARAMETER~STATION, value.var='normCover'))
#print("metsRiparianVegetation.componentCovers tt:"); print(dcast(subset(tt, UID==1000222), PARAMETER~coverSuffix, value.var='RESULT'))
#print(str(allFractions)); print(str(tt))
						
						
	rc <- rbind(meanCover, sdCover, countCover)
	return(rc)
}


metsRiparianVegetation.compositeIndex <- function(groupData, indexName)
# Composite vegetation indicex.  These are based on the sum of normalized
# cover values for selected cover types at a station/subid, and the mean
# for the entire site as the resulting metric. 
{
#print('')
#print(subset(groupData, UID==6665 & coverSuffix=='_SYN'))
	tt <- aggregate(groupData$characteristicCover
				   ,list('UID'=groupData$UID
		                ,'STATION'=groupData$STATION
						,coverSuffix = groupData$coverSuffix
						)
				   ,protectedSum, na.rm=TRUE
				   )
#print(subset(tt, UID==6665))
				   
	qq <- aggregate(list(RESULT = tt$x)
				   ,list('UID'=tt$UID, coverSuffix = tt$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	rc <- within(qq, PARAMETER <- indexName)
#print(subset(rc, UID==6665))
	
	return(rc)
}



# end of file
