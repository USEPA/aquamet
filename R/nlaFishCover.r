metsFishCoverNLA <- function(df, createSyntheticCovers=TRUE,
   fillinDrawdown=TRUE) {

################################################################################
# Function: metsFishCoverNLA
# Title: Calculate NLA Fish Cover Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 7, 2008
# Description:
#   This function calculates the fish cover portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.  NOTE: These calculations do not currently return
#   every metric for every site, as this did not occur for 2007.  Using the test
#   data for the 2007 unit test shows that site 7545 is lacking
#   FC[FC|FP|V][BRUSH|SNAGS] and site has2NAvalues is lacking another 41
#   metrics.
# Function Revisions:
#   10/07/08 cws: Added fcfpAll and fcnAll.
#   10/09/08 cws: Cover values normalized at each station/subid prior to
#            metrics calculation.
#   10/20/08 cws: Removed normalization, as individual FC categories are 
#            independent.
#   10/21/08 cws: Corrected counts of missing parameters.  These are now zero.
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   07/18/13 cws: Adding unit test based on 2007 data, results assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).  The metrics function was changed
#            to set those counts to integer 0 within the function.
#   07/22/13 cws: Improved handling NA data values by reordering if/else if/else 
#            testing to check for NA values before others. Unit test still
#            passes.  Modifying unit test data to have a site with an NA value.
#            Upcasing column names, renamed subid to STATION, changed FC_SNAG to
#            FC_SNAGS.
#   07/24/13 cws: Changed output from wide to long; removing rows with NA values
#            that were artifacts of the former wide organization (uid 7545 has
#            no BRUSH or SNAGS data, so FC[FC|FP|V][BRUSH|SNAGS] rows were
#            removed from the expected values in the unit test). Subsetting
#            initial input to remove NA results. 
#   07/31/13 cws: Including drawdown cover values and synthesized cover
#            calculations.  Added argument createSyntheticCovers to specify
#            whether synthetic covers would be calculated.  Unit tests added.
#   08/02/13 cws: Extended calculations to include 'synthetic' covers.  Unit
#            test based on SAS calculation results.
#   08/06/13 cws: Changed names of metrics to add _LIT suffix to riparian
#            metrics when createSyntheticCovers is TRUE, e.g. FCIALL became
#            FCIALL_LIT. Unit test updated.
#   08/07/13 Changed _RIP suffix to _LIT.
#   08/08/13 Updated unit test to reflect changes in calcSynCovers.  This
#            affected 8 values at UID=7263, which has no HORIZ_DIST_DD.
#   08/12/13 Updated unit test to reflect 9 August changes to calcSynCovers.
#            Moved parameter suffix determination, done twice in this function,
#            to metsFishCover.splitParameterNames(). New *_SYN calculations,
#            which rely on assumptions about the data are included BUT NOT YET
#            ADDED TO THE UNIT TEST SINCE THAT'S TIME CONSUMING.  The values
#            look correct based on hand calculations of FC[FP|N|V]_AQUATIC_SYN
#            for UID 6160, 6189, 6227.
#   08/13/13 cws: Standardized calculations to use coverSuffix as a by-variable.
#            Notifying user of presence/lack of *_SYN metrics in unit test.
#   08/26/13 cws: Modified to use 10m as maximal drawdown into littoral region
#            instead of 15.  Using expand.data.frame() so that each metric
#            occurs for each site.
#   09/16/13 cws: Changing _SYN metrics to _SIM, as they are simulations of
#            littoral cover rather than synthesized 2007-esque cover values.
#            Using DRAWDOWN parameter to fill in unrecorded drawdown covers and
#            HORIZ_DIST_DD values.  Unit test NOT YET modified accordingly, as
#            Phil wants these values quickly.
#   09/26/13 cws: Added DRAWDOWN fillin to unit test. Based on
#            projects/metsFishCoverTest.sas which gets most things right but
#            includes some extra _LIT* values which are NA and absent from R,
#            and miscalculates 15 FCN*_DD counts in 3 UID, so the SAS
#          results were corrected.
#   11/06/13 cws: Now including checks of *_SIM values in unit test when filling
#            in values based on DRAWDOWN (using fillinDrawdownData()).  Still
#            removing *_SIM values in unit test when NOT filling in values based
#            on DRAWDOWN.
#   11/12/13 cws: Updated unit test counts after correcting fillinDrawdownData()
#            on this date.  Addressing calculations with simulated cover values
#            when NOT filling in DD values.  Using anonymous functions (like in
#            HI mets) in place of mean() to correctly handle cases where all
#            data is NA.
#   11/14/13 cws: Corrected synthetic calculations when drawdown values are not
#            'filled in', and updated unit test accordingly. Regression test
#            against 2007 values shows zero differences > 1e-13 and 522
#            differences due to 2007 counts being 0 and 2012 counts (FCN*) being
#            NA. These are for 58 UIDs which have no FC data and hence no way
#            inside this function to set those values.
#   12/16/13 cws: Using protectedMean and protectedSum functions in place of
#            anonymous functions.  Regression test with entire 2007 data passed 
#            with expected 522 differences in FCN* values (was 0, now NA when 
#            data is absent).
#   06/12/14 tmk: Removed calls to the require() function.
# Arguments:
#   df = a data frame containing fish cover data.  The data frame must include
#     columns that are named as follows:
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
#                   argument has the following values: FC_AQUATIC, FC_BOULDERS, 
#                   FC_BRUSH, FC_LEDGES, FC_TREES, FC_OVERHANG, FC_SNAGS,
#                   and FC_STRUCTURES.  Each fish cover class occurs at least
#                   once in the input dataframe (this is because of how NA
#                   counts are set to 0 at the end of the function.
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
    cat('Fish Cover calculations:\n')
    intermediateMessage('Fish Cover mets', loc='start')

	# List expected cover type parameters.  HORIZ_DIST_DD is also expected when drawdown
	# data is included.
	coverClasses <- c('FC_AQUATIC', 'FC_BOULDERS','FC_BRUSH', 'FC_LEDGES'
				  	 ,'FC_LIVETREES', 'FC_OVERHANG','FC_SNAGS', 'FC_STRUCTURES'
				  	 ,'FC_AQUATIC_DD', 'FC_BOULDERS_DD','FC_BRUSH_DD', 'FC_LEDGES_DD'
				  	 ,'FC_LIVETREES_DD', 'FC_OVERHANG_DD','FC_SNAGS_DD', 'FC_STRUCTURES_DD'
				  	 )
	
    # Create table for converting field values to calculation values
	coverClassInfo<-data.frame(field = c(NA,'0','1','2','3','4')
						 	  ,characteristicCover = c(NA,0,0.05,0.25,0.575,0.875)
						 	  ,presence = c(NA,0,1,1,1,1)
						 	  ,stringsAsFactors=FALSE
						 	  )
	
    intermediateMessage('.1')

	
	# Fill in unrecorded cover and HORIZ_DIST_DD based on DRAWDOWN.
	if(fillinDrawdown) {
		intermediateMessage('.fill')
		tt <- subset(df, PARAMETER %in% c(coverClasses,'HORIZ_DIST_DD','DRAWDOWN'))
		df <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
	} else {
		df <- df
	}

	
	# Filter out any unnecessary data
	fcData <- subset(df, PARAMETER %in% coverClasses & !is.na(RESULT))
	hdData <- subset(df, PARAMETER %in% 'HORIZ_DIST_DD' & !is.na(RESULT))
	
	
	# Assign numeric values for cover classes and assign cover types to
	# specific groups.
	fcDataCalcs <- merge(fcData
					    ,coverClassInfo
						,by.x='RESULT'
						,by.y='field'
						,all.x=TRUE
						,sort=FALSE
					  	)
						
	
	# Simulate cover values from riparian and drawdown cover values if requested. 
	# Values of HORIZ_DIST_DD are used for these calculations
	if(createSyntheticCovers) {
		
		horizDist <- within(subset(hdData, PARAMETER %in% 'HORIZ_DIST_DD' & !is.na(RESULT))
						   ,{characteristicCover <- NA
							 presence <- NA
						    }
				           )
						   
		synValues <- calcSynCovers(rbind(fcDataCalcs,horizDist), 10, assumptions=FALSE)
		synValues$PARAMETER <- gsub('(_SYN)$','_SIM', synValues$PARAMETER)
								
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
							
		fcDataCalcs <- rbind(fcDataCalcs, newValues)
	}				
	

	# Classify PARAMETERs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later on
	# For 2007, all locations will be riparian
	splitParams <- metsFishCover.splitParameterNames(fcDataCalcs$PARAMETER)
	fcDataCalcs <- within(fcDataCalcs
						 ,{coverSuffix <- ifelse(splitParams$suffix=='', '_LIT', splitParams$suffix)
						   PARAMETER <- splitParams$base
						  }
				         )
						 
	intermediateMessage('.a')

	
	fcMets <- metsFishCover.calculateMets(fcDataCalcs)

	
	# When not calculating synthetic cover values, rename the _LIT metrics back to 2007 names (no suffix). 	
	fcMets <- within(fcMets
					,{PARAMETER <- paste0(PARAMETER, coverSuffix)
					  coverSuffix <- NULL
					 }
					)
	
	if(!createSyntheticCovers) {
		fcMets <- within(fcMets
						,PARAMETER <- ifelse(grepl('_LIT$', PARAMETER), substr(PARAMETER, 1, nchar(PARAMETER)-4), PARAMETER)
						)
	}
	
	intermediateMessage(' Done.', loc='end')
	
	return(fcMets)
}


metsFishCover.calculateMets <- function(fcDataCalcs)
#
{
	# Calculate mets for individual cover types - counts, means, presence, sd
	indivCovers <- metsFishCover.indivCovers(fcDataCalcs)

#	fcMeans <- subset(indivCovers, grepl('^FCFC', PARAMETER))

	fcIndices <- metsFishCover.groupIndices(fcDataCalcs)
  	intermediateMessage('.6')
	
    
    # Determine fish cover presence at each station, then calculate mean
    # presence and count for site.
    pp <- aggregate(list(any=fcDataCalcs$characteristicCover)
                   ,list(UID = fcDataCalcs$UID
                        ,STATION = fcDataCalcs$STATION
                        ,PARAMETER = fcDataCalcs$PARAMETER
						,coverSuffix = fcDataCalcs$coverSuffix
                        )
                   ,function(x) { (if(is.na(x)) return(NA)		# recode cover to P/A
								   else if(x==0) return(0) 
                                   else return(1)
                                  )
                                }
                   )
				   
	fcExists <- aggregate(list(fcExists = pp$any)
                   		 ,list(UID = pp$UID
                         	  ,STATION = pp$STATION
							  ,coverSuffix = pp$coverSuffix
							  )
                   		 ,function(x) { return(any(x>0)) }
                   		 )
						 

	meanAll <- aggregate(list(RESULT = fcExists$fcExists)
                   		,list(UID = fcExists$UID
							 ,coverSuffix = fcExists$coverSuffix
							 )
                   		,protectedMean, na.rm=TRUE
                   		)
	meanAll$PARAMETER <- 'FCFPALL'


    nAll <- aggregate(list(RESULT = fcExists$fcExists)
                   	 ,list(UID = fcExists$UID, coverSuffix = fcExists$coverSuffix)
                     ,count
                     )
	nAll$PARAMETER <- 'FCNALL'
	
    intermediateMessage('.7')


    # combine results into a dataframe.  
	all <- rbind(indivCovers, fcIndices, meanAll, nAll)

    return(all)
}


metsFishCover.splitParameterNames <- function(parameters)
# Splits the PARAMETER value into the base and suffix portions,
# e.g. 'FC_BOULDERS_DD' becomes 'FC_BOULDERS' and '_DD'.  Returns
# named list of two character vectors, base and suffix with
# these values, in the same order as the input vector
#
# ARGUMENTS:
# parameters	character vector of parameter names
#
{
	rxExpectedSuffices <- '(_DD|_LIT|_SYN|_SIM|_SYN0)'

	base <- ifelse(grepl(paste0('^(.+)', rxExpectedSuffices, '$'), parameters)
			 	  ,gsub(paste0('^(.+)', rxExpectedSuffices, '$'), '\\1', parameters)
				  ,parameters 
				  )
	suffix <- ifelse(grepl(paste0('^(.+)', rxExpectedSuffices, '$'), parameters)
				    ,gsub(paste0('^(.+)', rxExpectedSuffices, '$'), '\\2', parameters)
					,''
					)

	rc <- list(base=base, suffix=suffix)
	
	return(rc)
}


metsFishCover.indivCovers <- function(fcDataCalcs)
# Calculate simple mets for individual cover types
# ARGUMENTS:
# fcDataCalcs	dataframe with fish cover data that is augmented with 
#				  characteristic cover values and presence values
#
{
	# Calculate presence mean of each type of fish cover
	# Convert field results to calculable values
	intermediateMessage('.indivCovers')
	tt<-aggregate(list(RESULT = fcDataCalcs$presence)
				 ,list(UID = fcDataCalcs$UID
					  ,PARAMETER = fcDataCalcs$PARAMETER
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,protectedMean, na.rm=TRUE
				 )
#	allFP <- expand.data.frame(tt, c('UID','PARAMETER','coverSuffix'))
	meanPresence <- within(tt, PARAMETER <- gsub('^FC_(.+)$', 'FCFP\\1', PARAMETER))
	intermediateMessage('.2')
#print(dcast(subset(fcDataCalcs, UID==7263), STATION+coverSuffix~PARAMETER, value.var='characteristicCover'))	
#print(dcast(subset(meanPresence, UID==7263), PARAMETER~coverSuffix, value.var='RESULT'))	
#print(subset(meanPresence, UID==7263))	
	
	
	# Calculate cover counts of each type of fish cover
	tt<-aggregate(list(RESULT = fcDataCalcs$characteristicCover)
				 ,list(UID = fcDataCalcs$UID
					  ,PARAMETER = fcDataCalcs$PARAMETER
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,count
				 )
	allCounts <- expand.data.frame(tt, c('UID','PARAMETER','coverSuffix'))
	nCover <- within(allCounts
					,{PARAMETER <- gsub('^FC_(.+)$', 'FCN\\1', PARAMETER)
					  RESULT <- ifelse(is.na(RESULT), 0L, RESULT)
					 }
					)
	intermediateMessage('.3')
	
	
	# Calculate cover standard deviation of each type of fish cover
	tt<-aggregate(list(RESULT = fcDataCalcs$characteristicCover)
				 ,list(UID = fcDataCalcs$UID
					  ,PARAMETER = fcDataCalcs$PARAMETER
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,sd, na.rm=TRUE
				 )
#	allV <- expand.data.frame(tt, c('UID','PARAMETER','coverSuffix'))
	sdCover <- within(tt,{PARAMETER <- gsub('^FC_(.+)$', 'FCV\\1', PARAMETER)})
	intermediateMessage('.4')
	
	
	# Calculate cover mean of each type of fish cover
	# Convert field results to calculable values.
	fcMeans<-aggregate(list(RESULT = fcDataCalcs$characteristicCover)
					  ,list(UID = fcDataCalcs$UID
						   ,PARAMETER = fcDataCalcs$PARAMETER
						   ,coverSuffix = fcDataCalcs$coverSuffix
					  	   )
					  ,protectedMean, na.rm=TRUE
					  )
#	allFC <- expand.data.frame(fcMeans, c('UID','PARAMETER','coverSuffix'))
	meanCover <- within(fcMeans, PARAMETER <- gsub('^FC_(.+)$', 'FCFC\\1', PARAMETER))
	
	intermediateMessage('.5')
	
	rc <- rbind(meanPresence, nCover, sdCover, meanCover)
	return(rc)
}


metsFishCover.groupIndices <- function(fcDataCalcs)
# Calculate cover indices: total, large, natural and riparian vegetation
# grouped cover 
#
# ARGUMENTS:
# fcMeans	dataframe with mean cover for each cover type at a site, with columns 
#             UID, PARAMETER, RESULT, coverSuffix.
{

	fcMeans<-aggregate(list(RESULT = fcDataCalcs$characteristicCover)
					  ,list(UID = fcDataCalcs$UID
						   ,PARAMETER = fcDataCalcs$PARAMETER
						   ,coverSuffix = fcDataCalcs$coverSuffix
						   )
					  ,protectedMean, na.rm=TRUE
					  )
	
	fcMeans<-within(fcMeans
				   ,{isBig <- grepl('^FC_(BOULDERS|LEDGES|OVERHANG|STRUCTURES)', PARAMETER) 
					 isNatural <- !grepl('^FC_STRUCTURES', PARAMETER)
					 isRipVegetation <-grepl('^FC_(BRUSH|LIVETREES|SNAGS)', PARAMETER)
				    }
				   )
				   
	fciAll <- aggregate(list(RESULT = fcMeans$RESULT)
					   ,list(UID = fcMeans$UID
			                ,coverSuffix = fcMeans$coverSuffix
					        )
					   ,protectedSum, na.rm=TRUE
					   )
	fciAll$PARAMETER <- 'FCIALL'
#print(dcast(subset(fcDataCalcs, UID==1000057), STATION+coverSuffix~PARAMETER, value.var='characteristicCover'))	
#print(dcast(subset(fcMeans, UID==1000057), PARAMETER~coverSuffix, value.var='RESULT'))	
#print(subset(fciAll, UID==1000057))	


	tt <- subset(fcMeans, isBig)
	fciBig <- aggregate(list(RESULT = tt$RESULT)
					   ,list(UID = tt$UID
				 		    ,coverSuffix = tt$coverSuffix
							)
#					   ,sum, na.rm=TRUE
					   ,protectedSum, na.rm=TRUE
					   )
	fciBig$PARAMETER <- 'FCIBIG'
	
	
	tt <- subset(fcMeans, isNatural)
	fciNatural <- aggregate(list(RESULT = tt$RESULT)
						   ,list(UID = tt$UID
								,coverSuffix = tt$coverSuffix
				   				)
#						   ,sum, na.rm=TRUE
						   ,protectedSum, na.rm=TRUE
						   )
	fciNatural$PARAMETER <- 'FCINATURAL'
	
	
	tt <- subset(fcMeans, isRipVegetation)
	fciRipVeg <- aggregate(list(RESULT = tt$RESULT)
						  ,list(UID = tt$UID
							   ,coverSuffix = tt$coverSuffix
				  			   )
#						  ,sum, na.rm=TRUE
						  ,protectedSum, na.rm=TRUE
						  )
	fciRipVeg$PARAMETER <- 'FCIRIPVEG'
#print(dcast(subset(fcDataCalcs, UID==1000057), STATION+coverSuffix~PARAMETER, value.var='characteristicCover'))	
#print(dcast(subset(fcMeans, UID==1000057), PARAMETER~coverSuffix, value.var='RESULT'))	
#print(subset(fciRipVeg, UID==1000057))	
	
	intermediateMessage('.i')
	
	groupIndices <- rbind(fciAll, fciBig, fciNatural, fciRipVeg)[c('UID','PARAMETER','RESULT', 'coverSuffix')]
	intermediateMessage('.j')
	
	return(groupIndices)
}



# end of file
