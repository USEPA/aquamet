metsHumanImpact <- function(df, data2007=FALSE, fillinDrawdown=TRUE) {

################################################################################
# Function: metsHumanImpact
# Title: Calculate NLA Human Impact Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 21, 2008
# Description:
#   This function calculates the human impact portion of the physical habitat
#   metrics for National Lakes Assessment (NLA) data.  The function requires a
#   data frame containing validated physical habitat data collected using the
#   NLA protocol.
# Function Revisions:
#   10/21/08 cws: corrected spelling of mets names. Correcting counts of missing 
#            parameters.  These are now zero.
#   11/04/08 cws: Added additional mets as specified by PRK.  Reorganizing code
#            to simplify additional calculations
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   07/18/13 cws: Adding unit test based on 2007 data, results assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).
#   07/22/13 cws: Upcased column names, changed subid to STATION. Unit test
#            works.
#   07/24/13 cws: Upcased metric names; filtering NA RESULT values from input
#            data;  returning metrics in long instead of wide organization.
#   08/02/13 cws: Added handling of separate drawdown parameters.  Still passes 
#            unit test with 2007 data.
#   08/06/13 cws: Changed names of metrics to add _RIP suffix to riparian
#            metrics when createSyntheticCovers is TRUE.
#   08/07/13 cws: Changed _RIP suffix to _LIT.
#   08/21/13 cws: Changed _LIT suffix back to _RIP. Refactored function to
#            separate types of calculations.  Changed argument
#            createSyntheticCovers to data2007, as synthetic calculations are
#            not currently defined for this data.
#   08/23/13 cws: Allowing HI_OTHER, HI_OTHER_DD parameters in non-2007 data.
#            These have isAg set to FALSE.  Since may or may not be
#            agricultural, isAg could reasonably be set to NA for these, but
#            that would require additional changes to the coding (NA values in
#            aggregate grouping columns cause those rows to be eliminated;
#            ifelse() calls would also require modification).  Unit test created
#            on agreement with results from projects/metsHumanInfluenceTest.sas.
#   08/30/13 cws: Adding calculation of synthetic influence values
#   09/04/13 cws: Using DRAWDOWN parameter to fill in unrecorded drawdown covers
#            and HORIZ_DIST_DD values.  Added DRAWDOWN to unit test data.  Unit
#            test NOT YET modified accordingly, as Phil wants these values
#            quickly.
#   11/14/13 cws: correcting synthetic calculations based on comparisons with
#            SAS output.  Updated unit test for data with DRAWDOWN and filled in
#            drawdown values.  Some aggregate calculations now use anonymous
#            functions to return NA when all the values are NA; sum() returns 0
#            and mean() returns NaN when all values are NA and na.rm=TRUE. Unit
#            test metsHumanImpactTest.withDrawDown() now has appropriate test
#            data.  These changes resulted in additional rows of results where
#            counts are zero (e.g. HINAG) and other values are NA (e.g. HIIAG,
#            HIPWAG).  Otherwise the results are unchanged.
#   12/12/13 cws: Changed remaining aggregate calls to mean() with anonymous
#            function to avoid NaN results when all values are NA.
#   12/13/13 cws: Using protectedmean() and protectedSum() instead of anonymous
#            functions to avoid NaN results when all values are NA.
#   12/16/13 cws: Regression test with entire 2007 data passed with expected 855
#            differences in HIN* values (was 0, now NA when data is absent).
#   06/12/14 tmk: Removed calls to the require() function.
# Arguments:
#   df = a data frame containing human influence data.  The data frame must
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
#                   argument has the following values: HI_BUILDINGS,
#                   HI_COMMERCIAL, HI_CROPS, HI_DOCKS, HI_LANDFILL, HI_LAWN,
#                   HI_ORCHARD, HI_PARK, HI_PASTURE, HI_POWERLINES, HI_ROADS,
#                   and HI_WALLS.
#       RESULT - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
#   data2007 = a logical value, which equals TRUE if 2007 data is being
#     processed.  The default value is FALSE.
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
  cat('Human Influence calculations:\n')
	intermediateMessage('Human Influence mets', loc='start')

	# Expected parameters
	if(data2007) {
	  	impactClasses <- c('HI_BUILDINGS', 'HI_COMMERCIAL'
			  			  ,'HI_CROPS', 'HI_DOCKS', 'HI_LANDFILL'
			  			  ,'HI_LAWN', 'HI_ORCHARD', 'HI_PARK'
		  				  ,'HI_PASTURE', 'HI_POWERLINES', 'HI_ROADS'
		  				  ,'HI_WALLS'
  						  ,'HI_BUILDINGS_DD', 'HI_COMMERCIAL_DD'
						  ,'HI_CROPS_DD', 'HI_DOCKS_DD', 'HI_LANDFILL_DD'
						  ,'HI_LAWN_DD', 'HI_ORCHARD_DD', 'HI_PARK_DD'
						  ,'HI_PASTURE_DD', 'HI_POWERLINES_DD', 'HI_ROADS_DD'
						  ,'HI_WALLS_DD'
						  )
		hiTypes <- data.frame(PARAMETER=impactClasses
							 ,isAg=c(FALSE, FALSE
									,TRUE, FALSE, FALSE
							 		,FALSE, TRUE, FALSE
							 		,TRUE, FALSE, FALSE
							 		,FALSE
							 		,FALSE, FALSE
							 		,TRUE, FALSE, FALSE
							 		,FALSE, TRUE, FALSE
							 		,TRUE, FALSE, FALSE
							 		,FALSE
							 		)
						  	 )
	} else {
		impactClasses <- c('HI_BUILDINGS', 'HI_COMMERCIAL'
						  ,'HI_CROPS', 'HI_DOCKS', 'HI_LANDFILL'
						  ,'HI_LAWN', 'HI_ORCHARD', 'HI_OTHER', 'HI_PARK'				# HI_OTHER is new
						  ,'HI_PASTURE', 'HI_POWERLINES', 'HI_ROADS'
						  ,'HI_WALLS'
						  ,'HI_BUILDINGS_DD', 'HI_COMMERCIAL_DD'
						  ,'HI_CROPS_DD', 'HI_DOCKS_DD', 'HI_LANDFILL_DD'
						  ,'HI_LAWN_DD', 'HI_ORCHARD_DD', 'HI_OTHER_DD', 'HI_PARK_DD'	# HI_OTHER_DD is new
						  ,'HI_PASTURE_DD', 'HI_POWERLINES_DD', 'HI_ROADS_DD'
						  ,'HI_WALLS_DD'
				  		  )
		hiTypes <- data.frame(PARAMETER=impactClasses
							 ,isAg=c(FALSE, FALSE
									,TRUE, FALSE, FALSE
									,FALSE, TRUE, FALSE, FALSE
									,TRUE, FALSE, FALSE
									,FALSE
									,FALSE, FALSE
									,TRUE, FALSE, FALSE
									,FALSE, TRUE, FALSE, FALSE
									,TRUE, FALSE, FALSE
									,FALSE
				)
		)
	}
	
  	# Define weighing influence proximity values 
  	weights0CP <- data.frame(proximity=c('0', 'P', 'C')
                          	,calc=     c(0.0, 0.5, 1.0)
                          	,circa=    c(0,   0,   1)
                          	,present=  c(0,   1,   1)
                          	)


	# Fill in unrecorded cover and HORIZ_DIST_DD based on DRAWDOWN.
	if(fillinDrawdown) {
		intermediateMessage('.fill')
		tt <- subset(df, PARAMETER %in% c(impactClasses,'HORIZ_DIST_DD','DRAWDOWN'))
		dfStart <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
	} else {
		print("No 'fill-in' of drawdown data will be attempted")
		dfStart <- df
	}
	

  	hiData <- subset(dfStart, PARAMETER %in% impactClasses & !is.na(RESULT))
	
  
  	# Convert proximity classes to numeric values and characterize influence
  	# types
  	hiData <- merge(hiData, weights0CP
                   ,by.x='RESULT'
                   ,by.y='proximity'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
  	hiData <- merge(hiData, hiTypes
                   ,by.x='PARAMETER'
                   ,by.y='PARAMETER'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
				   
	intermediateMessage('.0')
				   

	# Create synthetic influence values if not 2007esque data
	if(!data2007) {
		
		intermediateMessage('.syn')
		horizDist <- within(subset(dfStart, PARAMETER %in% 'HORIZ_DIST_DD' & !is.na(RESULT))
						   ,{calc <- NA
							 circa <- NA
							 present <- NA
							 isAg <- NA
							}
						   )

		intermediateMessage('.synA')
		synValues <- calcSynInfluence(rbind(hiData,horizDist))
		intermediateMessage('.synB')
		
		# Make the resulting dataframe look like the data we're calculating metrics with.
		# THIS IS POORLY FACTORED; IDENTIFYING AG INFLUENCES SHOULD ONLY BE DONE ONCE
		newValues <- within(merge(synValues[c('UID','STATION','PARAMETER','RESULT','calc')]
								 ,data.frame(PARAMETER = c('HI_BUILDINGS_SYN', 'HI_COMMERCIAL_SYN'
						  								  ,'HI_CROPS_SYN', 'HI_DOCKS_SYN', 'HI_LANDFILL_SYN'
						  								  ,'HI_LAWN_SYN', 'HI_ORCHARD_SYN', 'HI_OTHER_SYN', 'HI_PARK_SYN'
						  								  ,'HI_PASTURE_SYN', 'HI_POWERLINES_SYN', 'HI_ROADS_SYN'
						  								  ,'HI_WALLS_SYN'
				  		  								  )
								  			,isAg=c(FALSE, FALSE
												   ,TRUE, FALSE, FALSE
												   ,FALSE, TRUE, FALSE, FALSE
												   ,TRUE, FALSE, FALSE
												   ,FALSE
												   )
						   					,stringsAsFactors=FALSE
											)
								 ,'PARAMETER'
								 ,all.x=TRUE
						 		 )
						   ,{SAMPLE_TYPE<-'PHAB'
							 FORM_TYPE <- ''
							 FLAG <- as.character(NA)
							 circa <- ifelse(calc >= (1.0 - 1e-15), TRUE, FALSE)
							 present <- ifelse(calc > (1e-15), TRUE, FALSE)
							}
						   )
						   
		intermediateMessage('.synC')
		hiData <- rbind(hiData, newValues)
		intermediateMessage('.synD')
	}
#print("HI hiData"); print(dcast(subset(testData, UID==6303), PARAMETER~STATION, value.var='RESULT'))
	

	# Classify PARAMETERs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later
	# on.  For 2007, all locations will be riparian
	intermediateMessage('.splitting')
	splitParams <- metsFishCover.splitParameterNames(hiData$PARAMETER)
	intermediateMessage('.classifying')
	hiData <- within(hiData
					,{coverSuffix <- ifelse(splitParams$suffix=='', '_RIP', splitParams$suffix)
					  PARAMETER <- splitParams$base
					 }
					)
	intermediateMessage('.calculating')
					
	hiMets <- metsHumanImpact.calculateMets(hiData)
	
	
	# PARAMETER gets name of metric including suffix
	hiMets <- within(hiMets
					,{PARAMETER <- paste0(PARAMETER, coverSuffix)
					  coverSuffix <- NULL
					 }
					)
	
	# When calculating values for 2007 study, rename the _RIP metrics back to 
	# 2007 names (no suffix). 	
	if(data2007) {
		hiMets <- within(hiMets
						,{PARAMETER <- ifelse(grepl('_RIP$', PARAMETER)
				                             ,substr(PARAMETER, 1, nchar(PARAMETER)-4)
							 				 ,PARAMETER
							 				 )
					  	 }
						)
	}
	intermediateMessage(' Done.', loc='end')
	
	return(hiMets)
}


metsHumanImpact.calculateMets <- function(hiData)
# Do all the calculationy stuff
{
	intermediateMessage('.w')
	weightedIndividualInfluence <- metsHumanImpact.weightedIndividualInfluence(hiData)
	intermediateMessage('.a')
	anyPresence <- metsHumanImpact.anyPresence(hiData)
	intermediateMessage('.o')
	overallInfluence <- metsHumanImpact.overallInfluence(hiData)
	intermediateMessage('.c')
	circaInfluence <- metsHumanImpact.circaInfluence(hiData)
	intermediateMessage('.g')
	weightedGroupInfluence <- metsHumanImpact.weightedGroupInfluence(hiData)
	intermediateMessage('.X')
	
	all <- rbind(weightedIndividualInfluence, anyPresence, overallInfluence
			   	,circaInfluence, weightedGroupInfluence
			   	)
	rc <- expand.data.frame(all, c('UID','PARAMETER','coverSuffix'))
	rc$RESULT <- with(rc, ifelse(grepl('^HIN', PARAMETER) & is.na(RESULT), 0, RESULT))
	return(rc)
}


metsHumanImpact.weightedIndividualInfluence <- function(hiData)
# Calculate means and counts for individual influence classes
{
  	# Calculate means for individual influence classes
  	tt <- aggregate(list(RESULT = hiData$calc)
                   ,list(UID = hiData$UID
                        ,PARAMETER = hiData$PARAMETER
						,coverSuffix = hiData$coverSuffix
		   				,isAg = hiData$isAg
                        )
                   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                   )
  	separateMeans <- within(tt
				           ,{PARAMETER <- gsub('^HI_(.+)$', 'HIPW\\1', PARAMETER)
						     isAg <- NULL
					   	    }
						   )
						 
  	intermediateMessage('.1')

	# Determine sample sizes for individual influence classes
	tt <- aggregate(list(RESULT = hiData$calc)
				   ,list('UID'=hiData$UID
						,'PARAMETER'=hiData$PARAMETER
						,coverSuffix = hiData$coverSuffix
						)
				   ,count
				   )
	
	separateCounts <- within(tt, PARAMETER <- gsub('^HI_(.+)$', 'HIN\\1', PARAMETER))
	intermediateMessage('.5')
	
	rc <- rbind(separateMeans, separateCounts)
	
	return(rc)
}


metsHumanImpact.overallInfluence <- function(hiData)	
# Calculate overall influence indicies, defined as the sum of the individual
# mean influences at a site.  These influences are summed as a whole, and
# separately for agricultural and nonagricultural influences.
{
	hiMeans <- aggregate(list(RESULT = hiData$calc)
						,list(UID = hiData$UID
							 ,PARAMETER = hiData$PARAMETER
							 ,coverSuffix = hiData$coverSuffix
							 ,isAg = hiData$isAg
							 )
#						,mean, na.rm=TRUE
						,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						)
	hiiAll <- aggregate(list(RESULT = hiMeans$RESULT)
                       ,list('UID'=hiMeans$UID, coverSuffix=hiMeans$coverSuffix)
                       ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                       )
    hiiAll$PARAMETER <- 'HIIALL'
 
  
    tt <- aggregate(list(RESULT = hiMeans$RESULT)
                   ,list('UID'=hiMeans$UID, coverSuffix=hiMeans$coverSuffix, isAg=hiMeans$isAg)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
    hiiSep <- within(tt
				    ,{PARAMETER <- ifelse(isAg, 'HIIAG','HIINONAG')
					  isAg <- NULL
				     }
		            )

    intermediateMessage('.2')
#print('metsHumanImpact.overallInfluence hiData: calc SYN'); print(dcast(subset(hiData, UID==6303 & coverSuffix=='_SYN'), PARAMETER~STATION, value.var='calc'))
#print('metsHumanImpact.overallInfluence hiData: present SYN'); print(dcast(subset(hiData, UID==6303 & coverSuffix=='_SYN'), PARAMETER~STATION, value.var='present'))
#print('metsHumanImpact.overallInfluence hiMeans:'); print(dcast(subset(hiMeans, UID==6303), PARAMETER~coverSuffix, value.var='RESULT'))
#print('metsHumanImpact.overallInfluence hiiAll:'); print(subset(hiiAll, UID==6303))
	
	hiiOverall <- rbind(hiiAll, hiiSep)
	
	return(hiiOverall)
}


metsHumanImpact.circaInfluence <- function(hiData)
# Calculate circa influence indicies, which limit calculation to include
# only influences found within the plot area at a station.  These are
# the sum of the site mean circa-weights, summed as a whole and 
# separately for agricultural and nonagricultural influences.  Another
# way to think of the means are as the fractional presence of influences
# recorded within the station plot (recorded as 'C' for circa).
{
    hiCircaMeans <- aggregate(hiData$circa
                             ,list(UID = hiData$UID
                                  ,PARAMETER = hiData$PARAMETER
								  ,coverSuffix = hiData$coverSuffix
								  ,isAg = hiData$isAg
								  )
#                             ,mean, na.rm=TRUE
				 			 ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
							 )
    hiiAllCirca <- aggregate(list(RESULT = hiCircaMeans$x)
                            ,list(UID = hiCircaMeans$UID
				                 ,coverSuffix = hiCircaMeans$coverSuffix
			                     )
                            ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
							)
    hiiAllCirca$PARAMETER <- 'HIIALLCIRCA'

  
    tt <- aggregate(list(RESULT = hiCircaMeans$x)
                   ,list(UID = hiCircaMeans$UID
		                ,coverSuffix = hiCircaMeans$coverSuffix
	  				    ,isAg = hiCircaMeans$isAg
	  				    )
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
		   		   )
    hiiSepCirca <- within(tt
				  	     ,{PARAMETER <- ifelse(isAg, 'HIIAGCIRCA','HIINONAGCIRCA')
  					   	   isAg <- NULL
					      }
		          	     )
						 
    hiiOverallCirca <- rbind(hiiAllCirca, hiiSepCirca)
    intermediateMessage('.3')
	
	return(hiiOverallCirca)
}

  
metsHumanImpact.anyPresence <- function(hiData)  
# Calculate fractional presence of any disturbance, both anywhere
# at the station (circa and proximal), and just within the study plot (circa).
{
  	tt <- aggregate(hiData$present
                   ,list('UID'=hiData$UID
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
                      	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
	tt0<-tt
    tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAny <- aggregate(list(RESULT = tt$x)
                      ,list('UID'=tt$UID, coverSuffix = tt$coverSuffix)
                      ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                      )
	hiAny$PARAMETER <- 'HIFPANY'


  	tt <- aggregate(hiData$circa
                   ,list('UID'=hiData$UID
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
	  				  	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
  	tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAnyCirca <- aggregate(list(RESULT = tt$x)
                           ,list('UID'=tt$UID, coverSuffix = tt$coverSuffix)
                           ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                           )
  	hiAnyCirca$PARAMETER <- 'HIFPANYCIRCA'

#print('metsHumanImpact.anyPresence hiData: calc SYN'); print(dcast(subset(hiData, UID==6303 & coverSuffix=='_SYN'), PARAMETER~STATION, value.var='calc'))
#print('metsHumanImpact.anyPresence hiData: present SYN'); print(dcast(subset(hiData, UID==6303 & coverSuffix=='_SYN'), PARAMETER~STATION, value.var='present'))
#print('metsHumanImpact.anyPresence tt0:'); print(dcast(subset(tt0, UID==6303), coverSuffix~STATION, value.var='x'))
#print('metsHumanImpact.anyPresence hiAny:'); print(subset(hiAny, UID==6303))
  
  	hiAny <- rbind(hiAny, hiAnyCirca)
  	intermediateMessage('.4')

	return(hiAny)
}


metsHumanImpact.weightedGroupInfluence <- function(hiData)
# Calculate weighted influence of agricultural, nonagricultural and total impacts
{
  	# Calculate mean agricultural influence and sample size
  	hiAg <- subset(hiData, isAg==TRUE)

    agMean <- aggregate(list(RESULT = hiAg$calc)
                 	   ,list('UID'=hiAg$UID, coverSuffix = hiAg$coverSuffix)
#		         	   ,mean, na.rm=TRUE
					   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		         	   )
    agMean$PARAMETER <- 'HIPWAG'

  	agCount <- aggregate(list(RESULT = hiAg$calc)
						,list('UID'=hiAg$UID, coverSuffix = hiAg$coverSuffix)
						,count
						)
	agCount$PARAMETER <- 'HINAG'
#print('metsHumanImpact.weightedGroupInfluence hiAg:'); print(dcast(subset(hiAg, UID==6228), PARAMETER~STATION, value.var='RESULT'))
#print('metsHumanImpact.weightedGroupInfluence agMean:'); print(dcast(subset(agMean, UID==6228), PARAMETER~STATION, value.var='RESULT'))
#print('metsHumanImpact.weightedGroupInfluence agCount:'); print(dcast(subset(agCount, UID==6228), PARAMETER~STATION, value.var='RESULT'))

  	intermediateMessage('.6')


  	# Calculate mean nonagricultural influence
  	hiNonAg <- subset(hiData, isAg==FALSE)
  	nonagMean <- aggregate(list(RESULT = hiNonAg$calc)
  						  ,list('UID'=hiNonAg$UID, coverSuffix = hiNonAg$coverSuffix)
						  ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						  )
  	nonagMean$PARAMETER <- 'HIPWNONAG'
 
	nonagCount <- aggregate(list(RESULT = hiNonAg$calc)
						   ,list('UID'=hiNonAg$UID, coverSuffix = hiNonAg$coverSuffix)
				   		   ,count
						   )
	nonagCount$PARAMETER <- 'HINNONAG'

  	intermediateMessage('.7')


  	# Calculate mean total human influence
  	totMean <- aggregate(list(RESULT = hiData$calc)
  				 		,list('UID'=hiData$UID, coverSuffix = hiData$coverSuffix)
		 		 		,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		 		 		)
  	totMean$PARAMETER <- 'HIPWALL'
  
	totCount <- aggregate(list(RESULT = hiData$calc)
  				 	 	 ,list('UID'=hiData$UID, coverSuffix = hiData$coverSuffix)
		 		 	 	 ,count
				 	 	 )
	totCount$PARAMETER <- 'HINALL'

  	intermediateMessage('.8')

  	
	rc <- rbind(agMean, agCount, nonagMean, nonagCount, totMean, totCount)

  	return(rc)
	
}



# end of file
