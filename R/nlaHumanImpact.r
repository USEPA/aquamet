nlaHumanImpact <- function(df, data2007=FALSE, fillinDrawdown=TRUE) {

################################################################################
# Function: nlaHumanImpact
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
#       SITE - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single SITE.
#               For NLA, transect is used for this purpose.
#       CLASS - parameter name, which identifies the variables used in
#                   calculations. In wide data frame format, this argument
#                   would be used to name columns.  It is assumed that this
#                   argument has the following values: HI_BUILDINGS,
#                   HI_COMMERCIAL, HI_CROPS, HI_DOCKS, HI_LANDFILL, HI_LAWN,
#                   HI_ORCHARD, HI_PARK, HI_PASTURE, HI_POWERLINES, HI_ROADS,
#                   and HI_WALLS.
#       VALUE - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
#   data2007 = a logical value, which equals TRUE if 2007 data is being
#     processed.  The default value is FALSE.
#   fillinDrawdown = a logical value, which specifies whether to use the
#     DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
#     The default value is TRUE.
# Output:
#   A data frame that contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

  # Print initial messages
  cat('Human Influence calculations:\n')
	intermediateMessage('NLA human influence metrics', loc='start')

	# Expected parameters
	if(data2007) {
    	intermediateMessage('.2007data')
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
		hiTypes <- data.frame(CLASS=impactClasses
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
    	intermediateMessage('.2012+data')
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
		hiTypes <- data.frame(CLASS=impactClasses
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
		tt <- subset(df, CLASS %in% c(impactClasses,'HORIZ_DIST_DD','DRAWDOWN'))
		dfStart <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
	} else {
		print("No 'fill-in' of drawdown data will be attempted")
		dfStart <- df
	}
	intermediateMessage('.1')
	

  	hiData <- subset(dfStart, CLASS %in% impactClasses & !is.na(VALUE))
	intermediateMessage('.2')
	
  
  	# Convert proximity classes to numeric values and characterize influence
  	# types
  	hiData <- merge(hiData, weights0CP
                   ,by.x='VALUE'
                   ,by.y='proximity'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
  	hiData <- merge(hiData, hiTypes
                   ,by.x='CLASS'
                   ,by.y='CLASS'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
	intermediateMessage('.3')
				   

	# Create synthetic influence values if not 2007esque data
	if(!data2007) {
		
		intermediateMessage('.syn')
		horizDist <- within(subset(dfStart, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE))
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
		newValues <- within(merge(synValues[c('SITE','STATION','CLASS','VALUE','calc')]
								 ,data.frame(CLASS = c('HI_BUILDINGS_SYN', 'HI_COMMERCIAL_SYN'
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
								 ,'CLASS'
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


	# Classify CLASSs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later
	# on.  For 2007, all locations will be riparian
	intermediateMessage('.splitting')
	splitParams <- nlaFishCover.splitParameterNames(hiData$CLASS)
	intermediateMessage('.classifying')
	hiData <- within(hiData
					,{coverSuffix <- ifelse(splitParams$suffix=='', '_RIP', splitParams$suffix)
					  CLASS <- splitParams$base
					 }
					)
	intermediateMessage('.calculating')
					
	hiMets <- nlaHumanImpact.calculateMets(hiData)
	
	
	# METRIC gets name of metric including suffix
	hiMets <- within(hiMets
					,{METRIC <- paste0(METRIC, coverSuffix)
					  coverSuffix <- NULL
					 }
					)
	
	# When calculating values for 2007 study, rename the _RIP metrics back to 
	# 2007 names (no suffix). 	
	if(data2007) {
		hiMets <- within(hiMets
						,{METRIC <- ifelse(grepl('_RIP$', METRIC)
				                             ,substr(METRIC, 1, nchar(METRIC)-4)
							 				 ,METRIC
							 				 )
					  	 }
						)
	}
	intermediateMessage(' Done.', loc='end')
	
	return(hiMets)
}


nlaHumanImpact.calculateMets <- function(hiData)
# Do all the calculationy stuff
{
	intermediateMessage('.w')
	weightedIndividualInfluence <- nlaHumanImpact.weightedIndividualInfluence(hiData)
	intermediateMessage('.a')
	anyPresence <- nlaHumanImpact.anyPresence(hiData)
	intermediateMessage('.o')
	overallInfluence <- nlaHumanImpact.overallInfluence(hiData)
	intermediateMessage('.c')
	circaInfluence <- nlaHumanImpact.circaInfluence(hiData)
	intermediateMessage('.g')
	weightedGroupInfluence <- nlaHumanImpact.weightedGroupInfluence(hiData)
	intermediateMessage('.X')
#print(str(weightedIndividualInfluence));print(str(anyPresence));print(str(overallInfluence));print(str(circaInfluence));print(str(weightedGroupInfluence));
	all <- rbind(weightedIndividualInfluence, anyPresence, overallInfluence
			   	,circaInfluence, weightedGroupInfluence
			   	)
	intermediateMessage('.Y')
	rc <- expand.data.frame(all, c('SITE','METRIC','coverSuffix'))
	intermediateMessage('.Z')
	rc$VALUE <- with(rc, ifelse(grepl('^HIN', METRIC) & is.na(VALUE), 0, VALUE))
	return(rc)
}


nlaHumanImpact.weightedIndividualInfluence <- function(hiData)
# Calculate means and counts for individual influence classes
{
  	# Calculate means for individual influence classes
  	tt <- aggregate(list(VALUE = hiData$calc)
                   ,list(SITE = hiData$SITE
                        ,CLASS = hiData$CLASS
						,coverSuffix = hiData$coverSuffix
		   				,isAg = hiData$isAg
                        )
                   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                   )
  	separateMeans <- within(tt
				           ,{METRIC <- gsub('^HI_(.+)$', 'HIPW\\1', CLASS)
				             CLASS <- NULL
						     isAg <- NULL
					   	    }
						   )
						 
  	intermediateMessage('.1')

	# Determine sample sizes for individual influence classes
	tt <- aggregate(list(VALUE = hiData$calc)
				   ,list('SITE'=hiData$SITE
						,'CLASS'=hiData$CLASS
						,coverSuffix = hiData$coverSuffix
						)
				   ,count
				   )
	
	separateCounts <- mutate(tt
	                        ,METRIC = gsub('^HI_(.+)$', 'HIN\\1', CLASS)
	                        ,CLASS = NULL
	                        )
	intermediateMessage('.5')
	
	rc <- rbind(separateMeans, separateCounts)
	
	return(rc)
}


nlaHumanImpact.overallInfluence <- function(hiData)	
# Calculate overall influence indicies, defined as the sum of the individual
# mean influences at a site.  These influences are summed as a whole, and
# separately for agricultural and nonagricultural influences.
{
	hiMeans <- aggregate(list(VALUE = hiData$calc)
						,list(SITE = hiData$SITE
							 ,CLASS = hiData$CLASS
							 ,coverSuffix = hiData$coverSuffix
							 ,isAg = hiData$isAg
							 )
#						,mean, na.rm=TRUE
						,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						)
	hiiAll <- aggregate(list(VALUE = hiMeans$VALUE)
                       ,list('SITE'=hiMeans$SITE, coverSuffix=hiMeans$coverSuffix)
                       ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                       )
    hiiAll$METRIC <- 'HIIALL'
 
  
    tt <- aggregate(list(VALUE = hiMeans$VALUE)
                   ,list('SITE'=hiMeans$SITE, coverSuffix=hiMeans$coverSuffix, isAg=hiMeans$isAg)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
    hiiSep <- within(tt
				    ,{METRIC <- ifelse(isAg, 'HIIAG','HIINONAG')
					  isAg <- NULL
				     }
		            )

    intermediateMessage('.2')

	hiiOverall <- rbind(hiiAll, hiiSep)
	
	return(hiiOverall)
}


nlaHumanImpact.circaInfluence <- function(hiData)
# Calculate circa influence indicies, which limit calculation to include
# only influences found within the plot area at a station.  These are
# the sum of the site mean circa-weights, summed as a whole and 
# separately for agricultural and nonagricultural influences.  Another
# way to think of the means are as the fractional presence of influences
# recorded within the station plot (recorded as 'C' for circa).
{
    hiCircaMeans <- aggregate(hiData$circa
                             ,list(SITE = hiData$SITE
                                  ,CLASS = hiData$CLASS
								  ,coverSuffix = hiData$coverSuffix
								  ,isAg = hiData$isAg
								  )
#                             ,mean, na.rm=TRUE
				 			 ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
							 )
    hiiAllCirca <- aggregate(list(VALUE = hiCircaMeans$x)
                            ,list(SITE = hiCircaMeans$SITE
				                 ,coverSuffix = hiCircaMeans$coverSuffix
			                     )
                            ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
							)
    hiiAllCirca$METRIC <- 'HIIALLCIRCA'

  
    tt <- aggregate(list(VALUE = hiCircaMeans$x)
                   ,list(SITE = hiCircaMeans$SITE
		                ,coverSuffix = hiCircaMeans$coverSuffix
	  				    ,isAg = hiCircaMeans$isAg
	  				    )
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
		   		   )
    hiiSepCirca <- within(tt
				  	     ,{METRIC <- ifelse(isAg, 'HIIAGCIRCA','HIINONAGCIRCA')
  					   	   isAg <- NULL
					      }
		          	     )
						 
    hiiOverallCirca <- rbind(hiiAllCirca, hiiSepCirca)
    intermediateMessage('.3')
	
	return(hiiOverallCirca)
}

  
nlaHumanImpact.anyPresence <- function(hiData)  
# Calculate fractional presence of any disturbance, both anywhere
# at the station (circa and proximal), and just within the study plot (circa).
{
  	tt <- aggregate(hiData$present
                   ,list('SITE'=hiData$SITE
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
                      	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
	tt0<-tt
    tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAny <- aggregate(list(VALUE = tt$x)
                      ,list('SITE'=tt$SITE, coverSuffix = tt$coverSuffix)
                      ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                      )
	hiAny$METRIC <- 'HIFPANY'


  	tt <- aggregate(hiData$circa
                   ,list('SITE'=hiData$SITE
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
	  				  	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
  	tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAnyCirca <- aggregate(list(VALUE = tt$x)
                           ,list('SITE'=tt$SITE, coverSuffix = tt$coverSuffix)
                           ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                           )
  	hiAnyCirca$METRIC <- 'HIFPANYCIRCA'

  	hiAny <- rbind(hiAny, hiAnyCirca)
  	intermediateMessage('.4')

	return(hiAny)
}


nlaHumanImpact.weightedGroupInfluence <- function(hiData)
# Calculate weighted influence of agricultural, nonagricultural and total impacts
{
  	# Calculate mean agricultural influence and sample size
  	hiAg <- subset(hiData, isAg==TRUE)

    agMean <- aggregate(list(VALUE = hiAg$calc)
                 	   ,list('SITE'=hiAg$SITE, coverSuffix = hiAg$coverSuffix)
					   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		         	   )
    agMean$METRIC <- 'HIPWAG'

  	agCount <- aggregate(list(VALUE = hiAg$calc)
						,list('SITE'=hiAg$SITE, coverSuffix = hiAg$coverSuffix)
						,count
						)
	agCount$METRIC <- 'HINAG'

  	intermediateMessage('.6')


  	# Calculate mean nonagricultural influence
  	hiNonAg <- subset(hiData, isAg==FALSE)
  	nonagMean <- aggregate(list(VALUE = hiNonAg$calc)
  						  ,list('SITE'=hiNonAg$SITE, coverSuffix = hiNonAg$coverSuffix)
						  ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						  )
  	nonagMean$METRIC <- 'HIPWNONAG'
 
	nonagCount <- aggregate(list(VALUE = hiNonAg$calc)
						   ,list('SITE'=hiNonAg$SITE, coverSuffix = hiNonAg$coverSuffix)
				   		   ,count
						   )
	nonagCount$METRIC <- 'HINNONAG'

  	intermediateMessage('.7')


  	# Calculate mean total human influence
  	totMean <- aggregate(list(VALUE = hiData$calc)
  				 		,list('SITE'=hiData$SITE, coverSuffix = hiData$coverSuffix)
		 		 		,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		 		 		)
  	totMean$METRIC <- 'HIPWALL'
  
	totCount <- aggregate(list(VALUE = hiData$calc)
  				 	 	 ,list('SITE'=hiData$SITE, coverSuffix = hiData$coverSuffix)
		 		 	 	 ,count
				 	 	 )
	totCount$METRIC <- 'HINALL'

  	intermediateMessage('.8')

  	
	rc <- rbind(agMean, agCount, nonagMean, nonagCount, totMean, totCount)

  	return(rc)
	
}

# end of file