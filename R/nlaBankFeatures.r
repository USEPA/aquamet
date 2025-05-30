#' @export
#' @title Calculate NLA Bank Features Metrics
#' @description This function calculates the bank features portion of the physical
#' habitat metrics for National Lakes Assessment (NLA) data.  
#' @param angle A data frame containing bank angle class values for sites 
#' sampled using the boatable protocol, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE character string, with legal values of 'FLAT','GRADUAL','STEEP',
#' 'NEAR_VERTICAL'. Values of 'NEAR_VERTICAL_UNDERCUT' must be updated to 
#' 'NEAR_VERTICAL'.
#' }
#' @param drawdown A data frame containing drawdown value for sites sampled
#' using the boatable protocol, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character string indicating the presence of drawdown. Valid
#' values begin with Y for yes and N for No.
#' }
#' @param horizontalDistance A data frame containing the horizontal 
#' distance from waterline to high water mark (m) when flooding is present, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an numeric value, or character value that is castable to an 
#' numeric.
#' }
#' @param horizontalDistanceDrawdown A data frame containing the horizontal 
#' distance from waterline to high water mark (m) when drawdown is present, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an numeric value, or character value that is castable to an 
#' numeric.
#' }
#' @param verticalHeight A data frame containing the vertical 
#' height from waterline to high water mark (m) when flooding is present, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an numeric value, or character value that is castable to an 
#' numeric.
#' }
#' @param verticalHeightDrawdown A data frame containing the vertical 
#' height from waterline to high water mark (m) when drawdown is present, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an numeric value, or character value that is castable to an 
#' numeric.
#' }
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics include:
#' BFOANGLE, BFFFLAT, BFFGRADUAL, BFFSTEEP, BFFVERTICAL, BFNANGLE, BFXHORIZDIST,
#' BFXHORIZDIST_DD, BFXVERTHEIGHT_DD, BFNHORIZDIST, BFNHORIZDIST_DD, 
#' BFNVERTHEIGHT_DD
#' 
#' Descriptions for all NLA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NLA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'  head(nlaPhab)
#'   
#'  # Must subset example dataset to create inputs, keeping only SITE, STATION,
#'  #  and VALUE
#'  # For angle subset, we must revalue NEAR_VERTICAL_UNDERCUT to NEAR_VERTICAL 
#'  #  to work with function.
#'  angle <- subset(nlaPhab,PARAMETER=='ANGLE',select=-PARAMETER)  
#'  angle$VALUE <- with(angle, ifelse(VALUE=='NEAR_VERTICAL_UNDERCUT', 
#'                            'NEAR_VERTICAL', VALUE))
#'   
#'  drawdown <- subset(nlaPhab, PARAMETER=='DRAWDOWN', select=-PARAMETER)
#'  
#'  # For most input subsets, we also need to make sure VALUE is numeric.
#'  horizontalDistance <- subset(nlaPhab,PARAMETER=='HORIZ_DIST', select=-PARAMETER)  
#'  horizontalDistance$VALUE <- with(horizontalDistance, as.numeric(VALUE))
#'     
#'  horizontalDistanceDrawdown <- subset(nlaPhab,PARAMETER=='HORIZ_DIST_DD', 
#'  select=-PARAMETER)  
#'  horizontalDistanceDrawdown$VALUE <- with(horizontalDistanceDrawdown, 
#'  as.numeric(VALUE))
#'  
#'  verticalHeight <- subset(nlaPhab, PARAMETER=='VERT_HEIGHT', select=-PARAMETER) 
#'  verticalHeight$VALUE <- with(verticalHeight, as.numeric(VALUE))
#'    
#'  verticalHeightDrawdown <- subset(nlaPhab, PARAMETER=='VERT_HEIGHT_DD', 
#'            select=-PARAMETER)
#'  verticalHeightDrawdown$VALUE <- with(verticalHeightDrawdown, as.numeric(VALUE)) 
#'            
#'  exBankFeatures <- nlaBankFeatures(angle, drawdown, 
#'   horizontalDistance, 
#'   horizontalDistanceDrawdown, 
#'   verticalHeight, 
#'   verticalHeightDrawdown)   
#'  head(exBankFeatures)
#'  
#' 

nlaBankFeatures <- function(angle = NULL
                           ,drawdown = NULL
                           ,horizontalDistance = NULL
                           ,horizontalDistanceDrawdown = NULL
                           ,verticalHeight = NULL
                           ,verticalHeightDrawdown = NULL
                           ,isUnitTest = FALSE
                           ) {

################################################################################
# Function: nlaBankFeatures
# Title: Calculate NLA Bank Features Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 21, 2008
# Description:
#   This function calculates the bank features portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.
# Function Revisions:
#   10/21/08 cws: Corrected counts of missing parameters.  These are now zero.
#   11/06/08 cws: Removed code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   12/20/13 cws: Developed unit test for 2007 data and refactored metrics
#            function, updating column names and upcasing parameter names.
#            Using modalValues() instead of single use code.  Regression test
#            with 2007 data shows the following expected differences: 642 values
#            for 76 sites without BF data are absent instead of NA; 11 values of
#            BFFFLAT, BFFGRADUAL, BFFSTEEP, BFFVERTICAL, BFXHORIZDIST,
#            BFXVERTHEIGHT due to floating point truncation; and 13 values of
#            BFOANGLE due to reordering of multiple mode values for a total of
#            666 differences. Using protectedMean() to deter production of NaN
#            value for BFXVERTHEIGHT noted earlier in UID 6601 of the 2012
#            study. Included drawdown distances for 2012 processing.
#   12/23/13 cws: Completed unit test with 2013 data.  Standardized input data.
#            Regression test with 2007 data passes as described 12/20.
#   02/19/14 cws: Modified metsBankFeatures.fillinDistancesTest() to expect only
#            distance and drawdown related parameters in the returned dataframe.
#   06/12/14 tmk: Removed calls to the require() function.
#    6/30/17 cws Renamed from metsBankFeatures to nlaBankFeatures.
#            Changed argument from single dataframe to one dataframe per value,
#            as was done with the NRSA metrics functions.  Call interface 
#            modified. Changed UID to SITE and RESULT to VALUE throughout, and
#            returning metric name in METRIC rather than PARAMETER, but
#            otherwise left the body of the function intact. Returning NULL if
#            arguments contain no data. Removed data2007 argument as it wasnt 
#            being used.
#    7/18/17 cws Corrected to put metrics values in METRICS column
#    3/19/19 cws Added isUnitTest argument for consistency.
#    3/21/19 cws Added validation checking of arguments. Modified unit test 
#            accordingly.
#   12/31/19 cws Allowing SITE to be character as well as integer in all arguments.
#    4/12/24 cws Modified nlaBankFeatures.fillinDistances to allow missing values
#            to be changed to zero when DRAWDOWN is N, as well as NO. Unit test
#            not modified.
#    5/21/24 cws Modified to rename expand.data.frame to expandDataFrame
#    
# Arguments:
#   df = a data frame containing bank features data.  The data frame must
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
#                   argument has the following values: ANGLE, HORIZ_DIST,
#                   VERT_HEIGHT, HORIZ_DIST_DD, and VERT_HEIGHT_DD.
#       RESULT - parameter values, which are the values used in calculations.
# Output:
#   A data frame that contains the following columns:
#     UID - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

  # Print initial messages
  intermediateMessage('NLA Bank Features metrics', loc='start')

    # Standardize arguments, then combine them into single dataframe as expected
    # in the rest of the function
    addParameter <- function(df, ...) {
        
        args <- list(...)
        
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(PARAMETER=args[[1]])
        return(rc)
        
    }
    angle <- aquametStandardizeArgument(angle, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','FLAT','GRADUAL','STEEP','NEAR_VERTICAL')), 'ANGLE', stopOnError = !isUnitTest)
    drawdown <- aquametStandardizeArgument(drawdown, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('character')), legalValues=list(VALUE=c(NA,'','N','NO','Y','YES')), 'DRAWDOWN', stopOnError = !isUnitTest)
    horizontalDistance <- aquametStandardizeArgument(horizontalDistance, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','double')), rangeLimits = list(VALUE=c(0,1000)), 'HORIZ_DIST', stopOnError = !isUnitTest)
    horizontalDistanceDrawdown <- aquametStandardizeArgument(horizontalDistanceDrawdown, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','double')), rangeLimits = list(VALUE=c(0,200)), 'HORIZ_DIST_DD', stopOnError = !isUnitTest)
    verticalHeight <- aquametStandardizeArgument(verticalHeight, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','double')), rangeLimits = list(VALUE=c(0,5)), 'VERT_HEIGHT', stopOnError = !isUnitTest)
    verticalHeightDrawdown <- aquametStandardizeArgument(verticalHeightDrawdown, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','double')), rangeLimits = list(VALUE=c(0,100)), 'VERT_HEIGHT_DD', stopOnError = !isUnitTest)

    df <- rbind(angle, drawdown, horizontalDistance, horizontalDistanceDrawdown, verticalHeight, verticalHeightDrawdown)
	stdData <- nlaBankFeatures.standardizeData(df)

	angleMets <- nlaBankFeatures.bankAngle(stdData)
	distanceMets <- nlaBankFeatures.distances(stdData) 

  bfMets <- rbind(angleMets, distanceMets)
	
  intermediateMessage(' Done.', loc='end')

  return(bfMets)
}

#' @keywords internal
nlaBankFeatures.standardizeData <- function(df)
# returns dataframe with standardized input data.
# NEAR VERTICAL is from 2007 and NEAR_VERTICAL_UNDERCUT is from 2012.  These
# are standardized here (and should eventually be changed in the database)
## TODO Standardize ANGLE values in 2007 and 2012 NLA tables.
{
	rc <- within(df, VALUE <- ifelse(PARAMETER %in% c('ANGLE') & 
							          VALUE %in% c('NEAR VERTICAL','NEAR_VERTICAL_UNDERCUT')
					 				 ,'NEAR_VERTICAL'
						  			 ,VALUE
									 )
				)
				
	return(rc)
}

#' @keywords internal
nlaBankFeatures.bankAngle <- function(df)
# calculates bank angle metrics.  Returns dataframe
# with columns SITE, PARAMETER, VALUE
{
	angles<-subset(df, PARAMETER=='ANGLE' & 
					   VALUE %in% c('FLAT','GRADUAL','STEEP','NEAR_VERTICAL')
				  )
	
	# mode of bank angle class
	modeAngle<-aggregate(list(VALUE=angles$VALUE)
						,list(SITE=angles$SITE)
						,modalValues, delim=', ', na.rm=TRUE
						)
	modeAngle$METRIC <- 'BFOANGLE'
	intermediateMessage('.1')
	
	
	# calculate fraction of lake with specific bank angles:
	flatAngle<-aggregate(list(VALUE=(angles$VALUE=='FLAT'))
						,list(SITE=angles$SITE)
						,protectedMean, na.rm=TRUE
						)
	flatAngle <- within(flatAngle
					   ,{METRIC <- 'BFFFLAT'
						 VALUE <- as.character(VALUE)
					 	}
					   )	
	
	gradualAngle<-aggregate(list(VALUE=(angles$VALUE=='GRADUAL'))
						   ,list(SITE=angles$SITE)
						   ,protectedMean, na.rm=TRUE
					 	   )
	gradualAngle <- within(gradualAngle
						  ,{METRIC <- 'BFFGRADUAL'
							VALUE <- as.character(VALUE)
						   }
						  )	
	
	steepAngle<-aggregate(list(VALUE=(angles$VALUE=='STEEP'))
						 ,list(SITE=angles$SITE)
						 ,protectedMean, na.rm=TRUE
						 )
	steepAngle$METRIC <- 'BFFSTEEP'
	steepAngle <- within(steepAngle
					   ,{METRIC <- 'BFFSTEEP'
						 VALUE <- as.character(VALUE)
						}
					   )	
	
	verticalAngle<-aggregate(list(VALUE=(angles$VALUE=='NEAR_VERTICAL'))
							,list(SITE=angles$SITE)
							,protectedMean, na.rm=TRUE
							)
	verticalAngle$METRIC <- 'BFFVERTICAL'
	verticalAngle <- within(verticalAngle
					   	   ,{METRIC <- 'BFFVERTICAL'
						   	 VALUE <- as.character(VALUE)
							}
					   	   )	
	
	
	# number of slopes recorded 
	nAngle<-aggregate(list(VALUE=I(angles$VALUE))
					 ,list(SITE=angles$SITE)
					 ,count
					 )
	nAngle$METRIC <- 'BFNANGLE'
	nAngle <- within(nAngle
					,{METRIC <- 'BFNANGLE'
					  VALUE <- as.character(VALUE)
					 }
					)	
	
	intermediateMessage('.2')

	rc <- rbind(modeAngle, flatAngle, gradualAngle, steepAngle, verticalAngle, nAngle)
	intermediateMessage('.3')
	
	return(rc)
}

#' @keywords internal
nlaBankFeatures.distances <- function(df)
# calculates metrics for horizontal distances and vertical heights.  
# Returns dataframe with columns SITE, METRIC, VALUE.
{
	# Modify drawdown data based on assumptions. This is only meaningful if 
	# DRAWDOWN data is present, but that is (currently) handled inside the 
	# function.
	intermediateMessage('.4')
	df <- nlaBankFeatures.fillinDistances(df)
	intermediateMessage('.5')
	
	
	# mean vertical and horizontal distances, and counts
	# Note: any conversion of results to numerics within subset()
	#   results in generation of warning messages about coercion 
	#   introducing NA results.
	tt <- subset(df, PARAMETER %in% c('VERT_HEIGHT','HORIZ_DIST', 'VERT_HEIGHT_DD','HORIZ_DIST_DD'))
	tt$VALUE <- as.numeric(tt$VALUE)
	
	meanDists<-aggregate(list(VALUE=tt$VALUE)
						,list(SITE=tt$SITE
							 ,PARAMETER=tt$PARAMETER
							 )
						,protectedMean, na.rm=TRUE
						)
	meanDists$METRIC <- with(meanDists
							   , ifelse(PARAMETER=='VERT_HEIGHT', 	'BFXVERTHEIGHT'
								,ifelse(PARAMETER=='HORIZ_DIST',  	'BFXHORIZDIST'
								,ifelse(PARAMETER=='VERT_HEIGHT_DD','BFXVERTHEIGHT_DD'
								,ifelse(PARAMETER=='HORIZ_DIST_DD', 'BFXHORIZDIST_DD', 'BFXUNKNOWN'
								 ))))
							   )
	meanDists$PARAMETER <- NULL	
	
	nDists<-aggregate(list(VALUE=tt$VALUE)
					 ,list(SITE=tt$SITE
						  ,PARAMETER=tt$PARAMETER
						  )
					 ,count
					 )
	nDists$METRIC <- with(nDists
							, ifelse(PARAMETER=='VERT_HEIGHT', 'BFNVERTHEIGHT'
							 ,ifelse(PARAMETER=='HORIZ_DIST',  'BFNHORIZDIST'
							 ,ifelse(PARAMETER=='VERT_HEIGHT_DD','BFNVERTHEIGHT_DD'
							 ,ifelse(PARAMETER=='HORIZ_DIST_DD', 'BFNHORIZDIST_DD', 'BFNUNKNOWN'
							  ))))
			 )
	nDists$PARAMETER <- NULL
	
	intermediateMessage('.6')
	rc <- rbind(meanDists, nDists)
}

#' @keywords internal
nlaBankFeatures.fillinDistances <- function(df)
# Used to fill in missing values of HORIZ_DIST_DD and VERT_HEIGHT_DD with zeros where
# it is safe to assume they are zero, i.e. when there is no drawdown noted.
#
# ARGUMENTS:
# df		dataframe with bank feature data.
#
# ASSUMPTIONS:
# PARAMETER values include at least one row of HORIZ_DIST_DD and VERT_HEIGHT_DD, otherwise
#   the expandDataFrame() call will not work.
# df has columns SITE, STATION, PARAMETER, VALUE and none other.
#
{
	distances <- subset(df
					   ,PARAMETER %in% c('DRAWDOWN','HORIZ_DIST','VERT_HEIGHT','HORIZ_DIST_DD','VERT_HEIGHT_DD')
			   		   ,select=c(SITE, STATION, PARAMETER, VALUE)
					   )
	
	# Make absent VERT_DIST_DD and HORIZ_DIST_DD values present in data with NA values.
	# This is done so they can be set to zero later.
	indivSites <- lapply(unique(distances$SITE)
				 		,function(uid) {
							
							thisSite <- subset(distances, SITE==uid)
									
							if('DRAWDOWN' %in% thisSite$PARAMETER) {
										
								# make sure 'HORIZ_DIST_DD','VERT_HEIGHT_DD' occur in this visit data
								tt <- rbind(thisSite
										   ,data.frame(SITE=uid
							   						  ,STATION='DELETEME'
								  					  ,PARAMETER=c('HORIZ_DIST_DD','VERT_HEIGHT_DD')
								  					  ,VALUE=as.character(NA)
								  					  ,stringsAsFactors=FALSE
								  				  	  )
							   		   	   )
									
								# Add absent rows for this SITE and then remove the rows 'seeded' above.
								rc <- subset(expandDataFrame(tt, c('SITE','STATION','PARAMETER'))
											,STATION != 'DELETEME'
											)
													
							} else {
										
								# Change nothing as we can assume nothing without DRAWDOWN
								rc <- thisSite	
									
							}	
									
							return(rc)
						 }
						)
	dfExpanded <- do.call(rbind, indivSites)
										 
						 
	# Determine rows where missing distances can be assumed to be zero unless 
	# recorded as otherwise.
	correctableValues <- subset(distances, PARAMETER=='DRAWDOWN' & VALUE %in% c('N','NO'))
	
	
	# Change missing values to zero when DRAWDOWN is 'NO'. 
	dfFilledin <- within(dfExpanded
						,VALUE <- ifelse(paste(SITE, STATION) %in% with(correctableValues, paste(SITE, STATION))
										  & PARAMETER %in% c('HORIZ_DIST_DD','VERT_HEIGHT_DD')
										  & is.na(VALUE)
										 ,'0'
										 ,VALUE
										 )
						)
						
	
	return(dfFilledin)
}

# end of file