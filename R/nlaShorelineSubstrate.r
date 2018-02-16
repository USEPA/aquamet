#' @export
#' @title Calculate NLA Shoreline Substrate Metrics
#' @description This function calculates the shoreline substrate portion 
#' of the physical habitat metrics for National Lakes Assessment (NLA) data.  
#' The function requires a data frame containing validated physical habitat 
#' data collected using the NLA protocol.
#' @param bedrock A data frame containing bedrock class values, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the bedrock bottom substrate cover
#' category.
#' }
#' @param boulder A data frame containing boulder class values, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the boulder bottom substrate cover
#' category.
#' }
#' @param cobble A data frame containing cobble bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the cobble bottom substrate cover
#' category.
#' }
#' @param gravel A data frame containing gravel bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the gravel bottom substrate cover
#' category.
#' }
#' @param organic A data frame containing organic bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the organic bottom substrate cover
#' category.
#' }
#' @param other A data frame containing other bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the other bottom substrate cover
#' category.
#' }
#' @param sand A data frame containing sand bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the sand bottom substrate cover
#' category.
#' }
#' @param silt A data frame containing silt bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the silt bottom substrate cover
#' category.
#' }
#' @param wood A data frame containing wood bottom substrate class 
#' values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the wood bottom substrate cover
#' category.
#' }
#' @param substrateCovers a data frame containing substrate cover category 
#' values (VALUE), the lower proportional cover value for each category (cover),
#' and an indicator variable of presence of a substrate type for each category.
#' The default values are:
#' \itemize{
#' \item VALUE '0', '1', '2', '3', '4', NA
#' \item cover 0, 0.05, 0.25, 0.575, 0.875, NA
#' \item presence 0, 1, 1, 1, 1, NA
#' }  
#' @param substrateSizes a data frame containing substrate class names (CLASS)
#' and corresponding geometric mean of diameter ranges in mm (diam), as well as
#' an indicator of whether to include the class in estimates of substrate size
#' for the site. 
#' The default values are: 
#' \itemize{
#' \item CLASS c('BEDROCK', 'BOULDERS','COBBLE', 'GRAVEL', 'SAND', 'SILT', 
#' 'ORGANIC', 'WOOD')
#' \item diam c(5656.854, 1000, 126.4911, 11.31371, 0.3464102, 0.007745967, 
#' NA, NA)  
#' \item inPopulationEstimate c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
#' } 
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics include:
#' SSFPBEDROCK, SSFPBOULDERS, SSFPCOBBLE, SSFPGRAVEL, SSFPORGANIC, SSFPOTHER
#' SSFPSAND, SSFPSILT, SSFPWOOD, SSFCBEDROCK, SSFCBOULDERS, SSFCCOBBLE
#' SSFCGRAVEL, SSFCORGANIC, SSFCOTHER, SSFCSAND, SSFCSILT, SSFCWOOD
#' SSISTAVARIETY, SSISITEVARIETY, SSVBEDROCK, SSVBOULDERS, SSVCOBBLE, SSVGRAVEL
#' SSVORGANIC, SSVOTHER, SSVSAND, SSVSILT, SSVWOOD, SSNBEDROCK
#' SSNBOULDERS, SSNCOBBLE, SSNGRAVEL, SSNORGANIC, SSNOTHER, SSNSAND
#' SSNSILT, SSNWOOD, SSXLDIA, SSVLDIA, SS16LDIA, SS25LDIA
#' SS50LDIA, SS75LDIA, SS84LDIA, SSOPCLASS, SSOFCLASS
#'  
#' Descriptions for all metrics are included in 
#' \emph{NLA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'   head(nlaPhabEx)
#'   
#'   bedrock <- subset(nlaPhabEx,PARAMETER=='SS_BEDROCK',select=-PARAMETER)
#'   boulder <- subset(nlaPhabEx,PARAMETER=='SS_BOULDERS',select=-PARAMETER)
#'   cobble <- subset(nlaPhabEx,PARAMETER=='SS_COBBLE',select=-PARAMETER)
#'   gravel <- subset(nlaPhabEx,PARAMETER=='SS_GRAVEL',select=-PARAMETER)
#'   organic <- subset(nlaPhabEx,PARAMETER=='SS_ORGANIC',select=-PARAMETER)
#'   other <- subset(nlaPhabEx,PARAMETER=='SS_OTHER',select=-PARAMETER)
#'   sand <- subset(nlaPhabEx,PARAMETER=='SS_SAND',select=-PARAMETER)
#'   silt <- subset(nlaPhabEx,PARAMETER=='SS_SILT',select=-PARAMETER)
#'   wood <- subset(nlaPhabEx,PARAMETER=='SS_WOOD',select=-PARAMETER)
#'   
#'   exShoreSubstrate <- nlaShorelineSubstrate(bedrock,boulder,
#'   cobble,gravel,organic,other,sand,silt,wood)
#'   
#'   head(exShoreSubstrate)
#'  
#' @keywords survey
nlaShorelineSubstrate <- function(bedrock = NULL
	                             ,boulder = NULL
	                             ,cobble = NULL
	                             ,gravel = NULL
	                             ,organic = NULL
	                             ,other = NULL
	                             ,sand = NULL
	                             ,silt = NULL
	                             ,wood = NULL
                                 ,substrateCovers=data.frame(VALUE	= c(NA, '0', '1', '2', '3', '4')
                       	                                    ,cover	= c(NA, 0, 0.05, 0.25, 0.575, 0.875)
					   	                                    ,presence= as.integer(c(NA, 0, 1, 1, 1, 1))
                       	                                    ,stringsAsFactors=FALSE
                       	                                    )
                                 ,substrateSizes=data.frame(CLASS=c('BEDROCK', 'BOULDERS'
                                                                   ,'COBBLE',  'GRAVEL'
                                                                   ,'SAND',    'SILT'
                                                                   ,'ORGANIC', 'WOOD'
                                                                   )
                                                           ,diam=c(gmean(c(4000,8000)), gmean(c(250,4000))
                       	                                          ,gmean(c(64,250)),    gmean(c(2,64))
                       	                                          ,gmean(c(0.06,2)),    gmean(c(0.001,0.06))
                       	                                          ,NA,                  NA
                       	                                          )
                                                           ,inPopulationEstimate = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
                                                           ,stringsAsFactors=FALSE
                                                           )
                                 ) {

################################################################################
# Function: nlaShorelineSubstrate
# Title: Calculate NLA Shoreline Substrate Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 7, 2008
# Description:
#   This function calculates the shoreline substrate portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.
# Function Revisions:
#   10/07/08 cws: quantile calculations using type 3 algorithm to mimic SAS 
#            results.
#   10/16/08 cws: changed to use normalizedCover() for all cover mets; was just
#            normalizing the diameter percentile mets using inline code.
#   10/17/08 cws: changed to use type 2 quantile calculation method, as this
#            actually matches SAS output using proc univariate.
#   10/21/08 cws: Correcting counts of missing parameters.  These are now zero.
#   02/27/09 cws: Calculation of ssiSiteVariety corrected.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   12/06/13 cws: Added unit test based on 2007 data, RESULTs assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).  The metrics function was changed
#            to set those counts to integer 0 within the function and to 
#            return a long dataframe instead of a wide one.  The metrics
#            function was also changed to not return NA values for substrates
#            which were not recorded at a site (e.g. 7533 has no gravel, so
#            ssfcGravel, ssvGravel and ssfpGravel were NA but now are no longer
#            reported).  These NA values were an artifact of the 'wide'
#            organization of the output, and are commented out in the code
#            creating the expected values.
#   12/11/13 cws: Completed unit test with 2007 data. Corrected SAS results for
#            SSISITEVARIETY values for 7533, 7682 and 7961. Upcased metric
#            names.  Regression test using entire 2007 dataset shows the
#            following differences which are all expected:  SSISITEVARIETY is
#            changed from NA to the correct count in 19 sites (7518, 7533, 7534,
#            7598, 7682, 7724, 7760, 7791, 7818, 7862, 7895, 7926, 7956, 7961,
#            8069, 8101, 8797, 8845, 8846) which are the only sites with absent
#            substrate data.  Also SSN* counts at 57 sites which have no
#            shoreline substrate data are NA instead of 0.  Also the values of
#            SSOPCLASS and SSOFCLASS have all changed from NA to actual values.
#   06/12/14 tmk: Removed calls to the require() function.
#    7/17/17 cws Renamed from metsShorelineSubstrate and updated to new calling
#            interface, changing UID, and RESULT to SITE and VALUE, and changing
#            output column PARAMETER to METRIC.  Added arguments substrateCovers
#            and substrateSizes, just like nlaBottomSubstrate; code will need
#            some serious cleanup.
#
# Arguments:
#   df = a data frame containing shoreline substrate data.  The data frame must
#     include columns that are named as follows:
#       SITE - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single SITE.
#               For NLA, transect is used for this purpose.
#       PARAMETER - parameter name, which identifies the variables used in
#                   calculations. In wide data frame format, this argument
#                   would be used to name columns.  It is assumed that this
#                   argument has the following values: SS_BEDROCK, SS_BOULDERS, 
#                   SS_COBBLE, SS_GRAVEL, SS_SAND, SS_SILT, SS_ORGANIC,
#                   SS_OTHER, and SS_WOOD.
#       VALUE - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
# Output:
#   A data frame that contains the following columns:
#     SITE - universal ID value
#     PARAMETER - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    # Print initial messages
    intermediateMessage('Shoreline substrate metrics', loc='start')
    
    addClass <- function(df, ...) {
        
        args <- list(...)
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(CLASS = args[[1]])
        return(rc)
    }

    bedrock <- bedrock %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'BEDROCK')
	boulder <- boulder %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'BOULDERS')
	cobble <- cobble %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'COBBLE')
	gravel <- gravel %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'GRAVEL')
	organic <- organic %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'ORGANIC')
	other <- other %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'OTHER')
	sand <- sand %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'SAND')
	silt <- silt %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'SILT')
	wood <- wood %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')),  'WOOD')
	
	substrateCovers <- substrateCovers %>% aquametStandardizeArgument(struct = list(VALUE='character', cover='double', presence = 'integer'))
	substrateSizes <- substrateSizes %>% aquametStandardizeArgument(struct = list(CLASS='character', diam='double', inPopulationEstimate='logical'))
	
	if(is.null(substrateSizes)) {
	    return("  This function requires substrate class size information as an argument.  You might consider using the default values by removing the value from your call.")
	} else if (nrow(substrateSizes) == 0) {
	    return("  This function requires substrate class size information as an argument.  You might consider using the default values by removing the value from your call.")
	}
	
	ssData <- rbind(bedrock, boulder, cobble, gravel, organic, other, sand, silt, wood)
	
    intermediateMessage('.1')


  # Calculate fractional presence of each substrate class
  ssPresence <- merge(ssData, substrateCovers
                     ,by.x='VALUE', by.y='VALUE'
                     ,all.x=TRUE
                     )
  tt <- aggregate(list(VALUE = ssPresence$presence)
                 ,list('SITE'=ssPresence$SITE
                      ,'CLASS'=ssPresence$CLASS
                      ) 
                 ,mean, na.rm=TRUE
                 )
  meanPresence <- within(tt
				        ,METRIC <- ifelse(CLASS == 'BEDROCK', 'SSFPBEDROCK'
							      ,ifelse(CLASS == 'BOULDERS', 'SSFPBOULDERS'
								  ,ifelse(CLASS == 'COBBLE', 'SSFPCOBBLE'
								  ,ifelse(CLASS == 'GRAVEL', 'SSFPGRAVEL'
								  ,ifelse(CLASS == 'SAND', 'SSFPSAND'
								  ,ifelse(CLASS == 'SILT', 'SSFPSILT'
								  ,ifelse(CLASS == 'ORGANIC', 'SSFPORGANIC'
								  ,ifelse(CLASS == 'WOOD', 'SSFPWOOD'
								  ,ifelse(CLASS == 'OTHER', 'SSFPOTHER', 'SSFPUNKNOWN'
								   )))))))))
					    ) %>% 
                  select(SITE, METRIC, VALUE)

  intermediateMessage('.3')


  # Calculate variety metrics: 
  #     ssiStaVariety = mean number of substrate classes at each station.
  #     ssiSiteVariety = total number of substrate classes present in site
  # The number of substrate classes at a station is the sum of their presences.
  # Note: SS_OTHER is not included in site variety index
  tt <- aggregate(list(sum=ssPresence$presence)
                 ,list('SITE'=ssPresence$SITE
                      ,'STATION'=ssPresence$STATION
                      )
                 ,sum, na.rm=TRUE
                 )
  ssiVariety <- aggregate(list(VALUE=tt$sum)
                            ,list('SITE'=tt$SITE)
                            ,mean, na.rm=TRUE
                            )
  ssiVariety$METRIC <- 'SSISTAVARIETY'

  tt <- subset(meanPresence, METRIC != 'SSFPOTHER')
  ssiSiteVariety <- aggregate(list(VALUE = tt$VALUE)
  							 ,list(SITE=tt$SITE)
					 		 ,function(x) {
								 sum(ifelse(x>0, 1, 0), na.rm=TRUE)		# sum up substrate presences
							  }
					  		 )
  ssiSiteVariety$METRIC <- 'SSISITEVARIETY'
  ssiVariety <- rbind(ssiVariety, ssiSiteVariety)

  intermediateMessage('.4')


  # Normalize covers of all shore substrate classes. 
  sscover <- merge(ssData, substrateCovers, by.x='VALUE', by.y='VALUE', all.x=TRUE) %>% merge(substrateSizes, by='CLASS', all.x=TRUE)
  sscover <- normalizedCover(sscover, 'cover', 'normCover')
  intermediateMessage('.')
  
  # calculate mean cover of each substrate class
  tt <- aggregate(list(VALUE = sscover$normCover)
                 ,list('SITE'=sscover$SITE
                      ,'CLASS'=sscover$CLASS
                      ) 
                 ,mean, na.rm=TRUE
                 )
#
  meanCover <- within(tt
					 ,METRIC <- ifelse(CLASS == 'BEDROCK', 'SSFCBEDROCK'
					 		   ,ifelse(CLASS == 'BOULDERS', 'SSFCBOULDERS'
							   ,ifelse(CLASS == 'COBBLE', 'SSFCCOBBLE'
							   ,ifelse(CLASS == 'GRAVEL', 'SSFCGRAVEL'
							   ,ifelse(CLASS == 'SAND', 'SSFCSAND'
							   ,ifelse(CLASS == 'SILT', 'SSFCSILT'
							   ,ifelse(CLASS == 'ORGANIC', 'SSFCORGANIC'
							   ,ifelse(CLASS == 'WOOD', 'SSFCWOOD'
							   ,ifelse(CLASS == 'OTHER', 'SSFCOTHER', 'SSFCUNKNOWN'
							    )))))))))
					 ) %>% 
                  select(SITE, METRIC, VALUE)

  intermediateMessage('.5')

  # calculate stdev cover of each substrate class
  tt <- aggregate(list(VALUE = sscover$normCover)
                 ,list('SITE'=sscover$SITE
                      ,'CLASS'=sscover$CLASS
                      ) 
                 ,sd, na.rm=TRUE
                 )
  sdCover <- within(tt
				   ,{METRIC <- ifelse(CLASS == 'BEDROCK', 'SSVBEDROCK'
						      ,ifelse(CLASS == 'BOULDERS', 'SSVBOULDERS'
							  ,ifelse(CLASS == 'COBBLE', 'SSVCOBBLE'
							  ,ifelse(CLASS == 'GRAVEL', 'SSVGRAVEL'
							  ,ifelse(CLASS == 'SAND', 'SSVSAND'
							  ,ifelse(CLASS == 'SILT', 'SSVSILT'
							  ,ifelse(CLASS == 'ORGANIC', 'SSVORGANIC'
							  ,ifelse(CLASS == 'WOOD', 'SSVWOOD'
							  ,ifelse(CLASS == 'OTHER', 'SSVOTHER', 'SSVUNKNOWN'
							   )))))))))
					   
				    }
		   		   ) %>% 
                  select(SITE, METRIC, VALUE)

  intermediateMessage('.6')


  # count valid cover of each substrate class.  Fill in missing (NA) counts with 0, as was
  # done elsewhere in 2007.
  tt <- aggregate(list(VALUE = sscover$normCover)
                 ,list('SITE'=sscover$SITE
                      ,'CLASS'=sscover$CLASS
                      ) 
                 ,count
                 )
  tt <- within(expand.data.frame(tt, c('SITE','CLASS'))
			  ,VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			  )
  countSubstrates <- within(tt
						   ,{METRIC <- ifelse(CLASS == 'BEDROCK', 'SSNBEDROCK'
									  ,ifelse(CLASS == 'BOULDERS', 'SSNBOULDERS'
									  ,ifelse(CLASS == 'COBBLE', 'SSNCOBBLE'
									  ,ifelse(CLASS == 'GRAVEL', 'SSNGRAVEL'
									  ,ifelse(CLASS == 'SAND', 'SSNSAND'
									  ,ifelse(CLASS == 'SILT', 'SSNSILT'
									  ,ifelse(CLASS == 'ORGANIC', 'SSNORGANIC'
									  ,ifelse(CLASS == 'WOOD', 'SSNWOOD'
									  ,ifelse(CLASS == 'OTHER', 'SSNOTHER', 'SSNUNKNOWN'
									   )))))))))
						    }
						   ) %>% 
                  select(SITE, METRIC, VALUE)
						   
						   
  intermediateMessage('.7')


  # Calculations using characteristic diameters of the substrate are based
  # on mean diameter*cover values at each transect.  Cover values are 
  # normalized prior to their use as weights for these means.
  #
  # Determine mean diameters, weighted by the normalized covers.  Note that
  # the normalization includes non-mineral substrates, but the following
  # calculations do not take them into account as they have no meaningful 
  # diameter.
  mineralCover <- subset(sscover, inPopulationEstimate
                        # ,CLASS %in% c('BEDROCK','BOULDERS','COBBLE'
                        #                  ,'GRAVEL', 'SAND', 'SILT'
                        #                  )
                        ,select=names(sscover)[names(sscover) != 'normCover']
                  )
  mineralCover <- normalizedCover(mineralCover, 'cover', 'normCover')
  
  # tt <- merge(mineralCover, substrateSizes
  #            ,by.x='CLASS', by.y='CLASS'
  #            ,all.x=TRUE
  #            ) %>%
  #       mutate(lDiam = log10(diam))
  #  tt$wtLDiam <- tt$lDiam * tt$normCover
    mineralCover <- mineralCover %>% mutate(lDiam = log10(diam), wtLDiam = lDiam * normCover)

  diamSubstrate <- aggregate(list(meanLDiam = mineralCover$wtLDiam)
                            ,list('SITE'=mineralCover$SITE
                                 ,'STATION'=mineralCover$STATION
                                 ) 
                            ,mean, na.rm=TRUE
                            )

  intermediateMessage('.8')


  # Estimate measures of diameter populations
  meanLDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,mean, na.rm=TRUE
                 )
  meanLDia$METRIC <- 'SSXLDIA'

  sdLDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,sd, na.rm=TRUE
                 )
  sdLDia$METRIC <- 'SSVLDIA'

  p16LDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                 )
  p16LDia$METRIC <- 'SS16LDIA'

  p25LDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                 )
  p25LDia$METRIC <- 'SS25LDIA'

  p50LDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                 )
  p50LDia$METRIC <- 'SS50LDIA'

  p75LDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                 )
  p75LDia$METRIC <- 'SS75LDIA'

  p84LDia <- aggregate(list(VALUE = diamSubstrate$meanLDiam)
                 ,list('SITE'=diamSubstrate$SITE)
                 ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                 )
  p84LDia$METRIC <- 'SS84LDIA'


  intermediateMessage('.9')


  # Determine most common substrate class by presence and by cover
  # (requires same site visits in meanPresence and meanCover in same order)
  #
  # Initialize these metrics with missing values, and add classes
  tt <- dcast(rbind(meanPresence,meanCover), SITE~METRIC, value.var='VALUE')
  modeClasses <- subset(tt, select='SITE')
  modeClasses$SSOPCLASS <- NA
  modeClasses$SSOFCLASS <- NA

  for(i in 1:nrow(tt)) {
      modeClasses$SSOPCLASS[i] <- modalClass(tt[i,]
                                            ,c('SSFPBEDROCK', 'SSFPBOULDERS'
                                              ,'SSFPCOBBLE',  'SSFPGRAVEL'
                                              ,'SSFPSAND',    'SSFPSILT'
                                              ,'SSFPORGANIC', 'SSFPWOOD'
                                              ,'SSFPOTHER'
                                              )
                                            ,c('Bedrock', 'Boulders'
                                              ,'Cobble',  'Gravel'
                                              ,'Sand',    'Silt'
                                              ,'Organic', 'Wood'
                                              ,'Other'
                                              )
                                            )
      modeClasses$SSOFCLASS[i] <- modalClass(tt[i,]
                                            ,c('SSFCBEDROCK', 'SSFCBOULDERS'
                                              ,'SSFCCOBBLE',  'SSFCGRAVEL'
                                              ,'SSFCSAND',    'SSFCSILT'
                                              ,'SSFCORGANIC', 'SSFCWOOD'
                                              ,'SSFCOTHER'
                                              )
                                            ,c('Bedrock', 'Boulders'
                                              ,'Cobble',  'Gravel'
                                              ,'Sand',    'Silt'
                                              ,'Organic', 'Wood'
                                              ,'Other'
                                              )
                                            )
  }

  modeClasses <- within(melt(modeClasses, 'SITE'
						    ,variable.name='METRIC'
			                ,value.name='VALUE'
			                )
					   ,METRIC <- as.character(METRIC)
			   		   )
					   
  intermediateMessage('.10')

  # combine VALUEs into a dataframe
  ssMets <- rbind(meanPresence, meanCover, ssiVariety, sdCover, countSubstrates
				 ,meanLDia, sdLDia, p16LDia, p25LDia, p50LDia, p75LDia, p84LDia
				 ,modeClasses
				 )
  intermediateMessage(' Done.', loc='end')

  return(ssMets)

}


# end of file