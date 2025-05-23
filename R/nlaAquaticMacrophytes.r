#' @export
#' @title Calculate NLA Aquatic Macrophyte Metrics
#' @description This function calculates the aquatic macrophyte portion of the physical
#' habitat metrics for National Lakes Assessment (NLA) data.  
#' @param emergent A data frame containing emergent vegetation class values for sites 
#' sampled with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the emergent macrophyte cover
#' category.
#' }
#' @param floating A data frame containing floating vegetation class value for 
#' sites sampled with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the floating macrophyte cover
#' category.
#' }
#' @param submergent A data frame containing submergent class value for sites 
#' sampled, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the subemergent macrophyte cover
#' category.
#' }
#' @param totalCover A data frame containing total cover class value for sites 
#' sampled, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the total macrophyte cover
#' category.
#' }
#' @param dataInformation A data frame containing aquatic macrophyte 
#' categorical values and corresponding numeric cover values used in 
#' calculations. as well as indicators of cover type presence or absence 
#' for each category. The default values (and required column names) are:
#' \itemize{
#' \item value c(NA,'0','1','2','3','4')
#' \item weights c(NA,0,0.05,0.25,0.575,0.875)
#' \item presence c(NA,FALSE,TRUE,TRUE,TRUE,TRUE) 
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
#' AMFPEMERGENT, AMFPFLOATING, AMFPSUBMERGENT, AMFPALL, AMFCEMERGENT,   
#' AMFCFLOATING, AMFCSUBMERGENT, AMFCALL, AMVEMERGENT, AMVFLOATING,    
#' AMVSUBMERGENT, AMVALL, AMNEMERGENT, AMNFLOATING, AMNSUBMERGENT, AMNALL,
#' AMIQALL, AMIDALL, AMITOTAL 
#' 
#' Descriptions for all NLA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NLA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}. 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'   head(nlaPhab)
#'   
#'   # Subset dataset into individual parameter inputs, keeping only SITE, 
#'   #   STATION, and VALUE
#'   emerg <- subset(nlaPhab,PARAMETER=='AM_EMERGENT',select=-PARAMETER)
#'   float <- subset(nlaPhab,PARAMETER=='AM_FLOATING',select=-PARAMETER)
#'   submerg <- subset(nlaPhab,PARAMETER=='AM_SUBMERGENT',select=-PARAMETER)
#'   totcvr <- subset(nlaPhab,PARAMETER=='AM_TOTALCOVER',select=-PARAMETER)
#'   
#'   # Use above dataframes as inputs to function
#'   exAquMacro <- nlaAquaticMacrophytes(emergent=emerg,
#'      floating=float,
#'      submergent=submerg,
#'      totalCover=totcvr)
#'   
#'   head(exAquMacro)
#'  
#' 
nlaAquaticMacrophytes <- function(emergent=NULL, floating=NULL, submergent=NULL, totalCover=NULL
                                 ,dataInformation = data.frame(value    = c(NA,'0','1','2','3','4')
                                                              ,weights  = c(NA,0,0.05,0.25,0.575,0.875)
                                                              ,presence = c(NA,0L,1L,1L,1L,1L) %>% as.logical()
                                                              ,stringsAsFactors=FALSE
                                                              )
                                 ,isUnitTest = FALSE
                                 ) {

################################################################################
# Function: nlaAquaticMacrophytes
# Title: Calculate NLA Aquatic Macrophyte Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 14, 2008
# Description:
#   This function calculates the aquatic macrophyte portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.
# Function Revisions:
#   10/14/08 cws: Cover values normalized at each station/subid prior to metrics
#            calculation.
#   10/20/08 cws: Removed normalization, as individual AM categories are 
#            independent.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   12/24/13 cws: Developed unit test for 2007 data and refactored metrics
#            function, updating column names and upcasing parameter names.
#            Using modalValues() instead of single use code.  Regression test
#            with 2007 data shows the following expected differences: 1103 NA
#            and 0 values for 57 sites which have no AM data and 2 more sites
#            with only AM_EMERGENT and AM_FLOATING data; 145 values which differ
#            due to floating point issues.  No separate unit check using 2012
#            data was created because the data has not changed.  Refactored
#            metrics code. Regression test with 2007 passes but differently than
#            described above: 1103 NA and 0 values absent as above, but only one
#            difference due to floating point issues, AMVFLOATING at 8465.
#   06/12/14 tmk: Removed calls to the require() function.
#    6/29/17 cws Renamed from metsAquaticMacrophytes to nlaAquaticMacrophytes.
#            Changed argument from single dataframe to one dataframe per value,
#            as was done with the NRSA metrics functions.  Call interface 
#            modified. Changed UID to SITE and RESULT to VALUE throughout, and
#            returning metric name in METRIC rather than PARAMETER, but
#            otherwise left the body of the function intact. Returning NULL if
#            arguments contain no data.
#    3/19/19 cws Added isUnitTest argument for consistency.
#    3/21/19 cws Added coverCalculationValues argument. Using it to validate
#            data arguments
#    3/28/19 cws Standardized metadata argument naming
#   12/31/19 cws Allowing SITE to be character as well as integer in all arguments.
#    9/25/20 cws Removing reshape2 functions in favour of tidyr due to deprecation.
#    7/01/21 cws Converted tidyr functions gather to pivot_longer and spread to
#            pivot_wider
#    6/12/24 cws Removing reliance on plyr package by rewriting call to ddply to 
#            use group_by/summarise instead
#
# Arguments:
#   df = a data frame containing aquatic macrophyte data.  The data frame must
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
#                   argument has the following values: AM_EMERGENT, AM_FLOATING,
#                   AM_SUBMERGENT, and AM_TOTALCOVER.
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
  intermediateMessage('NLA Aquatic Macrophyte metrics', loc='start')

    # Standardize arguments, then combine them into single dataframe as expected
    # in the rest of the function
    addParameter <- function(df, ...) {
        
        args <- list(...)
        
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(PARAMETER=args[[1]])
        return(rc)
        
    }
    
    dataInformation <- aquametStandardizeArgument(dataInformation
                                                 ,struct = list(value=c('character','integer'), weights='double', presence=c('logical','integer'))
                                                 ,rangeLimits = list(weights=c(0,1))
                                                 ,legalValues = list(value=c(NA,'','0','1','2','3','4'), presence=c(NA,FALSE,TRUE))
                                                 ,stopOnError = !isUnitTest
                                                 )

    emergent <- aquametStandardizeArgument(emergent, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'', dataInformation$value)), 'AM_EMERGENT', stopOnError = !isUnitTest)
    floating <- aquametStandardizeArgument(floating, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'', dataInformation$value)), 'AM_FLOATING', stopOnError = !isUnitTest)
    submergent <- aquametStandardizeArgument(submergent, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'', dataInformation$value)), 'AM_SUBMERGENT', stopOnError = !isUnitTest)
    totalCover <- aquametStandardizeArgument(totalCover, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'', dataInformation$value)), 'AM_TOTALCOVER', stopOnError = !isUnitTest)

    coverCalculationValues <- dplyr::rename(dataInformation, field=value, calc=weights)
    
    df <- rbind(emergent, floating, submergent, totalCover)
    if(is.null(df)) {
        intermediateMessage(' -- arguments contain no data', loc='end')
        return(NULL)
    }
    
    ########## End of changes to function
    
    
  # Subset the input data frame
  amData <- subset(df, PARAMETER %in% c('AM_EMERGENT', 'AM_FLOATING'
                                     ,'AM_SUBMERGENT', 'AM_TOTALCOVER'
                                     )
                    ,c(SITE,STATION,PARAMETER,VALUE)
                    )

    # Create tables for converting field values to calculation values
    cover04 <- coverCalculationValues %>% select(field, calc)
    presence04 <- coverCalculationValues %>% mutate(calc=presence) %>% select(field, calc)
    # cover04<-data.frame(field=c(NA,'0','1','2','3','4')
    #                    ,calc=c(NA,0,0.05,0.25,0.575,0.875)
    #                    ,stringsAsFactors=FALSE
    #                    )
    # 
    # presence04<-data.frame(field=c(NA,'0','1','2','3','4')
    #                       ,calc=c(NA,0,1,1,1,1)
    #                       ,stringsAsFactors=FALSE
    #                       )

    intermediateMessage('.1')

    # Calculate fractional presence and cover values of individual cover classes
	indivCovers <- nlaAquaticMacrophytes.individualCover(amData, presence04, cover04)
	
	intermediateMessage('.2')
	
	
    # Calculate interquartile range of cover in total layer
	amCover<-merge(amData, cover04
				  ,by.x='VALUE'
				  ,by.y='field'
				  ,all.x=TRUE, sort=FALSE
				  )
	tt<-subset(amCover, PARAMETER=='AM_TOTALCOVER')
    iqrCover<-aggregate(list(VALUE=tt$calc)
                       ,list(SITE=tt$SITE)
                       ,iqr
                       )
 	iqrCover$METRIC <- 'AMIQALL'   
    

    # Calculate interdecile range of cover in total layer
    idrCover<-aggregate(list(VALUE = tt$calc)
                       ,list(SITE = tt$SITE)
                       ,idr
                       )
    idrCover$METRIC <- 'AMIDALL'
    				 
    intermediateMessage('.3')


    # Calculate index of total macrophyte cover, defined as the mean of
    # the sum of cover values at each transect
	amiTotal <- nlaAquaticMacrophytes.indices(amCover)

    intermediateMessage('.4')


    # combine metrics for aquatic macrophytes, sort result
    amMets <- within(rbind(indivCovers, iqrCover, idrCover, amiTotal)
	                ,VALUE <- as.character(VALUE)
					)
    intermediateMessage(' Done.', loc='end')

    return(amMets)
}

#' @keywords internal
nlaAquaticMacrophytes.individualCover <- function(df, presenceWeights, coverWeights)
# Determines fractional presence, fractional cover, cover sd and cover counts. 
# Returns dataframe with columns SITE, METRIC, VALUE
{
	# Fractional presence
	amPresence<-merge(df
					 ,presenceWeights
					 ,by.x='VALUE'
					 ,by.y='field'
					 ,all.x=TRUE
					 ,sort=FALSE
					 )
	
	tt<-aggregate(list(VALUE = amPresence$calc)
				 ,list(SITE = amPresence$SITE
					  ,PARAMETER = amPresence$PARAMETER
					  )
				,mean, na.rm=TRUE
				)
	meanPresence<-within(tt
						,{PARAMETER <- ifelse(PARAMETER == 'AM_EMERGENT', 'AMFPEMERGENT'
									  ,ifelse(PARAMETER == 'AM_FLOATING', 'AMFPFLOATING'
									  ,ifelse(PARAMETER == 'AM_SUBMERGENT', 'AMFPSUBMERGENT'
									  ,ifelse(PARAMETER == 'AM_TOTALCOVER', 'AMFPALL', 'AMFPUNKNOWN'
									   ))))
						 }
						)
	
	intermediateMessage('a')
	
	
	# Mean, sd and counts of midpoint cover.  These are not normalized
	# since they are all independent categories.
	amCover<-merge(df, coverWeights
			,by.x='VALUE'
			,by.y='field'
			,all.x=TRUE, sort=FALSE
	)
	
	# tt<-ddply(subset(amCover, grepl('^AM.+',PARAMETER))  # OLD CODE
	# 		 ,c('SITE','PARAMETER')
	# 		 ,summarise
	# 		 ,FC = mean(calc, na.rm=TRUE)
	# 		 ,V  = sd(calc, na.rm=TRUE)
	# 		 ,N  = length(na.omit(calc))
	# 		 )
	tt <- amCover %>%                                      # NEW CODE
	      subset(grepl('^AM.+',PARAMETER)) %>%
	      group_by(SITE, PARAMETER) %>%
	      summarise(FC = mean(calc, na.rm=TRUE)
			           ,V  = sd(calc, na.rm=TRUE)
			           ,N  = length(na.omit(calc))
			           )
    coverMets <- tt %>% 
                 #tidyr::gather(key='met', value='VALUE', c('FC','V','N')) %>%
                 tidyr::pivot_longer(c(FC,V,N), names_to='met', values_to='VALUE') %>%
                 mutate(PARAMETER = paste0('AM', met
				    	    			  ,ifelse(PARAMETER=='AM_TOTALCOVER', 'ALL', gsub('^AM_(.+)$', '\\1', PARAMETER))
							    	      )
    			        ,met = NULL
                        )
	intermediateMessage('b')
			
	rc <- rbind(meanPresence, coverMets) %>% dplyr::rename(METRIC=PARAMETER)
}


#' @keywords internal
nlaAquaticMacrophytes.indices <- function(df)
# Calculates AMITOTAL defined as the mean of the sum of cover values at each transect
# Returns dataframe with columns SITE, METRIC, VALUE
{
	qq <- subset(df, PARAMETER %in% c('AM_EMERGENT', 'AM_FLOATING', 'AM_SUBMERGENT'))
	
	tt <- aggregate(qq$calc
				   ,list(SITE = qq$SITE
						,STATION = qq$STATION
						)
				   ,sum, na.rm=TRUE
				   )
	
	amiTotal <- aggregate(list(VALUE = tt$x)
						 ,list(SITE = tt$SITE)
						 ,mean, na.rm=TRUE
						 )
	amiTotal$METRIC <- 'AMITOTAL'
	
	return(amiTotal)
}



# end of file
