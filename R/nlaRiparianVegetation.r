nlaRiparianVegetation <- function(bigTrees = NULL
                                 ,bigTrees_dd = NULL
                                 ,smallTrees = NULL
                                 ,smallTrees_dd= NULL
                                 ,canopyType = NULL
                                 ,canopyType_dd= NULL
                                 ,groundcoverBare = NULL
                                 ,groundcoverBare_dd= NULL
                                 ,groundcoverInundated = NULL
                                 ,groundcoverInundated_dd= NULL
                                 ,groundcoverNonwoody = NULL
                                 ,groundcoverNonwoody_dd= NULL
                                 ,groundcoverWoody = NULL
                                 ,groundcoverWoody_dd= NULL
                                 ,understoryNonwoody = NULL
                                 ,understoryNonwoody_dd= NULL
                                 ,understoryWoody = NULL
                                 ,understoryWoody_dd= NULL
                                 ,understoryType = NULL
                                 ,understoryType_dd= NULL
                                 ,drawdown = NULL
                                 ,horizontalDistance_dd = NULL
                                 ,createSyntheticCovers=TRUE
                                 ,fillinDrawdown=TRUE
                                 ) {

################################################################################
# Function: nlaRiparianVegetation
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
#    7/14/17 cws Created nlaRiparianVegetation from metsRiparianVegetationNLA.
#            Changed UID to SITE, RESULT to VALUE, PARAMETER to CLASS for input
#            and METRIC for output.
#    7/17/17 cws Updated to test with new calling interface.
#   
# Arguments:
#   df = a data frame containing riparian zone and vegetation data.  The data
#     frame must include columns that are named as follows:
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
#                   argument has the following values: CANOPY, C_BIGTREES, 
#                   C_SMALLTREES, UNDERSTORY, U_NONWOODY, U_WOODY, GC_BARE,
#                   GC_INUNDATED, GC_NONWOODY, GC_WOODY, HORIZ_DIST_DD,
#                   and DRAWDOWN.
#       VALUE - parameter values, which are the values used in calculations.
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
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    # Print initial messages
    intermediateMessage('NLA riparian zone and vegetation metrics', loc='start')

    addClass <- function(df, ...) {
        
        args <- list(...)
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(CLASS = args[[1]])
        return(rc)
    }
    bigTrees <- bigTrees %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'C_BIGTREES')
    bigTrees_dd  <- bigTrees_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'C_BIGTREES_DD')
    smallTrees <- smallTrees %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'C_SMALLTREES')
    smallTrees_dd <- smallTrees_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'C_SMALLTREES_DD')
    canopyType <- canopyType %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'CANOPY')
    canopyType_dd <- canopyType_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'CANOPY_DD')
    groundcoverBare <- groundcoverBare %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_BARE')
    groundcoverBare_dd <- groundcoverBare_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_BARE_DD')
    groundcoverInundated <- groundcoverInundated %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_INUNDATED')
    groundcoverInundated_dd <- groundcoverInundated_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_INUNDATED_DD')
    groundcoverNonwoody <- groundcoverNonwoody %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_NONWOODY')
    groundcoverNonwoody_dd <- groundcoverNonwoody_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_NONWOODY_DD')
    groundcoverWoody <- groundcoverWoody %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_WOODY')
    groundcoverWoody_dd <- groundcoverWoody_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'GC_WOODY_DD')
    understoryNonwoody <- understoryNonwoody %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'U_NONWOODY')
    understoryNonwoody_dd <- understoryNonwoody_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'U_NONWOODY_DD')
    understoryWoody <- understoryWoody %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'U_WOODY')
    understoryWoody_dd <- understoryWoody_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'U_WOODY_DD')
    understoryType <- understoryType %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'UNDERSTORY')
    understoryType_dd <- understoryType_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'UNDERSTORY_DD')
    drawdown <- drawdown %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'DRAWDOWN')
    horizontalDistance_dd <- horizontalDistance_dd %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'HORIZ_DIST_DD')
    
    df <- rbind(bigTrees,bigTrees_dd,smallTrees,smallTrees_dd
               ,canopyType,canopyType_dd,groundcoverBare,groundcoverBare_dd
               ,groundcoverInundated,groundcoverInundated_dd
               ,groundcoverNonwoody,groundcoverNonwoody_dd
               ,groundcoverWoody,groundcoverWoody_dd,understoryNonwoody,understoryNonwoody_dd
               ,understoryWoody,understoryWoody_dd,understoryType,understoryType_dd
               ,drawdown,horizontalDistance_dd
               )

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
		tt <- subset(df, CLASS %in% c(coverParams,'HORIZ_DIST_DD','DRAWDOWN'))
        intermediateMessage('.a')
		tt <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
        intermediateMessage('.b')
		dfStart <- rbind(tt, subset(df, CLASS %in% typeParams))
        intermediateMessage('.c')
	} else {
		dfStart <- df
	}
	
    intermediateMessage('.1')
    
  	# Create table for converting field values to calculation values
  	coverClassInfo<-data.frame(VALUE = c(NA,'0','1','2','3','4')
						  	  ,characteristicCover = c(NA,0,0.05,0.25,0.575,0.875)
							  ,presence = c(NA,0,1,1,1,1)
							  ,stringsAsFactors=FALSE
							  )
				
  	rvData <- subset(dfStart, CLASS %in% vegParams & !is.na(VALUE))

  	rvData <- merge(rvData, coverClassInfo, by='VALUE', all.x=TRUE)
    intermediateMessage('.2')


  	# Synthesize 2007esque cover values from riparian and drawdown cover values
  	# if requested.  Values of HORIZ_DIST_DD are used for these calculations
  	if(createSyntheticCovers) {
		intermediateMessage('.synth')
		
	  	horizDist <- within(subset(dfStart, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE))
			  			   ,{characteristicCover <- NA
						     presence <- NA
			   			    }
	                       )
		synValues <- calcSynCovers(rbind(rvData, horizDist), 15, assumptions=FALSE)

		# Make the resulting dataframe look like the data we're calculating metrics with.
		newValues <- within(synValues[c('SITE','STATION','CLASS','VALUE','characteristicCover')]
						   ,{presence <- ifelse(is.na(characteristicCover),	NA
										,ifelse(characteristicCover > 0, 	1, 	0
										 ))
							}
						   )

	  	rvData <- rbind(rvData, newValues)
  	}
	
	intermediateMessage('.calcs')
	
	# Classify CLASSs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later on
	# For 2007, all locations will be riparian
	splitParams <- nlaFishCover.splitParameterNames(rvData$CLASS)
	rvData <- within(rvData
					,{coverSuffix <- ifelse(splitParams$suffix=='', '_RIP', splitParams$suffix)
					  CLASS <- splitParams$base		#ifelse(splitParams$suffix == '', paste0(CLASS,'_RIP'), CLASS)
					 }
				  	)

	rvMets <- nlaRiparianVegetation.calculateMets(rvData)
	
  	# combine results into a dataframe.  When not calculating synthetic cover values,
  	# rename the _RIP metrics back to 2007 names (no suffix). 
	rvMets <- within(rvMets
			  		,{METRIC <- paste0(METRIC, coverSuffix)
				  	  coverSuffix <- NULL
		  			 }
  			 		)
  
  	if(!createSyntheticCovers) {
		rvMets <- within(rvMets
			  			,METRIC <- ifelse(grepl('_RIP$', METRIC), substr(METRIC, 1, nchar(METRIC)-4), METRIC)
	  					)
  	}
  
  	return(rvMets)
}


nlaRiparianVegetation.calculateMets <- function(rvData)
# Do all the calculationy work.  
#
{
  	intermediateMessage('.1')
	
	# Metrics about the canopy & understory types are currently undefined
	# for synthetic data, so these metrics make sense for drawdown and littoral 
	# data only.
	noSyn <- subset(rvData, coverSuffix %in% c('_DD','_RIP'))
  	canPresence <- nlaRiparianVegetation.canopyTypePresence(noSyn)
  	intermediateMessage('.2')

  	undPresence <- nlaRiparianVegetation.understoryTypePresence(noSyn)
  	intermediateMessage('.3')

	
	# Presences of individual layer components
  	componentPresence <- nlaRiparianVegetation.componentPresence(rvData)
  	intermediateMessage('.4')


  	# Convert classes to values to fractional values for calculations.  
  	# Normalize fractional cover within each layer and zone(drawdown, riparian, synthetic).
	# Ground cover must add to exactly 100 percent cover since one of the 'layers' 
	# is bare ground.
	# Note that this code would be simpler if normalizedCover were modified to accept
	# grouping variables as an argument.
	ripData <- subset(rvData, coverSuffix == '_RIP')
	if(nrow(ripData) > 0) {
      	intermediateMessage('.')
  		canFrac_RIP <- normalizedCover(subset(ripData, CLASS %in% c('C_BIGTREES','C_SMALLTREES'))
									  ,'characteristicCover', 'normCover'
    	                          	  ,allowTotalBelow100=TRUE
								  	  )
      	intermediateMessage('.')
		undFrac_RIP <- normalizedCover(subset(ripData, CLASS %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
								  	  ,allowTotalBelow100=TRUE
								  	  )
  	    intermediateMessage('.')
		gndFrac_RIP <- normalizedCover(subset(ripData, CLASS %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
								  	  ,'characteristicCover', 'normCover'
								  	  )
							  
      	intermediateMessage('.')
	} else {
		canFrac_RIP <- NULL
		undFrac_RIP <- NULL
		gndFrac_RIP <- NULL
	}

	ddData <- subset(rvData, coverSuffix == '_DD')
	if(nrow(ddData) > 0) {
		canFrac_DD <- normalizedCover(subset(ddData, CLASS %in% c('C_BIGTREES','C_SMALLTREES'))
									 ,'characteristicCover', 'normCover'
									 ,allowTotalBelow100=TRUE
									 )
		undFrac_DD <- normalizedCover(subset(ddData, CLASS %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		gndFrac_DD <- normalizedCover(subset(ddData, CLASS %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
									 ,'characteristicCover', 'normCover'
									 )
	} else {
		canFrac_DD <- NULL
		undFrac_DD <- NULL
		gndFrac_DD <- NULL
	}
	
	synData <- subset(rvData, coverSuffix == '_SYN')
	if(nrow(synData) > 0) {
		canFrac_SYN <- normalizedCover(subset(synData, CLASS %in% c('C_BIGTREES','C_SMALLTREES'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		undFrac_SYN <- normalizedCover(subset(synData, CLASS %in% c('U_NONWOODY','U_WOODY'))
									  ,'characteristicCover', 'normCover'
									  ,allowTotalBelow100=TRUE
									  )
		gndFrac_SYN <- normalizedCover(subset(synData, CLASS %in% c('GC_BARE','GC_INUNDATED','GC_NONWOODY','GC_WOODY'))
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
  
  	componentCovers <- nlaRiparianVegetation.componentCovers(allFractions)
  	intermediateMessage('.6')

	
	tt <- subset(allFractions, CLASS %in% c('C_BIGTREES', 'C_SMALLTREES'))
	iCan <- nlaRiparianVegetation.compositeIndex(tt, 'RVICANOPY')
	
	tt <- subset(allFractions, CLASS %in% c('U_NONWOODY', 'U_WOODY'))
	iUnd <- nlaRiparianVegetation.compositeIndex(tt, 'RVIUNDERSTORY')
	
	tt <- subset(allFractions, CLASS %in% c('GC_INUNDATED', 'GC_NONWOODY', 'GC_WOODY'))
	iGnd <- nlaRiparianVegetation.compositeIndex(tt, 'RVIGROUND')
	
	tt <- subset(allFractions, CLASS %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_WOODY'))
	iTallw <- nlaRiparianVegetation.compositeIndex(tt, 'RVITALLWOOD')
	
	tt <- subset(allFractions, CLASS %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_WOODY', 'GC_WOODY'))
	iWoody <- nlaRiparianVegetation.compositeIndex(tt, 'RVIWOODY')
	
	tt <- subset(allFractions, CLASS %in% c('U_NONWOODY', 'GC_NONWOODY'))
	iHerbs <- nlaRiparianVegetation.compositeIndex(tt, 'RVIHERBS')
	
	tt <- subset(allFractions, CLASS %in% c('C_BIGTREES', 'C_SMALLTREES', 'U_NONWOODY', 'U_WOODY'))
	iCanUnd <- nlaRiparianVegetation.compositeIndex(tt, 'RVICANUND')
	
	tt <- subset(allFractions
				,CLASS %in% c('C_BIGTREES', 'C_SMALLTREES'
								 ,'U_NONWOODY', 'U_WOODY'
								 ,'GC_NONWOODY', 'GC_WOODY'
								 )
				)
	itotVeg <- nlaRiparianVegetation.compositeIndex(tt, 'RVITOTALVEG')
	intermediateMessage('.7')
	

  	# Collect all metrics calculations into single dataframe, expand the results
  	# so sites without data for metrics have those metrics with NA results,
  	# and make NA counts zero.  The results of this expansion should not include 
	# synthetic cover types, which don't/shouldn't exist.
  	rv <- rbind(canPresence, undPresence, componentPresence, componentCovers
			   ,iCan, iUnd, iGnd, iTallw, iWoody, iHerbs, iCanUnd, itotVeg
			   )
  	rv <- within(expand.data.frame(rv, c('SITE','METRIC','coverSuffix'))
                ,VALUE <- ifelse(grepl('^RVN.+', METRIC) & is.na(VALUE), 0, VALUE)
	            )
	rv <- subset(rv
				,!(METRIC %in% c('RVNUNDERSTORY','RVNCANOPY'
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


nlaRiparianVegetation.canopyTypePresence <- function(rvData)
# Calculate mean presence (ranges 0-1) of each canopy type
{
	cantype <- subset(rvData, CLASS=='CANOPY')

	tt <- aggregate(list(VALUE = cantype$VALUE=='B')
				   ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canB <- within(tt, METRIC <- 'RVFPCANBROADLEAF')
	
	tt <- aggregate(list(VALUE = cantype$VALUE=='C')
			       ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canC <- within(tt, METRIC <- 'RVFPCANCONIFEROUS')
	
	tt <- aggregate(list(VALUE = cantype$VALUE=='D')
				   ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canD <- within(tt, METRIC <- 'RVFPCANDECIDUOUS')
	
	tt <- aggregate(list(VALUE = cantype$VALUE=='M')
				   ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canM <- within(tt, METRIC <- 'RVFPCANMIXED')
	
	tt <- aggregate(list(VALUE = cantype$VALUE=='N')
				   ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	canN <- within(tt, METRIC <- 'RVFPCANNONE')
	
	tt <- aggregate(list(VALUE = I(cantype$VALUE))
				   ,list('SITE'=cantype$SITE, coverSuffix = cantype$coverSuffix)
				   ,count
				   )
	canCount <- within(tt
					  ,{METRIC <- 'RVNCANOPY'
						VALUE <- ifelse(is.na(VALUE), 0L, VALUE)
					   }
			          )
	
	rc <- rbind(canB, canC, canD, canM, canN, canCount)
	return(rc)
}


nlaRiparianVegetation.understoryTypePresence <- function(rvData)
# Calculate mean presence (ranges 0-1) of each understory type
{
	undtype <- subset(rvData, CLASS=='UNDERSTORY')
	tt <- aggregate(list(VALUE = undtype$VALUE=='B')
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undB <- within(tt, METRIC <- 'RVFPUNDBROADLEAF')
	
	tt <- aggregate(list(VALUE = undtype$VALUE=='C')
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undC <- within(tt, METRIC <- 'RVFPUNDCONIFEROUS')
	
	tt <- aggregate(list(VALUE = undtype$VALUE=='D')
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	undD <- within(tt, METRIC <- 'RVFPUNDDECIDUOUS')
	
	tt <- aggregate(list(VALUE = undtype$VALUE=='M')
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
		   		   )
	undM <- within(tt, METRIC <- 'RVFPUNDMIXED')
	
	tt <- aggregate(list(VALUE = undtype$VALUE=='N')
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,protectedMean, na.rm=TRUE
		   		   )
	undN <- within(tt, METRIC <- 'RVFPUNDNONE')
	
	tt <- aggregate(list(VALUE = I(undtype$VALUE))
				   ,list('SITE'=undtype$SITE, coverSuffix = undtype$coverSuffix)
				   ,count
				   )
	undCount <- within(tt, {METRIC <- 'RVNUNDERSTORY'; VALUE <- ifelse(is.na(VALUE), 0L, VALUE)})
	
	rc <- rbind(undB, undC, undD, undM, undN, undCount)
	return(rc)
}


nlaRiparianVegetation.componentPresence <- function(rvData)
# Calculate mean presences for all layer components 
{
	paData <- subset(rvData, CLASS %in% c('C_BIGTREES', 'C_SMALLTREES'
											 ,'U_NONWOODY', 'U_WOODY'
											 ,'GC_BARE', 'GC_INUNDATED'
											 ,'GC_NONWOODY', 'GC_WOODY'
											 )
					)
	
	tt <- aggregate(list(VALUE = paData$presence)
				   ,list('SITE'=paData$SITE, 'CLASS'=paData$CLASS, coverSuffix = paData$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
				   
	paMets <- within(tt, METRIC <- ifelse(CLASS == 'C_BIGTREES',		'RVFPCANBIG'
									 ,ifelse(CLASS == 'C_SMALLTREES', 	'RVFPCANSMALL'
									 ,ifelse(CLASS == 'U_NONWOODY', 	'RVFPUNDNONW'
									 ,ifelse(CLASS == 'U_WOODY', 		'RVFPUNDWOODY'
									 ,ifelse(CLASS == 'GC_BARE', 		'RVFPGNDBARE'
									 ,ifelse(CLASS == 'GC_INUNDATED', 	'RVFPGNDINUNDATED'
									 ,ifelse(CLASS == 'GC_NONWOODY',  	'RVFPGNDNONW'
									 ,ifelse(CLASS == 'GC_WOODY', 		'RVFPGNDWOODY'
																		   ,'UnkRVPAClass'
									  ))))))))
	                ) %>%
	          select(SITE, METRIC, VALUE, coverSuffix)

	return(paMets)
}


nlaRiparianVegetation.componentCovers <- function(allFractions)
# Calculate cover mean, stdev and count for individual components
{
	# Means
	tt <- aggregate(list(VALUE = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("SITE" = allFractions$SITE
				   		,"CLASS" = allFractions$CLASS
						,coverSuffix = allFractions$coverSuffix
				   		)
			  	   ,protectedMean, na.rm=TRUE
				   )
	meanCover <- within(tt, METRIC <- ifelse(CLASS == 'C_BIGTREES',	'RVFCCANBIG'
										,ifelse(CLASS == 'C_SMALLTREES','RVFCCANSMALL'
										,ifelse(CLASS == 'U_NONWOODY', 	'RVFCUNDNONW'
										,ifelse(CLASS == 'U_WOODY', 	'RVFCUNDWOODY'
										,ifelse(CLASS == 'GC_BARE', 	'RVFCGNDBARE'
										,ifelse(CLASS == 'GC_INUNDATED','RVFCGNDINUNDATED'
										,ifelse(CLASS == 'GC_NONWOODY', 'RVFCGNDNONW'
										,ifelse(CLASS == 'GC_WOODY', 	'RVFCGNDWOODY'
																			,'UnkRVCoverClass'
										 ))))))))
	                    ) %>%
	             select(SITE, METRIC, VALUE, coverSuffix)
	
	
	# Stdev
	tt <- aggregate(list(VALUE = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("SITE"=allFractions$SITE
				   		,"CLASS"=allFractions$CLASS
						,coverSuffix = allFractions$coverSuffix
						)
				   ,sd, na.rm=TRUE
				   )
	sdCover <- within(tt, METRIC <- ifelse(CLASS == 'C_BIGTREES',	'RVVCANBIG'
									  ,ifelse(CLASS == 'C_SMALLTREES',	'RVVCANSMALL'
									  ,ifelse(CLASS == 'U_NONWOODY', 	'RVVUNDNONW'
									  ,ifelse(CLASS == 'U_WOODY', 		'RVVUNDWOODY'
									  ,ifelse(CLASS == 'GC_BARE', 		'RVVGNDBARE'
									  ,ifelse(CLASS == 'GC_INUNDATED',	'RVVGNDINUNDATED'
									  ,ifelse(CLASS == 'GC_NONWOODY', 	'RVVGNDNONW'
									  ,ifelse(CLASS == 'GC_WOODY', 		'RVVGNDWOODY'
																		   ,'UnkRVSDClass'
									   ))))))))
                      ) %>%
               select(SITE, METRIC, VALUE, coverSuffix)

	
	# Counts. 
	# Expand data so that all parameters occur in all SITE, and thus counts (of 0) 
	# will exist for all SITE.  This could be done above when creating allFractions 
	# without introduction of error, but it is done here to localize the change. 
	tt <- aggregate(list(VALUE = allFractions$normCover)		########################## CHANGE THESE BACK!!!!!!!!!!!!!!!!! #############
				   ,list("SITE"=allFractions$SITE
						,"CLASS"=allFractions$CLASS
						,coverSuffix = allFractions$coverSuffix
						)
				   ,count
				   )
	countCover <- within(tt
						,{METRIC <- ifelse(CLASS == 'C_BIGTREES',	'RVNCANBIG'
									  ,ifelse(CLASS == 'C_SMALLTREES',	'RVNCANSMALL'
									  ,ifelse(CLASS == 'U_NONWOODY', 	'RVNUNDNONW'
									  ,ifelse(CLASS == 'U_WOODY', 		'RVNUNDWOODY'
									  ,ifelse(CLASS == 'GC_BARE', 		'RVNGNDBARE'
									  ,ifelse(CLASS == 'GC_INUNDATED',	'RVNGNDINUNDATED'
									  ,ifelse(CLASS == 'GC_NONWOODY', 	'RVNGNDNONW'
									  ,ifelse(CLASS == 'GC_WOODY', 		'RVNGNDWOODY'
																		   ,'UnkRVNClass'
									   ))))))))
						  VALUE <- ifelse(is.na(VALUE), 0L, VALUE)
						 }
                        ) %>%
                  select(SITE, METRIC, VALUE, coverSuffix)

	rc <- rbind(meanCover, sdCover, countCover)
	return(rc)
}


nlaRiparianVegetation.compositeIndex <- function(groupData, indexName)
# Composite vegetation indicex.  These are based on the sum of normalized
# cover values for selected cover types at a station/subid, and the mean
# for the entire site as the resulting metric. 
{
	tt <- aggregate(groupData$characteristicCover
				   ,list('SITE'=groupData$SITE
		                ,'STATION'=groupData$STATION
						,coverSuffix = groupData$coverSuffix
						)
				   ,protectedSum, na.rm=TRUE
				   )

	qq <- aggregate(list(VALUE = tt$x)
				   ,list('SITE'=tt$SITE, coverSuffix = tt$coverSuffix)
				   ,protectedMean, na.rm=TRUE
				   )
	rc <- within(qq, METRIC <- indexName)

	return(rc)
}



# end of file