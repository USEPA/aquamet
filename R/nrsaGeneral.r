nrsaGeneral <- function(sampledTransects = NULL, sideChannels = NULL, transectSpacing = NULL
                       ,sideChannelTransects = c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')
                       ) {
  
################################################################################
# Function: metsGeneral
# Title: Calculate NRSA General Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Tom Kincaid
# Date: January 4, 2010
# Description:
#   This function calculates the general portion of the physical habitat metrics
#   for National Rivers and Streams Assessment (NRSA) data.  The function
#   requires data frames containing the thalweg and channel geometry data files.
# Function Revisions:
#   01/04/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r,NA_filler.r and
#            summaryby.r
#   03/22/10 cws: Added all=TRUE argument to merge() of expected and actual
#            values in unit test.
#   04/02/10 cws: Modified unit test and metrics code to handle data with just
#            one protocol.  Added comment about change in sidecnt value in unit
#            test.
#   06/03/10 cws: Modified reachlen calculation to work with single increment
#            value at A 0 instead of one for each transect.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   02/17/11 cws: Removed calculation of SAMPLED metric, as it was an
#            unneccessary holdover from WEMAP. Changes made to both metrics code
#            and unit test.  This is made somewhat easier by the fact that this
#            metric escaped inclusion in the unit test.
#   04/08/11 cws: Using GPS based DISTANCE parameter (calculated with GIS
#            currently) to base REACHLEN on when ACTRANSP is not available.
#            Unit test updated accordingly -- many changes to function creating
#            test data, and no longer writing REACHLEN if ACTRANSP or DISTANCE
#            are NA.
#   03/08/12 cws: Changed name of output csv from metsGeneralUsingDistance.csv 
#            to metsGeneral.csv.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify the names of the data files.  Added argument NRSAdir to
#            the function to identify the directory from which the data file is
#            read and to which the output metrics file is written.
#   12/21/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   12/29/15 cws Modified calling interface for generalized use.
#    1/19/16 cws Pulled list of side channel transect values out as argument
#            'sideChannelTransects'. No change to unit test at this time.
#    1/20/16 cws No longer requiring TRANSECT column in sideChannels argument.
#
# Arguments:
#   thalweg = a data frame containing the thalweg data file.  The data frame
#     must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       STATION - station number along thalweg between transects
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
#   channelgeometry = a data frame containing the channel geometry data file.
#     The data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       TRANLINE - location (mid-channel or bank)along transect
#       BANK - bank (left or right) along transect
#       LINE - way point (1,2, etc.) between transects
#       METHOD - method used to measure slope
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
#   Note that possible values for variables in the input data frames are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     UID - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
#   metsGeneral.1 - calculate metrics
################################################################################
# 
# 
# # Print an initial message
#   cat('General calculations:\n')
# 
# # Convert factors to character variables in the input data frames
#   intermediateMessage('.1 Convert factors to character variables.', loc='end')
#   thalweg <- convert_to_char(thalweg)
#   channelgeometry <- convert_to_char(channelgeometry)
# 
# # Combine the data frames
#   intermediateMessage('.2 Combine the data frames.', loc='end')
#   thal <- thalweg[c('SITE', 'TRANSECT', 'STATION', 'SAMPLE_TYPE', 'PARAMETER',
#     'VALUE')]
#   channelgeometry$STATION <- 0
#   actransp <- channelgeometry[c('SITE', 'TRANSECT', 'STATION',
#     'SAMPLE_TYPE', 'PARAMETER', 'VALUE')]
#   actransp <- subset(actransp, PARAMETER %in% c('ACTRANSP', 'DISTANCE'))
#   rawdat<- rbind(thal, actransp)
# 
# # Calculate the metrics
#   intermediateMessage('.3 Call function metsGeneral.1.', loc='end')
#   mets <- metsGeneral.1(rawdat)
#   row.names(mets) <- 1:nrow(mets)
# 
# # Print an exit message
#   intermediateMessage('Done.', loc='end')
# 
# # Return results
#   return(mets)
# }
# 
# 
# 
# metsGeneral.1 <- function(indat) {
# # Does all the real work for metsGeneral.
# # Returns a dataframe of calculations if successful
# # or a character string describing the problem if
# # one was encountered.
# #
# # ARGUMENTS:
# # indat		dataframe of channel data.
# # protocols	dataframe relating UID to the
# #			  sampling protocol used at the site.
# #

  intermediateMessage('General mets ', loc='start')
#   thal <- thalweg[c('SITE', 'TRANSECT', 'STATION', 'SAMPLE_TYPE', 'PARAMETER', 'VALUE')]
#   if(nrow(channelgeometry)==0) {
#       actransp <- NULL
#   } else {
#       channelgeometry$STATION <- 0
#       actransp <- channelgeometry[c('SITE', 'TRANSECT', 'STATION', 'SAMPLE_TYPE', 'PARAMETER', 'VALUE')]
#       actransp <- subset(actransp, PARAMETER %in% c('ACTRANSP', 'DISTANCE'))
#   }
#   indat<- rbind(thal, actransp)

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdfTransects <- function(df, ...) {
        rc <- df %>% 
              select(SITE, TRANSECT) %>% 
              unique()
        return(rc)
    }
    ifdfValues <- function(df, ...) {
        rc <- df %>% 
              select(SITE, TRANSECT, VALUE) 
        return(rc)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% 
              select(SITE, VALUE) 
        return(rc)
    }
    
    sampledTransects <- absentAsNULL(sampledTransects, ifdfTransects)
    sideChannels <- absentAsNULL(sideChannels, ifdf)
    transectSpacing <- absentAsNULL(transectSpacing, ifdfValues)
   
#    sideChannelTransects <- c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')


    # Calculate count of side channels SIDECNT (only meaningful for wadeables)
    if(is.null(sampledTransects)) {
        sidecnt <- NULL
    } else {
        sidecnt <- sampledTransects %>%
                   ddply('SITE', summarise
                        ,VALUE=protectedSum(TRANSECT %in% sideChannelTransects
                                            ,na.rm=TRUE
                                            )
                        ) %>%
                   mutate(METRIC = 'sidecnt')
    }

    intermediateMessage('.1')

    
    # Calculate percent of reach with side channels PCT_SIDE
    if(is.null(sideChannels)) {
        pct_side <- NULL
    } else {
        pct_side <- sideChannels %>%
                    subset(VALUE %in% c('Y','N',NA)) %>%
                    mutate(standardizedPresent = VALUE=='Y') %>%
                    ddply('SITE', summarise
                         ,VALUE = 100 * protectedMean(standardizedPresent, na.rm=TRUE)
                         ) %>%
                    mutate(METRIC = 'pct_side')
    }

    intermediateMessage('.2')


    # Calculate reach length REACHLEN
    if(is.null(transectSpacing)) {
        reachlen <- NULL
    } else {
        reachlen <- transectSpacing %>%
                    ddply('SITE', summarise
                         ,VALUE = protectedSum(as.numeric(VALUE), na.rm=TRUE)
                         ) %>%
                    mutate(METRIC = 'reachlen')
    }

    intermediateMessage('.3')


    rc <- rbind(sidecnt, pct_side, reachlen)
    intermediateMessage('.Done', loc='end')

    return(rc)
}


foo <- function() { # old code
    cdData <- subset(indat
                  ,PARAMETER %in% c('ACTRANSP','DISTANCE','INCREMNT','SIDCHN'
                                   ,'OFF_CHAN','REACHLENGTH'
                                   )
                  )

  # calculate PCT_SIDE (side channels) for wadeable(SIDCHN) and boatable (OFF_CHAN)
  sidec<-subset(cdData
               ,PARAMETER %in% c('SIDCHN','OFF_CHAN') &
                VALUE %in% c('Y','N',NA) &
                !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                 ,'XG','XH','XI','XJ','XK'
                                 )
                 )
               )
 
  intermediateMessage('.1')

  ps <- summaryby(sidec,'count',"pct_side")
  tyout<-aggregate( list(typesum=sidec$VALUE),list(SITE=sidec$SITE),function(x){sum(x=='Y',na.rm=TRUE)})
  ps<-merge(ps,tyout,by='SITE',all.x=TRUE)
  ps$VALUE <- (ps$typesum/ps$VALUE)*100
  ps<-ps[c('SITE','METRIC','VALUE')]

  # subset data for wadeable only and count side channel transect types (SIDECNT)
  wdata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THALW' & PARAMETER == 'INCREMNT')
  sc <- NULL
  if(nrow(wdata)>0) {
      wdata  <-unique(wdata[c('SITE','TRANSECT')])
      wdata$VALUE<-ifelse(wdata$TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                                ,'XG','XH','XI','XJ','XK'
                                                )
                          ,1
                          ,0
                          )
      sc<-summaryby(wdata,'sum','sidecnt')
   }

  #calculate REACHLEN using parameter values REACHLENGTH and ACTRANSP
  incr  <- subset(cdData
                 ,SAMPLE_TYPE == 'PHAB_THALW' &
                  PARAMETER == 'INCREMNT' &
                  TRANSECT == 'A' & STATION == 0
                 ,select=c(SITE,VALUE)
                 )
  rlw <- NULL
  if(nrow(incr)>0) {
      w2<- nWadeableStationsPerTransect(cdData)
      transpc <- merge(incr,w2,by=c('SITE'),all.x=TRUE)
      transpc$transpc<-(as.numeric(transpc$VALUE) * transpc$nSta)
      transpc$VALUE<- ifelse(transpc$transpc<=0,NA,transpc$transpc)
      rlw <- summaryby(transpc,'sum',"reachlen")

      # need to subtract one increment from each SITE, since the last transect has 1 less increment than the others.
      rlw<-merge(rlw,incr,by='SITE',all.x=TRUE, suffix=c('.transpcTot','.incremnt'))
      rlw$VALUE<-rlw$VALUE.transpcTot - as.numeric(rlw$VALUE.incremnt)
      rlw<-rlw[c('SITE','METRIC','VALUE')]
  }

  bdata  <- subset(cdData,PARAMETER %in% c('ACTRANSP','DISTANCE'))
  rl <- NULL
  if(nrow(bdata)>0) {
      # Condense ACTRANSP (field value) and DISTANCE (GIS calculated value)
      # into a single value for each transect at a reach.  Use ACTRANSP
      # preferably, supplementing it with DISTANCE when necessary.
      actransp <- subset(bdata, PARAMETER %in% c('ACTRANSP') & !is.na(VALUE))
      distance <- subset(bdata, PARAMETER %in% c('DISTANCE') & !is.na(VALUE) &
                                paste(SITE,TRANSECT) %nin% paste(actransp$SITE, actransp$TRANSECT)
                        )
      bdata <-rbind(actransp,distance)
      bdata$VALUE<-as.numeric(bdata$VALUE)
      bdata$nSta<-1
      bdata$VALUE<- ifelse(bdata$VALUE<=0, NA, bdata$VALUE)
      rl <- summaryby(bdata,'sum',"reachlen")
  }

  intermediateMessage('.2')

  mets <- rbind(ps,sc,rlw,rl)
  mets$VALUE<-ifelse(mets$VALUE=='NaN',NA,mets$VALUE)

  intermediateMessage('.Done', loc='end')
   
  return(mets)
}



# end of file
