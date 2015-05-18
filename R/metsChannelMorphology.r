metsChannelMorphology <- function(bankgeometry, thalweg, visits) {

################################################################################
# Function: metsChannelMorphology
# Title: Calculate NRSA Channel Morphology Metrics
# Programmers: Marlys Cappert
#              Curt Seeliger
#              Tom Kincaid
# Date: January 28, 2010
# Description:
#   This function calculates the channel morphology portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the bank geometry, thalweg, and
#   stream verification form data files.
# Stream Metrics:
#   bfwd_rat, n_bfrat, n_bh, n_bw, n_d, n_incis, n_w, n_wd,n_wdr, sdbkf_h,
#   sdbkf_w, sddepth, sdinc_h, sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth,
#   xinc_h, xwd_rat, xwidth, xwxd
# River Metrics:
#   bfwd_rat, n_bfrat, n_bh, n_bw, n_d, n_incis,  n_wd,n_wdr,sdbkf_h, sdbkf_w,
#   sddepth, sdinc_h, sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth, xinc_h,
#   xwd_rat, xwidth, xwxd
# Function Revisions:
#   01/28/10 mrc: Started.
#   03/17/10 mrc: Recalculate mean and sd mets with appropriate use of side.
#            channels. Replace upData with rename.
#   03/26/10 cws Moved creation of test dataframes to separate functions.
#            Modified test data and metrics code to reflect thalweg
#            station numbering starting at 0 instead of at 1 as in EMAP.
#            Modifying calculation and test code so it works with real data.
#   03/30/10 cws: Cleaned up on.exit() call of metsChannelMorphology.cleanup().
#   04/01/10 cws: Modified unit test and metrics calculations to handle data
#            with only one protocol.
#   04/13/10 cws: Converting xdepth and sddepth calculations from m to cm as
#            requested. Modified unit test accordingly.
#   09/16/10 cws: Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   01/11/11 cws: Changed units in metsChannelMorphology.createBankGeometryData
#            from NONE to M.  Changed thalweg PARAMETER WETWID to WETWIDTH to
#            match data in unit test.
#   10/21/11 cws: Modified unit test to use correct parameters DEP_SONR and 
#            DEP_POLE instead of DEPTH, and have data in FT for for DEP_POLE.  
#            Previously, this resulted in not detecting a bug in the 
#            calculations that resulted in river depths in feet not being 
#            converted correctly to meters.  Values and units are changed in 
#            code rather than while creating the dataframes, just to get this 
#            done, but explicitely changed bfwd_rat at UID 8 to 39.01601245
#            and at UID 9 from 26.5309389 to 35.63173461.
#   10/25/11 cws: Ammended above modification as follows: test thalweg data
#            using SAMPLE_TYPE = PHAB_THAL,PHAB_THALW instead of PHAB_CHANW,
#            PHAB_CHANBFRONT; PARAMETER is DEPTH instead of DEP_SONR for
#            UIDs 1:6.  Ran test thalweg data through valStructThalweg()
#            which shows boatable reaches have unexpected stations 15:19,
#            which is not a problem.  Calculations accidentally modified 
#            earlier to use DEP_SONR instead of DEPTH throughout code;
#            this was also changed.
#   08/03/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written.
#   12/14/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   06/03/14 cws: Handling NA UNITS values gracefully by translating NA to ""
#            so that the units conversion of cm and ft to m doesn't set RESULT 
#            to NA.
# Arguments:
#   bankgeometry = a data frame containing the bank geometry data file.  The
#     data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       TRANSDIR - transverse location along transect
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
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
#   visits = a data frame containing the stream verification form data file.
#     The data frame contains a protocol value for each UID value.  It also
#     should include columns that relate UID to values meaningfull to the user,
#     e.g., site ID, date collected, and visit number.  The data frame must
#     include columns that are named as follows:
#       UID - universal ID value
#       VALXSITE - protocol used during a site visit (BOATABLE, PARBYBOAT,
#         ALTERED, INTWADE, PARBYWADE, WADEABLE)
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
#   siteProtocol determine sampling protocol values
#   metsChannelMorphology.1 - calculate metrics
################################################################################

# Print an initial message
  cat('Channel Morphology calculations:\n')

# Convert factors to character variables in the input data frames
  intermediateMessage('.1 Convert factors to character variables.', loc='end')
  bankgeometry <- convert_to_char(bankgeometry)
  thalweg <- convert_to_char(thalweg)
  visits <- convert_to_char(visits)

# Determine protocol used for each site
  intermediateMessage('.2 Set protocols.', loc='end')
  protocols <- siteProtocol(unique(bankgeometry$UID), visits)

# Calculate the metrics
  intermediateMessage('.3 Call function metsChannelMorphology.1.', loc='end')
  mets <- metsChannelMorphology.1(bankgeometry, thalweg, protocols)
  row.names(mets) <- 1:nrow(mets)

# Print an exit message
  intermediateMessage('Done.', loc='end')

# Return results
  return(mets)
}



metsChannelMorphology.1 <- function(df1, df2, protocols) {
# Returns a dataframe of calculations if successful or a character string
# describing the problem if one was encountered.
#
# ARGUMENTS:
# df1   dataframe of the bank geometry data.
# df2   dataframe of the thalweg data.
# protocols   dataframe relating UID to the sampling protocol used at that site

  intermediateMessage ('metsChannelMorphology started', loc='start')

  # Narrow down data to the neccessities.
  df1 <- subset(df1
               ,PARAMETER %in% c('BANKHGT', 'INCISHGT', 'BANKHT'
                                ,'INCISED', 'BANKWID'
                                )
                |
                (PARAMETER == 'WETWID' &
                 UID %in% subset(protocols, PROTOCOL == 'BOATABLE')$UID
                )
               )
  df2 <- subset (df2
                ,PARAMETER %in% c('DEPTH', 'WETWIDTH', 'DEP_SONR', 'DEP_POLE')
                )

  # Make dang sure UNITS is never NA
  df1 <- within(df1, UNITS <- ifelse(is.na(UNITS), '', UNITS))
  df2 <- within(df2, UNITS <- ifelse(is.na(UNITS), '', UNITS))

  #do the calculations
 
  # many counts required, here we don't care about TRANSDIR, and for these, only keep
  #the thalweg depths at STATION =0
  #these summaries EXCLUDE sidechannels
  #get all the units right...... meters for all!
 
  #right from the get go figure out the stream/river and units.
 
  #for streams need the bankhgt, bankwid, incishgt from bankgeo, need the wetwid, depth from thal
 
  df1$STATION <- 'NONE'
  df3 <- rbind (subset(df1, select=-c(SAMPLE_TYPE,FLAG,TRANSDIR))
               ,subset(df2, select=-c(SAMPLE_TYPE,FLAG))
               )
  df4 <- merge(df3, protocols, by='UID', all.x=TRUE, all.y=FALSE)
  df4$RESULT <- as.numeric(df4$RESULT)

  #change all the units to m
#  mm2 <- subset (df4, UNITS %in% c('NONE', 'M'))
#  testo <- subset (df4, UNITS=='CM')
#  if(nrow(testo) == 0) {
#      testo <- NULL
#  } else {
#      testo$RESULT <- testo$RESULT/100
#      testo$UNITS <- 'M'
#  }
#  
#  testo1 <- subset (df4, df4$UNITS=='FT')
#  if (nrow (testo1) == 0 ) {
#      testo1 <- NULL
#  } else {
#      testo1$RESULT <- ifelse(testo1$PROTOCOL=='BOATABLE' &
#                            testo1$PARAMETER=='DEP_SONR' &
#                            testo1$UNITS=='FT'
#                           ,testo1$RESULT*0.3048
#                           ,testo1$RESULT
#                           )
#      testo1$UNITS <- 'M'
#  }
# 
#  df5 <- rbind (mm2, testo, testo1)
  df5 <- within(df4, RESULT <- ifelse(UNITS=='CM', RESULT/100
                              ,ifelse(UNITS=='FT', RESULT*0.3048, RESULT 
                               ))
               )
 
  # Standardize column and parameter names
  df5 <- rename (df5, 'protocol', 'PROTOCOL')
  df5$PARAMETER <- ifelse (df5$PARAMETER =='BANKHT', 'BANKHGT', df5$PARAMETER)
  df5$PARAMETER <- ifelse (df5$PARAMETER =='INCISED', 'INCISHGT', df5$PARAMETER)
  df5$PARAMETER <- ifelse (df5$PARAMETER =='WETWIDTH', 'WETWID', df5$PARAMETER)
  df5$PARAMETER <- ifelse (df5$PARAMETER =='DEP_POLE', 'DEPTH', df5$PARAMETER)
  df5$PARAMETER <- ifelse (df5$PARAMETER =='DEP_SONR', 'DEPTH', df5$PARAMETER)

  intermediateMessage ( '.1. Get the input data in.', loc='end')

  #spilt up by protocol
  wade <- subset (df5, df5$PROTOCOL=='WADEABLE')
  wade <- subset (wade, !(wade$PARAMETER=='WETWID' & wade$UNITS=='NONE'))#, select=-c(FLAG,SAMPLE_TYPE))
  boat <- subset (df5, df5$PROTOCOL=='BOATABLE')#, select=-c(FLAG,SAMPLE_TYPE))
 
  # these summaries do use the sidechannels, but is odd an different ways.
  # for all of these, keep the side channels and then either pick the mean, max, or sum
  # of the transect pair, depending on the metric
  if(nrow(wade)>0) {
      # this data (wwidnox1 is used to calculate: xwidth, sdwidth, n_w (max of
      # the sidechannel pair)
      wadenox <- within(wade, TRANSECT<-gsub('X' ,'',TRANSECT))

      wwidnox <- subset (wadenox, wadenox$PARAMETER=='WETWID')
      wwidnox1 <- aggregate(wwidnox$RESULT
                       ,list('UID'=wwidnox$UID, 'TRANSECT'=wwidnox$TRANSECT, 'STATION'=wwidnox$STATION
                       )
                       ,max
                       )
      wwidnox1 <- rename(wwidnox1, 'x', 'RESULT')
      #used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #          (sum of the sidechannel pair)
      wwidnox <- subset (wadenox, wadenox$PARAMETER=='WETWID')
      wwidnox1s <- aggregate(wwidnox$RESULT
                       ,list('UID'=wwidnox$UID
                            ,'TRANSECT'=wwidnox$TRANSECT
                            ,'STATION'=wwidnox$STATION
                            )
                       ,sum, na.rm=TRUE
                       )
      wwidnox1s <- rename(wwidnox1s, 'x', 'RESULT')

      #this data (bwidnox1 is used to calculate:xbkf_w, sdbkf_w, n_bw (sum of the sidechannel pair))
      bwidnox <- subset (wadenox, wadenox$PARAMETER=='BANKWID')
      bwidnox1 <- aggregate(bwidnox$RESULT
                           ,list('UID'=bwidnox$UID, 'TRANSECT'=bwidnox$TRANSECT)
                           ,sum , na.rm=TRUE
                           )
      bwidnox1 <- rename(bwidnox1, 'x', 'RESULT')
 
      #this data (incnox1 is used to calculate: xinc_h, sdinc_h, n_inc (max of
      # the sidechannel pair))
      inchnox <- subset (wadenox, wadenox$PARAMETER=='INCISHGT')
      inchnox1 <- aggregate(inchnox$RESULT
                           ,list('UID'=inchnox$UID, 'TRANSECT'=inchnox$TRANSECT)
                           ,max
                           )
      inchnox1 <- rename(inchnox1, 'x', 'RESULT')

      # this data (bhgtnox1 is used to calculate: xbkf_h, sdbkf_h (max of the
      # sidechannel pair))
      bhgtnox <- subset (wadenox, wadenox$PARAMETER=='BANKHGT')
      bhgtnox1 <- aggregate(bhgtnox$RESULT
                       ,list('UID'=bhgtnox$UID, 'TRANSECT'=bhgtnox$TRANSECT 
                       )
                       ,max 
                       )
      bhgtnox1 <- rename(bhgtnox1, 'x', 'RESULT')
     
      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                      (mean of the sidechannel pair)
      bhgtnox1x <- aggregate(bhgtnox$RESULT
                            ,list('UID'=bhgtnox$UID, 'TRANSECT'=bhgtnox$TRANSECT)
                            ,mean, na.rm=TRUE
                            )
      bhgtnox1x <- rename(bhgtnox1x, 'x', 'RESULT')
     
  }

  if(nrow(boat)>0) {
      # subset the boat data
      boatnox <- within(boat, TRANSECT<-gsub('X' ,'',TRANSECT))

      # this data (wwidnoxb1 is used to calculate: xwidth, sdwidth, n_w (max of
      # the sidechannel pair))
      wwidnoxb <- subset (boatnox, boatnox$PARAMETER=='WETWID')
      wwidnoxb1 <- aggregate(wwidnoxb$RESULT
                            ,list('UID'=wwidnoxb$UID
                                 ,'TRANSECT'=wwidnoxb$TRANSECT
                                 )
                            ,max
                            )
      wwidnoxb1 <- rename(wwidnoxb1, 'x', 'RESULT')
     
      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                         (sum of the sidechannel pair)
      wwidnoxb1s <- aggregate(wwidnoxb$RESULT
                             ,list('UID'=wwidnoxb$UID
                                  ,'TRANSECT'=wwidnoxb$TRANSECT
                                  )
                             ,sum, na.rm=TRUE
                             )
      wwidnoxb1s <- rename(wwidnoxb1s, 'x', 'RESULT')

      # this data (bwidnoxb1 is used to calculate: bkf_w, sdbkf_w, n_bw (sum of
      # the sidechannel pair))
      bwidnoxb <- subset (boatnox, boatnox$PARAMETER=='BANKWID')
      bwidnoxb1 <- aggregate(bwidnoxb$RESULT
                            ,list('UID'=bwidnoxb$UID
                                 ,'TRANSECT'=bwidnoxb$TRANSECT
                                 )
                            ,sum , na.rm=TRUE
                            )
      bwidnoxb1 <- rename(bwidnoxb1, 'x', 'RESULT')
    
      # this data (incnoxb1 is used to calculate: xinc_h, sdinc_h, n_inc (max of the sidechannel pair)
 
      inchnoxb <- subset (boatnox, boatnox$PARAMETER=='INCISHGT')
      inchnoxb1 <- aggregate(inchnoxb$RESULT
                            ,list('UID'=inchnoxb$UID
                                 ,'TRANSECT'=inchnoxb$TRANSECT
                                 )
                            ,max
                            )
      inchnoxb1 <- rename(inchnoxb1, 'x', 'RESULT')
      # this data (bhgtnoxb1 is used to calculate:xbkf_h, sdbkf_h, (max of the
      # sidechannel pair))
      bhgtnoxb <- subset (boatnox, boatnox$PARAMETER=='BANKHGT')
      bhgtnoxb1 <- aggregate(bhgtnoxb$RESULT
                            ,list('UID'=bhgtnoxb$UID
                                 ,'TRANSECT'=bhgtnoxb$TRANSECT
                                 )
                            ,max
                            )
      bhgtnoxb1 <- rename(bhgtnoxb1, 'x', 'RESULT')
     
      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                 (mean of the sidechannel pair)
      bhgtnoxb1x <- aggregate(bhgtnoxb$RESULT
                             ,list('UID'=bhgtnoxb$UID
                                  ,'TRANSECT'=bhgtnoxb$TRANSECT
                                  )
                             ,mean, na.rm=TRUE
                             )
      bhgtnoxb1x <- rename(bhgtnoxb1x, 'x', 'RESULT')
  }

  # special case to fix an error in the field manual.  If the INCISED
  # height is 0 or NA, the INCISED height is = BANKHGT, assuming BANKHGT > 0)
  if(nrow(wade)>0) {
      #wade
      incbank <- merge(inchnox1, bhgtnox1
                      ,by=c('UID','TRANSECT'), all.x=TRUE, all.y=FALSE
                      )

      incbank$RESULT.x <- ifelse (is.na(incbank$RESULT.x), 0, incbank$RESULT.x)
      incbank$RESULT.x <- ifelse (incbank$RESULT.x<=0
                                 ,incbank$RESULT.y
                                 ,incbank$RESULT.x
                                 )
     
      inchnox1 <- incbank
 
      inchnox1$RESULT.y <- NULL

      inchnox1 <- rename (inchnox1, 'RESULT.x', 'RESULT')
  }

  if(nrow(boat)>0) {
       #boat
       incbankb <- merge(inchnoxb1, bhgtnoxb1
                        ,by=c('UID','TRANSECT'), all.x=TRUE, all.y=FALSE
                        )

       incbankb$RESULT.x <- ifelse (is.na(incbankb$RESULT.x)
                                   ,0
                                   ,incbankb$RESULT.x
                                   )
       incbankb$RESULT.x <- ifelse (incbankb$RESULT.x<=0
                                   ,incbankb$RESULT.y
                                   ,incbankb$RESULT.x)

       ihgtb <- incbankb
 
       ihgtb$RESULT.y <- NULL
 
       ihgtb <- rename (ihgtb, 'RESULT.x', 'RESULT')
  }

  intermediateMessage ('.2') 

#sent these to summaryby, summaries by the transect
  intest <- NULL
  if(nrow(wade)>0) {
      #wade
      wwidx <- summaryby(wwidnox1,'mean',"xwidth")
      wwidsd <- summaryby(wwidnox1,'sd',"sdwidth")
      wwidc <- summaryby(wwidnox1,'count',"n_w")
     
      bwidx <- summaryby(bwidnox1,'mean',"xbkf_w")
      bwidsd <- summaryby(bwidnox1,'sd',"sdbkf_w")
      bwidc <- summaryby(bwidnox1,'count',"n_bw")
           
      bhidx <- summaryby(bhgtnox1,'mean',"xbkf_h")
      bhidsd <- summaryby(bhgtnox1,'sd',"sdbkf_h")
      bhidc <- summaryby(bhgtnox1,'count',"n_bh")
            
      ihidx <- summaryby(inchnox1,'mean',"xinc_h")
      ihidsd <- summaryby(inchnox1,'sd',"sdinc_h")
      ihidc <- summaryby(inchnox1,'count',"n_incis")

      intest <- rbind (wwidx,wwidsd, wwidc
                      ,bwidx, bwidsd, bwidc
                      ,bhidx, bhidsd, bhidc
                      ,ihidx, ihidsd, ihidc
                      )
  }
  if(nrow(boat)) {
      #boat
      wwidxb <- summaryby(wwidnoxb1,'mean',"xwidth")
      wwidsdb <- summaryby(wwidnoxb1,'sd',"sdwidth")
      wwidcb <- summaryby(wwidnoxb1,'count',"n_w")

      bwidxb <- summaryby(bwidnoxb1,'mean',"xbkf_w")
      bwidsdb <- summaryby(bwidnoxb1,'sd',"sdbkf_w")
      bwidcb <- summaryby(bwidnoxb1,'count',"n_bw")

      bhidxb <- summaryby(bhgtnoxb1,'mean',"xbkf_h")
      bhidsdb <- summaryby(bhgtnoxb1,'sd',"sdbkf_h")
      bhidcb <- summaryby(bhgtnoxb1,'count',"n_bh")

      ihidxb <- summaryby(inchnoxb1,'mean',"xinc_h")
      ihidsdb <- summaryby(inchnoxb1,'sd',"sdinc_h")
      ihidcb <- summaryby(inchnoxb1,'count',"n_incis")

      intest <- rbind (intest,wwidxb, wwidsdb, wwidcb
                      ,bwidxb, bwidsdb, bwidcb
                      ,bhidxb, bhidsdb, bhidcb
                      ,ihidxb, ihidsdb, ihidcb
                      )
  }


  intermediateMessage ( ' .3  Complete summaryby summaries.')

  #Do some resahping to calculate the ratios
  #need to use the data WITH the SIDE Channels for this part
  #the side channels are used as above with some slight variations.
  #for BANKHGT use the mean of the sidechannel pair  (bhgtnox1x)
  #for BANKWID use the sum of the sidechannel pair   (bwidnox1)
  #for WETWID use the sum of the sidechannel pair    (wwidnox1s)

  #need to do each protocol separately

  #need to put back together the datasets after the sidechannels have been figured out.
  #need wetwid, bankhgt, bankwid, and depth with the appropriate side channel treatment
  wadeableRatios <- NULL
  if(nrow(wade)>0) {
      #for wetwid, we use the SUM of the sidechannel pair
      wwidnox1s$PARAMETER <- 'WETWID'
      wwidnox1s$UNITS <- 'M'
      wwidnox1s$PROTOCOL <- 'WADEABLE'

      #for bankhgt, we use the MEAN of the sidechannel pair
      bhgtnox1x$PARAMETER <- 'BANKHGT'
      bhgtnox1x$STATION <- 'NONE'
      bhgtnox1x$UNITS <- 'NONE'
      bhgtnox1x$PROTOCOL <- 'WADEABLE'

      #for bankwid, we ust the SUM of the sidechannel pair.

      bwidnox1$PARAMETER <- 'BANKWID'
      bwidnox1$STATION <- 'NONE'
      bwidnox1$UNITS <- 'NONE'
      bwidnox1$PROTOCOL <- 'WADEABLE'

      wadezero <- subset (wade, wade$STATION %in% c('0', 'NONE'))
  
      wadezerow1 <- subset (wadezero, PARAMETER!='WETWID')
      wadezerow2 <- subset (wwidnox1s, STATION=='0')
      wadezeroww <- rbind (wadezerow1, wadezerow2)

      wadezerobh1 <- subset (wadezeroww, PARAMETER!='BANKHGT')
      wadezerobh <- rbind (wadezerobh1, bhgtnox1x)

      wadezerobw1 <- subset (wadezerobh, PARAMETER!='BANKWID')
      wadezerobw <- rbind (wadezerobw1, bwidnox1)

      wadew1 <- subset (wade, PARAMETER!='WETWID')
      wadeww <- rbind (wadew1, wwidnox1s)

      wadebh1 <- subset (wadeww, PARAMETER!='BANKHGT')
      wadebh <- rbind (wadebh1, bhgtnox1x)

      wadebw1 <- subset (wadebh, PARAMETER!='BANKWID')
      wadebw <- rbind (wadebw1, bwidnox1)

      intermediateMessage ('.4')

      ###########start the experiment here
      #wadebwminus <- subset (wadebw, wadebw$PARAMETER!='DEP_SONR')
      #wadebwminus$STATION <- 0
 
      #  rs1wminus<- reshape (subset(wadebwminus), idvar=c('UID','TRANSECT', 'PROTOCOL', 'STATION'), direction='wide', timevar='PARAMETER')
      #  names(wadebwminus) <- gsub('RESULT\\.', '', names(wadebwminus))
 
      #put depth back in
 
      # wadebwdepth <- subset (wadebw, wadebw$PARAMETER=='DEP_SONR' & STATION=='1', select=-STATION)
  
  
      #rs1w <-  merge (rs1wminus, wadebwdepth, by= c('UID', 'TRANSECT', 'PROTOCOL'), all=TRUE)

      #rs1w <- rename(rs1w, 'RESULT', 'RESULT.DEP_SONR')

      ###########end the experiment here

      rs1w0 <- reshape(subset (wadezerobw, select= - STATION), idvar=c('UID','TRANSECT', 'PROTOCOL'), direction='wide', timevar='PARAMETER')
      names(wadezerobw) <- gsub('RESULT\\.', '', names(wadezerobw))
  
      rs1w <- reshape(wadebw, idvar=c('UID','TRANSECT', 'PROTOCOL','STATION'), direction='wide', timevar='PARAMETER')
      names(wadebw) <- gsub('RESULT\\.', '', names(wadebw))
 
      #generate seed values to later calculate ratios... see use of sidechannel data as described above.

      rs1w0$bfwd_ratseed <- (rs1w0$RESULT.BANKWID/(rs1w0$RESULT.BANKHGT+rs1w0$RESULT.DEPTH))
  

      rs1w$wdprod <- (rs1w$RESULT.WETWID * rs1w$RESULT.DEPTH)
      rs1w$wdratio <- (rs1w$RESULT.WETWID / rs1w$RESULT.DEPTH)

      intermediateMessage ('.5')

      # next summarize (means)the ratios/products
      mn <- aggregate(rs1w0$bfwd_ratseed
                     ,list('UID'=rs1w0$UID)
                     ,mean , na.rm=TRUE
                     )
    
      mn$METRIC <- 'bfwd_rat'

      mn <- rename (mn, 'x', 'RESULT')
      mn1 <- aggregate(rs1w$wdprod
                      ,list('UID'=rs1w$UID)
                      ,mean , na.rm=TRUE
                      )
    
      mn1$METRIC <- 'xwxd'
      mn1 <- rename(mn1, 'x', 'RESULT')
    
      mn2 <- aggregate(rs1w$wdratio
                      ,list('UID'=rs1w$UID)
                      ,mean , na.rm=TRUE
                      )
    
      mn2$METRIC <- 'xwd_rat'
      mn2 <- rename(mn2, 'x', 'RESULT')
    
      #standard deviations (product and ratio)
    
      sd1 <- aggregate(rs1w$wdprod
                      ,list('UID'=rs1w$UID)
                      ,sd , na.rm=TRUE
                      )
    
      sd1$METRIC <- 'sdwxd'
      sd1 <- rename(sd1, 'x', 'RESULT')
    
      sd2 <- aggregate(rs1w$wdratio
                      ,list('UID'=rs1w$UID)
                      ,sd , na.rm=TRUE
                      )
    
      sd2$METRIC <- 'sdwd_rat'
      sd2 <- rename(sd2, 'x', 'RESULT')
    
    
      # counts for later calculations
      cc <- aggregate(rs1w0$bfwd_ratseed
                     ,list('UID'=rs1w0$UID)
                     ,count
                     )
       cc$METRIC <- 'n_bfrat'
       cc <- rename(cc, 'x', 'RESULT')

      cc1 <- aggregate(rs1w$wdprod
                      ,list('UID'=rs1w$UID)
                      ,count
                      )
    
      cc1$METRIC <- 'n_wd'
      cc1 <- rename(cc1, 'x', 'RESULT')
    
      cc2 <- aggregate(rs1w$wdratio
                      ,list('UID'=rs1w$UID)
                      ,count
                      )
    
      cc2$METRIC <- 'n_wdr'
      cc2 <- rename(cc2, 'x', 'RESULT')

      wadeableRatios <- rbind(mn, mn1, mn2, sd1, sd2, cc, cc1, cc2)
      intermediateMessage ('.6')
  }

  boatableRatios <- NULL
  if(nrow(boat) > 0) {
      # do this for the boat sites
      # for wetwid, we use the SUM of the sidechannel pair
      wwidnoxb1s$PARAMETER <- 'WETWID'
      wwidnoxb1s$STATION <- '0'
      wwidnoxb1s$UNITS <- 'M'
      wwidnoxb1s$PROTOCOL <- 'BOATABLE'

      # for bankhgt, we use the MEAN of the sidechannel pair
      bhgtnoxb1x$PARAMETER <- 'BANKHGT'
      bhgtnoxb1x$STATION <- 'NONE'
      bhgtnoxb1x$UNITS <- 'NONE'
      bhgtnoxb1x$PROTOCOL <- 'BOATABLE'

      # for bankwid, we ust the SUM of the sidechannel pair.
      bwidnoxb1$PARAMETER <- 'BANKWID'
      bwidnoxb1$STATION <- 'NONE'
      bwidnoxb1$UNITS <- 'NONE'
      bwidnoxb1$PROTOCOL <- 'BOATABLE'
 
      boatw1 <- subset (boat, PARAMETER!='WETWID')
      boatww <- rbind (boatw1, wwidnoxb1s)

      boatbh1 <- subset (boatww, PARAMETER!='BANKHGT')
      boatbh <- rbind (boatbh1, bhgtnoxb1x)
 
      boatbw1 <- subset (boatbh, PARAMETER!='BANKWID')
      boatbw <- rbind (boatbw1, bwidnoxb1)

      boatbwminus <- subset (boatbw, boatbw$PARAMETER!='DEPTH')
      boatbwminus$STATION <- 0
 
      rs1bminus<- reshape (subset(boatbwminus), idvar=c('UID','TRANSECT', 'PROTOCOL', 'STATION'), direction='wide', timevar='PARAMETER')
      names(boatbwminus) <- gsub('RESULT\\.', '', names(boatbwminus))
 
      # put depth back in
      boatbwdepth <- subset (boatbw, boatbw$PARAMETER=='DEPTH' & STATION=='0', select=-STATION)
  
      intermediateMessage ('.7')
  
      rs1b <-  merge (rs1bminus, boatbwdepth, by= c('UID', 'TRANSECT', 'PROTOCOL'), all=TRUE)

      rs1b <- rename(rs1b, 'RESULT', 'RESULT.DEPTH')
  
      # do these for the boat

      rs1b$bfwd_ratseed <- (rs1b$RESULT.BANKWID/(rs1b$RESULT.BANKHGT+rs1b$RESULT.DEPTH))

      rs1b$wdprod <- (rs1b$RESULT.WETWID * rs1b$RESULT.DEPTH)  #####CHANGED: Use WETWID, not BANKWID #####
      rs1b$wdratio <- (rs1b$RESULT.WETWID / rs1b$RESULT.DEPTH) #####CHANGED: Use WETWID, not BANKWID #####

      intermediateMessage ('.8')

      # next summarize (means)the ratios/products
      mnb <- aggregate(rs1b$bfwd_ratseed
                      ,list('UID'=rs1b$UID)  #####CHANGED: Use rs1w0, not rs1w #####
                      ,mean , na.rm=TRUE
                      )
    
      mnb$METRIC <- 'bfwd_rat'
      mnb <- rename(mnb, 'x', 'RESULT')
      mn1b <- aggregate(rs1b$wdprod
                       ,list('UID'=rs1b$UID)
                       ,mean , na.rm=TRUE
                       )
    
      mn1b$METRIC <- 'xwxd'
      mn1b <- rename (mn1b, 'x', 'RESULT')
    
      mn2b <- aggregate(rs1b$wdratio
                       ,list('UID'=rs1b$UID)
                       ,mean , na.rm=TRUE
                       )
    
      mn2b$METRIC <- 'xwd_rat'
      mn2b <- rename(mn2b, 'x', 'RESULT')
    
      # standard deviations (product and ratio)
      sd1b <- aggregate(rs1b$wdprod
                       ,list('UID'=rs1b$UID)
                       ,sd , na.rm=TRUE
                       )
    
      sd1b$METRIC <- 'sdwxd'
      sd1b <- rename(sd1b, 'x', 'RESULT')
    
      sd2b <- aggregate(rs1b$wdratio
                       ,list('UID'=rs1b$UID)
                       ,sd , na.rm=TRUE
                       )
    
      sd2b$METRIC <- 'sdwd_rat'
      sd2b <- rename(sd2b, 'x', 'RESULT')

      # counts for later calculations
      ccb <- aggregate(rs1b$bfwd_ratseed
                      ,list('UID'=rs1b$UID) ##### CHANGED: Use rs1w0, not rs1w #####
                      ,count
                      )
      ccb$METRIC <- 'n_bfrat'   
      ccb <- rename(ccb,'x', 'RESULT')   

      cc1b <- aggregate(rs1b$wdprod
                       ,list('UID'=rs1b$UID)
                       ,count
                       )
    
      cc1b$METRIC <- 'n_wd'
      cc1b <- rename(cc1b, 'x', 'RESULT')
    
      cc2b <- aggregate(rs1b$wdratio
                       ,list('UID'=rs1b$UID)
                       ,count
                       )
    
      cc2b$METRIC <- 'n_wdr'
      cc2b <- rename(cc2b, 'x', 'RESULT')
      
      boatableRatios <- rbind(mnb, mn1b, mn2b, sd1b,sd2b,ccb,cc1b,cc2b)
  }
  
  intermediateMessage ( '.9 Complete ratios.')
    
  # whats left are the thalweg depth summaries.... these are computed at
  # ALL stations.  Convert xdepth and sddepth from m to cm, to be
  # backward-compatible with interpretations of EMAP data.
  thal <- subset (df5, PARAMETER=='DEPTH')
  thal$RESULT <- as.numeric(thal$RESULT)
  thal$RESULT <- ifelse(thal$PROTOCOL=='WADEABLE', thal$RESULT*100, thal$RESULT)
      
  depthx <- summaryby(thal,'mean',"xdepth")
  depthsd <- summaryby(thal,'sd',"sddepth")
  depthc <- aggregate( list('RESULT'=thal$RESULT),list(UID=thal$UID),count)
  depthc$METRIC <- 'n_d'
      
  intermediateMessage ( ' Complete depth summaries.', loc='end')

  # things to put together
  mets <- rbind (intest, wadeableRatios, boatableRatios, depthx, depthsd, depthc)

  intermediateMessage('  Done.', loc='end')

  return(mets)

}



# end of file
