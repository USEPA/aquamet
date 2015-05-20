metsSubstrateCharacterization <- function(channelcrosssection, thalweg,
   littoral) {

################################################################################
# Function: metsSubstrateCharacterization
# Title: Calculate NRSA Substrate Characterization Metrics
# Programmers: Marlys Cappert
#              Curt Seeliger
#              Tom Kincaid
# Date: February 11, 2010
# Description:
#   This function calculates the substrate characterization portion of the
#   physical habitat metrics for National Rivers and Streams Assessment (NRSA)
#   data.  The function requires data frames containing the channel
#   crosssection, thalweg, and littoral data files.
# Stream Metrics:
#   d16, d50, d84, dgm, lsub2d16, lsub2d16inor, lsub2d25, lsub2d50,
#   lsub2d50inor, lsub2d75, lsub2d84, lsub2d84inor, lsub2dmm, lsub2dmm_nor,
#   lsub2iqr, lsubd2sd,lsubd2sd_nor, lsubd_sd, lsubd_sd_nor, lsub_d16, lsub_d25,
#   lsub_d50, lsub_d75, lsub_d84, lsub_dmm, lsub_dmm_nor, lsub_iqr, n, n_nor,
#   pct_bdrk, pct_bigr, pct_bl, pct_cb, pct_fn, pct_gc, pct_gf, pct_hp, pct_org,
#   pct_om, pct_ot, pct_rc, pct_rr, pct_rs, pct_sa, pct_safn, pct_sb, pct_sfgf,
#   pct_wd, pct_xb, sub2dmm_nor, subd2sd_nor, subd_sd_nor, sub_dmm_nor
# River Metrics:
#   d16, d50, d84, dgm, LDCBF_G08, lsub2d16inor, lsub2d50inor, lsub2d84inor,
#   lsubd_sd, lsub_d16, lsub_d25, lsub_d50, lsub_d75, lsub_d84, lsub_dmm,
#   lsub_iqr, n,pct_bh, pct_bl, pct_cb, pct_dbbl, pct_dbcb, pct_dbfn, pct_dbgc,
#   pct_dbgf, pct_dbhp, pct_dbom, pct_dbot, pct_dbrc, pct_dbrr, pct_dbrs,
#   pct_dbsa, pct_dbsb, pct_dbwd, pct_dbxb, pct_dsbl, pct_dscb, pct_dsfn,
#   pct_dsgc, pct_dsgf, pct_dshp, pct_dsom, pct_dsot, pct_dsrc, pct_dsrr,
#   pct_dsrs, pct_dssa, pct_dssb, pct_dswd, pct_dsxb, pct_fn, pct_gr, pct_ot,
#   pct_sa, pct_safn, pct_sbbl, pct_sbcb, pct_sbfn, pct_sbgc, pct_sbgf,
#   pct_sbhp, pct_sbom, pct_sbot, pct_sbrc, pct_sbrr, pct_sbrs, pct_sbsa,
#   pct_sbsb, pct_sbwd, pct_sbxb, pct_ssbl, pct_sscb, pct_ssfn, pct_ssgc,
#   pct_ssgf, pct_sshp, pct_ssom, pct_ssot, pct_ssrc, pct_ssrr, pct_ssrs,
#   pct_sssa, pct_sssb, pct_sswd, pct_ssxb,
# Function Revisions:
#   02/11/10 mrc: Started.
#   03/19/10 cws: Set all=TRUE in unit test merge of expected and actual
#          results.  Removed unwanted calculations from metrics, adding missing
#          calcs to metrics, correcting code and test data.
#   03/31/10 cws: Added call to on.exit().
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/03/10 cws: Modified to handle single-protocol datasets, and unit test
#            updated to test this.
#   11/09/10 cws: Modified to put size class information (diameters and
#            groupings) at top of function definition where they can be quickly
#            found and changed if need be. Updated file-io portion to handle
#            cases when a absent is absent from the database (i.e with single
#            protocol datasets)
#   01/20/11 cws: Determined that unit test did not contain any sites with HP or
#            RS class substrate.  Documented which WEMAP sites were used for
#            which UID in the tests, and added a case with HP. Calculations
#            changed as follows: Added HP to wadeableNumericTwoBoulderClasses,
#            removed HP from wadeableMobileTwoBoulderClasses and
#            wadeableMobileOneBoulderClass; added min & max for HP class in
#            subsInfo data.frame.  Modificaitons result in changes to lsub_d16,
#            lsub_d25, lsub_d50, lsub_d75, lsub_d84, lsub2d16, lsub2d25,
#            lsub2d50, lsub2d75, lsub2d84, lsub_dmm, lsub2dmm, lsub_dmm_nor,
#            lsub2dmm_nor, lsub_iqr, lsub2iqr, lsubd_sd, lsubd_sd_nor,
#            sub_dmm_nor, sub_sd_nor.
#   03/21/12 cws: PCT_ values not showing up when sites are entirely bedrock
#            classes. Counts made for individual class groups (all, mineral and
#            measurable) were merged in a way that excluded a site if it did not
#            occur in one of them.  Now retaining all counts during merge, and
#            setting NA counts to 0 (note: sites with no substrate data will
#            have no substrate mets, so N in these cases will still be NA).
#            Changed deprecated ** operator to ^.  Unit test updated
#            accordingly.
#   08/03/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written.
#   12/27/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   05/21/14 kab: Simplified aggregation functions to accomodate smaller datasets
#            that might not have some substrates and breaks old code.
#    5/18/15 cws: Explicit use of aquamet::rename causes development problems 
#            when a) that package is not installed, and b) when the current code
#            base differs from the available package.  Rephrased to use name
#            the column correctly in aggregate(), or use dplyr::rename instead,
#            as appropriate
#
# Arguments:
#   channelcrosssection = a data frame containing the channel crosssection data
#     file.  The data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       TRANSDIR - transverse location along transect
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
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
#   littoral = a data frame containing the littoral data file.  The
#     data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       FLAG - flag
#   Note that possible values for variables in the input data frame are
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
#   metsSubstrateCharacterization.1 - calculate metrics
################################################################################

# Print an initial message
  cat('Substrate Characterization calculations:\n')

# Convert factors to character variables in the input data frames
  intermediateMessage('.1 Convert factors to character variables.', loc='end')
  channelcrosssection <- convert_to_char(channelcrosssection)
  thalweg <- convert_to_char(thalweg)
  littoral <- convert_to_char(littoral)

# Subset the channelcrosssection, thalweg, and littoral data frames to retain
# desired values in the column named PARAMETER
  intermediateMessage('.2 Subset the data frame.', loc='end')
  df1 <- subset(channelcrosssection, PARAMETER %in% c('SIZE_CLS', 'XSIZE_CLS'))
  df2 <- subset (thalweg, PARAMETER == 'SIZE_CLS')
  df3 <- subset (littoral, PARAMETER %in% c('SHOREDOM', 'BOTTOMSEC', 'SHORESEC',
    'BOTTOMDOM'))

# Calculate the metrics
  intermediateMessage('.3 Call function metsSubstrateCharacterization.1.', loc='end')
  mets <- metsSubstrateCharacterization.1 (df1, df2, df3)
  row.names(mets) <- 1:nrow(mets)

# Print an exit message
  intermediateMessage('Done.', loc='end')

# Return results
  return(mets)
}



metsSubstrateCharacterization.1 <- function(df1, df2, df3) {

#Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.
#
# ARGUMENTS:
# df1   dataframe of the channel cross section data.
# df2   dataframe of the thalweg data.
# df3   dataframe of the littoral data.

  intermediateMessage ('Substrate Characterization', loc='start')

  # Substrate class information
  tt <- textConnection(
                "class min     max
                    RS 4000    8000
                    RR 4000    8000
                    RC 4000    8000
                    BH 4000    8000
                    XB 1000    4000
                    SB  250    1000
                    BL  250    4000
                    CB   64     250
                    GC   16      64
                    GF    2      16
                    GR    2      64
                    SA    0.06    2
                    FN    0.001   0.06
                    HP 4000    8000
#                    HP   NA      NA
                    WD   NA      NA
                    OT   NA      NA
                "
               )
  subsInfo <- read.table(tt, header=TRUE, stringsAsFactors=FALSE)
  close(tt)
  subsInfo$diam <- NA
  for(s in 1:nrow(subsInfo)) {
      subsInfo[s,]$diam = gmean(c(subsInfo[s,]$min, subsInfo[s,]$max))
  }
  subsInfo$lmin = log10(subsInfo$min)
  subsInfo$lmax = log10(subsInfo$max)
  subsInfo$lDiam <- log10(subsInfo$diam)

  # Specify simple subsets of substrate salient for each sampling protocol
  # and groups of metrics
  wadeableAllTwoBoulderClasses <- c('RS', 'RR', 'RC', 'XB', 'SB', 'CB', 'GC'
                                   ,'GF', 'SA', 'FN', 'HP', 'WD', 'OT'
                                  )
  wadeableMobileTwoBoulderClasses <- c('XB', 'SB', 'CB', 'GC', 'GF', 'SA'
                                      ,'FN', 'WD', 'OT'
                                      )
  wadeableMeasurableTwoBoulderClasses <- c('XB','SB','CB','GC','GF','SA','FN')
  wadeableAllOneBoulderClass <- c('RS', 'RR', 'RC', 'BL', 'CB', 'GC', 'GF'
                                 ,'SA', 'FN', 'HP', 'WD', 'OT'
                                 )
  wadeableMobileOneBoulderClass <- c('BL','CB','GC','GF','SA', 'FN'
                                    ,'WD', 'OT'
                                    )
  wadeableNumericTwoBoulderClasses <- c('RS', 'RR', 'RC', 'XB', 'SB', 'CB'
                                       ,'GC', 'GF', 'SA', 'FN', 'HP'
                                       )
  boatableAllThalwegClasses <- c('BH', 'BL', 'CB', 'GR', 'SA', 'FN', 'OT')
  boatableNumericThalwegClasses <- c('BH', 'BL', 'CB', 'GR', 'SA', 'FN')
  boatableLittoralClasses <- c('RS', 'RR', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA'
                              ,'FN', 'HP', 'WD', 'OT', 'BL', 'OM', 'RC'
                              )

  intermediateMessage('.1')
 
  mets <- NULL   # Accumulate calculations here
  
  if(nrow(as.data.frame(df1))>0) {
      # for each of these subsets of data above, we want summaries (lDiam) for all
      # classes with numeric values 16, 25, 50, 75, 84, mean, std, iqr
      ldBugmm <- merge(df1
                      ,subset(subsInfo
                             ,class %in% wadeableAllTwoBoulderClasses
                             ,select=c(class,lDiam)
                             ) #diametersmm
                      ,by.x='RESULT', by.y='class'
                      ,all.x=TRUE)

      ldBug2mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2mm$METRIC <- 'lsub2d16'

      ldBug4mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4mm$METRIC <- 'lsub2d25'

      ldBug6mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug6mm$METRIC <- 'lsub2d50'

      ldBug8mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8mm$METRIC <- 'lsub2d75'

      ldBug10mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug10mm$METRIC <- 'lsub2d84'

      # additional summaries
      ldBug12mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                            ,list('UID'=ldBugmm$UID)
                            ,mean, na.rm=TRUE
                            )
      ldBug12mm$METRIC <- 'lsub2dmm'
      ldBug14mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                            ,list('UID'=ldBugmm$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14mm$METRIC <- 'lsubd2sd'
      ldBug16mm <- aggregate(list(RESULT = ldBugmm$lDiam)
                            ,list('UID'=ldBugmm$UID)
                            ,iqr
                            )
      ldBug16mm$METRIC <- 'lsub2iqr'

      # Completed size classes summaries for streams (ldiam)
      intermediateMessage('.2')


      # summaries for the tt dataset (NOR) (ldiam AND diam)
      # USE wadeableMobileTwoBoulderClasses
      ldBugtt <- merge(df1
                      ,subset(subsInfo
                             ,class %in% wadeableMobileTwoBoulderClasses
                             ,select=c(class,diam,lDiam)
                             ) # diameterstt
                      ,by.x='RESULT', by.y='class'
                      ,all.x=TRUE)
      ldBug12tt <- aggregate(list(lsub2dmm_nor = ldBugtt$lDiam)
                            ,list('UID'=ldBugtt$UID)
                            ,mean, na.rm=TRUE
                            )
#    ldBug12tt$METRIC <- 'lsub2dmm_nor'
      intermediateMessage('.3')
    
      # special extra calculation for DGM
      ldBug12tt$dgm <- 10^ldBug12tt$lsub2dmm_nor

      ldBug12tt <- reshape(ldBug12tt, idvar=c('UID'), direction='long'
                          ,varying=names(ldBug12tt)[names(ldBug12tt) != 'UID']
                          ,times=names(ldBug12tt)[names(ldBug12tt) != 'UID']
                          ,v.names='RESULT', timevar='METRIC'
                          )
      row.names(ldBug12tt)<-NULL


      ldBug14tt <- aggregate(list(RESULT = ldBugtt$lDiam)
                            ,list('UID'=ldBugtt$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14tt$METRIC <- 'lsubd2sd_nor'

      dBug12tt <- aggregate(list(RESULT = ldBugtt$diam)
                           ,list('UID'=ldBugtt$UID)
                           ,mean, na.rm=TRUE
                           )
      dBug12tt$METRIC <- 'sub2dmm_nor'

      dBug14tt <- aggregate(list(RESULT = ldBugtt$diam)
                           ,list('UID'=ldBugtt$UID)
                           ,sd, na.rm=TRUE
                           )
      dBug14tt$METRIC <- 'subd2sd_nor'

      intermediateMessage('.4')

      streamld <- rbind ( ldBug2mm, ldBug4mm, ldBug6mm, ldBug8mm, ldBug10mm
                      ,ldBug12mm, ldBug14mm, ldBug16mm,ldBug12tt, ldBug14tt
                      ,dBug12tt, dBug14tt)


      #interpolated metrics
      # USE wadeableMeasurableTwoBoulderClasses
      interpdata <- subset (df1, RESULT %in% wadeableMeasurableTwoBoulderClasses)#c('XB','SB','CB','GC','GF','SA','FN'))
      measurable <- subset(subsInfo
                          ,class %in% wadeableMeasurableTwoBoulderClasses
                          ,select=c(class,lmin,lmax)
                          ) %>%
                    dplyr::rename(CLASS=class, min=lmin, max=lmax)
      c16 <- interpolatePercentile(interpdata, 'RESULT', 16, 'lsub2d16inor'
                                  ,measurable
                                  )
      c50 <- interpolatePercentile(interpdata, 'RESULT', 50, 'lsub2d50inor', measurable)
      c84 <- interpolatePercentile(interpdata, 'RESULT', 84, 'lsub2d84inor', measurable)

      c16$d16 <- 10^(c16$lsub2d16inor)
      c50$d50 <- 10^(c50$lsub2d50inor)
      c84$d84 <- 10^(c84$lsub2d84inor)

#    c16 <- aquamet::rename(c16, 'lsub2d16InoR', 'RESULT')
#    c16$METRIC <- 'lsub2d16inor'

      calcs <- merge(c16
                    ,merge(c50, c84, by='UID', all=TRUE)
                    ,by='UID'
                    ,all=TRUE
                    )
      calcs <- reshape(calcs, idvar=c('UID'), direction='long'
                      ,varying=names(calcs)[names(calcs) != 'UID']
                      ,times=names(calcs)[names(calcs) != 'UID']
                      ,v.names='RESULT', timevar='METRIC'
                      )
      row.names(calcs)<-NULL

      # Completed size classes summaries for streams (ldiam- NOR)
      intermediateMessage('.5')


      # all the same metrics for the subset with the lumped boulder classes
      df1lb <- df1
      df1lb$RESULT <- ifelse (df1lb$RESULT %in% c('XB', 'SB'),'BL', df1lb$RESULT)

      ldBuglb <- merge(df1lb
                      ,subset(subsInfo
                             ,class %in% wadeableAllOneBoulderClass
                             ,select=c(class,diam,lDiam)
                             ) # diameterslb
                      ,by.x='RESULT', by.y='class'
                      ,all.x=TRUE)

      ldBug2lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2lb$METRIC <- 'lsub_d16'


      ldBug4lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4lb$METRIC <- 'lsub_d25'

      ldBug6lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug6lb$METRIC <- 'lsub_d50'

      ldBug8lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8lb$METRIC <- 'lsub_d75'

      ldBug10lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                            ,list('UID'=ldBuglb$UID)
                            ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                            )
      ldBug10lb$METRIC <- 'lsub_d84'

      intermediateMessage('.6')

      # additional summaries
      ldBug12lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                            ,list('UID'=ldBuglb$UID)
                            ,mean, na.rm=TRUE
                            )
      ldBug12lb$METRIC <- 'lsub_dmm'

      ldBug14lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                            ,list('UID'=ldBuglb$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14lb$METRIC <- 'lsubd_sd'

      ldBug16lb <- aggregate(list(RESULT = ldBuglb$lDiam)
                            ,list('UID'=ldBuglb$UID)
                            ,iqr
                            )
      ldBug16lb$METRIC <- 'lsub_iqr'

      # Complete size classes summaries for streams (lumped boulder class)
      intermediateMessage('.6')


      # special few extra summaries that use the lumped boulder class for the NOR
      df1ttlb <- df1
      df1ttlb$RESULT <- ifelse (df1ttlb$RESULT %in% c('XB', 'SB'),'BL', df1ttlb$RESULT)

      ldBugttbl <- merge(df1ttlb
                        ,subset(subsInfo
                               ,class %in% wadeableMobileOneBoulderClass
                               ,select=c(class,diam,lDiam)
                               ) # diametersttbl
                        ,by.x='RESULT', by.y='class'
                        ,all.x=TRUE)
      ldBug12ttbl <- aggregate(list(RESULT = ldBugttbl$lDiam)
                              ,list('UID'=ldBugttbl$UID)
                              ,mean, na.rm=TRUE
                              )
      ldBug12ttbl$METRIC <- 'lsub_dmm_nor'

      ldBug14ttbl <- aggregate(list(RESULT = ldBugttbl$lDiam)
                              ,list('UID'=ldBugttbl$UID)
                              ,sd, na.rm=TRUE
                              )
      ldBug14ttbl$METRIC <- 'lsubd_sd_nor'

      dBug12ttbl <- aggregate(list(RESULT = ldBugttbl$diam)
                             ,list('UID'=ldBugttbl$UID)
                             ,mean, na.rm=TRUE
                             )
      dBug12ttbl$METRIC <- 'sub_dmm_nor'

      dBug14ttbl <- aggregate(list(RESULT = ldBugttbl$diam)
                             ,list('UID'=ldBugttbl$UID)
                             ,sd, na.rm=TRUE
                             )
      dBug14ttbl$METRIC <- 'subd_sd_nor'

      # Complete size classes summaries for streams (lumped boulder class- NOR).6
      intermediateMessage('.7')

      streamlb <- rbind ( ldBug2lb, ldBug4lb, ldBug6lb, ldBug8lb, ldBug10lb
                        ,ldBug12lb, ldBug14lb, ldBug16lb, ldBug12ttbl, ldBug14ttbl
                        ,dBug12ttbl, dBug14ttbl
                        )


      # moving on to counts and percentages for these, want to count SIZE_CLS/XSIZE_CLS THE SAME,
      # there are three 'n' values associated with these counts
      # realallsize.... every size class
      # allsize ....... every size class with a numeric value
      # norsize........ all the mobile size classes with characteristic diameters
      realallsize <- df1
      realallsize$PARAMETER <- NULL
      allsize <- subset (realallsize, RESULT %in% wadeableNumericTwoBoulderClasses)
      norsize <- subset (realallsize, RESULT %in% wadeableMeasurableTwoBoulderClasses)

      allSZ <- ddply(allsize,c('UID'),summarise,METRIC='n',RESULT=length(na.omit(RESULT)))
      allSZ2 <- ddply(realallsize,c('UID'),summarise,METRIC='n2',RESULT=length(na.omit(RESULT)))
      allNOR <- ddply(norsize,c('UID'),summarise,METRIC='n_nor',RESULT=length(na.omit(RESULT)))

      # get counts for each size class
      realallsize <- mutate(realallsize,METRIC=paste('n',RESULT,sep=''))
      allSZ.size <- ddply(subset(realallsize,RESULT!='BL'),c('UID','METRIC'),summarise,RESULT=length(na.omit(RESULT))) 
      allSZBL <- ddply(subset(realallsize,RESULT %in% c('XB','SB')),c('UID'),summarise,METRIC='nBL',RESULT=length(na.omit(RESULT)))
      allSZ.comb <- rbind(allSZ.size,allSZBL)

      # Create empty data frame with all expected output metric names to make sure they are included
      empty.df <- expand.grid(UID=unique(df1$UID),METRIC=c('nBL','nCB','nFN','nGC','nGF','nHP','nOT','nOM','nRC','nRR','nRS','nSA','nSB','nWD','nXB'))
      allSZ.comb.1 <- merge(empty.df,allSZ.comb,by=c('UID','METRIC'),all.x=TRUE)
      allSZ.comb.1 <- mutate(allSZ.comb.1,RESULT=ifelse(is.na(RESULT),0,RESULT))
      
  
#       allSZBL <- subset (realallsize, realallsize$RESULT %in% c('XB', 'SB'))
#       allSZBL <- ddply(allSZBL,c('UID'),summarise,METRIC='nBL',)
#       allSZBL<-aggregate(allSZBL$RESULT
#                         ,list('UID'=allSZBL$UID)
#                         ,count
#                         )
#       allSZBL <- rename(allSZBL, 'x', 'nBL')
# 
#       allSZCB <- subset (realallsize, realallsize$RESULT=='CB')
#       allSZCB<-aggregate(allSZCB$RESULT
#                         ,list('UID'=allSZCB$UID)
#                         ,count
#                         )
#       allSZCB <- rename(allSZCB, 'x', 'nCB')
#       allSZFN <- subset (realallsize, realallsize$RESULT=='FN')
# 
#       allSZFN<-aggregate(allSZFN$RESULT
#                         ,list('UID'=allSZFN$UID)
#                         ,count
#                         )
#       allSZFN <- rename(allSZFN, 'x', 'nFN')
# 
#       allSZGC <- subset (realallsize, realallsize$RESULT=='GC')
#       allSZGC<-aggregate(allSZGC$RESULT
#                         ,list('UID'=allSZGC$UID)
#                         ,count
#                         )
#       allSZGC <- rename(allSZGC, 'x', 'nGC')
# 
#       allSZGF <- subset (realallsize, realallsize$RESULT=='GF')
#       allSZGF<-aggregate(allSZGF$RESULT
#                         ,list('UID'=allSZGF$UID)
#                         ,count
#                         )
#       allSZGF <- rename(allSZGF, 'x', 'nGF')
# 
#       allSZHP <- subset (realallsize, realallsize$RESULT=='HP')
#       if (nrow (allSZHP) == 0 ) {
#           allSZHP <- data.frame ('UID'=unique(realallsize$UID)
#                                 ,nHP=0
#                                 ,stringsAsFactors=FALSE
#                                 )
# 
#       } else {
#           allSZHP<-aggregate(allSZHP$RESULT
#                             ,list('UID'=allSZHP$UID)
#                             ,count
#                             )
#           allSZHP <- rename(allSZHP, 'x', 'nHP')
#       }
# 
#       allSZOT <- subset (realallsize, realallsize$RESULT=='OT')
#       allSZOT<-aggregate(allSZOT$RESULT
#                         ,list('UID'=allSZOT$UID)
#                         ,count
#                         )
#       allSZOT <- rename(allSZOT, 'x', 'nOT')
# 
#       allSZOM <- subset (realallsize, realallsize$RESULT=='OM')
#       if (nrow (allSZOM) == 0 ) {
#           allSZOM <- data.frame ('UID'=unique(realallsize$UID)
#                                 ,nOM=0
#                                 ,stringsAsFactors=FALSE
#                                 )
#       } else {
#           allSZOM<-aggregate(allSZOM$RESULT
#                             ,list('UID'=allSZOM$UID)
#                             ,count
#                             )
#           allSZOM <- rename(allSZOM, 'x', 'nOM')
#       }
# 
#       allSZRC <- subset (realallsize, realallsize$RESULT=='RC')
#       if (nrow (allSZRC) == 0 ) {
#           allSZRC <- data.frame ('UID'=unique(realallsize$UID)
#                                 ,nRC=0
#                                 ,stringsAsFactors=FALSE
#                                 )
#       } else {
#           allSZRC<-aggregate(allSZRC$RESULT
#                             ,list('UID'=allSZRC$UID)
#                             ,count
#                             )
#           allSZRC <- rename(allSZRC, 'x', 'nRC')
#       }
# 
#       allSZRR <- subset (realallsize, realallsize$RESULT=='RR')
#       allSZRR<-aggregate(allSZRR$RESULT
#                         ,list('UID'=allSZRR$UID)
#                         ,count
#                         )
#       allSZRR <- rename(allSZRR, 'x', 'nRR')
# 
#       allSZRS <- subset (realallsize, realallsize$RESULT=='RS')
#       allSZRS<-aggregate(allSZRS$RESULT
#                         ,list('UID'=allSZRS$UID)
#                         ,count
#                         )
#       allSZRS <- rename(allSZRS, 'x', 'nRS')
# 
#       allSZSA <- subset (realallsize, realallsize$RESULT=='SA')
#       allSZSA<-aggregate(allSZSA$RESULT
#                         ,list('UID'=allSZSA$UID)
#                         ,count
#                         )
#       allSZSA <- rename(allSZSA, 'x', 'nSA')
# 
#       allSZSB <- subset (realallsize, realallsize$RESULT=='SB')
#       allSZSB<-aggregate(allSZSB$RESULT
#                         ,list('UID'=allSZSB$UID)
#                         ,count
#                         )
#       allSZSB <- rename(allSZSB, 'x', 'nSB')
# 
#       allSZWD <- subset (realallsize, realallsize$RESULT=='WD')
#       allSZWD<-aggregate(allSZWD$RESULT
#                         ,list('UID'=allSZWD$UID)
#                         ,count
#                         )
#       allSZWD <- rename(allSZWD, 'x', 'nWD')
# 
#       allSZXB <- subset (realallsize, realallsize$RESULT=='XB')
#       allSZXB<-aggregate(allSZXB$RESULT
#                         ,list('UID'=allSZXB$UID)
#                         ,count
#                         )
#       allSZXB <- rename(allSZXB, 'x', 'nXB')

      one <-   dplyr::rename(allNOR, n_nor = RESULT)
      one$METRIC <- NULL
      two <-   dplyr::rename(allSZ, n = RESULT)
      two$METRIC <- NULL
      three<-   dplyr::rename(allSZ2, n2 = RESULT)
      three$METRIC <- NULL

      intermediateMessage('.8')

#      pct <- merge(one, two, by='UID', all.x=TRUE, all.y=FALSE)
#      pct0 <- merge (pct, three, by='UID', all.x=TRUE, all.y=FALSE)
      pct0 <- within(merge(merge(one, two, by='UID', all.x=TRUE, all.y=TRUE)
                          ,three, by='UID', all.x=TRUE, all.y=TRUE
                          )
                    ,{n <- ifelse(is.na(n), 0, n)
                      n_nor <- ifelse(is.na(n_nor), 0, n_nor)
                      n2 <- ifelse(is.na(n2), 0, n2)
                     }
                    )
      pct1 <- merge(allSZ.comb.1,pct0,by='UID') 
      pct2 <- mutate(pct1,METRIC=paste('pct_',tolower(substring(METRIC,2,3)),sep=''),RESULT=(RESULT/n2)*100)

#       pct1 <- merge(pct0, allSZBL, by='UID', all.x=TRUE, all.y=FALSE)
#       pct2 <- merge(pct1, allSZCB, by='UID', all.x=TRUE, all.y=FALSE)
#       pct3 <- merge(pct2, allSZFN, by='UID', all.x=TRUE, all.y=FALSE)
#       pct4 <- merge(pct3, allSZGC, by='UID', all.x=TRUE, all.y=FALSE)
#       pct5 <- merge(pct4, allSZGF, by='UID', all.x=TRUE, all.y=FALSE)
#       pct6 <- merge(pct5, allSZHP, by='UID', all.x=TRUE, all.y=FALSE)
#       pct7 <- merge(pct6, allSZOT, by='UID', all.x=TRUE, all.y=FALSE)
#       pct7b<- merge(pct7, allSZOM, by='UID', all.x=TRUE, all.y=FALSE)
#       pct8 <- merge(pct7b, allSZRC, by='UID', all.x=TRUE, all.y=FALSE)
#       pct9 <- merge(pct8, allSZRR, by='UID', all.x=TRUE, all.y=FALSE)
#       pct10 <- merge(pct9, allSZRS, by='UID', all.x=TRUE, all.y=FALSE)
#       pct11 <- merge(pct10, allSZSA, by='UID', all.x=TRUE, all.y=FALSE)
#       pct12 <- merge(pct11, allSZSB, by='UID', all.x=TRUE, all.y=FALSE)
#       pct13 <- merge(pct12, allSZWD, by='UID', all.x=TRUE, all.y=FALSE)
#       pct14 <- merge(pct13, allSZXB, by='UID', all.x=TRUE, all.y=FALSE)
# 
#       pct14$pct_bl <- ifelse (is.na(pct14$nBL), 0, (pct14$nBL/pct14$n2)*100)
#       pct14$pct_cb <- ifelse (is.na(pct14$nCB), 0, (pct14$nCB/pct14$n2)*100)
#       pct14$pct_fn <- ifelse (is.na(pct14$nFN), 0, (pct14$nFN/pct14$n2)*100)
#       pct14$pct_gc <- ifelse (is.na(pct14$nGC), 0, (pct14$nGC/pct14$n2)*100)
#       pct14$pct_gf <- ifelse (is.na(pct14$nGF), 0, (pct14$nGF/pct14$n2)*100)
#       pct14$pct_hp <- ifelse (is.na(pct14$nHP), 0, (pct14$nHP/pct14$n2)*100)
#       pct14$pct_om <- ifelse (is.na(pct14$nOM), 0, (pct14$nOM/pct14$n2)*100)
#       pct14$pct_ot <- ifelse (is.na(pct14$nOT), 0, (pct14$nOT/pct14$n2)*100)
#       pct14$pct_rc <- ifelse (is.na(pct14$nRC), 0, (pct14$nRC/pct14$n2)*100)
#       pct14$pct_rr <- ifelse (is.na(pct14$nRR), 0, (pct14$nRR/pct14$n2)*100)
#       pct14$pct_rs <- ifelse (is.na(pct14$nRS), 0, (pct14$nRS/pct14$n2)*100)
#       pct14$pct_sa <- ifelse (is.na(pct14$nSA), 0, (pct14$nSA/pct14$n2)*100)
#       pct14$pct_sb <- ifelse (is.na(pct14$nSB), 0, (pct14$nSB/pct14$n2)*100)
#       pct14$pct_wd <- ifelse (is.na(pct14$nWD), 0, (pct14$nWD/pct14$n2)*100)
#       pct14$pct_xb <- ifelse (is.na(pct14$nXB), 0, (pct14$nXB/pct14$n2)*100)

    
      #some groupings
      pct_bigr <- ddply(subset(pct2,METRIC %in% c('pct_rr','pct_rs','pct_rc','pct_bl','pct_cb','pct_gc'))
                        ,c('UID'),summarise,METRIC='pct_bigr',RESULT=sum(RESULT))
      pct_bdrk <- ddply(subset(pct2,METRIC %in% c('pct_rr','pct_rs')),c('UID'),summarise
                        ,METRIC='pct_bdrk',RESULT=sum(RESULT))
      pct_safn <- ddply(subset(pct2,METRIC %in% c('pct_sa','pct_fn')),c('UID'),summarise
                        ,METRIC='pct_safn',RESULT=sum(RESULT))
      pct_sfgf <- ddply(subset(pct2,METRIC %in% c('pct_sa','pct_fn','pct_gf')),c('UID'),summarise
                        ,METRIC='pct_sfgf',RESULT=sum(RESULT))
      pct_org <- ddply(subset(pct2,METRIC %in% c('pct_om','pct_wd')),c('UID'),summarise
                       ,METRIC='pct_org',RESULT=sum(RESULT))

#       pct14$pct_bigr <- (pct14$pct_rr+pct14$pct_rs+pct14$pct_rc+pct14$pct_bl+pct14$pct_cb+pct14$pct_gc)
#       pct14$pct_bdrk <- (pct14$pct_rr+pct14$pct_rs)
#       pct14$pct_safn <- (pct14$pct_sa+pct14$pct_fn)
#       pct14$pct_sfgf <- (pct14$pct_sa+ pct14$pct_fn+ pct14$pct_gf)
#       pct14$pct_org  <- pct14$pct_om + pct14$pct_wd

      #reshape to bind with earlier metrics
#       pct14$n2 <- NULL
#       pct14$nBL <- NULL
#       pct14$nCB <- NULL
#       pct14$nFN <- NULL
#       pct14$nGC <- NULL
#       pct14$nGF <- NULL
#       pct14$nHP <- NULL
#       pct14$nOM <- NULL
#       pct14$nOT <- NULL
#       pct14$nRS <- NULL
#       pct14$nRC <- NULL
#       pct14$nRR <- NULL
#       pct14$nSA <- NULL
#       pct14$nSB <- NULL
#       pct14$nWD <- NULL
#       pct14$nXB <- NULL
#       pct15 <- reshape(pct14, idvar=c('UID'), direction='long'
#                       ,varying=names(pct14)[names(pct14) != 'UID']
#                       ,times=names(pct14)[names(pct14) != 'UID']
#                       ,v.names='RESULT', timevar='METRIC'
#                       )
#       row.names(pct15)<-NULL
      alln <- melt(subset(pct0,select=-n2),id.vars='UID',variable.name='METRIC',value.name='RESULT')
      pct3 <- rbind(subset(pct2,select=c('UID','METRIC','RESULT')),pct_bigr,pct_bdrk,pct_safn,pct_sfgf,pct_org,alln)
      # Completed size classes percentages for streams
      intermediateMessage('.9')
      
      mets <- rbind (streamld, streamlb, calcs, pct3) 
      intermediateMessage('.10')

  } # end of wadeable substrate processing

  
  # on to the rivers
  # input from the boatable site is in the thalweg2 and littoral2
  # df2 contains all the SIZE_CLS responses from boatable
  if(nrow(as.data.frame(df2))>0) {
      # SIZE_CLS from the rivers have slightly different gmeans.
      # 16, 25, 50, 75, 84, mean, std, iqr....... for rivers data
      ldRivmm <- merge(df2
                      ,subset(subsInfo
                             ,class %in% boatableNumericThalwegClasses
                             ,select=c(class,diam,lDiam)
                             )  # diametersrr
                      ,by.x='RESULT', by.y='class'
                      ,all.x=TRUE)

      ldRivCt <- ddply(df2,c('UID'),summarise,METRIC='n',RESULT=length(na.omit(RESULT)))
#       ldRivCt <- aggregate(df2$RESULT
#                           ,list('UID'=df2$UID)
#                           ,count
#                           )
#       ldRivCt <- rename(ldRivCt, 'x', 'RESULT')
#       ldRivCt$METRIC <- 'n'

      ldRiv1mm <- ddply(ldRivmm,c('UID'),summarise,lsub_d16=quantile(lDiam,probs=0.16,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d25=quantile(lDiam,probs=0.25,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d50=quantile(lDiam,probs=0.50,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d75=quantile(lDiam,probs=0.75,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d84=quantile(lDiam,probs=0.84,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_dmm=mean(lDiam,na.rm=TRUE),lsubd_sd=sd(lDiam,na.rm=TRUE)
                        ,lsub_iqr=lsub_d75-lsub_d25)
      ldRiv1mm.1 <- melt(ldRiv1mm,id.vars='UID',variable.name='METRIC',value.name='RESULT',na.rm=FALSE)

#       ldRiv1mm <- aggregate(ldRivmm$lDiam
#                            ,list('UID'=ldRivmm$UID)
#                            ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
#                            )
#       ldRiv2mm <- rename(ldRiv1mm, 'x', 'RESULT')
#       ldRiv2mm$METRIC <- 'lsub_d16'

#       ldRiv3mm <- aggregate(ldRivmm$lDiam
#                            ,list('UID'=ldRivmm$UID)
#                            ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
#                            )
#       ldRiv4mm <- rename(ldRiv3mm, 'x', 'RESULT')
#       ldRiv4mm$METRIC <- 'lsub_d25'
# 
#       ldRiv5mm <- aggregate(ldRivmm$lDiam
#                            ,list('UID'=ldRivmm$UID)
#                            ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
#                            )
#       ldRiv6mm <- rename(ldRiv5mm, 'x', 'RESULT')
#       ldRiv6mm$METRIC <- 'lsub_d50'
# 
#       ldRiv7mm <- aggregate(ldRivmm$lDiam
#                            ,list('UID'=ldRivmm$UID)
#                            ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
#                            )
#       ldRiv8mm <- rename(ldRiv7mm, 'x', 'RESULT')
#       ldRiv8mm$METRIC <- 'lsub_d75'
# 
#       ldRiv9mm <- aggregate(ldRivmm$lDiam
#                            ,list('UID'=ldRivmm$UID)
#                            ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
#                            )
#       ldRiv10mm <- rename(ldRiv9mm, 'x', 'RESULT')
#       ldRiv10mm$METRIC <- 'lsub_d84'
# 
      intermediateMessage('.11')

      # additional summaries
#       ldRiv11mm <- aggregate(ldRivmm$lDiam
#                             ,list('UID'=ldRivmm$UID)
#                             ,mean, na.rm=TRUE
#                             )
#       ldRiv12mm <- rename(ldRiv11mm, 'x', 'RESULT')
#       ldRiv12mm$METRIC <- 'lsub_dmm'
# 
#       ldRiv13mm <- aggregate(ldRivmm$lDiam
#                             ,list('UID'=ldRivmm$UID)
#                             ,sd, na.rm=TRUE
#                             )
#       ldRiv14mm  <- rename(ldRiv13mm, 'x', 'RESULT')
#       ldRiv14mm$METRIC <- 'lsubd_sd'
# 
#       ldRiv15mm <- aggregate(ldRivmm$lDiam
#                             ,list('UID'=ldRivmm$UID)
#                             ,iqr
#                             )
#       ldRiv16mm <- rename(ldRiv15mm, 'x', 'RESULT')
#       ldRiv16mm$METRIC <- 'lsub_iqr'

      intermediateMessage('.12')

      # put together this summaries
      riv1 <- rbind(ldRivCt, ldRiv1mm.1)
      


      # Initial summaries for rivers.  Get counts for each size class from the
      # back of the thalweg form
      indivcl <- ddply(df2,c('UID','PARAMETER','RESULT'),summarise,n=length(na.omit(RESULT)))
#       indivcl <- aggregate (list('n'=df2$RESULT)
#                            ,list('UID'=df2$UID, 'PARAMETER'=df2$PARAMETER, 'RESULT'=df2$RESULT)
#                            ,count
#                            )
      allct <- ddply(df2,c('UID','PARAMETER'),summarise,nAll=length(na.omit(RESULT)))
#       allct <- aggregate (list('nAll'=df2$RESULT)
#                          ,list ('UID'=df2$UID, 'PARAMETER'=df2$PARAMETER)
#                          ,count
#                          )
      scCTS <- merge(indivcl, allct, by=c('UID', 'PARAMETER'))

      sc <- expand.grid (UID=unique(scCTS$UID)
                        ,RESULT=boatableAllThalwegClasses # c('BH', 'BL', 'CB', 'GR', 'SA', 'FN','OT')
                        )
      sc$RESULT <- as.character(sc$RESULT)

      ss3m <- merge(scCTS, sc, by=c('RESULT', 'UID'), all.y=TRUE)

      ss3m$pct <- (ss3m$n/ss3m$nAll)*100

      ss3m$METRIC <-  paste('pct_', tolower(ss3m$RESULT), sep='')
      ss3m$pct <- ifelse (is.na(ss3m$pct), 0, ss3m$pct)


      # eliminate unesscesary vars to bind with other metric parts.
      ss3m$RESULT <- NULL
      ss3m$PARAMETER <- NULL
      ss3m$n <- NULL
      ss3m$nAll <- NULL

      ss3m <- dplyr::rename(ss3m, RESULT = pct)

      intermediateMessage('.13')

      # Calculate composite metric(s)
      safn <- within(merge(subset(ss3m, METRIC=='pct_sa', select=c(UID,RESULT))
                          ,subset(ss3m, METRIC=='pct_fn', select=c(UID,RESULT))
                          ,by='UID', all=TRUE, suffix=c('.sa', '.fn')
                          )
                    ,RESULT <- RESULT.sa + RESULT.fn
                    )
      safn$METRIC <- 'pct_safn'
      safn <- safn[c('UID','RESULT','METRIC')]
      
      mets <- rbind(mets, riv1, ss3m, safn)

      # Completed counts (pct) from the thawlweg data
      intermediateMessage('.14')
  } # end of boatable thalweg substrate processing
  
  # on to the littoral data
  if(nrow(as.data.frame(df3))>0) {
      indiv <- ddply(df3,c('UID','PARAMETER','RESULT'),summarise,n=length(na.omit(RESULT)))
#       indiv <- aggregate (list('n'=df3$RESULT)
#                          ,list('UID'=df3$UID, 'PARAMETER'=df3$PARAMETER, 'RESULT'=df3$RESULT)
#                          ,count
#                          )

      big4 <- ddply(df3,c('UID','PARAMETER'),summarise,n4=length(na.omit(RESULT)))
#       big4 <- aggregate (list('n4'=df3$RESULT)
#                         ,list ('UID'=df3$UID, 'PARAMETER'=df3$PARAMETER)
#                         ,count
#                         )
      ss4m <- merge(indiv, big4, by=c('UID', 'PARAMETER'))

      ss <- expand.grid (UID=unique(ss4m$UID)
                        ,PARAMETER=c('BOTTOMDOM', 'BOTTOMSEC'
                                    ,'SHOREDOM', 'SHORESEC'
                                    )
                        ,RESULT=boatableLittoralClasses
                        )
      ss$PARAMETER <- as.character(ss$PARAMETER)
      ss$RESULT <- as.character(ss$RESULT)

      ss4m <- merge(ss4m, ss
                   ,by=c('RESULT', 'UID', 'PARAMETER')
                   ,all.y=TRUE
                   )

      ss4m$pct <- (ss4m$n/ss4m$n4)*100
      ss4m$pct <- ifelse (is.na(ss4m$pct), 0, ss4m$pct)

      ss4m$METRIC <-  paste('pct_', ifelse(ss4m$PARAMETER=='BOTTOMDOM', 'db'
                                   ,ifelse(ss4m$PARAMETER=='BOTTOMSEC', 'sb'
                                   ,ifelse(ss4m$PARAMETER=='SHOREDOM' , 'ds'
                                   ,ifelse(ss4m$PARAMETER=='SHORESEC' , 'ss', 'DAMMIT'
                                   ))))
                           ,tolower(ss4m$RESULT)
                           ,sep=''
                           )

      intermediateMessage('.15')

      # eliminate unnecessary vars to bind with other metric parts.
      ss4m$RESULT <- NULL
      ss4m$PARAMETER <- NULL
      ss4m$n <- NULL
      ss4m$n4 <- NULL

      ss4m <- dplyr::rename(ss4m, RESULT = pct)

      # Completed pct from the littoral data
      intermediateMessage('.16')

      #things to put together
      mets <- rbind (mets, ss4m)
  } # end of boatable littoral substrate processing
  
  if(is.null(mets)) {
      intermediateMessage('. No metrics calculated')
  }
  intermediateMessage ( ' Done.', loc='end')

  return(mets)
}



# end of file
