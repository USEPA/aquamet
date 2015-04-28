# metsSubstrateCharacterization.r
#
#calculate Substrate Characterization Metrics for streams and rivers
#
#  2/11/2010 mrc started
#  3/4/2010 errs are currently at 10-3
#  3/19/10 cws setting all=TRUE in unit test merge of expected and actual
#          results.  Removing unwanted calculations from metrics, adding missing
#          calcs to metrics, correcting code and test data.
#  3/31/10 cws Added call to on.exit()
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 11/03/10 cws Modified to handle single-protocol datasets, and unit test
#          updated to test this.
# 11/09/10 cws Modified to put size class information (diameters and groupings)
#          at top of function definition where they can be quickly found and
#          changed if need be. Updated file-io portion to handle cases when
#          a absent is absent from the database (i.e with single protocol
#          datasets)
#  1/20/11 cws Determined that unit test did not contain any sites with HP or RS
#          class substrate.  Documented which WEMAP sites were used for which
#          UID in the tests, and added a case with HP. Calculations changed as
#          follows: Added HP to wadeableNumericTwoBoulderClasses, removed HP
#          from wadeableMobileTwoBoulderClasses and wadeableMobileOneBoulderClass;
#          added min & max for HP class in subsInfo data.frame.  Modificaitons
#          result in changes to lsub_d16, lsub_d25, lsub_d50, lsub_d75, lsub_d84,
#          lsub2d16, lsub2d25, lsub2d50, lsub2d75, lsub2d84, lsub_dmm, lsub2dmm,
#          lsub_dmm_nor, lsub2dmm_nor, lsub_iqr, lsub2iqr, lsubd_sd,
#          lsubd_sd_nor, sub_dmm_nor, sub_sd_nor.
#  3/21/12 cws PCT_ values not showing up when sites are entirely bedrock classes.
#          Counts made for individual class groups (all, mineral and measurable)
#          were merged in a way that excluded a site if it did not occur in one 
#          of them.  Now retaining all counts during merge, and setting NA counts
#          to 0 (note: sites with no substrate data will have no substrate mets,
#          so N in these cases will still be NA).  Changed deprecated ** operator 
#          to ^.  Unit test updated accordingly.
#  6/02/14 cws Modified to handle counts of individual classes when those classes
#          were absent within the data.
#

metsSubstrateCharacterization <- function ()
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none
#
#Stream mets
#,d16,d50,d84,dgm,lsub2d16,lsub2d16inor,lsub2d25,lsub2d50,lsub2d50inor,lsub2d75,
#,lsub2d84,lsub2d84inor,lsub2dmm,lsub2dmm_nor,lsub2iqr,lsubd2sd,lsubd2sd_nor,
#,lsubd_sd,lsubd_sd_nor,lsub_d16,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,
#,lsub_dmm_nor,lsub_iqr,n,n_nor,pct_bdrk,pct_bigr,pct_bl,pct_cb,pct_fn,pct_gc,
#,pct_gf,pct_hp,pct_org,pct_om,pct_ot,pct_rc,pct_rr,pct_rs,pct_sa,pct_safn,pct_sb,
#,pct_sfgf,pct_wd,pct_xb,sub2dmm_nor,subd2sd_nor,subd_sd_nor,sub_dmm_nor,
#
#River mets  
#,d16,d50,d84,dgm,LDCBF_G08,lsub2d16inor,lsub2d50inor,lsub2d84inor,lsubd_sd,lsub_d16,
# ,lsub_d25,lsub_d50,lsub_d75,lsub_d84,lsub_dmm,lsub_iqr,n,pct_bh,pct_bl,pct_cb,
# ,pct_dbbl,pct_dbcb,pct_dbfn,pct_dbgc,pct_dbgf,pct_dbhp,pct_dbom,pct_dbot,pct_dbrc,
# ,pct_dbrr,pct_dbrs,pct_dbsa,pct_dbsb,pct_dbwd,pct_dbxb,pct_dsbl,pct_dscb,pct_dsfn,
# ,pct_dsgc,pct_dsgf,pct_dshp,pct_dsom,pct_dsot,pct_dsrc,pct_dsrr,pct_dsrs,pct_dssa,
# ,pct_dssb,pct_dswd,pct_dsxb,pct_fn,pct_gr,pct_ot,pct_sa,pct_safn,pct_sbbl,pct_sbcb,
# ,pct_sbfn,pct_sbgc,pct_sbgf,pct_sbhp,pct_sbom,pct_sbot,pct_sbrc,pct_sbrr,pct_sbrs,
# ,pct_sbsa,pct_sbsb,pct_sbwd,pct_sbxb,pct_ssbl,pct_sscb,pct_ssfn,pct_ssgc,pct_ssgf,
# ,pct_sshp,pct_ssom,pct_ssot,pct_ssrc,pct_ssrr,pct_ssrs,pct_sssa,pct_sssb,pct_sswd,
# ,pct_ssxb,
{
  #read in the data from tblChannelCrossSection2, tblThalweg2, tblLittoral2
  chan <- odbcConnect (NRSAdbName)
  on.exit(odbcClose(chan))
  df1 <- fetchNRSATable (chan, 'tblChannelCrossSection2')
  if(is.character(df1)) {
      if(df1=="Table 'tblChannelCrossSection2' does not exist in the database") {
          df1<-NULL
      } else {
          return(df1)
      }
  } else {
      df1 <- subset(df1, df1$PARAMETER %in% c('SIZE_CLS', 'XSIZE_CLS'))
  }

  df2 <- fetchNRSATable (chan, 'tblThalweg2')
  if(is.character(df2)) {
      if(df2=="Table 'tblThalweg2' does not exist in the database") {
          df2<-NULL
      } else {
          return(df2)
      }
  } else {
      df2 <- subset (df2, df2$PARAMETER =='SIZE_CLS')
  }

  df3 <- fetchNRSATable (chan, 'tblLittoral2')
  if(is.character(df3)) {
      if(df3=="Table 'tblLittoral2' does not exist in the database") {
          df3<-NULL
      } else {
          return(df3)
      }
  } else {
      df3 <- subset (df3, df3$PARAMETER %in% c('SHOREDOM', 'BOTTOMSEC', 'SHORESEC', 'BOTTOMDOM'))
  }


  #calculate the metrics
  mets <- metsSubstrateCharacterization.1 (df1, df2, df3)
  if(is.character(mets)) return (mets)

  #write the results
  rc <- writeNRSACalcResults (mets, 'metsSubstrateCharacterization.csv')

  return (rc)
}


metsSubstrateCharacterization.1 <- function (df1, df2, df3)
#Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.
#
# ARGUMENTS:
# df1   dataframe of the channel cross section data.
# df2   dataframe of the thalweg data.
# df3   dataframe of the littoral data.
{
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

      ldBug1mm <- aggregate(ldBugmm$lDiam
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2mm <- rename(ldBug1mm, 'x', 'RESULT')
      ldBug2mm$METRIC <- 'lsub2d16'

      ldBug3mm <- aggregate(ldBugmm$lDiam
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4mm <- rename(ldBug3mm, 'x', 'RESULT')
      ldBug4mm$METRIC <- 'lsub2d25'

      ldBug5mm <- aggregate(ldBugmm$lDiam
                           ,list('UID'=ldBugmm$UID)
                          ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                          )
      ldBug6mm <- rename(ldBug5mm, 'x', 'RESULT')
      ldBug6mm$METRIC <- 'lsub2d50'

      ldBug7mm <- aggregate(ldBugmm$lDiam
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8mm <- rename(ldBug7mm, 'x', 'RESULT')
      ldBug8mm$METRIC <- 'lsub2d75'

      ldBug9mm <- aggregate(ldBugmm$lDiam
                           ,list('UID'=ldBugmm$UID)
                           ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug10mm <- rename(ldBug9mm, 'x', 'RESULT')
      ldBug10mm$METRIC <- 'lsub2d84'

      # additional summaries
      ldBug11mm <- aggregate(ldBugmm$lDiam
                            ,list('UID'=ldBugmm$UID)
                            ,mean, na.rm=TRUE
                            )
      ldBug12mm <- rename(ldBug11mm, 'x', 'RESULT')
      ldBug12mm$METRIC <- 'lsub2dmm'
      ldBug13mm <- aggregate(ldBugmm$lDiam
                            ,list('UID'=ldBugmm$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14mm <- rename(ldBug13mm, 'x', 'RESULT')
      ldBug14mm$METRIC <- 'lsubd2sd'
      ldBug15mm <- aggregate(ldBugmm$lDiam
                            ,list('UID'=ldBugmm$UID)
                            ,iqr
                            )
      ldBug16mm <- rename(ldBug15mm, 'x', 'RESULT')
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
      ldBug11tt <- aggregate(ldBugtt$lDiam
                            ,list('UID'=ldBugtt$UID)
                            ,mean, na.rm=TRUE
                            )
#    ldBug12tt <- rename(ldBug11tt, 'x', 'RESULT')
#    ldBug12tt$METRIC <- 'lsub2dmm_nor'
      intermediateMessage('.3')

      # special extra calculation for DGM
      ldBug11tt$dgm <- 10^ldBug11tt$x
      ldBug11tt <- rename(ldBug11tt, 'x', 'lsub2dmm_nor')

      ldBug12tt <- reshape(ldBug11tt, idvar=c('UID'), direction='long'
                          ,varying=names(ldBug11tt)[names(ldBug11tt) != 'UID']
                          ,times=names(ldBug11tt)[names(ldBug11tt) != 'UID']
                          ,v.names='RESULT', timevar='METRIC'
                          )
      row.names(ldBug12tt)<-NULL


      ldBug13tt <- aggregate(ldBugtt$lDiam
                            ,list('UID'=ldBugtt$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14tt <- rename(ldBug13tt, 'x', 'RESULT')
      ldBug14tt$METRIC <- 'lsubd2sd_nor'

      dBug11tt <- aggregate(ldBugtt$diam
                           ,list('UID'=ldBugtt$UID)
                           ,mean, na.rm=TRUE
                           )
      dBug12tt <- rename(dBug11tt, 'x', 'RESULT')
      dBug12tt$METRIC <- 'sub2dmm_nor'

      dBug13tt <- aggregate(ldBugtt$diam
                           ,list('UID'=ldBugtt$UID)
                           ,sd, na.rm=TRUE
                           )
      dBug14tt <- rename(dBug13tt, 'x', 'RESULT')
      dBug14tt$METRIC <- 'subd2sd_nor'

      intermediateMessage('.4')

      streamld <- rbind ( ldBug2mm, ldBug4mm, ldBug6mm, ldBug8mm, ldBug10mm
                      ,ldBug12mm, ldBug14mm, ldBug16mm,ldBug12tt, ldBug14tt
                      ,dBug12tt, dBug14tt)


      #interpolated metrics
      # USE wadeableMeasurableTwoBoulderClasses
      interpdata <- subset (df1, RESULT %in% wadeableMeasurableTwoBoulderClasses)#c('XB','SB','CB','GC','GF','SA','FN'))
      measurable <- rename(subset(subsInfo
                                 ,class %in% wadeableMeasurableTwoBoulderClasses
                                 ,select=c(class,lmin,lmax)
                                 )
                          ,c('class','lmin','lmax')
                          ,c('CLASS','min','max')
                          ) # sizestt
      c16 <- interpolatePercentile(interpdata, 'RESULT', 16, 'lsub2d16inor'
                                  ,measurable
                                  )
      c50 <- interpolatePercentile(interpdata, 'RESULT', 50, 'lsub2d50inor', measurable)
      c84 <- interpolatePercentile(interpdata, 'RESULT', 84, 'lsub2d84inor', measurable)

      c16$d16 <- 10^(c16$lsub2d16inor)
      c50$d50 <- 10^(c50$lsub2d50inor)
      c84$d84 <- 10^(c84$lsub2d84inor)

#    c16 <- rename(c16, 'lsub2d16InoR', 'RESULT')
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

      ldBug1lb <- aggregate(ldBuglb$lDiam
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2lb <- rename(ldBug1lb, 'x', 'RESULT')
      ldBug2lb$METRIC <- 'lsub_d16'


      ldBug3lb <- aggregate(ldBuglb$lDiam
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4lb <- rename(ldBug3lb, 'x', 'RESULT')
      ldBug4lb$METRIC <- 'lsub_d25'

      ldBug5lb <- aggregate(ldBuglb$lDiam
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug6lb <- rename(ldBug5lb, 'x', 'RESULT')
      ldBug6lb$METRIC <- 'lsub_d50'

      ldBug7lb <- aggregate(ldBuglb$lDiam
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8lb <- rename(ldBug7lb, 'x', 'RESULT')
      ldBug8lb$METRIC <- 'lsub_d75'

      ldBug9lb <- aggregate(ldBuglb$lDiam
                           ,list('UID'=ldBuglb$UID)
                           ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug10lb <- rename(ldBug9lb, 'x', 'RESULT')
      ldBug10lb$METRIC <- 'lsub_d84'

      intermediateMessage('.6')

      # additional summaries
      ldBug11lb <- aggregate(ldBuglb$lDiam
                            ,list('UID'=ldBuglb$UID)
                            ,mean, na.rm=TRUE
                            )
      ldBug12lb <- rename(ldBug11lb, 'x', 'RESULT')
      ldBug12lb$METRIC <- 'lsub_dmm'

      ldBug13lb <- aggregate(ldBuglb$lDiam
                            ,list('UID'=ldBuglb$UID)
                            ,sd, na.rm=TRUE
                            )
      ldBug14lb <- rename(ldBug13lb, 'x', 'RESULT')
      ldBug14lb$METRIC <- 'lsubd_sd'

      ldBug15lb <- aggregate(ldBuglb$lDiam
                            ,list('UID'=ldBuglb$UID)
                            ,iqr
                            )
      ldBug16lb <- rename(ldBug15lb, 'x', 'RESULT')
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
      ldBug11ttbl <- aggregate(ldBugttbl$lDiam
                              ,list('UID'=ldBugttbl$UID)
                              ,mean, na.rm=TRUE
                              )
      ldBug12ttbl <- rename(ldBug11ttbl, 'x', 'RESULT')
      ldBug12ttbl$METRIC <- 'lsub_dmm_nor'

      ldBug13ttbl <- aggregate(ldBugttbl$lDiam
                              ,list('UID'=ldBugttbl$UID)
                              ,sd, na.rm=TRUE
                              )
      ldBug14ttbl <- rename(ldBug13ttbl, 'x', 'RESULT')
      ldBug14ttbl$METRIC <- 'lsubd_sd_nor'

      dBug11ttbl <- aggregate(ldBugttbl$diam
                             ,list('UID'=ldBugttbl$UID)
                             ,mean, na.rm=TRUE
                             )
      dBug12ttbl <- rename(dBug11ttbl, 'x', 'RESULT')
      dBug12ttbl$METRIC <- 'sub_dmm_nor'

      dBug13ttbl <- aggregate(ldBugttbl$diam
                             ,list('UID'=ldBugttbl$UID)
                             ,sd, na.rm=TRUE
                             )
      dBug14ttbl <- rename(dBug13ttbl, 'x', 'RESULT')
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

      allSZ<-aggregate(allsize$RESULT
                      ,list('UID'=allsize$UID)
                      ,count
                      )
      allSZ <- rename(allSZ, 'x', 'RESULT')
      allSZ$METRIC <- 'n'

      allSZ2<-aggregate(realallsize$RESULT
                       ,list('UID'=realallsize$UID)
                       ,count
                       )
      allSZ2 <- rename(allSZ2, 'x', 'RESULT')
      allSZ2$METRIC <- 'n2'

      allNOR<-aggregate(norsize$RESULT
                       ,list('UID'=norsize$UID)
                       ,count
                       )
      allNOR <- rename(allNOR, 'x', 'RESULT')
      allNOR$METRIC <- 'n_nor'

      # get counts for each size class
      allSZBL <- subset (realallsize, realallsize$RESULT %in% c('XB', 'SB'))
      if (nrow (allSZBL) == 0 ) {
          allSZBL <- data.frame ('UID'=unique(realallsize$UID)
                                ,nBL=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZBL<-aggregate(allSZBL$RESULT
                            ,list('UID'=allSZBL$UID)
                            ,count
                            )
          allSZBL <- rename(allSZBL, 'x', 'nBL')
          
      }
      
      allSZCB <- subset (realallsize, realallsize$RESULT=='CB')
      if (nrow (allSZCB) == 0 ) {
          allSZCB <- data.frame ('UID'=unique(realallsize$UID)
                                ,nCB=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZCB<-aggregate(allSZCB$RESULT
                            ,list('UID'=allSZCB$UID)
                            ,count
                            )
          allSZCB <- rename(allSZCB, 'x', 'nCB')
      }
      
      allSZFN <- subset (realallsize, realallsize$RESULT=='FN')
      if (nrow (allSZFN) == 0 ) {
          allSZFN <- data.frame ('UID'=unique(realallsize$UID)
                                ,nFN=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZFN<-aggregate(allSZFN$RESULT
                            ,list('UID'=allSZFN$UID)
                            ,count
                            )
          allSZFN <- rename(allSZFN, 'x', 'nFN')
      }
      
      allSZGC <- subset (realallsize, realallsize$RESULT=='GC')
      if (nrow (allSZGC) == 0 ) {
          allSZGC <- data.frame ('UID'=unique(realallsize$UID)
                                ,nGC=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZGC<-aggregate(allSZGC$RESULT
                            ,list('UID'=allSZGC$UID)
                            ,count
                            )
          allSZGC <- rename(allSZGC, 'x', 'nGC')
      }
      
      allSZGF <- subset (realallsize, realallsize$RESULT=='GF')
      if (nrow (allSZGF) == 0 ) {
          allSZGF <- data.frame ('UID'=unique(realallsize$UID)
                                ,nGF=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZGF<-aggregate(allSZGF$RESULT
                        ,list('UID'=allSZGF$UID)
                        ,count
                        )
          allSZGF <- rename(allSZGF, 'x', 'nGF')
      }
      
      allSZHP <- subset (realallsize, realallsize$RESULT=='HP')
      if (nrow (allSZHP) == 0 ) {
          allSZHP <- data.frame ('UID'=unique(realallsize$UID)
                                ,nHP=0
                                ,stringsAsFactors=FALSE
                                )

      } else {
          allSZHP<-aggregate(allSZHP$RESULT
                            ,list('UID'=allSZHP$UID)
                            ,count
                            )
          allSZHP <- rename(allSZHP, 'x', 'nHP')
      }

      allSZOT <- subset (realallsize, realallsize$RESULT=='OT')
      if (nrow (allSZOT) == 0 ) {
          allSZOT <- data.frame ('UID'=unique(realallsize$UID)
                                ,nOT=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZOT<-aggregate(allSZOT$RESULT
                            ,list('UID'=allSZOT$UID)
                            ,count
                            )
          allSZOT <- rename(allSZOT, 'x', 'nOT')
      }
      
      allSZOM <- subset (realallsize, realallsize$RESULT=='OM')
      if (nrow (allSZOM) == 0 ) {
          allSZOM <- data.frame ('UID'=unique(realallsize$UID)
                                ,nOM=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZOM<-aggregate(allSZOM$RESULT
                            ,list('UID'=allSZOM$UID)
                            ,count
                            )
          allSZOM <- rename(allSZOM, 'x', 'nOM')
      }

      allSZRC <- subset (realallsize, realallsize$RESULT=='RC')
      if (nrow (allSZRC) == 0 ) {
          allSZRC <- data.frame ('UID'=unique(realallsize$UID)
                                ,nRC=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZRC<-aggregate(allSZRC$RESULT
                            ,list('UID'=allSZRC$UID)
                            ,count
                            )
          allSZRC <- rename(allSZRC, 'x', 'nRC')
      }

      allSZRR <- subset (realallsize, realallsize$RESULT=='RR')
      if (nrow (allSZRR) == 0 ) {
          allSZRR <- data.frame ('UID'=unique(realallsize$UID)
                                ,nRR=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZRR<-aggregate(allSZRR$RESULT
                        ,list('UID'=allSZRR$UID)
                        ,count
                        )
          allSZRR <- rename(allSZRR, 'x', 'nRR')
      }
      
      allSZRS <- subset (realallsize, realallsize$RESULT=='RS')
      if (nrow (allSZRS) == 0 ) {
          allSZRS <- data.frame ('UID'=unique(realallsize$UID)
                                ,nRS=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZRS<-aggregate(allSZRS$RESULT
                            ,list('UID'=allSZRS$UID)
                            ,count
                            )
          allSZRS <- rename(allSZRS, 'x', 'nRS')
      }

      allSZSA <- subset (realallsize, realallsize$RESULT=='SA')
      if (nrow (allSZSA) == 0 ) {
          allSZsA <- data.frame ('UID'=unique(realallsize$UID)
                                ,nSA=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZSA<-aggregate(allSZSA$RESULT
                            ,list('UID'=allSZSA$UID)
                            ,count
                            )
          allSZSA <- rename(allSZSA, 'x', 'nSA')
      }

      allSZSB <- subset (realallsize, realallsize$RESULT=='SB')
      if (nrow (allSZSB) == 0 ) {
          allSZSB <- data.frame ('UID'=unique(realallsize$UID)
                                ,nSB=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZSB<-aggregate(allSZSB$RESULT
                        ,list('UID'=allSZSB$UID)
                        ,count
                        )
          allSZSB <- rename(allSZSB, 'x', 'nSB')
      }
      
      allSZWD <- subset (realallsize, realallsize$RESULT=='WD')
      if (nrow (allSZWD) == 0 ) {
          allSZWD <- data.frame ('UID'=unique(realallsize$UID)
                                ,nWD=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZWD<-aggregate(allSZWD$RESULT
                        ,list('UID'=allSZWD$UID)
                        ,count
                        )
          allSZWD <- rename(allSZWD, 'x', 'nWD')
      }
      
      allSZXB <- subset (realallsize, realallsize$RESULT=='XB')
      if (nrow (allSZXB) == 0 ) {
          allSZXB <- data.frame ('UID'=unique(realallsize$UID)
                                ,nXB=0
                                ,stringsAsFactors=FALSE
                                )
      } else {
          allSZXB<-aggregate(allSZXB$RESULT
                        ,list('UID'=allSZXB$UID)
                        ,count
                        )
          allSZXB <- rename(allSZXB, 'x', 'nXB')
      }
      
      one <-   rename(allNOR, 'RESULT', 'n_nor')
      one$METRIC <- NULL
      two <-   rename(allSZ, 'RESULT', 'n')
      two$METRIC <- NULL
      three<-   rename(allSZ2, 'RESULT', 'n2')
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
      
      pct1 <- merge(pct0, allSZBL, by='UID', all.x=TRUE, all.y=FALSE)
      pct2 <- merge(pct1, allSZCB, by='UID', all.x=TRUE, all.y=FALSE)
      pct3 <- merge(pct2, allSZFN, by='UID', all.x=TRUE, all.y=FALSE)
      pct4 <- merge(pct3, allSZGC, by='UID', all.x=TRUE, all.y=FALSE)
      pct5 <- merge(pct4, allSZGF, by='UID', all.x=TRUE, all.y=FALSE)
      pct6 <- merge(pct5, allSZHP, by='UID', all.x=TRUE, all.y=FALSE)
      pct7 <- merge(pct6, allSZOT, by='UID', all.x=TRUE, all.y=FALSE)
      pct7b<- merge(pct7, allSZOM, by='UID', all.x=TRUE, all.y=FALSE)
      pct8 <- merge(pct7b, allSZRC, by='UID', all.x=TRUE, all.y=FALSE)
      pct9 <- merge(pct8, allSZRR, by='UID', all.x=TRUE, all.y=FALSE)
      pct10 <- merge(pct9, allSZRS, by='UID', all.x=TRUE, all.y=FALSE)
      pct11 <- merge(pct10, allSZSA, by='UID', all.x=TRUE, all.y=FALSE)
      pct12 <- merge(pct11, allSZSB, by='UID', all.x=TRUE, all.y=FALSE)
      pct13 <- merge(pct12, allSZWD, by='UID', all.x=TRUE, all.y=FALSE)
      pct14 <- merge(pct13, allSZXB, by='UID', all.x=TRUE, all.y=FALSE)

      pct14$pct_bl <- ifelse (is.na(pct14$nBL), 0, (pct14$nBL/pct14$n2)*100)
      pct14$pct_cb <- ifelse (is.na(pct14$nCB), 0, (pct14$nCB/pct14$n2)*100)
      pct14$pct_fn <- ifelse (is.na(pct14$nFN), 0, (pct14$nFN/pct14$n2)*100)
      pct14$pct_gc <- ifelse (is.na(pct14$nGC), 0, (pct14$nGC/pct14$n2)*100)
      pct14$pct_gf <- ifelse (is.na(pct14$nGF), 0, (pct14$nGF/pct14$n2)*100)
      pct14$pct_hp <- ifelse (is.na(pct14$nHP), 0, (pct14$nHP/pct14$n2)*100)
      pct14$pct_om <- ifelse (is.na(pct14$nOM), 0, (pct14$nOM/pct14$n2)*100)
      pct14$pct_ot <- ifelse (is.na(pct14$nOT), 0, (pct14$nOT/pct14$n2)*100)
      pct14$pct_rc <- ifelse (is.na(pct14$nRC), 0, (pct14$nRC/pct14$n2)*100)
      pct14$pct_rr <- ifelse (is.na(pct14$nRR), 0, (pct14$nRR/pct14$n2)*100)
      pct14$pct_rs <- ifelse (is.na(pct14$nRS), 0, (pct14$nRS/pct14$n2)*100)
      pct14$pct_sa <- ifelse (is.na(pct14$nSA), 0, (pct14$nSA/pct14$n2)*100)
      pct14$pct_sb <- ifelse (is.na(pct14$nSB), 0, (pct14$nSB/pct14$n2)*100)
      pct14$pct_wd <- ifelse (is.na(pct14$nWD), 0, (pct14$nWD/pct14$n2)*100)
      pct14$pct_xb <- ifelse (is.na(pct14$nXB), 0, (pct14$nXB/pct14$n2)*100)

      #some groupings
      pct14$pct_bigr <- (pct14$pct_rr+pct14$pct_rs+pct14$pct_rc+pct14$pct_bl+pct14$pct_cb+pct14$pct_gc)
      pct14$pct_bdrk <- (pct14$pct_rr+pct14$pct_rs)
      pct14$pct_safn <- (pct14$pct_sa+pct14$pct_fn)
      pct14$pct_sfgf <- (pct14$pct_sa+ pct14$pct_fn+ pct14$pct_gf)
      pct14$pct_org  <- pct14$pct_om + pct14$pct_wd

      #reshape to bind with earlier metrics
      pct14$n2 <- NULL
      pct14$nBL <- NULL
      pct14$nCB <- NULL
      pct14$nFN <- NULL
      pct14$nGC <- NULL
      pct14$nGF <- NULL
      pct14$nHP <- NULL
      pct14$nOM <- NULL
      pct14$nOT <- NULL
      pct14$nRS <- NULL
      pct14$nRC <- NULL
      pct14$nRR <- NULL
      pct14$nSA <- NULL
      pct14$nSB <- NULL
      pct14$nWD <- NULL
      pct14$nXB <- NULL
      pct15 <- reshape(pct14, idvar=c('UID'), direction='long'
                      ,varying=names(pct14)[names(pct14) != 'UID']
                      ,times=names(pct14)[names(pct14) != 'UID']
                      ,v.names='RESULT', timevar='METRIC'
                      )
      row.names(pct15)<-NULL

      # Completed size classes percentages for streams
      intermediateMessage('.9')
      
      mets <- rbind (streamld, streamlb, calcs, pct15)
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

      ldRivCt <- aggregate(df2$RESULT
                          ,list('UID'=df2$UID)
                          ,count
                          )
      ldRivCt <- rename(ldRivCt, 'x', 'RESULT')
      ldRivCt$METRIC <- 'n'

      ldRiv1mm <- aggregate(ldRivmm$lDiam
                           ,list('UID'=ldRivmm$UID)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldRiv2mm <- rename(ldRiv1mm, 'x', 'RESULT')
      ldRiv2mm$METRIC <- 'lsub_d16'

      ldRiv3mm <- aggregate(ldRivmm$lDiam
                           ,list('UID'=ldRivmm$UID)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldRiv4mm <- rename(ldRiv3mm, 'x', 'RESULT')
      ldRiv4mm$METRIC <- 'lsub_d25'

      ldRiv5mm <- aggregate(ldRivmm$lDiam
                           ,list('UID'=ldRivmm$UID)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldRiv6mm <- rename(ldRiv5mm, 'x', 'RESULT')
      ldRiv6mm$METRIC <- 'lsub_d50'

      ldRiv7mm <- aggregate(ldRivmm$lDiam
                           ,list('UID'=ldRivmm$UID)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldRiv8mm <- rename(ldRiv7mm, 'x', 'RESULT')
      ldRiv8mm$METRIC <- 'lsub_d75'

      ldRiv9mm <- aggregate(ldRivmm$lDiam
                           ,list('UID'=ldRivmm$UID)
                           ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                           )
      ldRiv10mm <- rename(ldRiv9mm, 'x', 'RESULT')
      ldRiv10mm$METRIC <- 'lsub_d84'

      intermediateMessage('.11')

      # additional summaries
      ldRiv11mm <- aggregate(ldRivmm$lDiam
                            ,list('UID'=ldRivmm$UID)
                            ,mean, na.rm=TRUE
                            )
      ldRiv12mm <- rename(ldRiv11mm, 'x', 'RESULT')
      ldRiv12mm$METRIC <- 'lsub_dmm'

      ldRiv13mm <- aggregate(ldRivmm$lDiam
                            ,list('UID'=ldRivmm$UID)
                            ,sd, na.rm=TRUE
                            )
      ldRiv14mm  <- rename(ldRiv13mm, 'x', 'RESULT')
      ldRiv14mm$METRIC <- 'lsubd_sd'

      ldRiv15mm <- aggregate(ldRivmm$lDiam
                            ,list('UID'=ldRivmm$UID)
                            ,iqr
                            )
      ldRiv16mm <- rename(ldRiv15mm, 'x', 'RESULT')
      ldRiv16mm$METRIC <- 'lsub_iqr'

      intermediateMessage('.12')

      # put together this summaries
      riv1 <- rbind(ldRivCt, ldRiv2mm, ldRiv4mm, ldRiv6mm, ldRiv8mm, ldRiv10mm, ldRiv12mm
                     , ldRiv14mm, ldRiv16mm)


      # Initial summaries for rivers.  Get counts for each size class from the
      # back of the thalweg form
      indivcl <- aggregate (list('n'=df2$RESULT)
                           ,list('UID'=df2$UID, 'PARAMETER'=df2$PARAMETER, 'RESULT'=df2$RESULT)
                           ,count
                           )

      allct <- aggregate (list('nAll'=df2$RESULT)
                         ,list ('UID'=df2$UID, 'PARAMETER'=df2$PARAMETER)
                         ,count
                         )
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

      ss3m <- rename(ss3m, 'pct', 'RESULT')

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
      indiv <- aggregate (list('n'=df3$RESULT)
                         ,list('UID'=df3$UID, 'PARAMETER'=df3$PARAMETER, 'RESULT'=df3$RESULT)
                         ,count
                         )

      big4 <- aggregate (list('n4'=df3$RESULT)
                        ,list ('UID'=df3$UID, 'PARAMETER'=df3$PARAMETER)
                        ,count
                        )
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

      ss4m <- rename(ss4m, 'pct', 'RESULT')

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
  

metsSubstrateCharacterizationTest <- function ()
# test of metsSubstrateCharacterization function
# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none
{
  # Create test datasets with both protocols
  df1 <- metsSubstrateCharacterization.wadeableSubstrateData()
  df2 <- metsSubstrateCharacterization.boatableThalwegSubstrateData()
  df3 <- metsSubstrateCharacterization.boatableLittoralSubstrateData()
  expected <- metsSubstrateCharacterization.expectedResults()

#  intermediateMessage ('fetch_test_data.1', loc='start')

  # Test calculations using data with both protocols
  # calculate the metrics
  rr <- metsSubstrateCharacterization.1 (df1, df2, df3)
  checkTrue(is.data.frame(rr)
           ,paste("Error: metsSubstrateCharacterization returns error message:"
                 ,rr
                 )
           )

#  intermediateMessage ( ' Done.', loc='end')


  tt <- merge(expected, rr, by=c('UID','METRIC'),all=TRUE
             ,suffixes=c('.expected', '.actual')
             )
 
#  zz <- subset(tt, is.na(RESULT.x) | is.na(RESULT.y))
#
#  xx <- sort(unique(rr$METRIC))
#  yy <- sort(unique(expected$METRIC))
#
#  xxOut <- subset(xx, !(xx %in% metrics$METRIC))
#  yyOut <- subset(xx, !(yy %in% metrics$METRIC))
 
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  errs <- subset(tt
                ,abs(diff) > 10^-3 |
                 is.na(RESULT.expected) != is.na(RESULT.actual)
                )
  checkEquals(0, nrow(errs)
             ,"Error: substrateCharacterization metrics are broken"
             )


  # Test calculations with just wadeable data
  rr <- metsSubstrateCharacterization.1 (df1, subset(df2, FALSE), subset(df3, FALSE))
  checkTrue(is.data.frame(rr)
           ,paste("Error: metsSubstrateCharacterization (wadeable only) returns error message:"
                 ,rr
                 )
           )
  expectedWadeable <- subset(expected, UID %in% unique(df1$UID))
  
  tt <- merge(expectedWadeable, rr, by=c('UID','METRIC'),all=TRUE
             ,suffixes=c('.expected', '.actual')
             )
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  errs <- subset(tt
                ,abs(diff) > 10^-3 |
                 is.na(RESULT.expected) != is.na(RESULT.actual)
                )
  checkEquals(0, nrow(errs)
             ,"Error: substrateCharacterization metrics (wadeable only) are broken"
             )


  # Test calculations with just boatable data
  rr <- metsSubstrateCharacterization.1 (subset(df1, FALSE), df2, df3)
  checkTrue(is.data.frame(rr)
           ,paste("Error: metsSubstrateCharacterization (wadeable only) returns error message:"
                 ,rr
                 )
           )
  expectedBoatable <- subset(expected, UID %in% c(unique(df2$UID),unique(df3$UID)))

  tt <- merge(expectedBoatable, rr, by=c('UID','METRIC'),all=TRUE
             ,suffixes=c('.expected', '.actual')
             )
  tt$RESULT.expected <- as.numeric (tt$RESULT.expected)
  tt$RESULT.actual <- as.numeric (tt$RESULT.actual)
  tt$diff <- tt$RESULT.expected - tt$RESULT.actual
  errs <- subset(tt
                ,abs(diff) > 10^-3 |
                 is.na(RESULT.expected) != is.na(RESULT.actual)
                )
  checkEquals(0, nrow(errs)
             ,"Error: substrateCharacterization metrics (boatable only) are broken"
             )
}
  
  
metsSubstrateCharacterization.wadeableSubstrateData <- function()
# Creates dataframe of wadeable protocol substrate data for unit test
# Use this SAS code to grab and format values from another site, if needed:
#
#   libname wemap 's:/wemap/data';
#
#   data sub; set wemap.sub_bank; where site_id='WORP99-0885'; run;
#   data _NULL_; set sub;
#     put '                         '  transect
#         '          ' transdir
#         '         ' size_cls
#         '    SIZE_CLS     11';
#   run;
#
#   data meso; set wemap.mesosub; where site_id='WORP99-0885'; run;
#   proc transpose data=meso out=tmeso;
#     by transect;
#     var XSUB_LFT XSUBLCTR XSUB_CTR XSUBRCTR XSUB_RGT;
#   run;
#   data tmeso; set tmeso;
#     if (_name_='XSUB_LFT') then transdir='LF';
#     if (_name_='XSUBLCTR') then transdir='LC';
#     if (_name_='XSUB_CTR') then transdir='CT';
#     if (_name_='XSUBRCTR') then transdir='RC';
#     if (_name_='XSUB_RGT') then transdir='RT';
#   run;
#   options nocenter;
#   data _NULL_; set tmeso;
#     put '                         '  transect
#         '          ' transdir
#         '        ' col1
#         '     XSIZE_CLS    11';
#   run;
#
#   data mets; set wemap.phabmet;   where site_id='WORP99-0885'; run;
#   proc transpose data=mets(keep=d16 d50 d84 dgm lsub_d16 lsub_d25 lsub_d50
#                                 lsub_d75 lsub_d84 lsub_dmm lsub_dmm_nor
#                                 lsub_iqr lsub2d16 lsub2d16inor lsub2d25
#                                 lsub2d50 lsub2d50inor lsub2d75 lsub2d84
#                                 lsub2d84inor lsub2dmm lsub2dmm_nor lsub2iqr
#                                 lsubd_sd lsubd_sd_nor lsubd2sd lsubd2sd_nor n
#                                 n_nor pct_bdrk pct_bigr pct_bl pct_cb pct_fn
#                                 pct_gc pct_gf pct_hp pct_om pct_org pct_ot
#                                 pct_rc pct_rr pct_rs pct_sa pct_safn pct_sb
#                                 pct_sfgf pct_wd pct_xb sub_dmm_nor sub2dmm_nor
#                                 subd_sd_nor subd2sd_nor
#                           )
#                  out=tmets;
#   run;
#   data _NULL_; set tmets;
#     put '                   11  ' _name_ '        ' col1;
#   run;
#
{

  bob <- textConnection("TRANSECT TRANSDIR  RESULT      PARAMETER   UID
                         A           CT         RS      SIZE_CLS    FAKE_ONLY_BEDROCK
                         A           LC         RS      SIZE_CLS    FAKE_ONLY_BEDROCK
                         A           LF         RS      SIZE_CLS    FAKE_ONLY_BEDROCK
                         A           RC         RS      SIZE_CLS    FAKE_ONLY_BEDROCK
                         A           RT         RS      SIZE_CLS    FAKE_ONLY_BEDROCK
                         A           CT         RS     XSIZE_CLS    FAKE_ONLY_BEDROCK
                         A           LC         RS     XSIZE_CLS    FAKE_ONLY_BEDROCK
                         A           LF         RS     XSIZE_CLS    FAKE_ONLY_BEDROCK
                         A           RC         RS     XSIZE_CLS    FAKE_ONLY_BEDROCK
                         A           RT         RS     XSIZE_CLS    FAKE_ONLY_BEDROCK
                         A           CT         OT      SIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           LC         OT      SIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           LF         OT      SIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           RC         OT      SIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           RT         OT      SIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           CT         OT     XSIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           LC         OT     XSIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           LF         OT     XSIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           RC         OT     XSIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           RT         OT     XSIZE_CLS    FAKE_ONLY_NONMINERAL
                         A           CT         SA      SIZE_CLS      1
                         A           LC         GC      SIZE_CLS      1
                         A           LF         FN      SIZE_CLS      1
                         A           RC         RR      SIZE_CLS      1
                         A           RT         FN      SIZE_CLS      1
                         B           CT         GF      SIZE_CLS      1
                         B           LC         GC      SIZE_CLS      1
                         B           LF         SA      SIZE_CLS      1
                         B           RC         GC      SIZE_CLS      1
                         B           RT         FN      SIZE_CLS      1
                         C           CT         GC      SIZE_CLS      1
                         C           LC         SA      SIZE_CLS      1
                         C           LF         FN      SIZE_CLS      1
                         C           RC         XB      SIZE_CLS      1
                         C           RT         XB      SIZE_CLS      1
                         D           CT         SB      SIZE_CLS      1
                         D           LC         GF      SIZE_CLS      1
                         D           LF         GC      SIZE_CLS      1
                         D           RC         CB      SIZE_CLS      1
                         D           RT         SB      SIZE_CLS      1
                         E           CT         GC      SIZE_CLS      1
                         E           LC         SA      SIZE_CLS      1
                         E           LF         SA      SIZE_CLS      1
                         E           RC         SB      SIZE_CLS      1
                         E           RT         SA      SIZE_CLS      1
                         F           CT         CB      SIZE_CLS      1
                         F           LC         SA      SIZE_CLS      1
                         F           LF         SA      SIZE_CLS      1
                         F           RC         GF      SIZE_CLS      1
                         F           RT         GC      SIZE_CLS      1
                         G           CT         GC      SIZE_CLS      1
                         G           LC         SA      SIZE_CLS      1
                         G           LF         GC      SIZE_CLS      1
                         G           RC         GC      SIZE_CLS      1
                         G           RT         GC      SIZE_CLS      1
                         H           CT         GC      SIZE_CLS      1
                         H           LC         CB      SIZE_CLS      1
                         H           LF         RR      SIZE_CLS      1
                         H           RC         GC      SIZE_CLS      1
                         H           RT         SA      SIZE_CLS      1
                         I           CT         SB      SIZE_CLS      1
                         I           LC         SB      SIZE_CLS      1
                         I           LF         GC      SIZE_CLS      1
                         I           RC         SB      SIZE_CLS      1
                         I           RT         SA      SIZE_CLS      1
                         J           CT         SA      SIZE_CLS      1
                         J           LC         SA      SIZE_CLS      1
                         J           LF         FN      SIZE_CLS      1
                         J           RC         GC      SIZE_CLS      1
                         J           RT         CB      SIZE_CLS      1
                         K           CT         GC      SIZE_CLS      1
                         K           LC         GC      SIZE_CLS      1
                         K           LF         GC      SIZE_CLS      1
                         K           RC         GF      SIZE_CLS      1
                         K           RT         SA      SIZE_CLS      1
                         A           CT         CB      SIZE_CLS      2
                         A           LC         CB      SIZE_CLS      2
                         A           LF         OT      SIZE_CLS      2
                         A           RC         GF      SIZE_CLS      2
                         A           RT         SB      SIZE_CLS      2
                         B           CT         GC      SIZE_CLS      2
                         B           LC         CB      SIZE_CLS      2
                         B           LF         GF      SIZE_CLS      2
                         B           RC         GC      SIZE_CLS      2
                         B           RT         SB      SIZE_CLS      2
                         C           CT         GC      SIZE_CLS      2
                         C           LC         GC      SIZE_CLS      2
                         C           LF         SA      SIZE_CLS      2
                         C           RC         GF      SIZE_CLS      2
                         C           RT         GF      SIZE_CLS      2
                         D           CT         SB      SIZE_CLS      2
                         D           LC         XB      SIZE_CLS      2
                         D           LF         GF      SIZE_CLS      2
                         D           RC         XB      SIZE_CLS      2
                         D           RT         CB      SIZE_CLS      2
                         E           CT         XB      SIZE_CLS      2
                         E           LC         GC      SIZE_CLS      2
                         E           LF         CB      SIZE_CLS      2
                         E           RC         SB      SIZE_CLS      2
                         E           RT         CB      SIZE_CLS      2
                         F           CT         CB      SIZE_CLS      2
                         F           LC         GC      SIZE_CLS      2
                         F           LF         GF      SIZE_CLS      2
                         F           RC         CB      SIZE_CLS      2
                         F           RT         CB      SIZE_CLS      2
                         G           CT         GC      SIZE_CLS      2
                         G           LC         GC      SIZE_CLS      2
                         G           LF         GC      SIZE_CLS      2
                         G           RC         GC      SIZE_CLS      2
                         G           RT         GC      SIZE_CLS      2
                         H           CT         GF      SIZE_CLS      2
                         H           LC         SB      SIZE_CLS      2
                         H           LF         RR      SIZE_CLS      2
                         H           RC         FN      SIZE_CLS      2
                         H           RT         SA      SIZE_CLS      2
                         I           CT         XB      SIZE_CLS      2
                         I           LC         SB      SIZE_CLS      2
                         I           LF         GC      SIZE_CLS      2
                         I           RC         CB      SIZE_CLS      2
                         I           RT         SB      SIZE_CLS      2
                         J           CT         GC      SIZE_CLS      2
                         J           LC         CB      SIZE_CLS      2
                         J           LF         FN      SIZE_CLS      2
                         J           RC         CB      SIZE_CLS      2
                         J           RT         GC      SIZE_CLS      2
                         K           CT         GC      SIZE_CLS      2
                         K           LC         GC      SIZE_CLS      2
                         K           LF         GC      SIZE_CLS      2
                         K           RC         CB      SIZE_CLS      2
                         K           RT         SB      SIZE_CLS      2
                         A           CT         CB      SIZE_CLS      3
                         A           LC         CB      SIZE_CLS      3
                         A           LF         GC      SIZE_CLS      3
                         A           RC         GC      SIZE_CLS      3
                         A           RT         SA      SIZE_CLS      3
                         B           CT         CB      SIZE_CLS      3
                         B           LC         SB      SIZE_CLS      3
                         B           LF         CB      SIZE_CLS      3
                         B           RC         SB      SIZE_CLS      3
                         B           RT         CB      SIZE_CLS      3
                         C           CT         CB      SIZE_CLS      3
                         C           LC         WD      SIZE_CLS      3
                         C           LF         WD      SIZE_CLS      3
                         C           RC         GC      SIZE_CLS      3
                         C           RT         SA      SIZE_CLS      3
                         D           CT         CB      SIZE_CLS      3
                         D           LC         GC      SIZE_CLS      3
                         D           LF         SA      SIZE_CLS      3
                         D           RC         CB      SIZE_CLS      3
                         D           RT         WD      SIZE_CLS      3
                         E           CT         SB      SIZE_CLS      3
                         E           LC         CB      SIZE_CLS      3
                         E           LF         CB      SIZE_CLS      3
                         E           RC         GC      SIZE_CLS      3
                         E           RT         GC      SIZE_CLS      3
                         F           CT         CB      SIZE_CLS      3
                         F           LC         SB      SIZE_CLS      3
                         F           LF         FN      SIZE_CLS      3
                         F           RC         CB      SIZE_CLS      3
                         F           RT         FN      SIZE_CLS      3
                         G           CT         SB      SIZE_CLS      3
                         G           LC         GC      SIZE_CLS      3
                         G           LF         FN      SIZE_CLS      3
                         G           RC         CB      SIZE_CLS      3
                         G           RT         GC      SIZE_CLS      3
                         H           CT         GC      SIZE_CLS      3
                         H           LC         SA      SIZE_CLS      3
                         H           LF         FN      SIZE_CLS      3
                         H           RC         GC      SIZE_CLS      3
                         H           RT         GC      SIZE_CLS      3
                         I           CT         SB      SIZE_CLS      3
                         I           LC         SB      SIZE_CLS      3
                         I           LF         FN      SIZE_CLS      3
                         I           RC         CB      SIZE_CLS      3
                         I           RT         GC      SIZE_CLS      3
                         J           CT         CB      SIZE_CLS      3
                         J           LC         GC      SIZE_CLS      3
                         J           LF         SA      SIZE_CLS      3
                         J           RC         CB      SIZE_CLS      3
                         J           RT         FN      SIZE_CLS      3
                         K           CT         CB      SIZE_CLS      3
                         K           LC         CB      SIZE_CLS      3
                         K           LF         GC      SIZE_CLS      3
                         K           RC         CB      SIZE_CLS      3
                         K           RT         GC      SIZE_CLS      3
                         XC          CT         GC      SIZE_CLS      3
                         XC          LC         GC      SIZE_CLS      3
                         XC          LF         FN      SIZE_CLS      3
                         XC          RC         CB      SIZE_CLS      3
                         XC          RT         SA      SIZE_CLS      3
                         XK          CT         CB      SIZE_CLS      3
                         XK          LC         FN      SIZE_CLS      3
                         XK          LF         FN      SIZE_CLS      3
                         XK          RC         CB      SIZE_CLS      3
                         XK          RT         GC      SIZE_CLS      3
                         A           CT         CB      SIZE_CLS      4
                         A           LC         GC      SIZE_CLS      4
                         A           LF         GC      SIZE_CLS      4
                         A           RC         GC      SIZE_CLS      4
                         A           RT         FN      SIZE_CLS      4
                         B           CT         CB      SIZE_CLS      4
                         B           LC         GC      SIZE_CLS      4
                         B           LF         GC      SIZE_CLS      4
                         B           RC         CB      SIZE_CLS      4
                         B           RT         GC      SIZE_CLS      4
                         C           CT         CB      SIZE_CLS      4
                         C           RC         GC      SIZE_CLS      4
                         C           RT         CB      SIZE_CLS      4
                         D           CT         CB      SIZE_CLS      4
                         D           LC         CB      SIZE_CLS      4
                         D           LF         GC      SIZE_CLS      4
                         D           RC         CB      SIZE_CLS      4
                         D           RT         WD      SIZE_CLS      4
                         E           CT         SB      SIZE_CLS      4
                         E           LC         CB      SIZE_CLS      4
                         E           LF         FN      SIZE_CLS      4
                         E           RC         GC      SIZE_CLS      4
                         E           RT         GF      SIZE_CLS      4
                         F           CT         CB      SIZE_CLS      4
                         F           LC         CB      SIZE_CLS      4
                         F           LF         GC      SIZE_CLS      4
                         F           RC         CB      SIZE_CLS      4
                         F           RT         GC      SIZE_CLS      4
                         G           CT         CB      SIZE_CLS      4
                         G           LC         SB      SIZE_CLS      4
                         G           LF         CB      SIZE_CLS      4
                         G           RC         SB      SIZE_CLS      4
                         G           RT         GC      SIZE_CLS      4
                         H           CT         GC      SIZE_CLS      4
                         H           LC         GC      SIZE_CLS      4
                         H           LF         FN      SIZE_CLS      4
                         H           RC         GC      SIZE_CLS      4
                         H           RT         FN      SIZE_CLS      4
                         I           CT         CB      SIZE_CLS      4
                         I           LC         SB      SIZE_CLS      4
                         I           LF         FN      SIZE_CLS      4
                         I           RC         CB      SIZE_CLS      4
                         I           RT         GC      SIZE_CLS      4
                         J           CT         CB      SIZE_CLS      4
                         J           LC         GC      SIZE_CLS      4
                         J           LF         GC      SIZE_CLS      4
                         J           RC         CB      SIZE_CLS      4
                         J           RT         GC      SIZE_CLS      4
                         K           CT         CB      SIZE_CLS      4
                         K           LC         CB      SIZE_CLS      4
                         K           LF         GC      SIZE_CLS      4
                         K           RC         CB      SIZE_CLS      4
                         K           RT         GC      SIZE_CLS      4
                         XC          CT         CB      SIZE_CLS      4
                         XC          LC         GC      SIZE_CLS      4
                         XC          LF         CB      SIZE_CLS      4
                         XC          RC         GC      SIZE_CLS      4
                         XC          RT         FN      SIZE_CLS      4
                         XK          CT         CB      SIZE_CLS      4
                         XK          LC         CB      SIZE_CLS      4
                         XK          LF         GC      SIZE_CLS      4
                         XK          RC         SB      SIZE_CLS      4
                         XK          RT         GF      SIZE_CLS      4
                         A           CT         GC      SIZE_CLS      5
                         A           LC         GF      SIZE_CLS      5
                         A           LF         FN      SIZE_CLS      5
                         A           RC         GC      SIZE_CLS      5
                         A           RT         GC      SIZE_CLS      5
                         B           CT         GC      SIZE_CLS      5
                         B           LC         GF      SIZE_CLS      5
                         B           LF         FN      SIZE_CLS      5
                         B           RC         GF      SIZE_CLS      5
                         B           RT         GF      SIZE_CLS      5
                         C           CT         GF      SIZE_CLS      5
                         C           LC         GF      SIZE_CLS      5
                         C           LF         FN      SIZE_CLS      5
                         C           RC         GF      SIZE_CLS      5
                         C           RT         GF      SIZE_CLS      5
                         D           CT         GF      SIZE_CLS      5
                         D           LC         GF      SIZE_CLS      5
                         D           LF         GF      SIZE_CLS      5
                         D           RC         GF      SIZE_CLS      5
                         D           RT         FN      SIZE_CLS      5
                         E           CT         GF      SIZE_CLS      5
                         E           LC         GF      SIZE_CLS      5
                         E           LF         FN      SIZE_CLS      5
                         E           RC         FN      SIZE_CLS      5
                         E           RT         FN      SIZE_CLS      5
                         F           CT         SA      SIZE_CLS      5
                         F           LC         GF      SIZE_CLS      5
                         F           LF         FN      SIZE_CLS      5
                         F           RC         GF      SIZE_CLS      5
                         F           RT         GC      SIZE_CLS      5
                         G           CT         GF      SIZE_CLS      5
                         G           LC         GF      SIZE_CLS      5
                         G           LF         GC      SIZE_CLS      5
                         G           RC         GF      SIZE_CLS      5
                         G           RT         FN      SIZE_CLS      5
                         H           CT         GC      SIZE_CLS      5
                         H           LC         GF      SIZE_CLS      5
                         H           LF         GF      SIZE_CLS      5
                         H           RC         GF      SIZE_CLS      5
                         H           RT         FN      SIZE_CLS      5
                         I           CT         GF      SIZE_CLS      5
                         I           LC         GC      SIZE_CLS      5
                         I           LF         GF      SIZE_CLS      5
                         I           RC         GF      SIZE_CLS      5
                         I           RT         FN      SIZE_CLS      5
                         J           CT         GC      SIZE_CLS      5
                         J           LC         GF      SIZE_CLS      5
                         J           LF         CB      SIZE_CLS      5
                         J           RC         GF      SIZE_CLS      5
                         J           RT         SA      SIZE_CLS      5
                         K           CT         GF      SIZE_CLS      5
                         K           LC         GF      SIZE_CLS      5
                         K           LF         FN      SIZE_CLS      5
                         K           RC         GF      SIZE_CLS      5
                         K           RT         SA      SIZE_CLS      5
                         A           CT         CB      SIZE_CLS      6
                         A           LC         GC      SIZE_CLS      6
                         A           LF         CB      SIZE_CLS      6
                         A           RC         GC      SIZE_CLS      6
                         A           RT         GF      SIZE_CLS      6
                         B           CT         GF      SIZE_CLS      6
                         B           LC         GF      SIZE_CLS      6
                         B           LF         SA      SIZE_CLS      6
                         B           RC         GF      SIZE_CLS      6
                         B           RT         GF      SIZE_CLS      6
                         C           CT         GC      SIZE_CLS      6
                         C           LC         CB      SIZE_CLS      6
                         C           LF         SA      SIZE_CLS      6
                         C           RC         CB      SIZE_CLS      6
                         C           RT         SA      SIZE_CLS      6
                         D           CT         SA      SIZE_CLS      6
                         D           LC         GF      SIZE_CLS      6
                         D           LF         SA      SIZE_CLS      6
                         D           RC         CB      SIZE_CLS      6
                         D           RT         GF      SIZE_CLS      6
                         E           CT         GF      SIZE_CLS      6
                         E           LC         SA      SIZE_CLS      6
                         E           LF         SA      SIZE_CLS      6
                         E           RC         WD      SIZE_CLS      6
                         E           RT         CB      SIZE_CLS      6
                         F           CT         XB      SIZE_CLS      6
                         F           LC         WD      SIZE_CLS      6
                         F           LF         SA      SIZE_CLS      6
                         F           RC         SA      SIZE_CLS      6
                         F           RT         SA      SIZE_CLS      6
                         G           CT         GF      SIZE_CLS      6
                         G           LC         GF      SIZE_CLS      6
                         G           LF         SA      SIZE_CLS      6
                         G           RC         GF      SIZE_CLS      6
                         G           RT         SA      SIZE_CLS      6
                         H           CT         GF      SIZE_CLS      6
                         H           LC         GF      SIZE_CLS      6
                         H           LF         SA      SIZE_CLS      6
                         H           RC         SA      SIZE_CLS      6
                         H           RT         SA      SIZE_CLS      6
                         I           CT         GF      SIZE_CLS      6
                         I           LC         GF      SIZE_CLS      6
                         I           LF         SA      SIZE_CLS      6
                         I           RC         GF      SIZE_CLS      6
                         I           RT         WD      SIZE_CLS      6
                         J           CT         GF      SIZE_CLS      6
                         J           LC         GC      SIZE_CLS      6
                         J           LF         GF      SIZE_CLS      6
                         J           RC         WD      SIZE_CLS      6
                         J           RT         SA      SIZE_CLS      6
                         K           CT         GF      SIZE_CLS      6
                         K           LC         SA      SIZE_CLS      6
                         K           LF         SA      SIZE_CLS      6
                         K           RC         GF      SIZE_CLS      6
                         K           RT         GF      SIZE_CLS      6
                         A           CT          GC     SIZE_CLS     10
                         A           LC          GF     SIZE_CLS     10
                         A           LF          CB     SIZE_CLS     10
                         A           RC          CB     SIZE_CLS     10
                         A           RT          CB     SIZE_CLS     10
                         B           CT          GF     SIZE_CLS     10
                         B           LC          GC     SIZE_CLS     10
                         B           LF          CB     SIZE_CLS     10
                         B           RC          GC     SIZE_CLS     10
                         B           RT          SA     SIZE_CLS     10
                         C           CT          GC     SIZE_CLS     10
                         C           LC          GC     SIZE_CLS     10
                         C           LF          CB     SIZE_CLS     10
                         C           RC          GC     SIZE_CLS     10
                         C           RT          CB     SIZE_CLS     10
                         D           CT          GC     SIZE_CLS     10
                         D           LC          GF     SIZE_CLS     10
                         D           LF          GF     SIZE_CLS     10
                         D           RC          SB     SIZE_CLS     10
                         D           RT          GC     SIZE_CLS     10
                         E           CT          SB     SIZE_CLS     10
                         E           LC          SA     SIZE_CLS     10
                         E           LF          GC     SIZE_CLS     10
                         E           RC          XB     SIZE_CLS     10
                         E           RT          XB     SIZE_CLS     10
                         F           CT          GC     SIZE_CLS     10
                         F           LC          GC     SIZE_CLS     10
                         F           LF          CB     SIZE_CLS     10
                         F           RC          GC     SIZE_CLS     10
                         F           RT          CB     SIZE_CLS     10
                         G           CT          GC     SIZE_CLS     10
                         G           LC          XB     SIZE_CLS     10
                         G           LF          XB     SIZE_CLS     10
                         G           RC          GC     SIZE_CLS     10
                         G           RT          CB     SIZE_CLS     10
                         H           CT          CB     SIZE_CLS     10
                         H           LC          GC     SIZE_CLS     10
                         H           LF          CB     SIZE_CLS     10
                         H           RC          CB     SIZE_CLS     10
                         H           RT          CB     SIZE_CLS     10
                         I           CT          SB     SIZE_CLS     10
                         I           LC          GC     SIZE_CLS     10
                         I           LF          SA     SIZE_CLS     10
                         I           RC          SB     SIZE_CLS     10
                         I           RT          XB     SIZE_CLS     10
                         J           CT          GC     SIZE_CLS     10
                         J           LC          CB     SIZE_CLS     10
                         J           LF          SA     SIZE_CLS     10
                         J           RC          GC     SIZE_CLS     10
                         J           RT          WD     SIZE_CLS     10
                         K           CT          CB     SIZE_CLS     10
                         K           LC          SA     SIZE_CLS     10
                         K           LF          CB     SIZE_CLS     10
                         K           RC          CB     SIZE_CLS     10
                         K           RT          HP     SIZE_CLS     10
                         A           CT          RS     SIZE_CLS     11
                         A           LC          RS     SIZE_CLS     11
                         A           LF          SB     SIZE_CLS     11
                         A           RC          SA     SIZE_CLS     11
                         A           RT          SA     SIZE_CLS     11
                         B           CT          GC     SIZE_CLS     11
                         B           LC          SA     SIZE_CLS     11
                         B           LF          WD     SIZE_CLS     11
                         B           RC          GC     SIZE_CLS     11
                         B           RT          GF     SIZE_CLS     11
                         C           CT          RS     SIZE_CLS     11
                         C           LC          GC     SIZE_CLS     11
                         C           LF          FN     SIZE_CLS     11
                         C           RC          GC     SIZE_CLS     11
                         C           RT          FN     SIZE_CLS     11
                         D           CT          CB     SIZE_CLS     11
                         D           LC          CB     SIZE_CLS     11
                         D           LF          FN     SIZE_CLS     11
                         D           RC          CB     SIZE_CLS     11
                         D           RT          FN     SIZE_CLS     11
                         E           CT          GF     SIZE_CLS     11
                         E           LC          GF     SIZE_CLS     11
                         E           LF          FN     SIZE_CLS     11
                         E           RC          FN     SIZE_CLS     11
                         E           RT          FN     SIZE_CLS     11
                         F           CT          SA     SIZE_CLS     11
                         F           LC          GC     SIZE_CLS     11
                         F           LF          FN     SIZE_CLS     11
                         F           RC          SA     SIZE_CLS     11
                         F           RT          FN     SIZE_CLS     11
                         G           CT          GC     SIZE_CLS     11
                         G           LC          GF     SIZE_CLS     11
                         G           LF          FN     SIZE_CLS     11
                         G           RC          GC     SIZE_CLS     11
                         G           RT          WD     SIZE_CLS     11
                         H           CT          RS     SIZE_CLS     11
                         H           LC          GC     SIZE_CLS     11
                         H           LF          WD     SIZE_CLS     11
                         H           RC          RS     SIZE_CLS     11
                         H           RT          FN     SIZE_CLS     11
                         I           CT          GC     SIZE_CLS     11
                         I           LC          GC     SIZE_CLS     11
                         I           LF          FN     SIZE_CLS     11
                         I           RC          FN     SIZE_CLS     11
                         I           RT          FN     SIZE_CLS     11
                         J           CT          GC     SIZE_CLS     11
                         J           LC          GC     SIZE_CLS     11
                         J           LF          GC     SIZE_CLS     11
                         J           RC          GC     SIZE_CLS     11
                         J           RT          GC     SIZE_CLS     11
                         K           CT          SB     SIZE_CLS     11
                         K           LC          SA     SIZE_CLS     11
                         K           LF          FN     SIZE_CLS     11
                         K           RC          RS     SIZE_CLS     11
                         K           RT          WD     SIZE_CLS     11
                         A           LF         FN      XSIZE_CLS     1
                         B           LF         XB      XSIZE_CLS     1
                         C           LF         SA      XSIZE_CLS     1
                         D           LF         GC      XSIZE_CLS     1
                         E           LF         GC      XSIZE_CLS     1
                         F           LF         CB      XSIZE_CLS     1
                         G           LF         SA      XSIZE_CLS     1
                         H           LF         RR      XSIZE_CLS     1
                         I           LF         GC      XSIZE_CLS     1
                         J           LF         FN      XSIZE_CLS     1
                         A           LF         FN      XSIZE_CLS     2
                         B           LF         GC      XSIZE_CLS     2
                         C           LF         SA      XSIZE_CLS     2
                         D           LF         GF      XSIZE_CLS     2
                         E           LF         CB      XSIZE_CLS     2
                         F           LF         GF      XSIZE_CLS     2
                         G           LF         FN      XSIZE_CLS     2
                         H           LF         RR      XSIZE_CLS     2
                         I           LF         FN      XSIZE_CLS     2
                         J           LF         SB      XSIZE_CLS     2
                         A           LF         SB      XSIZE_CLS     3
                         B           LF         GC      XSIZE_CLS     3
                         C           LF         GC      XSIZE_CLS     3
                         D           LF         WD      XSIZE_CLS     3
                         E           LF         SA      XSIZE_CLS     3
                         F           LF         FN      XSIZE_CLS     3
                         G           LF         SA      XSIZE_CLS     3
                         H           LF         FN      XSIZE_CLS     3
                         I           LF         GF      XSIZE_CLS     3
                         J           LF         SA      XSIZE_CLS     3
                         A           LF         CB      XSIZE_CLS     4
                         B           LF         GC      XSIZE_CLS     4
                         C           LF         GC      XSIZE_CLS     4
                         D           LF         FN      XSIZE_CLS     4
                         E           LF         GC      XSIZE_CLS     4
                         F           LF         GC      XSIZE_CLS     4
                         G           LF         FN      XSIZE_CLS     4
                         H           LF         FN      XSIZE_CLS     4
                         I           LF         SA      XSIZE_CLS     4
                         J           LF         GC      XSIZE_CLS     4
                         A           LF         CB      XSIZE_CLS     5
                         B           LF         FN      XSIZE_CLS     5
                         C           LF         GF      XSIZE_CLS     5
                         D           LF         GF      XSIZE_CLS     5
                         E           LF         FN      XSIZE_CLS     5
                         F           LF         FN      XSIZE_CLS     5
                         G           LF         FN      XSIZE_CLS     5
                         H           LF         SA      XSIZE_CLS     5
                         I           LF         SA      XSIZE_CLS     5
                         J           LF         GF      XSIZE_CLS     5
                         A           LF         GC      XSIZE_CLS     6
                         B           LF         CB      XSIZE_CLS     6
                         C           LF         GF      XSIZE_CLS     6
                         D           LF         GC      XSIZE_CLS     6
                         E           LF         SA      XSIZE_CLS     6
                         F           LF         SA      XSIZE_CLS     6
                         G           LF         GF      XSIZE_CLS     6
                         H           LF         RS      XSIZE_CLS     6
                         I           LF         CB      XSIZE_CLS     6
                         J           LF         GF      XSIZE_CLS     6
                         A           LC         GC      XSIZE_CLS     1
                         B           LC         GC      XSIZE_CLS     1
                         C           LC         GC      XSIZE_CLS     1
                         D           LC         CB      XSIZE_CLS     1
                         E           LC         SB      XSIZE_CLS     1
                         F           LC         CB      XSIZE_CLS     1
                         G           LC         GF      XSIZE_CLS     1
                         H           LC         RR      XSIZE_CLS     1
                         I           LC         GC      XSIZE_CLS     1
                         J           LC         FN      XSIZE_CLS     1
                         A           LC         GF      XSIZE_CLS     2
                         B           LC         FN      XSIZE_CLS     2
                         C           LC         SB      XSIZE_CLS     2
                         D           LC         GC      XSIZE_CLS     2
                         E           LC         CB      XSIZE_CLS     2
                         F           LC         GC      XSIZE_CLS     2
                         G           LC         FN      XSIZE_CLS     2
                         H           LC         CB      XSIZE_CLS     2
                         I           LC         FN      XSIZE_CLS     2
                         J           LC         GC      XSIZE_CLS     2
                         A           LC         CB      XSIZE_CLS     3
                         B           LC         CB      XSIZE_CLS     3
                         C           LC         CB      XSIZE_CLS     3
                         D           LC         CB      XSIZE_CLS     3
                         E           LC         CB      XSIZE_CLS     3
                         F           LC         GC      XSIZE_CLS     3
                         G           LC         CB      XSIZE_CLS     3
                         H           LC         CB      XSIZE_CLS     3
                         I           LC         CB      XSIZE_CLS     3
                         J           LC         CB      XSIZE_CLS     3
                         A           LC         SA      XSIZE_CLS     4
                         B           LC         CB      XSIZE_CLS     4
                         C           LC         CB      XSIZE_CLS     4
                         D           LC         GC      XSIZE_CLS     4
                         E           LC         CB      XSIZE_CLS     4
                         F           LC         GC      XSIZE_CLS     4
                         G           LC         CB      XSIZE_CLS     4
                         H           LC         CB      XSIZE_CLS     4
                         I           LC         CB      XSIZE_CLS     4
                         J           LC         GC      XSIZE_CLS     4
                         A           LC         GF      XSIZE_CLS     5
                         B           LC         GF      XSIZE_CLS     5
                         C           LC         GF      XSIZE_CLS     5
                         D           LC         GF      XSIZE_CLS     5
                         E           LC         SA      XSIZE_CLS     5
                         F           LC         SA      XSIZE_CLS     5
                         G           LC         GF      XSIZE_CLS     5
                         H           LC         SA      XSIZE_CLS     5
                         I           LC         GF      XSIZE_CLS     5
                         J           LC         WD      XSIZE_CLS     5
                         A           LC         GC      XSIZE_CLS     6
                         B           LC         CB      XSIZE_CLS     6
                         C           LC         CB      XSIZE_CLS     6
                         D           LC         CB      XSIZE_CLS     6
                         E           LC         GC      XSIZE_CLS     6
                         F           LC         CB      XSIZE_CLS     6
                         G           LC         GC      XSIZE_CLS     6
                         H           LC         RS      XSIZE_CLS     6
                         I           LC         GF      XSIZE_CLS     6
                         J           LC         GC      XSIZE_CLS     6
                         A           CT         FN      XSIZE_CLS     1
                         B           CT         GC      XSIZE_CLS     1
                         C           CT         CB      XSIZE_CLS     1
                         D           CT         SB      XSIZE_CLS     1
                         E           CT         CB      XSIZE_CLS     1
                         F           CT         CB      XSIZE_CLS     1
                         G           CT         GC      XSIZE_CLS     1
                         H           CT         SA      XSIZE_CLS     1
                         I           CT         GC      XSIZE_CLS     1
                         J           CT         GC      XSIZE_CLS     1
                         A           CT         CB      XSIZE_CLS     2
                         B           CT         GC      XSIZE_CLS     2
                         C           CT         SB      XSIZE_CLS     2
                         D           CT         CB      XSIZE_CLS     2
                         E           CT         SB      XSIZE_CLS     2
                         F           CT         GC      XSIZE_CLS     2
                         G           CT         FN      XSIZE_CLS     2
                         H           CT         FN      XSIZE_CLS     2
                         I           CT         CB      XSIZE_CLS     2
                         J           CT         GF      XSIZE_CLS     2
                         A           CT         CB      XSIZE_CLS     3
                         B           CT         CB      XSIZE_CLS     3
                         C           CT         CB      XSIZE_CLS     3
                         D           CT         CB      XSIZE_CLS     3
                         E           CT         CB      XSIZE_CLS     3
                         F           CT         GC      XSIZE_CLS     3
                         G           CT         SB      XSIZE_CLS     3
                         H           CT         SB      XSIZE_CLS     3
                         I           CT         SB      XSIZE_CLS     3
                         J           CT         CB      XSIZE_CLS     3
                         A           CT         CB      XSIZE_CLS     4
                         B           CT         CB      XSIZE_CLS     4
                         C           CT         CB      XSIZE_CLS     4
                         D           CT         GC      XSIZE_CLS     4
                         E           CT         CB      XSIZE_CLS     4
                         F           CT         CB      XSIZE_CLS     4
                         G           CT         SB      XSIZE_CLS     4
                         H           CT         GC      XSIZE_CLS     4
                         I           CT         SB      XSIZE_CLS     4
                         J           CT         CB      XSIZE_CLS     4
                         A           CT         CB      XSIZE_CLS     5
                         B           CT         GF      XSIZE_CLS     5
                         C           CT         GF      XSIZE_CLS     5
                         D           CT         GF      XSIZE_CLS     5
                         E           CT         GF      XSIZE_CLS     5
                         F           CT         GF      XSIZE_CLS     5
                         G           CT         GF      XSIZE_CLS     5
                         H           CT         GF      XSIZE_CLS     5
                         I           CT         GC      XSIZE_CLS     5
                         J           CT         GC      XSIZE_CLS     5
                         A           CT         GF      XSIZE_CLS     6
                         B           CT         CB      XSIZE_CLS     6
                         C           CT         GC      XSIZE_CLS     6
                         D           CT         WD      XSIZE_CLS     6
                         E           CT         GC      XSIZE_CLS     6
                         F           CT         GF      XSIZE_CLS     6
                         G           CT         WD      XSIZE_CLS     6
                         H           CT         RS      XSIZE_CLS     6
                         I           CT         CB      XSIZE_CLS     6
                         J           CT         GC      XSIZE_CLS     6
                         A           RC         GC      XSIZE_CLS     1
                         B           RC         GC      XSIZE_CLS     1
                         C           RC         CB      XSIZE_CLS     1
                         D           RC         CB      XSIZE_CLS     1
                         E           RC         CB      XSIZE_CLS     1
                         F           RC         CB      XSIZE_CLS     1
                         G           RC         GF      XSIZE_CLS     1
                         H           RC         SA      XSIZE_CLS     1
                         I           RC         SA      XSIZE_CLS     1
                         J           RC         FN      XSIZE_CLS     1
                         A           RC         CB      XSIZE_CLS     2
                         B           RC         GC      XSIZE_CLS     2
                         C           RC         CB      XSIZE_CLS     2
                         D           RC         SB      XSIZE_CLS     2
                         E           RC         GC      XSIZE_CLS     2
                         F           RC         GC      XSIZE_CLS     2
                         G           RC         FN      XSIZE_CLS     2
                         H           RC         SA      XSIZE_CLS     2
                         I           RC         CB      XSIZE_CLS     2
                         J           RC         CB      XSIZE_CLS     2
                         A           RC         GC      XSIZE_CLS     3
                         B           RC         CB      XSIZE_CLS     3
                         C           RC         CB      XSIZE_CLS     3
                         D           RC         CB      XSIZE_CLS     3
                         E           RC         CB      XSIZE_CLS     3
                         F           RC         GC      XSIZE_CLS     3
                         G           RC         CB      XSIZE_CLS     3
                         H           RC         GC      XSIZE_CLS     3
                         I           RC         CB      XSIZE_CLS     3
                         J           RC         GC      XSIZE_CLS     3
                         A           RC         GC      XSIZE_CLS     4
                         B           RC         CB      XSIZE_CLS     4
                         C           RC         CB      XSIZE_CLS     4
                         D           RC         CB      XSIZE_CLS     4
                         E           RC         SB      XSIZE_CLS     4
                         F           RC         GC      XSIZE_CLS     4
                         G           RC         GC      XSIZE_CLS     4
                         H           RC         SA      XSIZE_CLS     4
                         I           RC         GC      XSIZE_CLS     4
                         J           RC         GC      XSIZE_CLS     4
                         A           RC         GC      XSIZE_CLS     5
                         B           RC         GF      XSIZE_CLS     5
                         C           RC         GF      XSIZE_CLS     5
                         D           RC         GF      XSIZE_CLS     5
                         E           RC         GF      XSIZE_CLS     5
                         F           RC         GF      XSIZE_CLS     5
                         G           RC         CB      XSIZE_CLS     5
                         H           RC         GC      XSIZE_CLS     5
                         I           RC         GC      XSIZE_CLS     5
                         J           RC         FN      XSIZE_CLS     5
                         A           RC         GC      XSIZE_CLS     6
                         B           RC         CB      XSIZE_CLS     6
                         C           RC         GF      XSIZE_CLS     6
                         D           RC         CB      XSIZE_CLS     6
                         E           RC         GC      XSIZE_CLS     6
                         F           RC         GF      XSIZE_CLS     6
                         G           RC         GF      XSIZE_CLS     6
                         H           RC         RS      XSIZE_CLS     6
                         I           RC         GC      XSIZE_CLS     6
                         J           RC         CB      XSIZE_CLS     6
                         A           RT         RR      XSIZE_CLS     1
                         B           RT         FN      XSIZE_CLS     1
                         C           RT         FN      XSIZE_CLS     1
                         D           RT         CB      XSIZE_CLS     1
                         E           RT         SB      XSIZE_CLS     1
                         F           RT         SA      XSIZE_CLS     1
                         G           RT         SA      XSIZE_CLS     1
                         H           RT         SA      XSIZE_CLS     1
                         I           RT         CB      XSIZE_CLS     1
                         J           RT         FN      XSIZE_CLS     1
                         A           RT         FN      XSIZE_CLS     2
                         B           RT         OT      XSIZE_CLS     2
                         C           RT         XB      XSIZE_CLS     2
                         D           RT         OT      XSIZE_CLS     2
                         E           RT         GF      XSIZE_CLS     2
                         F           RT         FN      XSIZE_CLS     2
                         G           RT         FN      XSIZE_CLS     2
                         H           RT         FN      XSIZE_CLS     2
                         I           RT         CB      XSIZE_CLS     2
                         J           RT         XB      XSIZE_CLS     2
                         A           RT         SA      XSIZE_CLS     3
                         B           RT         CB      XSIZE_CLS     3
                         C           RT         GC      XSIZE_CLS     3
                         D           RT         SA      XSIZE_CLS     3
                         E           RT         FN      XSIZE_CLS     3
                         F           RT         FN      XSIZE_CLS     3
                         G           RT         FN      XSIZE_CLS     3
                         H           RT         CB      XSIZE_CLS     3
                         I           RT         GC      XSIZE_CLS     3
                         J           RT         FN      XSIZE_CLS     3
                         A           RT         GF      XSIZE_CLS     4
                         B           RT         GC      XSIZE_CLS     4
                         C           RT         GC      XSIZE_CLS     4
                         D           RT         GC      XSIZE_CLS     4
                         E           RT         FN      XSIZE_CLS     4
                         F           RT         FN      XSIZE_CLS     4
                         G           RT         SA      XSIZE_CLS     4
                         H           RT         GF      XSIZE_CLS     4
                         I           RT         GC      XSIZE_CLS     4
                         J           RT         GF      XSIZE_CLS     4
                         A           RT         FN      XSIZE_CLS     5
                         B           RT         FN      XSIZE_CLS     5
                         C           RT         FN      XSIZE_CLS     5
                         D           RT         FN      XSIZE_CLS     5
                         E           RT         SA      XSIZE_CLS     5
                         F           RT         SA      XSIZE_CLS     5
                         G           RT         GC      XSIZE_CLS     5
                         H           RT         FN      XSIZE_CLS     5
                         I           RT         SA      XSIZE_CLS     5
                         J           RT         GF      XSIZE_CLS     5
                         A           RT         GF      XSIZE_CLS     6
                         B           RT         GF      XSIZE_CLS     6
                         C           RT         GF      XSIZE_CLS     6
                         D           RT         CB      XSIZE_CLS     6
                         E           RT         GF      XSIZE_CLS     6
                         F           RT         SA      XSIZE_CLS     6
                         G           RT         WD      XSIZE_CLS     6
                         H           RT         RS      XSIZE_CLS     6
                         I           RT         GF      XSIZE_CLS     6
                         J           RT         CB      XSIZE_CLS     6
                         A           LF         GC      XSIZE_CLS    10
                         B           LF         GF      XSIZE_CLS    10
                         C           LF         GC      XSIZE_CLS    10
                         D           LF         SA      XSIZE_CLS    10
                         E           LF         GC      XSIZE_CLS    10
                         F           LF         CB      XSIZE_CLS    10
                         G           LF         CB      XSIZE_CLS    10
                         H           LF         XB      XSIZE_CLS    10
                         I           LF         SA      XSIZE_CLS    10
                         J           LF         RS      XSIZE_CLS    10
                         A           LC         GC      XSIZE_CLS    10
                         B           LC         GC      XSIZE_CLS    10
                         C           LC         CB      XSIZE_CLS    10
                         D           LC         SB      XSIZE_CLS    10
                         E           LC         CB      XSIZE_CLS    10
                         F           LC         SA      XSIZE_CLS    10
                         G           LC         GC      XSIZE_CLS    10
                         H           LC         CB      XSIZE_CLS    10
                         I           LC         GC      XSIZE_CLS    10
                         J           LC         XB      XSIZE_CLS    10
                         A           CT         GF      XSIZE_CLS    10
                         B           CT         GC      XSIZE_CLS    10
                         C           CT         GC      XSIZE_CLS    10
                         D           CT         SB      XSIZE_CLS    10
                         E           CT         SB      XSIZE_CLS    10
                         F           CT         GC      XSIZE_CLS    10
                         G           CT         GC      XSIZE_CLS    10
                         H           CT         GC      XSIZE_CLS    10
                         I           CT         GF      XSIZE_CLS    10
                         J           CT         CB      XSIZE_CLS    10
                         A           RC         GF      XSIZE_CLS    10
                         B           RC         GF      XSIZE_CLS    10
                         C           RC         GC      XSIZE_CLS    10
                         D           RC         CB      XSIZE_CLS    10
                         E           RC         CB      XSIZE_CLS    10
                         F           RC         GC      XSIZE_CLS    10
                         G           RC         CB      XSIZE_CLS    10
                         H           RC         GC      XSIZE_CLS    10
                         I           RC         GF      XSIZE_CLS    10
                         J           RC         GC      XSIZE_CLS    10
                         A           RT         GC      XSIZE_CLS    10
                         B           RT         SA      XSIZE_CLS    10
                         C           RT         CB      XSIZE_CLS    10
                         D           RT         CB      XSIZE_CLS    10
                         E           RT         CB      XSIZE_CLS    10
                         F           RT         CB      XSIZE_CLS    10
                         G           RT         SA      XSIZE_CLS    10
                         H           RT         GC      XSIZE_CLS    10
                         I           RT         SA      XSIZE_CLS    10
                         J           RT         GF      XSIZE_CLS    10
                         A           LF         FN      XSIZE_CLS    11
                         A           LC         GC      XSIZE_CLS    11
                         A           CT         GC      XSIZE_CLS    11
                         A           RC         GC      XSIZE_CLS    11
                         A           RT         CB      XSIZE_CLS    11
                         B           LF         GC      XSIZE_CLS    11
                         B           LC         CB      XSIZE_CLS    11
                         B           CT         CB      XSIZE_CLS    11
                         B           RC         CB      XSIZE_CLS    11
                         B           RT         FN      XSIZE_CLS    11
                         C           LF         HP      XSIZE_CLS    11
                         C           LC         RS      XSIZE_CLS    11
                         C           CT         RS      XSIZE_CLS    11
                         C           RC         FN      XSIZE_CLS    11
                         C           RT         WD      XSIZE_CLS    11
                         D           LF         FN      XSIZE_CLS    11
                         D           LC         CB      XSIZE_CLS    11
                         D           CT         FN      XSIZE_CLS    11
                         D           RC         GC      XSIZE_CLS    11
                         D           RT         FN      XSIZE_CLS    11
                         E           LF         FN      XSIZE_CLS    11
                         E           LC         GC      XSIZE_CLS    11
                         E           CT         OT      XSIZE_CLS    11
                         E           RC         GF      XSIZE_CLS    11
                         E           RT         FN      XSIZE_CLS    11
                         F           LF         FN      XSIZE_CLS    11
                         F           LC         GC      XSIZE_CLS    11
                         F           CT         RS      XSIZE_CLS    11
                         F           RC         RS      XSIZE_CLS    11
                         F           RT         FN      XSIZE_CLS    11
                         G           LF         FN      XSIZE_CLS    11
                         G           LC         GC      XSIZE_CLS    11
                         G           CT         GC      XSIZE_CLS    11
                         G           RC         GC      XSIZE_CLS    11
                         G           RT         FN      XSIZE_CLS    11
                         H           LF         FN      XSIZE_CLS    11
                         H           LC         FN      XSIZE_CLS    11
                         H           CT         GF      XSIZE_CLS    11
                         H           RC         FN      XSIZE_CLS    11
                         H           RT         FN      XSIZE_CLS    11
                         I           LF         WD      XSIZE_CLS    11
                         I           LC         GC      XSIZE_CLS    11
                         I           CT         GC      XSIZE_CLS    11
                         I           RC         RS      XSIZE_CLS    11
                         I           RT         SA      XSIZE_CLS    11
                         J           LF         FN      XSIZE_CLS    11
                         J           LC         GC      XSIZE_CLS    11
                         J           CT         GC      XSIZE_CLS    11
                         J           RC         GC      XSIZE_CLS    11
                         J           RT         FN      XSIZE_CLS    11
                         "
                       )
  df1 <- read.table(bob, header=TRUE,stringsAsFactors=FALSE)
  close(bob)
  
  return(df1)
}


metsSubstrateCharacterization.boatableThalwegSubstrateData <- function()
# Creates dataframe of boatable protocol substrate data for unit test
{


  bob <- textConnection("TRANSECT STA_NUM RESULT UID PARAMETER
                           A            1        GR       7     SIZE_CLS
                           A            3        GR       7     SIZE_CLS
                           A            5        CB       7     SIZE_CLS
                           A            7        CB       7     SIZE_CLS
                           A            9        CB       7     SIZE_CLS
                           A           11        CB       7     SIZE_CLS
                           A           13        CB       7     SIZE_CLS
                           A           15        CB       7     SIZE_CLS
                           A           17        CB       7     SIZE_CLS
                           A           19        CB       7     SIZE_CLS
                           B            1        CB       7     SIZE_CLS
                           B            3        CB       7     SIZE_CLS
                           B            5        CB       7     SIZE_CLS
                           B            7        CB       7     SIZE_CLS
                           B            9        CB       7     SIZE_CLS
                           B           11        CB       7     SIZE_CLS
                           B           13        CB       7     SIZE_CLS
                           B           15        CB       7     SIZE_CLS
                           B           17        CB       7     SIZE_CLS
                           B           19        CB       7     SIZE_CLS
                           C            1        CB       7     SIZE_CLS
                           C            3        CB       7     SIZE_CLS
                           C            5        CB       7     SIZE_CLS
                           C            7        CB       7     SIZE_CLS
                           C            9        CB       7     SIZE_CLS
                           C           11        CB       7     SIZE_CLS
                           C           13        CB       7     SIZE_CLS
                           C           15        CB       7     SIZE_CLS
                           C           17        CB       7     SIZE_CLS
                           C           19        CB       7     SIZE_CLS
                           D            1        GR       7     SIZE_CLS
                           D            3        SA       7     SIZE_CLS
                           D            5        SA       7     SIZE_CLS
                           D            7        CB       7     SIZE_CLS
                           D            9        SA       7     SIZE_CLS
                           D           11        CB       7     SIZE_CLS
                           D           13        CB       7     SIZE_CLS
                           D           15        GR       7     SIZE_CLS
                           D           17        CB       7     SIZE_CLS
                           D           19        GR       7     SIZE_CLS
                           E            1        CB       7     SIZE_CLS
                           E            3        GR       7     SIZE_CLS
                           E            5        GR       7     SIZE_CLS
                           E            7        CB       7     SIZE_CLS
                           E            9        CB       7     SIZE_CLS
                           E           11        CB       7     SIZE_CLS
                           E           13        CB       7     SIZE_CLS
                           E           15        CB       7     SIZE_CLS
                           E           17        CB       7     SIZE_CLS
                           E           19        CB       7     SIZE_CLS
                           F            1        GR       7     SIZE_CLS
                           F            3        GR       7     SIZE_CLS
                           F            5        CB       7     SIZE_CLS
                           F            7        CB       7     SIZE_CLS
                           F            9        CB       7     SIZE_CLS
                           F           11        CB       7     SIZE_CLS
                           F           13        CB       7     SIZE_CLS
                           F           15        CB       7     SIZE_CLS
                           F           17        GR       7     SIZE_CLS
                           F           19        GR       7     SIZE_CLS
                           G            1        GR       7     SIZE_CLS
                           G            3        GR       7     SIZE_CLS
                           G            5        GR       7     SIZE_CLS
                           G            7        GR       7     SIZE_CLS
                           G            9        GR       7     SIZE_CLS
                           G           11        GR       7     SIZE_CLS
                           G           13        GR       7     SIZE_CLS
                           G           15        GR       7     SIZE_CLS
                           G           17        GR       7     SIZE_CLS
                           G           19        GR       7     SIZE_CLS
                           H            1        GR       7     SIZE_CLS
                           H            3        CB       7     SIZE_CLS
                           H            5        CB       7     SIZE_CLS
                           H            7        CB       7     SIZE_CLS
                           H            9        GR       7     SIZE_CLS
                           H           11        CB       7     SIZE_CLS
                           H           13        CB       7     SIZE_CLS
                           H           15        GR       7     SIZE_CLS
                           H           17        GR       7     SIZE_CLS
                           H           19        CB       7     SIZE_CLS
                           I            1        CB       7     SIZE_CLS
                           I            3        CB       7     SIZE_CLS
                           I            5        CB       7     SIZE_CLS
                           I            7        CB       7     SIZE_CLS
                           I            9        CB       7     SIZE_CLS
                           I           11        CB       7     SIZE_CLS
                           I           13        GR       7     SIZE_CLS
                           I           15        GR       7     SIZE_CLS
                           I           17        GR       7     SIZE_CLS
                           I           19        CB       7     SIZE_CLS
                           J            1        GR       7     SIZE_CLS
                           J            3        GR       7     SIZE_CLS
                           J            5        CB       7     SIZE_CLS
                           J            7        CB       7     SIZE_CLS
                           J            9        CB       7     SIZE_CLS
                           J           11        CB       7     SIZE_CLS
                           J           13        CB       7     SIZE_CLS
                           J           15        CB       7     SIZE_CLS
                           J           17        CB       7     SIZE_CLS
                           J           19        CB       7     SIZE_CLS
                           A            1        CB       8     SIZE_CLS
                           A            3        CB       8     SIZE_CLS
                           A            5        CB       8     SIZE_CLS
                           A            7        CB       8     SIZE_CLS
                           A            9        CB       8     SIZE_CLS
                           A           11        CB       8     SIZE_CLS
                           A           13        CB       8     SIZE_CLS
                           A           15        CB       8     SIZE_CLS
                           A           17        CB       8     SIZE_CLS
                           A           19        CB       8     SIZE_CLS
                           B            1        CB       8     SIZE_CLS
                           B            3        GR       8     SIZE_CLS
                           B            5        CB       8     SIZE_CLS
                           B            7        GR       8     SIZE_CLS
                           B            9        CB       8     SIZE_CLS
                           B           11        CB       8     SIZE_CLS
                           B           13        CB       8     SIZE_CLS
                           B           15        CB       8     SIZE_CLS
                           B           17        CB       8     SIZE_CLS
                           B           19        CB       8     SIZE_CLS
                           C            1        CB       8     SIZE_CLS
                           C            3        CB       8     SIZE_CLS
                           C            5        CB       8     SIZE_CLS
                           C            7        CB       8     SIZE_CLS
                           C            9        CB       8     SIZE_CLS
                           C           11        CB       8     SIZE_CLS
                           C           13        CB       8     SIZE_CLS
                           C           15        BL       8     SIZE_CLS
                           C           17        CB       8     SIZE_CLS
                           C           19        CB       8     SIZE_CLS
                           D            1        BH       8     SIZE_CLS
                           D            3        CB       8     SIZE_CLS
                           D            5        CB       8     SIZE_CLS
                           D            7        CB       8     SIZE_CLS
                           D            9        CB       8     SIZE_CLS
                           D           11        GR       8     SIZE_CLS
                           D           13        GR       8     SIZE_CLS
                           D           15        CB       8     SIZE_CLS
                           D           17        CB       8     SIZE_CLS
                           D           19        CB       8     SIZE_CLS
                           E            1        CB       8     SIZE_CLS
                           E            3        CB       8     SIZE_CLS
                           E            5        CB       8     SIZE_CLS
                           E            7        GR       8     SIZE_CLS
                           E            9        GR       8     SIZE_CLS
                           E           11        BL       8     SIZE_CLS
                           E           13        BL       8     SIZE_CLS
                           E           15        BL       8     SIZE_CLS
                           E           17        BL       8     SIZE_CLS
                           E           19        CB       8     SIZE_CLS
                           F            1        CB       8     SIZE_CLS
                           F            3        GR       8     SIZE_CLS
                           F            5        SA       8     SIZE_CLS
                           F            7        SA       8     SIZE_CLS
                           F            9        GR       8     SIZE_CLS
                           F           11        SA       8     SIZE_CLS
                           F           13        GR       8     SIZE_CLS
                           F           15        SA       8     SIZE_CLS
                           F           17        CB       8     SIZE_CLS
                           F           19        GR       8     SIZE_CLS
                           G            1        CB       8     SIZE_CLS
                           G            3        BL       8     SIZE_CLS
                           G            5        BL       8     SIZE_CLS
                           G            7        BL       8     SIZE_CLS
                           G            9        CB       8     SIZE_CLS
                           G           11        GR       8     SIZE_CLS
                           G           13        SA       8     SIZE_CLS
                           G           15        CB       8     SIZE_CLS
                           G           17        SA       8     SIZE_CLS
                           G           19        CB       8     SIZE_CLS
                           H            1        CB       8     SIZE_CLS
                           H            3        CB       8     SIZE_CLS
                           H            5        CB       8     SIZE_CLS
                           H            7        CB       8     SIZE_CLS
                           H            9        CB       8     SIZE_CLS
                           H           11        SA       8     SIZE_CLS
                           H           13        CB       8     SIZE_CLS
                           H           15        CB       8     SIZE_CLS
                           H           17        CB       8     SIZE_CLS
                           H           19        CB       8     SIZE_CLS
                           I            1        CB       8     SIZE_CLS
                           I            3        CB       8     SIZE_CLS
                           I            5        CB       8     SIZE_CLS
                           I            7        SA       8     SIZE_CLS
                           I            9        CB       8     SIZE_CLS
                           I           11        SA       8     SIZE_CLS
                           I           13        CB       8     SIZE_CLS
                           I           15        CB       8     SIZE_CLS
                           I           17        CB       8     SIZE_CLS
                           I           19        CB       8     SIZE_CLS
                           J            1        CB       8     SIZE_CLS
                           J            3        CB       8     SIZE_CLS
                           J            5        CB       8     SIZE_CLS
                           J            7        GR       8     SIZE_CLS
                           J            9        CB       8     SIZE_CLS
                           J           11        CB       8     SIZE_CLS
                           J           13        BH       8     SIZE_CLS
                           J           15        GR       8     SIZE_CLS
                           J           17        GR       8     SIZE_CLS
                           J           19        GR       8     SIZE_CLS
                           A            1        CB       9     SIZE_CLS
                           A            3        GR       9     SIZE_CLS
                           A            5        GR       9     SIZE_CLS
                           A            7        CB       9     SIZE_CLS
                           A            9        CB       9     SIZE_CLS
                           A           11        CB       9     SIZE_CLS
                           A           13        CB       9     SIZE_CLS
                           A           15        CB       9     SIZE_CLS
                           A           17        CB       9     SIZE_CLS
                           A           19        CB       9     SIZE_CLS
                           B            1        CB       9     SIZE_CLS
                           B            3        GR       9     SIZE_CLS
                           B            5        CB       9     SIZE_CLS
                           B            7        CB       9     SIZE_CLS
                           B            9        CB       9     SIZE_CLS
                           B           11        CB       9     SIZE_CLS
                           B           13        CB       9     SIZE_CLS
                           B           15        CB       9     SIZE_CLS
                           B           17        CB       9     SIZE_CLS
                           B           19        CB       9     SIZE_CLS
                           C            1        SA       9     SIZE_CLS
                           C            3        SA       9     SIZE_CLS
                           C            5        GR       9     SIZE_CLS
                           C            7        GR       9     SIZE_CLS
                           C            9        CB       9     SIZE_CLS
                           C           11        CB       9     SIZE_CLS
                           C           13        CB       9     SIZE_CLS
                           C           15        CB       9     SIZE_CLS
                           C           17        CB       9     SIZE_CLS
                           C           19        CB       9     SIZE_CLS
                           D            1        CB       9     SIZE_CLS
                           D            3        CB       9     SIZE_CLS
                           D            5        CB       9     SIZE_CLS
                           D            7        CB       9     SIZE_CLS
                           D            9        GR       9     SIZE_CLS
                           D           11        GR       9     SIZE_CLS
                           D           13        GR       9     SIZE_CLS
                           D           15        GR       9     SIZE_CLS
                           D           17        GR       9     SIZE_CLS
                           D           19        CB       9     SIZE_CLS
                           E            1        CB       9     SIZE_CLS
                           E            3        GR       9     SIZE_CLS
                           E            5        GR       9     SIZE_CLS
                           E            7        GR       9     SIZE_CLS
                           E            9        GR       9     SIZE_CLS
                           E           11        CB       9     SIZE_CLS
                           E           13        CB       9     SIZE_CLS
                           E           15        CB       9     SIZE_CLS
                           E           17        CB       9     SIZE_CLS
                           E           19        CB       9     SIZE_CLS
                           F            1        GR       9     SIZE_CLS
                           F            3        GR       9     SIZE_CLS
                           F            5        GR       9     SIZE_CLS
                           F            7        GR       9     SIZE_CLS
                           F            9        GR       9     SIZE_CLS
                           F           11        CB       9     SIZE_CLS
                           F           13        CB       9     SIZE_CLS
                           F           15        CB       9     SIZE_CLS
                           F           17        CB       9     SIZE_CLS
                           F           19        CB       9     SIZE_CLS
                           G            1        CB       9     SIZE_CLS
                           G            3        GR       9     SIZE_CLS
                           G            5        SA       9     SIZE_CLS
                           G            7        SA       9     SIZE_CLS
                           G            9        GR       9     SIZE_CLS
                           G           11        GR       9     SIZE_CLS
                           G           13        SA       9     SIZE_CLS
                           G           15        SA       9     SIZE_CLS
                           G           17        SA       9     SIZE_CLS
                           G           19        SA       9     SIZE_CLS
                           H            1        CB       9     SIZE_CLS
                           H            3        CB       9     SIZE_CLS
                           H            5        CB       9     SIZE_CLS
                           H            7        CB       9     SIZE_CLS
                           H            9        CB       9     SIZE_CLS
                           H           11        CB       9     SIZE_CLS
                           H           13        CB       9     SIZE_CLS
                           H           15        CB       9     SIZE_CLS
                           H           17        GR       9     SIZE_CLS
                           H           19        SA       9     SIZE_CLS
                           I            1        SA       9     SIZE_CLS
                           I            3        SA       9     SIZE_CLS
                           I            5        GR       9     SIZE_CLS
                           I            7        CB       9     SIZE_CLS
                           I            9        CB       9     SIZE_CLS
                           I           11        CB       9     SIZE_CLS
                           I           13        SA       9     SIZE_CLS
                           I           15        CB       9     SIZE_CLS
                           I           17        CB       9     SIZE_CLS
                           I           19        CB       9     SIZE_CLS
                           J            1        CB       9     SIZE_CLS
                           J            3        SA       9     SIZE_CLS
                           J            5        GR       9     SIZE_CLS
                           J            7        GR       9     SIZE_CLS
                           J            9        GR       9     SIZE_CLS
                           J           11        CB       9     SIZE_CLS
                           J           13        CB       9     SIZE_CLS
                           J           15        CB       9     SIZE_CLS
                           J           17        CB       9     SIZE_CLS
                           J           19        CB       9     SIZE_CLS
                               "
                            )
      df2 <- read.table(bob, header=TRUE, stringsAsFactors=FALSE)
      close(bob)
      
      return(df2)
}


metsSubstrateCharacterization.boatableLittoralSubstrateData <- function()
# Creates dataframe of boatable protocol substrate data for unit test
{
  bob <- textConnection("TRANSECT RESULT UID PARAMETER
                            A          GF       7     SHOREDOM
                            B          FN       7     SHOREDOM
                            C          FN       7     SHOREDOM
                            D          GC       7     SHOREDOM
                            E          SA       7     SHOREDOM
                            F          SA       7     SHOREDOM
                            G          CB       7     SHOREDOM
                            H          SA       7     SHOREDOM
                            I          SA       7     SHOREDOM
                            J          SA       7     SHOREDOM
                            K          RS       7     SHOREDOM
                            A          GC       8     SHOREDOM
                            B          CB       8     SHOREDOM
                            C          CB       8     SHOREDOM
                            D          RR       8     SHOREDOM
                            E          SB       8     SHOREDOM
                            F          RR       8     SHOREDOM
                            G          CB       8     SHOREDOM
                            H          CB       8     SHOREDOM
                            I          SA       8     SHOREDOM
                            J          SA       8     SHOREDOM
                            K          CB       8     SHOREDOM
                            A          GC       9     SHOREDOM
                            B          SA       9     SHOREDOM
                            C          GF       9     SHOREDOM
                            D          GC       9     SHOREDOM
                            E          GC       9     SHOREDOM
                            F          CB       9     SHOREDOM
                            G          GC       9     SHOREDOM
                            H          CB       9     SHOREDOM
                            I          RR       9     SHOREDOM
                            J          GC       9     SHOREDOM
                            K          GC       9     SHOREDOM
                            A          CB       7     SHORESEC
                            B          SA       7     SHORESEC
                            C          SA       7     SHORESEC
                            D          GF       7     SHORESEC
                            E          GF       7     SHORESEC
                            F          FN       7     SHORESEC
                            G          SA       7     SHORESEC
                            H          GF       7     SHORESEC
                            I          CB       7     SHORESEC
                            J          FN       7     SHORESEC
                            K          SA       7     SHORESEC
                            A          GF       8     SHORESEC
                            B          SB       8     SHORESEC
                            C          GC       8     SHORESEC
                            D          SA       8     SHORESEC
                            E          CB       8     SHORESEC
                            F          XB       8     SHORESEC
                            G          GC       8     SHORESEC
                            H          GC       8     SHORESEC
                            I          CB       8     SHORESEC
                            J          GF       8     SHORESEC
                            K          GC       8     SHORESEC
                            A          GF       9     SHORESEC
                            B          CB       9     SHORESEC
                            C          GC       9     SHORESEC
                            D          GF       9     SHORESEC
                            E          GF       9     SHORESEC
                            F          GC       9     SHORESEC
                            G          CB       9     SHORESEC
                            H          GC       9     SHORESEC
                            I          SA       9     SHORESEC
                            J          CB       9     SHORESEC
                            K          GF       9     SHORESEC
                            A          CB       7     BOTTOMDOM
                            B          CB       7     BOTTOMDOM
                            C          CB       7     BOTTOMDOM
                            D          CB       7     BOTTOMDOM
                            E          GC       7     BOTTOMDOM
                            F          SA       7     BOTTOMDOM
                            G          CB       7     BOTTOMDOM
                            H          CB       7     BOTTOMDOM
                            I          SA       7     BOTTOMDOM
                            J          FN       7     BOTTOMDOM
                            K          SA       7     BOTTOMDOM
                            A          GC       8     BOTTOMDOM
                            B          CB       8     BOTTOMDOM
                            C          SB       8     BOTTOMDOM
                            D          CB       8     BOTTOMDOM
                            E          CB       8     BOTTOMDOM
                            F          RR       8     BOTTOMDOM
                            G          CB       8     BOTTOMDOM
                            H          CB       8     BOTTOMDOM
                            I          CB       8     BOTTOMDOM
                            J          GF       8     BOTTOMDOM
                            K          CB       8     BOTTOMDOM
                            A          GC       9     BOTTOMDOM
                            B          CB       9     BOTTOMDOM
                            C          GF       9     BOTTOMDOM
                            D          GC       9     BOTTOMDOM
                            E          GC       9     BOTTOMDOM
                            F          CB       9     BOTTOMDOM
                            G          GF       9     BOTTOMDOM
                            H          GC       9     BOTTOMDOM
                            I          SA       9     BOTTOMDOM
                            J          CB       9     BOTTOMDOM
                            K          GC       9     BOTTOMDOM
                            A          GF       7     BOTTOMSEC
                            B          GC       7     BOTTOMSEC
                            C          GC       7     BOTTOMSEC
                            D          GC       7     BOTTOMSEC
                            E          GF       7     BOTTOMSEC
                            F          FN       7     BOTTOMSEC
                            G          SA       7     BOTTOMSEC
                            H          GC       7     BOTTOMSEC
                            I          CB       7     BOTTOMSEC
                            J          SA       7     BOTTOMSEC
                            K          RS       7     BOTTOMSEC
                            A          GF       8     BOTTOMSEC
                            B          SB       8     BOTTOMSEC
                            C          GC       8     BOTTOMSEC
                            D          RR       8     BOTTOMSEC
                            E          SB       8     BOTTOMSEC
                            F          XB       8     BOTTOMSEC
                            G          GC       8     BOTTOMSEC
                            H          GC       8     BOTTOMSEC
                            I          GC       8     BOTTOMSEC
                            J          SA       8     BOTTOMSEC
                            K          GC       8     BOTTOMSEC
                            A          GF       9     BOTTOMSEC
                            B          GC       9     BOTTOMSEC
                            C          SA       9     BOTTOMSEC
                            D          CB       9     BOTTOMSEC
                            E          CB       9     BOTTOMSEC
                            F          GC       9     BOTTOMSEC
                            G          GC       9     BOTTOMSEC
                            H          GF       9     BOTTOMSEC
                            I          RR       9     BOTTOMSEC
                            J          GC       9     BOTTOMSEC
                            K          CB       9     BOTTOMSEC
                            "
                            )

      df3 <- read.table(bob, header=TRUE, stringsAsFactors=FALSE)
      close(bob)
      
      return(df3)
}


metsSubstrateCharacterization.expectedResults <- function()
# returns dataframe of expected metrics calculation results for unit test
# UID 1 is 2004  WAZP99-0893 v1  (wadeable)
# UID 2 is 2004  WAZP99-0893 v2  (wadeable)
# UID 3 is 2004  WIDP99-0769 v1  (wadeable)
# UID 4 is 2004  WIDP99-0769 v2  (wadeable)
# UID 5 is 2004  WMTP99-0733 v1  (wadeable)
# UID 6 is 2004  WMTP99-0837 v1  (wadeable)
# UID 7 is 2004  WAZP99-0504 v1  (boatable)
# UID 8 is 2004  WCAP99-0817 v1  (boatable)
# UID 9 is 2004  WCAP99-0817 v2  (boatable)
# UID 10 is 2004 WCAP03-R236 v1  (wadeable)
# UID 11 is 2002 WORP99-0885 v1  (wadeable)
{
  bob <- textConnection("UID METRIC RESULT
                    FAKE_ONLY_BEDROCK       n           10
                    FAKE_ONLY_BEDROCK       n_nor       0
                    FAKE_ONLY_BEDROCK       pct_bdrk    100
                    FAKE_ONLY_BEDROCK       pct_bigr    100
                    FAKE_ONLY_BEDROCK       pct_bl      0
                    FAKE_ONLY_BEDROCK       pct_cb      0
                    FAKE_ONLY_BEDROCK       pct_fn      0
                    FAKE_ONLY_BEDROCK       pct_gc      0
                    FAKE_ONLY_BEDROCK       pct_gf      0
                    FAKE_ONLY_BEDROCK       pct_hp      0
                    FAKE_ONLY_BEDROCK       pct_om      0
                    FAKE_ONLY_BEDROCK       pct_org     0
                    FAKE_ONLY_BEDROCK       pct_ot      0
                    FAKE_ONLY_BEDROCK       pct_rc      0
                    FAKE_ONLY_BEDROCK       pct_rr      0
                    FAKE_ONLY_BEDROCK       pct_rs      100
                    FAKE_ONLY_BEDROCK       pct_sa      0
                    FAKE_ONLY_BEDROCK       pct_safn    0
                    FAKE_ONLY_BEDROCK       pct_sb      0
                    FAKE_ONLY_BEDROCK       pct_sfgf    0
                    FAKE_ONLY_BEDROCK       pct_wd      0
                    FAKE_ONLY_BEDROCK       pct_xb      0
                    FAKE_ONLY_BEDROCK       lsub_d16    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_d25    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_d50    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_d75    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_d84    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_dmm    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub_iqr    0
                    FAKE_ONLY_BEDROCK       lsub2d16    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2d25    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2d50    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2d75    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2d84    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2dmm    3.75257498915995
                    FAKE_ONLY_BEDROCK       lsub2iqr    0
                    FAKE_ONLY_BEDROCK       lsubd_sd    0
                    FAKE_ONLY_BEDROCK       lsubd2sd    0
                    FAKE_ONLY_NONMINERAL    n           0
                    FAKE_ONLY_NONMINERAL    n_nor       0
                    FAKE_ONLY_NONMINERAL    pct_bdrk    0
                    FAKE_ONLY_NONMINERAL    pct_bigr    0
                    FAKE_ONLY_NONMINERAL    pct_bl      0
                    FAKE_ONLY_NONMINERAL    pct_cb      0
                    FAKE_ONLY_NONMINERAL    pct_fn      0
                    FAKE_ONLY_NONMINERAL    pct_gc      0
                    FAKE_ONLY_NONMINERAL    pct_gf      0
                    FAKE_ONLY_NONMINERAL    pct_hp      0
                    FAKE_ONLY_NONMINERAL    pct_om      0
                    FAKE_ONLY_NONMINERAL    pct_org     0
                    FAKE_ONLY_NONMINERAL    pct_ot      100
                    FAKE_ONLY_NONMINERAL    pct_rc      0
                    FAKE_ONLY_NONMINERAL    pct_rr      0
                    FAKE_ONLY_NONMINERAL    pct_rs      0
                    FAKE_ONLY_NONMINERAL    pct_sa      0
                    FAKE_ONLY_NONMINERAL    pct_safn    0
                    FAKE_ONLY_NONMINERAL    pct_sb      0
                    FAKE_ONLY_NONMINERAL    pct_sfgf    0
                    FAKE_ONLY_NONMINERAL    pct_wd      0
                    FAKE_ONLY_NONMINERAL    pct_xb      0
                    FAKE_ONLY_NONMINERAL    lsub_d16    NA
                    FAKE_ONLY_NONMINERAL    lsub_d25    NA
                    FAKE_ONLY_NONMINERAL    lsub_d50    NA
                    FAKE_ONLY_NONMINERAL    lsub_d75    NA
                    FAKE_ONLY_NONMINERAL    lsub_d84    NA
                    FAKE_ONLY_NONMINERAL    lsub_dmm    NA
                    FAKE_ONLY_NONMINERAL    lsub_iqr    NA
                    FAKE_ONLY_NONMINERAL    lsub2d16    NA
                    FAKE_ONLY_NONMINERAL    lsub2d25    NA
                    FAKE_ONLY_NONMINERAL    lsub2d50    NA
                    FAKE_ONLY_NONMINERAL    lsub2d75    NA
                    FAKE_ONLY_NONMINERAL    lsub2d84    NA
                    FAKE_ONLY_NONMINERAL    lsub2dmm    NA
                    FAKE_ONLY_NONMINERAL    lsub2iqr    NA
                    FAKE_ONLY_NONMINERAL    lsubd_sd    NA
                    FAKE_ONLY_NONMINERAL    lsubd2sd    NA
                    1	lsub_dmm	   0.971427701
                    2	lsub_dmm	   1.270724227
                    3	lsub_dmm	   1.205053726
                    4	lsub_dmm	   1.371489818
                    5	lsub_dmm	   0.178988038
                    6	lsub_dmm	   1.040771394
                    1	pct_gf	      5.714285714
                    2	pct_gf	      11.42857143
                    3	pct_gf	      0.869565217
                    4	pct_gf	      4.424778761
                    5	pct_gf	      49.52380952
                    6	pct_gf	      33.33333333
                    1	pct_rs	      0
                    2	pct_rs	      0
                    3	pct_rs	      0
                    4	pct_rs	      0
                    5	pct_rs	      0
                    6	pct_rs	      4.761904762
                    1	pct_ot	      0
                    2	pct_ot	      2.857142857
                    3	pct_ot	      0
                    4	pct_ot	      0
                    5	pct_ot	      0
                    6	pct_ot	      0
                    1	pct_fn	      12.38095238
                    2	pct_fn	      14.28571429
                    3	pct_fn	      13.04347826
                    4	pct_fn	      9.734513274
                    5	pct_fn	      20.95238095
                    6	pct_fn	      0
                    1	pct_om	      0
                    2	pct_om	      0
                    3	pct_om	      0
                    4	pct_om	      0
                    5	pct_om	      0
                    6	pct_om	      0
                    1	pct_sa	      20.95238095
                    2	pct_sa	      3.80952381
                    3	pct_sa	      9.565217391
                    4	pct_sa	      3.539823009
                    5	pct_sa	      10.47619048
                    6	pct_sa	      20.95238095
                    1	pct_rr	      4.761904762
                    2	pct_rr	      1.904761905
                    3	pct_rr	      0
                    4	pct_rr	      0
                    5	pct_rr	      0
                    6	pct_rr	      0
                    1	pct_gc	      29.52380952
                    2	pct_gc	      24.76190476
                    3	pct_gc	      24.34782609
                    4	pct_gc	      38.05309735
                    5	pct_gc	      14.28571429
                    6	pct_gc	      15.23809524
                    1	pct_cb	      15.23809524
                    2	pct_cb	      22.85714286
                    3	pct_cb	      39.13043478
                    4	pct_cb	      36.28318584
                    5	pct_cb	      3.80952381
                    6	pct_cb	        18.0952381
                    1	pct_wd	        0
                    2	pct_wd	        0
                    3	pct_wd	        3.47826087
                    4	pct_wd	        0.884955752
                    5	pct_wd	        0.952380952
                    6	pct_wd	        6.666666667
                    1	pct_hp	        0
                    2	pct_hp	        0
                    3	pct_hp	        0
                    4	pct_hp	        0
                    5	pct_hp	        0
                    6	pct_hp	        0
                    1	pct_bl	        11.42857143
                    2	pct_bl	        18.0952381
                    3	pct_bl	        9.565217391
                    4	pct_bl	        7.079646018
                    5	pct_bl	        0
                    6	pct_bl	        0.952380952
                    1	pct_rc	        0
                    2	pct_rc	        0
                    3	pct_rc	        0
                    4	pct_rc	        0
                    5	pct_rc	        0
                    6	pct_rc	        0
                    1	pct_sb	        8.571428571
                    2	pct_sb	        12.38095238
                    3	pct_sb	        9.565217391
                    4	pct_sb	        7.079646018
                    5	pct_sb	        0
                    6	pct_sb	        0
                    1	pct_xb	        2.857142857
                    2	pct_xb	        5.714285714
                    3	pct_xb	        0
                    4	pct_xb	        0
                    5	pct_xb	        0
                    6	pct_xb	        0.952380952
                    1	pct_safn    	33.33333333
                    2	pct_safn    	18.0952381
                    3	pct_safn    	22.60869565
                    4	pct_safn    	13.27433628
                    5	pct_safn    	31.42857143
                    6	pct_safn    	20.95238095
                    1	pct_sfgf    	39.04761905
                    2	pct_sfgf    	29.52380952
                    3	pct_sfgf    	23.47826087
                    4	pct_sfgf    	17.69911504
                    5	pct_sfgf    	80.95238095
                    6	pct_sfgf    	54.28571429
                    1	pct_bigr    	60.95238095
                    2	pct_bigr    	67.61904762
                    3	pct_bigr    	73.04347826
                    4	pct_bigr    	81.4159292
                    5	pct_bigr    	18.0952381
                    6	pct_bigr    	39.04761905
                    1	pct_bdrk    	4.761904762
                    2	pct_bdrk    	1.904761905
                    3	pct_bdrk    	0
                    4	pct_bdrk    	0
                    5	pct_bdrk    	0
                    6	pct_bdrk    	4.761904762
                    1	pct_org	     0
                    2	pct_org	     0
                    3	pct_org	     3.47826087
                    4	pct_org	     0.884955752
                    5	pct_org	     0.952380952
                    6	pct_org	     6.666666667
                    1	n	           105
                    2	n	           102
                    3	n	           111
                    4	n	           112
                    5	n	           104
                    6	n	           98
                    1	lsubd_sd    	1.659011998
                    2	lsubd_sd    	1.646761798
                    3	lsubd_sd    	1.56617958
                    4	lsubd_sd    	1.317263084
                    5	lsubd_sd    	1.315109981
                    6	lsubd_sd    	1.102684228
                    1	lsub_d75    	2.102056193
                    2	lsub_d75    	2.102056193
                    3	lsub_d75    	2.102056193
                    4	lsub_d75    	2.102056193
                    5	lsub_d75    	0.752578502
                    6	lsub_d75    	2.102056193
                    1	lsub_d50    	1.505149978
                    2	lsub_d50    	1.505149978
                    3	lsub_d50    	2.102056193
                    4	lsub_d50    	1.505149978
                    5	lsub_d50    	0.752578502
                    6	lsub_d50    	0.752578502
                    1	lsub_d25    	-0.460409579
                    2	lsub_d25    	0.752578502
                    3	lsub_d25    	1.505149978
                    4	lsub_d25    	1.505149978
                    5	lsub_d25    	-0.460409579
                    6	lsub_d25    	0.752578502
                    1	lsub_iqr    	2.562465772
                    2	lsub_iqr    	1.349477691
                    3	lsub_iqr    	0.596906214
                    4	lsub_iqr    	0.596906214
                    5	lsub_iqr    	1.212988081
                    6	lsub_iqr    	1.349477691
                    1	lsub_d16    	-0.460409579
                    2	lsub_d16    	-0.460409579
                    3	lsub_d16    	-0.460409579
                    4	lsub_d16    	0.752578502
                    5	lsub_d16    	-2.110922507
                    6	lsub_d16    	-0.460409579
                    1	lsub_d84    	3
                    2	lsub_d84    	3
                    3	lsub_d84    	2.102056193
                    4	lsub_d84    	2.102056193
                    5	lsub_d84    	1.505149978
                    6	lsub_d84    	2.102056193
                    1	n_nor       	100
                    2	n_nor       	100
                    3	n_nor       	111
                    4	n_nor       	112
                    5	n_nor       	104
                    6	n_nor       	93
                    1	d84         	177.827941
                    2	d84         	344.2523628
                    3	d84         	203.725082
                    4	d84         	179.7889295
                    5	d84         	19.89958814
                    6	d84         	92.39399177
                    1	d50         	23.92828612
                    2	d50         	44.06430244
                    3	d50         	64.97631618
                    4	d50         	51.07051154
                    5	d50         	4.275679891
                    6	d50         	8.5741877
                    1	d16         	0.09678689
                    2	d16         	0.144168685
                    3	d16         	0.144628996
                    4	d16         	6.736503629
                    5	d16         	0.022127327
                    6	d16         	0.642940904
                    1	dgm         	6.520912324
                    2	dgm         	15.84948086
                    3	dgm         	14.97000428
                    4	dgm         	22.38657606
                    5	dgm         	1.510038563
                    6	dgm         	7.910650452
                    1	lsub2d84inor	2.25
                    2	lsub2d84inor	2.53687693
                    3	lsub2d84inor	2.309044501
                    4	lsub2d84inor	2.254762947
                    5	lsub2d84inor	1.298844088
                    6	lsub2d84inor	1.965643731
                    1	lsub2d50inor	1.378911593
                    2	lsub2d50inor	1.644086899
                    3	lsub2d50inor	1.812755086
                    4	lsub2d50inor	1.708170208
                    5	lsub2d50inor	0.631005183
                    6	lsub2d50inor	0.933192987
                    1	lsub2d16inor	-1.014183466
                    2	lsub2d16inor	-0.841129063
                    3	lsub2d16inor	-0.839744628
                    4	lsub2d16inor	0.828434548
                    5	lsub2d16inor	-1.655071054
                    6	lsub2d16inor	-0.191828944
                    1	subd2sd_nor   358.1641638
                    2	subd2sd_nor   478.0721722
                    3	subd2sd_nor   140.7835083
                    4	subd2sd_nor   123.7008047
                    5	subd2sd_nor   25.15376106
                    6	subd2sd_nor   209.5420598
                    1	lsub_dmm_nor	0.832370161
                    2	lsub_dmm_nor	1.221087142
                    3	lsub_dmm_nor	1.205053726
                    4	lsub_dmm_nor	1.371489818
                    5	lsub_dmm_nor	0.178988038
                    6	lsub_dmm_nor	0.894975313
                    1	sub_dmm_nor 	150.5750312
                    2	sub_dmm_nor 	229.3714463
                    3	sub_dmm_nor 	158.5372396
                    4	sub_dmm_nor 	130.2843335
                    5	sub_dmm_nor 	12.34711271
                    6	sub_dmm_nor 	44.31099484
                    1	lsubd_sd_nor	1.575166066
                    2	lsubd_sd_nor	1.624710327
                    3	lsubd_sd_nor	1.56617958
                    4	lsubd_sd_nor	1.317263084
                    5	lsubd_sd_nor	1.315109981
                    6	lsubd_sd_nor	0.927813992
                    1	subd_sd_nor 	318.1488465
                    2	subd_sd_nor 	378.0582464
                    3	subd_sd_nor 	285.3835134
                    4	subd_sd_nor 	247.493601
                    5	subd_sd_nor 	25.15376106
                    6	subd_sd_nor 	111.2624768
                    1	lsub2dmm    	0.954225987
                    2	lsub2dmm    	1.250065306
                    3	lsub2dmm    	1.175221925
                    4	lsub2dmm    	1.349987675
                    5	lsub2dmm    	0.178988038
                    6	lsub2dmm    	1.043843129
                    1	lsubd2sd    	1.64073679
                    2	lsubd2sd    	1.629810781
                    3	lsubd2sd    	1.533954309
                    4	lsubd2sd    	1.292510265
                    5	lsubd2sd    	1.315109981
                    6	lsubd2sd    	1.108601705
                    1	lsub2d75    	2.102056193
                    2	lsub2d75    	2.102056193
                    3	lsub2d75    	2.102056193
                    4	lsub2d75    	2.102056193
                    5	lsub2d75    	0.752578502
                    6	lsub2d75    	2.102056193
                    1	lsub2d50    	1.505149978
                    2	lsub2d50    	1.505149978
                    3	lsub2d50    	2.102056193
                    4	lsub2d50    	1.505149978
                    5	lsub2d50    	0.752578502
                    6	lsub2d50    	0.752578502
                    1	lsub2d25    	-0.460409579
                    2	lsub2d25    	0.752578502
                    3	lsub2d25    	1.505149978
                    4	lsub2d25    	1.505149978
                    5	lsub2d25     -0.460409579
                    6	lsub2d25     	0.752578502
                    1	lsub2iqr    	2.562465772
                    2	lsub2iqr    	1.349477691
                    3	lsub2iqr    	0.596906214
                    4	lsub2iqr    	0.596906214
                    5	lsub2iqr    	1.212988081
                    6	lsub2iqr    	1.349477691
                    1	lsub2d16     -0.460409579
                    2	lsub2d16     -0.460409579
                    3	lsub2d16     -0.460409579
                    4	lsub2d16      0.752578502
                    5	lsub2d16     -2.110922507
                    6	lsub2d16     -0.460409579
                    1	lsub2d84    	2.698970004
                    2	lsub2d84    	2.698970004
                    3	lsub2d84    	2.102056193
                    4	lsub2d84    	2.102056193
                    5	lsub2d84    	1.505149978
                    6	lsub2d84    	2.102056193
                    1	lsub2dmm_nor	0.814308361
                    2	lsub2dmm_nor	1.200015042
                    3	lsub2dmm_nor	1.175221925
                    4	lsub2dmm_nor	1.349987675
                    5	lsub2dmm_nor	0.178988038
                    6	lsub2dmm_nor	0.898212195
                    1	sub2dmm_nor 	135.5750312
                    2	sub2dmm_nor 	224.3714463
                    3	sub2dmm_nor 	108.9876901
                    4	sub2dmm_nor 	94.57004773
                    5	sub2dmm_nor 	12.34711271
                    6	sub2dmm_nor 	55.06368301
                    1	lsubd2sd_nor	1.553289803
                    2	lsubd2sd_nor	1.606517515
                    3	lsubd2sd_nor	1.533954309
                    4	lsubd2sd_nor	1.292510265
                    5	lsubd2sd_nor	1.315109981
                    6	lsubd2sd_nor	0.935728999
                    7	lsub_dmm	1.710650214
                    7	ldcbf_g08     NA
                    7	pct_bl	0
                    7	pct_cb	67
                    7	pct_fn	0
                    7	pct_sa	3
                    7	pct_gr	30
                    7	pct_bh	0
                    7	pct_ot	0
                    7	pct_safn	3
                    7	n	100
                    7	lsubd_sd	0.614271537
                    7	lsub_d75	2.102056193
                    7	lsub_d50	2.102056193
                    7	lsub_d25	1.053616174
                    7	lsub_iqr	1.048440018
                    7	lsub_d16	1.053616174
                    7	lsub_d84	2.102056193
                    7	pct_dbbl	0
                    7	pct_dbcb	54.54545455
                    7	pct_dbgf	0
                    7	pct_dbsa	27.27272727
                    7	pct_dsbl	0
                    7	pct_dscb	9.090909091
                    7	pct_dsrr	0
                    7	pct_dssa	45.45454546
                    7	pct_sbcb	9.090909091
                    7	pct_sbgc	36.36363636
                    7	pct_sbrr	0
                    7	pct_sbsa	18.18181818
                    7	pct_ssbl	0
                    7	pct_sscb	18.18181818
                    7	pct_ssgf	27.27272727
                    7	pct_ssrr	0
                    7	pct_sssa	36.36363636
                    7	pct_ssgc	0
                    7	pct_dbgc	9.090909091
                    7	pct_dbrr	0
                    7	pct_dbrs	0
                    7	pct_dsrs	9.090909091
                    7	pct_sbfn	9.090909091
                    7	pct_sbrs	9.090909091
                    7	pct_ssfn	18.18181818
                    7	pct_ssrs	0
                    7	pct_dsgc	9.090909091
                    7	pct_sbbl	0
                    7	pct_sbgf	18.18181818
                    7	pct_dsfn	18.18181818
                    7	pct_dshp	0
                    7	pct_dbfn	9.090909091
                    7	pct_dsgf	9.090909091
                    7	pct_dbrc	0
                    7	pct_dsrc	0
                    7	pct_ssrc	0
                    7	pct_dsot	0
                    7	pct_dbot	0
                    7	pct_sbot	0
                    7	pct_ssot	0
                    7	pct_sbrc	0
                    7	pct_dbhp	0
                    7	pct_dswd	0
                    7	pct_sswd	0
                    7	pct_sbwd	0
                    7	pct_sshp	0
                    7	pct_sbhp	0
                    7	pct_dbsb	0
                    7	pct_dbxb	0
                    7	pct_dssb	0
                    7	pct_dsxb	0
                    7	pct_sbsb	0
                    7	pct_sbxb	0
                    7	pct_sssb	0
                    7	pct_ssxb	0
                    7	pct_dbwd	0
                    7	pct_dbom	0
                    7	pct_dsom	0
                    7	pct_sbom	0
                    7	pct_ssom	0
                    7	dgm       NA
                    7	d16       NA
                    7	lsub2d16inor NA
                    7	d50           NA
                    7	lsub2d50inor  NA
                    7	d84          NA
                    7	lsub2d84inor  NA
                    8	lsub_dmm	1.81901
                    8	ldcbf_g08     NA
                    8	pct_bl	8
                    8	pct_cb	66
                    8	pct_fn	0
                    8	pct_sa	9
                    8	pct_gr	15
                    8	pct_bh	2
                    8	pct_ot	0
                    8	pct_safn	9
                    8	n	100
                    8	lsubd_sd	0.89596
                    8	lsub_d75	2.102056193
                    8	lsub_d50	2.102056193
                    8	lsub_d25	2.102056193
                    8	lsub_iqr	0
                    8	lsub_d16	1.053616174
                    8	lsub_d84	2.102056193
                    8	pct_dbbl	0
                    8	pct_dbcb	63.63636364
                    8	pct_dbgf	9.090909091
                    8	pct_dbsa	0
                    8	pct_dbgc	9.090909091
                    8	pct_dbrr	9.090909091
                    8	pct_dbrs	0
                    8	pct_dbfn	0
                    8	pct_dbrc	0
                    8	pct_dbot	0
                    8	pct_dbhp	0
                    8	pct_dbsb	9.090909091
                    8	pct_dbxb	0
                    8	pct_dbwd	0
                    8	pct_dbom	0
                    8	pct_dsbl	0
                    8	pct_dscb	45.45454546
                    8	pct_dsrr	18.18181818
                    8	pct_dssa	18.18181818
                    8	pct_dsrs	0
                    8	pct_dsgc	9.090909091
                    8	pct_dsfn	0
                    8	pct_dshp	0
                    8	pct_dsgf	0
                    8	pct_dsrc	0
                    8	pct_dsot	0
                    8	pct_dswd	0
                    8	pct_dssb	9.090909091
                    8	pct_dsxb	0
                    8	pct_sbcb	0
                    8	pct_sbgc	45.45454546
                    8	pct_sbrr	9.090909091
                    8	pct_sbsa	9.090909091
                    8	pct_ssbl	0
                    8	pct_sscb	18.18181818
                    8	pct_ssgf	18.18181818
                    8	pct_ssrr	0
                    8	pct_sssa	9.090909091
                    8	pct_ssgc	36.36363636
                    8	pct_sbfn	0
                    8	pct_sbrs	0
                    8	pct_ssfn	0
                    8	pct_ssrs	0
                    8	pct_sbbl	0
                    8	pct_sbgf	9.090909091
                    8	pct_ssrc	0
                    8	pct_sbot	0
                    8	pct_ssot	0
                    8	pct_sbrc	0
                    8	pct_sswd	0
                    8	pct_sbwd	0
                    8	pct_sshp	0
                    8	pct_sbhp	0
                    8	pct_sbsb	18.18181818
                    8	pct_sbxb	9.090909091
                    8	pct_sssb	9.090909091
                    8	pct_ssxb	9.090909091
                    8	pct_dsom	0
                    8	pct_sbom	0
                    8	pct_ssom	0
                    8	dgm           NA
                    8	d16           NA
                    8	lsub2d16inor NA
                    8	d50           NA
                    8	lsub2d50inor NA
                    8	d84           NA
                    8	lsub2d84inor NA
                    9	lsub_dmm	1.485856837
                    9	ldcbf_g08    NA
                    9	pct_bl	0
                    9	pct_cb	60
                    9	pct_fn	0
                    9	pct_sa	13
                    9	pct_gr	27
                    9	pct_bh	0
                    9	pct_ot	0
                    9	pct_safn	13
                    9	n	100
                    9	lsubd_sd	0.882317604
                    9	lsub_d75	2.102056193
                    9	lsub_d50	2.102056193
                    9	lsub_d25	1.053616174
                    9	lsub_iqr	1.048440018
                    9	lsub_d16	1.053616174
                    9	lsub_d84	2.102056193
                    9	pct_dbbl	0
                    9	pct_dbcb	27.27272727
                    9	pct_dbgf	18.18181818
                    9	pct_dbsa	9.090909091
                    9	pct_dsbl	0
                    9	pct_dscb	18.18181818
                    9	pct_dsrr	9.090909091
                    9	pct_dssa	9.090909091
                    9	pct_sbcb	27.27272727
                    9	pct_sbgc	36.36363636
                    9	pct_sbrr	9.090909091
                    9	pct_sbsa	9.090909091
                    9	pct_ssbl	0
                    9	pct_sscb	27.27272727
                    9	pct_ssgf	36.36363636
                    9	pct_ssrr	0
                    9	pct_sssa	9.090909091
                    9	pct_ssgc	27.27272727
                    9	pct_dbgc	45.45454546
                    9	pct_dbrr	0
                    9	pct_dbrs	0
                    9	pct_dsrs	0
                    9	pct_sbfn	0
                    9	pct_sbrs	0
                    9	pct_ssfn	0
                    9	pct_ssrs	0
                    9	pct_dsgc	54.54545455
                    9	pct_sbbl	0
                    9	pct_sbgf	18.18181818
                    9	pct_dsfn	0
                    9	pct_dshp	0
                    9	pct_dbfn	0
                    9	pct_dsgf	9.090909091
                    9	pct_dbrc	0
                    9	pct_dsrc	0
                    9	pct_ssrc	0
                    9	pct_dsot	0
                    9	pct_dbot	0
                    9	pct_sbot	0
                    9	pct_ssot	0
                    9	pct_sbrc	0
                    9	pct_dbhp	0
                    9	pct_dswd	0
                    9	pct_sswd	0
                    9	pct_sbwd	0
                    9	pct_sshp	0
                    9	pct_sbhp	0
                    9	pct_dbsb	0
                    9	pct_dbxb	0
                    9	pct_dssb	0
                    9	pct_dsxb	0
                    9	pct_sbsb	0
                    9	pct_sbxb	0
                    9	pct_sssb	0
                    9	pct_ssxb	0
                    9	pct_dbwd	0
                    9	pct_dbom	0
                    9	pct_dsom	0
                    9	pct_sbom	0
                    9	pct_ssom	0
                    9	dgm           NA
                    9	d16           NA
                    9	lsub2d16inor NA
                    9	d50           NA
                    9	lsub2d50inor NA
                    9	d84           NA
                    9	lsub2d84inor NA
                   10  lsub_dmm        1.634289424450710
                   10  PCT_GF        10.476190476190400
                   10  PCT_RS        0.952380952380950
                   10  PCT_OT        0.000000000000000
                   10  PCT_FN        0.000000000000000
                   10  PCT_OM        0.000000000000000
                   10  PCT_SA        10.476190476190400
                   10  PCT_RR        0.000000000000000
                   10  PCT_GC        34.285714285714200
                   10  PCT_CB        28.571428571428500
                   10  PCT_WD        0.952380952380950
                   10  PCT_HP        0.952380952380950
                   10  PCT_BL        13.333333333333300
                   10  PCT_RC        0.000000000000000
                   10  PCT_SB        6.666666666666660
                   10  PCT_XB        6.666666666666660
                   10  pct_safn        10.476190476190400
                   10  pct_sfgf        20.952380952380900
                   10  pct_bigr        77.142857142857100
                   10  pct_bdrk        0.952380952380950
                   10  pct_org        0.952380952380950
                   10  n        104.000000000000000
                   10  lsubd_sd        0.980727467292130
                   10  lsub_d75        2.102056192575180
                   10  lsub_d50        1.505149978319900
                   10  lsub_d25        1.505149978319900
                   10  lsub_iqr        0.596906214255280
                   10  lsub_d16        0.752578501556020
                   10  lsub_d84        2.102056192575180
                   10  n_noR        102.000000000000000
                   10  lsub_dmm_noR        1.592754344507460
                   10  sub_dmm_noR        186.399376568627000
                   10  lsubd_sd_noR        0.943544254418900
                   10  subd_sd_noR        329.614937585140000
                   10  lsub2dmm        1.634289424450710
                   10  lsubd2sd        0.986987091432880
                   10  lsub2d75        2.102056192575180
                   10  lsub2d50        1.505149978319900
                   10  lsub2d25        1.505149978319900
                   10  lsub2iqr        0.596906214255280
                   10  lsub2d16        0.752578501556020
                   10  lsub2d84        2.102056192575180
                   10  lsub2dmm_noR        1.592754344507460
                   10  sub2dmm_noR        220.713102058823000
                   10  lsubd2sd_noR        0.950177255901020
                   10  subd2sd_noR        500.478513992762000
                   10  lsub2d16INoR        0.737797153009170
                   10  lsub2d50INoR        1.689112753447890
                   10  lsub2d84INoR        2.352177232656150
                   10  dgm        39.152035384187300
                   10  d16        5.467605264623430
                   10  d50        48.877924189268500
                   10  d84        224.997261719329000
                   11  lsub_dmm         0.4554495114
                   11  PCT_GF         5.7142857143
                   11  PCT_RS         10.476190476
                   11  PCT_OT         0.9523809524
                   11  PCT_FN         31.428571429
                   11  PCT_OM         0
                   11  PCT_SA         6.6666666667
                   11  PCT_RR         0
                   11  PCT_GC         28.571428571
                   11  PCT_CB         7.619047619
                   11  PCT_WD         5.7142857143
                   11  PCT_HP         0.9523809524
                   11  PCT_BL         1.9047619048
                   11  PCT_RC         0
                   11  PCT_SB         1.9047619048
                   11  PCT_XB         0
                   11  pct_safn         38.095238095
                   11  pct_sfgf         43.80952381
                   11  pct_bigr         48.571428571
                   11  pct_bdrk         10.476190476
                   11  pct_org         5.7142857143
                   11  n         98
                   11  lsubd_sd         2.0855340858
                   11  lsub_d75         1.5051499783
                   11  lsub_d50         1.5051499783
                   11  lsub_d25         -2.110922507
                   11  lsub_iqr         3.6160724857
                   11  lsub_d16         -2.110922507
                   11  lsub_d84         2.1020561926
                   11  n_noR         86
                   11  lsub_dmm_noR         -0.004614999
                   11  sub_dmm_noR         46.610952186
                   11  lsubd_sd_noR         1.7929320898
                   11  subd_sd_noR         152.31811339
                   11  lsub2dmm         0.4493060421
                   11  lsubd2sd         2.0783877582
                   11  lsub2d75         1.5051499783
                   11  lsub2d50         1.5051499783
                   11  lsub2d25         -2.110922507
                   11  lsub2iqr         3.6160724857
                   11  lsub2d16         -2.110922507
                   11  lsub2d84         2.1020561926
                   11  lsub2dmm_noR         -0.011615696
                   11  sub2dmm_noR         34.983045209
                   11  lsubd2sd_noR         1.7816072453
                   11  subd2sd_noR         80.707361644
                   11  lsub2d16INoR         -2.258564812
                   11  lsub2d50INoR         0.7525749892
                   11  lsub2d84INoR         1.7307217884
                   11  dgm         0.9736083807
                   11  d16         0.0055135991
                   11  d50         5.6568542495
                   11  d84         53.792507416

                    "
                  )
  metstest <- read.table(bob, header=TRUE, stringsAsFactors=FALSE)
  close(bob)
  metstest$METRIC <- tolower(metstest$METRIC)
  
  return(metstest)
}


metsSubstrateCharacterization.fred <- function()
# Code snippet of unknown value
{
fred <- textConnection("METRIC
 d16
 d50
 d84
 dgm
 lsub2d16
 lsub2d16inor
 lsub2d25
 lsub2d50
 lsub2d50inor
 lsub2d75
 lsub2d84
 lsub2d84inor
 lsub2dmm
 lsub2dmm_nor
 lsub2iqr
 lsubd2sd
 lsubd2sd_nor
 lsubd_sd
 lsubd_sd_nor
 lsub_d16
 lsub_d25
 lsub_d50
 lsub_d75
 lsub_d84
 lsub_dmm
 lsub_dmm_nor
 lsub_iqr
 n
 n_nor
 pct_bdrk
 pct_bigr
 pct_bl
 pct_cb
 pct_fn
 pct_gc
 pct_gf
 pct_hp
 pct_org
 pct_om
 pct_ot
 pct_rc
 pct_rr
 pct_rs
 pct_sa
 pct_safn
 pct_sb
 pct_sfgf
 pct_wd
 pct_xb
 sub2dmm_nor
 subd2sd_nor
 subd_sd_nor
 sub_dmm_nor
 d16
 d50
 d84
 dgm
 ldcbf_g08
 lsub2d16inor
 lsub2d50inor
 lsub2d84inor
 lsubd_sd
 lsub_d16
 lsub_d25
 lsub_d50
 lsub_d75
 lsub_d84
 lsub_dmm
 lsub_iqr
 n
 pct_bh
 pct_bl
 pct_cb
 pct_dbbl
 pct_dbcb
 pct_dbfn
 pct_dbgc
 pct_dbgf
 pct_dbhp
 pct_dbom
 pct_dbot
 pct_dbrc
 pct_dbrr
 pct_dbrs
 pct_dbsa
 pct_dbsb
 pct_dbwd
 pct_dbxb
 pct_dsbl
 pct_dscb
 pct_dsfn
 pct_dsgc
 pct_dsgf
 pct_dshp
 pct_dsom
 pct_dsot
 pct_dsrc
 pct_dsrr
 pct_dsrs
 pct_dssa
 pct_dssb
 pct_dswd
 pct_dsxb
 pct_fn
 pct_gr
 pct_ot
 pct_sa
 pct_safn
 pct_sbbl
 pct_sbcb
 pct_sbfn
 pct_sbgc
 pct_sbgf
 pct_sbhp
 pct_sbom
 pct_sbot
 pct_sbrc
 pct_sbrr
 pct_sbrs
 pct_sbsa
 pct_sbsb
 pct_sbwd
 pct_sbxb
 pct_ssbl
 pct_sscb
 pct_ssfn
 pct_ssgc
 pct_ssgf
 pct_sshp
 pct_ssom
 pct_ssot
 pct_ssrc
 pct_ssrr
 pct_ssrs
 pct_sssa
 pct_sssb
 pct_sswd
 pct_ssxb
" )               

  metrics <- read.table(fred, header=TRUE,stringsAsFactors=FALSE)
 close(fred)

}

# end of file