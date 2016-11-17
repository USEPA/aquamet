#' @export
#' @title Calculate NRSA Substrate Characterization Metrics
#' @description This function calculates the substrate characterization
#' portion of the physical habitat metrics for National Rivers and 
#' Streams Assessment (NRSA) data.  The function requires data frames 
#' containing the channel crosssection, thalweg, and littoral data files.
#' @param bBottomDom A data frame containing size class data for the dominant 
#' substrate class in the channel bottom, with the following columns:
#' \itemize{
#'        \item SITE integer or character specifying the site 
#'                   visit
#'        \item TRANSECT character value specifying the transect
#'                       for which the value was recorded.
#'        \item VALUE character values
#' }
#' @param bBottomSec A data frame containing size class data for the 
#' secondary substrate class in the channel bottom, with the following 
#' columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param bShoreDom A data frame containing size class data for 
#' the dominant substrate class at the channel shore, with the 
#' following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param bShoreSec A data frame containing size class data for the 
#' secondary substrate class at the channel show, with the following 
#' columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param bSizeClass A data frame containing size class data collected 
#' in the thalweg of boatable channels with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param wSizeClass A data frame containing size class data collected 
#' at transects of wadeable channels with the following columns:
#' \itemize{
#'        \item SITE integer or character specifying the site 
#'                   visit
#'        \item VALUE numeric or character values
#' }
#' @param wMezzoSizeClass A data frame containing size class data 
#' collected at mezzo-transects of wadeable channels with the following 
#' columns:
#' \itemize{
#'        \item SITE integer or character specifying the site 
#'                   visit
#'        \item VALUE numeric or character values
#' }
#'
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' } 
#' Wadeable metrics calculated include: 
#' d16, d50, d84, dgm, lsub2d16, lsub2d16inor, lsub2d25, lsub2d50,
#'   lsub2d50inor, lsub2d75, lsub2d84, lsub2d84inor, lsub2dmm, lsub2dmm_nor,
#'   lsub2iqr, lsubd2sd,lsubd2sd_nor, lsubd_sd, lsubd_sd_nor, lsub_d16, lsub_d25,
#'   lsub_d50, lsub_d75, lsub_d84, lsub_dmm, lsub_dmm_nor, lsub_iqr, n, n_nor,
#'   pct_bdrk, pct_bigr, pct_bl, pct_cb, pct_fn, pct_gc, pct_gf, pct_hp, pct_org,
#'   pct_om, pct_ot, pct_rc, pct_rr, pct_rs, pct_sa, pct_safn, pct_sb, pct_sfgf,
#'   pct_wd, pct_xb, sub2dmm_nor, subd2sd_nor, subd_sd_nor, sub_dmm_nor
#'   
#' Boatable metrics calculated include:
#' d16, d50, d84, dgm, LDCBF_G08, lsub2d16inor, lsub2d50inor, lsub2d84inor,
#'   lsubd_sd, lsub_d16, lsub_d25, lsub_d50, lsub_d75, lsub_d84, lsub_dmm,
#'   lsub_iqr, n,pct_bh, pct_bl, pct_cb, pct_dbbl, pct_dbcb, pct_dbfn, pct_dbgc,
#'   pct_dbgf, pct_dbhp, pct_dbom, pct_dbot, pct_dbrc, pct_dbrr, pct_dbrs,
#'   pct_dbsa, pct_dbsb, pct_dbwd, pct_dbxb, pct_dsbl, pct_dscb, pct_dsfn,
#'   pct_dsgc, pct_dsgf, pct_dshp, pct_dsom, pct_dsot, pct_dsrc, pct_dsrr,
#'   pct_dsrs, pct_dssa, pct_dssb, pct_dswd, pct_dsxb, pct_fn, pct_gr, pct_ot,
#'   pct_sa, pct_safn, pct_sbbl, pct_sbcb, pct_sbfn, pct_sbgc, pct_sbgf,
#'   pct_sbhp, pct_sbom, pct_sbot, pct_sbrc, pct_sbrr, pct_sbrs, pct_sbsa,
#'   pct_sbsb, pct_sbwd, pct_sbxb, pct_ssbl, pct_sscb, pct_ssfn, pct_ssgc,
#'   pct_ssgf, pct_sshp, pct_ssom, pct_ssot, pct_ssrc, pct_ssrr, pct_ssrs,
#'   pct_sssa, pct_sssb, pct_sswd, pct_ssxb,
#' 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}


nrsaSubstrateCharacterization <- function(bBottomDom=NULL
                                         ,bBottomSec=NULL
                                         ,bShoreDom=NULL
                                         ,bShoreSec=NULL
                                         ,bSizeClass=NULL
                                         ,wSizeClass=NULL
                                         ,wMezzoSizeClass=NULL) {

# TODO: add arguments bSizeClassInfo and wSizeClassInfo to specify size class
#       codes, characteristic diameters, and related information.
    
################################################################################
# Function: nrsaSubstrateCharacterization
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
#   10/21/15 cws Modified calling interface of metsBankMorphology for general use.
#    1/28/16 cws Removed useless PARAMETER column from boatable substrate count
#            section.  Updated absentAsNULL and using ifdf* functions to enforce
#            data standards.
#    3/16/16 cws Documenting arguments in comments at top. Removing old commented-out
#            code throughout.
#
# Arguments:
# bBottomDom    dataframe containing size class data for the dominant 
#               substrate class in the channel bottom, with the following 
#               columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       character values
#
# bBottomSec    dataframe containing size class data for the secondary 
#               substrate class in the channel bottom, with the following 
#               columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       numeric or character values
#
# bShoreDom     dataframe containing size class data for the dominant 
#               substrate class at the channel shore, with the following 
#               columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       numeric or character values
#
# bShoreSec     dataframe containing size class data for the secondary 
#               substrate class at the channel show, with the following 
#               columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       numeric or character values
#
# bSizeClass    dataframe containing size class data collected in the thalweg
#               of boatable channels with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       numeric or character values
#
# wSizeClass    dataframe containing size class data collected at transects
#               of wadeable channels with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           VALUE       numeric or character values
#
# wMezzoSizeClass   dataframe containing size class data collected at 
#                   mezzo-transects of wadeable channels with the following 
#                   columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           VALUE       numeric or character values
#
#
#   Note that possible values for variables in the input data frame are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################
    
    intermediateMessage ('Substrate Characterization', loc='start')
    
    # Standardize data arguments with no data as NULL
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdfLittoral <- function(df, ...) {
        rc <- df %>% 
              select(SITE, TRANSECT, VALUE)
        return(rc)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% 
              select(SITE, VALUE) 
        return(rc)
    }
    bBottomDom <- absentAsNULL(bBottomDom, ifdfLittoral)
    bBottomSec <- absentAsNULL(bBottomSec, ifdfLittoral)
    bShoreDom <- absentAsNULL(bShoreDom, ifdfLittoral)
    bShoreSec <- absentAsNULL(bShoreSec, ifdfLittoral)
    bSizeClass <- absentAsNULL(bSizeClass, ifdfLittoral)
    wSizeClass <- absentAsNULL(wSizeClass, ifdf)
    wMezzoSizeClass <- absentAsNULL(wMezzoSizeClass, ifdf)


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
  
  wadeableSubstrate <- try(rbind(wSizeClass, wMezzoSizeClass))
  if(class(wadeableSubstrate) == 'try-error') {
      intermediateMessage(sprintf('.  Unable to combine wSizeClass and wMezzoSizeClass', wadeableSubstrate))
  } else if (is.null(wadeableSubstrate)) {
      intermediateMessage('.  Combination of wSizeClass and wXSizeClass is NULL')
  } else if (nrow(wadeableSubstrate) == 0) {
      intermediateMessage('.  Combination of wSizeClass and wXSizeClass has zero rows')      
  } else {
      intermediateMessage('.  Wadeable substrate begun')      

      # for each of these subsets of data above, we want summaries (lDiam) for all
      # classes with numeric values 16, 25, 50, 75, 84, mean, std, iqr
      ldBugmm <- merge(wadeableSubstrate
                      ,subset(subsInfo
                             ,class %in% wadeableAllTwoBoulderClasses
                             ,select=c(class,lDiam)
                             ) #diametersmm
                      ,by.x='VALUE', by.y='class'
                      ,all.x=TRUE)
      ldBug2mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                           ,list('SITE'=ldBugmm$SITE)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2mm$METRIC <- 'lsub2d16'

      ldBug4mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                           ,list('SITE'=ldBugmm$SITE)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4mm$METRIC <- 'lsub2d25'

      ldBug6mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                           ,list('SITE'=ldBugmm$SITE)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug6mm$METRIC <- 'lsub2d50'

      ldBug8mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                           ,list('SITE'=ldBugmm$SITE)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8mm$METRIC <- 'lsub2d75'

      ldBug10mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                           ,list('SITE'=ldBugmm$SITE)
                           ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug10mm$METRIC <- 'lsub2d84'

      # additional summaries
      ldBug12mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                            ,list('SITE'=ldBugmm$SITE)
                            ,mean, na.rm=TRUE
                            )
      ldBug12mm$METRIC <- 'lsub2dmm'
      ldBug14mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                            ,list('SITE'=ldBugmm$SITE)
                            ,sd, na.rm=TRUE
                            )
      ldBug14mm$METRIC <- 'lsubd2sd'
      ldBug16mm <- aggregate(list(VALUE = ldBugmm$lDiam)
                            ,list('SITE'=ldBugmm$SITE)
                            ,iqr
                            )
      ldBug16mm$METRIC <- 'lsub2iqr'

      # Completed size classes summaries for streams (ldiam)
      intermediateMessage('.2')


      # summaries for the tt dataset (NOR) (ldiam AND diam)
      # USE wadeableMobileTwoBoulderClasses
      ldBugtt <- merge(wadeableSubstrate
                      ,subset(subsInfo
                             ,class %in% wadeableMobileTwoBoulderClasses
                             ,select=c(class,diam,lDiam)
                             ) # diameterstt
                      ,by.x='VALUE', by.y='class'
                      ,all.x=TRUE)
      ldBug12tt <- aggregate(list(lsub2dmm_nor = ldBugtt$lDiam)
                            ,list('SITE'=ldBugtt$SITE)
                            ,mean, na.rm=TRUE
                            )
      intermediateMessage('.3')
    
      # special extra calculation for DGM
      ldBug12tt$dgm <- 10^ldBug12tt$lsub2dmm_nor

      ldBug12tt <- reshape(ldBug12tt, idvar=c('SITE'), direction='long'
                          ,varying=names(ldBug12tt)[names(ldBug12tt) != 'SITE']
                          ,times=names(ldBug12tt)[names(ldBug12tt) != 'SITE']
                          ,v.names='VALUE', timevar='METRIC'
                          )
      row.names(ldBug12tt)<-NULL


      ldBug14tt <- aggregate(list(VALUE = ldBugtt$lDiam)
                            ,list('SITE'=ldBugtt$SITE)
                            ,sd, na.rm=TRUE
                            )
      ldBug14tt$METRIC <- 'lsubd2sd_nor'

      dBug12tt <- aggregate(list(VALUE = ldBugtt$diam)
                           ,list('SITE'=ldBugtt$SITE)
                           ,mean, na.rm=TRUE
                           )
      dBug12tt$METRIC <- 'sub2dmm_nor'

      dBug14tt <- aggregate(list(VALUE = ldBugtt$diam)
                           ,list('SITE'=ldBugtt$SITE)
                           ,sd, na.rm=TRUE
                           )
      dBug14tt$METRIC <- 'subd2sd_nor'

      intermediateMessage('.4')

      streamld <- rbind ( ldBug2mm, ldBug4mm, ldBug6mm, ldBug8mm, ldBug10mm
                      ,ldBug12mm, ldBug14mm, ldBug16mm,ldBug12tt, ldBug14tt
                      ,dBug12tt, dBug14tt)


      #interpolated metrics
      # USE wadeableMeasurableTwoBoulderClasses
      interpdata <- subset (wadeableSubstrate, VALUE %in% wadeableMeasurableTwoBoulderClasses)#c('XB','SB','CB','GC','GF','SA','FN'))
      measurable <- subset(subsInfo
                          ,class %in% wadeableMeasurableTwoBoulderClasses
                          ,select=c(class,lmin,lmax)
                          ) %>%
                    dplyr::rename(CLASS=class, min=lmin, max=lmax)

      c16 <- interpolatePercentile(interpdata, 'VALUE', 16, 'lsub2d16inor'
                                  ,measurable
                                  )
      c50 <- interpolatePercentile(interpdata, 'VALUE', 50, 'lsub2d50inor', measurable)
      c84 <- interpolatePercentile(interpdata, 'VALUE', 84, 'lsub2d84inor', measurable)

      c16$d16 <- 10^(c16$lsub2d16inor)
      c50$d50 <- 10^(c50$lsub2d50inor)
      c84$d84 <- 10^(c84$lsub2d84inor)

      calcs <- merge(c16
                    ,merge(c50, c84, by='SITE', all=TRUE)
                    ,by='SITE'
                    ,all=TRUE
                    )
      calcs <- reshape(calcs, idvar=c('SITE'), direction='long'
                      ,varying=names(calcs)[names(calcs) != 'SITE']
                      ,times=names(calcs)[names(calcs) != 'SITE']
                      ,v.names='VALUE', timevar='METRIC'
                      )
      row.names(calcs)<-NULL

      # Completed size classes summaries for streams (ldiam- NOR)
      intermediateMessage('.5')


      # all the same metrics for the subset with the lumped boulder classes
      wadeableLumpedBoulders <- within(wadeableSubstrate
                                      ,VALUE <- ifelse (VALUE %in% c('XB', 'SB'), 'BL', VALUE)
                                      )

      ldBuglb <- merge(wadeableLumpedBoulders
                      ,subset(subsInfo
                             ,class %in% wadeableAllOneBoulderClass
                             ,select=c(class,diam,lDiam)
                             ) # diameterslb
                      ,by.x='VALUE', by.y='class'
                      ,all.x=TRUE)

      ldBug2lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                           ,list('SITE'=ldBuglb$SITE)
                           ,quantile, 0.16, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug2lb$METRIC <- 'lsub_d16'


      ldBug4lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                           ,list('SITE'=ldBuglb$SITE)
                           ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug4lb$METRIC <- 'lsub_d25'

      ldBug6lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                           ,list('SITE'=ldBuglb$SITE)
                           ,quantile, 0.50, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug6lb$METRIC <- 'lsub_d50'

      ldBug8lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                           ,list('SITE'=ldBuglb$SITE)
                           ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
                           )
      ldBug8lb$METRIC <- 'lsub_d75'

      ldBug10lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                            ,list('SITE'=ldBuglb$SITE)
                            ,quantile, 0.84, na.rm=TRUE, names=FALSE, type=2
                            )
      ldBug10lb$METRIC <- 'lsub_d84'

      intermediateMessage('.6')

      # additional summaries
      ldBug12lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                            ,list('SITE'=ldBuglb$SITE)
                            ,mean, na.rm=TRUE
                            )
      ldBug12lb$METRIC <- 'lsub_dmm'

      ldBug14lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                            ,list('SITE'=ldBuglb$SITE)
                            ,sd, na.rm=TRUE
                            )
      ldBug14lb$METRIC <- 'lsubd_sd'

      ldBug16lb <- aggregate(list(VALUE = ldBuglb$lDiam)
                            ,list('SITE'=ldBuglb$SITE)
                            ,iqr
                            )
      ldBug16lb$METRIC <- 'lsub_iqr'

      # Complete size classes summaries for streams (lumped boulder class)
      intermediateMessage('.6')


      # special few extra summaries that use the lumped boulder class for the NOR
      df1ttlb <- wadeableSubstrate
      df1ttlb$VALUE <- ifelse (df1ttlb$VALUE %in% c('XB', 'SB'),'BL', df1ttlb$VALUE)

      ldBugttbl <- merge(df1ttlb
                        ,subset(subsInfo
                               ,class %in% wadeableMobileOneBoulderClass
                               ,select=c(class,diam,lDiam)
                               ) # diametersttbl
                        ,by.x='VALUE', by.y='class'
                        ,all.x=TRUE)
      ldBug12ttbl <- aggregate(list(VALUE = ldBugttbl$lDiam)
                              ,list('SITE'=ldBugttbl$SITE)
                              ,mean, na.rm=TRUE
                              )
      ldBug12ttbl$METRIC <- 'lsub_dmm_nor'

      ldBug14ttbl <- aggregate(list(VALUE = ldBugttbl$lDiam)
                              ,list('SITE'=ldBugttbl$SITE)
                              ,sd, na.rm=TRUE
                              )
      ldBug14ttbl$METRIC <- 'lsubd_sd_nor'

      dBug12ttbl <- aggregate(list(VALUE = ldBugttbl$diam)
                             ,list('SITE'=ldBugttbl$SITE)
                             ,mean, na.rm=TRUE
                             )
      dBug12ttbl$METRIC <- 'sub_dmm_nor'

      dBug14ttbl <- aggregate(list(VALUE = ldBugttbl$diam)
                             ,list('SITE'=ldBugttbl$SITE)
                             ,sd, na.rm=TRUE
                             )
      dBug14ttbl$METRIC <- 'subd_sd_nor'

      # Complete size classes summaries for streams (lumped boulder class- NOR).6
      intermediateMessage('.7')

      streamlb <- rbind ( ldBug2lb, ldBug4lb, ldBug6lb, ldBug8lb, ldBug10lb
                        ,ldBug12lb, ldBug14lb, ldBug16lb, ldBug12ttbl, ldBug14ttbl
                        ,dBug12ttbl, dBug14ttbl
                        )

intermediateMessage('a')

      # moving on to counts and percentages for these, want to count SIZE_CLS/XSIZE_CLS THE SAME,
      # there are three 'n' values associated with these counts
      # realallsize.... every size class
      # allsize ....... every size class with a numeric value
      # norsize........ all the mobile size classes with characteristic diameters
      realallsize <- wadeableSubstrate
      realallsize$PARAMETER <- NULL
      allsize <- subset (realallsize, VALUE %in% wadeableNumericTwoBoulderClasses)
      norsize <- subset (realallsize, VALUE %in% wadeableMeasurableTwoBoulderClasses)
intermediateMessage('b')

      allSZ <- ddply(allsize,c('SITE'),summarise,METRIC='n',VALUE=length(na.omit(VALUE)))
      allSZ2 <- ddply(realallsize,c('SITE'),summarise,METRIC='n2',VALUE=length(na.omit(VALUE)))
      allNOR <- ddply(norsize,c('SITE'),summarise,METRIC='n_nor',VALUE=length(na.omit(VALUE)))
intermediateMessage('c')

      # get counts for each size class
      realallsize <- mutate(realallsize,METRIC=paste('n',VALUE,sep=''))
      allSZ.size <- ddply(subset(realallsize,VALUE!='BL'),c('SITE','METRIC'),summarise,VALUE=length(na.omit(VALUE))) 
      allSZBL <- ddply(subset(realallsize,VALUE %in% c('XB','SB')),c('SITE'),summarise,METRIC='nBL',VALUE=length(na.omit(VALUE)))
      allSZ.comb <- rbind(allSZ.size,allSZBL)
intermediateMessage('d')

      # Create empty data frame with all expected output metric names to make sure they are included
      empty.df <- expand.grid(SITE=unique(wadeableSubstrate$SITE),METRIC=c('nBL','nCB','nFN','nGC','nGF','nHP','nOT','nOM','nRC','nRR','nRS','nSA','nSB','nWD','nXB'))
      allSZ.comb.1 <- merge(empty.df,allSZ.comb,by=c('SITE','METRIC'),all.x=TRUE)
      allSZ.comb.1 <- mutate(allSZ.comb.1,VALUE=ifelse(is.na(VALUE),0,VALUE))

      intermediateMessage('e')
  

      one <-   dplyr::rename(allNOR, n_nor = VALUE)
      one$METRIC <- NULL
      two <-   dplyr::rename(allSZ, n = VALUE)
      two$METRIC <- NULL
      three<-   dplyr::rename(allSZ2, n2 = VALUE)
      three$METRIC <- NULL

      intermediateMessage('.8')

      pct0 <- within(merge(merge(one, two, by='SITE', all.x=TRUE, all.y=TRUE)
                          ,three, by='SITE', all.x=TRUE, all.y=TRUE
                          )
                    ,{n <- ifelse(is.na(n), 0, n)
                      n_nor <- ifelse(is.na(n_nor), 0, n_nor)
                      n2 <- ifelse(is.na(n2), 0, n2)
                     }
                    )
      pct1 <- merge(allSZ.comb.1,pct0,by='SITE') 
      pct2 <- mutate(pct1,METRIC=paste('pct_',tolower(substring(METRIC,2,3)),sep=''),VALUE=(VALUE/n2)*100)

      #some groupings
      pct_bigr <- ddply(subset(pct2,METRIC %in% c('pct_rr','pct_rs','pct_rc','pct_bl','pct_cb','pct_gc'))
                        ,c('SITE'),summarise,METRIC='pct_bigr',VALUE=sum(VALUE))
      pct_bdrk <- ddply(subset(pct2,METRIC %in% c('pct_rr','pct_rs')),c('SITE'),summarise
                        ,METRIC='pct_bdrk',VALUE=sum(VALUE))
      pct_safn <- ddply(subset(pct2,METRIC %in% c('pct_sa','pct_fn')),c('SITE'),summarise
                        ,METRIC='pct_safn',VALUE=sum(VALUE))
      pct_sfgf <- ddply(subset(pct2,METRIC %in% c('pct_sa','pct_fn','pct_gf')),c('SITE'),summarise
                        ,METRIC='pct_sfgf',VALUE=sum(VALUE))
      pct_org <- ddply(subset(pct2,METRIC %in% c('pct_om','pct_wd')),c('SITE'),summarise
                       ,METRIC='pct_org',VALUE=sum(VALUE))

      alln <- melt(subset(pct0,select=-n2),id.vars='SITE',variable.name='METRIC',value.name='VALUE')
      pct3 <- rbind(subset(pct2,select=c('SITE','METRIC','VALUE')),pct_bigr,pct_bdrk,pct_safn,pct_sfgf,pct_org,alln)
      # Completed size classes percentages for streams
      intermediateMessage('.9')
      
      mets <- rbind (streamld, streamlb, calcs, pct3) 
      intermediateMessage('.  Wadeable substrate done')      

  } # end of wadeable substrate processing

  
  # on to the rivers
  # input from the boatable site is in the thalweg2 and littoral2
  # df2 contains all the SIZE_CLS responses from boatable
  if(nrow(as.data.frame(bSizeClass)) == 0) {
      intermediateMessage('.  Boatable thalweg substrate has no data')      
  } else {
      intermediateMessage('.  Boatable thalweg substrate begun')      
      
      # SIZE_CLS from the rivers have slightly different gmeans.
      # 16, 25, 50, 75, 84, mean, std, iqr....... for rivers data
      ldRivmm <- merge(bSizeClass
                      ,subset(subsInfo
                             ,class %in% boatableNumericThalwegClasses
                             ,select=c(class,diam,lDiam)
                             )  # diametersrr
                      ,by.x='VALUE', by.y='class'
                      ,all.x=TRUE)

      ldRivCt <- ddply(bSizeClass, c('SITE'), summarise, METRIC='n', VALUE=length(na.omit(VALUE)))

      ldRiv1mm <- ddply(ldRivmm,c('SITE'),summarise,lsub_d16=quantile(lDiam,probs=0.16,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d25=quantile(lDiam,probs=0.25,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d50=quantile(lDiam,probs=0.50,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d75=quantile(lDiam,probs=0.75,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_d84=quantile(lDiam,probs=0.84,names=FALSE,type=2,na.rm=TRUE)
                        ,lsub_dmm=mean(lDiam,na.rm=TRUE),lsubd_sd=sd(lDiam,na.rm=TRUE)
                        ,lsub_iqr=lsub_d75-lsub_d25)
      ldRiv1mm.1 <- melt(ldRiv1mm,id.vars='SITE',variable.name='METRIC',value.name='VALUE',na.rm=FALSE)

      intermediateMessage('.11')

      # put together this summaries
      riv1 <- rbind(ldRivCt, ldRiv1mm.1)
      


      # Initial summaries for rivers.  Get counts for each size class from the
      # back of the thalweg form
      indivcl <- ddply(bSizeClass, c('SITE','VALUE'), summarise, n=length(na.omit(VALUE)))
      allct <- ddply(bSizeClass, c('SITE'), summarise, nAll=length(na.omit(VALUE)))
      scCTS <- merge(indivcl, allct, by=c('SITE'))

      sc <- expand.grid (SITE=unique(scCTS$SITE)
                        ,VALUE=boatableAllThalwegClasses # c('BH', 'BL', 'CB', 'GR', 'SA', 'FN','OT')
                        )
      sc$VALUE <- as.character(sc$VALUE)

      ss3m <- merge(scCTS, sc, by=c('VALUE', 'SITE'), all.y=TRUE)

      ss3m$pct <- (ss3m$n/ss3m$nAll)*100

      ss3m$METRIC <-  paste('pct_', tolower(ss3m$VALUE), sep='')
      ss3m$pct <- ifelse (is.na(ss3m$pct), 0, ss3m$pct)


      # eliminate unesscesary vars to bind with other metric parts.
      ss3m$VALUE <- NULL
      ss3m$PARAMETER <- NULL
      ss3m$n <- NULL
      ss3m$nAll <- NULL

      ss3m <- dplyr::rename(ss3m, VALUE = pct)

      intermediateMessage('.13')

      # Calculate composite metric(s)
      safn <- within(merge(subset(ss3m, METRIC=='pct_sa', select=c(SITE,VALUE))
                          ,subset(ss3m, METRIC=='pct_fn', select=c(SITE,VALUE))
                          ,by='SITE', all=TRUE, suffix=c('.sa', '.fn')
                          )
                    ,VALUE <- VALUE.sa + VALUE.fn
                    )
      safn$METRIC <- 'pct_safn'
      safn <- safn[c('SITE','VALUE','METRIC')]
      
      mets <- rbind(mets, riv1, ss3m, safn)

      # Completed counts (pct) from the thawlweg data
      intermediateMessage('.  Boatable thalweg substrate done')      
  } # end of boatable thalweg substrate processing
  

  # on to the littoral data
    setParameter <- function(df, parName) {
        if(is.null(df)) rc <- NULL
        else if(!is.data.frame(df)) rc <- NULL
        else if(nrow(df) == 0) rc <- NULL
        else {
            rc <- df %>% select(SITE,TRANSECT,VALUE) %>% mutate(PARAMETER=parName)
        }
        
        return(rc)
    }

  littoralSubstrate <- try(rbind(setParameter(bBottomDom, 'BOTTOMDOM')
                                ,setParameter(bBottomSec, 'BOTTOMSEC')
                                ,setParameter(bShoreDom, 'SHOREDOM')
                                ,setParameter(bShoreSec, 'SHORESEC')
                                )
                           )

  if(class(littoralSubstrate) == 'try-error') {
      intermediateMessage(sprintf('.  Unable to combine bBottomDom, bBottomSec, bShoreDom, bShoreSec', as.character(littoralSubstrate)))
  } else if(nrow(as.data.frame(littoralSubstrate)) == 0) {
      intermediateMessage('.  Boatable littoral substrate has no data')      
  } else {
      intermediateMessage('.  Boatable littoral substrate begun')      
      
      indiv <- ddply(littoralSubstrate, c('SITE','PARAMETER','VALUE'), summarise, n=length(na.omit(VALUE)))

      big4 <- ddply(littoralSubstrate,c('SITE','PARAMETER'),summarise,n4=length(na.omit(VALUE)))

      ss4m <- merge(indiv, big4, by=c('SITE', 'PARAMETER'))

      ss <- expand.grid (SITE=unique(ss4m$SITE)
                        ,PARAMETER=c('BOTTOMDOM', 'BOTTOMSEC'
                                    ,'SHOREDOM', 'SHORESEC'
                                    )
                        ,VALUE=boatableLittoralClasses
                        )
      ss$PARAMETER <- as.character(ss$PARAMETER)
      ss$VALUE <- as.character(ss$VALUE)

      ss4m <- merge(ss4m, ss
                   ,by=c('VALUE', 'SITE', 'PARAMETER')
                   ,all.y=TRUE
                   )

      ss4m$pct <- (ss4m$n/ss4m$n4)*100
      ss4m$pct <- ifelse (is.na(ss4m$pct), 0, ss4m$pct)

      ss4m$METRIC <-  paste('pct_', ifelse(ss4m$PARAMETER=='BOTTOMDOM', 'db'
                                   ,ifelse(ss4m$PARAMETER=='BOTTOMSEC', 'sb'
                                   ,ifelse(ss4m$PARAMETER=='SHOREDOM' , 'ds'
                                   ,ifelse(ss4m$PARAMETER=='SHORESEC' , 'ss', 'DAMMIT'
                                   ))))
                           ,tolower(ss4m$VALUE)
                           ,sep=''
                           )

      intermediateMessage('.15')

      # eliminate unnecessary vars to bind with other metric parts.
      ss4m$VALUE <- NULL
      ss4m$PARAMETER <- NULL
      ss4m$n <- NULL
      ss4m$n4 <- NULL

      ss4m <- dplyr::rename(ss4m, VALUE = pct)

      # Completed pct from the littoral data
      intermediateMessage('.  Boatable littoral substrate done')      

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
