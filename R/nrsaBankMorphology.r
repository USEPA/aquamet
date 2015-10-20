nrsaBankMorphology <- function(bAngle=NULL, wAngle=NULL, wUndercut=NULL) {

################################################################################
# Function: nrsaBankMorphology
# Title: Calculate NRSA Bank Morphology Metrics
# Programmers: Marlys Cappert
#              Suzanne San Romani
#              Curt Seeliger
#              Tom Kincaid
# Date: January 21, 2010
# Description:
#   This function calculates the bank morphology portion of the physical habitat
#   metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the bank geometry and stream
#   verification form data files.
# Wadeable Metrics:
#   bka_q1, bka_q3, bkun_q1, bkun_q3, intqbka, intqbkun, medbkun, medbk_a,
#   n_ba, n_un, sdbk_a, sdun, xbka, sdun, sbka, xun
# Boatable Metrics:
#   bangmode, bap_low, bap_med, bap_mis,bap_stp, bap_vst, n_ba
# Function Revisions:
#   01/21/10 mrc: Started.
#   01/25/10 mrc: Completed metrics. Wrote tests.
#   03/16/10 mrc: Replace external text files with openTextConnection
#            instructions.
#   03/17/10 mrc: Replaced upData calls with rename.
#   03/18/10 ssr: Added boatable n_ba to data for export.
#   03/22/10 ssr: Moved creation of unit test dataframes to separate
#            functions.
#   03/25/10 cws: Changed diff() calls to dfCompare().
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   04/05/12 cws: Removed calculation of n_w (only done for for boatables), as 
#            this is done in metsChannelMorphology() for both protocols.  Unit
#            test modified accordingly, and rewritten to eliminate production
#            of NAs by coercion, which had the effect of excluding text metric 
#            bangmode from the test.
#   07/26/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written. In function metsBankMorphologyTest.process, renamed
#            argument by to byVars in the call to dfCompare.
#   09/07/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
# Metrics Revisions:
#   10/25/12 cws River bank angle mets changed.  Calculation of mean, median,
#            and quartile values based on arithmetic midpoints of bins is
#            arguably silly, and are replaced by percent of reach in each
#            category and the distributional mode as described by Zar
#            (2ed), p 23.  This makes the calculations completely different
#            for rivers vs. streams.
#   10/19/15 cws Modified calling interface of metsBankMorphology for general use.
# 
# Arguments:
#   bAngle =    dataframe containing bank angle class value for sites sampled using
#               the boatable protocol, with the columns
#                   SITE    an integer or character value identifying a single site 
#                           visit.
#                   VALUE   a character value containing the bank angle class.
#   wAngle =    dataframe containing bank angle class value for sites sampled using
#               the boatable protocol, with the columns
#                   SITE    an integer or character value identifying a single site 
#                           visit.
#                   VALUE   an integer value, or character value that is castable
#                           to an integer, containing the bank angle.
#   bUndercut = dataframe containing bank angle class value for sites sampled using
#                   SITE    an integer or character value identifying a single site 
#                           visit.
#                   VALUE   an integer value, or character value that is castable
#                           to an integer, containing the bank angle.
#
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
#     SITE - site identifying value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
#   siteProtocol - determine sampling protocol values
#   metsBankMorphology.1 - calculate metrics
################################################################################

# Print an initial message
  intermediateMessage('Bank Morphology calculations:', loc='start')

# # Convert factors to character variables in the input data frames
#   intermediateMessage('.1 Convert factors to character variables.', loc='end')
#   bankgeometry <- convert_to_char(bankgeometry)
#   visits <- convert_to_char(visits)
# 
# # Determine protocol used for each site
#   intermediateMessage('.2 Set protocols.', loc='end')
#   protocols <- siteProtocol(unique(bankgeometry$UID), visits)

# Calculate the metrics
  mets <- nrsaBankMorphology.1(bAngle, wAngle, wUndercut)
  row.names(mets) <- 1:nrow(mets)

# Print an exit message
  intermediateMessage('Done.', loc='end')

# Return results
  return(mets)
}



nrsaBankMorphology.1 <- function(bAngle, wAngle, wUndercut) {
# Returns a dataframe of calculations if successful or a character string 
# describing the problem if one was encountered.
#

    # Standardize data arguments with no data as NULL
    absentAsNULL <- function(df) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else return(df)
    }
    bAngle <- absentAsNULL(bAngle)
    wAngle <- absentAsNULL(wAngle)
    wUndercut <- absentAsNULL(wUndercut)
  
    
    if(is.null(wAngle)) {
        intermediateMessage('. wAngle has no data')
        wAngleMets <- NULL
    } else {
        intermediateMessage('. wAngle begun')
        
        wAngle$VALUE <- as.numeric(wAngle$VALUE)  ## NAs by coersion
        
        ct <- aggregate(wAngle$VALUE
                        ,list('SITE'=wAngle$SITE
                        )
                        ,count
        )
        
        ct$METRIC <- 'n_ba'
        ct <- rename(ct, 'x', 'VALUE')
        
        mn <- aggregate(wAngle$VALUE
                        ,list('SITE'=wAngle$SITE
                        )
                        ,mean , na.rm=TRUE
        )
        mn$METRIC <- 'xbka'
        mn <- rename(mn, 'x', 'VALUE')
        
        stt <- aggregate(wAngle$VALUE
                         ,list('SITE'=wAngle$SITE
                         )
                         ,sd  , na.rm=TRUE
        )
        stt$METRIC <- 'sdbk_a'
        stt <- rename(stt, 'x', 'VALUE')
        
        iqq <- aggregate(wAngle$VALUE
                         ,list('SITE'=wAngle$SITE
                         )
                         ,iqr
        )
        iqq$METRIC <- 'intqbka'
        iqq <- rename(iqq, 'x', 'VALUE')
        
        med <- aggregate(wAngle$VALUE
                         ,list('SITE'=wAngle$SITE
                         )
                         ,median , na.rm=TRUE
        )
        
        med$METRIC <- 'medbk_a'
        med <- rename(med,'x', 'VALUE')
        
        upq1 <- aggregate(wAngle$VALUE
                          ,list ('SITE'=wAngle$SITE
                          )
                          ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
        )
        
        upq1$METRIC <- 'bka_q1'
        upq1 <- rename(upq1, 'x', 'VALUE')
        
        upq3 <- aggregate(wAngle$VALUE
                          ,list ('SITE'=wAngle$SITE)
                          ,quantile, .75, na.rm=TRUE, names=FALSE, type=2
        )
        upq3$METRIC <- 'bka_q3'
        upq3 <- rename(upq3, 'x', 'VALUE')
        
        #put these together
        
        wAngleMets <- rbind (ct, mn, stt, iqq, med, upq1, upq3)
        intermediateMessage('. wAngle done')
    
    }
    
    
    #do this same step for undercut (streams)
    if(is.null(wUndercut)) {
        intermediateMessage('. wUndercut has no data')
        wUndercutMets <- NULL
    } else {
        intermediateMessage('. wUndercut begun')
        wUndercut$VALUE <- as.numeric(wUndercut$VALUE)
        
        ct <- aggregate(wUndercut$VALUE
                        ,list('SITE'=wUndercut$SITE
                        )
                        ,count
        )
        
        ct$METRIC <- 'n_un'
        ct <- rename(ct, 'x', 'VALUE')
        
        mn <- aggregate(wUndercut$VALUE
                        ,list('SITE'=wUndercut$SITE
                        )
                        ,mean , na.rm=TRUE
        )
        mn$METRIC <- 'xun'
        mn <- rename(mn,'x', 'VALUE')
        
        stt <- aggregate(wUndercut$VALUE
                         ,list('SITE'=wUndercut$SITE
                         )
                         ,sd , na.rm=TRUE
        )
        stt$METRIC <- 'sdun'
        stt <- rename(stt, 'x', 'VALUE')
        
        iqq <- aggregate(wUndercut$VALUE
                         ,list('SITE'=wUndercut$SITE
                         )
                         ,iqr
        )
        iqq$METRIC <- 'intqbkun'
        iqq <- rename(iqq, 'x', 'VALUE')
        
        med <- aggregate(wUndercut$VALUE
                         ,list('SITE'=wUndercut$SITE
                         )
                         ,median , na.rm=TRUE
        )
        
        med$METRIC <- 'medbkun'
        med <- rename(med, 'x', 'VALUE')
        
        upq1 <- aggregate(wUndercut$VALUE
                          ,list ('SITE'=wUndercut$SITE
                          )
                          ,quantile, 0.25, na.rm=TRUE, names=FALSE, type=2
        )
        
        upq1$METRIC <- 'bkun_q1'
        upq1 <- rename(upq1, 'x', 'VALUE')
        
        upq3 <- aggregate(wUndercut$VALUE
                          ,list ('SITE'=wUndercut$SITE
                          )
                          ,quantile, 0.75, na.rm=TRUE, names=FALSE, type=2
        )
        upq3$METRIC <- 'bkun_q3'
        upq3 <- rename(upq3, 'x', 'VALUE')
        
        #put these together
        
        wUndercutMets <- rbind (ct, mn, stt, iqq, med, upq1, upq3)
        intermediateMessage('. wUndercut done')
    }


    if(is.null(bAngle)) {
        intermediateMessage('. bAngle has no data')
        bAngleMets <- NULL
    } else {
        intermediateMessage('. bAngle begun')
        ct <- aggregate(bAngle$VALUE
                        ,list('SITE'=bAngle$SITE)
                        ,count
        )
        ct$METRIC <- 'n_ba'
        ct <- rename(ct, 'x', 'n_ba')
        intermediateMessage('.4')
        
        #use the total ang count to get other counts
        
#        bap_low <- subset (mmboat, mmboat$PARAMETER=='ANGLE' )
        bap_low <- aggregate(bAngle$VALUE
                             ,list('SITE'=bAngle$SITE)
                             ,function (VALUE) {sum(VALUE=='0-5')}
        )
        bap_low <- rename(bap_low, 'x', 'low')
        
        
        bb <- merge(ct, bap_low,  by='SITE', all.x=TRUE, all.y=FALSE)
        intermediateMessage('.5')
        
        
        bb$VALUE <- (bb$low/bb$n_ba) * 100
        bb$low <- NULL
        bb$n_ba <- NULL
        bb$METRIC <- NULL
        bb$METRIC <- 'bap_low'
        
#        bap_med <- subset (mmboat, mmboat$PARAMETER=='ANGLE')
        bap_med <- aggregate(bAngle$VALUE
                             ,list('SITE'=bAngle$SITE)
                             ,function (VALUE) {sum(VALUE=='5-30')}
        )
        bap_med <- rename(bap_med, 'x', 'med')
        bb1 <- merge(ct, bap_med,  by='SITE', all.x=TRUE, all.y=FALSE)
        
        intermediateMessage('.6')
        
        if(nrow(bb1)>0) {
            bb1$VALUE <- (bb1$med/bb1$n_ba) *100
            bb1$med <- NULL
            bb1$n_ba <- NULL
            bb1$METRIC <- NULL
            bb1$METRIC <- 'bap_med'
        }
        
        bap_stp <- aggregate(bAngle$VALUE
                             ,list('SITE'=bAngle$SITE)
                             , function (VALUE) {sum(VALUE=='30-75')}
        )
        bap_stp <- rename(bap_stp, 'x', 'stp')
        
        bb2 <- merge(ct, bap_stp,  by='SITE', all.x=TRUE, all.y=FALSE)
        
        if(nrow(bb2)>0) {
            bb2$VALUE <- (bb2$stp/bb2$n_ba) *100
            bb2$stp <- NULL
            bb2$n_ba <- NULL
            bb2$METRIC <- NULL
            bb2$METRIC <- 'bap_stp'
        }
        
        bap_vst <- aggregate(bAngle$VALUE
                             ,list('SITE'=bAngle$SITE)
                             ,function (VALUE) {sum(VALUE=='75-100')}
        )
        bap_vst<- rename(bap_vst, 'x', 'vst')
        bb3 <- merge(ct, bap_vst,  by='SITE', all.x=TRUE, all.y=FALSE)
        
        intermediateMessage('.7')
        
        if(nrow(bb3)>0) {
            bb3$VALUE <- (bb3$vst/bb3$n_ba) *100
            bb3$vst <- NULL
            bb3$n_ba <- NULL
            bb3$METRIC <- NULL
            bb3$METRIC <- 'bap_vst'
        }
        
        # Configuring ct for rbind
        ct <- rename(ct,'n_ba','VALUE')
        ct <- subset(ct, select=c('SITE','METRIC','VALUE'))
        
        intermediateMessage('.8')
        
        boat <- rbind (bb, bb1,bb2,bb3,ct)
        
        intermediateMessage('.9')
        
        # work on the BANGMODE code for rivers.  First ensure that illegal
        # values are not present, as they'll mess up the results.
        bAngle <- subset(bAngle
                       ,PARAMETER=='ANGLE' &
                           VALUE %in% c('0-5','5-30','30-75','75-100')
        )
        
        tt <- aggregate(bAngle$VALUE=='0-5'
                        ,list('SITE'=bAngle$SITE)
                        ,mean, na.rm=TRUE
        )
        lowbap <- rename(tt, 'x', 'xlow')
        
        tt <- aggregate(bAngle$VALUE=='5-30'
                        ,list('SITE'=bAngle$SITE)
                        ,mean, na.rm=TRUE
        )
        medbap <- rename(tt, 'x', 'xmed')
        
        tt <- aggregate(bAngle$VALUE=='30-75'
                        ,list('SITE'=bAngle$SITE)
                        ,mean, na.rm=TRUE
        )
        stpbap <- rename(tt, 'x', 'xstp')
        
        tt <- aggregate(bAngle$VALUE=='75-100'
                        ,list('SITE'=bAngle$SITE)
                        ,mean, na.rm=TRUE
        )
        vstbap <- rename(tt, 'x', 'xvst')
        
        
        intermediateMessage('.10')
        
        # Determine color mode (most common bank angle)
        fracbangmode<-merge(lowbap, medbap
                            ,by='SITE'
                            ,all=TRUE
                            ,sort=FALSE
        )
        fracbangmode<-merge(fracbangmode, stpbap
                            ,by='SITE'
                            ,all=TRUE
                            ,sort=FALSE
        )
        fracbangmode<-merge(fracbangmode, vstbap
                            ,by='SITE'
                            ,all=TRUE
                            ,sort=FALSE
        )
        
        intermediateMessage('.11')
        
        
        modebang <- subset(fracbangmode, select='SITE')
        modebang$bsobang <- NA
        for(i in 1:nrow(modebang)) {
            modebang$bsobang[i] <- modalClass(fracbangmode[i,]
                                              ,c('xlow','xmed','xstp','xvst'
                                              )
                                              ,c('0-5','5-30','30-75','75-100'
                                              )
            )
        }
        
        intermediateMessage('.12')
        
        modebang$METRIC <- 'bangmode'
        modebang <- rename(modebang, 'bsobang', 'VALUE')
        
        modebang$VALUE <- ifelse(modebang$VALUE =='0-5', 'low', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='5-30', 'med', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='30-75', 'stp', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='75-100', 'vst', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='0-5, 5-30', 'low-med', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='5-30, 30-75', 'med-stp', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE =='30-75, 75-100', 'stp-vst', modebang$VALUE)
        modebang$VALUE <- ifelse(modebang$VALUE %in% c('low', 'med','stp', 'vst'
                                                         ,'low-med','med-stp', 'stp-vst'
        )
        ,modebang$VALUE, 'None'
        )
        
        bAngleMets <- rbind (modebang, boat)
        intermediateMessage('. bAngle done')
        
  }

  metsfakeo <- rbind (bAngleMets, wAngleMets, wUndercutMets)
 
  intermediateMessage('  Done.', loc='end')
  return(metsfakeo)

}



# end of file
