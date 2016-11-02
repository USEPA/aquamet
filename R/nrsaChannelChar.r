#' @export
#' @title Calculate NRSA Channel Characteristic Metrics
#' @description This function calculates the channel characteristics 
#' portion of the physical habitat metrics for National Rivers and 
#' Streams Assessment (NRSA) data.  The function requires data frames 
#' containing the bank geometry and channel characteristics data files.
#' @param bankfullWidth  A data frame containing bankfull width from 
#' channel constraint form for all reaches, with the following columns:
#' \itemize{
#'      \item SITE        integer or character specifying the site visit
#'      \item VALUE       numeric or character values
#' }
#' Note: This value is simply copied to the output without change
#' @param channelPattern  A data frame containing pattern type from 
#' channel constraint form for all reaches, with the following columns:
#' \itemize{
#'      \item SITE        integer or character specifying the site visit
#'      \item VALUE       character values
#' }
#'  Note: This value is simply copied to the output without change
#' @param constraintFeatures A data frame containing constraint features from 
#' channel constraint form for all reaches, with the following columns:
#' \itemize{
#'      \item SITE        integer or character specifying the site visit
#'      \item VALUE       character values
#' }
#' Note: This value is simply copied to the output without change
#' @param constraintMultiple A data frame containing constraining features from 
#' channel riparian forms for boatable reaches, with the following columns:
#' \itemize{
#'     \item SITE        integer or character specifying the site visit
#'     \item VALUE       character values expected to be one of 'B',
#'  }                                 'C','N','U' or NA
#'
#' @param constraintSingle  A data frame containing constraint features from 
#' channel constraint form for all reaches, with the following columns:
#' \itemize{
#'     \item SITE        integer or character specifying the site visit
#'     \item VALUE       numeric values
#'     }
#'  Note: This value is simply copied to the output without change
#' 
#' @param constraintPercent A data frame containing percent constraint 
#' estimates from channel constraint form for allreaches, with the following 
#' columns:
#' \itemize{
#'     \item SITE        integer or character specifying the site visit
#'     \item VALUE       character or numeric values
#'     }
#'  Note: This value is simply copied to the output without change
#'
#' @param seeOverBank A data frame containing variable indicating
#' whether crew could see over the bank (N/Y) from channel 
#' riparian forms for boatable reaches, with the following columns:
#' \itemize{
#'     \item SITE        integer or character specifying the site visit
#'     \item VALUE       character values = 'YES' if true, other value
#'                        if not true
#'  }
#'
#' @param shoreToVegDistance A data frame containing distance (in meters) 
#' from shore to vegetation from channel riparian forms for boatable reaches,
#' with the following columns:
#' \itemize{
#'     \item SITE        integer or character specifying the site visit
#'     \item VALUE       numeric values
#'    }
#'
#' @param valleyConstraintUnseen A data frame containing canopy densiometer 
#' from channel constraint form for all reaches, with the following
#' columns:
#' \itemize{
#'     \item SITE    integer or character specifying the site visit 
#'     \item VALUE   character values
#'  }
#'  Note: This value is simply copied to the output without change
#'
#' @param valleyWidth A data frame containing canopy densiometer from channel 
#' constraint form for all reaches, with the following columns:
#' \itemize{
#'     \item SITE  integer or character specifying the site visit
#'     \item VALUE character or numeric values 
#'  }
#'  Note: This value is simply copied to the output without change.
#' @return Either a data frame when metric calculation is successful or a 
#' NULL when metric calculation is not successful.  The data frame contains 
#' the following columns:
#'  \itemize{
#'  \item SITE universal ID value
#'  \item METRIC metric name
#'  \item VALUE metric value
#'  }
#'  
#' Metrics calculated include: xshor2vg, mxshor, mnshor, pct_ovrb, 
#' pctch_b, pctch_c, pctch_n, pctch_u, conbankfull, confeatures, 
#' conpattern, conpercent, constraint, convalley, convalleybox
#'  
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#' head(chancharEx)
#' 
#' bkfW <- subset(chancharEx, PARAMETER=='BANKFULL')
#' chanPat <- subset(chancharEx, PARAMETER=='PATTERN')
#' conFeat <- subset(chancharEx, PARAMETER=='FEATURES')
#' conMult <- subset(bankgeomEx, PARAMETER=='CONSTRT')
#' conSing <- subset(chancharEx, PARAMETER=='CONSTRNT')
#' conPer <- subset(chancharEx, PARAMETER=='PERCENT')
#' seeOver <- subset(bankgeomEx, PARAMETER=='SEEOVRBK')
#' shoreToVeg <- subset(chancharEx, PARAMETER=='SHOR2RIP')
#' valCon <- subset(chancharEx, PARAMETER=='VALLYBOX')
#' valWid <- subset(chancharEx, PARAMETER=='VALLEY')
#' 
#' chanCharOut <- nrsaChannelChar(bankfullWidth=bkfW, 
#' channelPattern=chanPat, constraintFeatures=conFeat,
#' constraintMultiple=conMult, constraintSingle=conSing,
#' constraintPercent=conPer, seeOverBank=seeOver,
#' shoreToVegDistance=shoreToVeg, valleyConstraintUnseen=valCon,
#' valleyWidth=valWid)
#' 
#' head(chanCharOut)
#' 

nrsaChannelChar <- function(bankfullWidth = NULL            # both, on channel constraint form
                           ,channelPattern = NULL           # both, on channel constraint form
                           ,constraintFeatures = NULL       # both, on channel constraint form
                           ,constraintMultiple = NULL       # boatable only 
                           ,constraintSingle = NULL         # both, on channel constraint form 
                           ,constraintPercent = NULL        # both, on channel constraint form
                           ,seeOverBank = NULL              # boatable only
                           ,shoreToVegDistance = NULL       # boatable only
                           ,valleyConstraintUnseen = NULL   # both, on channel constraint form
                           ,valleyWidth = NULL              # both, on channel constraint form
                           ) {
  
################################################################################
# Function: nrsaChannelChar
# Title: Calculate NRSA Channel Characteristic Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Suzanne San Romani
#              Tom Kincaid
# Date: January 27, 2010
# Description:
#   This function calculates the channel characteristics portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the bank geometry and  channel
#   characteristics data files.
# Metrics:
#   xshor2vg, mxshor, mnshor, pct_ovrb, pctch_b, pctch_c, pctch_n, pctch_u and
#   conbankfull, confeatures, conpattern, conpercent, constraint, convalley,
#   convalleybox
# Function Revisions:
#   01/27/10 rch: copied, plagerized and made up this code.
#   02/18/10 cws: removed source() of NRSAValidation.r and summaryby.r
#   03/23/10 ssr: moved creation of unit test dataframes to separate functions.
#   06/10/10 cws: removed list of created metrics as incorrect.
#   06/24/10 cws: Added mets from tblChannelChar2
#   09/16/10 cws: Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/04/10 cws: Modified to handle single-protocol data, updated unit test
#            accordingly.
#   11/18/10 cws: added require(Hmisc) for %nin% operator
#   07/26/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify the names of the data files.  Added argument NRSAdir to
#            the function to identify the directory from which the data file is
#            read and to which the output metrics file is written.
#   12/20/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   11/18/15 cws Modified calling interface; Using SITE and VALUE instead of
#            UID and RESULT.
#    2/23/16 cws Updated argument descriptions, and cleaned up comments a tad.
#
# ARGUMENTS:
# bankfullWidth     dataframe containing bankfull width from channel constraint  
#                   form for allreaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       numeric or character values
#                   Note: This value is simply copied to the output without change
#
# channelPattern    dataframe containing pattern type from channel constraint 
#                   form for allreaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character values
#                   Note: This value is simply copied to the output without change
#
# constraintFeatures dataframe containing constraint features from channel  
#                    constraint form for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character values
#                   Note: This value is simply copied to the output without change
#
# constraintMultiple dataframe containing constraining features from channel 
#                    riparian forms for boatable reaches, with the following 
#                    columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character values expected to be one of 'B',
#                                   'C','N','U' or NA
#
# constraintSingle  dataframe containing canopy densiometer from channel  
#                   constraint form for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       numeric values
#                   Note: This value is simply copied to the output without change
#
# constraintPercent dataframe containing percent constraint estimates from channel 
#                   constraint form for allreaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character or numeric values
#                   Note: This value is simply copied to the output without change
#
# seeOverBank       dataframe containing canopy densiometer from channel 
#                   riparian forms for boatable reaches, with the following 
#                   columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character values = 'YES' if true, other value
#                                   if not true
#
# shoreToVegDistance dataframe containing distance (in meters) from shore to 
#                    vegetation from channel riparian forms for boatable reaches,
#                    with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       numeric values
#
# valleyConstraintUnseen dataframe containing canopy densiometer from channel 
#                        constraint form for all reaches, with the following
#                        columns:
#                           SITE    integer or character specifying the site visit 
#                           VALUE   character values
#                   Note: This value is simply copied to the output without change
#
# valleyWidth       dataframe containing canopy densiometer from channel 
#                   constraint form for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character or numeric values 
#                   Note: This value is simply copied to the output without change
#
# Output:
#   Either a data frame when metric calculation is successful or a NULL
#   when metric calculation is not successful.  The data frame contains the 
#   following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
#
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################


    intermediateMessage('Channel Characteristic mets', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% select(SITE,VALUE)
        return(rc)
    }

    intermediateMessage('.1')
    bankfullWidth <- absentAsNULL(bankfullWidth, ifdf)
    channelPattern <- absentAsNULL(channelPattern, ifdf)
    constraintFeatures <- absentAsNULL(constraintFeatures, ifdf)
    constraintMultiple <- absentAsNULL(constraintMultiple, ifdf)
    constraintPercent <- absentAsNULL(constraintPercent, ifdf)
    constraintSingle <- absentAsNULL(constraintSingle, ifdf)
    seeOverBank <- absentAsNULL(seeOverBank, ifdf)
    shoreToVegDistance <- absentAsNULL(shoreToVegDistance, ifdf)
    valleyConstraintUnseen <- absentAsNULL(valleyConstraintUnseen, ifdf)
    valleyWidth <- absentAsNULL(valleyWidth, ifdf)

    intermediateMessage('.2')

    mets <- NULL   # Assemble calculations here.
    if(!is.null(shoreToVegDistance)) {
        #Use summaryby for the three metrics of shor2rip distance (boatable only)
        shoreToVegDistance <- shoreToVegDistance %>% mutate(VALUE=as.numeric(VALUE))
        xsh <- summaryby(shoreToVegDistance,'mean',"xshor2vg")
        mxs <- summaryby(shoreToVegDistance,'max',"mxshor")
        mns <- summaryby(shoreToVegDistance,'min',"mnshor")
        mets <- rbind(xsh, mxs, mns)
        intermediateMessage('.3')
    }
  
    if(!is.null(seeOverBank)) {
        #Calculate percentages for each of the pct_* metrics and format the results (boatable only)
 
        tco <- summaryby(seeOverBank,'count',"pct_ovrb")
        tyout<-aggregate( list(yessum=seeOverBank$VALUE),list(SITE=seeOverBank$SITE),function(x){sum(x=='YES',na.rm=TRUE)})
        tco<-merge(tco,tyout,by='SITE',all.x=TRUE)
        tco$yessum<- ifelse( is.na(tco$yessum),0,tco$yessum)
        tco$VALUE <- (tco$yessum/tco$VALUE)*100
        tco<-tco[c('SITE','METRIC','VALUE')]
        mets <- rbind(mets, tco)
        intermediateMessage('.4')
    }
  
      if(!is.null(constraintMultiple)) {
          tbb <- summaryby(constraintMultiple,'count',"pctch_b")
          bb<-aggregate(list(letsum=constraintMultiple$VALUE),list(SITE=constraintMultiple$SITE),function(x){sum(x=='B',na.rm=TRUE)})
          tbb<-merge(tbb,bb,by='SITE',all.x=TRUE)
          tbb$VALUE <- (tbb$letsum/tbb$VALUE)*100
          tbb<-tbb[c('SITE','METRIC','VALUE')]

          tcc <- summaryby(constraintMultiple,'count',"pctch_c")
          cc<-aggregate(list(letsum=constraintMultiple$VALUE),list(SITE=constraintMultiple$SITE),function(x){sum(x=='C',na.rm=TRUE)})
          tcc<-merge(tcc,cc,by='SITE',all.x=TRUE)
          tcc$VALUE <- (tcc$letsum/tcc$VALUE)*100
          tcc<-tcc[c('SITE','METRIC','VALUE')]

          tnn <- summaryby(constraintMultiple,'count',"pctch_n")
          nn<-aggregate(list(letsum=constraintMultiple$VALUE),list(SITE=constraintMultiple$SITE),function(x){sum(x=='N',na.rm=TRUE)})
          tnn<-merge(tnn,nn,by='SITE',all.x=TRUE)
          tnn$VALUE <- (tnn$letsum/tnn$VALUE)*100
          tnn<-tnn[c('SITE','METRIC','VALUE')]

          tuu <- summaryby(constraintMultiple,'count',"pctch_u")
          uu<-aggregate(list(letsum=constraintMultiple$VALUE),list(SITE=constraintMultiple$SITE),function(x){sum(x=='U',na.rm=TRUE)})
          tuu<-merge(tuu,uu,by='SITE',all.x=TRUE)
          tuu$VALUE <- (tuu$letsum/tuu$VALUE)*100
          tuu<-tuu[c('SITE','METRIC','VALUE')]
          mets <- rbind(mets, tbb, tcc, tnn, tuu)

          intermediateMessage('.5')
      }

      # Metrics using data from Constraint form is just the recorded values.
      intermediateMessage('.6')
      justAddMetricColumn <- function(df, metric) {
          if(is.null(df)) rc <- NULL
          else rc <- mutate(df, METRIC=metric)
          return(rc)
      }
      ccMets <- rbind(justAddMetricColumn(bankfullWidth, 'conbankfull')
                     ,justAddMetricColumn(constraintSingle, 'constraint')
                     ,justAddMetricColumn(constraintFeatures, 'confeatures')
                     ,justAddMetricColumn(channelPattern, 'conpattern')
                     ,justAddMetricColumn(constraintPercent, 'conpercent')
                     ,justAddMetricColumn(valleyWidth, 'convalley')
                     ,justAddMetricColumn(valleyConstraintUnseen, 'convalleybox')
                     )

      # Build final collection of mets
      mets <- rbind(mets, ccMets)
      if(is.null(mets)) {
          mets <- subset(data.frame(SITE='a',METRIC='a',VALUE='a', stringsAsFactors=FALSE), FALSE)
          intermediateMessage('.7')
      }
      intermediateMessage('.  Done.', loc='end')

      return(mets)
}

# end of file
