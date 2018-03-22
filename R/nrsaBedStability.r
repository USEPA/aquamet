#' @export
#' @title Calculate NRSA Bed Stability Metrics
#' @description This function calculates the bed stability portion 
#' of the physical habitat metrics for National Rivers and Streams 
#' Assessment (NRSA) data.  The function requires numerous inputs
#' of different types of physical habitat measures.
#' @param bXdepth A data frame containing depth means (units are m) 
#' for boatable reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param bSddepth A data frame containing depth standard deviations 
#' (units are m) for boatable reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param wXdepth A data frame containing mean depths (units 
#' are cm) for wadeable reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param wSddepth dataframe containing depth standard 
#' deviations (units are cm) for wadeable reaches, with the 
#' following columns: 
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param lsub_dmm dataframe containing log10 of the mean 
#' substrate diameter (units are mm) using the old 
#' one-boulder-class system for all reaches
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param lsub2dmm A data frame containing log10 of the mean 
#' substrate diameter (units are mm) using the two-boulder-class 
#' system for all reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param rp100 dataframe containing rp100 values (linear 
#' density of residual pools) for all reaches, with the 
#' following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param v1w_msq A data frame containing v1w_msq values 
#' (areal density of LWD volume in the channel) for all reaches, 
#' with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param xbkf_h A data frame containing xbkf_h values (mean 
#' bank full height) for all reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param xbkf_w A data frame containing xbkf_w values (mean 
#' bank full width) for all reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param xfc_lwd A data frame containing xfc_lwd values (mean 
#' fish cover provided by large woody debris) for all reaches, 
#' with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param xslope A data frame containing xslope values (mean 
#' channel slope at site, in percent) for all reaches, with 
#' the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @param xwidth A data frame containing xwidth values (mean 
#' channel width) for all reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric values
#' }           
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics for all sites include: ltest, lrbs_tst, ldmb_bw5,
#' s_ldmb_bw5, ldmb_bw4, lrbs_bw4, lrbs_bw5, s_lrbs_bw5, lrbs_bw6,  
#' s_lrbs_bw6, Dcbf_g08, ldcbf_g08, lrbs_g08, s_rp100, s_Dcbf_g08, 
#' s_ldcbf_g08, s_lrbs_g08, rb3, ct_rpwd, cp3_mill, cp3ctrpwd_rat, 
#' rrpw3, reyp3, shld_px3 
#'  
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}

nrsaBedStability <- function(bXdepth = NULL, bSddepth = NULL
                            ,wXdepth = NULL, wSddepth = NULL
                            ,lsub_dmm = NULL, lsub2dmm = NULL
                            ,rp100 = NULL
                            ,v1w_msq = NULL, xbkf_h = NULL, xbkf_w = NULL
                            ,xfc_lwd = NULL, xslope = NULL, xwidth = NULL
                            ,isUnitTest=FALSE
                            ) {
# nrsaBedStability <- function(bankgeometry, thalweg, visits, channelgeometry,
#   channelcrosssection, littoral, wood, fishcover, gisCalcs=NULL) {

################################################################################
# Function: nrsaBedStability
# Title: Calculate NRSA Bed Stability Metrics
# Programmers: Curt Seeliger
#              Suzanne San Romani
#              Marlys Cappert
#              Tom Kincaid
# Date: December 24, 2009
# Description:
#   This function calculates the bed stability portion of the physical habitat
#   metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the bankgeometry, thalweg, visits,
#   channelgeometry, channelcrosssection, littoral, wood, and fishcover data
#   files.
# Wadeable Metrics:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
#   s_ldmb_bw5 s_lrbs_bw5 s_lrbs_bw6
# Boatable Metrics:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
# Function Revisions:
#   12/24/09 cws: created
#    3/11/10 cws: Reading metrics calcs with readNRSACalculationResults()
#            instead of readNRSAValidationResults().  Call to
#            writeNRSACalcResults()corrected.
#    3/12/10 cws: Changed PROTOCOL to protocol.  Adding s_rp100 calculation here
#            since it is used here, and modifying unit test accordingly.
#    03/18/10 ssr: Changed 'protocol' from siteProtocol to 'PROTOCOL'
#    03/22/10 ssr: moved creation of unit test dataframes to separate functions.
#     3/25/10 cws: Changed diff() calls to dfCompare().
#    04/02/10 mrc: Modified unit test and metrics code to handle data with just
#           one protocol.  
#    04/13/10 cws: Increasing precision of s_rp100 values in unit test by
#             separating checks of s_* and non-s_* metrics, setting precision
#             to 10E-4 and 10E-7 respectively.  The difference in attainable
#             accuracy is due to the exponential calculation of s_rp100.
#   11/12/10 cws: Modified to handle single protocol studies by allowing
#            lsub2dmm metric to not be required for calculations.  Updated unit
#            test accordingly.
#   11/18/10 cws: added require(Hmisc) for %nin% operator
#    3/14/11 cws: Added calculation of s_Dcbf_g08 and s_ldcbf_g08.  Unit test
#            updated accordingly.
#   12/20/11 cws: Restandardized names of individual metrics files, removing 
#            WITHGPSSLOPES and the like.
#    3/08/12 cws: Handling cases when mets results are character type; also
#            changing 'NA' results to NA, which is what they should be.
#    3/12/12 cws: Temporarily add hydraulic radii of the roughness components to 
#            the output.
#    3/13/12 cws: Temporarily modifying BOATABLE v1w_msq to estimate areal
#            density of entire bankfull channel rather than the 10 m littoral
#            band which was sampled. Based on discussion with Phil, this assumes
#            there is no wood in the channel and there is no research indicating
#            this to be true, but the current use is incorrect as it assumes the
#            same density of lwd in the channel as the littoral.
#    3/15/12 cws: Reverting to 'classic' lwd handling and not exporting
#            intermediate values (that code is commented out).  Passes unit
#            test.  Exporting different intermediate values now.
#    3/27/12 cws: Added intermediate calculations Cp3_mill Cp3Ctrpwd_rat,
#            Ct_rpwd, Rb3 ReyP3, Prpw3, Shld_Px3.  Using 1e-4 as minimum value
#            of slope detectable by any method.  Updated unit test accordingly.
#   07/26/12 tmk: Removed calls to the require() function.  Added argument tbl
#            to the function to identify name of the data file.  Added argument
#            NRSAdir to the function to identify the directory from which
#            metrics files are read and to which the output metrics file is
#            written.
#   12/28/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Calculated channel morphology, slope and
#            bearing, substrate characterization, residual pools, large woody
#            debris, and fish cover metrics directly rather than reading metric
#            calculation results from a file.  Modified output to be a data
#            frame rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#    2/23/16 cws Updated argument descriptions
#
# ARGUMENTS:
# bXdepth       dataframe containing depth means (units are m) for boatable 
#               reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values
#
# bSddepth      dataframe containing depth standard deviations (units are m) for
#                boatable reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values
#
# wXdepth       dataframe containing mean depths (units are cm) for wadetable 
#               reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values
#
# wSddepth      dataframe containing depth standard deviations (units are cm) for 
#               wadeable reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values
#
# lsub_dmm      dataframe containing log10 of the mean substrate diameter (units 
#               are mm) using the old one-boulder-class system for all reaches, 
#               with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# lsub2dmm      dataframe containing log10 of the mean substrate diameter (units 
#               are mm) using the two-boulder-class system for all reaches, 
#               with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# rp100         dataframe containing rp100 values (linear density of residual 
#               pools) for all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# v1w_msq       dataframe containing v1w_msq values (areal density of LWD volume 
#               in the channel) for all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# xbkf_h        dataframe containing xbkf_h values (mean bank full height) for 
#               all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# xbkf_w        dataframe containing xbkf_w values (mean bank full width) for 
#               all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# xfc_lwd       dataframe containing xfc_lwd values (mean fish cover provided by 
#               large woody debris) for 
#               all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# xslope        dataframe containing xslope values (mean channel slope at site,
#               in percent) for all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# xwidth        dataframe containing xwidth values (mean channel width) for 
#               all reaches, with the following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric values 
#
# Output:
#   Either a data frame when metric calculation is successful or a NULL
#   when metric calculation is not successful.  The data frame contains the 
#   following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
#


  intermediateMessage('Bed stability calculations', loc='start')
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        metName <- args[[1]]
        rc <- df %>% 
              select(SITE, VALUE) %>% 
              mutate(METRIC=metName)
        return(rc)
    }
    ifdfProtocol <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE) %>% 
              mutate(PROTOCOL=pName)
        return(rc)
    }
    a<-1+2
    bXdepth  <- aquametStandardizeArgument(bXdepth, 'xdepth', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,20)))
    bSddepth <- aquametStandardizeArgument(bSddepth, 'sddepth', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,NA)))
    wXdepth  <- aquametStandardizeArgument(wXdepth, 'xdepth', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,20)))
    wSddepth <- aquametStandardizeArgument(wSddepth, 'sddepth', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,NA)))
    lsub_dmm <- aquametStandardizeArgument(lsub_dmm, 'lsub_dmm', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'))
    lsub2dmm <- aquametStandardizeArgument(lsub2dmm, 'lsub2dmm', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'))
    rp100    <- aquametStandardizeArgument(rp100, 'rp100', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,NA)))
    v1w_msq  <- aquametStandardizeArgument(v1w_msq, 'v1w_msq', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,1)))
    xbkf_h   <- aquametStandardizeArgument(xbkf_h, 'xbkf_h', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,15)))
    xbkf_w   <- aquametStandardizeArgument(xbkf_w, 'xbkf_w', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,100)))
    xfc_lwd  <- aquametStandardizeArgument(xfc_lwd, 'xfc_lwd', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,1)))
    xslope   <- aquametStandardizeArgument(xslope, 'xslope', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,20)))
    xwidth   <- aquametStandardizeArgument(xwidth, 'xwidth', ifdf=ifdf, struct=list(SITE=c('integer','character'), VALUE='double'), rangeLimits = list(VALUE=c(0,100)))
    
    mets <- rbind(absentAsNULL(bXdepth,  ifdf, 'xdepth')
                 ,absentAsNULL(bSddepth, ifdf, 'sddepth')
                 ,absentAsNULL(wXdepth,  ifdf, 'xdepth')
                 ,absentAsNULL(wSddepth, ifdf, 'sddepth')
                 ,absentAsNULL(lsub_dmm, ifdf, 'lsub_dmm')
                 ,absentAsNULL(lsub2dmm, ifdf, 'lsub2dmm')
                 ,absentAsNULL(rp100,    ifdf, 'rp100')
                 ,absentAsNULL(v1w_msq,  ifdf, 'v1w_msq')
                 ,absentAsNULL(xbkf_h,   ifdf, 'xbkf_h')
                 ,absentAsNULL(xbkf_w,   ifdf, 'xbkf_w')
                 ,absentAsNULL(xfc_lwd,  ifdf, 'xfc_lwd')
                 ,absentAsNULL(xslope,   ifdf, 'xslope')
                 ,absentAsNULL(xwidth,   ifdf, 'xwidth')
                 )
    protocols <- rbind(absentAsNULL(bXdepth, ifdfProtocol, 'BOATABLE')
                      ,absentAsNULL(bSddepth, ifdfProtocol, 'BOATABLE')
                      ,absentAsNULL(wXdepth, ifdfProtocol, 'WADEABLE')
                      ,absentAsNULL(wSddepth, ifdfProtocol, 'WADEABLE')
                      ) %>%
                 unique()
    if(is.null(protocols) | is.null(mets)) return(NULL)

    # The following code is unchanged

  # Convert long to wide so metrics are all on same row.  Drop prefix for
  # column names.
  mets$VALUE <- as.numeric(mets$VALUE)
  mm <- reshape(mets, idvar='SITE', direction='wide', timevar='METRIC')
  names(mm) <- gsub('VALUE\\.', '', names(mm))
  
  # Convert boatable depths from m to cm to match wadeable units, using the
  # protocols dataframe
  mm <- merge(mm, protocols, by='SITE', all.x=TRUE, all.y=FALSE)
  mm$xdepth <- ifelse(mm$PROTOCOL=='BOATABLE', mm$xdepth * 100, mm$xdepth)
  mm$sddepth <- ifelse(mm$PROTOCOL=='BOATABLE', mm$sddepth * 100, mm$sddepth)

  # Make zero slopes slighly positive so we can log them
  mm$xslope <- ifelse(mm$xslope<=0.0001, 0.0001, mm$xslope)

#  # Adjust v1w density for boatables: was over 20*10 plot, now over 20*xbkf_w area
#  # Zero out v1w density for boatables, unless it's missing.
#  mm$v1w_msq <- with(mm, ifelse(PROTOCOL=='BOATABLE' & !is.na(v1w_msq), 0, v1w_msq))

  # Calculate s_rp100 here.
  mm$s_rp100 <- 10^(-0.44767 +
                    1.25381*log10(mm$sddepth) +
                    -0.20675*log10(mm$xslope)
                   )

  intermediateMessage('.1')

  # Crude estimate of critical erodible substrate diamter (mm).  This estimate
  # of the hydrologic radius (Rbf) assumes flow through a very simple channel.
  critdia <- 13.7 * (0.5 * mm$xdepth * 10) * (mm$xslope / 100)
  mm$ltest <- ifelse(critdia > 0, log10(critdia), NA)
  mm$lrbs_tst <- mm$lsub_dmm - mm$ltest

  intermediateMessage('.2')
  
  # Refined estimate of critical erodible substrate diameter (mm), taking LWD
  # and residual pool 'roughness' into account.  Do this for actual pools and
  # estimated pools.  Remove Rw from Rbf up to 90% of Rbf value.
  Rbf <- 0.5 * ((mm$xdepth - mm$rp100) * 10 + mm$xbkf_h * 1000)
  s_Rbf <- 0.5 * ((mm$xdepth - mm$s_rp100) * 10 + mm$xbkf_h * 1000)

  Rw <-ifelse(is.na(mm$v1w_msq)
             ,ifelse(mm$xfc_lwd == 0    # fill in missing LWD volume as 0 when
                    ,0                  # no LWD fishcover
                    ,NA                 # otherwise we really can't guess.
                    )
             , mm$v1w_msq * 1000
             )

  Rbf <- ifelse(Rw >= 0.9 * Rbf, 0.1 * Rbf, Rbf - Rw)
  s_Rbf <- ifelse(Rw >= 0.9 * s_Rbf, 0.1 * s_Rbf, s_Rbf - Rw)

  critdia <- 13.7 * Rbf * mm$xslope / 100
  s_critdia <- 13.7 * s_Rbf * mm$xslope / 100

  mm$ldmb_bw5 <- log10(ifelse(critdia > 0, critdia, NA))
  mm$s_ldmb_bw5 <- log10(ifelse(s_critdia > 0, s_critdia, NA))

  intermediateMessage('.3')

#  # Temporarily add hydraulic radii of the roughness components to the output.
#  mm$Rw5 <- Rw
#  mm$Rbf5 <- Rbf
#  mm$s_Rbf5 <- s_Rbf
#  mm$critDia5 <- critdia
#  mm$s_critDia5 <- s_critdia
  
  # Calculate old version of logged critical diameter that were based on the
  # wood density when it accidentally used the wetted widths instead of the
  # bankfull widths.
  Rw <- ifelse(!is.na(Rw) & !is.na(mm$xbkf_w) & mm$xwidth > 0
              ,Rw * mm$xbkf_w / mm$xwidth
              ,NA
              )

  Rbf <- ifelse(Rw >= 0.9 * Rbf, 0.1 * Rbf, Rbf - Rw)
  critdia <- 13.7 * Rbf * mm$xslope / 100
  mm$ldmb_bw4 <- log10(ifelse(critdia > 0, critdia, NA))

  intermediateMessage('.4')

  # Calculate log10 of the bed stability values based on previous estimates
  # of critical substrate diameter.
  mm$lrbs_bw4   <- mm$lsub_dmm - mm$ldmb_bw4
  mm$lrbs_bw5   <- mm$lsub_dmm - mm$ldmb_bw5
  mm$s_lrbs_bw5 <- mm$lsub_dmm - mm$s_ldmb_bw5
  if ('lsub2dmm' %in% names(mm)) {
      mm$lrbs_bw6   <- mm$lsub2dmm - mm$ldmb_bw5
      mm$s_lrbs_bw6 <- mm$lsub2dmm - mm$s_ldmb_bw5
  }
  intermediateMessage('.5')
  
#  # Temporarily add hydraulic radii of the roughness components to the output.
#  mm$Rw4 <- Rw
#  mm$Rbf4 <- Rbf

  # Calculate some better than refined, research-based bed stability estimates
  # Kaufmann, P.R. et al., A roughness-corrected index of relative bed stability
  # for regional stream surveys.  Geomorphology (2007)
  rho <- 998
  rhoSed <- 2650
  g <- 9.807
  Dbf_th <- mm$xbkf_h + (mm$xdepth / 100)
  Rb3 <- 0.65 * Dbf_th
  rp <- mm$rp100 / 100
  s_rp <- mm$s_rp100 / 100
  s <- mm$xslope / 100
  viscosity <- 0.00000102
  v1w_msq <- mm$v1w_msq
  lsub_dmm <- mm$lsub_dmm

  # Total hydraulic resistance, eqn 13b
  Ct_rpwd <- 1.21 * (rp^1.08) * ((rp + v1w_msq)^0.638) * (Dbf_th^-3.32)
  s_Ct_rpwd <- 1.21 * (s_rp^1.08) * ((s_rp + v1w_msq)^0.638) * (Dbf_th^-3.32)

  # Hydraulic resistance due to particles
  tt <- (1/8) * (2.03 * log10(12.2 * Rb3 / (((10^lsub_dmm)/1000)) ))^(-2)
  Cp3_mill <- ifelse(tt < 0.002, 0.002, tt)

  # Intermediate calculations, with restriction as described for eqn 16
  tt <- Cp3_mill / Ct_rpwd
  Cp3Ctrpwd_rat <- ifelse(tt > 1, 1, tt)

  tt <- Cp3_mill / s_Ct_rpwd
  s_Cp3Ctrpwd_rat <- ifelse(tt > 1, 1, tt)

  # Adjustment to bankfull shear stress, eqn 10b
  Rrpw3 <- Rb3 * (Cp3Ctrpwd_rat^(1/3))
  s_Rrpw3 <- Rb3 * (s_Cp3Ctrpwd_rat^(1/3))

  # Reynolds number at bankfull, eqn 14
  ReyP3 <- ((g * Rb3 * s)^0.5) * ((10^lsub_dmm)/1000) / viscosity

  # Shields parameter, eqn 15a, 15b.  Note 15a uses an abbreviated value of
  # the exponent in the case of small Reynolds numbers
  Shld_Px3 <- ifelse(ReyP3>0
                    ,ifelse(ReyP3 < 26
                           ,0.04 * ReyP3^(-0.24)
                           ,0.5 * ((0.22 * (ReyP3^(-0.6))) +
                                   0.06 * (10^(-7.7 * (ReyP3^(-0.6))))
                                  )
                           )
                    ,NA
                    )

  # Provide intermediate calculation results for debugging
  mm <- within(mm
              ,{rb3 <- Rb3
                ct_rpwd <- Ct_rpwd
                cp3_mill <- Cp3_mill
                cp3ctrpwd_rat <- Cp3Ctrpwd_rat
                rrpw3 <- Rrpw3
                reyp3 <- ReyP3
                shld_px3 <- Shld_Px3
               }
              )

  # Bed surface particle critical diameter (mm) Dcbf* from eqn 16.
  mm$Dcbf_g08 <- 1000 * (rho * g * Rrpw3 * s) / (Shld_Px3 * (rhoSed - rho) * g)
  mm$s_Dcbf_g08 <- 1000 * (rho * g * s_Rrpw3 * s) / (Shld_Px3 * (rhoSed - rho) * g)

  mm$ldcbf_g08 <- log10(mm$Dcbf_g08)
  mm$s_ldcbf_g08 <- log10(mm$s_Dcbf_g08)
  mm$lrbs_g08 <- mm$lsub_dmm - mm$ldcbf_g08
  mm$s_lrbs_g08 <- mm$lsub_dmm - mm$s_ldcbf_g08

  intermediateMessage('.6')
  # Transpose wide to long format, and clean up factors, rownames and attributes
  mm2 <- subset(mm, select=-c(xdepth,sddepth,xbkf_h,xbkf_w,xwidth,xslope
                             ,lsub_dmm,rp100,v1w_msq,xfc_lwd
                             ,PROTOCOL
                             )
               )
  if ('lsub2dmm' %in% names(mm)) {
    mm2 <- subset(mm2, select=-lsub2dmm)
  }
  tmm <- reshape(mm2, idvar=c('SITE'), direction='long'
                ,varying=names(mm2)[names(mm2) != 'SITE']
                ,times=names(mm2)[names(mm2) != 'SITE']
                ,v.names='VALUE', timevar='METRIC'
#                ,drop=c('xdepth','xbkf_h','xbkf_w','xwidth','xslope','lsub_dmm'
#                       ,'lsub2dmm','rp100','s_rp100','v1w_msq','xfc_lwd'
#                       ,'PROTOCOL'
#                       )
                )
  row.names(tmm)<-NULL
  tmm$SITE <- as.character(tmm$SITE)
  tmm <- data.frame(tmm)

  intermediateMessage('  Done.', loc='end')
  return(tmm)
}



# end of file
