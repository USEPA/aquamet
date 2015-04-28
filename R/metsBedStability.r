# metsBedStability.r
#
# 12/24/09 cws created
#  3/11/10 cws Reading metrics calcs with readNRSACalculationResults() instead
#          of readNRSAValidationResults().  Call to writeNRSACalcResults()
#          corrected.
#  3/12/10 cws Changed PROTOCOL to protocol.  Adding s_rp100 calculation here
#          since it is used here, and modifying unit test accordingly.
#
#  03/18/10 ssr Changed 'protocol' from siteProtocol to 'PROTOCOL'
#  03/22/10 ssr moved creation of unit test dataframes to separate functions.
#   3/25/10 cws Changed diff() calls to dfCompare().
#
#  04/02/10 mrc Modified unit test and metrics code to handle data with just
#           one protocol.  
#  04/13/10 cws Increasing precision of s_rp100 values in unit test by
#           separating checks of s_* and non-s_* metrics, setting precision
#           to 10E-4 and 10E-7 respectively.  The difference in attainable
#           accuracy is due to the exponential calculation of s_rp100.
# 11/12/10 cws Modified to handle single protocol studies by allowing lsub2dmm
#          metric to not be required for calculations.  Updated unit test
#          accordingly.
# 11/18/10 cws added require(Hmisc) for %nin% operator
#  3/14/11 cws Added calculation of s_Dcbf_g08 and s_ldcbf_g08.  Unit test
#          updated accordingly.
# 12/20/11 cws Restandardized names of individual metrics files, removing 
#          WITHGPSSLOPES and the like.
#  3/08/12 cws Handling cases when mets results are character type; also
#          changing 'NA' results to NA, which is what they should be.
#  3/12/12 cws Temporarily add hydraulic radii of the roughness components to 
#          the output.
#  3/13/12 cws Temporarily modifying BOATABLE v1w_msq to estimate areal density of
#          entire bankfull channel rather than the 10 m littoral band which was 
#          sampled. Based on discussion with Phil, this assumes there is no 
#          wood in the channel and there is no research indicating this to be 
#          true, but the current use is incorrect as it assumes the same density
#          of lwd in the channel as the littoral.
#  3/15/12 cws Reverting to 'classic' lwd handling and not exporting intermediate
#          values (that code is commented out).  Passes unit test.  Exporting
#          different intermediate values now.
#  3/27/12 cws Added intermediate calculations Cp3_mill Cp3Ctrpwd_rat, Ct_rpwd, Rb3
#          ReyP3, Prpw3, Shld_Px3.  Using 1e-4 as minimum value of slope detectable
#          by any method.  Updated unit test accordingly.
#
# TO DO: Expand check of wadeable reaches.
#        Handle case when protocol can not be determined.
#
require(RODBC)
require(RUnit)
require(Hmisc)

metsBedStability <- function()
# Calculates NRSA bed stability metrics:
#   Wadeable protocol:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
#   s_ldmb_bw5 s_lrbs_bw5 s_lrbs_bw6
#
#   Boatable protocol:
#   ldmb_bw4 ldmb_bw5 lrbs_bw4 lrbs_bw5 lrbs_bw6 lrbs_g08 lrbs_tst ltest
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Bed stability metrics are calculated entirely with previously calculated
  # metrics.  Retrieve these metrics from their individual temporary csv
  # files and combine in a single dataframe.  Make sure 'NA' values are changed
  # to NA, which is assumed by the calculation function.
  cm <- readNRSACalculationResults('metsChannelMorphology.csv')  # xdepth, sddepth, xbkf_h, xbkf_w, xwidth
  if(is.character(cm)) return(cm)

  sb <- readNRSACalculationResults('metsSlopeBearing.csv')       # xslope
  if(is.character(sb)) return(sb)

  sc <- readNRSACalculationResults('metsSubstrateCharacterization.csv') # lsub_dmm, lsub2dmm
  if(is.character(sc)) return(sc)

  rp <- readNRSACalculationResults('metsResidualPools.csv')      # rp100, s_rp100
  if(is.character(rp)) return(rp)

  lwd <- readNRSACalculationResults('metsLargeWoody.csv')   # v1w_msq
  if(is.character(lwd)) return(lwd)

  fc <- readNRSACalculationResults('metsFishCover.csv')          # xfc_lwd
  if(is.character(fc)) return(fc)

  mets <- rbind(subset(cm, METRIC %in% c('xdepth', 'sddepth', 'xbkf_h', 'xbkf_w', 'xwidth'))
               ,subset(sb, METRIC %in% c('xslope'))
               ,subset(sc, METRIC %in% c('lsub_dmm', 'lsub2dmm'))
               ,subset(rp, METRIC %in% c('rp100', 's_rp100'))
               ,subset(lwd, METRIC %in% c('v1w_msq'))
               ,subset(fc, METRIC %in% c('xfc_lwd'))
               )
  mets$RESULT <- with(mets, ifelse(RESULT == 'NA', NA, RESULT))       

  protocols <- siteProtocol(unique(mets$UID))

  bs <- metsBedStability.1(mets, protocols)
  if(is.character(bs)) return(bs)
  bs$RESULT <- as.numeric(bs$RESULT)
  rc <- writeNRSACalcResults(bs, 'metsBedStability.csv')
}

metsBedStability.1 <- function(mets, protocols)
# Does the work for for metsBedStability
#
# ARGUMENTS:
# mets      dataframe of relevant metrics: xdepth, sddepth, xbkf_h, xbkf_w,
#             xwidth, xslope, lsub_dmm, rp100, v1w_msq, xfc_lwd
# protocols dataframe specifying the protocol each UID was sampled with.
{
  intermediateMessage('Beginning bed stability calculations', loc='start')

  # Convert long to wide so metrics are all on same row.  Drop prefix for
  # column names.
  mets$RESULT <- as.numeric(mets$RESULT)
  mm <- reshape(mets, idvar='UID', direction='wide', timevar='METRIC')
  names(mm) <- gsub('RESULT\\.', '', names(mm))

  # Convert boatable depths from m to cm to match wadeable units, using the
  # protocols dataframe
  mm <- merge(mm, protocols, by='UID', all.x=TRUE, all.y=FALSE)
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
  tmm <- reshape(mm2, idvar=c('UID'), direction='long'
                ,varying=names(mm2)[names(mm2) != 'UID']
                ,times=names(mm2)[names(mm2) != 'UID']
                ,v.names='RESULT', timevar='METRIC'
#                ,drop=c('xdepth','xbkf_h','xbkf_w','xwidth','xslope','lsub_dmm'
#                       ,'lsub2dmm','rp100','s_rp100','v1w_msq','xfc_lwd'
#                       ,'PROTOCOL'
#                       )
                )
  row.names(tmm)<-NULL
  tmm$UID <- as.character(tmm$UID)
  tmm <- data.frame(tmm)

  intermediateMessage('  Done.', loc='end')
  return(tmm)
}


metsBedStabilityTest <- function()
# Unit tests metsBedStability()
# Expected values for UID 5+ taken from WEMAP calculations unchanged except
# for the following:
#   At UID 11, s_ldmb_bw5 changed from NA to -0.080588008, s_lrbs_bw5 changed
#     from NA to 1.2704980082 and s_rp100 changed from NA to 441.9807675
#     as EMAP does not calculate estimated bed stability for rivers.


{
  protocols <- metsBedStability.protocols ()
  metsExpected <- metsBedStability.expectedMets ()

  intermediateMessage('.2.0 Test with both protocols', loc='end')
  testData <- metsBedStability.testData ()
  metsBedStabilityTest.process (testData, metsExpected, protocols)
  
  intermediateMessage ('.2.1 Test with wadeable protocol', loc='end')
  test.w <- subset(testData, UID %in% subset (protocols, PROTOCOL=='WADEABLE')$UID)
  expected.w <- subset (metsExpected, UID %in% subset (protocols, PROTOCOL=='WADEABLE')$UID)
  metsBedStabilityTest.process (test.w, expected.w, protocols)
  
  intermediateMessage ('.2.2 Test with boatable protocol', loc='end')
  test.b <- subset(testData
                  ,UID %in% subset (protocols, PROTOCOL=='BOATABLE')$UID &
                   METRIC != 'lsub2dmm'
                  )
  expected.b <- subset(metsExpected
                      ,UID %in% subset (protocols, PROTOCOL=='BOATABLE')$UID &
                       METRIC %nin% c('lrbs_bw6','s_lrbs_bw6')
                      )
  metsBedStabilityTest.process (test.b, expected.b, protocols)
} 


metsBedStabilityTest.process <- function (testData, metsExpected, protocols)   
#
{
  rr <- metsBedStability.1(testData, protocols)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.  The calculation of s_rp100 is only
  # accurate to 10E-4 due to the exponential calculations. The calculation of
  # s_Dcbf_g08 is accurate to 1e-2.  The checks of calculation results are split
  # along this line.
  errs <- within(merge(subset(metsExpected
                          ,METRIC %in% c('rb3', 'ct_rpwd', 'cp3_mill', 'cp3ctrpwd_rat', 'rrpw3', 'reyp3', 'shld_px3')
                          )
                      ,subset(rr
                             ,METRIC %in% c('rb3', 'ct_rpwd', 'cp3_mill', 'cp3ctrpwd_rat', 'rrpw3', 'reyp3', 'shld_px3')
                             )
                      ,c('UID','METRIC')
                      )
                ,{relerr <- (RESULT.y-RESULT.x)/RESULT.x}
                )
  checkEquals(0, nrow(subset(errs, abs(relerr) > 2e-7))
             ,"Error: Bed stability metrics are broken"
             )

  errs <- dfCompare(subset(metsExpected
                          ,METRIC %in% c('ltest', 'lrbs_tst', 'ldmb_bw5', 'ldmb_bw4', 'lrbs_bw4', 'lrbs_bw5', 'lrbs_bw6', 'Dcbf_g08', 'ldcbf_g08', 'lrbs_g08')
                          )
                   ,subset(rr
                          ,METRIC %in% c('ltest', 'lrbs_tst', 'ldmb_bw5', 'ldmb_bw4', 'lrbs_bw4', 'lrbs_bw5', 'lrbs_bw6', 'Dcbf_g08', 'ldcbf_g08', 'lrbs_g08')
                          )
                   ,c('UID','METRIC'), zeroFudge=10^-7
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability metrics are broken"
             )

  errs <- dfCompare(subset(metsExpected
                          ,substr(METRIC,1,2) == 's_' & METRIC != 's_Dcbf_g08'
                          )
                   ,subset(rr
                          ,substr(METRIC,1,2) == 's_' & METRIC != 's_Dcbf_g08'
                          )
                   ,c('UID','METRIC'), zeroFudge=10^-4
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability s_* metric is broken"
             )

  errs <- dfCompare(subset(metsExpected, METRIC=='s_Dcbf_g08')
                   ,subset(rr, METRIC=='s_Dcbf_g08')
                   ,c('UID','METRIC'), zeroFudge=10^-2
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability s_* metric is broken"
             )

}


metsBedStability.protocols <- function ()
# create dataframe of protocolas for bed stability unit tests
 {
  protocols <- data.frame(UID=as.character(1:22)
                        ,PROTOCOL=c(rep('WADEABLE',10)
                                   ,rep('BOATABLE', 10)
                                   ,rep('NONE', 2)
                                   )
                        ,stringsAsFactors=FALSE
                        )
   return (protocols)                     
   }
   
metsBedStability.testData <- function()
# creates dataframe of bed stability data for unit test
{
  testData <- rbind(# Very simple numbers to simply test functionality
                # Simple reach, no missing values
                data.frame(UID='1'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,         1,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and nonzero fishcover
               ,data.frame(UID='2'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and zero fishcover
               ,data.frame(UID='3'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          0,     3.95494014
                                   )
                          )
               # Simple reach nonmissing LWD and missing fishcover
               ,data.frame(UID='4'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,       0.3,         NA,     3.95494014
                                   )
                          )

                # Real numbers from WEMAP to test accuracy of calculations
                # 2004 WUTP99-0735  2 -- normal wadeable reach
               ,data.frame(UID='5'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(51.16,	0.75545,	21.818,	 10.0105
                                   ,1.22,  0.5408,    0.5408, 19.0993
                                   ,22.08840009, 0.00004,   0,      27.7557
                                   )
                          )
                          
               # 2004 WWAP04-R048  1-- big substrate
               ,data.frame(UID='6'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(71.19, 0.61818, 6.673, 5.255
                                   ,9.75, 3.204, 3.19253, 28.3768
                                   ,25.80377156, 0.01461, 0.01364, 44.2636
                                   )
                          )

               # 2003 WWAP99-0542  1 -- Lots of LWD
               ,data.frame(UID='7'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(24.007, 0.3, 2.927, 2.445
                                   ,8.05, 0.75694, 0.7244, 6.4191
                                   ,4.41366392, 0.37977, 0.11818, 10.4878
                                   )
                          )

               #  2004 WDEQ-0003       1 - has zero xslope
               ,data.frame(UID='8'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(31.033,	0.4,	3.4818,	2.385
                                   ,0, 0.37664, 0.37664, 27.2566
                                   , 46.12060600, 0.00015, 0, 22.6112
                                   )
                          )

               # 2004 WSDP04-R050  1 -- missing v1w_msq (really zero) and xfc_lwd==0
               ,data.frame(UID='9'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(66.51, 0, 8.745, 9.1474
                                   ,1.57, 0.31911, 0.29885, 22.2393
                                   ,21.92365464,  NA,		0, 28.7622
                                   )
                          )
               # 2004 WCOP04-R004  1-- missing v1w_msq (really zero) with non-zero xfc_lwd
               ,data.frame(UID='10'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(42.67, 0.33636, 3.718, 3.085
                                   , 0.775, -1.31451, -1.31451, 10.3489
                                   ,5.80084933, NA, 0.00455, 8.8661
                                   )
                          )
               # End of values from wadeable reaches

               # 2004   WCAP99-1103        1 -- normal boatable reach
               ,data.frame(UID='11'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,RESULT=c(3.29555, 1.47273, 255.273, 163.364
                                   ,0.036, 1.18991, NA, 112.73
                                   ,NA, 0.005818, 0.00455, 1.69386
                                   )
                          )
               #
#               ,data.frame(UID=rep('12',12)
#                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
#                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
#                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
#                                   )
#                          ,RESULT=c(1,         1,          1,          1
#                                   ,1,         1,          2,          1
#                                   ,2,       0.3,         NA,     3.95494014
#                                   )
#                          )

               )
  testData$UID <- as.character(testData$UID)
  testData$METRIC <- as.character(testData$METRIC)
  testData <- subset(testData, METRIC != 's_rp100')
return(testData)
}


metsBedStability.expectedMets <- function()
# creates dataframe of bank morphology metrics calculation results for unit test
# NOTE: UID 8 is a river with zero slope.  The expected results were changed from the
# WEMAP values to conform to the change in minimum slope from 0.01% to 0.0001%.
{
  metsExpected <- rbind(data.frame(UID='1'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 0.835690571
                                       ,0.831325766, -0.164309429, 1.164309429
                                       ,0.164309429, 0.168674234, 1.164309429
                                       ,1.168674234, 113.9277151, 2.056629387
                                       ,-1.056629387, 2, 88.58264244
                                       ,1.947348631, -0.947348631
                                       ,0.6565, 0.00815071, 0.003597874, 0.441418512, 0.499863675, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(UID='2'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(-0.164309429,	1.164309429, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 2, NA
                                       ,NA, NA
                                       ,0.6565, NA, 0.003597874, NA, NA, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(UID='3'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 1.835690571
                                       ,1.831325766, 1.835690571, -0.835690571
                                       ,-0.835690571, -0.831325766, 0.164309429
                                       ,0.168674234, NA, NA
                                       ,NA, 2, NA
                                       ,NA, NA
                                       ,0.6565, NA, 0.003597874, NA, NA, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(UID='4'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(-0.164309429, 1.164309429, 1.437750563
                                       ,1.426755179, 0.437750563, 0.562249437
                                       ,-0.437750563, -0.426755179, 0.562249437
                                       ,0.573244821, 146.4599268, 2.165718813
                                       ,-1.165718813, 2, 113.3484813
                                       ,2.054415706, -1.054415706
                                       ,0.6565, 0.003836429, 0.003597874, 0.93781855, 0.642600417, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(UID='5'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(1.630980938, -1.090180938, 1.95385339
                                       ,1.941619453, 1.953783007, -1.412983007
                                       ,-1.41305339, -1.400819453, -1.41305339
                                       ,-1.400819453, 105.9303106, 2.025020246
                                       ,-1.484220246, 22.08840009, 97.4687271
                                       ,1.988865294, -1.448065294
                                       ,0.8235825, 0.032088995, 0.002531907, 0.078902658, 0.353240905, 1069.058291, 0.024577125
                                       )
                              )
                   ,data.frame(UID='6'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(2.67711418, 0.52688582, 2.832055426
                                       ,2.842904539, 2.815915646, 0.388084354
                                       ,0.371944574, 0.361095461, 0.360474574
                                       ,0.349625461, 1588.155449, 3.200893009
                                       ,0.003106991, 25.80377156, 1675.254566
                                       ,3.22408081, -0.02008081
                                       ,0.864552, 0.055668934, 0.045205373, 0.812039493, 0.806585054, 1425823.55, 0.029914536
                                       )
                              )
                   ,data.frame(UID='7'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(2.121824344, -1.364884344, 1.418982992
                                       ,1.436906832, 0.418982992, 0.337957008
                                       ,-0.662042992, -0.679966832, -0.694582992
                                       ,-0.712506832, 148.8617733, 2.172783188
                                       ,-1.415843188, 4.41366392, 172.0294129
                                       ,2.235602707, -1.478662707
                                       ,0.3510455, 0.287163515, 0.003670352, 0.012781401, 0.082077269, 2949.08684, 0.026813659
                                       )
                              )
                   ,data.frame(UID='8'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(-2.67248566708815e+00, 3.04912566708815e+00, -2.52336710885127e+00
                                       ,NA, -2.52380211577484e+00, 2.90044211577484e+00
                                       ,2.90000710885127e+00, NA, 2.90000710885127e+00
                                       ,NA, 1.92130883311715e-03, -2.71640282059464e+00
                                       ,3.09304282059464e+00, 1.19507314484124e+02, 8.24192404294506e-04
                                       ,-3.08397139222369e+00, 3.46061139222369e+00
                                       ,0.4617145, 0.403859626, 0.002664427, 0.006597408, 0.086596024, 4.9658623, 0.027228347
                                       )
                              )
                   ,data.frame(UID='9'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(1.854477172, -1.535367172, 1.677706613
                                       ,1.680788736, 1.677706613, -1.358596613
                                       ,-1.358596613, -1.361678736, -1.378856613
                                       ,-1.381938736, NA, NA
                                       ,NA, 21.92365464, NA
                                       ,NA, NA
                                       ,0.432315, NA, 0.002619276, NA, NA, 527.3851469, 0.022418957
                                       )
                              )
                   ,data.frame(UID='10'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(1.355114917, -2.669624917, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 5.80084933, NA
                                       ,NA, NA
                                       ,0.495989, NA, 0.002, NA, NA, 9.226665785, 0.02346655
                                       )
                              )
                   ,data.frame(UID='11'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,RESULT=c(0.909920977, 0.279989023, 0.951821206
                                       ,-0.080588008, 0.949639991, 0.240270009
                                       ,0.238088794, 1.2704980082, NA
                                       ,NA, 17.97017283, 1.254552254
                                       ,-0.064642254, 441.9807675, 8.224442973
                                       ,0.915106494, 0.274803506
                                       ,3.099382, 0.008345542, 0.002643027, 0.316699289, 2.112633373, 1588.059315, 0.02556789
                                       )
                              )
                   )
  metsExpected$UID <- as.character(metsExpected$UID)
  metsExpected$METRIC <- as.character(metsExpected$METRIC)

return(metsExpected)
}

# end of file
