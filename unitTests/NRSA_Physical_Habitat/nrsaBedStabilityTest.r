# nrsaBedStability.r
# RUnit tests
#
#  2/27/18 cws Modified unit test in response to use of aquametStandardizeArgument().


nrsaBedStabilityTest <- function()
# Unit tests nrsaBedStability()
# Expected values for SITE 5+ taken from WEMAP calculations unchanged except
# for the following:
#   At SITE 11, s_ldmb_bw5 changed from NA to -0.080588008, s_lrbs_bw5 changed
#     from NA to 1.2704980082 and s_rp100 changed from NA to 441.9807675
#     as EMAP does not calculate estimated bed stability for rivers.
{
  protocols <- nrsaBedStabilityTest.protocols ()
  metsExpected <- nrsaBedStabilityTest.expectedMets ()

  intermediateMessage('.2.0 Test with both protocols', loc='end')
  testData <- nrsaBedStabilityTest.testData ()
  nrsaBedStabilityTestTest.process (testData, metsExpected, protocols)
  
  intermediateMessage ('.2.1 Test with wadeable protocol', loc='end')
  test.w <- subset(testData, SITE %in% subset (protocols, PROTOCOL=='WADEABLE')$SITE)
  expected.w <- subset (metsExpected, SITE %in% subset (protocols, PROTOCOL=='WADEABLE')$SITE)
  nrsaBedStabilityTestTest.process (test.w, expected.w, protocols)
  
  intermediateMessage ('.2.2 Test with boatable protocol', loc='end')
  test.b <- subset(testData
                  ,SITE %in% subset (protocols, PROTOCOL=='BOATABLE')$SITE &
                   METRIC != 'lsub2dmm'
                  )
  expected.b <- subset(metsExpected
                      ,SITE %in% subset (protocols, PROTOCOL=='BOATABLE')$SITE &
                       METRIC %nin% c('lrbs_bw6','s_lrbs_bw6')
                      )
  nrsaBedStabilityTestTest.process (test.b, expected.b, protocols)
} 



nrsaBedStabilityTestTest.process <- function (testData, metsExpected, protocols)   
#
{
  rr <- nrsaBedStability(bXdepth =  subset(testData, METRIC == 'xdepth' & SITE %in% subset(protocols, PROTOCOL=='BOATABLE')$SITE) %>% select(SITE, VALUE)
                        ,bSddepth = subset(testData, METRIC == 'sddepth' & SITE %in% subset(protocols, PROTOCOL=='BOATABLE')$SITE) %>% select(SITE, VALUE)
                        ,wXdepth =  subset(testData, METRIC == 'xdepth' & SITE %in% subset(protocols, PROTOCOL=='WADEABLE')$SITE) %>% select(SITE, VALUE)
                        ,wSddepth = subset(testData, METRIC == 'sddepth' & SITE %in% subset(protocols, PROTOCOL=='WADEABLE')$SITE) %>% select(SITE, VALUE)
                        ,lsub_dmm = subset(testData, METRIC == 'lsub_dmm') %>% select(SITE, VALUE)
                        ,lsub2dmm = subset(testData, METRIC == 'lsub2dmm') %>% select(SITE, VALUE)
                        ,rp100 =    subset(testData, METRIC == 'rp100') %>% select(SITE, VALUE)
                        ,v1w_msq =  subset(testData, METRIC == 'v1w_msq') %>% select(SITE, VALUE)
                        ,xbkf_h =   subset(testData, METRIC == 'xbkf_h') %>% select(SITE, VALUE)
                        ,xbkf_w =   subset(testData, METRIC == 'xbkf_w') %>% select(SITE, VALUE)
                        ,xfc_lwd =  subset(testData, METRIC == 'xfc_lwd') %>% select(SITE, VALUE)
                        ,xslope =   subset(testData, METRIC == 'xslope') %>% select(SITE, VALUE)
                        ,xwidth =   subset(testData, METRIC == 'xwidth') %>% select(SITE, VALUE)
                        )

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
                      ,c('SITE','METRIC')
                      )
                ,{relerr <- (VALUE.y-VALUE.x)/VALUE.x}
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
                   ,c('SITE','METRIC'), zeroFudge=10^-7
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
                   ,c('SITE','METRIC'), zeroFudge=10^-4
                   )
print(errs)
  checkEquals(NULL, errs
             ,"Error: Bed stability s_* metric is broken"
             )

  errs <- dfCompare(subset(metsExpected, METRIC=='s_Dcbf_g08')
                   ,subset(rr, METRIC=='s_Dcbf_g08')
                   ,c('SITE','METRIC'), zeroFudge=10^-2
                   )
  checkEquals(NULL, errs
             ,"Error: Bed stability s_* metric is broken"
             )

}



nrsaBedStabilityTest.protocols <- function ()
# create dataframe of protocolas for bed stability unit tests
{
  protocols <- data.frame(SITE=as.character(1:22)
                        ,PROTOCOL=c(rep('WADEABLE',10)
                                   ,rep('BOATABLE', 10)
                                   ,rep('NONE', 2)
                                   )
                        ,stringsAsFactors=FALSE
                        )
   return (protocols)                     
}



nrsaBedStabilityTest.testData <- function()
# creates dataframe of bed stability data for unit test
{
  testData <- rbind(# Very simple numbers to simply test functionality
                # Simple reach, no missing values
                data.frame(SITE='1'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,         1,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and nonzero fishcover
               ,data.frame(SITE='2'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          1,     3.95494014
                                   )
                          )
               # Simple reach missing LWD and zero fishcover
               ,data.frame(SITE='3'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,        NA,          0,     3.95494014
                                   )
                          )
               # Simple reach nonmissing LWD and missing fishcover
               ,data.frame(SITE='4'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(1,         1,          1,          1
                                   ,1,         1,          2,          1
                                   ,2,       0.3,         NA,     3.95494014
                                   )
                          )

                # Real numbers from WEMAP to test accuracy of calculations
                # 2004 WUTP99-0735  2 -- normal wadeable reach
               ,data.frame(SITE='5'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(51.16,	0.75545,	21.818,	 10.0105
                                   ,1.22,  0.5408,    0.5408, 19.0993
                                   ,22.08840009, 0.00004,   0,      27.7557
                                   )
                          )
                          
               # 2004 WWAP04-R048  1-- big substrate
               ,data.frame(SITE='6'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(71.19, 0.61818, 6.673, 5.255
                                   ,9.75, 3.204, 3.19253, 28.3768
                                   ,25.80377156, 0.01461, 0.01364, 44.2636
                                   )
                          )

               # 2003 WWAP99-0542  1 -- Lots of LWD
               ,data.frame(SITE='7'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(24.007, 0.3, 2.927, 2.445
                                   ,8.05, 0.75694, 0.7244, 6.4191
                                   ,4.41366392, 0.37977, 0.11818, 10.4878
                                   )
                          )

               #  2004 WDEQ-0003       1 - has zero xslope
               ,data.frame(SITE='8'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(31.033,	0.4,	3.4818,	2.385
                                   ,0, 0.37664, 0.37664, 27.2566
                                   , 46.12060600, 0.00015, 0, 22.6112
                                   )
                          )

               # 2004 WSDP04-R050  1 -- missing v1w_msq (really zero) and xfc_lwd==0
               ,data.frame(SITE='9'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(66.51, 0, 8.745, 9.1474
                                   ,1.57, 0.31911, 0.29885, 22.2393
                                   ,21.92365464,  NA,		0, 28.7622
                                   )
                          )
               # 2004 WCOP04-R004  1-- missing v1w_msq (really zero) with non-zero xfc_lwd
               ,data.frame(SITE='10'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(42.67, 0.33636, 3.718, 3.085
                                   , 0.775, -1.31451, -1.31451, 10.3489
                                   ,5.80084933, NA, 0.00455, 8.8661
                                   )
                          )
               # End of values from wadeable reaches

               # 2004   WCAP99-1103        1 -- normal boatable reach
               ,data.frame(SITE='11'
                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
                                   )
                          ,VALUE=c(3.29555, 1.47273, 255.273, 163.364
                                   ,0.036, 1.18991, NA, 112.73
                                   ,NA, 0.005818, 0.00455, 1.69386
                                   )
                          )
               #
#               ,data.frame(SITE=rep('12',12)
#                          ,METRIC=c('xdepth',  'xbkf_h',   'xbkf_w',   'xwidth'
#                                   ,'xslope',  'lsub_dmm', 'lsub2dmm', 'rp100'
#                                   ,'s_rp100', 'v1w_msq', 'xfc_lwd', 'sddepth'
#                                   )
#                          ,VALUE=c(1,         1,          1,          1
#                                   ,1,         1,          2,          1
#                                   ,2,       0.3,         NA,     3.95494014
#                                   )
#                          )

               )
  testData$SITE <- as.character(testData$SITE)
  testData$METRIC <- as.character(testData$METRIC)
  testData <- subset(testData, METRIC != 's_rp100')
return(testData)
}



nrsaBedStabilityTest.expectedMets <- function()
# creates dataframe of bank morphology metrics calculation results for unit test
# NOTE: SITE 8 is a river with zero slope.  The expected results were changed from the
# WEMAP values to conform to the change in minimum slope from 0.01% to 0.0001%.
{
  metsExpected <- rbind(data.frame(SITE='1'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(-0.164309429, 1.164309429, 0.835690571
                                       ,0.831325766, -0.164309429, 1.164309429
                                       ,0.164309429, 0.168674234, 1.164309429
                                       ,1.168674234, 113.9277151, 2.056629387
                                       ,-1.056629387, 2, 88.58264244
                                       ,1.947348631, -0.947348631
                                       ,0.6565, 0.00815071, 0.003597874, 0.441418512, 0.499863675, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(SITE='2'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(-0.164309429,	1.164309429, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 2, NA
                                       ,NA, NA
                                       ,0.6565, NA, 0.003597874, NA, NA, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(SITE='3'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(-0.164309429, 1.164309429, 1.835690571
                                       ,1.831325766, 1.835690571, -0.835690571
                                       ,-0.835690571, -0.831325766, 0.164309429
                                       ,0.168674234, NA, NA
                                       ,NA, 2, NA
                                       ,NA, NA
                                       ,0.6565, NA, 0.003597874, NA, NA, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(SITE='4'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(-0.164309429, 1.164309429, 1.437750563
                                       ,1.426755179, 0.437750563, 0.562249437
                                       ,-0.437750563, -0.426755179, 0.562249437
                                       ,0.573244821, 146.4599268, 2.165718813
                                       ,-1.165718813, 2, 113.3484813
                                       ,2.054415706, -1.054415706
                                       ,0.6565, 0.003836429, 0.003597874, 0.93781855, 0.642600417, 2487.62711, 0.026505908
                                       )
                              )
                   ,data.frame(SITE='5'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(1.630980938, -1.090180938, 1.95385339
                                       ,1.941619453, 1.953783007, -1.412983007
                                       ,-1.41305339, -1.400819453, -1.41305339
                                       ,-1.400819453, 105.9303106, 2.025020246
                                       ,-1.484220246, 22.08840009, 97.4687271
                                       ,1.988865294, -1.448065294
                                       ,0.8235825, 0.032088995, 0.002531907, 0.078902658, 0.353240905, 1069.058291, 0.024577125
                                       )
                              )
                   ,data.frame(SITE='6'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(2.67711418, 0.52688582, 2.832055426
                                       ,2.842904539, 2.815915646, 0.388084354
                                       ,0.371944574, 0.361095461, 0.360474574
                                       ,0.349625461, 1588.155449, 3.200893009
                                       ,0.003106991, 25.80377156, 1675.254566
                                       ,3.22408081, -0.02008081
                                       ,0.864552, 0.055668934, 0.045205373, 0.812039493, 0.806585054, 1425823.55, 0.029914536
                                       )
                              )
                   ,data.frame(SITE='7'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(2.121824344, -1.364884344, 1.418982992
                                       ,1.436906832, 0.418982992, 0.337957008
                                       ,-0.662042992, -0.679966832, -0.694582992
                                       ,-0.712506832, 148.8617733, 2.172783188
                                       ,-1.415843188, 4.41366392, 172.0294129
                                       ,2.235602707, -1.478662707
                                       ,0.3510455, 0.287163515, 0.003670352, 0.012781401, 0.082077269, 2949.08684, 0.026813659
                                       )
                              )
                   ,data.frame(SITE='8'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(-2.67248566708815e+00, 3.04912566708815e+00, -2.52336710885127e+00
                                       ,NA, -2.52380211577484e+00, 2.90044211577484e+00
                                       ,2.90000710885127e+00, NA, 2.90000710885127e+00
                                       ,NA, 1.92130883311715e-03, -2.71640282059464e+00
                                       ,3.09304282059464e+00, 1.19507314484124e+02, 8.24192404294506e-04
                                       ,-3.08397139222369e+00, 3.46061139222369e+00
                                       ,0.4617145, 0.403859626, 0.002664427, 0.006597408, 0.086596024, 4.9658623, 0.027228347
                                       )
                              )
                   ,data.frame(SITE='9'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(1.854477172, -1.535367172, 1.677706613
                                       ,1.680788736, 1.677706613, -1.358596613
                                       ,-1.358596613, -1.361678736, -1.378856613
                                       ,-1.381938736, NA, NA
                                       ,NA, 21.92365464, NA
                                       ,NA, NA
                                       ,0.432315, NA, 0.002619276, NA, NA, 527.3851469, 0.022418957
                                       )
                              )
                   ,data.frame(SITE='10'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(1.355114917, -2.669624917, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, NA, NA
                                       ,NA, 5.80084933, NA
                                       ,NA, NA
                                       ,0.495989, NA, 0.002, NA, NA, 9.226665785, 0.02346655
                                       )
                              )
                   ,data.frame(SITE='11'
                              ,METRIC=c('ltest','lrbs_tst','ldmb_bw5'
                                       ,'s_ldmb_bw5', 'ldmb_bw4','lrbs_bw4'
                                       ,'lrbs_bw5','s_lrbs_bw5','lrbs_bw6'
                                       ,'s_lrbs_bw6','Dcbf_g08','ldcbf_g08'
                                       ,'lrbs_g08','s_rp100','s_Dcbf_g08'
                                       ,'s_ldcbf_g08','s_lrbs_g08'
                                       ,'rb3','ct_rpwd','cp3_mill','cp3ctrpwd_rat','rrpw3','reyp3','shld_px3'
                                       )
                              ,VALUE=c(0.909920977, 0.279989023, 0.951821206
                                       ,-0.080588008, 0.949639991, 0.240270009
                                       ,0.238088794, 1.2704980082, NA
                                       ,NA, 17.97017283, 1.254552254
                                       ,-0.064642254, 441.9807675, 8.224442973
                                       ,0.915106494, 0.274803506
                                       ,3.099382, 0.008345542, 0.002643027, 0.316699289, 2.112633373, 1588.059315, 0.02556789
                                       )
                              )
                   )
  metsExpected$SITE <- as.character(metsExpected$SITE)
  metsExpected$METRIC <- as.character(metsExpected$METRIC)

return(metsExpected)
}



# end of file
