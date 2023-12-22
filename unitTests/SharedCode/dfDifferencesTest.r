# dfDifferencesTest.r
#
#  6/09/23 cws Moved here from sharedSupport.r. Reorganized unit test for
#          readability. Using all_of() around variables specifying column names
#          as needed.
#  6/28/23 cws Comparing text values as numeric if both are numeric. This is to
#          facillitate comparison of 'long format' data where
#          values in a column may be text (e.g. taxa) and numbers (w.g. lengths, 
#          counts). Unit test NOT extended to test this.
#  7/10/23 cws Modified comparison of characters as numbers to account for values
#          such as NA, NaN, inf.
# 12/19/23 cws Copied to aquamet for use with dfDifferences. Unit test separated
#          to file in aquamet/unitTests/SharedCode/dfDifferencesTest.r
#

dfDifferencesTest <- function()
# unit test for dfDifferences
{
    testData <- dfDifferencesTest.baseData()
    
    # test case with identical dataframes
    expected <- dfDifferencesTest.baseDifferences(testData)
    actual <- dfDifferences(testData, testData, c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when dataframes are identical")
    
    # test case with dataframes with many differences
    tt <- dfDifferencesTest.createDifferencesAndResults(testData)
    altData <- tt$altData
    expected <- tt$expected
    actual <- dfDifferences(testData, altData, c('aa','bb'))
    dd <- dfCompare(expected, actual, c('aa','bb','column'))
#return(list(e=expected, a=actual, dd=dd, d1=testData, d2=altData))
    checkTrue(is.null(dd), "Incorrect values regardless of ordering")
    checkEquals(expected, actual, "Incorrect when dataframes are different")

    # test case with absent byVars
    expected <- "df1 does not contain all of the by-variables"
    actual <- dfDifferences(testData %>% dplyr::rename(aa1=aa), testData, c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when byVars not all in dataframe 1")

    expected <- "df2 does not contain all of the by-variables"
    actual <- dfDifferences(testData, testData %>% dplyr::rename(aa2=aa), c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when byVars not all in dataframe 2")

}

dfDifferencesTest.baseData <- function()
{
    rc <- data.frame(aa = rep(letters[1:5], each=3)
                    ,bb = rep(1:3, times=5)
                    ,cc = LETTERS[1:15]
                    ,rr = runif(15)
                    ,uu = runif(15)
                    ,stringsAsFactors = FALSE
                    )
    return(rc)
}

dfDifferencesTest.baseDifferences <- function(baseData)
{
    rc <- baseData %>%
          mutate(rr = rr %>% as.character()
                ,uu = uu %>% as.character()
                ) %>%
          pivot_longer(-c('aa','bb'), names_to='column', values_to='value') %>%
          mutate(first = value
                ,second = value
                ,diff = ifelse(column=='cc'
                              ,as.numeric(FALSE)
                              ,suppressWarnings(as.numeric(second) - as.numeric(first))
                              )
                ,type = 'same'
                ,type2 = 'same:genl'
                ) %>%
          select(-value) %>%
          arrange(aa, bb, column) %>%
          data.frame()
    return(rc)
}

dfDifferencesTest.createDifferencesAndResults <- function(testData)
{
    altData <- testData %>%
               dplyr::rename(newuu = uu) %>%               # check different columns
               mutate(cc = ifelse (aa=='a' & bb==1, 'a'    # check case sensitivity
                          ,ifelse (aa=='a' & bb==2, '   B' # check whitespace sensitivity
                          ,ifelse (aa=='a' & bb==3, '   c' # check case and whitespace sensitivity
                          ,ifelse (aa=='b' & bb==1, 'DDD'  # check difference
                          ,ifelse (aa=='b' & bb==2, ''     # check blank
                          ,ifelse (aa=='b' & bb==3, NA     # check NA
                                  ,cc
                          ))))))
                     ,rr = ifelse (aa=='a' & bb==1, rr - 3e-18 # check allowance for rounding
                          ,ifelse (aa=='a' & bb==2, rr - 1     # check different number
                          ,ifelse (aa=='a' & bb==3, NA         # check NA
                          ,ifelse (aa=='b' & bb==1, +Inf       # check Inf
                          ,ifelse (aa=='b' & bb==2, -Inf       # check -Inf
                          ,ifelse (aa=='b' & bb==3, NaN        # check NaN
                                  ,rr
                           ))))))
                     ,aa = ifelse(aa=='c' & bb == 1, 'cc', aa) # check absent rows in each
                     )
    expected <- dfDifferencesTest.baseDifferences(testData) %>%
                mutate(second = ifelse (aa=='a' & bb==1 & column=='cc', subset(altData, aa=='a' & bb==1)$cc
                               ,ifelse (aa=='a' & bb==2 & column=='cc', subset(altData, aa=='a' & bb==2)$cc
                               ,ifelse (aa=='a' & bb==3 & column=='cc', subset(altData, aa=='a' & bb==3)$cc
                               ,ifelse (aa=='b' & bb==1 & column=='cc', subset(altData, aa=='b' & bb==1)$cc
                               ,ifelse (aa=='b' & bb==2 & column=='cc', subset(altData, aa=='b' & bb==2)$cc   
                               ,ifelse (aa=='b' & bb==3 & column=='cc', subset(altData, aa=='b' & bb==3)$cc
                               ,ifelse (aa=='c' & bb==1 & column=='cc', NA
                               ,ifelse (aa=='a' & bb==1 & column=='rr', subset(altData, aa=='a' & bb==1)$rr
                               ,ifelse (aa=='a' & bb==2 & column=='rr', subset(altData, aa=='a' & bb==2)$rr
                               ,ifelse (aa=='a' & bb==3 & column=='rr', subset(altData, aa=='a' & bb==3)$rr
                               ,ifelse (aa=='b' & bb==1 & column=='rr', subset(altData, aa=='b' & bb==1)$rr
                               ,ifelse (aa=='b' & bb==2 & column=='rr', subset(altData, aa=='b' & bb==2)$rr
                               ,ifelse (aa=='b' & bb==3 & column=='rr', subset(altData, aa=='b' & bb==3)$rr
                               ,ifelse (aa=='c' & bb==1 & column=='rr', NA
                                       ,second
                                ))))))))))))))
                      ,diff = ifelse (aa=='a' & bb==1 & column=='cc', 1
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 1
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==3 & column=='cc', NA
                             ,ifelse (aa=='c' & bb==1 & column=='cc', NA
                             ,ifelse (aa=='a' & bb==1 & column=='rr', subset(altData, aa=='a' & bb==1)$rr - subset(testData, aa=='a' & bb==1)$rr
                             ,ifelse (aa=='a' & bb==2 & column=='rr', -1
                             ,ifelse (aa=='a' & bb==3 & column=='rr', NA
                             ,ifelse (aa=='b' & bb==1 & column=='rr', +Inf
                             ,ifelse (aa=='b' & bb==2 & column=='rr', -Inf
                             ,ifelse (aa=='b' & bb==3 & column=='rr', NaN
                             ,ifelse (aa=='c' & bb==1 & column=='rr', NA
                                     ,0
                              ))))))))))))))
                      ,type = ifelse (aa=='a' & bb==1 & column=='cc', 'sameCI'
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 'sameSI'
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 'sameCSI'
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 'diff'
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 'diff'
                             ,ifelse (aa=='b' & bb==3 & column=='cc', 'diff'
                             ,ifelse (aa=='c' & bb==1 & column=='cc', 'only1'
                             ,ifelse (aa=='a' & bb==1 & column=='rr', 'same'
                             ,ifelse (aa=='a' & bb==2 & column=='rr', 'diff'
                             ,ifelse (aa=='a' & bb==3 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==1 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==2 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==3 & column=='rr', 'diff'
                             ,ifelse (aa=='c' & bb==1 & column=='rr', 'only1'
                                                                    , 'same'
                              ))))))))))))))
                      ,type2= ifelse (aa=='a' & bb==1 & column=='cc', 'sameCI:genl'
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 'sameSI:genl'
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 'sameCSI:genl'
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 'genl:genl'
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 'genl:blank'
                             ,ifelse (aa=='b' & bb==3 & column=='cc', 'genl:na'
                             ,ifelse (aa=='c' & bb==1 & column=='cc', 'only1:genl'
                             ,ifelse (aa=='a' & bb==1 & column=='rr', 'same:genl'
                             ,ifelse (aa=='a' & bb==2 & column=='rr', 'genl:genl'
                             ,ifelse (aa=='a' & bb==3 & column=='rr', 'genl:na'
                             ,ifelse (aa=='b' & bb==1 & column=='rr', 'genl:+inf'
                             ,ifelse (aa=='b' & bb==2 & column=='rr', 'genl:-inf'
                             ,ifelse (aa=='b' & bb==3 & column=='rr', 'genl:nan'
                             ,ifelse (aa=='c' & bb==1 & column=='rr', 'only1:genl'
                                                                    , 'same:genl'
                              ))))))))))))))
                      
                      ) %>%
                # Remove rows for the uu column which only occurs in the first 
                # dataframe, and add them below, just because it's easier.
                subset(column %nin% c('uu')) %>%
                rbind(# add this row as uniquely occuring in the second dataframe
                      dfDifferencesTest.baseDifferences(testData) %>%
                      subset(column %nin% c('uu')) %>%
                      subset(aa=='c' & bb == 1) %>%
                      mutate(aa = 'cc'
                            ,first = as.numeric(NA)
                            ,diff = as.numeric(NA)
                            ,type  = 'only2'
                            ,type2 = 'only2:genl'
                            )
                     ,# add rows for column uu occuring only in the first dataframe
                      testData %>%
                      mutate(column = 'uu'
                            ,first = uu
                            ,second =  '<absent>'
                            ,diff = '<absent>'
                            ,type = '<absent>'
                            ,type2 = '<absent>'
                            ) %>%
                      select(aa, bb, column, first, second, diff, type, type2)
                     ,# add rows for column newuu occuring only in the second dataframe
                      altData %>%
                      mutate(column = 'newuu'
                            ,first = '<absent>'
                            ,second = newuu
                            ,diff = '<absent>'
                            ,type = '<absent>'
                            ,type2 = '<absent>'
                            ) %>%
                      select(aa, bb, column, first, second, diff, type, type2)
                     ) %>%
              arrange(aa, bb, column)
    row.names(expected) <- NULL
    
    rc <- list(altData = altData, expected = expected)
    return(rc)

}
# end of file