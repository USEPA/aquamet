# dfDifferences.r
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
#  1/10/24 cws Added zeroish argument to replace hardcoded value. Modified unit
#          test to include 'zeroish' classification in output. Made classification
#          of numeric values stored in character columns the same as when stored 
#          in numeric columns. Prettyprinted. Corrected difference checking of
#          infinite numeric values when expressed in character columns. Added
#          sameABS value of type to indicate only difference in numbers is the 
#          sign.
#  2/19/24 cws Using all_of() within select() in compareText() when selecting 
#          columns using character strings instead of unquoted column names.
#  2/22/24 cws Using suppressWarnings() to address warnings due to attempting to
#          treat character values as if numeric.
#  4/06/24 cws Corrected classNumber to class number as zero if it was within 
#          zeroFudge of zero. Modified classCharacter to use classNumber to 
#          further classify numeric value instead of just 'number'. Also continued
#          change on 2/19/24 to use all_of() around thisVar in remaining calls 
#          to select(). No change to unit test.
#  4/29/24 cws Updated with version in Aquamet to incorporate extended unit test
#          and the changes needed in the comparison results to standardize the
#          results in type and type2. Unit test now uses much wider selection of
#          values in a cartesian pairing to exercise all comparison paths.
#  6/13/24 cws Modified to use dplyr instead of plyr for pipe.
#
require(dplyr) # for pipe - %>%
require(tidyr)
require(Hmisc) # for %nin%


dfDifferences <- function(df1, df2, byVars
                         ,zeroFudge = 1e-17
                         ,absentValue = '<absent>'
                         ,zeroish = 1e-6
                         )
# Used to find and categorize differences in data.frames. Returns data.frame
# of differences
#
# ARGUMENTS:
# df1, df2    Dataframes to compare.  These must share key variables.
# byVars      character vector of Key variable name(s) which uniquely identify 
#               specific rows in each dataframe.
# zeroFudge   Largest difference between numeric values that is regarded as zero,
#             but are not due to floating point issues.
# absentValue character value to place in columns 'first' or 'second' when a
#             column is absent in the first or second dataframe arguments
# zeroish     Small numeric value > 0 that is used to classify numbers with
#             absolute values which are suspiciously close to zero. If the
#             absolute value of a number is less than this value, it gets 
#             classed as 'zeroish'.
#
{
    # define constants
    altNumericStrings <- c('NA','inf','-inf','+inf','nan','NaN') # strings that can be numbers
    
    # sanity checks
    if(!all(byVars %in% names(df1))) return("df1 does not contain all of the by-variables")
    if(!all(byVars %in% names(df2))) return("df2 does not contain all of the by-variables")
    if(setequal(byVars, names(df1))) return("df1 contains only by-variables")
    if(setequal(byVars, names(df2))) return("df2 contains only by-variables")
    
    # setup comparisons
    only1Vars <- setdiff(names(df1), names(df2))
    only2Vars <- setdiff(names(df2), names(df1))
    compareVars <- intersect(names(df1), names(df2)) %>% setdiff(byVars)
    allDiffs <- NULL
    
    classCharacter <- function(cc) {
        # classify individual character strings
        rc <- ifelse(cc %in% '',         'blank'
             ,ifelse(trimws(cc) %in% '', 'spaces'
             ,ifelse(is.number(cc),      classNumber(suppressWarnings(as.numeric(cc))) # was 'number'
             ,ifelse(is.na(cc),          'na'
                                        ,'genl'
              ))))
        return(rc)
    }
    
    classNumber <- function(vv) {
        # classify individual numeric vvalues. Assumes vv is numeric type
        rc <- ifelse(is.infinite(vv) & sign(vv) == 1,  '+inf'
             ,ifelse(is.infinite(vv) & sign(vv) == -1, '-inf'
             ,ifelse(is.nan(vv),                       'nan'      
             ,ifelse(is.na(vv) & !is.nan(vv),          'na'
             ,ifelse(abs(vv) <= zeroFudge,             'zero'
             ,ifelse(abs(vv) < zeroish,                'zeroish'
                                                      ,'number' #'genl'       
              ))))))
        return(rc)
    }
    
    compareNumbers <- function(dd) {
        # compare values as numbers
        rc <- dd %>%
              mutate(first =    as.numeric(first)
                    ,second =   as.numeric(second)
                    ,classFirst = classNumber(first)
                    ,classSecond = classNumber(second)
                    ,diff =     second - first
                    ,type =     ifelse(inFirst & !inSecond,                   'only1'
                               ,ifelse(!inFirst & inSecond,                   'only2'
                               ,ifelse(inFirst & inSecond,  
                                       ifelse(is.na(first) & is.na(second),   'same'
                                      ,ifelse(is.nan(first) & is.nan(second), 'same'
                                      ,ifelse(is.infinite(first) &
                                              is.infinite(second) & 
                                              (sign(first) == sign(second)),  'same'
                                       ,ifelse(is.infinite(first) &
                                              is.infinite(second) & 
                                              (sign(first) != sign(second)),  'sameABS'
                                     ,ifelse(!is.na(diff) & 
                                              abs(diff) <= zeroFudge,         'same'
                                      ,ifelse(!is.na(diff) & 
                                              abs(abs(first) - abs(second)) <= zeroFudge, 'sameABS'
                                                                             ,'diff'
                                       ))))))
                               ,ifelse(!inFirst & !inSecond,                  'impossible'
                                                                             ,'beyondImpossible'
                                ))))
                    ,type2 =    ifelse(type == 'same',  paste('same', classFirst, sep=':')
                               ,ifelse(type == 'only1', paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2', paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff',  paste(classFirst, classSecond, sep=':')
                                                       ,type
                                ))))
                    ) %>%
              select(all_of(byVars), column, first, second, diff, type, type2)           
        return(rc)
    }
    
    compareText <- function(dd) {
        # compare character values. If both character strings that can be
        # interpreted as numeric values, then cast them to numeric and compare
        # them as such.
        rc <- dd %>%
              mutate(classFirst  = classCharacter(first)
                    ,classSecond = classCharacter(second)
                    ,compareAsNumbers = (classFirst %in% c('number','zero','zeroish')  | first %in% altNumericStrings) &
                                        (classSecond %in% c('number','zero','zeroish') | second %in% altNumericStrings)
                    ,diff =    ifelse(compareAsNumbers
                                     ,suppressWarnings( as.numeric(second) -  as.numeric(first) )
                                     ,suppressWarnings( as.numeric(second != first) )
                                     )
                    ,absDiff = ifelse(compareAsNumbers
                                     ,suppressWarnings( abs(as.numeric(second)) - abs(as.numeric(first)) )
                                     ,suppressWarnings( as.numeric(second != first) )
                                     )
                    ,sameAbs = as.logical(second == first) 
                    ,sameci =  !sameAbs & as.logical(toupper(second) == toupper(first))
                    ,samesi =  !sameAbs & as.logical(trimws(second) == trimws(first))
                    ,samecsi = !sameAbs & !sameci & !samesi & 
                               as.logical(trimws(toupper(second)) == trimws(toupper(first)))
                    ,type =     ifelse(inFirst & !inSecond,                        'only1'
                               ,ifelse(!inFirst & inSecond,                        'only2'
                               ,ifelse(inFirst & inSecond,  
                                       # compare as characters if appropriate
                                       ifelse(is.na(first) & is.na(second),        'same'
                                      ,ifelse(sameAbs %in% TRUE,                   'same'
                                      ,ifelse(samecsi %in% TRUE,                   'sameCSI'
                                      ,ifelse(sameci %in% TRUE,                    'sameCI'
                                      ,ifelse(samesi %in% TRUE,                    'sameSI'
                                      ,ifelse(compareAsNumbers,
                                              # compare as numeric values
                                              ifelse(first %in% c(NA,'NA') & 
                                                     second %in% c(NA,'NA'),       'same'
                                             ,ifelse(first %in% c('nan','NaN') & 
                                                     second %in% c('nan','NaN'),   'same'
                                             ,ifelse(first %in% 'inf' &
                                                     second %in% 'inf' | 
                                                     first %in% '+inf' &
                                                     second %in% '+inf' | 
                                                     first %in% '-inf' &
                                                     second %in% '-inf',           'same'
                                             ,ifelse(grepl('inf', first) &
                                                     grepl('inf', second),         'sameABS'
                                             ,ifelse(!is.na(diff) & 
                                                     compareAsNumbers &
                                                     suppressWarnings( abs(diff) <= zeroFudge )
                                                    ,                              'same'
                                             ,ifelse(!is.na(absDiff) & 
                                                     compareAsNumbers &
                                                     suppressWarnings( abs(absDiff) <= zeroFudge ),    'sameABS'
                                                                                  ,'diff'
                                              ))))) )
                                             ,'diff'
                                       ))))))
                               ,ifelse(!inFirst & !inSecond, 'absent', 'bigError'
                                ))))
                    ,type2 =    ifelse(type == 'same',    paste('same', classFirst, sep=':')
                               ,ifelse(type == 'sameCI',  paste('sameCI', classFirst, sep=':')
                               ,ifelse(type == 'sameSI',  paste('sameSI', classFirst, sep=':')
                               ,ifelse(type == 'sameCSI', paste('sameCSI', classFirst, sep=':')
                               ,ifelse(type == 'sameABS', paste('sameABS', classFirst, sep=':')
                               ,ifelse(type == 'only1',   paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2',   paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff',    paste(classFirst, classSecond, sep=':')
                                      ,type
                                ))))))))
                    ) %>%
              select(all_of(byVars), column, first, second, diff, type, type2)           
              
    }
    
    for(thisVar in sort(c(only1Vars, only2Vars, compareVars))) {
        if(thisVar %in% only1Vars)
            thisComparison <- df1[c(byVars, thisVar)] %>%
                              dplyr::rename(first=all_of(thisVar)) %>%
                              mutate(second = absentValue
                                    ,column = thisVar
                                    ,diff = absentValue
                                    ,type = absentValue
                                    ,type2 = absentValue
                                    ) %>%
                              select(all_of(byVars), column, first, second, diff, type, type2)
        else if (thisVar %in% only2Vars) {
            thisComparison <- df2[c(byVars, thisVar)] %>%
                              dplyr::rename(second=all_of(thisVar)) %>%
                              mutate(first = absentValue
                                    ,column = thisVar
                                    ,diff = absentValue
                                    ,type = absentValue
                                    ,type2 = absentValue
                                    ) %>%
                              select(all_of(byVars), column, first, second, diff, type, type2)
        } else {
            bothValues <- merge(df1 %>% select(all_of(byVars), all_of(thisVar)) %>%
                                dplyr::rename(first=all_of(thisVar)) %>%
                                mutate(inFirst=TRUE)
                               ,df2 %>% select(all_of(byVars), all_of(thisVar)) %>%
                                dplyr::rename(second=all_of(thisVar)) %>%
                                mutate(inSecond=TRUE)
                               ,by = byVars
                               ,all = TRUE
                               ) %>%
                          mutate(inFirst =  inFirst %in% TRUE
                                ,inSecond = inSecond %in% TRUE
                                ,column = thisVar
                                ) %>%
                          mutate(first = ifelse(!inFirst & inSecond, absentValue, first)
                                ,second = ifelse(inFirst & !inSecond, absentValue, second)
                                )
            if (all(is.number(df1[[thisVar]]) | is.na(df1[[thisVar]]) | is.nan(df1[[thisVar]]) | is.infinite(df1[[thisVar]])) & 
                all(is.number(df2[[thisVar]]) | is.na(df2[[thisVar]]) | is.nan(df2[[thisVar]]) | is.infinite(df2[[thisVar]]))
               ) {
                thisComparison <- compareNumbers(bothValues)
            }else {
                thisComparison <- compareText(bothValues)
            }
        }
        
        if(is.null(allDiffs))
            allDiffs <- thisComparison
        else
            allDiffs <- rbind(allDiffs
                             ,thisComparison %>% select(all_of(names(allDiffs)))
                             )
    }
    
    allDiffs <- allDiffs %>% 
                arrange(across(all_of(byVars)), column) %>% 
                data.frame()
    return(allDiffs)        
}

dfDifferencesTest <- function()
# unit test for dfDifferences
{
    # test case with identical dataframes
    testData <- dfDifferencesTest.baseData()
    expected <- dfDifferencesTest.baseDifferences(testData)
    actual <- dfDifferences(testData, testData, c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when dataframes are identical")
    
    # test case with dataframes with many differences
    dfDifferencesTest.explicitTypes()

    # Explicitly exercise each type and type2
    dfDifferencesTest.explicitTypes()
    
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
                ,type2 = ifelse(column=='cc', 'same:genl', 'same:number')
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
                             ,ifelse (aa=='a' & bb==1 & column=='rr', 'same:number'   #'same:genl'
                             ,ifelse (aa=='a' & bb==2 & column=='rr', 'number:number' #'genl:genl'
                             ,ifelse (aa=='a' & bb==3 & column=='rr', 'number:na'     #'genl:na'
                             ,ifelse (aa=='b' & bb==1 & column=='rr', 'number:+inf'   #'genl:+inf'
                             ,ifelse (aa=='b' & bb==2 & column=='rr', 'number:-inf'   #'genl:-inf'
                             ,ifelse (aa=='b' & bb==3 & column=='rr', 'number:nan'    #'genl:nan'
                             ,ifelse (aa=='c' & bb==1 & column=='rr', 'only1:number'  #'only1:genl'
                             #                                       , 'same:genl'
                             ,ifelse(column=='cc', 'same:genl', 'same:number')
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
                            ,type2 = ifelse(column=='cc', 'only2:genl', 'only2:number') #'only2:genl'
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

dfDifferencesTest.explicitTypes <- function()
# Explicitly exercise each type and type2
{
    # create set of values for each character and numeric class. This may involve
    # multiple examples for a class.
    tc <- textConnection("value    class 
                          ''       blank
                          '   '    spaces
                          '  \t'   spaces
                          'abc'    genl
                          'abc '   genl
                          'ABC'    genl
                          ' abc \t' genl
                          'Inf'    inf
                          '-Inf'   -inf
                          'NaN'    nan
                          'NA'     na
                          0        zero
                          1e-16    zero
                          -1e-16   zero
                          1e-10    zeroish
                          -1e-10   zeroish
                          1e-4     number
                          -1e-4    number
                          -56      number
                          -56.0000000000000001  number
                          -56.0000000001  number
                         ")
    valuesClasses <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    rm(tc)
    
    columnValue <- function(v1, v2, cname) {
        rc <- subset(dfDifferencesTest.expectedColumnValues()
                    ,first %in% v1 & second %in% v2
                    )[[cname]]
        return(rc)

    }
    df1 <- data.frame(key=1:nrow(valuesClasses)^2
                     ,x=rep(valuesClasses$value, times=nrow(valuesClasses))
                     ,stringsAsFactors=FALSE
                     ) %>%
           rbind(data.frame(key=nrow(valuesClasses)^2 + 1 # for only1 type
                           ,x = 'abc'
                           ,stringsAsFactors=FALSE
                           )
                )
    df2 <- data.frame(key=1:nrow(valuesClasses)^2
                     ,x=rep(valuesClasses$value, each=nrow(valuesClasses))
                     ,stringsAsFactors=FALSE
                     ) %>%
           rbind(data.frame(key=nrow(valuesClasses)^2 + 2 # for only2 type
                           ,x = 'ABC'
                           ,stringsAsFactors=FALSE
                           )
                )
    
    expected <- merge(df1 %>% dplyr::rename(first=x)    # Create expected results 
                     ,df2 %>% dplyr::rename(second=x)   # based on test data
                     ,by='key'
                     ,all=TRUE
                     ) %>%
                mutate(column = 'x'
                      ,first = ifelse(key %in% c(442,443) & is.na(first), '<absent>', first)
                      ,second = ifelse(key %in% c(442,443) & is.na(second), '<absent>', second)
                      # ,diff = ifelse(first == second #| 
                      #                #is.na(first) & is.na(second) |
                      #                #is.nan(first) & is.nan(second) |
                      #                #(is.infinite(first) & is.infinite(second) & 
                      #                # sign(first) == sign(second)
                      #                #)
                      #               ,0
                      #               ,1
                      #               )
                      # ,type = type1Determination(first, second)
                      # ,type2 = type2Determination(first, second)
                      ,diff = columnValue(first, second, 'diff')
                      ,type = columnValue(first, second, 'type')
                      ,type2 = columnValue(first, second, 'type2')
                      )
    actual <- dfDifferences(df1, df2, c('key'), zeroFudge=1e-15, zeroish = 1e-8)
    dd <- dfDifferences(expected, actual, c('key','column'), zeroFudge=1e-14, zeroish = 1e-8)
    checkTrue(all(dd$type %in% 'same'), "Unexpected result when exercising all class combinations")
}

dfDifferencesTest.expectedColumnValues <- function()
# Returns the expected values for the comparison of test values as a dataframe.
# The contents of the text connection was based on the following code and
# hand checked:
# actual %>% 
#   with(sprintf("                         %-22s %-22s %-23s %-7s %-20s\n"
#       ,paste0("'",first,"'"), paste0("'",second,"'"), paste0("'",diff,"'")
#       ,paste0("'",type,"'"), paste0("'",type2,"'")
#       )) %>% 
#   cat()
{
    tc <- textConnection("first                  second                 diff                    type    type2 
                          ''                     ''                     '0'                     'same'  'same:blank'        
                          '   '                  ''                     '1'                     'sameSI' 'sameSI:spaces'     
                          '  \t'                 ''                     '1'                     'sameSI' 'sameSI:spaces'     
                          'abc'                  ''                     '1'                     'diff'  'genl:blank'        
                          'abc '                 ''                     '1'                     'diff'  'genl:blank'        
                          'ABC'                  ''                     '1'                     'diff'  'genl:blank'        
                          ' abc 	'               ''                     '1'                     'diff'  'genl:blank'        
                          'Inf'                  ''                     '1'                     'diff'  'genl:blank'        
                          '-Inf'                 ''                     '1'                     'diff'  'genl:blank'        
                          'NaN'                  ''                     '1'                     'diff'  'genl:blank'        
                          'NA'                   ''                     'NA'                    'diff'  'na:blank'          
                          '0'                    ''                     '1'                     'diff'  'zero:blank'        
                          '1e-16'                ''                     '1'                     'diff'  'zero:blank'        
                          '-1e-16'               ''                     '1'                     'diff'  'zero:blank'        
                          '1e-10'                ''                     '1'                     'diff'  'zeroish:blank'     
                          '-1e-10'               ''                     '1'                     'diff'  'zeroish:blank'     
                          '1e-4'                 ''                     '1'                     'diff'  'number:blank'      
                          '-1e-4'                ''                     '1'                     'diff'  'number:blank'      
                          '-56'                  ''                     '1'                     'diff'  'number:blank'      
                          '-56.0000000000000001' ''                     '1'                     'diff'  'number:blank'      
                          '-56.0000000001'       ''                     '1'                     'diff'  'number:blank'      
                          ''                     '   '                  '1'                     'sameSI' 'sameSI:blank'      
                          '   '                  '   '                  '0'                     'same'  'same:spaces'       
                          '  \t'                 '   '                  '1'                     'sameSI' 'sameSI:spaces'     
                          'abc'                  '   '                  '1'                     'diff'  'genl:spaces'       
                          'abc '                 '   '                  '1'                     'diff'  'genl:spaces'       
                          'ABC'                  '   '                  '1'                     'diff'  'genl:spaces'       
                          ' abc \t'               '   '                  '1'                     'diff'  'genl:spaces'       
                          'Inf'                  '   '                  '1'                     'diff'  'genl:spaces'       
                          '-Inf'                 '   '                  '1'                     'diff'  'genl:spaces'       
                          'NaN'                  '   '                  '1'                     'diff'  'genl:spaces'       
                          'NA'                   '   '                  'NA'                    'diff'  'na:spaces'         
                          '0'                    '   '                  '1'                     'diff'  'zero:spaces'       
                          '1e-16'                '   '                  '1'                     'diff'  'zero:spaces'       
                          '-1e-16'               '   '                  '1'                     'diff'  'zero:spaces'       
                          '1e-10'                '   '                  '1'                     'diff'  'zeroish:spaces'    
                          '-1e-10'               '   '                  '1'                     'diff'  'zeroish:spaces'    
                          '1e-4'                 '   '                  '1'                     'diff'  'number:spaces'     
                          '-1e-4'                '   '                  '1'                     'diff'  'number:spaces'     
                          '-56'                  '   '                  '1'                     'diff'  'number:spaces'     
                          '-56.0000000000000001' '   '                  '1'                     'diff'  'number:spaces'     
                          '-56.0000000001'       '   '                  '1'                     'diff'  'number:spaces'     
                          ''                     '  \t'                 '1'                     'sameSI' 'sameSI:blank'      
                          '   '                  '  \t'                  '1'                     'sameSI' 'sameSI:spaces'     
                          '  \t'                 '  \t'                  '0'                     'same'  'same:spaces'       
                          'abc'                  '  \t'                  '1'                     'diff'  'genl:spaces'       
                          'abc '                 '  \t'                  '1'                     'diff'  'genl:spaces'       
                          'ABC'                  '  \t'                  '1'                     'diff'  'genl:spaces'       
                          ' abc \t'               '  \t'                  '1'                     'diff'  'genl:spaces'       
                          'Inf'                  '  \t'                  '1'                     'diff'  'genl:spaces'       
                          '-Inf'                 '  \t'                  '1'                     'diff'  'genl:spaces'       
                          'NaN'                  '  \t'                  '1'                     'diff'  'genl:spaces'       
                          'NA'                   '  \t'                  'NA'                    'diff'  'na:spaces'         
                          '0'                    '  \t'                  '1'                     'diff'  'zero:spaces'       
                          '1e-16'                '  \t'                  '1'                     'diff'  'zero:spaces'       
                          '-1e-16'               '  \t'                  '1'                     'diff'  'zero:spaces'       
                          '1e-10'                '  \t'                  '1'                     'diff'  'zeroish:spaces'    
                          '-1e-10'               '  \t'                  '1'                     'diff'  'zeroish:spaces'    
                          '1e-4'                 '  \t'                  '1'                     'diff'  'number:spaces'     
                          '-1e-4'                '  \t'                  '1'                     'diff'  'number:spaces'     
                          '-56'                  '  \t'                  '1'                     'diff'  'number:spaces'     
                          '-56.0000000000000001' '  \t'                  '1'                     'diff'  'number:spaces'     
                          '-56.0000000001'       '  \t'                  '1'                     'diff'  'number:spaces'     
                          ''                     'abc'                  '1'                     'diff'  'blank:genl'        
                          '   '                  'abc'                  '1'                     'diff'  'spaces:genl'       
                          '  \t'                 'abc'                  '1'                     'diff'  'spaces:genl'       
                          'abc'                  'abc'                  '0'                     'same'  'same:genl'         
                          'abc '                 'abc'                  '1'                     'sameSI' 'sameSI:genl'       
                          'ABC'                  'abc'                  '1'                     'sameCI' 'sameCI:genl'       
                          ' abc \t'               'abc'                  '1'                     'sameSI' 'sameSI:genl'       
                          'Inf'                  'abc'                  '1'                     'diff'  'genl:genl'         
                          '-Inf'                 'abc'                  '1'                     'diff'  'genl:genl'         
                          'NaN'                  'abc'                  '1'                     'diff'  'genl:genl'         
                          'NA'                   'abc'                  'NA'                    'diff'  'na:genl'           
                          '0'                    'abc'                  '1'                     'diff'  'zero:genl'         
                          '1e-16'                'abc'                  '1'                     'diff'  'zero:genl'         
                          '-1e-16'               'abc'                  '1'                     'diff'  'zero:genl'         
                          '1e-10'                'abc'                  '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               'abc'                  '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 'abc'                  '1'                     'diff'  'number:genl'       
                          '-1e-4'                'abc'                  '1'                     'diff'  'number:genl'       
                          '-56'                  'abc'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' 'abc'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       'abc'                  '1'                     'diff'  'number:genl'       
                          ''                     'abc '                 '1'                     'diff'  'blank:genl'        
                          '   '                  'abc '                 '1'                     'diff'  'spaces:genl'       
                          '  \t'                 'abc '                 '1'                     'diff'  'spaces:genl'       
                          'abc'                  'abc '                 '1'                     'sameSI' 'sameSI:genl'       
                          'abc '                 'abc '                 '0'                     'same'  'same:genl'         
                          'ABC'                  'abc '                 '1'                     'sameCSI' 'sameCSI:genl'      
                          ' abc \t'               'abc '                 '1'                     'sameSI' 'sameSI:genl'       
                          'Inf'                  'abc '                 '1'                     'diff'  'genl:genl'         
                          '-Inf'                 'abc '                 '1'                     'diff'  'genl:genl'         
                          'NaN'                  'abc '                 '1'                     'diff'  'genl:genl'         
                          'NA'                   'abc '                 'NA'                    'diff'  'na:genl'           
                          '0'                    'abc '                 '1'                     'diff'  'zero:genl'         
                          '1e-16'                'abc '                 '1'                     'diff'  'zero:genl'         
                          '-1e-16'               'abc '                 '1'                     'diff'  'zero:genl'         
                          '1e-10'                'abc '                 '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               'abc '                 '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 'abc '                 '1'                     'diff'  'number:genl'       
                          '-1e-4'                'abc '                 '1'                     'diff'  'number:genl'       
                          '-56'                  'abc '                 '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' 'abc '                 '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       'abc '                 '1'                     'diff'  'number:genl'       
                          ''                     'ABC'                  '1'                     'diff'  'blank:genl'        
                          '   '                  'ABC'                  '1'                     'diff'  'spaces:genl'       
                          '  \t'                 'ABC'                  '1'                     'diff'  'spaces:genl'       
                          'abc'                  'ABC'                  '1'                     'sameCI' 'sameCI:genl'       
                          'abc '                 'ABC'                  '1'                     'sameCSI' 'sameCSI:genl'      
                          'ABC'                  'ABC'                  '0'                     'same'  'same:genl'         
                          ' abc \t'               'ABC'                  '1'                     'sameCSI' 'sameCSI:genl'      
                          'Inf'                  'ABC'                  '1'                     'diff'  'genl:genl'         
                          '-Inf'                 'ABC'                  '1'                     'diff'  'genl:genl'         
                          'NaN'                  'ABC'                  '1'                     'diff'  'genl:genl'         
                          'NA'                   'ABC'                  'NA'                    'diff'  'na:genl'           
                          '0'                    'ABC'                  '1'                     'diff'  'zero:genl'         
                          '1e-16'                'ABC'                  '1'                     'diff'  'zero:genl'         
                          '-1e-16'               'ABC'                  '1'                     'diff'  'zero:genl'         
                          '1e-10'                'ABC'                  '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               'ABC'                  '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 'ABC'                  '1'                     'diff'  'number:genl'       
                          '-1e-4'                'ABC'                  '1'                     'diff'  'number:genl'       
                          '-56'                  'ABC'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' 'ABC'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       'ABC'                  '1'                     'diff'  'number:genl'       
                          ''                     ' abc \t'               '1'                     'diff'  'blank:genl'        
                          '   '                  ' abc \t'               '1'                     'diff'  'spaces:genl'       
                          '  \t'                 ' abc \t'               '1'                     'diff'  'spaces:genl'       
                          'abc'                  ' abc \t'               '1'                     'sameSI' 'sameSI:genl'       
                          'abc '                 ' abc \t'               '1'                     'sameSI' 'sameSI:genl'       
                          'ABC'                  ' abc \t'               '1'                     'sameCSI' 'sameCSI:genl'      
                          ' abc \t'               ' abc \t'               '0'                     'same'  'same:genl'         
                          'Inf'                  ' abc \t'               '1'                     'diff'  'genl:genl'         
                          '-Inf'                 ' abc \t'               '1'                     'diff'  'genl:genl'         
                          'NaN'                  ' abc \t'               '1'                     'diff'  'genl:genl'         
                          'NA'                   ' abc \t'               'NA'                    'diff'  'na:genl'           
                          '0'                    ' abc \t'               '1'                     'diff'  'zero:genl'         
                          '1e-16'                ' abc \t'               '1'                     'diff'  'zero:genl'         
                          '-1e-16'               ' abc \t'               '1'                     'diff'  'zero:genl'         
                          '1e-10'                ' abc \t'               '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               ' abc \t'               '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 ' abc \t'               '1'                     'diff'  'number:genl'       
                          '-1e-4'                ' abc \t'               '1'                     'diff'  'number:genl'       
                          '-56'                  ' abc \t'               '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' ' abc \t'               '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       ' abc \t'               '1'                     'diff'  'number:genl'       
                          ''                     'Inf'                  '1'                     'diff'  'blank:genl'        
                          '   '                  'Inf'                  '1'                     'diff'  'spaces:genl'       
                          '  \t'                 'Inf'                  '1'                     'diff'  'spaces:genl'       
                          'abc'                  'Inf'                  '1'                     'diff'  'genl:genl'         
                          'abc '                 'Inf'                  '1'                     'diff'  'genl:genl'         
                          'ABC'                  'Inf'                  '1'                     'diff'  'genl:genl'         
                          ' abc \t'               'Inf'                  '1'                     'diff'  'genl:genl'         
                          'Inf'                  'Inf'                  '0'                     'same'  'same:genl'         
                          '-Inf'                 'Inf'                  '1'                     'diff'  'genl:genl'         
                          'NaN'                  'Inf'                  '1'                     'diff'  'genl:genl'         
                          'NA'                   'Inf'                  'NA'                    'diff'  'na:genl'           
                          '0'                    'Inf'                  '1'                     'diff'  'zero:genl'         
                          '1e-16'                'Inf'                  '1'                     'diff'  'zero:genl'         
                          '-1e-16'               'Inf'                  '1'                     'diff'  'zero:genl'         
                          '1e-10'                'Inf'                  '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               'Inf'                  '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 'Inf'                  '1'                     'diff'  'number:genl'       
                          '-1e-4'                'Inf'                  '1'                     'diff'  'number:genl'       
                          '-56'                  'Inf'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' 'Inf'                  '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       'Inf'                  '1'                     'diff'  'number:genl'       
                          ''                     '-Inf'                 '1'                     'diff'  'blank:genl'        
                          '   '                  '-Inf'                 '1'                     'diff'  'spaces:genl'       
                          '  \t'                 '-Inf'                 '1'                     'diff'  'spaces:genl'       
                          'abc'                  '-Inf'                 '1'                     'diff'  'genl:genl'         
                          'abc '                 '-Inf'                 '1'                     'diff'  'genl:genl'         
                          'ABC'                  '-Inf'                 '1'                     'diff'  'genl:genl'         
                          ' abc \t'               '-Inf'                 '1'                     'diff'  'genl:genl'         
                          'Inf'                  '-Inf'                 '1'                     'diff'  'genl:genl'         
                          '-Inf'                 '-Inf'                 '0'                     'same'  'same:genl'         
                          'NaN'                  '-Inf'                 '1'                     'diff'  'genl:genl'         
                          'NA'                   '-Inf'                 'NA'                    'diff'  'na:genl'           
                          '0'                    '-Inf'                 '1'                     'diff'  'zero:genl'         
                          '1e-16'                '-Inf'                 '1'                     'diff'  'zero:genl'         
                          '-1e-16'               '-Inf'                 '1'                     'diff'  'zero:genl'         
                          '1e-10'                '-Inf'                 '1'                     'diff'  'zeroish:genl'      
                          '-1e-10'               '-Inf'                 '1'                     'diff'  'zeroish:genl'      
                          '1e-4'                 '-Inf'                 '1'                     'diff'  'number:genl'       
                          '-1e-4'                '-Inf'                 '1'                     'diff'  'number:genl'       
                          '-56'                  '-Inf'                 '1'                     'diff'  'number:genl'       
                          '-56.0000000000000001' '-Inf'                 '1'                     'diff'  'number:genl'       
                          '-56.0000000001'       '-Inf'                 '1'                     'diff'  'number:genl'       
                          ''                     'NaN'                  '1'                     'diff'  'blank:genl'        
                          '   '                  'NaN'                  '1'                     'diff'  'spaces:genl'       
                          '  \t'                 'NaN'                  '1'                     'diff'  'spaces:genl'       
                          'abc'                  'NaN'                  '1'                     'diff'  'genl:genl'         
                          'abc '                 'NaN'                  '1'                     'diff'  'genl:genl'         
                          'ABC'                  'NaN'                  '1'                     'diff'  'genl:genl'         
                          ' abc \t'               'NaN'                  '1'                     'diff'  'genl:genl'         
                          'Inf'                  'NaN'                  '1'                     'diff'  'genl:genl'         
                          '-Inf'                 'NaN'                  '1'                     'diff'  'genl:genl'         
                          'NaN'                  'NaN'                  'NaN'                   'same'  'same:genl'         
                          'NA'                   'NaN'                  'NA'                    'diff'  'na:genl'           
                          '0'                    'NaN'                  'NaN'                   'diff'  'zero:genl'         
                          '1e-16'                'NaN'                  'NaN'                   'diff'  'zero:genl'         
                          '-1e-16'               'NaN'                  'NaN'                   'diff'  'zero:genl'         
                          '1e-10'                'NaN'                  'NaN'                   'diff'  'zeroish:genl'      
                          '-1e-10'               'NaN'                  'NaN'                   'diff'  'zeroish:genl'      
                          '1e-4'                 'NaN'                  'NaN'                   'diff'  'number:genl'       
                          '-1e-4'                'NaN'                  'NaN'                   'diff'  'number:genl'       
                          '-56'                  'NaN'                  'NaN'                   'diff'  'number:genl'       
                          '-56.0000000000000001' 'NaN'                  'NaN'                   'diff'  'number:genl'       
                          '-56.0000000001'       'NaN'                  'NaN'                   'diff'  'number:genl'       
                          ''                     'NA'                   'NA'                    'diff'  'blank:na'          
                          '   '                  'NA'                   'NA'                    'diff'  'spaces:na'         
                          '  \t'                 'NA'                   'NA'                    'diff'  'spaces:na'         
                          'abc'                  'NA'                   'NA'                    'diff'  'genl:na'           
                          'abc '                 'NA'                   'NA'                    'diff'  'genl:na'           
                          'ABC'                  'NA'                   'NA'                    'diff'  'genl:na'           
                          ' abc \t'               'NA'                   'NA'                    'diff'  'genl:na'           
                          'Inf'                  'NA'                   'NA'                    'diff'  'genl:na'           
                          '-Inf'                 'NA'                   'NA'                    'diff'  'genl:na'           
                          'NaN'                  'NA'                   'NA'                    'diff'  'genl:na'           
                          'NA'                   'NA'                   'NA'                    'same'  'same:na'           
                          '0'                    'NA'                   'NA'                    'diff'  'zero:na'           
                          '1e-16'                'NA'                   'NA'                    'diff'  'zero:na'           
                          '-1e-16'               'NA'                   'NA'                    'diff'  'zero:na'           
                          '1e-10'                'NA'                   'NA'                    'diff'  'zeroish:na'        
                          '-1e-10'               'NA'                   'NA'                    'diff'  'zeroish:na'        
                          '1e-4'                 'NA'                   'NA'                    'diff'  'number:na'         
                          '-1e-4'                'NA'                   'NA'                    'diff'  'number:na'         
                          '-56'                  'NA'                   'NA'                    'diff'  'number:na'         
                          '-56.0000000000000001' 'NA'                   'NA'                    'diff'  'number:na'         
                          '-56.0000000001'       'NA'                   'NA'                    'diff'  'number:na'         
                          ''                     '0'                    '1'                     'diff'  'blank:zero'        
                          '   '                  '0'                    '1'                     'diff'  'spaces:zero'       
                          '  \t'                 '0'                    '1'                     'diff'  'spaces:zero'       
                          'abc'                  '0'                    '1'                     'diff'  'genl:zero'         
                          'abc '                 '0'                    '1'                     'diff'  'genl:zero'         
                          'ABC'                  '0'                    '1'                     'diff'  'genl:zero'         
                          ' abc \t'               '0'                    '1'                     'diff'  'genl:zero'         
                          'Inf'                  '0'                    '1'                     'diff'  'genl:zero'         
                          '-Inf'                 '0'                    '1'                     'diff'  'genl:zero'         
                          'NaN'                  '0'                    'NaN'                   'diff'  'genl:zero'         
                          'NA'                   '0'                    'NA'                    'diff'  'na:zero'           
                          '0'                    '0'                    '0'                     'same'  'same:zero'         
                          '1e-16'                '0'                    '-1e-16'                'same'  'same:zero'         
                          '-1e-16'               '0'                    '1e-16'                 'same'  'same:zero'         
                          '1e-10'                '0'                    '-1e-10'                'diff'  'zeroish:zero'      
                          '-1e-10'               '0'                    '1e-10'                 'diff'  'zeroish:zero'      
                          '1e-4'                 '0'                    '-1e-04'                'diff'  'number:zero'       
                          '-1e-4'                '0'                    '1e-04'                 'diff'  'number:zero'       
                          '-56'                  '0'                    '56'                    'diff'  'number:zero'       
                          '-56.0000000000000001' '0'                    '56'                    'diff'  'number:zero'       
                          '-56.0000000001'       '0'                    '56.0000000001'         'diff'  'number:zero'       
                          ''                     '1e-16'                '1'                     'diff'  'blank:zero'        
                          '   '                  '1e-16'                '1'                     'diff'  'spaces:zero'       
                          '  \t'                 '1e-16'                '1'                     'diff'  'spaces:zero'       
                          'abc'                  '1e-16'                '1'                     'diff'  'genl:zero'         
                          'abc '                 '1e-16'                '1'                     'diff'  'genl:zero'         
                          'ABC'                  '1e-16'                '1'                     'diff'  'genl:zero'         
                          ' abc \t'               '1e-16'                '1'                     'diff'  'genl:zero'         
                          'Inf'                  '1e-16'                '1'                     'diff'  'genl:zero'         
                          '-Inf'                 '1e-16'                '1'                     'diff'  'genl:zero'         
                          'NaN'                  '1e-16'                'NaN'                   'diff'  'genl:zero'         
                          'NA'                   '1e-16'                'NA'                    'diff'  'na:zero'           
                          '0'                    '1e-16'                '1e-16'                 'same'  'same:zero'         
                          '1e-16'                '1e-16'                '0'                     'same'  'same:zero'         
                          '-1e-16'               '1e-16'                '2e-16'                 'same'  'same:zero'         
                          '1e-10'                '1e-16'                '-9.99999e-11'          'diff'  'zeroish:zero'      
                          '-1e-10'               '1e-16'                '1.000001e-10'          'diff'  'zeroish:zero'      
                          '1e-4'                 '1e-16'                '-9.99999999999e-05'    'diff'  'number:zero'       
                          '-1e-4'                '1e-16'                '0.0001000000000001'    'diff'  'number:zero'       
                          '-56'                  '1e-16'                '56'                    'diff'  'number:zero'       
                          '-56.0000000000000001' '1e-16'                '56'                    'diff'  'number:zero'       
                          '-56.0000000001'       '1e-16'                '56.0000000001'         'diff'  'number:zero'       
                          ''                     '-1e-16'               '1'                     'diff'  'blank:zero'        
                          '   '                  '-1e-16'               '1'                     'diff'  'spaces:zero'       
                          '  \t'                 '-1e-16'               '1'                     'diff'  'spaces:zero'       
                          'abc'                  '-1e-16'               '1'                     'diff'  'genl:zero'         
                          'abc '                 '-1e-16'               '1'                     'diff'  'genl:zero'         
                          'ABC'                  '-1e-16'               '1'                     'diff'  'genl:zero'         
                          ' abc \t'               '-1e-16'               '1'                     'diff'  'genl:zero'         
                          'Inf'                  '-1e-16'               '1'                     'diff'  'genl:zero'         
                          '-Inf'                 '-1e-16'               '1'                     'diff'  'genl:zero'         
                          'NaN'                  '-1e-16'               'NaN'                   'diff'  'genl:zero'         
                          'NA'                   '-1e-16'               'NA'                    'diff'  'na:zero'           
                          '0'                    '-1e-16'               '-1e-16'                'same'  'same:zero'         
                          '1e-16'                '-1e-16'               '-2e-16'                'same'  'same:zero'         
                          '-1e-16'               '-1e-16'               '0'                     'same'  'same:zero'         
                          '1e-10'                '-1e-16'               '-1.000001e-10'         'diff'  'zeroish:zero'      
                          '-1e-10'               '-1e-16'               '9.99999e-11'           'diff'  'zeroish:zero'      
                          '1e-4'                 '-1e-16'               '-0.0001000000000001'   'diff'  'number:zero'       
                          '-1e-4'                '-1e-16'               '9.99999999999e-05'     'diff'  'number:zero'       
                          '-56'                  '-1e-16'               '56'                    'diff'  'number:zero'       
                          '-56.0000000000000001' '-1e-16'               '56'                    'diff'  'number:zero'       
                          '-56.0000000001'       '-1e-16'               '56.0000000001'         'diff'  'number:zero'       
                          ''                     '1e-10'                '1'                     'diff'  'blank:zeroish'     
                          '   '                  '1e-10'                '1'                     'diff'  'spaces:zeroish'    
                          '  \t'                 '1e-10'                '1'                     'diff'  'spaces:zeroish'    
                          'abc'                  '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          'abc '                 '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          'ABC'                  '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          ' abc \t'               '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          'Inf'                  '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          '-Inf'                 '1e-10'                '1'                     'diff'  'genl:zeroish'      
                          'NaN'                  '1e-10'                'NaN'                   'diff'  'genl:zeroish'      
                          'NA'                   '1e-10'                'NA'                    'diff'  'na:zeroish'        
                          '0'                    '1e-10'                '1e-10'                 'diff'  'zero:zeroish'      
                          '1e-16'                '1e-10'                '9.99999e-11'           'diff'  'zero:zeroish'      
                          '-1e-16'               '1e-10'                '1.000001e-10'          'diff'  'zero:zeroish'      
                          '1e-10'                '1e-10'                '0'                     'same'  'same:zeroish'      
                          '-1e-10'               '1e-10'                '2e-10'                 'sameABS' 'sameABS:zeroish'   
                          '1e-4'                 '1e-10'                '-9.99999e-05'          'diff'  'number:zeroish'    
                          '-1e-4'                '1e-10'                '0.0001000001'          'diff'  'number:zeroish'    
                          '-56'                  '1e-10'                '56.0000000001'         'diff'  'number:zeroish'    
                          '-56.0000000000000001' '1e-10'                '56.0000000001'         'diff'  'number:zeroish'    
                          '-56.0000000001'       '1e-10'                '56.0000000002'         'diff'  'number:zeroish'    
                          ''                     '-1e-10'               '1'                     'diff'  'blank:zeroish'     
                          '   '                  '-1e-10'               '1'                     'diff'  'spaces:zeroish'    
                          '  \t'                 '-1e-10'               '1'                     'diff'  'spaces:zeroish'    
                          'abc'                  '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          'abc '                 '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          'ABC'                  '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          ' abc \t'               '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          'Inf'                  '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          '-Inf'                 '-1e-10'               '1'                     'diff'  'genl:zeroish'      
                          'NaN'                  '-1e-10'               'NaN'                   'diff'  'genl:zeroish'      
                          'NA'                   '-1e-10'               'NA'                    'diff'  'na:zeroish'        
                          '0'                    '-1e-10'               '-1e-10'                'diff'  'zero:zeroish'      
                          '1e-16'                '-1e-10'               '-1.000001e-10'         'diff'  'zero:zeroish'      
                          '-1e-16'               '-1e-10'               '-9.99999e-11'          'diff'  'zero:zeroish'      
                          '1e-10'                '-1e-10'               '-2e-10'                'sameABS' 'sameABS:zeroish'   
                          '-1e-10'               '-1e-10'               '0'                     'same'  'same:zeroish'      
                          '1e-4'                 '-1e-10'               '-0.0001000001'         'diff'  'number:zeroish'    
                          '-1e-4'                '-1e-10'               '9.99999e-05'           'diff'  'number:zeroish'    
                          '-56'                  '-1e-10'               '55.9999999999'         'diff'  'number:zeroish'    
                          '-56.0000000000000001' '-1e-10'               '55.9999999999'         'diff'  'number:zeroish'    
                          '-56.0000000001'       '-1e-10'               '56'                    'diff'  'number:zeroish'    
                          ''                     '1e-4'                 '1'                     'diff'  'blank:number'      
                          '   '                  '1e-4'                 '1'                     'diff'  'spaces:number'     
                          '  \t'                 '1e-4'                 '1'                     'diff'  'spaces:number'     
                          'abc'                  '1e-4'                 '1'                     'diff'  'genl:number'       
                          'abc '                 '1e-4'                 '1'                     'diff'  'genl:number'       
                          'ABC'                  '1e-4'                 '1'                     'diff'  'genl:number'       
                          ' abc \t'               '1e-4'                 '1'                     'diff'  'genl:number'       
                          'Inf'                  '1e-4'                 '1'                     'diff'  'genl:number'       
                          '-Inf'                 '1e-4'                 '1'                     'diff'  'genl:number'       
                          'NaN'                  '1e-4'                 'NaN'                   'diff'  'genl:number'       
                          'NA'                   '1e-4'                 'NA'                    'diff'  'na:number'         
                          '0'                    '1e-4'                 '1e-04'                 'diff'  'zero:number'       
                          '1e-16'                '1e-4'                 '9.99999999999e-05'     'diff'  'zero:number'       
                          '-1e-16'               '1e-4'                 '0.0001000000000001'    'diff'  'zero:number'       
                          '1e-10'                '1e-4'                 '9.99999e-05'           'diff'  'zeroish:number'    
                          '-1e-10'               '1e-4'                 '0.0001000001'          'diff'  'zeroish:number'    
                          '1e-4'                 '1e-4'                 '0'                     'same'  'same:number'       
                          '-1e-4'                '1e-4'                 '2e-04'                 'sameABS' 'sameABS:number'    
                          '-56'                  '1e-4'                 '56.0001'               'diff'  'number:number'     
                          '-56.0000000000000001' '1e-4'                 '56.0001'               'diff'  'number:number'     
                          '-56.0000000001'       '1e-4'                 '56.0001000001'         'diff'  'number:number'     
                          ''                     '-1e-4'                '1'                     'diff'  'blank:number'      
                          '   '                  '-1e-4'                '1'                     'diff'  'spaces:number'     
                          '  \t'                 '-1e-4'                '1'                     'diff'  'spaces:number'     
                          'abc'                  '-1e-4'                '1'                     'diff'  'genl:number'       
                          'abc '                 '-1e-4'                '1'                     'diff'  'genl:number'       
                          'ABC'                  '-1e-4'                '1'                     'diff'  'genl:number'       
                          ' abc \t'               '-1e-4'                '1'                     'diff'  'genl:number'       
                          'Inf'                  '-1e-4'                '1'                     'diff'  'genl:number'       
                          '-Inf'                 '-1e-4'                '1'                     'diff'  'genl:number'       
                          'NaN'                  '-1e-4'                'NaN'                   'diff'  'genl:number'       
                          'NA'                   '-1e-4'                'NA'                    'diff'  'na:number'         
                          '0'                    '-1e-4'                '-1e-04'                'diff'  'zero:number'       
                          '1e-16'                '-1e-4'                '-0.0001000000000001'   'diff'  'zero:number'       
                          '-1e-16'               '-1e-4'                '-9.99999999999e-05'    'diff'  'zero:number'       
                          '1e-10'                '-1e-4'                '-0.0001000001'         'diff'  'zeroish:number'    
                          '-1e-10'               '-1e-4'                '-9.99999e-05'          'diff'  'zeroish:number'    
                          '1e-4'                 '-1e-4'                '-2e-04'                'sameABS' 'sameABS:number'    
                          '-1e-4'                '-1e-4'                '0'                     'same'  'same:number'       
                          '-56'                  '-1e-4'                '55.9999'               'diff'  'number:number'     
                          '-56.0000000000000001' '-1e-4'                '55.9999'               'diff'  'number:number'     
                          '-56.0000000001'       '-1e-4'                '55.9999000001'         'diff'  'number:number'     
                          ''                     '-56'                  '1'                     'diff'  'blank:number'      
                          '   '                  '-56'                  '1'                     'diff'  'spaces:number'     
                          '  \t'                 '-56'                  '1'                     'diff'  'spaces:number'     
                          'abc'                  '-56'                  '1'                     'diff'  'genl:number'       
                          'abc '                 '-56'                  '1'                     'diff'  'genl:number'       
                          'ABC'                  '-56'                  '1'                     'diff'  'genl:number'       
                          ' abc \t'               '-56'                  '1'                     'diff'  'genl:number'       
                          'Inf'                  '-56'                  '1'                     'diff'  'genl:number'       
                          '-Inf'                 '-56'                  '1'                     'diff'  'genl:number'       
                          'NaN'                  '-56'                  'NaN'                   'diff'  'genl:number'       
                          'NA'                   '-56'                  'NA'                    'diff'  'na:number'         
                          '0'                    '-56'                  '-56'                   'diff'  'zero:number'       
                          '1e-16'                '-56'                  '-56'                   'diff'  'zero:number'       
                          '-1e-16'               '-56'                  '-56'                   'diff'  'zero:number'       
                          '1e-10'                '-56'                  '-56.0000000001'        'diff'  'zeroish:number'    
                          '-1e-10'               '-56'                  '-55.9999999999'        'diff'  'zeroish:number'    
                          '1e-4'                 '-56'                  '-56.0001'              'diff'  'number:number'     
                          '-1e-4'                '-56'                  '-55.9999'              'diff'  'number:number'     
                          '-56'                  '-56'                  '0'                     'same'  'same:number'       
                          '-56.0000000000000001' '-56'                  '0'                     'same'  'same:number'       
                          '-56.0000000001'       '-56'                  '1.00001784630877e-10'  'diff'  'number:number'     
                          ''                     '-56.0000000000000001' '1'                     'diff'  'blank:number'      
                          '   '                  '-56.0000000000000001' '1'                     'diff'  'spaces:number'     
                          '  \t'                 '-56.0000000000000001' '1'                     'diff'  'spaces:number'     
                          'abc'                  '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          'abc '                 '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          'ABC'                  '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          ' abc \t'               '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          'Inf'                  '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          '-Inf'                 '-56.0000000000000001' '1'                     'diff'  'genl:number'       
                          'NaN'                  '-56.0000000000000001' 'NaN'                   'diff'  'genl:number'       
                          'NA'                   '-56.0000000000000001' 'NA'                    'diff'  'na:number'         
                          '0'                    '-56.0000000000000001' '-56'                   'diff'  'zero:number'       
                          '1e-16'                '-56.0000000000000001' '-56'                   'diff'  'zero:number'       
                          '-1e-16'               '-56.0000000000000001' '-56'                   'diff'  'zero:number'       
                          '1e-10'                '-56.0000000000000001' '-56.0000000001'        'diff'  'zeroish:number'    
                          '-1e-10'               '-56.0000000000000001' '-55.9999999999'        'diff'  'zeroish:number'    
                          '1e-4'                 '-56.0000000000000001' '-56.0001'              'diff'  'number:number'     
                          '-1e-4'                '-56.0000000000000001' '-55.9999'              'diff'  'number:number'     
                          '-56'                  '-56.0000000000000001' '0'                     'same'  'same:number'       
                          '-56.0000000000000001' '-56.0000000000000001' '0'                     'same'  'same:number'       
                          '-56.0000000001'       '-56.0000000000000001' '1.00001784630877e-10'  'diff'  'number:number'     
                          ''                     '-56.0000000001'       '1'                     'diff'  'blank:number'      
                          '   '                  '-56.0000000001'       '1'                     'diff'  'spaces:number'     
                          '  \t'                 '-56.0000000001'       '1'                     'diff'  'spaces:number'     
                          'abc'                  '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          'abc '                 '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          'ABC'                  '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          ' abc \t'               '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          'Inf'                  '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          '-Inf'                 '-56.0000000001'       '1'                     'diff'  'genl:number'       
                          'NaN'                  '-56.0000000001'       'NaN'                   'diff'  'genl:number'       
                          'NA'                   '-56.0000000001'       'NA'                    'diff'  'na:number'         
                          '0'                    '-56.0000000001'       '-56.0000000001'        'diff'  'zero:number'       
                          '1e-16'                '-56.0000000001'       '-56.0000000001'        'diff'  'zero:number'       
                          '-1e-16'               '-56.0000000001'       '-56.0000000001'        'diff'  'zero:number'       
                          '1e-10'                '-56.0000000001'       '-56.0000000002'        'diff'  'zeroish:number'    
                          '-1e-10'               '-56.0000000001'       '-56'                   'diff'  'zeroish:number'    
                          '1e-4'                 '-56.0000000001'       '-56.0001000001'        'diff'  'number:number'     
                          '-1e-4'                '-56.0000000001'       '-55.9999000001'        'diff'  'number:number'     
                          '-56'                  '-56.0000000001'       '-1.00001784630877e-10' 'diff'  'number:number'     
                          '-56.0000000000000001' '-56.0000000001'       '-1.00001784630877e-10' 'diff'  'number:number'     
                          '-56.0000000001'       '-56.0000000001'       '0'                     'same'  'same:number'       
                          'abc'                  '<absent>'             '1'                     'only1' 'only1:genl'        
                          '<absent>'             'ABC'                  '1'                     'only2' 'only2:genl'        
                         ")
    rc <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    rm(tc)  
    return(rc)
}


# end of file