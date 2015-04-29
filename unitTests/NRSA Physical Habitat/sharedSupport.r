# sharedSupport.r
# RUnit tests


countTest <- function() {
# Performs tests on count().

    # create test data.
    idx <- rep(c('foo','bar','baz'),17)
    n <- runif(length(idx))
    n[c(1,2,4,8,16,32)] <- NA
    t <- rep(letters[1:7], length.out=length(idx))
    t[c(1,2,3,5,8,13,21,34)] <- NA
    f <- factor(rep(letters[1:7], length.out=length(idx)))
    f[c(3,14,15,9,26)] <- NA
    mi<- as.integer(rep(NA, length(idx)))
    mc<- as.character(rep(NA, length(idx)))
    df <- data.frame(idx, n, t, f, mi, mc
                    ,stringsAsFactors=FALSE                     
                    )

    # test function behaviour for various types
    checkEquals(51-6, count(n), "Error: count() fails with vector of double")
    checkEquals(51-8, count(t), "Error: count() fails with vector of characters")
    checkEquals(51-5, count(f), "Error: count() fails with vector of factors")
    checkEquals(0, count(mi), "Error: count() fails with vector all missing")
}



dfCompareTest <- function() {
# Unit test for dfCompare

    # Test for false differences:
    aa <- data.frame(a = c(1:10, NA, Inf)
                    ,b = c(seq(0,1, length=10), NA, Inf)
                    ,c = c(letters[1:10], NA, NA)
                    ,d = as.factor(LETTERS[c(1:10, NA, NA)])
                    ,e = c(FALSE, TRUE, FALSE, FALSE, TRUE,FALSE, TRUE, FALSE, FALSE, TRUE, NA, NA)
                    ,stringsAsFactors=FALSE
                    )
   
    dd <- try( dfCompare(aa, aa, c('a')) , silent=TRUE)
    checkTrue(class(dd) != 'try-error', paste("Caught error in dfCompare between identical dataframes: ", as.character(dd)))
    checkEquals(NULL, dd, "Detected differences in identical dataframes")
    
    # Test for ability to compare across classes
    bb <- within(aa
                ,{a = as.character(a)
                  b = as.character(b)
                  c = as.factor(c)
                  d = LETTERS[c(1:10, NA, NA)]
                 }
                )
    dd <- try( dfCompare(aa, bb, c('a')) , silent=TRUE)
    checkTrue(class(dd) != 'try-error', paste("Caught error in dfCompare testing with class variation: ", as.character(dd)))
    checkEquals(NULL, dd, "Detected differences in dataframes varying by class")
    
    # Test if we can detect differences
    bb <- within(aa
                ,{b = c(seq(0,1, length=10), Inf, NA)       # Comparing NA, Inf
                  c = c(letters[1:12])                      # comparing NA, character
                  d = c(1:11,11)                            # comparing factor to integer
                  f = 'x'                                   # column not in other dataframe
                 }
                )
    exp <- as.matrix(c("Variables in second file not in first:f"          # column not in other dataframe
                      ,"Difference at a=Inf; b: First=<Inf> Second=<NA>"  # Comparing NA, Inf
                      ,"Difference at a=NA; b: First=<NA> Second=<Inf>"   
                      ,"Difference at a=Inf; c: First=<NA> Second=<l>"    # comparing NA, character
                      ,"Difference at a=NA; c: First=<NA> Second=<k>"     
                      ,"Difference at a=1; d: First=<A> Second=<1>"       # comparing factor to integer
                      ,"Difference at a=2; d: First=<B> Second=<2>"     
                      ,"Difference at a=3; d: First=<C> Second=<3>"     
                      ,"Difference at a=4; d: First=<D> Second=<4>"     
                      ,"Difference at a=5; d: First=<E> Second=<5>"     
                      ,"Difference at a=6; d: First=<F> Second=<6>"     
                      ,"Difference at a=7; d: First=<G> Second=<7>"     
                      ,"Difference at a=8; d: First=<H> Second=<8>"     
                      ,"Difference at a=9; d: First=<I> Second=<9>"     
                      ,"Difference at a=10; d: First=<J> Second=<10>"   
                      ,"Difference at a=Inf; d: First=<NA> Second=<11>" 
                      ,"Difference at a=NA; d: First=<NA> Second=<11>"  
                      )
                    ,nrow=1)
    dd <- try( dfCompare(aa, bb, c('a')) , silent=TRUE)
#    return(dd)
    checkTrue(class(dd) != 'try-error', paste("Caught error in dfCompare with differing values: ", as.character(dd)))
    checkEquals(exp, dd, "Detected unexpected differences in dataframes with different values")

    
}



firstTest <- function() {
# tests first()

  # Create test dataframe
  dfTest <- data.frame('a' = rep(1:3, each=20)
                      ,'b' = rep(1:4, each=5, times=3)
                      ,'c' = rep(1:5, times=12)
                      ,stringsAsFactors=FALSE
                      )
  dfTest$x <- runif(length(dfTest))

  # Try flagging column a
  result <- first(dfTest, 'a', 'first.a')
  expected <- dfTest
  expected$first.a <- ifelse(expected$b==1 & expected$c==1, TRUE, FALSE)
  checkIdentical(expected, result
                ,"ERROR: first() did not correctlyflag column a"
                )

  # Try flagging column b
  result <- first(dfTest, 'b', 'first.b')
  expected <- dfTest
  expected$first.b <- ifelse(expected$c==1, TRUE, FALSE)
  checkIdentical(expected, result
                ,"ERROR: first() did not correctly flag column b"
                )

  # Try flagging b when it has missing values
  missTest <- dfTest
  missTest[missTest$a==1 &
           missTest$b %in% c(1,3) &
           missTest$c %in% c(1,2)
          ,]$b <- NA

  result <- first(missTest, 'b', 'first.b')
  expected <- missTest
  expected$first.b <- ifelse(expected$c==1, TRUE, FALSE)
  expected[expected$a == 1 &
           expected$b %in% c(1,3) &
           expected$c == 3
          ,]$first.b <- TRUE
  checkIdentical(expected, result
                ,"ERROR: first() did not correctly flag column b with missing values"
                )


}



idrTest <- function() {
# tests idr()

    # test function behaviour for various types
    checkEquals(160, idr(seq(-100,100,0.1)), "Error: idr fails with doubles #1")
    checkEquals(164.4, idr(seq(-100,100,13.7)), "Error: idr fails with doubles #2")
    checkTrue(is.na(idr(as.integer(rep(NA, 43)))), "Error: idr fails with all NA")
    checkEquals(0, idr(1.2), "Error: idr fails with single value")
}



iqrTest <- function() {
# tests idr()

    # test function behaviour for various types
    checkEquals(100, iqr(seq(-100,100,0.1)), "Error: idr fails with doubles #1")
    checkEquals(109.6, iqr(seq(-100,100,13.7)), "Error: idr fails with doubles #2")
    checkTrue(is.na(iqr(as.integer(rep(NA, 43)))), "Error: idr fails with all NA")
    checkEquals(0, iqr(1.2), "Error: idr fails with single value")
}



interpolatePercentileTest <- function() {

# Tests interpolatePercentile()

  # Define test data and class boundaries.
  sizes <- data.frame('CLASS'=c('FN','SA','GF','GC','CB','SB','XB')
                     ,'min'=c(0.001, 0.06,2,   16,  64,  250, 1000)
                     ,'max'=c(0.06,  2,   16,  64,  250, 1000,4000)
                     ,stringsAsFactors=FALSE
                     )
  sizes$min <- log10(sizes$min)
  sizes$max <- log10(sizes$max)

  testData <- interpolatePercentile.testData()
  testData <- subset(testData
                    ,SIZE_CLS %in% c('FN','SA','GF','GC','CB','SB','XB')
                    )
  expectedResults <- interpolatePercentile.expectedResults()

  # calculate the values for which we have expected results
  c16 <- interpolatePercentile(testData, 'SIZE_CLS', 16, 'lsub2d16InoR', sizes)
  c50 <- interpolatePercentile(testData, 'SIZE_CLS', 50, 'lsub2d50InoR', sizes)
  c84 <- interpolatePercentile(testData, 'SIZE_CLS', 84, 'lsub2d84InoR', sizes)
  calcs <- merge(c16
                ,merge(c50, c84, by='UID', all=TRUE)
                ,by='UID'
                ,all=TRUE
                )

  # Compare results, which should be within a nominal calculation error
  errs <- dfCompare(expectedResults, calcs, 'UID', zeroFudge=1e-8)
  checkEquals(NULL, errs
              ,"Error: interpolatePercentile() results are incorrect"
              )
}



interpolatePercentile.testData <- function() {

# Creates the dataframe of EMAP substrate data used by interpolatePercentileTest()
# Values taken from WEMAP:
# 2002 WAZP99-0590 1    Has FN and others but no SA
# 2002 WCOP01-0725 1    Has single class (FN)
# 2002 WNDP02-R001 1    Has two disparate classes FN and CB
# 2002 WORP99-0866 1    Has all classes present

  testData <-read.table(textConnection(
                  "UID TRANSECT TRANSDIR SIZE_CLS
                  '2002 WAZP99-0590 1' A CT CB
                  '2002 WAZP99-0590 1' A LC CB
                  '2002 WAZP99-0590 1' A LF FN
                  '2002 WAZP99-0590 1' A RC SB
                  '2002 WAZP99-0590 1' A RT FN
                  '2002 WAZP99-0590 1' A mm GF
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm CB
                  '2002 WAZP99-0590 1' A mm SB
                  '2002 WAZP99-0590 1' B CT CB
                  '2002 WAZP99-0590 1' B LC CB
                  '2002 WAZP99-0590 1' B LF FN
                  '2002 WAZP99-0590 1' B RC SB
                  '2002 WAZP99-0590 1' B RT GC
                  '2002 WAZP99-0590 1' B mm GF
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm CB
                  '2002 WAZP99-0590 1' B mm RS
                  '2002 WAZP99-0590 1' C CT CB
                  '2002 WAZP99-0590 1' C LC XB
                  '2002 WAZP99-0590 1' C LF GF
                  '2002 WAZP99-0590 1' C RC SB
                  '2002 WAZP99-0590 1' C RT FN
                  '2002 WAZP99-0590 1' C mm RS
                  '2002 WAZP99-0590 1' C mm SB
                  '2002 WAZP99-0590 1' C mm CB
                  '2002 WAZP99-0590 1' C mm CB
                  '2002 WAZP99-0590 1' C mm FN
                  '2002 WAZP99-0590 1' D CT CB
                  '2002 WAZP99-0590 1' D LC CB
                  '2002 WAZP99-0590 1' D LF FN
                  '2002 WAZP99-0590 1' D RC XB
                  '2002 WAZP99-0590 1' D RT GF
                  '2002 WAZP99-0590 1' D mm FN
                  '2002 WAZP99-0590 1' D mm CB
                  '2002 WAZP99-0590 1' D mm SB
                  '2002 WAZP99-0590 1' D mm CB
                  '2002 WAZP99-0590 1' D mm GC
                  '2002 WAZP99-0590 1' E CT SB
                  '2002 WAZP99-0590 1' E LC CB
                  '2002 WAZP99-0590 1' E LF GF
                  '2002 WAZP99-0590 1' E RC CB
                  '2002 WAZP99-0590 1' E RT GC
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm CB
                  '2002 WAZP99-0590 1' E mm SB
                  '2002 WAZP99-0590 1' E mm FN
                  '2002 WAZP99-0590 1' F CT CB
                  '2002 WAZP99-0590 1' F LC CB
                  '2002 WAZP99-0590 1' F LF FN
                  '2002 WAZP99-0590 1' F RC CB
                  '2002 WAZP99-0590 1' F RT FN
                  '2002 WAZP99-0590 1' F mm FN
                  '2002 WAZP99-0590 1' F mm CB
                  '2002 WAZP99-0590 1' F mm GC
                  '2002 WAZP99-0590 1' F mm CB
                  '2002 WAZP99-0590 1' F mm FN
                  '2002 WAZP99-0590 1' G CT CB
                  '2002 WAZP99-0590 1' G LC CB
                  '2002 WAZP99-0590 1' G LF FN
                  '2002 WAZP99-0590 1' G RC CB
                  '2002 WAZP99-0590 1' G RT RS
                  '2002 WAZP99-0590 1' G mm FN
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm CB
                  '2002 WAZP99-0590 1' G mm FN
                  '2002 WAZP99-0590 1' H CT CB
                  '2002 WAZP99-0590 1' H LC GC
                  '2002 WAZP99-0590 1' H LF GC
                  '2002 WAZP99-0590 1' H RC CB
                  '2002 WAZP99-0590 1' H RT FN
                  '2002 WAZP99-0590 1' H mm CB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' H mm XB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' H mm SB
                  '2002 WAZP99-0590 1' I CT CB
                  '2002 WAZP99-0590 1' I LC CB
                  '2002 WAZP99-0590 1' I LF FN
                  '2002 WAZP99-0590 1' I RC CB
                  '2002 WAZP99-0590 1' I RT GC
                  '2002 WAZP99-0590 1' I mm FN
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm CB
                  '2002 WAZP99-0590 1' I mm FN
                  '2002 WAZP99-0590 1' J CT GC
                  '2002 WAZP99-0590 1' J LC CB
                  '2002 WAZP99-0590 1' J LF RS
                  '2002 WAZP99-0590 1' J RC CB
                  '2002 WAZP99-0590 1' J RT GC
                  '2002 WAZP99-0590 1' J mm RS
                  '2002 WAZP99-0590 1' J mm CB
                  '2002 WAZP99-0590 1' J mm CB
                  '2002 WAZP99-0590 1' J mm GC
                  '2002 WAZP99-0590 1' J mm FN
                  '2002 WAZP99-0590 1' K CT XB
                  '2002 WAZP99-0590 1' K LC XB
                  '2002 WAZP99-0590 1' K LF FN
                  '2002 WAZP99-0590 1' K RC CB
                  '2002 WAZP99-0590 1' K RT FN
                  '2002 WCOP01-0725 1' A CT FN
                  '2002 WCOP01-0725 1' A LC FN
                  '2002 WCOP01-0725 1' A LF FN
                  '2002 WCOP01-0725 1' A RC FN
                  '2002 WCOP01-0725 1' A RT FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' A mm FN
                  '2002 WCOP01-0725 1' B CT FN
                  '2002 WCOP01-0725 1' B LC FN
                  '2002 WCOP01-0725 1' B LF FN
                  '2002 WCOP01-0725 1' B RC FN
                  '2002 WCOP01-0725 1' B RT FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' B mm FN
                  '2002 WCOP01-0725 1' C CT FN
                  '2002 WCOP01-0725 1' C LC FN
                  '2002 WCOP01-0725 1' C LF FN
                  '2002 WCOP01-0725 1' C RC FN
                  '2002 WCOP01-0725 1' C RT FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' C mm FN
                  '2002 WCOP01-0725 1' D CT FN
                  '2002 WCOP01-0725 1' D LC FN
                  '2002 WCOP01-0725 1' D LF FN
                  '2002 WCOP01-0725 1' D RC FN
                  '2002 WCOP01-0725 1' D RT FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' D mm FN
                  '2002 WCOP01-0725 1' E CT FN
                  '2002 WCOP01-0725 1' E LC FN
                  '2002 WCOP01-0725 1' E LF FN
                  '2002 WCOP01-0725 1' E RC FN
                  '2002 WCOP01-0725 1' E RT FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' E mm FN
                  '2002 WCOP01-0725 1' F CT FN
                  '2002 WCOP01-0725 1' F LC FN
                  '2002 WCOP01-0725 1' F LF FN
                  '2002 WCOP01-0725 1' F RC FN
                  '2002 WCOP01-0725 1' F RT FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' F mm FN
                  '2002 WCOP01-0725 1' G CT FN
                  '2002 WCOP01-0725 1' G LC FN
                  '2002 WCOP01-0725 1' G LF FN
                  '2002 WCOP01-0725 1' G RC FN
                  '2002 WCOP01-0725 1' G RT FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' G mm FN
                  '2002 WCOP01-0725 1' H CT FN
                  '2002 WCOP01-0725 1' H LC FN
                  '2002 WCOP01-0725 1' H LF FN
                  '2002 WCOP01-0725 1' H RC FN
                  '2002 WCOP01-0725 1' H RT FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' H mm FN
                  '2002 WCOP01-0725 1' I CT FN
                  '2002 WCOP01-0725 1' I LC FN
                  '2002 WCOP01-0725 1' I LF FN
                  '2002 WCOP01-0725 1' I RC FN
                  '2002 WCOP01-0725 1' I RT FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' I mm FN
                  '2002 WCOP01-0725 1' J CT FN
                  '2002 WCOP01-0725 1' J LC FN
                  '2002 WCOP01-0725 1' J LF FN
                  '2002 WCOP01-0725 1' J RC FN
                  '2002 WCOP01-0725 1' J RT FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' J mm FN
                  '2002 WCOP01-0725 1' K CT FN
                  '2002 WCOP01-0725 1' K LC FN
                  '2002 WCOP01-0725 1' K LF FN
                  '2002 WCOP01-0725 1' K RC FN
                  '2002 WCOP01-0725 1' K RT FN
                  '2002 WNDP02-R001 1' A CT FN
                  '2002 WNDP02-R001 1' A LC FN
                  '2002 WNDP02-R001 1' A LF FN
                  '2002 WNDP02-R001 1' A RC FN
                  '2002 WNDP02-R001 1' A RT FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' A mm FN
                  '2002 WNDP02-R001 1' B CT FN
                  '2002 WNDP02-R001 1' B LC FN
                  '2002 WNDP02-R001 1' B LF FN
                  '2002 WNDP02-R001 1' B RC FN
                  '2002 WNDP02-R001 1' B RT FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' B mm FN
                  '2002 WNDP02-R001 1' C CT FN
                  '2002 WNDP02-R001 1' C LC FN
                  '2002 WNDP02-R001 1' C LF FN
                  '2002 WNDP02-R001 1' C RC FN
                  '2002 WNDP02-R001 1' C RT FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' C mm FN
                  '2002 WNDP02-R001 1' D CT FN
                  '2002 WNDP02-R001 1' D LC FN
                  '2002 WNDP02-R001 1' D LF FN
                  '2002 WNDP02-R001 1' D RC FN
                  '2002 WNDP02-R001 1' D RT FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' D mm FN
                  '2002 WNDP02-R001 1' E CT FN
                  '2002 WNDP02-R001 1' E LC FN
                  '2002 WNDP02-R001 1' E LF FN
                  '2002 WNDP02-R001 1' E RC FN
                  '2002 WNDP02-R001 1' E RT HP
                  '2002 WNDP02-R001 1' E mm FN
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' E mm HP
                  '2002 WNDP02-R001 1' F CT CB
                  '2002 WNDP02-R001 1' F LC CB
                  '2002 WNDP02-R001 1' F LF CB
                  '2002 WNDP02-R001 1' F RC CB
                  '2002 WNDP02-R001 1' F RT CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' F mm CB
                  '2002 WNDP02-R001 1' G CT CB
                  '2002 WNDP02-R001 1' G LC CB
                  '2002 WNDP02-R001 1' G LF HP
                  '2002 WNDP02-R001 1' G RC CB
                  '2002 WNDP02-R001 1' G RT CB
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm HP
                  '2002 WNDP02-R001 1' G mm FN
                  '2002 WNDP02-R001 1' H CT HP
                  '2002 WNDP02-R001 1' H LC HP
                  '2002 WNDP02-R001 1' H LF HP
                  '2002 WNDP02-R001 1' H RC HP
                  '2002 WNDP02-R001 1' H RT FN
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' H mm HP
                  '2002 WNDP02-R001 1' I CT OT
                  '2002 WNDP02-R001 1' I LC FN
                  '2002 WNDP02-R001 1' I LF HP
                  '2002 WNDP02-R001 1' I RC HP
                  '2002 WNDP02-R001 1' I RT FN
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' I mm HP
                  '2002 WNDP02-R001 1' J CT HP
                  '2002 WNDP02-R001 1' J LC HP
                  '2002 WNDP02-R001 1' J LF FN
                  '2002 WNDP02-R001 1' J RC HP
                  '2002 WNDP02-R001 1' J RT FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' J mm FN
                  '2002 WNDP02-R001 1' K CT FN
                  '2002 WNDP02-R001 1' K LC FN
                  '2002 WNDP02-R001 1' K LF FN
                  '2002 WNDP02-R001 1' K RC FN
                  '2002 WNDP02-R001 1' K RT FN
                  '2002 WORP99-0866 1' A CT GC
                  '2002 WORP99-0866 1' A LC GC
                  '2002 WORP99-0866 1' A LF GC
                  '2002 WORP99-0866 1' A RC GC
                  '2002 WORP99-0866 1' A RT GC
                  '2002 WORP99-0866 1' A mm SA
                  '2002 WORP99-0866 1' A mm GF
                  '2002 WORP99-0866 1' A mm GC
                  '2002 WORP99-0866 1' A mm RR
                  '2002 WORP99-0866 1' A mm RR
                  '2002 WORP99-0866 1' B CT CB
                  '2002 WORP99-0866 1' B LC GC
                  '2002 WORP99-0866 1' B LF GC
                  '2002 WORP99-0866 1' B RC GC
                  '2002 WORP99-0866 1' B RT CB
                  '2002 WORP99-0866 1' B mm GC
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' B mm CB
                  '2002 WORP99-0866 1' C CT GF
                  '2002 WORP99-0866 1' C LC GC
                  '2002 WORP99-0866 1' C LF CB
                  '2002 WORP99-0866 1' C RC CB
                  '2002 WORP99-0866 1' C RT FN
                  '2002 WORP99-0866 1' C mm GC
                  '2002 WORP99-0866 1' C mm SB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' C mm CB
                  '2002 WORP99-0866 1' D CT GF
                  '2002 WORP99-0866 1' D LC SB
                  '2002 WORP99-0866 1' D LF CB
                  '2002 WORP99-0866 1' D RC CB
                  '2002 WORP99-0866 1' D RT SB
                  '2002 WORP99-0866 1' D mm FN
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' D mm CB
                  '2002 WORP99-0866 1' E CT SB
                  '2002 WORP99-0866 1' E LC CB
                  '2002 WORP99-0866 1' E LF XB
                  '2002 WORP99-0866 1' E RC GF
                  '2002 WORP99-0866 1' E RT XB
                  '2002 WORP99-0866 1' E mm SB
                  '2002 WORP99-0866 1' E mm CB
                  '2002 WORP99-0866 1' E mm OT
                  '2002 WORP99-0866 1' E mm CB
                  '2002 WORP99-0866 1' E mm SB
                  '2002 WORP99-0866 1' F CT GC
                  '2002 WORP99-0866 1' F LC GC
                  '2002 WORP99-0866 1' F LF FN
                  '2002 WORP99-0866 1' F RC CB
                  '2002 WORP99-0866 1' F RT XB
                  '2002 WORP99-0866 1' F mm XB
                  '2002 WORP99-0866 1' F mm CB
                  '2002 WORP99-0866 1' F mm FN
                  '2002 WORP99-0866 1' F mm CB
                  '2002 WORP99-0866 1' F mm OT
                  '2002 WORP99-0866 1' G CT GC
                  '2002 WORP99-0866 1' G LC XB
                  '2002 WORP99-0866 1' G LF FN
                  '2002 WORP99-0866 1' G RC SB
                  '2002 WORP99-0866 1' G RT XB
                  '2002 WORP99-0866 1' G mm CB
                  '2002 WORP99-0866 1' G mm GF
                  '2002 WORP99-0866 1' G mm SB
                  '2002 WORP99-0866 1' G mm CB
                  '2002 WORP99-0866 1' G mm GC
                  '2002 WORP99-0866 1' H CT CB
                  '2002 WORP99-0866 1' H LC CB
                  '2002 WORP99-0866 1' H LF XB
                  '2002 WORP99-0866 1' H RC SA
                  '2002 WORP99-0866 1' H RT SB
                  '2002 WORP99-0866 1' H mm XB
                  '2002 WORP99-0866 1' H mm GF
                  '2002 WORP99-0866 1' H mm CB
                  '2002 WORP99-0866 1' H mm CB
                  '2002 WORP99-0866 1' H mm FN
                  '2002 WORP99-0866 1' I CT GC
                  '2002 WORP99-0866 1' I LC CB
                  '2002 WORP99-0866 1' I LF FN
                  '2002 WORP99-0866 1' I RC GC
                  '2002 WORP99-0866 1' I RT SB
                  '2002 WORP99-0866 1' I mm SA
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' I mm XB
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' I mm CB
                  '2002 WORP99-0866 1' J CT SA
                  '2002 WORP99-0866 1' J LC FN
                  '2002 WORP99-0866 1' J LF XB
                  '2002 WORP99-0866 1' J RC CB
                  '2002 WORP99-0866 1' J RT XB
                  '2002 WORP99-0866 1' J mm FN
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GF
                  '2002 WORP99-0866 1' J mm GC
                  '2002 WORP99-0866 1' K CT GF
                  '2002 WORP99-0866 1' K LC GF
                  '2002 WORP99-0866 1' K LF OT
                  '2002 WORP99-0866 1' K RC GC
                  '2002 WORP99-0866 1' K RT OT
                  "
                  ), header=TRUE, stringsAsFactors=FALSE
                  )
  return(testData)
}



interpolatePercentile.expectedResults <- function() {

# Creates the results dataframe used by interpolatePercentileTest()
# Values taken from WEMAP
# 2002 WAZP99-0590 1    Has FN and others but no SA
# 2002 WCOP01-0725 1    Has single class (FN)
# 2002 WNDP02-R001 1    Has two disparate classes FN and CB
# 2002 WORP99-0866 1    Has all classes present

  testResults <-read.table(textConnection(
                  "UID lsub2d16InoR lsub2d50InoR lsub2d84InoR
                  '2002 WAZP99-0590 1' -1.706799091   1.9698582815  2.3979400087
                  '2002 WCOP01-0725 1' -2.7154958    -2.110924375  -1.50635295
                  '2002 WNDP02-R001 1' -2.650199754  -1.906874231   1.8907171218
                  '2002 WORP99-0866 1'  0.5341914105  1.9019058619  2.7086029642
                  "
                  ), header=TRUE, stringsAsFactors=FALSE
                  )
  return(testResults)
}



is.subsetTest <- function() {
# Tests is.subset


  # Test numbers in various guises: as numbers
  checkEquals(TRUE, is.subset(c(1,2,4,8), rep(1:10))
             ,"Error: is.subset fails with number vector #1"
             )
  checkEquals(FALSE, is.subset(c(1,2,4,8,101), rep(1:10))
             ,"Error: is.subset fails with number vector #2"
             )

  # Test numbers in various guises: as list elements
  checkEquals(TRUE, is.subset(as.list(c(1,2,4,8)), rep(1:10))
             ,"Error: is.subset fails with number list #1"
             )
  checkEquals(FALSE, is.subset(as.list(c(1,2,4,8,101)), rep(1:10))
             ,"Error: is.subset fails with number list #2"
             )

  # Test characters in various guises: as characters
  checkEquals(TRUE, is.subset(c(1,2,4,8), rep(1:10))
             ,"Error: is.subset fails with character vector #1"
             )
  checkEquals(FALSE, is.subset(c(1,2,4,8,101), rep(1:10))
             ,"Error: is.subset fails with character vector #2"
             )

}



lagTest <- function() {
# Tests the lag() function

  # Create test dataframe
  dfTest <- data.frame('a'=LETTERS
                      ,'b'=letters
                      ,'c'=1:length(LETTERS)
                      ,stringsAsFactors=FALSE
                      )
  
  # Try lagging single column
  result <- lag(dfTest, 'c', 'prev.c')
  expected <- dfTest
  expected$prev.c <- c(NA, 1:(nrow(dfTest) - 1))
  checkIdentical(expected, result
                ,"ERROR: lag() of single column failed"
                )
  
  # Try lagging multiple columns
  result <- lag(dfTest, c('c','b'), c('prev.c','prev.b'), 3)
  expected <- dfTest
  expected$prev.c <- c(NA, NA, NA, 1:(nrow(dfTest) - 3))
  expected$prev.b <- c(NA, NA, NA, letters[1:(nrow(dfTest) - 3)])
  checkIdentical(expected, result
                ,"ERROR: lag() of multiple columns failed"
                )

}



lastTest <- function() {
# tests last()

  # Create test dataframe
  dfTest <- data.frame('a' = rep(1:3, each=20)
                      ,'b' = rep(1:4, each=5, times=3)
                      ,'c' = rep(1:5, times=12)
                      ,stringsAsFactors=FALSE
                      )
  dfTest$x <- runif(length(dfTest))

  # Try flagging column a
  result <- last(dfTest, 'a', 'last.a')
  expected <- dfTest
  expected$last.a <- ifelse(expected$b==4 & expected$c==5, TRUE, FALSE)
  checkIdentical(expected, result
                ,"ERROR: last() did not flag column a correctly"
                )

  # Try flagging column b
  result <- last(dfTest, 'b', 'last.b')
  expected <- dfTest
  expected$last.b <- ifelse(expected$c==5, TRUE, FALSE)
  checkIdentical(expected, result
                ,"ERROR: last() did not flag column b correctly"
                )

  # Try flagging b when it has missing values
  missTest <- dfTest
  missTest[missTest$a==1 &
           missTest$b %in% c(1,3) &
           missTest$c %in% c(1,2)
          ,]$b <- NA

  result <- last(missTest, 'b', 'last.b')
  expected <- missTest
  expected$last.b <- ifelse(expected$c==5, TRUE, FALSE)
  expected[expected$a == 1 &
           is.na(expected$b) &
           expected$c == 2
          ,]$last.b <- TRUE
  checkIdentical(expected, result
                ,"ERROR: last() did not correctly flag column b with missing values"
                )
}



leadTest <- function() {
# Tests the lead() function

  # Create test dataframe
  dfTest <- data.frame('a'=LETTERS
                      ,'b'=letters
                      ,'c'=1:length(LETTERS)
                      ,stringsAsFactors=FALSE
                      )

  # Try lagging single column
  result <- lead(dfTest, 'c', 'prev.c')
  expected <- dfTest
  expected$prev.c <- c(2:nrow(dfTest), NA)
  checkIdentical(expected, result
                ,"ERROR: lead() of single column failed"
                )

  # Try lagging multiple columns
  result <- lead(dfTest, c('c','b'), c('prev.c','prev.b'), 3)
  expected <- dfTest
  expected$prev.c <- c(4:nrow(dfTest), NA, NA, NA)
  expected$prev.b <- c(letters[4:nrow(dfTest)], NA, NA, NA)
  checkIdentical(expected, result
                ,"ERROR: lead() of multiple columns failed"
                )

}



modalClassTest <- function() {

    # create test data.
    dfTest<-data.frame(idx1=c('one','two','two','three')
                      ,idx2=c(1,1,2,3)
                      ,coverA=c(1, 1, 1 ,NA)
                      ,coverB=c(2, 2, 2 ,NA)
                      ,coverC=c(1, 1, NA,NA)
                      ,coverD=c(3, 3, 3 ,NA)
                      ,coverE=c(2, 2, NA,NA)
                      ,coverF=c(0, 3, 3 ,NA)
                      ,coverG=c(2, 2, 2 ,NA)
                      ,coverH=c(0, 2, 3 ,NA)
                      )

    # test function behaviour
    checkEquals('D'
               ,modalClass(dfTest[1,]
                          ,c('coverA','coverB','coverC','coverD'
                            ,'coverE','coverF','coverG','coverH'
                            )
                          ,LETTERS[1:8]
                          )
               ,"Error: modalClass failed with single mode"
               )
    checkEquals('D, F'
               ,modalClass(dfTest[2,]
                          ,c('coverA','coverB','coverC','coverD'
                            ,'coverE','coverF','coverG','coverH'
                            )
                          ,LETTERS[1:8]
                          )
               ,"Error: modalClass failed with two modes"
               )
    checkEquals('D, F, H'
               ,modalClass(dfTest[3,]
                          ,c('coverA','coverB','coverC','coverD'
                            ,'coverE','coverF','coverG','coverH'
                            )
                          ,LETTERS[1:8]
                          )
               ,"Error: modalClass failed with three modes and missing values"
               )
    checkEquals('D, F, H'
               ,modalClass(dfTest[3,]
                          ,c('coverA','coverC','coverE','coverG'
                            ,'coverB','coverD','coverF','coverH'
                            )
                          ,c('A','C','E','G','B','D','F','H')
                          )
               ,"Error: modalClass failed with three modes and missing values, names reordered"
               )
    checkTrue(is.na(modalClass(dfTest[4,]
                              ,c('coverA','coverB','coverC','coverD'
                                ,'coverE','coverF','coverG','coverH'
                                )
                              ,LETTERS[1:8]
                              )
                   )
             ,"Error: modalClass failed with all missing values"
             )

}



modalCountTest <- function() {

# tests modalCount()

  checkEquals(3L, modalCount(c(LETTERS,LETTERS, 'X'))
             ,'Error: modalCount broken for letters with single mode'
             )
  checkEquals(2L, modalCount(c(LETTERS,LETTERS))
             ,'Error: modalCount broken for letters with multiple modes'
             )

  checkEquals(3L, modalCount(c(1:10, 1:10, 4))
             ,'Error: modalCount broken for numbers with clear mode'
             )
  checkEquals(2L, modalCount(c(1:10, 1:10))
             ,'Error: modalCount broken for numbers with multiple modes'
             )

  checkEquals(3L, modalCount(data.frame(ff=c(LETTERS,LETTERS, 'X'))$ff)
             ,'Error: modalCount broken for factors with single mode'
             )
  checkEquals(2L, modalCount(data.frame(ff=c(LETTERS,LETTERS))$ff)
             ,'Error: modalCount broken for factors with multiple modes'
             )
}



modalvalueTest <- function() {
# tests modalvalue()
#
# NOTE: Should also test with na.rm=TRUE.

     checkEquals(7, modalvalue(c(5,5,3,7,2,7,0,7))
                ,"Error: modalvalue of numbers fails"
                )
     checkEquals(7, modalvalue(c(5,5,3,7,2,7,0,7,2,2,NA,NA))
                ,"Error: modalvalue of numbers fails with few values"
                )
     checkTrue(is.na(modalvalue(c(5,5,3,7,2,7,0,7,2,2,NA,NA,NA,NA,NA)))
                ,"Error: modalvalue of numbers fails with many missing values"
                )
     checkTrue(is.na(modalvalue(rep(NA,20)))
              ,"Error: modalvalue of numbers fails with all missing values"
              )
     checkEquals(7, modalvalue(c(5,5,3,7,2,7,0,7,2,2))
                ,"Error: modalvalue of numbers fails with more than one mode"
                )
     checkEquals('c', modalvalue(c(letters[1:6],'c'))
                ,"Error: modalvalue of characters fails"
                )
     checkEquals('b', modalvalue(c(letters[1:6],'c', 'b'))
                ,"Error: modalvalue of characters fails with > 1 mode"
                )

}



normalizedCoverTest <- function() {
# tests normalizedCover()

  # Set up parameter values that will be reused in each of the uids of the test
  # data.
  baseData <- data.frame(
    'parameter'= rep(c('cat1', 'cat2', 'cat3', 'cat4', 'cat1NA'
                      ,'catAllNA','cat1NaN','catAllNaN'
                      )
                    ,each=10
                    )
    ,'subid'   = rep(LETTERS[1:10], times=8)
    ,'cover'   = c(0.0, 0.5, 0.5, 0.0, 1.0, 0.3, 0.0, 0.5, 0.4, 0.7
                  ,0.3, 0.4, 0.7, 0.7, 0.9, 0.0, 0.1, 0.5, 0.4, 0.4
                  ,0.7, 0.8, 0.3, 0.4, 0.0, 0.2, 0.3, 0.0, 0.2, 0.3
                  ,0.0, 1.0, 0.0, 0.5, 0.9, 0.1, 0.4, 0.2, 0.3, 0.5
                  ,0.0, 0.5, 0.5, 0.0, 1.0, 0.3, 0.0, 0.5, 0.4, NA
                  ,NA , NA , NA , NA , NA , NA , NA , NA , NA , NA 
                  ,0.0, 0.5, 0.5, 0.0, 1.0, 0.3, 0.0, 0.5, 0.4, NaN
                  ,NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN
                  )
  )
  
  # Test integrity of base data before formal function tests by checking cover
  # means by parameter and subid to see if they've been changed without
  # adjusting the test.  Determine expected means here, and compare them to the
  # calculated means.
  # Note that while mean(NA) is NA, but mean(NA, na.rm=TRUE) is NaN.
  parameterMeans <- data.frame(
     parameter=c('cat1', 'cat2', 'cat3', 'cat4', 'cat1NA'
                ,'catAllNA','cat1NaN','catAllNaN'
                )
    ,expected=c(0.39, 0.44, 0.32, 0.39, 0.35555556
               ,NaN, 0.35555556, NaN
               )
  )
  subidMeans <- data.frame(
     subid=c(LETTERS[1:10])
    ,expected=c(0.166666667, 0.616666667, 0.416666667, 0.266666667
               , 0.8, 0.2, 0.133333333, 0.366666667, 0.35, 0.475
               )
  )

  tt1 <- aggregate(baseData$cover, list('parameter'=baseData$parameter)
                  ,mean, na.rm=TRUE
                  )
  tt2 <- aggregate(baseData$cover, list('subid'=baseData$subid)
                  ,mean, na.rm=TRUE
                  )

  # Compare expected and actual means.
  testParam<-NULL

  tt<-subset(merge(tt1,parameterMeans, by=c('parameter'))
            ,abs(x-expected)> 10^-8      | 
             is.na(x)!=is.na(expected)   | 
             is.nan(x)!=is.nan(expected)
            )
  checkEquals(0, nrow(tt)
             ,paste("Error: normalizedCoverTest baseData has unexpected parameter means:"
                   ,paste(tt$parameter
                         ,paste(' calc:',tt$x, sep='')
                         ,paste(' expected:',tt$expected, sep='')
                         ,collapse='; '
                         )
                   ,sep=' '
                   )
             )
   checkEquals(0, nrow(tt)
              ,paste("Error: normalizedCoverTest baseData has unexpected subid means:"
                    ,paste(tt$subid
                          ,paste('calc:',tt$x, sep='')
                          ,paste('expected:',tt$expected, sep='')
                          ,collapse='; '
                          )
                    ,sep=' '
                    )
              )


  # Build the test dataframe from baseData, first 1 parameter per uid, then
  # 4 parameters per UID.
  dfTest1  <- baseData[baseData$parameter %in% c('cat1'),]
  dfTest1$uid='test1'
  dfTest1b <- baseData[baseData$parameter %in% c('cat1NA'),]
  dfTest1b$uid='test1b'
  dfTest1c <- baseData[baseData$parameter %in% c('cat1NaN'),]
  dfTest1c$uid='test1c'
  dfTest1d <- baseData[baseData$parameter %in% c('catAllNA'),]
  dfTest1d$uid='test1d'
  dfTest1e <- baseData[baseData$parameter %in% c('catAllNaN'),]
  dfTest1e$uid='test1e'

  dfTest4  <- baseData[baseData$parameter %in% c('cat1','cat2','cat3','cat4'),]
  dfTest4$uid='test4'
  dfTest4b <- baseData[baseData$parameter %in% 
                       c('cat1','cat2','cat3','cat1NA'),]
  dfTest4b$uid='test4b'
  dfTest4c <- baseData[baseData$parameter %in% 
                       c('cat1','cat2','cat3','cat1NaN'),]
  dfTest4c$uid='test4c'
  dfTest4d <- baseData[baseData$parameter %in% 
                       c('cat1','cat2','cat3','catAllNA'),]
  dfTest4d$uid='test4d'
  dfTest4e <- baseData[baseData$parameter %in% 
                       c('cat1','cat2','cat3','catAllNaN'),]
  dfTest4e$uid='test4e'
  dfTest4f <- baseData[baseData$parameter %in% 
                       c('cat1','cat1NA','cat3','cat1NaN'),]
  dfTest4f$uid='test4f'

  dfTest<-rbind(dfTest1, dfTest1b, dfTest1c, dfTest1d, dfTest1e
               ,dfTest4, dfTest4b, dfTest4c, dfTest4d, dfTest4e, dfTest4f
               )


  # Build the expected results data frames separately for the case where the 
  # cover  categories are complete (i.e. must sum to 100% at a station) and 
  # the case where the cover categories are incomplete (i.e. must sum to 100% 
  # or less).  Expected results are for means of test data, with na.rm=TRUE.
  expectedCompleteResults <- as.data.frame(rbind(
       c('uid'='test1', 'parameter'='cat1', 'expected'=0.7)
      ,c('test1b', 'cat1NA', 6/9)
      ,c('test1c', 'cat1NaN', 6/9)
      ,c('test1d', 'catAllNA', NaN)       # mean(NA * 1/sum(NA)) isn't a number
      ,c('test1e', 'catAllNaN', NaN)      # mean(NaN * 1/sum(NA)) isn't a number

      ,c('test4',  'cat1', 0.24684414)
      ,c('test4',  'cat2', 0.27336287)
      ,c('test4',  'cat3', 0.24663705)
      ,c('test4',  'cat4', 0.23315594)

      ,c('test4b', 'cat1', 0.23161479)
      ,c('test4b', 'cat2', 0.29332886)
      ,c('test4b', 'cat3', 0.29344156)
      ,c('test4b', 'cat1NA', 0.20179421)

      ,c('test4c', 'cat1', 0.23161479)
      ,c('test4c', 'cat2', 0.29332886)
      ,c('test4c', 'cat3', 0.29344156)
      ,c('test4c', 'cat1NaN', 0.20179421)

      ,c('test4d', 'cat1', 0.31537668)
      ,c('test4d', 'cat2', 0.35477229)
      ,c('test4d', 'cat3', 0.32985103)
      ,c('test4d', 'catAllNA', NaN)       # mean(NA * 1/sum(x)) isn't a number
                                          # when na.rm=TRUE
      ,c('test4e', 'cat1', 0.31537668)
      ,c('test4e', 'cat2', 0.35477229)
      ,c('test4e', 'cat3', 0.32985103)
      ,c('test4e', 'catAllNaN', NaN)      # mean(NaN * 1/sum(x)) isn't a number
                                          # when na.rm=TRUE
      ,c('test4f', 'cat1', 0.24202773)
      ,c('test4f', 'cat1NA', 0.19114192)
      ,c('test4f', 'cat3', 0.41391681)
      ,c('test4f', 'cat1NaN', 0.19114192)

    )
    ,stringsAsFactors=FALSE
  )
  expectedCompleteResults$expected <- 
    as.numeric(expectedCompleteResults$expected)

  expectedIncompleteResults <- as.data.frame(rbind(
       c('uid'='test1', 'parameter'='cat1', 'expected'=0.39)
      ,c('test1b', 'cat1NA', 0.3555555556)
      ,c('test1c', 'cat1NaN', 0.3555555556)
      ,c('test1d', 'catAllNA', NaN)       # mean(NA * 1/sum(NA)) isn't a number
      ,c('test1e', 'catAllNaN', NaN)      # mean(NaN * 1/sum(NA)) isn't a number

      ,c('test4',  'cat1', 0.22684414)
      ,c('test4',  'cat2', 0.27086287)
      ,c('test4',  'cat3', 0.22580372)
      ,c('test4',  'cat4', 0.21648927)

      ,c('test4b', 'cat1', 0.22411479)
      ,c('test4b', 'cat2', 0.27832886)
      ,c('test4b', 'cat3', 0.24344156)
      ,c('test4b', 'cat1NA', 0.19346088)

      ,c('test4c', 'cat1', 0.22411479)
      ,c('test4c', 'cat2', 0.27832886)
      ,c('test4c', 'cat3', 0.24344156)
      ,c('test4c', 'cat1NaN', 0.19346088)

      ,c('test4d', 'cat1', 0.28537668)
      ,c('test4d', 'cat2', 0.33977229)
      ,c('test4d', 'cat3', 0.26485103)
      ,c('test4d', 'catAllNA', NaN)       # mean(NA * 1/sum(x)) isn't a number
                                          # when na.rm=TRUE
      ,c('test4e', 'cat1', 0.28537668)
      ,c('test4e', 'cat2', 0.33977229)
      ,c('test4e', 'cat3', 0.26485103)
      ,c('test4e', 'catAllNaN', NaN)      # mean(NaN * 1/sum(x)) isn't a number
                                          # when na.rm=TRUE
      ,c('test4f', 'cat1', 0.24202773)
      ,c('test4f', 'cat1NA', 0.19114192)
      ,c('test4f', 'cat3', 0.25391681)
      ,c('test4f', 'cat1NaN', 0.19114192)
    )
    ,stringsAsFactors=FALSE
  )
  expectedIncompleteResults$expected <- 
    as.numeric(expectedIncompleteResults$expected)


  # Make the normalization calculations
  tt <- normalizedCover(dfTest, 'cover', 'normCover')
  testResultsComplete <- aggregate(tt$normCover
                                  ,list('uid'=tt$uid, 'parameter'=tt$parameter)
                                  ,mean, na.rm=TRUE
                                  )

  tt <- normalizedCover(dfTest, 'cover', 'normCover', allowTotalBelow100=TRUE)
  testResultsIncomplete <- aggregate(tt$normCover
                                    ,list('uid'=tt$uid
                                         ,'parameter'=tt$parameter
                                         )
                                    ,mean, na.rm=TRUE
                                    )

  # Merge calculations and expected values for the test and accumulate detected
  # errors for both normalization types.
  tt<-subset(merge(expectedCompleteResults, testResultsComplete
                  ,by=c('uid','parameter')
                  ,all=TRUE
                  )
            ,abs(x-expected)> 10^-8      | 
             is.na(x)!=is.na(expected)   | 
             is.nan(x)!=is.nan(expected)
            )

  if(nrow(tt)>0) {
    testErrors <- rbind(testParam
                       ,paste('normalizedCover is incorrect for complete data'
                             ,tt$parameter
                             ,paste('calc:',tt$x, sep='')
                             ,paste('expected:',tt$expected, sep='')
                             )
                       )
  }

  tt<-subset(merge(expectedIncompleteResults, testResultsIncomplete
                  ,by=c('uid','parameter')
                  ,all=TRUE
                  )
            ,abs(x-expected)> 10^-8      | 
             is.na(x)!=is.na(expected)   | 
             is.nan(x)!=is.nan(expected)
            )

  checkEquals(0, nrow(tt)
             ,"Error: normalizedCover is incorrect for incomplete cover data"
             )
#    testErrors <- rbind(testParam
#                       ,paste('Normalization results differ for '
#                             ,tt$parameter
#                             ,paste('calc:',tt$x, sep='')
#                             ,paste('expected:',tt$expected, sep='')
#                             )
#                       )


#  # Notify user of any problems in normalization calculations
#  if(! is.null(testParam)) {
#      for(i in 1:dim(testParam)[1]) {
#          funcTestOutput(TRUE, verbose, testParam[i,])
#      }
#  }

}



nWadeableStationsPerTransectTest <- function() {

# tests nWadeableStationsPerTransect()

  fakeThal <- rbind(data.frame(UID=rep('std. stream A-K', 101)
                              ,TRANSECT=c(rep(LETTERS[1:10], each=10), 'K')
                              ,STATION=c(rep(0:9, 10), 0)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('std. stream A-J', 100)
                              ,TRANSECT=rep(LETTERS[1:10], each=10)
                              ,STATION=rep(0:9, 10)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('narrow stream A-J', 150)
                              ,TRANSECT=rep(LETTERS[1:10], each=15)
                              ,STATION=rep(0:14, 10)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w 2 long transects', 106)
                              ,TRANSECT=c(rep(LETTERS[1:8], each=10)
                                         ,rep(c('I','J'), each=13)
                                         )
                              ,STATION=c(rep(0:9, 8), 0:12, 0:12)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w 2 short transects', 98)
                              ,TRANSECT=c(rep(LETTERS[1:8], each=10)
                                         ,rep(c('I','J'), each=9)
                                         )
                              ,STATION=c(rep(0:9, 8), 0:8, 0:8)
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID=rep('stream w two modes', 100)
                              ,TRANSECT=c(rep(LETTERS[1:5], each=11)
                                         ,rep(LETTERS[6:10], each=9)
                                         )
                              ,STATION=c(rep(0:10, 5), rep(0:8, 5))
                              ,PARAMETER='foo'
                              ,stringsAsFactors=FALSE
                              )
                   )


  expected <- rbind(data.frame(UID='std. stream A-K'
                              ,TRANSECT=LETTERS[1:11]
                              ,nSta=c(rep(10, 10), 1)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='std. stream A-J'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(10, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='narrow stream A-J'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(15, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w 2 long transects'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=c(rep(10, 8), 13, 13)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w 2 short transects'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=rep(10, 10)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID='stream w two modes'
                              ,TRANSECT=LETTERS[1:10]
                              ,nSta=c(rep(11, 5), rep(9, 5))
                              ,stringsAsFactors=FALSE
                              )
                   )
  expected <- expected[order(expected$UID, expected$TRANSECT),]
  rownames(expected) <- NULL
 
  results <- nWadeableStationsPerTransect(fakeThal)
  results <- results[order(results$UID, results$TRANSECT),]
  rownames(results) <- NULL

  checkEquals(expected, results
             ,"Error: nWadeableStationsPerTransect is broken"
             )
}



renameTest <- function(verbose=FALSE) {   
    dfTest<-data.frame(idx1=c('one','two','two','three')
                      ,idx2=c(1,1,2,3)
                      ,coverA=c(1, 1, 1 ,NA)
                      ,coverB=c(2, 2, 2 ,NA)
                      ,coverC=c(1, 1, NA,NA)
                      ,coverD=c(3, 3, 3 ,NA)
                      ,coverE=c(2, 2, NA,NA)
                      ,coverF=c(0, 3, 3 ,NA)
                      ,coverG=c(2, 2, 2 ,NA)
                      ,coverH=c(0, 2, 3 ,NA)
                      )

    rr<-rename(dfTest
              ,c('coverA','coverB','coverC','coverD'
                ,'coverE','coverF','coverG','coverH'
                )
              ,LETTERS[1:8]
              )
    checkEquals(c('idx1','idx2',LETTERS[1:8]), names(rr)
               ,"Error: rename failed basic renaming"
               )

    rr<-rename(dfTest
              ,c('coverA','coverC','coverE','coverG'
                ,'coverB','coverD','coverF','coverH'
                )
              ,c('A','C','E','G','B','D','F','H')
              )
    checkEquals(c('idx1','idx2',LETTERS[1:8]), names(rr)
               ,"Error: rename failed out-of-order renaming"
               )

}



uidCreateSeparateTest <- function(verbose=FALSE) { 
# Tests uidCreate() and uidSeparate() in tandem.

  df0 <- expand.grid(k1=1:10
                    ,k2=c('a','b','c','d')
                    ,k3=1:3
                    ,k4=c('foo','bar','baz','gnip','gnop')
                    )
  df0 <- data.frame(df0, stringsAsFactors=FALSE)
  df0$k2 <- as.character(df0$k2)
  df0$k4 <- as.character(df0$k4)
  df0$x<-row(df0[1])
  df0$y<-runif(length(df0[,1]), 0, 1)
  
  # make a uid and parse it.
  df1<-df0
  df1$uid <- uidCreate(df1, c('k1','k2','k3','k4'))
  df1 <- uidSeparate(df1, 'uid', c('t1','t2','t3','t4'))
  df1$t1 <- as.integer(df1$t1)
  df1$t3 <- as.integer(df1$t3)

  # check the key values and their orders.
  checkTrue(all(df1$k1==df1$t1 & df1$k2==df1$t2 & df1$k3==df1$t3 & df1$k4==df1$t4)
           ,"Error: uidCreate/uidSeparate failed to maintain key values"
           )

  # check data associated with keys
  df2<-df1[,c('t1','t2','t3','t4','x','y')]
  df2<-rename(df2, c('t1','t2','t3','t4'), c('k1','k2','k3','k4'))
  checkTrue(identical(df0,df2)
           ,"Error: uidCreate/uidSeparate failed to maintain data values"
           )


  # try test again with odd separator in uid
  df1<-df0
  df1$uid <- uidCreate(df1, c('k1','k2','k3','k4'), sep='%\\^')
  df1 <- uidSeparate(df1, 'uid', c('t1','t2','t3','t4'), sep='%\\^')
  df1$t1 <- as.integer(df1$t1)
  df1$t3 <- as.integer(df1$t3)

  # check the key values and their orders.
  checkTrue(all(df1$k1==df1$t1 & df1$k2==df1$t2 & df1$k3==df1$t3 & df1$k4==df1$t4)
           ,"Error: uidCreate/uidSeparate failed to maintain key values with odd separator"
           )

  # check data associated with keys
  df2<-df1[,c('t1','t2','t3','t4','x','y')]
  df2<-rename(df2, c('t1','t2','t3','t4'), c('k1','k2','k3','k4'))
  checkTrue(identical(df0,df2)
           ,"Error: uidCreate/uidSeparate failed to maintain data values with odd separator"
           )

}



# end of file
