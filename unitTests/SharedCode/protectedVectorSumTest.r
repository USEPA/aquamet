# protectedVectorSumTest.r
#
# 12/27/23 cws Created for aquamet use initially.

protectedVectorSumTest <- function()
# unit test for protectedVectorSum
{
    # Try with normal cases, na.rm=default
    expected <- c(1, 2, 3, 4,NA, 6, NA,  8)
    actual <- protectedVectorSum(c( 1, 2, 3, 4,NA, 6, NA,  8), na.rm=TRUE)
    checkEquals(expected, actual, "incorrect with simple numeric case with one vector")
    
    expected <- c(11, 22, 33, NA, NA, 66,  NA, 88)
    actual <- protectedVectorSum(c( 1, 2, 3, 4,NA, 6, NA,  8)
                                ,c(10,20,30,NA,50,60, NA, 80)
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with two vectors")
    
    expected <- c(111, NA, 333,  NA, NA, 666,  NA, 888)
    actual <- protectedVectorSum(c(  1,  2,  3,  4, NA,  6, NA,   8)
                                ,c( 10, 20, 30, NA, 50, 60, NA,  80)
                                ,c(100, NA,300,400,500,600, NA, 800)
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with three vectors")

    
    # Try with normal cases, na.rm=TRUE
    expected <- c(1, 2, 3, 4,NA, 6, NA,  8)
    actual <- protectedVectorSum(c( 1, 2, 3, 4,NA, 6, NA,  8), na.rm=TRUE)
    checkEquals(expected, actual, "incorrect with simple numeric case with one vector, na.rm=TRUE")
    
    expected <- c(11, 22, 33,  4, 50, 66,  NA, 88)
    actual <- protectedVectorSum(c( 1, 2, 3, 4,NA, 6, NA,  8)
                                ,c(10,20,30,NA,50,60, NA, 80)
                                ,na.rm=TRUE
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with two vectors, na.rm=TRUE")
    
    expected <- c(111, 22, 333,  404, 550, 666,  NA, 888)
    actual <- protectedVectorSum(c(  1,  2,  3,  4, NA,  6, NA,   8)
                                ,c( 10, 20, 30, NA, 50, 60, NA,  80)
                                ,c(100, NA,300,400,500,600, NA, 800)
                                ,na.rm=TRUE
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with three vectors, na.rm=TRUE")

    actual <- protectedVectorSum(c(  1,  2,  3,  4, NA,  6, NA,   8) %>% as.integer()
                                ,c( 10, 20, 30, NA, 50, 60, NA,  80)
                                ,c(100, NA,300,400,500,600, NA, 800) %>% as.numeric()
                                ,na.rm=TRUE
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with three vectors of different types, na.rm=TRUE")

   
    # Exercise sanity checks
    expected <- "Data arguments must all be vectors"
    actual <- protectedVectorSum(c(), na.rm=TRUE)
    checkEquals(expected, actual, "incorrect with one vector c(), na.rm=TRUE")
    
    expected <- NULL
    actual <- protectedVectorSum(as.integer(), na.rm=TRUE)
    checkEquals(expected, actual, "incorrect with one integer vector of zero length, na.rm=TRUE")
    
    expected <- "Data arguments must all be vectors"
    actual <- protectedVectorSum(data.frame(c(  1,  2,  3,  4, NA,  6, NA,   8)
                                           ,c( 10, 20, 30, NA, 50, 60, NA,  80)
                                           )
                                , na.rm=TRUE)
    checkEquals(expected, actual, "incorrect with data.frame input, na.rm=TRUE")
    
    expected <- "Vectors must be numeric or integer"
    actual <- protectedVectorSum(c(  1,  2,  3,  4, NA,  6, NA,   8) %>% as.integer()
                                ,c( 10, 20, 30, NA, 50, 60, NA,  80) %>% as.character()
                                ,c(100, NA,300,400,500,600, NA, 800)
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with three vectors")
    
    expected <- "Vectors must all be the same length"
    actual <- protectedVectorSum(c(  1,  2,  3,  4, NA,  6, NA,   8)
                                ,c( 10, 20, 30, NA, 50, 60, NA,  80)
                                ,as.integer()
                                )
    checkEquals(expected, actual, "incorrect with simple numeric case with three vectors")
    
}

# end of file