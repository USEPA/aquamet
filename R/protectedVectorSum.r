# protectedVectorSum.r
#
# 12/27/23 cws Created for aquamet use initially.

protectedVectorSum <- function(...,  na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE)
# Returns vector with sums of individual elements from input vectors, or 
# character string describing the error if one is detected. Is like protectedSum
# but results in a vector instead of scalar value.
#
# Correctly ignore NA values unless values at a position are NA in all vectors,
# in which case the sum is NA rather than zero. For example,
#   a<-c( 1, 2, 3, 4,NA, 6, NA,  8)
#   b<-c(10,20,30,NA,50,60, NA, 80)
#   c(a,b) %>% matrix(ncol=2) %>% rowSums(na.rm=TRUE)
# results in 
#   [1] 11 22 33  4 50 66  0 88   :: That '0' should be NA
#
{
    vecs <- list(...)
    
    # Check data are all vectors
    allVectors <- vecs %>% lapply(is.vector) %>% unlist() %>% all()
    if(!allVectors) return('Data arguments must all be vectors')
    
    # Check vectors are all same length
    nLengths <- vecs  %>% lapply(length) %>% unlist() %>% unique() %>% length()
    if(nLengths != 1) return("Vectors must all be the same length")
    
    # Check vectors are all numbery
    nonNumberyTypes <- vecs  %>% lapply(class) %>% unlist() %>% setdiff(c('numeric','integer')) %>% length()
    if(nonNumberyTypes > 0) return('Vectors must be numeric or integer')
    
    # Sum up the vector elements
    vectorSum <- vecs %>% 
                 unlist() %>%
                 # combine vectors into a dataframe
                 # of 1 row per vector and with a column for each station
                 matrix(ncol=length(vecs)) %>% 
                 t() %>% 
                 data.frame() %>%
                 # sum each station, caring for NA values
                 lapply(function(x) {protectedSum(x,na.rm=na.rm, inf.rm=inf.rm, nan.rm=nan.rm)} ) %>% 
                 unlist()
    names(vectorSum) <- NULL
    return(vectorSum)
}

# end of file