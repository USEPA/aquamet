#' aquamet internal functions
#' @name aquamet-internal
#' @aliases assignTaxCat calcSynCovers calcSynInfluence convert_to_char count
#' dfCompare dfLengthen dfWiden expand.data.frame fillinDrawdownData fillinDrawdownData.expansion
#' first gmean idr iqr interpolatePercentile is.subset intermediateMessage lag last lead 
#' modalClass modalClasses modalCount modalvalue modalValues normalizedCover nWadeableStationsPerTransect 
#' protectedMean protectedSum rename summaryby trimws uidCreate uidSeparate 
#' 
#' @description These are internal functions to aquamet and not intended for use on their own.
#' 
#' @usage 
#' assignTaxCat(lt)
#' calcSynCovers(coverData, maxDrawdown, assumptions=FALSE)
#' calcSynInfluence(influenceData)
#' convert_to_char(df)
#' count(x)
#' dfCompare(df1, df2, byVars, zeroFudge=1e-17, verbose=FALSE)
#' dfLengthen(df, keys, name, value, values)
#' dfWiden(df, keys, name, values, makeNumeric=TRUE)
#' expand.data.frame(df, cols)
#' fillinDrawdownData(df, fillinValue='0', fillinHORIZ_DIST_DD='0')
#' fillinDrawdownData.expansion(df)
#' first(df, v, first.v)
#' gmean(x)
#' idr(x, method=2)
#' iqr(x, method=2)
#' interpolatePercentile(df, classVar, percentile, pctlVar, classBounds)
#' is.subset(a, b)
#' intermediateMessage(text, loc='middle', intermediateMessages=TRUE)
#' lag(df, v, lag.v, offset=1)
#' last(df, v, last.v)
#' lead(df, v, lead.v, offset=1)
#' modalClass(x, values, classes)
#' modalClasses(df, classes, weights, delim = ', ')
#' modalCount(x)
#' modalvalue(x, na.rm=FALSE)
#' modalValues(x, delim='&', na.rm=FALSE)
#' normalizedCover(df, coverValue, coverNorm, allowTotalBelow100=FALSE)
#' nWadeableStationsPerTransect(thal)
#' protectedMean(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
#' protectedSum(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
#' rename(df, old, new) 
#' siteProtocol(sites, visits)
#' siteProtocol.1(sites, siteInfo)
#' summaryby(xxx,yyy,zzz)
#' trimws(text)
#' uidCreate(df, keys, sep='+')
#' uidSeparate(df, uidName, keyNames, sep='+')
NULL