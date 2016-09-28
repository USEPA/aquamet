\name{aquamet-internal}
\alias{assignTaxCat}
\alias{calcSynCovers}
\alias{calcSynInfluence}
\alias{convert_to_char}
\alias{count}
\alias{dfCompare}
\alias{dfLengthen}
\alias{dfWiden}
\alias{expand.data.frame}
\alias{fillinDrawdownData}
\alias{fillinDrawdownData.expansion}
\alias{first}
\alias{gmean}
\alias{idr}
\alias{iqr}
\alias{interpolatePercentile}
\alias{is.subset}
\alias{intermediateMessage}
\alias{lag}
\alias{last}
\alias{lead}
\alias{modalClass}
\alias{modalClasses}
\alias{modalCount}
\alias{modalvalue}
\alias{modalValues}
\alias{normalizedCover}
\alias{nWadeableStationsPerTransect}
\alias{protectedMean}
\alias{protectedSum}
\alias{rename}
\alias{siteProtocol}
\alias{siteProtocol.1}
\alias{summaryby}
\alias{trimws}
\alias{uidCreate}
\alias{uidSeparate}

\alias{metsBankMorphology.1}
\alias{metsBedStability.1}
\alias{metsCanopyDensiometer.1}
\alias{metsChannelChar.1}
\alias{metsChannelHabitat.1}
\alias{metsChannelMorphology.1}
\alias{metsFishCover.1}
\alias{metsGeneral.1}
\alias{metsHumanInfluence.1}
\alias{metsInvasiveSpecies.1}
\alias{metsLgWoody.1}
\alias{metsLegacyTree.1}
\alias{metsLittoralDepth.1}
\alias{metsResidualPools.1}
\alias{metsResidualPools.dataOrganization}
\alias{metsResidualPools.dimensions}
\alias{metsResidualPools.poolCharacteristics}
\alias{metsResidualPools.siteSummaries}
\alias{metsRiparianVegetation.1}
\alias{metsSlopeBearing.1}
\alias{metsSlopeBearing.adjustElevations}
\alias{metsSubstrateCharacterization.1}
\alias{metsSubstrateEmbed.1}

\alias{runInvertMetrics}
\alias{runFishMetrics}
\alias{Dominance}
\alias{ShanDiversity}
\alias{tolindex}
\alias{tolindexFish}

\alias{metsAquaticMacrophytes.individualCover}
\alias{metsAquaticMacrophytes.indices}
\alias{metsBankFeatures.standardizeData}
\alias{metsBankFeatures.bankAngle}
\alias{metsBankFeatures.distances}
\alias{metsBankFeatures.fillinDistances}
\alias{metsBottomSubstrate.setupForParticleCalculations}
\alias{metsBottomSubstrate.indivPresence}
\alias{metsBottomSubstrate.variety}
\alias{metsBottomSubstrate.indivCover}
\alias{metsBottomSubstrate.populationEstimates}
\alias{metsBottomSubstrate.modeCover}
\alias{metsBottomSubstrate.indivColor}
\alias{metsBottomSubstrate.modeColor}
\alias{metsBottomSubstrate.indivOdor}
\alias{metsBottomSubstrate.modeOdor}
\alias{metsFishCover.calculateMets}
\alias{metsFishCover.splitParameterNames}
\alias{metsFishCover.indivCovers}
\alias{metsFishCover.groupIndices}
\alias{metsHumanImpact.calculateMets}
\alias{metsHumanImpact.weightedIndividualInfluence}
\alias{metsHumanImpact.overallInfluence}
\alias{metsHumanImpact.circaInfluence}
\alias{metsHumanImpact.anyPresence}
\alias{metsHumanImpact.weightedGroupInfluence}
\alias{metsLittoralMacrohabitat.cover}
\alias{metsLittoralMacrohabitat.coverTypes}
\alias{metsLittoralMacrohabitat.humanDist}
\alias{metsLittoralMacrohabitat.substrate}
\alias{metsLittoralZone.prepareInput}
\alias{metsLittoralZone.modeFilm}
\alias{metsLittoralZone.fractionalPresences}
\alias{metsLittoralZone.filmVariety}
\alias{metsRiparianVegetation.calculateMets}
\alias{metsRiparianVegetation.canopyTypePresence}
\alias{metsRiparianVegetation.understoryTypePresence}
\alias{metsRiparianVegetation.componentPresence}
\alias{metsRiparianVegetation.componentCovers}
\alias{metsRiparianVegetation.compositeIndex}
\alias{metsStationInformation.islandStations}
\alias{metsStationInformation.stationDepths}
\title{Internal aquamet Functions}
\description{
  These functions are internal aquamet functions.
}
\usage{
assignTaxCat(lt)
calcSynCovers(coverData, maxDrawdown, assumptions=FALSE)
calcSynInfluence(influenceData)
convert_to_char(df)
count(x)
dfCompare(df1, df2, byVars, zeroFudge=1e-17, verbose=FALSE)
dfLengthen(df, keys, name, value, values)
dfWiden(df, keys, name, values, makeNumeric=TRUE)
expand.data.frame(df, cols)
fillinDrawdownData(df, fillinValue='0', fillinHORIZ_DIST_DD='0')
fillinDrawdownData.expansion(df)
first(df, v, first.v)
gmean(x)
idr(x, method=2)
iqr(x, method=2)
interpolatePercentile(df, classVar, percentile, pctlVar, classBounds)
is.subset(a, b)
intermediateMessage(text, loc='middle', intermediateMessages=TRUE)
lag(df, v, lag.v, offset=1)
last(df, v, last.v)
lead(df, v, lead.v, offset=1)
modalClass(x, values, classes)
modalClasses(df, classes, weights, delim = ', ')
modalCount(x)
modalvalue(x, na.rm=FALSE)
modalValues(x, delim='&', na.rm=FALSE)
normalizedCover(df, coverValue, coverNorm, allowTotalBelow100=FALSE)
nWadeableStationsPerTransect(thal)
protectedMean(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
protectedSum(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
rename(df, old, new) 
siteProtocol(sites, visits)
siteProtocol.1(sites, siteInfo)
summaryby(xxx,yyy,zzz)
trimws(text)
uidCreate(df, keys, sep='+')
uidSeparate(df, uidName, keyNames, sep='+')

metsBankMorphology.1(df1, protocols)
metsBedStability.1(mets, protocols)
metsCanopyDensiometer.1(indat)
metsChannelChar.1(indat, chanCon)
metsChannelHabitat.1(indat)
metsChannelMorphology.1(df1, df2, protocols)
metsFishCover.1(fishcover, protocols)
metsGeneral.1(indat)
metsHumanInfluence.1(df)
metsInvasiveSpecies.1(df)
metsLgWoody.1(df1, df2, df3, protocols)
metsLegacyTree.1(df1)
metsLittoralDepth.1(indat)
metsResidualPools.1(thal, actransp, slopes, protocols,
  writeIntermediateFiles=FALSE, oldeMethods=FALSE)
metsResidualPools.dataOrganization(thal, actransp, slopes)
metsResidualPools.dimensions(thalSeries, thalProtocol, minSampPct=85,
  oldeMethods=FALSE)
metsResidualPools.poolCharacteristics(poolDims)
metsResidualPools.siteSummaries(poolInfo, stationInfo, protocols)
metsRiparianVegetation.1(visrip)
metsSlopeBearing.1(thal, chanGeom, gisCalcs, protocols)
metsSlopeBearing.adjustElevations(df)
metsSubstrateCharacterization.1(df1, df2, df3)
metsSubstrateEmbed.1(indat)

runInvertMetrics(indata, parlist, taxalist)
runFishMetrics(indata, parlist, prefix, taxalist)
Dominance(df, topN=1)
ShanDiversity(indata)
tolindex(indata, taxalist)
tolindexFish(indata, taxalist)

metsAquaticMacrophytes.individualCover(df, presenceWeights, coverWeights)
metsAquaticMacrophytes.indices(df)
metsBankFeatures.standardizeData(df, data2007)
metsBankFeatures.bankAngle(df)
metsBankFeatures.distances(df)
metsBankFeatures.fillinDistances(df)
metsBottomSubstrate.setupForParticleCalculations(df)
metsBottomSubstrate.indivPresence(bsPresence)
metsBottomSubstrate.variety(bsData)	
metsBottomSubstrate.indivCover(bsData)
metsBottomSubstrate.populationEstimates(bsData)
metsBottomSubstrate.modeCover(presences, covers)
metsBottomSubstrate.indivColor(df)
metsBottomSubstrate.modeColor(df)
metsBottomSubstrate.indivOdor(df)
metsBottomSubstrate.modeOdor(df)
metsFishCover.calculateMets(fcDataCalcs)
metsFishCover.splitParameterNames(parameters)
metsFishCover.indivCovers(fcDataCalcs)
metsFishCover.groupIndices(fcDataCalcs)
metsHumanImpact.calculateMets(hiData)
metsHumanImpact.weightedIndividualInfluence(hiData)
metsHumanImpact.overallInfluence(hiData)	
metsHumanImpact.circaInfluence(hiData)
metsHumanImpact.anyPresence(hiData)  
metsHumanImpact.weightedGroupInfluence(hiData)
metsLittoralMacrohabitat.cover(df)
metsLittoralMacrohabitat.coverTypes(df)
metsLittoralMacrohabitat.humanDist(df)
metsLittoralMacrohabitat.substrate(df)
metsLittoralZone.prepareInput(df, data2007)
metsLittoralZone.modeFilm(df, data2007)
metsLittoralZone.fractionalPresences(df, data2007)
metsLittoralZone.filmVariety(df, data2007)
metsRiparianVegetation.calculateMets(rvData)
metsRiparianVegetation.canopyTypePresence(rvData)
metsRiparianVegetation.understoryTypePresence(rvData)
metsRiparianVegetation.componentPresence(rvData)
metsRiparianVegetation.componentCovers(allFractions)
metsRiparianVegetation.compositeIndex(groupData, indexName)
metsStationInformation.islandStations(df)
metsStationInformation.stationDepths(df)
}
\details{
  These functions are not to be called by the user.
}
\keyword{internal}
