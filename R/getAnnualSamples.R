#' Get Annual Samples
#'
#' Used to get the number of measured animals each year for all species in all GSA
#' @param dat data.frame from \code{getFishLandData()} or \code{getDiscLenData()}
#' @export
getAnnualSamples = function(dat = getFishLandData()){

	ll2 = dat[dat$Sum>0,] # get only data with length class samples
	ll2ygs = with(ll2, aggregate(list(inds = Sum), list(code = species, gsa = gsa, year = year), sum))
	specTab = getSpecTab()
	tab1 = merge(ll2ygs, specTab, by.x = "code", by.y = "commcode", all.x = T)
	tab1 =tab1[,c("CODE", "gsa","year","inds")]
	tab1

}
