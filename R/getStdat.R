#' Get MEDITS Strata Features
#'
#' @export
getStdat = function(){

	stdat = read.csv("../data/stratificationScheme.csv", as.is = T) # load info for strata. The stratum is unique and thus the sector does not discriminate anything new
	ta = getMedits(con, statement = paste("select * from ta;",sep=""))
	ta$swept = with(ta, wing_opening/10*distance)/1000000 # in square kilometers
	names(ta)[3]<- "gsa"
	# next are the strata only in stdate and not in ta - I am not sure how I do have to deal with them. 
	setdiff(unique(stdat$nstrate), unique(ta$nstrate))
	# These are part from Morocco and part from European Countries

	stdat$country[stdat$nstrate %in% setdiff(unique(stdat$nstrate), unique(ta$nstrate))]


	stdat$country = with(stdat,ifelse(country == "Croatia","HRV", country))
	stdat$country = with(stdat,ifelse(country == "Italy","ITA", country))
	stdat$country = with(stdat,ifelse(country == "Spain","ESP", country))
	stdat$country = with(stdat,ifelse(country == "France","FRA", country))
	stdat$country = with(stdat,ifelse(country == "Cyprus","CYP", country))
	stdat$country = with(stdat,ifelse(country == "Slovenia","SVN", country))
	stdat$country = with(stdat,ifelse(country == "Greece","GRC", country))
	stdat$country = with(stdat,ifelse(country == "Malta","MLT", country))

	stdat$totalSurface = sum(stdat$surface) # total surface of the medits survey
	stdat$W = with(stdat, surface/totalSurface)

	stdat$avg_swept = mean(ta$swept, na.rm = T)
	stdat$N = with(stdat, totalSurface/avg_swept)
	gsaAreas = with(stdat, aggregate(list(surfaceGSA = surface), list(gsa = gsa), sum))
	stdat = merge(stdat, gsaAreas, by = "gsa", all.x = T)
	stdat$Wgsa = with(stdat, surface/surfaceGSA)
	stdat
}	