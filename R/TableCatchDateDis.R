#' Tabulate Catch Date Distribution
#' 
#' @param con connection to the MEDITS database
#' @param code species MEDITS code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @export
tableCatchDateDis = function(con, code, gsa){
	dat = getSpecData(con, code = code, gsa = gsa)
	dat = getSpecData(con, code, gsa)
	dat = dat[with(dat, (ptot+nbtot)>0),] # they should be either 0 or something
	require(lubridate)
	dat$Date = with(dat, as.Date(paste(year,month, day, sep="-"),format = "%Y-%m-%d"))
	dat$yday = yday(dat$Date)
	d1 = with(dat, aggregate(list(yday = yday), list(year = year), function(x)c(quantile(x, probs = c(0.05,0.25, 0.5,0.75, 0.95)))))
	d2 = with(dat, aggregate(list(meanYday = yday), list(year = year), mean))
	merge(d1, d2, by = "year")
}