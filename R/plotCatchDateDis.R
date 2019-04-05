#' Plot Catch Date Distribution
#'
#' boxplot showing min, max and mean day in year of survey data by year (or 5,50,95%).
#' @param con connection to the MEDITS database.
#' @param code species code.
#' @param gsa gsa number, vector of numbers (for gsa groups), or "all" for the entire MEDITS' survey area.
#' @export

plotCatchDateDis = function(con, code, gsa){
	require(lubridate)
	dat = getSpecData(con, code, gsa)
	dat = dat[with(dat, (ptot+nbtot)>0),] # they should be either 0 or something
	dat$Date = with(dat, as.Date(paste(year,month, day, sep="-"),format = "%Y-%m-%d"))
	dat$yday = yday(dat$Date)
	x<-boxplot(yday~year, dat, axes = F, xlab = "Year", main = "Date distributions")
	axis(1, at = 1:length(unique(dat$year)), labels = x$names)
	axis(2, at = seq(50,350,50),labels = month.abb[month(as.Date(seq(50,350,50), origin = 	"2005-01-01"))], las = 1)
}
