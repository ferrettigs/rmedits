#' Plot Measured Animals
#'
#' @param dat data.frame of aggregated length or discard data by GSA and Species
#' @param code MEDITS code
#' @export

plotMeasuredAnimals = function(dat = tab1, code){

		subtab1 = subset(tab1,CODE==code)
	
		p  = ggplot(subtab1, aes(year, gsa)) +
		geom_raster(aes(fill = log(inds)))+
		scale_x_discrete(limits=subtab1$year)+
		scale_y_discrete(limits=subtab1$gsa)+
		scale_fill_gradient(low = "white", high = "red")
		p

	
}

#' Tabulate Measured Animals
#'
#' @param dat data.frame of aggregated length or discard data by GSA and Species
#' @param code MEDITS code
#' @export
tabMeasuredAnimals = function(dat = tab1, code){

	subtab1 = subset(tab1,CODE==code)
	widetab1 = reshape2::acast(subtab1, year~gsa, value.var="inds")
	widetab1

}