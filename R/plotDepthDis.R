#' Plot Depth Distribution Density
#'
#' @param dat a merged TA and TB file with catches of a given species. This data.frame can be produced with the function \code{getSpecData()}
#' @export
plotDepthDis = function(dat){
		d <- density(dat$mdepth) # returns the density data
 		d2<- density(dat$mdepth[dat$ind_skm>0])
 		plot(d, ylim = range(c(d$y, d2$y)), xlab = "Depth (m)", main = "Haul distribution", axes = F)
 		axis(1)
 		axis(2, las = 1)
 		polygon(d2, col = "black")
}