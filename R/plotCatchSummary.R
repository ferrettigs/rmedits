#' Plot Index of Abundance Over Time
#'
#' @param dat data.frame coming from \code{catchSummary}.
#' @param ylab lab for y-axis in plot
#' @export
plotCatchSummary = function(dat = dat1, ylab = "#/km2"){
	main = ifelse(ylab == "#/km2", "CPUE", "BPUE")
	plot(tot_index~year, dat, pch = 16, axes =F, ylab = ylab, ylim = range(0, max(upper)), main = main)
	with(dat, segments(year, lower, year, upper))
	axis(1)
	axis(2, las = 1)
}