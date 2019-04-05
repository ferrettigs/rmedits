#' Cross Correlation Matrix of Catch at Age
#'
#' @param agedat data.frame having all age class data by species, GSA and year
#' @param species as MEDITS code
#' @param GSA
#' @export
crossCorMatrixPlot = function(dat, species, GSA){
	
	dat = subset(dat, CODE==species & gsa == GSA)
	if (nrow(dat)==0) next	
	dat2 = dat[,3:ncol(dat)]
	dat2 = dat2[order(dat2$year),]
	years = dat2$year # the years are variable
	dat2 = t(as.matrix(dat2))
	dat3 = dat2[2:nrow(dat2),]
	if (class(dat3)=="numeric") stop("not enough data") # there is only one year of data - hence it is a numeric vector
	#dimnames(dat3) = list(0:20, 2002:2016)
	dat3 = dat3[rowSums(dat3)!=0,] # exclude all ages for which there are no data
	#dat3.flq = FLQuant(dat3, dimnames=list(age=0:20, year = 2002:2016))
	dat3.flq = FLQuant(dat3, dimnames=list(age=0:(nrow(dat3)-1), year = years))
	# plot(dat3.flq) # does the line plot by age but it is different in essence than the 	plots I have done above. There should be a problem above. 
	fli <- FLIndex(index=dat3.flq)
	plot(fli)
}	