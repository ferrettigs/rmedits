#' Plot Age Class Time Series
#'
#' @param agedat data.frame having all age class data by species, GSA and year
#' @param species
#' @param GSA
#' @export
plotAgeTrend = function(agedat, species, GSA){
		dat = agedat[,c("CODE","gsa","year","ageclass","number")]
		#dat = agedat[agedat$CODE==species & agedat$gsa==GSA,]
		dat = subset(dat, CODE==species & gsa==GSA)
		if (nrow(dat)==0) stop(paste("There is no data for GSA ",GSA,sep=""))
		ggplot(hke,aes(x=year,y=number))+
      		geom_line() +
      		facet_wrap(~ageclass, nrow=21, scales = "free_y", strip.position = "right", dir = "v")+
      		labs(title=paste(species," in GSA ",GSA, sep = ""))
}      		


#' Cross Correlation Matrix of Catch at Age
#'
#' @param agedat data.frame having all age class data by species, GSA and year
#' @param species as MEDITS code
#' @param GSA
#' @export
plotAgeTrendFL = function(dat, species, GSA){
	
	dat = subset(dat, CODE==species & gsa == GSA)
	if (nrow(dat)==0 | nrow(dat)==1) stop("not enough data") # if there is only one year the plot does not work	
	dat2 = dat[,3:ncol(dat)]
	dat2 = dat2[order(dat2$year),]
	years = dat2$year # the years are variable
	dat2 = t(as.matrix(dat2))
	dat3 = dat2[2:nrow(dat2),]
	#dimnames(dat3) = list(0:20, 2002:2016)
	dat3 = dat3[rowSums(dat3)!=0,] # exclude all ages for which there are no data
	#dat3.flq = FLQuant(dat3, dimnames=list(age=0:20, year = 2002:2016))
	if (class(dat3)=="numeric") stop("there is only one cohort across years") # it is numeric because there is only a vectors if there were multiple classes it would have been a matrix
	dat3.flq = FLQuant(dat3, dimnames=list(age=0:(nrow(dat3)-1), year = years))
	plot(dat3.flq) # does the line plot by age but it is different in essence than the 	plots I have done above. There should be a problem above. 
	
}	