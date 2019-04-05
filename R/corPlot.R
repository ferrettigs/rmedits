#' Correlation Plot of Catch at Age
#'
#' @param agedat data.frame having all age class data by species, GSA and year
#' @param species as MEDITS code
#' @param GSA
#' @export
corPlot = function(dat, species, GSA){

	 dat = subset(tcnage3, CODE==species & gsa == GSA)
	 if (nrow(dat)==0) stop(paste("There is no data for GSA ",GSA,sep=""))

	par(mfrow=c(5,5), mar = c(4,4,1,1))
	for (i in 0:20){

		y = 4+i
		if (sum(dat[2:nrow(dat),y+1])==0) next
		plot(dat[1:nrow(dat)-1,y], dat[2:nrow(dat),y+1], xlab = paste("age ",i,sep=""), ylab = paste("age ",i+1,sep=""), las = 1, pch=16)
	
		m1 = lm(dat[2:nrow(dat),y+1]~dat[1:nrow(dat)-1,y])
	
	#if(class(lin)=="try-error") next
	
	
		lin = try(abline(m1))
	}
}