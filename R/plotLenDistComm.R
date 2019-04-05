#' Plot Length Distribution Commercial
#'
#' @param dat data.frame of Fisheries landings at length data
#' @param commCode commercial code
#' @details This function shows the contribution of each metier as stacked bar. Showing a plot for each metier was not practical as it would have generated tens of panels for species and GSA.
#' @export

plotLenDistComm = function(dat, meditsCode, GSA){

	llspec = dat[dat$CODE==meditsCode & dat$Sum>0 & dat$gsa==GSA,]
	if (nrow(llspec)==0) stop("No data for this stock")
	
	wide = llspec[,grep("lengthclass|year|metier", names(llspec))] # there are duplicate combinations of metier and year. I guess because of the different vessel length and quarter
	wide = with(wide, aggregate(wide[,grep("lengthclass", names(wide))], list(year = year, metier = metier), function(x)sum(x, na.rm = T))) # 3 I am not taking into account quarter and vessel length and also metier


	long = reshape(wide, idvar = c("year","metier"), varying = grep("lengthclass", names(wide), value=T), v.names = "number", direction = "long", timevar = "lengthclass", times = c(0:100))



	ggplot(long, aes(x=lengthclass, y = number, fill = metier)) + 
 		geom_bar(stat="identity")+	
 		facet_wrap(~ year, scales = "free") 
 		 #		scale_y_continuous(labels = scales::percent)
 	
 }		
 
#' BoxPlot of Commercial Length Data
#'
#' @param dat data.frame of Fisheries landings at length data
#' @param meditsCode medits code
#' @details Box plot of length distributions of the commercial landings aggregated across metiers.
#' @export 
 
boxplotLenDistComm = function(dat, meditsCode, GSA){
 
 	llspec = dat[dat$CODE==meditsCode & dat$Sum>0 & dat$gsa==GSA,]
 	
 	if (nrow(llspec)==0) stop("no data in this GSA")
	wide = llspec[,grep("lengthclass|year|metier", names(llspec))] # there are duplicate combinations of metier and year. I guess because of the different vessel length and quarter
	wide = with(wide, aggregate(wide[,grep("lengthclass", names(wide))], list(year = year, metier = metier), function(x)sum(x, na.rm = T))) # 3 I am not taking into account quarter and vessel length and also metier

	long = reshape(wide, idvar = c("year","metier"), varying = grep("lengthclass", names(wide), value=T), v.names = "number", direction = "long", timevar = "lengthclass", times = c(0:100))
	
	long2 = long[, c("year","lengthclass","number")]
	long2 = long2[long2$number>0,] # unclear how we can have fractional lengthclasses. these are number in thousands
	
	long3 = with(long2, aggregate(cbind(numberTot = number), list(year = year, lengthclass = lengthclass),sum))
	long2 = merge(long2, long3, by = c("year","lengthclass"), all.x =T)
	long2$number = long2$number/long2$numberTot*1000 # number per thousand
	
	
	#if (commCode=="BOG") long2$number = long2$number/1000
 	
 	counts<-data.frame(classes = rep(long2$lengthclass,long2$number), year = rep(long2$year, long2$number)) # rep sounds the number
 	boxplot(classes~year, counts)
 	
 	dat1 = do.call(data.frame,(with(long2, aggregate(lengthclass, list(year = year), function(x)c(quantile(x, p=0.05), quantile(x, p=0.5), quantile(x, p=0.95))))))
 	
 	# apply(dat1[,2:4],2,function(x)text(1:14,x,x, cex = 0.8)) # too cluttering
 	#apply(dat1[,2:4],2,function(x)points(1:14,x,col = "red", pch=16)) # include points at the 5, 50 and 95 percentile
 	apply(dat1[,2:4],2,function(x)segments((1:14)-0.3,x,(1:14)+0.3,x,col = "red", lty=2))
}