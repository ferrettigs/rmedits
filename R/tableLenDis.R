#' Tabulate Length Distribution
#' 
#' it connects to the tc table and estract the focal species and gsa. Then it tabulates the length classes of the sample
#' @param con database connection
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @export
tableLenDis = function(con, code, gsa){
	genus = substr(code,1,4)
	species = substr(code,5,7)
	if (gsa[1]=="all") cond = "" else cond = paste(" and area in (",paste(gsa,collapse=","),")", sep="")
	tcs = getMedits(con, statement = paste("select * from tc where genus = '",genus,"' and species = '",species,"'",cond,";", sep =""))
	with(tcs, aggregate(list(lc = length_class), list(year = year), function(x)quantile(x, probs = c(0.05,0.25, 0.5,0.75, 0.95))))
}

#' Boxplot of Length Distribution
#' 
#' it connects to the tc table and estract the focal species and gsa. Then it draws a boxplot of length distributions by year
#' @param con database connection
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @export
plotLenDis = function(con, code, gsa){
	genus = substr(code,1,4)
	species = substr(code,5,7)
	if (gsa[1]=="all") cond = "" else cond = paste(" and area in (",paste(gsa,collapse=","),")", sep="")
	tcs = getMedits(con, statement = paste("select * from tc where genus = '",genus,"' and species = '",species,"'",cond,";", sep =""))
	boxplot(length_class~year, tcs, xlab = "Year", ylab = "Length class (mm)", las = 1, axes = F, main = "Length distributions")
	axis(1)
	axis(2, las = 1)
}