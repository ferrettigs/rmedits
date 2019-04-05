#' Get Score for Stock
#'
#' @param dat data.frame having number of individuals per GSA, Stock and year.
#' @param code MEDITS code
#' @param GSA MEDITS gsa
#' @export
getStockScore = function(dat = tab1, code = "ARISFOL", GSA = 9){
	dat2 = tabMeasuredAnimals(dat = dat, code = code)
	dat3 = dat2[,as.character(GSA)]
	qu = quantile(dat3, p = c(.25), na.rm = TRUE)
	dat3 = dat3[!is.na(dat3)]
	score = length(dat3[dat3>qu])/length(dat3)
	score = list(score = score, years = length(dat3), ratio = paste(as.character(length(dat3[dat3>qu])),"/",as.character(length(dat3)), sep = ""))
	score	
}