#' Get MEDITS Reference List and correspondent commercial codes
#'
#' @export
getSpecTab = function(){
	
	specs = read.csv("../data/ASFIS_sp/ASFIS_sp_Feb_2017.txt",sep="\t", 	stringsAsFactors=FALSE)
	specs = merge(rl, specs, by.x = "sciname", by.y = "Scientific_name", all.x = T)
	specTab = specs[,c(1,2,5)]
	names(specTab) = c("sciname","CODE","commcode")
	specTab

}