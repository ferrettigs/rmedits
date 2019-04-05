#' Get and Process Fisheries Discards at Length Data
#'
#' @param dataFile csv of the Fisheries Landings at Length Data
#' @export
getDiscLenData = function(dataFile = "../data/DCF_catch/discards_length.csv"){


	dl = read.csv(dataFile)
	dl$metier = with(dl, paste(fishery, gear, mesh_size_range, sep = "_"))
	dl$gsa = as.numeric(gsub(" |GSA|SA","",dl$area))
	dl$Sum = apply(dl[, grep("lengthclass", names(dl))],1, function(x)sum(x[x!=-1], na.rm = T)) # 
	dl = dl[!dl$gsa %in% c(4, 12, 13, 14, 21, 24, 26:30), ] # removes all non MEDITS GSA

	# 11.1, 11.2 and 11 should go together. I am not sure whether Sardinia has du
	# there are only 8 records as 11.2 and are from Malta. I integrate them with Sardinia

	dl$gsa = with(dl, ifelse(gsa==11.2, 11, gsa)) # aggregate sardinia

	# subset ll for column having length data
	dl$Sum = apply(dl[, grep("lengthclass", names(dl))],1, function(x)sum(x[x!=-1], na.rm = T)) 
	
}	