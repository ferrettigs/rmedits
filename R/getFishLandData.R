#' Get and Process Fisheries Landings at Length Data
#'
#' @param dataFile csv of the Fisheries Landings at Length Data
#' @export
getFishLandData = function(dataFile = "../data/DCF_catch/landings_length.csv"){

	ll = read.csv(dataFile)

# Meta-data for this file are here: 
# https://datacollection.jrc.ec.europa.eu/c/document_library/get_file?uuid=056ed476-519f-4be6-a74f-e99fabf8df8d&groupId=10213
# LENGTHCLASS0 (numbers in thousands, precision =3 digits after the comma)
# not really clesr there are some values that are larger than millions

	ll$metier = with(ll, paste(fishery, gear, mesh_size_range, sep = "_"))
	ll$gsa = as.numeric(gsub(" |GSA|SA","",ll$area))
	ll$Sum = apply(ll[, grep("lengthclass", names(ll))],1, function(x)sum(x[x!=-1], na.rm = T)) # create a variable with the cumulative sum of all fish reported in length classes
# there are 175 metiers but many of these need to be removed because they have null values.
	# subset for gsas in common with the medite surveys.
	ll = ll[!ll$gsa %in% c(4, 12, 13, 14, 21, 24, 26:30), ] # removes all non MEDITS GSA
# 11.1, 11.2 and 11 should go together. I am not sure whether Sardinia has du
# there are only 8 records as 11.2 and are from Malta. I integrate them with Sardinia
	ll$gsa = with(ll, ifelse(gsa==11.2, 11, gsa)) # aggregate sardinia
# subset ll for column having length data
	ll$Sum = apply(ll[, grep("lengthclass", names(ll))],1, function(x)sum(x[x!=-1], na.rm = T)) 
ll
}
