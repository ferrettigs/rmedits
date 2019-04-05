#' Format MEDITS Coordinates
#'
#' It transorms the coordinates of the hauls in decimal degrees.
#' @param latmed latitude in MEDITS format
#' @param lonmed longitude in MEDITS format
#' @param quadrant code for geographic quadrant. It could be 1 for hauls east of longitude 0, and 7 for hauls west of longitude 0.
#' @export
formatLat = function(latmed, lonmed, quadrant){
	frac <- function(x) abs( x - trunc(x)) # to get a fractional part of the number
	
	lonDeg = floor(lonmed/100) # degress
	
	
	lonMin = lonmed/100
	lonMin = frac(lonMin)/60*100
	lon = lonDeg+lonMin
	lon = ifelse(quadrant==1, lon, -1*lon) # getting right sign
	
	latDeg = floor(latmed/100) # degress
	
	
	latMin = latmed/100
	latMin = frac(latMin)/60*100
	lat = latDeg+latMin
	
	return(c(lat,lon))


}
