#' Load TA from the Database
#'
#' It loads the TA table and preforms some data management to obtain desired fields for analysis
#' @param con a connection to the medits Database. 
#' @export

getTA = function(con){
	ta = getMedits(con, statement = "select * from ta;")
	# calculating swept area. Horizontal opening is distance between wings (decimeters) by distance
	ta$swept = with(ta, wing_opening/10*distance)/1000000 # in square kilometers
	names(ta)[3]<- "gsa"
	stdat = getStdat()
	ta = merge(ta, stdat, by= intersect(names(ta), names(stdat)), all = TRUE)
	ta[,c("s_lat","s_lon")] = with(ta,formatLat(shooting_latitude, shooting_longitude, shooting_quadrant))
	ta
}	