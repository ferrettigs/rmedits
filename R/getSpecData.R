#' Get MEDITS data by Species
#'
#' @param con connection to the MEDITS database.
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @export
getSpecData = function(con, code = "ELEDMOS", gsa = "all"){
	
	if (gsa[1]=="all") cond = "" else cond = paste(" where area in (",paste(gsa,collapse=","),")", sep="")
	# load ta table
	ta = getMedits(con, statement = paste("select * from ta",cond,";",sep=""))
	ta$swept = with(ta, wing_opening/10*distance)/1000000 # in square kilometers
	names(ta)[3]<- "gsa"
	ta[,c("s_lat","s_lon")] = with(ta,formatLat(shooting_latitude, shooting_longitude, shooting_quadrant))
	ta$mdepth = apply(ta[,c("shooting_depth","hauling_depth")], 1, mean)
	
	# load tb table
	tb = getMedits(con, statement = paste("select * from tb",cond,";",sep=""))
	names(tb)[3]<- "gsa"
	tb$code = with(tb, paste(genus, species, sep = ""))
		
	# subset for species code
	tbs = tb[tb$code==code,] # subset tb by species
	# common fields
	haulid = c("country","gsa","year","month","day","vessel","haul_number") 
	 
	subdat = merge(tbs, ta, by = haulid, all.y = TRUE) # I will retain all the tows in ta. 
	subdat = subdat[subdat$validity=="V",] # retains only valid tows
	subdat = subdat[!is.na(subdat$haul_number),] # exclude tows without haul number
	subdat = subdat[!is.na(subdat$id.y),] # this line would be redundant with the above
	subdat$catch = with(subdat, ifelse(is.na(code), 0, 1)) # flag tows with no catches

	
	subdat$nbtot[is.na(subdat$nbtot)]<-0 
	subdat$ind_skm = with(subdat, nbtot/swept) # in ind/km2
	subdat$ind_skm[is.na(subdat$ind_skm)]<-0 # i am not sure why swept was not reported here
	subdat$ptot[is.na(subdat$ptot)]<-0 
	subdat$p_skm = with(subdat, ptot/swept) # in g/km2
	subdat$p_skm[is.na(subdat$p_skm)]<-0 # i am not sure why swept was not reported here
	subdat$p_skm = subdat$p_skm/1000 # for index in kg/km2

	subdat
}	