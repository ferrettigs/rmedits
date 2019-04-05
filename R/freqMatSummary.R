#' Summary table for Maturity frequencies
#'
#' @param con connection to the medits database
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @param matStage MEDITS maturity code, from 0 to 4 and ND. 0 undetermined, 1 = immature, 2 maturing, 3 mature, 4 spent resting. See manual for details.
#' @param index index of abundance of interest
#' @param stdat data.frame with data on strata
#' @export

freqMatSummary = function(con, code = "ELEDMOS", gsa = 11, stdat, matStage = 3){
	dat = getSpecData(code = code, gsa = gsa)
	tc = getMedits(con, statement = "select * from tc where genus = 'ELED' and species = 'MOS' and area = 11;")
	tc$gsa = tc$area
	haulid = c("country","gsa","year","month","day","vessel","haul_number") 

	tcagg = do.call(data.frame, with(tc, aggregate(list(mat = maturity), tc[, haulid], function(x)c(stage = table(x), ind = length(x))))) # tc aggregated by maturity stage
# the do.call is needed to create a regular dataframe, otherwise the maturity stage columns would be in a matrix form - so I would create a mixed data.frame/matrix
	tcagg$frac.mat = with(tcagg,get(paste("mat.stage.",matStage,sep=""))/mat.ind) # fraction of mature fish in that sample
	dat2 = merge(dat, tcagg, by = haulid, all.x = T)
	dat = dat2
	index = "frac.mat"

	meandat = aggregate(list(str_ind_skm = dat[,index]), dat[,c("year","nstrate")], function(x) mean(x, na.rm = T))
 	#vardat = with(dat, aggregate(list(s2h = index), list(year = year, nstrate = nstrate), function(x)var(x, na.rm = T))) # var index
 	vardat = aggregate(list(s2h = dat[,index]), dat[,c("year","nstrate")], function(x)(length(x)/(length(x)-1))*mean(x, na.rm = T)*(1-mean(x, na.rm = T))) # var index
 	meanvardat = merge(meandat,vardat,by = c("year","nstrate"))
 	towdat = with(dat, aggregate(list(nh = id.y), list(year = year, nstrate = nstrate), length)) # number of tows per strata
 	mvtdat = merge(meanvardat,towdat,by = c("year","nstrate"))
 	z = merge(mvtdat, stdat, by = "nstrate", all.x = TRUE)
	#z$s2h[is.na(z$s2h)]<-0 # having missing variances do not make any difference
	z$Nh = with(z, surface/avg_swept)
	z$N = with(z, totalSurface/avg_swept)
	
	if (gsa!="all") {#z = z[z$gsa==gsa,];
					z$N = with(z, surfaceGSA/avg_swept);
					z$W = z$Wgsa
					}
#summary across strata
	x1 = with(z, aggregate(list(tot_index = str_ind_skm*W), list(year = year), function(x)sum(x, na.rm = T)))
	y1 = with(z, aggregate(list(tot_index_var = (1-(nh/Nh))*(Nh/N)^2*((str_ind_skm*(1-str_ind_skm))/(nh-1))), list(year = year), function(x)sum(x, na.rm = T)))

	x1y1 = merge(x1, y1, by = "year")
	x1y1$CV = with(x1y1, sqrt(tot_index_var)/tot_index)
	x1y1$upper = with(x1y1, tot_index + sqrt(tot_index_var)*1.96)
	x1y1$lower = with(x1y1, tot_index - sqrt(tot_index_var)*1.96)
	x1y1
}



#' Summary table for Maturity frequencies
#'
#' Second method.
#' @param con connection to the medits database
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @param matStage MEDITS maturity code, from 0 to 4 and ND. 0 undetermined, 1 = immature, 2 maturing, 3 mature, 4 spent resting. See manual for details.
#' @param index index of abundance of interest
#' @param stdat data.frame with data on strata
#' @export

freqMatSummary2 = function(con, code = "ELEDMOS", gsa = 11, stdat, matStage = 3){
	dat = getSpecData(code = code, gsa = gsa)
	tc = getMedits(con, statement = "select * from tc where genus = 'ELED' and species = 'MOS' and area = 11;")
	tc$gsa = tc$area
	haulid = c("country","gsa","year","month","day","vessel","haul_number") 

	tcagg = do.call(data.frame, with(tc, aggregate(list(mat = maturity), tc[, haulid], function(x)c(stage = table(x), ind = length(x))))) # tc aggregated by maturity stage
# the do.call is needed to create a regular dataframe, otherwise the maturity stage columns would be in a matrix form - so I would create a mixed data.frame/matrix
	tcagg$frac.mat = with(tcagg,get(paste("mat.stage.",matStage,sep=""))/mat.ind) # fraction of mature fish in that sample
	dat2 = merge(dat, tcagg, by = haulid, all.x = T)
	
	dat3 = dat2[!is.na(dat2$frac.mat),]
	dat3$index = with(dat3, ind_skm*frac.mat)
	
	dat = dat2

# the following is essentially the catchSummary code.

	meandat = aggregate(list(str_ind_skm = dat[,index]), dat[,c("year","nstrate")], function(x) mean(x, na.rm = T))
 	#vardat = with(dat, aggregate(list(s2h = index), list(year = year, nstrate = nstrate), function(x)var(x, na.rm = T))) # var index
 	vardat = aggregate(list(s2h = dat[,index]), dat[,c("year","nstrate")], function(x)var(x, na.rm = T)) # var index
 	meanvardat = merge(meandat,vardat,by = c("year","nstrate"))
 	towdat = with(dat, aggregate(list(nh = id.y), list(year = year, nstrate = nstrate), length)) # number of tows per strata
 	mvtdat = merge(meanvardat,towdat,by = c("year","nstrate"))
 	z = merge(mvtdat, stdat, by = "nstrate", all.x = TRUE)
	#z$s2h[is.na(z$s2h)]<-0 # having missing variances do not make any difference
	z$Nh = with(z, surface/avg_swept)
	z$N = with(z, totalSurface/avg_swept)
	
	if (gsa!="all") {z = z[z$gsa==gsa,];
					z$N = with(z, surfaceGSA/avg_swept);
					z$W = z$Wgsa
					}
#summary across strata
	x1 = with(z, aggregate(list(tot_index = str_ind_skm*W), list(year = year), function(x)sum(x, na.rm = T)))
	y1 = with(z, aggregate(list(tot_index_var = (1-(nh/Nh))*(Nh/N)^2*(s2h/nh)), list(year = year), function(x)sum(x, na.rm = T)))

	x1y1 = merge(x1, y1, by = "year")
	x1y1$CV = with(x1y1, sqrt(tot_index_var)/tot_index)
	x1y1$upper = with(x1y1, tot_index + sqrt(tot_index_var)*1.96)
	x1y1$lower = with(x1y1, tot_index - sqrt(tot_index_var)*1.96)
	x1y1
}


#' Get Maturity Specific CPUE
#'
#' This function returns the medits cpue in terms of maturity stage
#' @param con connection to the medits database
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @param matStage MEDITS maturity code, from 0 to 4 and ND. 0 undetermined, 1 = immature, 2 maturing, 3 mature, 4 spent resting. See manual for details.
#' @param stdat data.frame with data on strata
#' @export

getFreqData = function(con, code = "ELEDMOS", gsa = 11, stdat, matStage = 3){
	dat = getSpecData(con, code = code, gsa = gsa)
	genus = substr(code,1,4)
	species = substr(code,5,7)
	if (gsa[1]=="all") cond = "" else cond = paste(" and area in (",paste(gsa,collapse=","),")", sep="")
	tc 	= getMedits(con, statement = paste("select * from tc where genus = '",genus,"' and species = '",species,"'",cond,";", sep =""))
	tc$gsa = tc$area
	haulid = c("country","gsa","year","month","day","vessel","haul_number") 

	tcagg = do.call(data.frame, with(tc, aggregate(list(mat = maturity), tc[, haulid], function(x)c(stage = table(x), ind = length(x))))) # tc aggregated by maturity stage
# the do.call is needed to create a regular dataframe, otherwise the maturity stage columns would be in a matrix form - so I would create a mixed data.frame/matrix
	tcagg$frac.mat = with(tcagg,get(paste("mat.stage.",matStage,sep=""))/mat.ind) # fraction of mature fish in that sample
	dat2 = merge(dat, tcagg, by = haulid, all.x = T)
	
	dat3 = dat2[!is.na(dat2$frac.mat),]
	dat3
}