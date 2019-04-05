#' Summary table for a focal Species and GSA
#'
#' @param con connection to the MEDITS database.
#' @param code species code
#' @param gsa gsa number or a vector of numbers (e.g. c(1,5,6)), or "all".
#' @param dat specific data with catches
#' @param index index of abundance of interest. It can be ind_skm or p_skm for biomass
#' @param stdat data.frame with data on strata
#' @param matFrac whether to correct the index for the fraction of fish having a certain maturation stage.
#' @param matStage MEDITS maturity code, from 0 to 4 and ND. 0 undetermined, 1 = immature, 2 maturing, 3 mature, 4 spent resting. See manual for details.
#' @export
catchSummary = function(con, code = "ELEDMOS", index = "ind_skm", stdat, gsa = "all", matFrac = FALSE, matStage = 3){

	if (matFrac) { dat = getFreqData(con, code = code, gsa = gsa, matStage = matStage)
				   dat[,index] = dat[,index]*dat[,"frac.mat"] 
				} else dat = getSpecData(con, code = code, gsa = gsa)

	meandat = aggregate(list(str_ind_skm = dat[,index]), dat[,c("year","nstrate")], function(x) mean(x, na.rm = T))

 	vardat = aggregate(list(s2h = dat[,index]), dat[,c("year","nstrate")], function(x)var(x, na.rm = T)) # var index
 	meanvardat = merge(meandat,vardat,by = c("year","nstrate"))
 	towdat = with(dat, aggregate(list(nh = id.y), list(year = year, nstrate = nstrate), length)) # number of tows per strata
 	mvtdat = merge(meanvardat,towdat,by = c("year","nstrate"))
 	z = merge(mvtdat, stdat, by = "nstrate", all.x = TRUE)
	
	z$Nh = with(z, surface/avg_swept)
	z$N = with(z, totalSurface/avg_swept)
	
	if (gsa[1]!="all") {z = z[z$gsa %in% gsa,];
					if(length(unique(z$gsa))>=1) z$surfaceGSA = sum(unique(z$surfaceGSA));
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