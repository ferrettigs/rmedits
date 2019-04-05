#' Get Standardized CPUE Through GLM Fit
#'
#' @param con connection to the medits database
#' @param code species code
#' @param index index of abundance of interest. It can be ind_skm or p_skm for biomass
#' @param stdat data.frame with data on strata
#' @param gsa gsa number either a number, a vector of numbers or "all".
#' @return a data.frame with predicted standardized CPUES. For The moment only the model for individuals is implemented as the biomass model should use a delta-LN or delta-Gamma model. This should be implemented shortly. 
#' @export
getGLMfit = function(con, code = "ELEDMOS", index = "ind_skm", stdat, gsa = "all"){
	
	require(MASS)
	dat = getSpecData(con, code = code, gsa = gsa)
	dat$yearF = factor(dat$year)	
	if(index=="ind_skm"){
		
		 m1f = try(glm.nb(nbtot~yearF+mdepth+s_lat+s_lon+offset(log(swept)), data = dat))
		 if(class(m1f)=="try-error") {
		 
		 	m1f = 	glm(nbtot~yearF+mdepth+s_lat+s_lon+offset(log(swept)), data = dat, family = poisson); # factorial model 
		 	m1c = glm(nbtot~year+mdepth+s_lat+s_lon+offset(log(swept)), data = dat, family = poisson) } else m1c = glm.nb(nbtot~year+mdepth+s_lat+s_lon+offset(log(swept)), data = dat)
		 
		 # continuous model
		 
		 # create newdata
		 newdat = data.frame(year = min(dat$year):max(dat$year), yearF = factor(min(dat$year):max(dat$year)), mdepth = mean(dat$mdepth), swept = 1, s_lat = mean(dat$s_lat), s_lon = mean(dat$s_lon))
		 
		 predsF = predict(m1f, se.fit = TRUE, newdata = newdat)
		 newdat$predF = exp(predsF$fit)
		 newdat$upperF = exp(predsF$fit+1.96*predsF$se.fit)
		 newdat$lowerF = exp(predsF$fit+1.96*predsF$se.fit)
		  
		 predsC = predict(m1c, se.fit = TRUE, newdata = newdat)
		 newdat$predC = exp(predsC$fit)
		 newdat$upperC = exp(predsC$fit+1.96*predsC$se.fit)
		 newdat$lowerC = exp(predsC$fit+1.96*predsC$se.fit)
		 newdat
		 
		 } else {
		 
		 # probably I woule need to make sure that ptot has all the zeros - in getSpecDat
		 # neet to subset for positive value too
		 formF = as.formula(ptot~yearF+mdepth+s_lat+s_lon+offset(log(swept)))
		 formC = as.formula(ptot~year+mdepth+s_lat+s_lon+offset(log(swept)))
		 m1f = deltaLN(formF, binary.form = formF, data = dat) # factorial model 
		 m1c = deltaLN(formC, binary.form = formC, data = dat) 
		 # continuous model
		 
		 # create newdata
		 newdat = data.frame(year = min(dat$year):max(dat$year), yearF = factor(min(dat$year):max(dat$year)), mdepth = mean(dat$mdepth), swept = 1, s_lat = mean(dat$s_lat), s_lon = mean(dat$s_lon), nz.offset = log(1))
		 
		 predsF = predict(m1f$lnMod, se.fit = TRUE, newdata = newdat)
		 newdat$predF = exp(predsF$fit)
		 newdat$upperF = exp(predsF$fit+1.96*predsF$se.fit)
		 newdat$lowerF = exp(predsF$fit+1.96*predsF$se.fit)
		  
		 predsC = predict(m1c, se.fit = TRUE, newdata = newdat)
		 newdat$predC = exp(predsC$fit)
		 newdat$upperC = exp(predsC$fit+1.96*predsC$se.fit)
		 newdat$lowerC = exp(predsC$fit+1.96*predsC$se.fit)
		 newdat
		 }
}		 