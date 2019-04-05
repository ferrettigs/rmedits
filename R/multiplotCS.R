#' Multipanel Plot for Catch Summary Page
#'
#' It produces the multipanel figure for the catch summary page as a pdf file.
#' @param con connection to database
#' @param code species code
#' @param gsa number, vector of numbers (for groups) or "all"
#' @export
multiplotCS = function(con, code, gsa){		
		
		dat = try(getSpecData(con, gsa = gsa, code = code))
		if(class(dat)=="try-error"){write(paste("there are no data for gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("there are no data for gsa ",gsa, sep=""))}
		if(sum(dat$nbtot)==0 & sum(dat$ptot)==0){write(paste("no catches for ",code," in gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("no catches for ",code," in gsa ",gsa, sep=""))}
		# create dat1
		allTows = with(dat,aggregate(list(ntows = id.y),list(year = year), length))
		posTows = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(ptows = id.y),list(year = year), length))
		sumStats = merge(allTows, posTows, by = "year", all.x = T) # there might be years without catches so I need to create zeros in these years
		sumStats$ptows = with(sumStats,ifelse(is.na(ptows), 0, ptows))
		
		meanDepth = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(meanDepth = mdepth),list(year = year), mean))
		sumStats = merge(sumStats, meanDepth, by = "year", all.x = T) # I leave meanDepth missing for years without catches
	
		depthRange = do.call(data.frame, with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(depthRange = mdepth),list(year = year), range)))
		sumStats = merge(sumStats, depthRange, by = "year", all.x = T)
		
		names(sumStats) = c("year","all_tows","pos_tows","meanDepth","minDepth","maxDepth")

		dat1 = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat())
		dat1 = merge(dat1, sumStats, by = "year")
		save(dat1, file = paste("../data/CS",code,paste(gsa,collapse="-"),"ind_skm",".RData",sep=""))
	
		# create dat2 - biomass
		dat2 = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat())
		save(dat2, file = paste("../data/CS",code,paste(gsa,collapse="-"),"p_skm",".RData",sep=""))
	
		pdf(paste("../maps/specSummary",code,paste(gsa,collapse="-"),".pdf",sep=""), width = 12, height = 7)
		
		par(mfrow =c(2,2), mar = c(4,4,1,1))
		if (gsa[1]=="all"){cex=0.5; toadd = 1} else {cex = par()$cex; toadd=0.5} # these are map parameters - toadd is the number of degrees to add to the map limits. If the map is the entire med, I add one degree, 0.5 otherwise.
		xlim = range(dat$s_lon)+c(-toadd,+toadd) # geographic limits for map of tows and a degree of extra space both directions
		ylim = range(dat$s_lat)+c(-toadd,+toadd)
		
		plotCatchSummary(dat = dat1, ylab = "#/km2")
		mtext("A", 3, adj = 0)
		plotCatchSummary(dat = dat2, ylab = "kg/km2")
		mtext("B", 3, adj = 0)
		plotCatchDateDis(con = con, code = code, gsa = gsa) # date distribution
		mtext("C", 3, adj = 0)

		map("worldHires", xlim = xlim, ylim = ylim, fill = T, mar=c(4,4,0,0))
		points(s_lat~s_lon, dat, pch=4, col = "blue", cex = cex)
		points(s_lat~s_lon, dat[dat$ind_skm>0 | dat$p_skm>0,], pch=16, col = "red", cex = cex)
		mtext("D", 3, adj = 0)
		dev.off()
}		



#' Multipanel Plot for Size and Maturity Stats
#'
#' It produces the multipanel figure for the size and maturity summary statistics.
#' @param con connection to database
#' @param code species code
#' @param gsa number, vector of numbers (for groups) or "all"
#' @export
multiplotSM = function(con, code, gsa){		
		
		dat = try(getSpecData(con, gsa = gsa, code = code))
		if(class(dat)=="try-error"){write(paste("there are no data for gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("there are no data for gsa ",gsa, sep=""))}
		if(sum(dat$nbtot)==0 & sum(dat$ptot)==0){write(paste("no catches for ",code," in gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("no catches for ",code," in gsa ",gsa, sep=""))}
		# create dat1
		allTows = with(dat,aggregate(list(ntows = id.y),list(year = year), length))
		posTows = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(ptows = id.y),list(year = year), length))
		sumStats = merge(allTows, posTows, by = "year", all.x = T) # there might be years without catches so I need to create zeros in these years
		sumStats$ptows = with(sumStats,ifelse(is.na(ptows), 0, ptows))
		
		meanDepth = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(meanDepth = mdepth),list(year = year), mean))
		sumStats = merge(sumStats, meanDepth, by = "year", all.x = T) # I leave meanDepth missing for years without catches
	
		depthRange = do.call(data.frame, with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(depthRange = mdepth),list(year = year), range)))
		sumStats = merge(sumStats, depthRange, by = "year", all.x = T)
		
		names(sumStats) = c("year","all_tows","pos_tows","meanDepth","minDepth","maxDepth")

		dat1 = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat(), matFrac=TRUE, matStage = 3)
		dat1 = merge(dat1, sumStats, by = "year")
		#	save(dat1, file = paste("../data/CS",code,paste(gsa,collapse="-"),"ind_skm",".RData",sep=""))
	
		# create dat2 - biomass
		dat2 = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat(), matFrac=TRUE, matStage = 3)
		#	save(dat2, file = paste("../data/CS",code,paste(gsa,collapse="-"),"p_skm",".RData",sep=""))
	
		pdf(paste("../maps/sizeMatSummary",code,paste(gsa,collapse="-"),".pdf",sep=""), width = 12, height = 7)
		par(mfrow =c(2,2), mar = c(4,4,1,1))
		plotCatchSummary(dat = dat1, ylab = "#/km2")
 		mtext("A", 3, adj = 0)
 		plotCatchSummary(dat = dat2, ylab = "kg/km2")
 		mtext("B", 3, adj = 0)
 		plotLenDis(con, code = code, gsa = gsa) # date distribution
 		mtext("C", 3, adj = 0)
 		# density plot of tows
 		d <- density(dat$mdepth) # returns the density data
 		d2<- density(dat$mdepth[dat$ind_skm>0])
 		plot(d, ylim = range(c(d$y, d2$y)), xlab = "Depth (m)", main = "Haul distribution")
 		polygon(d2, col = "black")
		mtext("D", 3, adj = 0)
		dev.off()
}		



#' Multipanel Plot for Catch Summary Page
#'
#' It produces the multipanel figure for the catch summary page as a pdf file.
#' @return A six panel figure with 
#' @param con connection to database
#' @param code species code
#' @param gsa number, vector of numbers (for groups) or "all"
#' @export
multiplotCStogether = function(con, code, gsa){	


	dat = try(getSpecData(con, gsa = gsa, code = code))
		if(class(dat)=="try-error"){write(paste("there are no data for gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("there are no data for gsa ",gsa, sep=""))}
		if(sum(dat$nbtot)==0 & sum(dat$ptot)==0){write(paste("no catches for ",code," in gsa ",gsa, sep=""), file = "../data/catchSummaryLog.text", append = T); stop(paste("no catches for ",code," in gsa ",gsa, sep=""))}
		# create dat1
		allTows = with(dat,aggregate(list(ntows = id.y),list(year = year), length))
		posTows = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(ptows = id.y),list(year = year), length))
		sumStats = merge(allTows, posTows, by = "year", all.x = T) # there might be years without catches so I need to create zeros in these years
		sumStats$ptows = with(sumStats,ifelse(is.na(ptows), 0, ptows))
		
		meanDepth = with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(meanDepth = mdepth),list(year = year), mean))
		sumStats = merge(sumStats, meanDepth, by = "year", all.x = T) # I leave meanDepth missing for years without catches
	
		depthRange = do.call(data.frame, with(dat[dat$nbtot>0 | dat$ptot>0,],aggregate(list(depthRange = mdepth),list(year = year), range)))
		sumStats = merge(sumStats, depthRange, by = "year", all.x = T)
		
		names(sumStats) = c("year","all_tows","pos_tows","meanDepth","minDepth","maxDepth")
		
		# canch ind.
		dat1 = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat())
		dat1 = merge(dat1, sumStats, by = "year")
		save(dat1, file = paste("../data/CS",code,paste(gsa,collapse="-"),"ind_skm",".RData",sep=""))
	
		# create dat2 - biomass
		dat2 = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat())
		save(dat2, file = paste("../data/CS",code,paste(gsa,collapse="-"),"p_skm",".RData",sep=""))
	
		# catch mat 3
		dat1M = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat(), matFrac=TRUE, matStage = 3)
		
		# catch mat 3 biomass
			# create dat2 - biomass
		dat2M = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat(), matFrac=TRUE, matStage = 3)
	
		pdf(paste("../maps/specSummary6Fig",code,paste(gsa,collapse="-"),".pdf",sep=""))
		par(mfrow =c(4,2), mar = c(4,4,1,1))
		if (gsa[1]=="all"){cex=0.5; toadd = 1} else {cex = par()$cex; toadd=0.5} # these are map parameters - toadd is the number of degrees to add to the map limits. If the map is the entire med, I add one degree, 0.5 otherwise.
		xlim = range(dat$s_lon)+c(-toadd,+toadd) # geographic limits for map of tows and a degree of extra space both directions
		ylim = range(dat$s_lat)+c(-toadd,+toadd)
		
		plotCatchSummary(dat = dat1, ylab = "#/km2")
		mtext("A", 3, adj = 0)
		plotCatchSummary(dat = dat2, ylab = "kg/km2")
		mtext("B", 3, adj = 0)
		plotCatchSummary(dat = dat1M, ylab = "#/km2")
 		mtext("C", 3, adj = 0)
 		plotCatchSummary(dat = dat2M, ylab = "kg/km2")
 		mtext("D", 3, adj = 0)

		# length distribution
		plotLenDis(con, code = code, gsa = gsa) # date distribution
 		mtext("E", 3, adj = 0)
 		
 		# depth distribution of tows
 		plotDepthDis(dat)
		mtext("F", 3, adj = 0)
		# date distribution
		plotCatchDateDis(con = con, code = code, gsa = gsa) # date distribution
		mtext("G", 3, adj = 0)

		map("worldHires", xlim = xlim, ylim = ylim, fill = T, mar=c(4,4,0,0))
		points(s_lat~s_lon, dat, pch=4, col = "blue", cex = cex)
		points(s_lat~s_lon, dat[dat$ind_skm>0 | dat$p_skm>0,], pch=16, col = "red", cex = cex)
		mtext("H", 3, adj = 0)
		dev.off()
}		






