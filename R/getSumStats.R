#' Get Summary Statistics for Index Time Series
#'
#' @param con connection to the MEDITS database
#' @param code species code
#' @param gsa gsa number, vector of gsas (for groups) or "all"
#' @param value logical if TRUE return the data table.
#' @export
getSumStats = function(con, code, gsa, value = F){

#-----------------------------------------------------------------------------------------
# Autocorrelation coefficient on mean abundance (1st order)
#-----------------------------------------------------------------------------------------

#dat1 = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat())
load(paste("../data/CS",code,gsa,"ind_skm.RData", sep = ""))
acf_ind_skm <-acf(dat1$tot_index, plot=F)
acf_ind = acf_ind_skm[1] #autocorrelation coefficient of lag1
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Fraction of years with the mean abundance outside median of mean values +-2CV
#-----------------------------------------------------------------------------------------
#dat1 = catchSummary(con, code = code, index = "ind_skm", gsa = gsa, stdat = getStdat())
dat1 = dat1[dat1$pos_tows>0,]
med = median(dat1$tot_index)
dat1$uppTres = med+(dat1$CV*2*dat1$tot_index)
dat1$lowTres = med-(dat1$CV*2*dat1$tot_index)
dat1$out = with(dat1, ifelse(uppTres<tot_index | tot_index<lowTres, 1, 0))
frac_ind = sum(dat1$out)/nrow(dat1)

#-----------------------------------------------------------------------------------------
# Autocorrelation coefficient on mean catch weight (1st order);
#-----------------------------------------------------------------------------------------
# dat1 = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat())
load(paste("../data/CS",code,gsa,"p_skm.RData", sep = ""))
dat2 = dat2[dat2$tot_index>0,]
acf_p_skm <-acf(dat2$tot_index, plot=F)
acf_p = acf_p_skm[1] #autocorrelation coefficient of lag1
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Fraction of years with the mean biomass outside median of mean values +-2CV
#-----------------------------------------------------------------------------------------

#dat1 = catchSummary(con, code = code, index = "p_skm", gsa = gsa, stdat = getStdat())

med = median(dat2$tot_index)
dat2$uppTres = med+(dat2$CV*2*dat2$tot_index)
dat2$lowTres = med-(dat1$CV*2*dat2$tot_index)
dat2$out = with(dat2, ifelse(uppTres<tot_index | tot_index<lowTres, 1, 0))
frac_p = sum(dat2$out/nrow(dat2)) 

#-----------------------------------------------------------------------------------------
# Autocorrelation coefficient on mean time (1st order); (this is possibly on mean date not sure)
#-----------------------------------------------------------------------------------------
dt1 = tableCatchDateDis(con, code = code, gsa = gsa)
acf_yday = acf(dt1$meanYday, plot=F)
acf_yday = acf_yday[1]
 
 
sumStats = c(acf_ind = as.numeric(unlist(acf_ind)[1]), acf_p = as.numeric(unlist(acf_p)[1]), acf_yday = as.numeric(unlist(acf_yday)[1]), frac_ind = frac_ind, frac_p = frac_p)
save(sumStats, file = paste("../data/SS",code,paste(gsa,collapse="-"),".RData",sep=""))
if (value) sumStats
}