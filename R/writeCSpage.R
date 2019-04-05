#' Write Catch Summary Page
#'
#' This function writes the catch summary page for each GSA. It writes in the \code{tex/} folder. It generates a Rnw file named \code{catchSummaryPage[code][gsa].Rnw}.
#' @param code species code
#' @param gsa number, vector of numbers (for gsa groups), or "all" for the entire survey domain.
#' @param sciname scientific name of the species.
#' @export
writeCSpage = function(code, gsa, sciname){
		x <- c(
		paste("\\subsubsection*{",code," (\\emph{",scinames[i],"})}",sep=""),
		  "",
		"<<echo=false>>=",
		paste("load('../data/CS",code,paste(gsa,collapse="-"),"ind_skm.RData')", sep = ""),
		"fracPosTows = with(dat1, sum(pos_tows)/sum(all_tows))",
		"@",
		"",
		  paste("\\emph{",sciname,"} has been taken in \\Sexpr{round(fracPosTows,2)*100}\\% of the tows, mainly around \\Sexpr{round(mean(dat1$meanDepth, na.rm =T))} meters, between \\Sexpr{min(dat1$minDepth, na.rm =T)} and \\Sexpr{max(dat1$maxDepth, na.rm = T)} meters. Average index of abundance across years was \\Sexpr{round(mean(dat1$tot_index, na.rm = T),2)} with an average coefficient of variation of \\Sexpr{round(mean(dat1$CV, na.rm = T),2)}.", sep = ""),
		"",

		"<<echo=false>>=",
		paste("load('../data/SS",code,paste(gsa,collapse="-"),".RData')", sep = ""),
		"@",
		"",
		"The annual CPUE time series had an autocorrelation coefficient (acf) of \\Sexpr{round(sumStats['acf_ind'],2)} for $ind./km2$ and \\Sexpr{round(sumStats['acf_p'],2)} for $kg/km^2$. The acf for mean catch time was \\Sexpr{round(sumStats['acf_yday'],2)}. The Fraction of years with mean abundance outide $\\pm 2\\sigma$ from the median is \\Sexpr{round(sumStats['frac_ind'],2)} for $ind/km^2$ and \\Sexpr{round(sumStats['frac_p'],2)} for $kg/km^2$.", 
		"",
		"",
		"\\begin{figure}[ht!]",
paste("\\includegraphics[width=1\\textwidth]{../maps/specSummary6Fig",code,paste(gsa,collapse="-"),"}",sep=""),
		"\\caption{Catch Summary with indices of abundance in number (A) and kg per square kilometer (B), and for maturity stage 3 (C and D); Annual length distribution of catches (E); Depth distribution of all hauls performed (white) and those were the species was caught (black) (F); Date distribution of hauls where the species was caught (G); and geographic distribution of all hauls performed (blue) and those were the species was caught (red).}",
		paste("\\label{specSummary",code,paste(gsa,collapse="-"),"}",sep=""),
		"\\end{figure}",
		"\\clearpage")
  
		sink(file = paste("../tex/catchSummaryPage",code,paste(gsa,collapse="-"),".Rnw", sep=""))
			cat(x, sep = "\n")
		sink()
}		  


#' Write SweaveInputs for CS Pages
#'
#' This functions list the species summary pages as SweaveInputs in a Rnw file called \code{gsa[n]summaries.Rnw} placed into the \code{tex} folder.
#' @param gsa number, vector of numbers (for gsa groups), or "all" for the entire survey domain.
#' @return Include only the species summaries that have correspondent 6-panel figures. This is a good check to identify data deficient species.
#' @export
writeSI = function(gsa){
    rl = read.csv("../data/referenceList.csv", as.is = TRUE)
	#system(paste("ls ../data/ | egrep 'CS[A-Z]+",gsa,"[a-z]\\_' > ../data/gsa",gsa,"data.txt", sep=""))
	system(paste("ls ../maps/ | egrep 'specSummary6Fig[A-Z]+",paste(gsa,collapse="-"),".pdf' > ../data/gsa",paste(gsa,collapse="-"),"data.txt", sep=""))
	gsa1 = read.table(paste("../data/gsa",paste(gsa,collapse="-"),"data.txt",sep=""))
	gsa1$CODE = substr(gsa1$V1,16,22)
	d1 = merge(rl, gsa1, by = "CODE")
	d1$file = paste("\\SweaveInput{../tex/catchSummaryPage",d1$CODE,paste(gsa,collapse="-"),".Rnw}",sep = "")
	write(d1$file, file = paste("../tex/gsa",paste(gsa,collapse="-"),"summaries.Rnw",sep=""))
}	


#' Write GSA summary Page
#'
#' This function writes a GSA summary page that precedes the species catch summaries for that GSA. It writes into the \code{tex} folder.
#' @param gsa number, vector of numbers (for gsa groups), or "all" for the entire survey domain.
#' @export
writeGSApage = function(gsa){ 
	x <- c(
	"<<echo=false>>=",
	"stdat=getStdat()",
	paste("yr = range((ta$year[ta$gsa %in% c(",paste(gsa,collapse = ","),")]))", sep = ""), # year range
	paste("ntows = nrow((ta[ta$gsa %in% c(",paste(gsa,collapse = ","),"),]))", sep = ""),
	paste("subareas = stdat$area[stdat$gsa %in% c(",paste(gsa,collapse = ","),")]",sep = ""),
	"areas = unique(gsub('South |North |East |West ','',subareas))",
	paste("dataRichSpec = as.numeric(system(paste(\"ls ../maps/ | egrep 'specSummary6Fig[A-Z]+\",",paste(gsa,collapse="-"),",\".pdf'  | wc -l\",sep=\"\"), intern=TRUE))", sep = ""), # this line has to be modified because it does not work with gsa groups
	"@",
	"",
	paste("\\subsubsection{GSA ",paste(gsa,collapse="-")," (\\Sexpr{areas})}", sep = ""),
	"",
	"Between \\Sexpr{yr[1]} and \\Sexpr{yr[2]} there were a total of \\Sexpr{ntows} tows carried out in \\Sexpr{areas}. All summary statistics could be estimated for \\Sexpr{dataRichSpec} species.",
	"",
	paste("\\input{notesGSA",paste(gsa,collapse="-"),".tex}",sep=""),
	"",
	"\\begin{figure}[ht!]",
	paste("\\includegraphics[width=1\\textwidth]{../maps/all_meditsArea_",paste(gsa,collapse="-"),"}",sep = ""),
	paste("\\caption{Tows carried out in GSA ",paste(gsa,collapse="-"),"}",sep = ""),
	paste("\\label{gsa",paste(gsa,collapse="-"),"Tows}",sep = ""),
	"\\end{figure}",
	"",
	"\\clearpage")

	sink(file = paste("../tex/GSASummaryPage",paste(gsa,collapse="-"),".Rnw", sep=""))
		cat(x, sep = "\n")
	sink()

}

	