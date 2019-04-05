#' Create Connection to MEDITS Database on localhost. 
#'
#' Create Connection to MEDITS Database on localhost. 
#' @param dbuser role name for database
#' @export
connectMedits = function(dbuser = "postgres"){
  require(RPostgreSQL)
  require(RH2) 	
  	dbname = "medits"
  	dbhost <- "localhost"
  	dbport <- 5432
  	drv <- dbDriver("PostgreSQL") 
  	con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname,  user=dbuser) 
}