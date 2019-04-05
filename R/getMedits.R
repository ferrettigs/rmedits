#' Get MEDITS data
#'
#' @param con connection to database
#' @param statement sql statement 
#' @export
getMedits = function(con, statement){
	dat = fetch(dbSendQuery(con, statement), n = -1)
 }
 