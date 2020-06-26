#' Safe SQL String
#'
#' @param var String to be made safe
#'
#' @return Safe sql string
#' @export
#'
#' @examples
#' safeSQLVar("unsafe' string")
safeSQLVar <- function(var) {

  if (is.na(var)) {
    return("NULL")
  }else if (is.character(var)) {
    return(paste("'", gsub("'", "\\\\'", var), "'", sep = ""))
  }else if (is.numeric(var)) {
    return(as.character(var))
  }else if (is.double(var)) {
    return(paste("'", as.character(var), "'", sep = ""))
  }else {
    return(var)
  }
}
