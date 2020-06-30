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

  ret_vec <- rep("", length(var))

  for (i in 1:length(var)) {
    if (is.na(var[i])) {
      ret_vec[i] <- "NULL"
    } else if (is.character(var[i])) {
      ret_vec[i] <- paste("'", gsub("'", "\\\\'", var[i]), "'", sep = "")
    } else if (is.numeric(var[i])) {
      ret_vec[i] <- as.character(var[i])
    } else if (is.double(var[i])) {
      ret_vec[i] <- paste("'", as.character(var[i]), "'", sep = "")
    } else {
      ret_vec[i] <- var[i]
    }
  }

  return(ret_vec)
}
