#'
#' Mode of a vector
#'
#' Returns the mode of a given vector, will convert factors and dates to characters!
#'
#' @param x Vector to return mean of
#'
#'
#' @examples
#'
#' x <- round(rnorm(100), 0)
#' x <- rnorm(100)
#'
#' @export
#'



mode <- function(x){
  if(length(x) == 1){
    if(class(x)[1] %in% c(
      "POSIXct", "POSIXt", "factor")){
      x <- as.character(x)
    }
    return(x)
  } else {
    class <- class(x)[1]
    if(length(x[!is.na(x)]) == 0){
      if(class(x)[1] %in% c(
        "POSIXct", "POSIXt", "factor")){
        output <- as.character(NA)
      } else {
        output <- as(NA, class)
      }
    } else {
      counts <- data.frame(table(x))
      if(max(counts$Freq == 1)){
        stop("All values are unique \n")
      }
      output <- as.character(
        counts[counts$Freq == max(counts$Freq), "x"])

      if(class %in% c(
        "POSIXct", "POSIXt", "factor")){
        x <- as.character(x)
        output <- as.character(output[1])
      } else {
        output <- as(output[1], class)
        if(!class(output) == class){
          warning("Classes do not match \n")
        }
      }
      output <- x[x == output][1]
    }

    return(output)
  }
}
