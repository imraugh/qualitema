#' Significance labels
#'
#' Converts a vector of *p* values to APA-style significance labels. Default performance is ` ` = *p* > .05, `*` = *p* < .05, `**` = *p* < .01, `***` = *p* < .001.
#'
#' @param vector Vector of *p* values
#' @param trend Should trend daggers (\U2020) be displayed? Default is FALSE
#' @param fourstar Should values < .0001 be labeled with four stars? Default is FALSE
#'
#' @return Vector of labels of same length as input.
#'
#' @examples
#'  p.values <- runif(100, min = 0, max = .5)
#'  stars <- sig_labels(p.values, trend = T)
#'
#' @export
sig_labels = function(vector, trend = FALSE, fourstar = FALSE){
  #### Creating markers vector
  if(trend == TRUE){
    markers <- c("***", "***", "**", "*", "\U2020", "")
  }
  if(fourstar == TRUE){
    markers <- c("****", "***", "**", "*", "", "")
  }
  if(trend == TRUE & fourstar == TRUE){
    markers <- c("****", "***", "**", "*", "\U2020", "")
  }
  #### ***Default performance!!***
  if(trend == FALSE & fourstar == FALSE){
    markers <- c("***", "***", "**", "*", "", "")
  }
  vector <- cut(
    vector,
    # More breaks are made to improve flexibility,
    #   would be more verbose to have different breaks
    #   based on trends and four-star
    breaks = c(-1, .0001, .001, .01, .05, .1, 2),
    labels = markers
  )
  return(vector)
}
