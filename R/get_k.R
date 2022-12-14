#' Get K
#'
#' Function to receive a dataframe and output the n for a grouping variable and k for each observation within that group. E.g., for EMA data where you have k observations per n individuals in multiple groups.
#'
#' @param frame Dataframe to provide n and k for
#' @param n.var Variable identifying grouping for n, can be left blank
#' @param k.var Variable identifying level of k, e.g., id
#' @param digits Digits for rounding, default is 2
#'
#' @import tidyr
#' @import dplyr
#'
#' @export
#'
#'
get_k <- function(
    frame, n.var = NULL, k.var,
    digits = 2
){
  output <- frame %>%
    group_by(across(
      c(all_of(k.var),
        all_of(n.var))
    )) %>%
    dplyr::summarize(obs = n(), .groups = "drop") %>%
    group_by(across(
      all_of(n.var)
    )) %>%
    dplyr::summarize(
      n = n(),
      k = sum(obs),
      k_mean = mean(obs),
      k_sd = sd(obs),
      .groups = "drop"
    ) %>%
    mutate(across(
      where(is.numeric),
      round, digits
    ))
  return(output)
}
