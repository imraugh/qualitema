
standardize_frame <- function(
  frame, center = TRUE, scale = TRUE,
  skip = NULL, choose = NULL,
  scale.factors = FALSE,
  scale.dichotomous = FALSE,
  grouping = NULL
){
  #'
  #' Standardize a dataframe
  #'
  #' Function receives an input dataframe and outputs a standardized version, with some additional helpers on what to standardize and how.
  #'
  #' @param frame Dataframe to standardize values in
  #' @param center Should data be centered? Default is TRUE. Will center on grand mean by default, grouping can be used to specify groups for group-mean centering
  #' @param scale Should data be scaled? Default is TRUE
  #' @param skip Which columns should be skipped? Default is any non-numeric and any column where only two values are observed (e.g., 0, 1)
  #' @param choose Which columns should be scaled? Useful if some numeric variables should be centered while others shouldn't. Complement to skip
  #' @param scale.factors Should factors be converted to numeric and scaled? Default is FALSE
  #' @param scale.dichotomous Should dichotomous variables be scaled? Default is FALSE
  #' @param grouping Should dataframe be grouped prior to scaling? Useful for group-mean or idiographic scalings
  #'
  #' @import tidyr
  #' @import dplyr
  #'
  #' @export
  #'
  #'
  ##### Default behavior:
  #####   Scales variables based on the *grand mean*
  ####
  #####   Scales *all* variables in the dataframe, except:
  #####     - Factors
  #####     - Characters
  #####     - Dates
  #####     - Logical
  #####     - Dichotomous numeric (e.g., 0-1, 1-2, etc.)
  ##### Options:
  #####   - choose = Specify which columns to standardize,
  #####       all other columns and options will be ignored
  #####   - skip = Identify columns that would be standardized
  #####       but should be skipped
  #####   - scale.factors & scale.dichotomous = override default
  #####       behavior to skip these
  #####   - grouping = identify how to scale
  #####       e.g., want to scale by group? grouping = "group",
  #####       want to scale by person? grouping = "group",
  #####       want to scale by trial type? grouping = "trial",
  #####       etc...
  ####
  if(is.null(choose)){
    choose <- colnames(select(frame, where(is.numeric)))
    if(scale.dichotomous == TRUE){
      choose <- c(
        choose,
        colnames(
          select(frame, where(is.numeric)) %>%
            select(where(~ length(levels(as.factor( . ))) == 2))
        )
      )
    } else {
      skip <- c(
        skip,
        colnames(
          select(frame, where(is.numeric)) %>%
            select(where(~ length(levels(as.factor( . ))) == 2))
        )
      )
    }
    if(scale.factors == TRUE){
      choose <- c(
        choose,
        colnames(select(frame, where(is.factor)))
      )
    } else {
      skip <- c(
        skip,
        colnames(select(frame, where(is.factor)))
      )
    }
    choose <- choose[!choose %in% skip]
  }

  frame <- frame %>%
    group_by(across(all_of(grouping))) %>%
    mutate(across(
      all_of(choose),
      ~ as.numeric(scale(
         . , center = center, scale = scale
      ))
    )) %>%
    ungroup()

  return(frame)
}

