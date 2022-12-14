

#' Center variables
#'
#' Function is a wrapper around a group_by and mutate combination which
#' can center and/or standardize the specified variables
#'
#' @param frame Dataframe in which variables are found
#' @param choose Which variables should be centered?
#' @param center Center variables? Default is TRUE
#' @param scale Scale variables? Default is FALSE
#' @param by How should variables be grouped? If grand mean centering is desired, leave blank. Otherwise, specify cluster variable(s)
#' @param .names How should new variables be named, default is to append "_ctr"
#'
#' @return Returns same dataframe but with additionally calculated variables.
#'
#' @import tidyr
#' @import dplyr
#'
#' @export
#'
#'
center_vars <- function(
    frame, choose,
    center = TRUE, scale = FALSE,
    by = NULL, .names = "{.col}_ctr"
){

  # Requires variables to be specified
  frame %>%
    group_by(across(all_of(by))) %>%
    mutate(across(
      .cols = all_of(choose),
      ~ as.numeric(scale( . , center = center, scale = scale)),
      .names = .names
    ))

}


#' Standardize model
#'
#' Designed as a shortcut for standardizing (z-scoring) the data used for a given model. The parameters package allows for standardized parameters, this is designed to work with other functions that work on a model rather than the parameters output, such as emmeans.
#'
#' @param model Model to standardize data for
#' @param ... Arguments to pass to standardize_frame
#'
#' @return Model with the same specifications and formula but standardized continuous variables
#' @export
#'
#'
standardize_model <- function(
    model, ...
){
  # Extract data
  data <- switch(
    class(model),
    "lm" = model$model,
    "glm" = model$model,
    "lme" = model$data,
    "lmerTest" = model@frame,
    "lmerModLmerTest" = model@frame,
    "lme4" = model@frame,
    "lmerMod" = model@frame,
    "glmerMod" = model@frame,
    "glmmTMB" = model$frame,
    warning(class(model), " not supported! \n")
  )

  standardized_data <- data %>%
    standardize_frame( ... )

  output <- update(
    model,
    data = standardized_data
  )

  return(output)

}


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
standardize_frame <- function(
  frame, center = TRUE, scale = TRUE,
  skip = NULL, choose = NULL,
  scale.factors = FALSE,
  scale.dichotomous = FALSE,
  grouping = NULL
){
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

