
#'
#' Mode of a vector
#'
#' Returns the mode of a given vector, will convert factors and dates to
#' characters!
#'
#' @param x Vector to return mode of
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
    # If length of an input is 1, returns it Will convert POSIX and factors to
    # characters, to avoid problems with joining differing classes
    if(class(x)[1] %in% c(
      "POSIXct", "POSIXt", "factor")){
      x <- as.character(x)
    }
    return(x)

  } else {

    class <- class(x)[1] #extracts only the first class
    if(length(x[!is.na(x)]) == 0){
      # If (non NA) length == 0, returns NA
      # Converts NA to relevant class, to avoid joining issues
      if(class(x)[1] %in% c(
        "POSIXct", "POSIXt", "factor")){
        output <- as.character(NA)
      } else {
        output <- methods::as(NA, class)
      }
    } else {
      counts <- data.frame(table(x))
      if(max(counts$Freq) == 1){
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

#' Centre variables
#'
#' Function is a wrapper around a `dplyr::mutate` and `dplyr::across`
#' combination which can centre and/or standardize the specified variables.
#' Allows for group specification through `.by = X`.
#'
#' @param frame Dataframe in which variables are found
#' @param choose Which variables should be centered?
#' @param center Center variables? Default is TRUE
#' @param scale Scale variables? Default is FALSE
#' @param .names How should new variables be named, default is to append "_ctr"
#' @param ... Other arguments passed into `dplyr::mutate`, especially useful to
#'   pass `.by`
#'
#' @return Returns same dataframe but with additionally calculated variables.
#'
#' @import tidyr
#' @import dplyr
#'
#' @export
#'
center_vars <- function(
    frame, choose,
    center = TRUE, scale = FALSE,
    ...,
    .names = "{.col}_ctr"
){

  # Requires variables to be specified
  frame %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyr::all_of(choose),
        ~ as.numeric(scale( . , center = center, scale = scale)),
        .names = .names
      ),
      ...)
  #Notes:
  # allow for use of center or centre

}

#' Standardize a dataframe
#'
#' Function receives an input dataframe and outputs a standardized version, with
#' some additional helpers on what to standardize and how.
#'
#' @param frame Dataframe to standardize values in
#' @param center Should data be centered? Default is TRUE. Will center on grand
#'   mean by default, grouping can be used to specify groups for group-mean
#'   centering
#' @param scale Should data be scaled? Default is TRUE
#' @param skip Which columns should be skipped? Default is any non-numeric and
#'   any column where only two values are observed (e.g., 0, 1)
#' @param choose Which columns should be scaled? Useful if some numeric
#'   variables should be centered while others shouldn't. Complement to skip
#' @param scale.factors Should factors be converted to numeric and scaled?
#'   Default is FALSE
#' @param scale.dichotomous Should dichotomous variables be scaled? Default is
#'   FALSE
#' @param grouping Should dataframe be grouped prior to scaling? Useful for
#'   group-mean or idiographic scalings; grouping variables will not be centered
#'   or scaled
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
  ##### Default behaviour:
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
  #####       behaviour to skip these
  #####   - grouping = identify how to scale
  #####       e.g., want to scale by group? grouping = "group",
  #####       want to scale by person? grouping = "group",
  #####       want to scale by trial type? grouping = "trial",
  #####       etc...
  ####
  skip <- c(skip, grouping)
  # will automatically skip any variables used in grouping
  if(is.null(choose)){
    choose <- colnames(
      dplyr::select(frame, dplyr::where(is.numeric))
    )
    if(scale.dichotomous == TRUE){
      choose <- c(
        choose,
        colnames(
          dplyr::select(frame, dplyr::where(is.numeric)) %>%
            dplyr::select(
              where(~ length(levels(as.factor( . ))) == 2)
            )
        )
      )
    } else {
      skip <- c(
        skip,
        colnames(
          dplyr::select(frame, dplyr::where(is.numeric)) %>%
            dplyr::select(
              dplyr::where(~ length(levels(as.factor( . ))) == 2)
            )
        )
      )
    }
    if(scale.factors == TRUE){
      choose <- c(
        choose,
        colnames(dplyr::select(frame, dplyr::where(is.factor)))
      )
    } else {
      skip <- c(
        skip,
        colnames(dplyr::select(frame, dplyr::where(is.factor)))
      )
    }
    choose <- choose[!choose %in% skip]
  }

  frame <- frame %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(grouping))) %>%
    dplyr::mutate(
      dplyr::across(
        tidyr::all_of(choose),
        ~ as.numeric(scale(
          . , center = center, scale = scale
        ))
      )) %>%
    ungroup()

  return(frame)
}


#' Standardize model
#'
#' Designed as a shortcut for standardizing (z-scoring) the data used for a
#' given model. The `parameters` package allows for standardized parameters,
#' this is designed to work with other functions that work on a model rather
#' than the parameters output, such as `emmeans`.
#'
#' @param model Model to standardize data for
#' @param ... Arguments to pass to `standardize_frame`
#'
#' @return Model with the same specifications and formula but standardized
#'   continuous variables
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


##### Unicode values #####
uni_beta <- "\U1D6FD"


uni_z <- "\U1D467"


uni_t <- "\U1D461"


uni_f <- "\U1D439"


uni_p <- "\U1D45D"


