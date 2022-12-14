
#' Descriptives for a given term in a model
#'
#' Helper function for model_desc which is looped
#'
#' @import tidyr
#' @import dplyr
#' @import stringr
#'

describe_term <- function(
  term, data, y_term, digits = 2
){

  # Identify numeric columns in data (for correlations)
  numeric = data %>%
    select(where(is.numeric) & all_of(term)) %>%
    colnames()

  # Identify string columns in data (for means)
  string = data %>%
    select(
      all_of(term) & where(is.character),
      all_of(term) & where(is.factor)
    ) %>%
    colnames()

  means <- data %>%
    select(all_of(y_term), all_of(numeric), all_of(string)) %>%
    group_by(across(all_of(string))) %>%
    summarize(across(
      where(is.numeric),
      list(mean = ~ mean( . , na.rm = T),
           sd = ~ sd( . , na.rm = T))
    ),
    .groups = "drop") %>%
    mutate(across(where(is.numeric), round, digits))

  if(length(numeric) > 0){
    cors <- data %>%
      rename(y_term = paste(y_term, sep = "")) %>%
      select(y_term, all_of(numeric), all_of(string)) %>%
      group_by(across(all_of(string))) %>%
      summarize(across(
        .cols = all_of(numeric),
        .fns = list(
          y_r = ~ cor.test(y = y_term, x = . )$estimate
        )
      ),
      .groups = "drop") %>%
      mutate(across(where(is.numeric), round, digits))

    means <- left_join(means, cors, by = all_of(string))

  }

  return(means)

}



#' Describe model
#'
#' Uses a formula interface to provide descriptive statistics per model term. Provides mean of outcome, sd of outcome, Pearson's r for continuous predictors with outcome; all based on categorical variables. Formula interface is for ability to translate model specifications into descriptives, future functionality will add ability to specify a model instead.
#'
#' @param formula Formula specification in the form of y ~ x. Note that lme4 syntax (y ~ x + (1 | j)) and correlation format ( . ~ x + y) are not supported
#' @param data Dataframe to reference
#' @param model Model object to provide descriptives for, will determine formula and data from the provided model object and flag if model type is not currently supported
#' @param digits Rounding digit, default is 2
#'
#' @return List of dataframes with descriptives by each term.
#'
#' @import tidyr
#' @import dplyr
#' @import stringr
#'
#'
#' @export
#'
#'
describe_model <- function(
  formula,
  data,
  model = NULL,
  digits = 2
){

  ##### In development #####
  # # Builds in flexibility to pull formula and data from a model
  # if(!is.null(model)){
  #
  #   # Supported models
  #   if(!any(class(model) %in% c(
  #     "lm", "lme", "glm", "lmerTest", "lme4",
  #     "lmerModLmerTest", "glmerMod", "glmmTMB"
  #   ))){
  #     print(class(model))
  #     stop("Model type not supported! \n")
  #   }
  #
  #   # Non-gaussian GLMs may not be appropriate for the stats provided,
  #   #   this provides a disclaimer about that
  #   if(class(model) %in% c("glm", "glmmTMB")){
  #     if(!as.character(model$call["family"]) == "gaussian"){
  #       print(class(model))
  #       warning("Function not developed for GLMs \n",
  #               "Results may not be appropriate \n")
  #     }
  #   }
  #
  #   # glmerMod is class 4, so requires a slightly different call
  #   if(class(model) %in% c("glmerMod")){
  #     if(!as.character(model@call["family"]) == "gaussian"){
  #       print(class(model))
  #       warning("Function not developed for GLMs \n",
  #               "Results may not be appropriate \n")
  #     }
  #   }
  #
  #   # uses specific structure of objects to extract data and formula
  #   if(any(class(model) %in% c("lm", "glm"))){
  #     data <- model$model
  #     formula <- model$call$formula
  #   }
  #
  #   if(any(class(model) %in% c("lme"))){
  #     data <- model$model
  #     formula <- model$call$formula
  #   }
  #
  #   if(any(class(model) %in% c("lmerTest", "lmerModLmerTest",
  #                              "lme4", "glmerMod"))){
  #     data <- model@frame
  #     formula <- model@call$formula
  #   }
  #
  #   if(any(class(model) %in% c("glmmTMB"))){
  #     data <- model$model
  #     formula <- model$call$formula
  #   }
  #
  # }

  ##### Live #####

  x_terms <- attr(terms(as.formula(formula)), "term.labels")
  y_term <- unlist(str_split(deparse(formula), pattern = " "))[1]

  output <- lapply(
    x_terms, function(x){
      terms <- unlist(str_split(x, pattern = ":"))
      output <- describe_term(
        terms,
        data,
        y_term,
        digits = digits
      )
    }
  )

  names(output) <- x_terms

  return(output)

}


