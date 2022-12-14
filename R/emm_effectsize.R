

#' Odds ratio using emmeans
#'
#' Convenience wrapper around emmeans for calculating odds ratios. This functionality is well covered by emmeans already, all this adds is the ability to specify confidence intervals for the odds ratio. Also rounds and can output a dataframe, for convenience. Printed output rounds to specified digits, frame output does not. Note that this function requires contrasts, it will fail otherwise
#'
#' @param model Model to identify emmeans, supports any model supported by emmeans
#' @param formula emmeans formula with the format of contrast ~ x | w
#' @param level Specifies level for confidence intervals, default is 95%
#' @param return How to return output, default is to print
#' @param digits Digits to round to, default is 4
#' @param ... Additional arguments to pass to emmeans, including type = "response" to get odds ratios. Behavior may be unexpected with certain other arguments, such as at, or if a different type is specified
#'
#'
#' @import tidyr
#' @import dplyr
#' @import emmeans
#'
#'
#' @export
emm_oddratio = function(
  model, formula, level = .95, standardize,
  return = c("print", "frame"), digits = 4,
  ...
){
  if(!missing(standardize)){
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

    model <- update(
      model,
      data = standardize_frame(
        data, ...
      )
    )

  }

  #
  # This is significantly different from emm_cohend because emmeans
  #   provides the SE for the odds ratio, so all we need to do
  #   is use that to calculate the CI for the confidence level
  input <- emmeans(
    model, specs = formula,
    level = level, type = "response",
    ...
  )

  contrasts <- data.frame(input$contrasts) %>%
    mutate(
      OR.lcl = odds.ratio - (qnorm(1 - ((1 - level) / 2)) * SE),
      OR.ucl = odds.ratio + (qnorm(1 - ((1 - level) / 2)) * SE)
      # OR.lcl = exp(log(odds.ratio) - (qnorm(1 - ((1 - level) / 2)) * SE)),
      # OR.ucl = exp(log(odds.ratio) + (qnorm(1 - ((1 - level) / 2)) * SE))
    )

  if(missing(return)){
    return <- "print"
  }
  if(return == "print"){
    output <- list(
      probs = data.frame(input$emmeans) %>%
        mutate(across(where(is.numeric), ~ round( . , digits = digits))),
      contrasts = contrasts %>%
        mutate(across(where(is.numeric), ~ round( . , digits = digits)))
    )
    print(output)
  }
  if(return == "frame"){
    output <- list(
      probs = data.frame(input$emmeans),
      contrasts = contrasts
    )
    output
  }
}


#' Cohen's d using emmeans
#'
#' Convenience function which wraps around emmeans to provide Cohen's d and confidence intervals. Also rounds and can output a dataframe, for convenience. Printed output rounds to specified digits, frame output does not. Supports repeated-measures or within-group calculation of CI using within. Note that this function requires contrasts, it will fail otherwise.
#'
#' @param model Model to identify emmeans, supports most models supported by emmeans; however, your mileage may vary. In particular, built in support for "lm", "lme", "glm", "lmerTest", "lme4", "lmerModLmerTest", "glmerMod", and "glmmTMB"
#' @param formula emmeans formula with the format of contrast ~ x | w
#' @param within Used to calculated error for Cohen's d, formula of format x ~ b + (k) where x is the within-group variable, b is a between-group (optional), and k defines within-group clusters with the format (level2/level1). Cohen's d calculated is dav (Lakens, 2013) when within is specified
#' @param level Specifies level for confidence intervals, default is 95%
#' @param var Supports use of emtrends and finding Cohen's d for difference in simple slopes
#' @param return How to return output, default is to print
#' @param digits Digits to round to, default is 4
#' @param ... Additional arguments to pass to emmeans, including type = "response" to get odds ratios. Behavior may be unexpected with certain other arguments, such as at, or if a different type is specified
#'
#' @export
emm_cohend = function(
    model, formula, within,
    standardize, level = .95, var = NULL,
    verbose = FALSE, digits = 4,
    return = c("print", "frame"),
    at = NULL,
    ...
){
  ##### Notes #####

  ##### 2) Extract data & call from model #####

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

  if(!missing(standardize)){
    model <- update(
      model,
      data = standardize_frame(
        data, ...
      )
    )

  }

  if(!missing(within)){
    outcome <- switch(
      class(model),
      "lm" = model$call,
      "glm" = model$call,
      "lme" = model$call,
      "lmerTest" = model@call,
      "lmerModLmerTest" = model@call,
      "lme4" = model@call,
      "lmerMod" = model@call,
      "glmerMod" = model@call,
      "glmmTMB" = model$call,
      warning(class(model),
              " not supported for within effects! \n")
    )
    outcome <- unlist(
      str_split(deparse((outcome)$formula),pattern = " ")
    )[1]
  }

  ##### 2) Identify relevant variables and their classes #####

  # variables
  vars <- unlist(str_split(deparse(formula), pattern = " "))[-1]
  vars <- vars[str_detect(vars, "[[:alpha:]]+")]
  focal <- vars[1]

  # numeric_focal <- any(class(pull(data, focal)) %in%
  #                        c("integer", "double"))
  #
  # if(numeric_focal){
  #   warning(focal, " coded numerically, model refit \n")
  #   data <- data %>% mutate(across(
  #     all_of(focal),
  #     as.character
  #   ))
  #
  #   model <- update(model, data = data)
  #
  # }


  # within
  if(!missing(within)){

    within <- deparse(within)

    if(
      !unlist(str_split(within, pattern = " "))[1] == focal
    ){
      stop("Focal =/= within, consider respecification of model")
    }

    nesting <- str_extract(within, "\\([[:graph:]]+\\)") %>%
      str_remove_all( . , "\\(|\\)") %>%
      str_split( . , pattern = "\\/") %>% unlist()

    between <- unlist(str_split(within, pattern = " "))
    between <- between[str_detect(between, "[[:alnum:]]+")]
    between <- between[-c(1, length(between))]
    if(length(between) == 1 & between[1] == "0"){
      between <- NULL
    }

  }

  # at
  if(!missing(at)){
    at_numeric <- names(
      sapply(at, class)[sapply(at, class) == "numeric"]
    )
    at_categorical <- names(
      sapply(at, class)[sapply(at, class) %in%
                          c("character", "factor")]
    )
  }


  ##### 3) Run emmeans #####

  if(!is.null(var)){
    input <- emtrends(
      model, specs = formula, var = var,
      level = level, at = at,
      ...
    )

    means <- input$emtrends %>% data.frame %>%
      rename(emmean = paste0(var, ".trend"))
    contrasts <- data.frame(input$contrasts)

  } else {

    input <- emmeans(
      model, specs = formula,
      level = level, at = at,
      ...
    )

    means <- data.frame(input$emmeans)
    contrasts <- data.frame(input$contrasts)

  }

  contrasts$contrast <- str_remove_all(contrasts$contrast, focal)

  ##### 4) Calculate obs and r #####

  if(!missing(at)){
    data <- data %>%
      mutate(across(
        all_of(at_categorical),
        list(
          filter = ~ (! . == unname(unlist(at[cur_column()])))
        )
      )) %>%
      mutate(
        filter = rowSums(select(
          . , ends_with("filter")
        ))
      ) %>%
      filter(!filter > 0)

    obs <- data %>%
      select(all_of(vars)) %>%
      na.omit() %>%
      mutate(across(
        all_of(at_numeric),
        ~ as.numeric(as.character(cut(
          . , breaks = c(
            -Inf, unname(unlist(at[cur_column()])), Inf
          ),
          # Makes the choice to drop the lowest observations,
          labels = c(
            NA, unname(unlist(at[cur_column()]))
          )
        )))
      )) %>%
      group_by(across(all_of(vars))) %>%
      summarize(n = n(), .groups = "drop")

  } else {
    obs <- data %>%
      select(all_of(vars)) %>%
      na.omit() %>%
      group_by(across(all_of(vars))) %>%
      summarize(n = n(), .groups = "drop")
  }

  if(!missing(within)){
    if(any(nesting %in% between)){
      stop("Nesting variable also a between variable,
           should only be one \n")
    }

    r_frame <- data %>%
      select(
        all_of(focal), all_of(between), all_of(nesting),
        all_of(outcome)
      ) %>%
      pivot_wider(
        names_from = all_of(focal),
        values_from = all_of(outcome),
        values_fn = mean
      ) %>%
      select(- all_of(nesting)) %>%
    corrsBy(
      groups = between,
      by = NULL
    ) %>%
      select(v1 = Var1, v2 = Var2,
             r, all_of(between))

  }

  ##### 5) Merge data to calculate statistics #####

  ##### Means
  means <- left_join(means, obs, by = all_of(vars))

  ##### Contrast mean, SE, & n
  cont_join <- contrasts %>%
    mutate(v = contrast) %>%
    separate(v, into = c("v1", "v2"), sep = " - ") %>%
    select(contrast, v1, v2, all_of(vars[!vars == focal])) %>%
    mutate(across(
      c(v1, v2),
      # As factor allows for consistent pairing later
      # regardless of numeric or categorical comparisons
      ~ as.factor(str_remove_all( . , "\\(|\\)"))
    ))

  # if(!missing(at)){
  #   if(focal %in% at_numeric){
  #     cont_join <- cont_join %>%
  #       mutate(across(
  #         c(v1, v2),
  #         ~ as.numeric(str_remove_all( . , "\\(|\\)"))
  #       ))
  #   }
  # }


  # Pairs descriptives for each contrast
  # V1 = reference category
  # V2 = "treatment" category
  contrasts <- left_join(
    contrasts,

    full_join(
      left_join(
        select(cont_join, - v2),

        # need to keep any other variables in the formula
        # for appropriate pairing
        select(means, all_of(vars),
               v1 = all_of(focal), n1 = n,
               m1 = emmean, se1 = SE) %>%
          # as factor was meant to improve stability,
          # but has been a problem
          mutate(v1 = as.factor(v1)),

        # need to keep any other variables in the formula
        # for appropriate pairing, but not the focal (now V1)
        by = all_of(c("v1", vars[!vars == focal]))
      ),

      left_join(
        select(cont_join, - v1),

        select(means, all_of(vars),
               v2 = all_of(focal), n2 = n,
               m2 = emmean, se2 = SE) %>%
          mutate(v2 = as.factor(v2)),

        by = all_of(c("v2", vars[!vars == focal]))
      ),

      by = all_of(c("contrast", vars[!vars == focal]))
    ),

    by = all_of(c("contrast", vars[!vars == focal]))
  )

  ##### 6) Calculations #####

  if(!missing(within)){
    contrasts <- left_join(
      contrasts, r_frame,
      by = c("v1", "v2", between)
    ) %>%
      mutate(
        # Convert se to sd
        sd1 = se1 * sqrt(n1),
        sd2 = se2 * sqrt(n2),
        # dav, lakens 2013

        dav = (m1 - m2) / ((sd1 + sd2) / 2),
        d.se = sqrt(
          # WITHIN
          ((2 * (1 - r)) / (n1 + n2)) +
            ((dav^2) / (2 * (n1 + n2 - 2)))
        ),

        # confidence intervals determined using z distribution
        d.lcl = dav - (qnorm(1 - ((1 - level) / 2)) * d.se),
        d.ucl = dav + (qnorm(1 - ((1 - level) / 2)) * d.se)
      )

    if(verbose == F){
      contrasts <- contrasts %>%
        select(
          contrast,
          all_of(vars[vars %in% colnames(contrasts)]),
          estimate, SE, df, ends_with("ratio"), p.value,
          r, dav, d.lcl, d.ucl
        )
    }

  } else {

    contrasts <- contrasts %>%
      mutate(
        # Convert se to sd
        sd1 = se1 * sqrt(n1),
        sd2 = se2 * sqrt(n2),
        # formulas for s.pooled differ, the one used here
        # is appropriate for when sample sizes do not match
        s.pooled = sqrt(
          (((n1 - 1) * (sd1^2)) + ((n2 - 1) * (sd2^2))) /
            ((n1 - 1) + (n2 - 1))
        ),

        d = (m1 - m2) / s.pooled,
        d.se = sqrt(
          # BETWEEN
          ((n1 + n2) / (n1 * n2)) +
            ((d^2) / (2 * (n1 + n2 - 2)))
        ),

        # confidence intervals determined using z distribution
        d.lcl = d - (qnorm(1 - ((1 - level) / 2)) * d.se),
        d.ucl = d + (qnorm(1 - ((1 - level) / 2)) * d.se)

      )

    if(verbose == F){
      contrasts <- contrasts %>%
        select(
          contrast,
          all_of(vars[vars %in% colnames(contrasts)]),
          estimate, SE, df, ends_with("ratio"), p.value,
          d, d.lcl, d.ucl
        )
    }

  }

  ##### 7) Output #####

  if(missing(return)){return <- "print"}

  if(return == "print"){
    means <- means %>%
      mutate(across(
        where(is.numeric),
        ~ round( . , digits = digits)
      ))

    contrasts <- contrasts %>%
      mutate(across(
        where(is.numeric),
        ~ round( . , digits = digits)
      ))

    if(!is.null(var)){
      trends <- means %>%
        rename_with(
          .cols = emmean,
          .fn = ~ paste(var, ".slope", sep = "")
        )

      output <- list(
        trends = trends, contrasts = contrasts
      )

    } else {
      output <- list(
        means = means, contrasts = contrasts
      )
    }

    print(output)
  }

  if(return == "frame"){
    if(!is.null(var)){
      trends <- means %>%
        rename_with(
          .cols = emmean,
          .fn = ~ paste(var, ".slope", sep = "")
        )

      output <- list(
        trends = trends, contrasts = contrasts
      )

    } else {
      output <- list(
        means = means, contrasts = contrasts
      )

    }

    return(output)

  }

}

