#' Correlations by group or specified list of variables
#'
#' Convenience function which allows correlations to be found by multiple groups or based on a set of specified focal variables (specified in by). Can conduct pearson or spearman correlations and filter output by specified alpha level.
#'
#' @param frame Matrix or dataframe of variables to correlate. Requires numeric except for grouping variable(s)
#' @param groups Retained for backwards compatibility. Vector of columns to group by, can be several using c()
#' @param by Retained for backwards compatibility. Vector of columns to display correlations by. Inspired from function in SPSS. Provides correlations y ~ x, where y is every other variable in the dataframe and x are the variable names provided for this argument. Can accept formula in the form . ~ x1 + x2 + [...].
#' @param type Pearson or spearman correlations
#' @param alpha Alpha level to display correlations, defaults to Inf (all printed)
#' @param p.adjust P level adjustment using p.adjust.methods. Evaluated after all other steps except rounding. Under the hood, all possible correlations are conducted (full correlation matrix) however, adjustments are only done on those of interest (e.g., those implied via by argument)
#' @param digits Rounding for displayed results, defaults to 3
#' @param autocor Should self-correlations (always 1) be included/ displayed, default is FALSE
#'
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @importFrom Hmisc rcorr
#'
#' @examples
#'
#' test_frame <- as.data.frame(
#'   matrix(rnorm(600), ncol = 10)
#' )
#' test_frame$group1 <- rep(c("A", "B"), 30)
#' test_frame$group2 <- rep(c("X", "Y", "Z"), 20)
#'
#' @export
#'
corrsBy = function(
  frame, specs = NULL,
  groups = NULL, by = NULL,
  type = "pearson", alpha = Inf,
  p.adjust = "none", digits = 3,
  autocor = FALSE
){

  if(!is.null(specs)){
    specs <- deparse(specs)
    split_loc = str_locate(specs, "~")[1]

    if(is.null(groups)){
      groups <- str_sub(specs, end = split_loc)
      groups <- unlist(str_split(groups, " "))
      groups <- groups[str_detect(groups, "[[:alnum:]]+")]
      if(length(groups) == 0){groups = NULL}
    }
    if(is.null(by)){
      by <- str_sub(specs, start = split_loc)
      by <- unlist(str_split(by, " "))
      by <- by[str_detect(by, "[[:alnum:]]+")]
      if(length(by) == 0){by = NULL}
    }


  }


  non_numeric <- colnames(select(
    frame, where(~ !is.numeric( . ))
  ))
  # non_numeric <- non_numeric[!non_numeric %in% groups]
  non_numeric <- non_numeric[!non_numeric %in% groups]

  if(length(non_numeric) > 0){
    stop("Not all columns are numeric: \n",
         non_numeric)
  }

  if(!is.data.frame(frame)){
    frame <- as.data.frame(frame)
    warning("Object coerced to dataframe \n")
  }


  if(class(by) == "formula"){
    by = unlist(str_split(deparse(by), " "))
    by = by[str_detect(by, "[[:alnum:]]+")]
  }

  if(is.null(groups)){
    frame <- nest(frame, data = everything())
  } else {
    frame <- frame %>%
      group_by(across(all_of(groups))) %>%
      nest()
  }

  frame <- frame %>%
    mutate(corrs = map(data, function(frame){
      Hmisc::rcorr(as.matrix(frame), type = type)
    }))

  if(is.null(groups)){
    names <- "All"
  } else {
    names <- frame %>%
      unite(col = names, - c(data, corrs)) %>%
      pull(names)
  }

  names(frame$corrs) <- names

  output <- lapply(
    names(frame$corrs),
    function(x){
      n <- as.data.frame(frame$corrs[[x]]$n) %>%
        mutate(Var1 = row.names( . )) %>%
        pivot_longer(
          - Var1,
          names_to = "Var2", values_to = "n"
        )

      r <- as.data.frame(frame$corrs[[x]]$r) %>%
        mutate(Var1 = row.names( . )) %>%
        pivot_longer(
          - Var1,
          names_to = "Var2", values_to = "r"
        ) %>%
        filter(!is.na(r)
        )

      p <- as.data.frame(frame$corrs[[x]]$P) %>%
        mutate(Var1 = row.names( . )) %>%
        pivot_longer(
          - Var1,
          names_to = "Var2", values_to = "p"
        )

      corrs <- full_join(n, r, by = c("Var1", "Var2"))
      corrs <- full_join(corrs, p, by = c("Var1", "Var2"))

      output <- data.frame(groups = x, corrs) %>%
        mutate(across(where(is.factor), as.character))

      return(output)
    }
  ) %>%
    bind_rows()

  if(autocor == FALSE){
    output <- filter(
      output,
      !Var1 == Var2
    )
    output <- mutate(output, p = ifelse(is.na(p), 0, p))
  } else {
    # sets autocor p to 0 so they aren't filtered
    output <- mutate(output, p = ifelse(is.na(p), 0, p))
  }

  if(!is.null(by)){
    output <- filter(output, Var1 %in% by, !Var2 %in% by)
  }

  if(p.adjust == "none"){
    # adjusts p values,
    #   done at the end to avoid overcorrection
    output <- output %>%
      mutate(p = p.adjust(p, method = p.adjust))
  }

  output <- filter(output, p <= alpha)

  output <- output %>%
    mutate(across(
      where(is.numeric),
      round, digits
    ))

  if(is.null(groups)){
    output <- output %>% select( - groups)
  } else {
    output <- output %>%
      separate(
        col = "groups",
        into = groups,
        sep = "_"
      )
  }

  return(output)
}


#' Plot correlations
#'
#' Creates a plot of correlations for one group or two.
#'
#' @param frame Dataframe to find correlations from, must be numeric except for grouping variable
#' @param groups Groups to use with a format list(groups = c("y", "x")); groups will be placed along the y triangle or x triangle based on placement
#' @param by Variables to sort plot by, limits x axis. Behavior with groups will likely not provide the desired output
#' @param type Type of correlations, Pearson or Spearman
#' @param alpha Alpha level, default is .05. Correlations significant below alpha will be colored, those above alpha will not be colored
#' @param p.adjust Should p values be adjusted before plotting?
#' @param digits Digits to display in the plot, default is 2
#' @param auto.name Should names be automatically capitalized and separated by spaces? Default is FALSE
#' @param colors How should values be colored? Accepts a list of color specifications with default colors = list(low = "red", med = "white", high = "blue")
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
#'

corrplot <- function(
  frame,
  groups = NULL, by = NULL,
  type = "pearson", alpha = .05,
  p.adjust = "none", digits = 2,
  auto.name = FALSE,
  colors = list(
    low = "red", med = "white", high = "blue"
  )
){

  if(is.null(groups)){

  } else {

    if(length(groups) > 1){
      stop("Only one pair of groups can be specified at a time \n")
    }

    if(!length(groups[[1]]) == 2){
      stop("Groups must specifiy two groups or be ommitted \n")
    }
  }

  if(!is.null(groups) & !is.null(by)){
    warning("Behavior with both groups and by is unpredictable,
            use of only one at a time is recommended \n")
  }

  ##### Run correlations #####

  corrs <- corrsBy(
    frame, specs = NULL,
    groups = names(groups),
    by = by,
    type = type, alpha = Inf,
    p.adjust = p.adjust,
    digits = Inf,
    autocor = TRUE
  )

  if(auto.name == TRUE){
    corrs <- mutate(
      corrs,
      Var1 = clean_string(Var1, sep = " ", case = "title"),
      Var2 = clean_string(Var2, sep = " ", case = "title")
    )
  }

  corrs$r <- round(corrs$r, digits = digits)


  if(is.null(groups)){

    ##### One group #####
    if(is.null(by)){

      corrs <- corrs %>%
        arrange(Var1, Var2) %>%
        mutate(
          Var1.num = as.numeric(
            factor(Var1)
          ),
          Var2.num = as.numeric(
            factor(Var2)
          )
        ) %>%
        filter(Var1.num <= Var2.num)
    }

    plot <- ggplot(
      corrs,
      aes(x = Var1, y = Var2)
    ) +
      #    Color tiles
      geom_tile(
        aes(fill = r),
        width = 1, height = 1
      ) +
      geom_tile(
        data = filter(corrs, p > alpha),
        aes(x = Var1, y = Var2), fill = "white",
        width = 1, height = 1
      ) +
      geom_text(
        aes(label = r)
      ) +
      #   Aesthetic mappings
      scale_fill_gradient2(
        low = colors[["low"]],
        high = colors[["high"]],
        mid = colors[["med"]],
        na.value = "white",
        midpoint = 0, limits = c(-1, 1)
      ) +
      theme_classic() +
      scale_x_discrete(position = "top")

    return(plot)
  } else {
    ##### Two groups #####
    A = groups[[1]][1]
    B = groups[[1]][2]

    corrs_A <- corrs %>%
      rename(
        groups = names(groups)
      ) %>%
      filter(groups == A) %>%
      arrange(Var1, Var2) %>%
      mutate(
        Var1.num = as.numeric(
          factor(Var1)
        ),
        Var2.num = as.numeric(
          factor(Var2)
        )
      ) %>%
      filter(Var1.num <= Var2.num)

    corrs_B <- corrs %>%
      rename(
        groups = names(groups)
      ) %>%
      filter(groups == B) %>%
      arrange(Var1, Var2) %>%
      mutate(
        Var1.num = as.numeric(
          factor(Var1)
        ),
        Var2.num = as.numeric(
          factor(Var2)
        )
      ) %>%
      filter(Var1.num > Var2.num)

    corrs <- bind_rows(corrs_A, corrs_B)

    plot <- ggplot(
      corrs,
      aes(x = Var1, y = Var2)
    ) +
      #    Color tiles
      geom_tile(
        aes(fill = r),
        width = 1, height = 1
      ) +
      geom_tile(
        data = filter(corrs, p > alpha),
        aes(x = Var1, y = Var2), fill = "white",
        width = 1, height = 1
      ) +
      geom_text(
        aes(label = r)
      ) +
      #   Aesthetic mappings
      scale_fill_gradient2(
        low = colors[["low"]],
        high = colors[["high"]],
        mid = colors[["med"]],
        na.value = "white",
        midpoint = 0, limits = c(-1, 1)
      ) +
      theme_classic() +
      labs(
        y = A, x = B
      )

    return(plot)

  }

}



corrplotBy <- function(
  frame, group, type = "pearson", names,
  low = "red", med = "white", high = "blue",
  alpha = .05, p.adjust = "none", facet_frame
){

  ##### In development #####
  stop(
    "Function not ready for use!\n"
  )

  ### About facet_frame:
  ###   facet_frame uses Y and a label for that Y
  ###   E.g., Y items 1:3 are from scale "SCLA",
  ###         Y items 4:5 are from scale "SCLB",
  ###         Y items 6:8 are from scale "SCLC":
  ###         facet_frame <- data.frame(
  ###             Y = 1:8,
  ###             facet = c(
  ###                 rep("SCLA", 3),
  ###                 rep("SCLB", 2),
  ###                 rep("SCLC", 3)
  ###               )
  ###             )
  ## Prep
  groups <- levels(as.factor(
    frame %>%
      select_at(vars(groups = one_of(group))) %>%
      pull(groups)
  ))
  if(!length(groups) == 2){
    stop("Must use two groups! If only one group desired, see corrplot \n")
  }
  if(missing(names)){
    names <- colnames(frame)[!str_detect(colnames(frame),
                                         group)]
  }
  if(!length(names) == (ncol(frame) - 1)){
    stop("Number of names does not match number of columns \n")
  }
  A <- groups[1]
  B <- groups[2]

  corrs <- corrsBy(
    frame, group = group, type = type,
    alpha = 1, p.adjust = p.adjust, round = 2,
    selfcorrs = TRUE
  )


  frame_r <- transmute(corrs, group, value = r,
                       X = rep(c(1:length(names)),
                               nrow(corrs) / length(names)),
                       Y = rep(
                         rep(c(1:length(names)),
                             each = length(names)), 2
                       ))
  frame_p <- transmute(corrs, group, value = p,
                       X = rep(c(1:length(names)),
                               nrow(corrs) / length(names)),
                       Y = rep(
                         rep(c(1:length(names)),
                             each = length(names)), 2
                       )) %>%
    filter(!value <= alpha)

  if(!missing(facet_frame)){
    if(!nrow(facet_frame) ==
       max(frame_r$Y)){
      stop("Error: Facets and Variables do not align \n")
    }
    if(!all(c("Y", "facet") %in% colnames(facet_frame))){
      stop("Error: Improper names, please use Y and facet \n")
    }
    facet_frame$facet <- factor(facet_frame$facet,
                                levels = unique(facet_frame$facet))
    frame_r <- left_join(frame_r, facet_frame, by = "Y")
    frame_p <- left_join(frame_p, facet_frame, by = "Y")
  }

  ## Plot
  plot <- ggplot() +
    #    A group color tiles
    geom_tile(data = frame_r[frame_r$group ==  A, ],
              aes(x = X, y = Y, fill = value),
              width = 1, height = 1) +
    geom_tile(data = frame_p[frame_p$group ==  A, ],
              aes(x = X, y = Y), fill = "white",
              width = 1, height = 1) +
    geom_text(data = frame_r[frame_r$group ==  A, ],
              aes(x = X, y = Y, label = value)) +
    #    B color tiles
    geom_tile(data = frame_r[frame_r$group ==  B, ],
              aes(x = X, y = Y, fill = value),
              width = 1, height = 1) +
    geom_tile(data = frame_p[frame_p$group ==  B, ],
              aes(x = X, y = Y), fill = "white",
              width = 1, height = 1) +
    geom_text(data = frame_r[frame_r$group ==  B, ],
              aes(x = X, y = Y, label = value)) +
    #   Aesthetic mappings
    scale_x_continuous(breaks = 1:length(names),
                       labels = names,
                       position = "top") +
    scale_y_reverse(
      breaks = 1:length(names),
      labels = names,
      sec.axis = sec_axis( ~ . ,
                           name = paste(B, " Correlations",
                                        sep = ""),
                           breaks = 1:length(names),
                           labels = names)
    ) +
    scale_fill_gradient2(low = low, high = high,
                         mid = med, na.value = "white",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = NULL,
         y = paste(A, " Correlations", sep = ""),
         x = NULL,
         fill = "Correlation magnitude") +
    theme_classic() +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle = 315, vjust = 1, hjust = 1),
          strip.background = element_rect(fill = "grey"))
  if(!missing(facet_frame)){
    plot <- plot +
      facet_grid(facet ~ . , scale = "free", switch = "both")
  }
  plot
}

# corrplot <- function(frame, type, alpha = 1, display = 0,
#                      p.adjust = NULL, round = 3, dupes = FALSE,
#                      preserve.names = TRUE, list = FALSE){
#   ##### In development #####
#   stop(
#     "Function not ready for use!\n"
#   )
#   ## Prep
#   if(!is.data.frame(frame)){
#     frame <- as.data.frame(frame)
#     warning("Object coerced to dataframe \n")
#   }
#   if(missing(type)){
#     type <- "pearson"
#   }
#
#
#   corr <- as.matrix(select_if(frame, is.numeric))
#
#   ## Run correlations
#   corr <- Hmisc::rcorr(corr, type = type)
#
#   ## R values
#   corr_r <- corr$r
#   if(preserve.names == FALSE){
#     colnames(corr_r) <- 1:ncol(corr_r)
#     rownames(corr_r) <- 1:ncol(corr_r)
#   }
#   if(dupes == FALSE){
#     corr_r[lower.tri(corr_r)] <- NA
#   }
#
#   corr_r <- as.data.frame(corr_r)
#   corr_r$Var1 <- rownames(corr_r)
#   corr_r <- gather(corr_r, - Var1,
#                    key = "Var2", value = "value") %>%
#     filter(!is.na(value)) %>%
#     mutate(value = as.numeric(value))
#   if(preserve.names == FALSE){
#     corr_r <- mutate(corr_r,
#                      Var1 = as.numeric(Var1),
#                      Var2 = as.numeric(Var2))
#   }
#   corr_r$value <- round(corr_r$value, round)
#
#
#   ## P values
#   corr_p <- corr$P
#   if(preserve.names == FALSE){
#     colnames(corr_p) <- 1:ncol(corr_p)
#     rownames(corr_p) <- 1:ncol(corr_p)
#   }
#   corr_p <- as.data.frame(corr_p)
#   corr_p$Var1 <- rownames(corr_p)
#   corr_p <- gather(corr_p, - Var1,
#                    key = "Var2", value = "value") %>%
#     filter(!is.na(value)) %>%
#     mutate(value = as.numeric(value))
#   if(preserve.names == FALSE){
#     corr_p <- mutate(corr_p,
#                      Var1 = as.numeric(Var1),
#                      Var2 = as.numeric(Var2))
#   }
#   corr_p$value <- round(corr_p$value, round)
#   if(!is.null(p.adjust)){
#     adjust.method <- p.adjust
#     corr_p$value <- p.adjust(corr_p$value, method = adjust.method)
#   }
#   corr_p <- corr_p[corr_p$value <= alpha &
#                      corr_p$value >= display, ]
#   ## Output
#   if(list == TRUE){
#     output <- list("r_values" = corr_r,
#                    "p_values" = corr_p)
#     output$r_values <- corr_r
#     output$p_values <- corr_p
#   } else {
#     output <- left_join(corr_r, corr_p,
#                         by = c("Var1", "Var2"),
#                         suffix = c(".r", ".p"))
#   }
#   return(output)
# }

correlogram <- function(frame, side = c("right", "left"), names, type,
                        alpha = .05, p.adjust = NULL,
                        low = "red", med = "white", high = "blue"){
  ##### In development #####
  stop(
    "Function not ready for use!\n"
  )
  ## Prep
  frame <- select_if(frame, is.numeric)
  if(missing(type)){
    type <- "pearson"
  }
  if(missing(names)){
    names <- colnames(frame)
  }
  if(!length(names) == ncol(frame)){
    stop("Number of names does not match number of columns \n")
  }
  if(length(side) > 1){
    side <- "left"
  }

  corr_list <- corrplot(frame, type = type, display = alpha,
                        round = 2, preserve.names = FALSE,
                        p.adjust = p.adjust, list = TRUE)
  frame_r <- corr_list$r_values
  frame_p <- corr_list$p_values


  ## Plot
  plot <- ggplot() +
    #    Color tiles
    geom_tile(data = frame_r,
              aes(x = Var1, y = Var2, fill = value),
              width = 1, height = 1) +
    geom_tile(data = frame_p,
              aes(x = Var1, y = Var2), fill = "white",
              width = 1, height = 1) +
    geom_text(data = frame_r,
              aes(x = Var1, y = Var2, label = value)) +
    #   Aesthetic mappings
    scale_fill_gradient2(low = low, high = high,
                         mid = med, na.value = "white",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = NULL,
         x = NULL,
         fill = "Correlation magnitude") +
    theme_classic() +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle = 315, vjust = 1, hjust = 1),
          strip.background = element_rect(fill = "grey"))
  if(side == "right"){
    plot <- plot +
      scale_x_reverse(breaks = 1:length(names),
                      labels = names,
                      position = "top") +
      scale_y_continuous(breaks = 1:length(names),
                         labels = names,
                         position = "right")
  }
  if(side == "left"){
    plot <- plot +
      scale_x_continuous(breaks = 1:length(names),
                         labels = names,
                         position = "top") +
      scale_y_continuous(breaks = 1:length(names),
                         labels = names)
  }
  plot
}

