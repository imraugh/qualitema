#
# summarize_terms <- function(
#     models, pattern,
#     output = c("list", "dataframe"),
#     ...
# ){
#   if(missing(models)){
#     models <- objects()
#
#     models <- models[str_detect(
#       models, pattern
#     )]
#
#   }
#
#
# }
#
#
# summarize_omnibus <- function(
#   models, pattern,
#   output = c("list", "dataframe"),
#   ...
# ){
#
#   if(missing(models)){
#     models <- objects()
#
#     models <- models[str_detect(
#       models, pattern
#     )]
#
#   }
#
#
#
# }
#
#
# omnibus_tests <- function(
#   model, ...
# ){
#   input <- car::Anova(get(model), ... )
#   output <- input %>%
#     data.frame %>%
#     mutate(
#       term = row.names( . )
#     )
# }
#
