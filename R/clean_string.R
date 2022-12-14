#' Clean string vector
#'
#' Cleans up a given string vector, particularly useful if you have a vector of names you want to work with (e.g., converting lowercase to Sentence case or replacing underscore separators with period separators). Can also be extended to data frames either through providing a dataframe object with names = TRUE or through clean_colnames. Depends on `stringr`.
#'
#' @param object Vector of column names or named vector. If a dataframe is provided, also specify names = TRUE, this is implemented to prevent confusion
#' @param sep Value to use to separate alphanumeric characters, default is a period "."
#' @param names Clean names of an object? Default is FALSE, use TRUE for objects where the names should be cleaned rather than the values
#' @param numlead Should numerical values be allowed at the start of a string? Default is FALSE
#' @param case What case should values be returned in? Default is unchanged, cases such as Title Case, lower case, UPPER CASE, or Sentence case supported. Case is implemented before separation, so the only difference between title and sentence is if input includes spaces to separate words.
#'
#' @examples

#' test_string <- c(
#'   "Bm n1sXhl2", "V2DQDwl#XK", "Bs Sd2s lZ", "l9puN&P9vf",
#'   "JYCaptNr K", "w$ W(NPP(U", "wNmbUDC8dY", "lL%FqtYYSV",
#'   "3oLZ9wlpLX", "4Xx%2gaG S"
#' )
#' clean_string(test_string, case = "title", sep = "_")
#'
#' named_vector <- setNames(
#' c(1:10), test_string
#' )
#' clean_string(named_vector, names = T)
#'
#' frame <- as.data.frame(matrix(rnorm(100), ncol = 10))
#' colnames(frame) <- test_string
#' clean_string(frame, names = T, sep = ".", case = "lower")
#' # compare with:
#' clean_colnames(frame, sep = ".", case = "lower")
#'
#' @export
clean_string = function(
  object, sep = ".",
  names = FALSE, numlead = FALSE,
  case = c("title", "lower", "upper", "sentence")
){
  object_test <- (is.character(object) | is.factor(object))

  if(object_test == FALSE & names == FALSE){
    stop("Object is not character string, should names = TRUE? \n")
  }

  if(sep == ""){
    # Used to allow for no separator
    sep <- "\\"
  }
  char <- sep
  # If names = TRUE, extracts names to edit
  if(names == TRUE){
    values <- object
    object <- names(object)
    if(is.null(object)){
      stop("No names found, should names = FALSE? \n")
    }
  }

  # Establish case, default is unchanged
  if(missing(case)){
    case <- "unspecified"
  }
  if(case == "title"){
    object <- stringr::str_to_title(object)
  }
  if(case == "lower"){
    object <- stringr::str_to_lower(object)
  }
  if(case == "upper"){
    object <- stringr::str_to_upper(object)
  }
  if(case == "sentence"){
    object <- stringr::str_to_sentence(object)
  }

  # Changes all characters & spaces to sep
  object <- stringr::str_replace_all(object, "[:punct:]", char)
  object <- stringr::str_replace_all(object, "[:space:]", char)

  # Removes double+, leading, and trailing seps
  if(numlead == FALSE){
    object <- stringr::str_remove(object, "^[:digit:]+")
  }
  if(sep %in% c(".", "\\*", "\\^", "\\(", "\\{", "\\+", "\\")){
    object <- stringr::str_replace_all(
      object,
      paste("\\", char, "{2,}", sep = ""),
      char
    )
    object <- stringr::str_remove(
      object, paste("^\\", char, sep = "")
    )
    object <- stringr::str_remove(
      object, paste("\\", char, "$", sep = "")
    )
  } else {
    object <- stringr::str_replace_all(
      object,
      paste(char, "{2,}", sep = ""),
      char
    )
    object <- stringr::str_remove(
      object, paste("^", char, sep = "")
      )
    object <- stringr::str_remove(
      object, paste(char, "$", sep = "")
      )
  }
  if(numlead == FALSE){
    object <- stringr::str_remove(object, "^[:digit:]+")
  }

  # Returns object
  if(names == TRUE){
    names(values) <- object
    object <- values
    remove(values)
  }
  return(object)
}

#' Clean string vector
#'
#' Cleans up a given string vector, particularly useful if you have a vector of names you want to work with (e.g., converting lowercase to Sentence case or replacing underscore separators with period separators). Can also be extended to data frames either through providing a dataframe object with names = TRUE or through clean_colnames. Depends on `stringr`.
#'
#' @param object Vector of column names or named vector. If a dataframe is provided, also specify names = TRUE, this is implemented to prevent confusion
#' @param sep Value to use to separate alphanumeric characters, default is a period "."
#' @param names Clean names of an object? Default is FALSE, use TRUE for objects where the names should be cleaned rather than the values
#' @param numlead Should numerical values be allowed at the start of a string? Default is FALSE
#' @param case What case should values be returned in? Default is unchanged, cases such as Title Case, lower case, UPPER CASE, or Sentence case supported. Case is implemented before separation, so the only difference between title and sentence is if input includes spaces to separate words.
#'
#' @import stringr
#'
#' @examples
#'
#' test_string <- c(
#'   "Bm n1sXhl2", "V2DQDwl#XK", "Bs Sd2s lZ", "l9puN&P9vf",
#'   "JYCaptNr K", "w$ W(NPP(U", "wNmbUDC8dY", "lL%FqtYYSV",
#'   "3oLZ9wlpLX", "4Xx%2gaG S"
#' )
#' clean_string(test_string, case = "title", sep = "_")
#'
#' named_vector <- setNames(
#' c(1:10), test_string
#' )
#' clean_string(named_vector, names = T)
#'
#' frame <- as.data.frame(matrix(rnorm(100), ncol = 10))
#' colnames(frame) <- test_string
#' clean_string(frame, names = T, sep = ".", case = "lower")
#' # compare with:
#' clean_colnames(frame, sep = ".", case = "lower")
#'
#'
#' @export
clean_colnames = function(
  object, sep = ".",
  names = TRUE, numlead = FALSE,
  case = c("title", "lower", "upper", "sentence")
){


  if(is.data.frame(object) == FALSE){
    names <- is.data.frame(try(as.data.frame(object)))
    if(names == FALSE){
      stop("Object cannot be coerced to data frame \n")
    } else {
      warning("Object coerced to data frame. \n")
    }
  }

  if(sep == ""){
    sep <- "\\"
  }
  char <- sep
  cols <- names(object)

  # Establish case, default is unchanged
  if(missing(case)){
    case <- "unspecified"
  }
  if(case == "title"){
    cols <- stringr::str_to_title(cols)
  }
  if(case == "lower"){
    cols <- stringr::str_to_lower(cols)
  }
  if(case == "upper"){
    cols <- stringr::str_to_upper(cols)
  }
  if(case == "sentence"){
    cols <- stringr::str_to_sentence(cols)
  }

  # Changes all characters & spaces to sep
  cols <- stringr::str_replace_all(cols, "[:punct:]", char)
  cols <- stringr::str_replace_all(cols, "[:space:]", char)

  # Removes douple+, leading, and trailing seps
  if(numlead == FALSE){
    cols <- stringr::str_remove(cols, "^[:digit:]+")
  }
  if(sep %in% c(".", "\\*", "\\^", "\\(", "\\{", "\\+", "\\")){
    cols <- stringr::str_replace_all(
      cols,
      paste("\\", char, "{2,}", sep = ""),
      char
    )
    cols <- stringr::str_remove(
      cols, paste("^\\", char, sep = "")
    )
    cols <- stringr::str_remove(
      cols, paste("\\", char, "$", sep = "")
    )
  } else {
    cols <- stringr::str_replace_all(
      cols,
      paste(char, "{2,}", sep = ""),
      char
    )
    cols <- stringr::str_remove(
      cols, paste("^", char, sep = "")
    )
    cols <- stringr::str_remove(
      cols, paste(char, "$", sep = "")
    )
  }
  if(numlead == FALSE){
    cols <- stringr::str_remove(cols, "^[:digit:]+")
  }
  dup_check <- sapply(1:length(cols), function(x) cols == cols[x])
  dup_check <- dup_check[upper.tri(dup_check)]
  if(length(dup_check[dup_check]) > 0){
    stop("Duplicate names found \n",
         cols[dup_check])
  }

  # Returns cols
  names(object) <- cols

  return(object)
}
