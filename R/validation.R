validate_new <- function(id, what, rds_tibble) {

  if(nrow(rds_tibble) > 9999L) {
    stop("Maximum number of ", what, "s reached.")
  }

  id <- validate_int(x = id, what = what, rds_tibble = NULL)

  if(is.na(id)) {
    id <- min(setdiff(1L:9999L, rds_tibble$id))
  }
  else if(id %in% rds_tibble$id) {
    stop('id number already taken. Try a different one or leave the ',
         'argument blank for automatic selection ',
         '(the lowest counting number still available).')
  }

  return(id)
}


validate_entry <- function(x, what, rds_tibble = NULL, max.length = 9999,
                           allow_dups = FALSE) {

  if(!checkmate::test_vector(x = x, any.missing = FALSE, min.len = 1,
                             max.len = max.length, unique = TRUE)) {
    stop("Must enter ", what, " as a vector of minimum length 1, maximum ",
         "length ", max.length, " and no missing values or duplicate values.")
  }

  ids <-
    lapply(X   = x,
           FUN = function(char) {
             num <- suppressWarnings(as.numeric(char))
             if(is.na(num)) {
               validate_char(x = char, what = what, rds_tibble = rds_tibble,
                             allow_dups = allow_dups)
             }
             else {
               validate_int(x = num, what = what, rds_tibble = rds_tibble)
             }
           }) %>%
    unlist()

  if(anyDuplicated(ids) > 0) {
    stop("The following ", what, "s are duplicates: ",
         paste(x[duplicated(ids)], collapse = ", "), ". Check all names/",
         "titles and id numbers.")
  }

  return(ids)
}



validate_int <- function(x, what, rds_tibble) {

  if(!checkmate::test_integerish(x, lower = 1, upper = 9999)) {
    stop(what, " id number must be a single integer between 1 and 9999.")
  }

  x <- as.integer(x)

  if(!is.null(rds_tibble)) {

    id_checks <-
      purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble$id)

    if(!all(id_checks)) {
      stop("The following ", what, " id(s) not found:",
           paste(x[!id_checks], collapse = ", "))
    }
  }

  return(x)
}



validate_char <- function(x, what, rds_tibble, allow_dups) {

  if(!checkmate::test_character(x, min.chars = 1)) {
    stop("Each ", what, " entered as a character strings must contain at ",
         "least 1 character")
  }

  matches <- grep(pattern     = x,
                  x           = paste(rds_tibble[[2]], rds_tibble[[3]]),
                  ignore.case = TRUE)

  if(length(matches) != 1) {

    if(length(matches) == 0) {
      stop("No ", what, " found containing \'", x, "\' in the ",
           colnames(rds_tibble)[2], "+", colnames(rds_tibble)[3])
    }
    else if(length(matches) > 1 && !allow_dups) {
      print(rds_tibble[matches, ])
      stop(x, " matches the ", colnames(rds_tibble)[2], "+",
           colnames(rds_tibble)[3], " of all of the above ", what,
           "s. Be more specific to differentiate or enter their ", what,
           " id numbers instead.")
    }
  }

  return(rds_tibble$id[matches])
}



#' @importFrom rlang .data
validate_assoc <- function(x, what, rds_tibble, what2, rds_tibble2) {
  id_checks <-
    purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble2$id2)

  if(!all(id_checks)) {
    print(dplyr::filter(rds_tibble, .data$id %in% x[!id_checks]))
    stop("The above ", what, "(s) not found in ", what2, "'s ", what, " list.")
  }

  return(x)
}


validate_directory <- function(path,
                               p_path = p_path_internal(),
                               make_directories = FALSE) {

  path <- fs::path_tidy(path)

  if(is.null(p_path)) {
    if(tolower(fs::path_file(path)) == "projects") {
      path <- fs::path_dir(path)
    }
  }
  else {
    if(!fs::path_has_parent(path, p_path)) {
      path <- fs::path(p_path, path)
    }
  }

  if(!make_directories && !fs::dir_exists(path)) {
    stop("Specified directory does not exist. Create it or set ",
         "make_directories = TRUE.")
  }

  return(unclass(path))
}


validate_stage <- function(stage, choices) {

  if(is.na(stage)) {

    return(choices[1])

  }
  else {

    stage <- as.character(stage)

    checkmate::assert_character(stage, min.chars = 1, any.missing = FALSE,
                                len = 1)

    stage <- factor(x      = grep(pattern     = stage,
                                  x           = choices,
                                  ignore.case = TRUE,
                                  value       = TRUE),
                    levels = choices)

    if(length(stage) != 1) {
      stop("User input for the 'stage' argument must match exactly one of ",
           "the following:\n", paste(choices, collapse = "\n"))
    }

    return(stage)
  }
}
