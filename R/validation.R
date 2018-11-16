validate_entry <- function(x, what, rds_tibble = NULL, max.length  = 9999L) {

  if(is.numeric(x)) {
    x <- validate_int(x = x, what = what, rds_tibble = rds_tibble,
                      max.length = max.length)
  }
  else {
    x <- validate_char(x = x, what = what, rds_tibble = rds_tibble,
                       max.length = max.length)
  }

  return(x)
}



validate_int <- function(x, what, rds_tibble = NULL, max.length) {

  if(!checkmate::test_integerish(x, lower = 1, upper = 9999,
                                 any.missing = FALSE,
                                 min.len = 1, max.len = max.length)) {

    if(max.length == 1) {
      stop("Please enter the ", what,
           " id as a single integer between 1 and 9999.")
    }
    else {
      stop("Please enter the ", what,
           " ids as a vector of integers, all between 1 and 9999.")
    }
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

validate_char <- function(x, what, rds_tibble, max.length) {

  if(is.null(rds_tibble)) {
    stop("id must be an integer")
  }

  if(!checkmate::test_character(x, min.chars = 1, any.missing = FALSE,
                                min.len = 1, max.len = max.length)) {
    stop("Entered ", what, "s must be a vector with maximum length of ",
         max.length, " with each entry having at least 1 character")
  }

  x <-
    purrr::map_int(
      x,
      function(string) {
        matches <- grep(pattern     = string,
                        x           = paste(rds_tibble[[2]], rds_tibble[[3]]),
                        ignore.case = TRUE)
        if(length(matches) != 1) {
          if(length(matches) == 0) {
            stop("No ", what, " found containing \'", string, "\' in the ",
                 colnames(rds_tibble)[2], "+", colnames(rds_tibble)[3])
          }
          else if(length(matches) > 1) {
            print(rds_tibble[matches, ])
            stop(string, " matches the ", colnames(rds_tibble)[2], "+",
                 colnames(rds_tibble)[3], " of all of the above ", what,
                 "s. Be more specific to differentiate or enter their ", what,
                 " id numbers instead.")
          }
        }
        return(rds_tibble$id[matches])
      }
    )

  return(x)
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


# For a character vector of id numbers (in character form, e.g., "5") and
# names/titles (e.g., "Smith")
validate_mixed <- function(vector, what, tibble) {

  ids <-
    sapply(
      X   = vector,
      FUN = function(x) {
        validate_entry(
          x          = ifelse(test = is.na(suppressWarnings(as.numeric(x))),
                              yes  = x,
                              no   = as.numeric(x)),
          what       = what,
          max.length = 1,
          rds_tibble = tibble)
      },
      USE.NAMES = FALSE)

  if(anyDuplicated(ids) > 0) {
    stop("The following ", what, "s are duplicates: ",
         paste(vector[duplicated(ids)], collapse = ", "), ". Check all names/",
         "titles and id numbers.")
  }

  return(ids)
}


validate_new <- function(id, what, rds_tibble) {

  if(nrow(rds_tibble) > 9999L) {
    stop("Maximum number of ", what, "s reached.")
  }

  if(is.na(id)) {
    id <- min(setdiff(1L:9999L, rds_tibble$id))
  }
  else {

    id <- validate_int(x = id, what = what, max.length = 1)

    if(id %in% rds_tibble$id) {
      stop('id number already taken. Try a different one or leave the ',
           'argument blank for automatic selection ',
           '(the lowest counting number still available).')
    }
  }

  return(id)
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
