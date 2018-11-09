validate_entry <- function(x, what, rds_tibble = NULL,
                           what2 = NA, rds_tibble2 = NULL,
                           max.length  = 9999L, any.missing = FALSE) {
  
  if(is.numeric(x) || any.missing == TRUE) {
    validate_int(x = x, what = what, rds_tibble = rds_tibble,
                max.length = max.length, any.missing = any.missing)
  }
  else {
    x <- validate_char(x = x, what = what, rds_tibble = rds_tibble,
                  max.length = max.length, any.missing = any.missing)
  }
  
  if(!is.null(rds_tibble2)) {
    validate_assoc(x     = x,
                   what  = what,  rds_tibble  = rds_tibble,
                   what2 = what2, rds_tibble2 = rds_tibble2)
  }
  
  return(x)
}



validate_int <- function(x, what, rds_tibble, any.missing, max.length) {
  
  if(!checkmate::test_integerish(x, lower = 1, upper = 9999,
                                 any.missing = any.missing,
                                 min.len = 1, max.len = max.length)) {
    
    if(max.length == 1) {
      stop("Please enter the ", what, " id as a single integer.")
    }
    else {
      stop("Please enter the ", what, " ids as a vector of integers.")
    }
  }
  
  if(!is.null(rds_tibble)) {
    
    id_checks <-
      purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble$id)
    
    if(!all(id_checks)) {
      stop("The following ", what, " id(s) not found:",
           paste(x[!id_checks], collapse = ", "))
    }
  }
}

validate_char <- function(x, what, rds_tibble, any.missing, max.length) {
  
  if(is.null(rds_tibble)) {
    stop("id must be an integer")
  }
  
  if(!checkmate::test_character(x, min.chars = 1, any.missing = any.missing,
                                min.len = 1, max.len = max.length)) {
    stop("Entered ", what, "s must be a vector with maximum length of ",
         max.length, " with each entry having at least 1 character")
  }
  
  x <- 
    purrr::map_int(
      x,
      function(string) {
        matches <- grep(string, rds_tibble[[2]], ignore.case = TRUE)
        if(length(matches) != 1) {
          if(length(matches) == 0) {
            stop("No ", what, " found containing ", string, " in the ",
                 colnames(rds_tibble)[2])
          }
          else if(length(matches) > 1) {
            print(rds_tibble[matches, ])
            stop(string, " matches the ", colnames(rds_tibble)[2],
                 " of all of the above ", what, "s. Be more specific to ",
                 "differentiate or enter their ", what, " id numbers ",
                 "instead.")
          }
        }
        return(rds_tibble$id[matches])
      }
    )
}



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
    if(!dplyr::between(id, 1L, 9999L)) {
      stop("id must be between 1 and 9999.")
    }
    if(id %in% rds_tibble$id) {
      stop('id number already taken. Try a different one or leave the ',
           'argument blank for automatic selection ',
           '(the lowest counting number still available).')
    }
    id <- as.integer(id)
  } 
    
  return(id)
}


validate_directory <- function(path,
                               make_directories = FALSE) {
  
  if(fs::path_ext(path) != "") {
    stop("path must not have a file extension.")
  }
  
  if(!fs::dir_exists(path) && !make_directories) {
    stop("Specified path does not exist. Create it or set ",
         "make_directories = TRUE.")
  }
  
  return(unclass(path))
}