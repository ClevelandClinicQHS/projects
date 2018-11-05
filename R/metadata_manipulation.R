################################################################################
################################################################################
# RDS helper functions

make_rds_path <- function(rds_name, p_path = p_path_internal()) {
  fs::path(p_path, "metadata", rds_name, ext = "rds")
}

get_rds <- function(rds_path) {
  
  if(!fs::file_exists(rds_path)) {
    stop(fs::path_file(rds_path), " file not found at ", rds_path,
         ". Please restore the file or [re]run setup_projects_folder()")
  }
  
  readRDS(rds_path)
}


################################################################################
# Used by all the new_*(), edit_*(), and delete_*() functions
#' @importFrom rlang .data
change_table <- function(rds_name,
                         p_path = p_path_internal(),
                         rds_path,
                         rds_tibble,
                         action,
                         ...) {
  
  changes    <- list(...)
  
  if(missing(rds_path) || missing(rds_tibble)) {
    rds_path   <- make_rds_path(rds_name, p_path)
    rds_tibble <- get_rds(rds_path)
  }
  
  if(is.na(changes$id)) {
    
    changes$id <- max(rds_tibble$id, 0L) + 1L
    
    if(changes$id > 9999) {
      if(!all(1:9999 %in% rds_tibble$id)) {
        changes$id <- min(setdiff(1L:9999L, rds_tibble$id))
      }
      else {
        stop("Maximum number of ", rds_name, " reached.")
      }
    }
  }
  else {
    changes$id <- as.integer(changes$id)
    
    if(action == "new") {
      if(changes$id %in% rds_tibble$id) {
        stop('id already taken. Try a different one or leave the argument ',
             'blank for automatic selection.')
      }
    }
    else {
      # if(!(changes$id %in% rds_tibble$id)) {
      #   stop('id not found. Double-check item exists using ',
      #        rds_name, '() or create the record')
      # }
      if(action == "edit") {
        changes <-
          purrr::map2(
            .x = changes,
            .y = as.list(dplyr::filter(rds_tibble, .data$id == changes$id)),
            .f = function(x, y) {
              if(is.null(x)) {
                return(NA)
              }
              if(is.na(x)) {
                return(y)
              }
              return(x)
            })
      }
      else {
        deleted_item <- rds_tibble %>% dplyr::filter(.data$id == changes$id)
      }
      rds_tibble <- rds_tibble %>% dplyr::filter(.data$id != changes$id)
    }
  }
  
  if(action != "delete") {
    rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
  }
  
  saveRDS(rds_tibble, rds_path)
  
  # if(action == "delete") {
  #   return(deleted_item)
  # }
  # else {
    return(changes)
  # }
}



################################################################################
#' @importFrom tibble tibble
#' @importFrom rlang .data
change_assoc <- function(assoc_path,
                         assoc_tibble = get_rds(assoc_path),
                         new,
                         ...) {
  
  assoc_change   <- tibble(...)
  
  if(new) {
    assoc_tibble <- dplyr::bind_rows(assoc_change, assoc_tibble)
  }
  else {
    assoc_tibble <- suppressMessages(dplyr::anti_join(assoc_tibble,
                                                      assoc_change))
  }
  
  saveRDS(assoc_tibble, assoc_path)
  
  return(assoc_tibble)
}


################################################################################
validate_entry <- function(x,
                           what,
                           max.length       = 9999L,
                           any.missing      = FALSE,
                           rds_tibble,
                           message_addendum = "") {
  
  if(is.numeric(x) || any.missing == TRUE) {
    
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
    
    if(!missing(rds_tibble)) {
      
      id_checks <-
        purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble$id)
      
      if(!all(id_checks)) {
        stop("The following ", what, " id(s) not found", message_addendum, ":",
             paste(x[!id_checks], collapse = ", "))
      }
    }
    return(x)
  }
  else {
    
    if(missing(rds_tibble)) {
      stop("id must be an integer")
    }
    
    if(!checkmate::test_character(x, min.chars = 1, any.missing = any.missing,
                                  min.len = 1, max.len = max.length)) {
      stop("Entered ", what, "s must be a vector with maximum length of ",
           max.length, " with each entry having at least 1 character")
    }
    
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
                 "differentiate or enter their ", what, " id numbers instead.")
          }
        }
        return(rds_tibble$id[matches])
      }
    )
  }
}


################################################################################
#' @export
set_default_author <- function(author, overwrite = FALSE) {
  
  old_default_author   <- Sys.getenv("PROJECTS_DEFAULT_AUTHOR")
  home_Renviron_path <- fs::path(Sys.getenv("HOME"), ".Renviron")
  
  # If overwite == TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_DEFAULT_AUTHOR in the home .Renviron file.
  #
  # If overwrite == FALSE, function will still run UNLESS a
  # PROJECTS_DEFAULT_AUTHOR value already exists and does not match up with the
  # user-specified path.
  if(!overwrite && old_default_author != "" && old_default_author != author) {
    stop('An .Renviron file (probably at ', home_Renviron_path,
         ') indicates that the default author was already set to ',
         old_default_author, '. To change, set overwrite = TRUE')
  }
  
  authors_tibble <- authors()
  
  author <- validate_entry(author,
                           what       = "author",
                           max.length = 1,
                           rds_tibble = authors_tibble)
  
  home_Renviron_file <- paste0("PROJECTS_DEFAULT_AUTHOR='", author, "'")
  
  # If a home .Renviron file already exists, it is overwritten with its original
  # contents, minus any old values of PROJECTS_DEFAULT_AUTHOR, plus the new value
  # of PROJECTS_DEFAULT_AUTHOR (i.e., the user-specified path, which could
  # actually be the same as the old value).
  if(fs::file_exists(home_Renviron_path)) {
    old_home_Renviron  <- readr::read_lines(home_Renviron_path)
    home_Renviron_file <-
      append(
        old_home_Renviron[!grepl("PROJECTS_DEFAULT_AUTHOR",
                                 old_home_Renviron)],
        # Contents of the old .Renviron file, excluding any pre-existing
        # PROJECTS_DEFAULT_AUTHOR value
        
        home_Renviron_file
      )
  }
  readr::write_lines(home_Renviron_file, path = home_Renviron_path)
  
  readRenviron(home_Renviron_path)
  
  message("Default author set to:")
  return(authors_tibble[authors_tibble$id == author,])
}