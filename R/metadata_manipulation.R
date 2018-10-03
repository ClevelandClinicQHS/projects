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
#' @importFrom tibble tibble
change_table <- function(rds_name,
                         p_path,
                         rds_path,
                         rds_tibble,
                         action,
                         ...) {
  
  changes    <- tibble(...)
  
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
      if(!(changes$id %in% rds_tibble$id)) {
        stop('id not found. Double-check item exists using ',
             rds_name, '() or create the record')
      }
      if(action == "edit") {
        changes <-
          tidyr::replace_na(
            data    = changes,
            replace = rds_tibble %>% dplyr::filter(.data$id == changes$id))
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
  
  if(action == "delete") {
    return(deleted_item)
  }
  else {
    return(changes)
  }
}



################################################################################
#' @importFrom tibble tibble
#' @importFrom rlang .data
change_assoc <- function(assoc_name,
                         p_path = p_path_internal(),
                         new,
                         ...) {
  
  assoc_change  <- tibble(...)
  
  # integerish_test <-
  #   purrr::map_lgl(assoc_change,
  #                  checkmate::test_integerish,
  #                  lower = 1, upper = 9999, any.missing = FALSE, min.len = 1)
  # 
  # if(!all(integerish_test)) {
  #   stop("For all additions and removals of affiliations, PIs, and ",
  #        "investigators, enter a vector of id numbers.")
  # }
  
  rds_path      <- make_rds_path(assoc_name, p_path)
  assoc_table   <- get_rds(rds_path)
  
  if(new) {
    assoc_table <- dplyr::bind_rows(assoc_change, assoc_table)
    
    if(anyDuplicated(assoc_table) > 0) {
      stop("At least one of these associations already exists")
    }
  }
  
  else {
    assoc_table <- suppressMessages(dplyr::anti_join(assoc_table, assoc_change))
  }
  
  saveRDS(assoc_table, rds_path)
  
  #if(new) {
    #assoc_table[assoc_table[[1]] == assoc_change[[1,1]],]
    return(assoc_table %>% dplyr::filter(.data$id1 == assoc_change[[1,1]]))
  # }
  # else {
  #   return(assoc_table)
  # }
}


################################################################################
validate_entry <- function(x,
                           what,
                           max.length  = 9999L,
                           any.missing = FALSE,
                           rds_tibble) {
  
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
        stop("The following ", what, " id(s) not found: ",
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
           max.length, " with each entry having at least 1")
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
