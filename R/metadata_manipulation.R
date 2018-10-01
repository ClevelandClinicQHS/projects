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
                         p_path = p_path_internal(),
                         action,
                         ...) {
  
  changes    <- tibble(...)
  
  rds_path   <- make_rds_path(rds_name, p_path)
  rds_tibble <- get_rds(rds_path)
  
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
    assoc_table <- dplyr::anti_join(assoc_table, assoc_change)
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

test_id_entry <- function(id, what, max.length = 9999, any.missing = FALSE,
                          set = NULL) {
  
  if(!checkmate::test_integerish(id, lower = 1, upper = 9999,
                                 any.missing = any.missing,
                                 min.len = 1, max.len = max.length)) {
    
    if(max.length == 1) {
      stop("Please enter the ", what, " id as a single integer.")
    }
    else {
      stop("Please enter the ", what, " ids as a vector of integers.")
    }
  }
  
  if(!is.null(set)) {
    
    id_checks <- purrr::map_lgl(id, checkmate::test_choice, choices = set)
    
    if(!all(id_checks)) {
      stop("The following ", what, "(s) not found: ",
           paste(id[!id_checks], collapse = ", "))
    }
  }
}
