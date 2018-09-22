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
  
  if(any(is.na(changes[[1]]))) {
    
    changes[[1]] <- max(rds_tibble$id, 0L) + 1L
    
    if(changes[[1]] > 9999) {
      if(!all(1:9999 %in% rds_tibble$id)) {
        changes[[1]] <- min(setdiff(1L:9999L, rds_tibble$id))
      }
      else {
        stop("Maximum number of ", rds_name, " reached.")
      }
    }
  }
  else {
    if(!checkmate::test_integerish(changes[[1]], lower = 1, upper = 9999,
                                   any.missing = FALSE, len = 1)) {
      stop("id must be an integer")
    }
    changes[[1]] <- as.integer(changes[[1]])
    
    id_match_vector <- changes[[1]] %in% rds_tibble$id
    if(action == "new") {
      if(any(id_match_vector)) {
        stop('id already taken. Try a different one or leave the argument ',
             'blank for automatic selection.')
      }
    }
    else {
      if(!all(id_match_vector)) {
        stop('At least one Not found. Double-check item(s) exist using ',
             rds_name, '() or create the record')
      }
      if(action == "edit") {
        changes <-
          tidyr::replace_na(
            data    = changes,
            replace = rds_tibble %>% dplyr::filter(.data$id == changes[[1]]))
      }
      else {
        deleted_items <- rds_tibble[rds_tibble[[1]] %in% changes[[1]]]
      }
      rds_tibble <- rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])]
    }
  }
  
  if(action != "delete") {
    rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
  }
  
  saveRDS(rds_tibble, rds_path)
  
  # This returns all rows in which the first id column's ids match its first
  # value.
  # In effect, this returns all rows associated with the newly created item
  
  if(action == "delete") {
    return(deleted_items)
  }
  else {
    return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
  }
}



################################################################################
#' @importFrom tibble tibble
change_assoc <- function(assoc_name,
                         p_path = p_path_internal(),
                         new,
                         ...) {
  
  assoc_change  <- tibble(...)
  
  rds_path    <- make_rds_path(assoc_name, p_path)
  assoc_table <- get_rds(rds_path)
  
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
  
  assoc_table[assoc_table[[1]] == assoc_change[[1,1]],]
}



# # @importFrom tibble tibble
# new_rds_item <- function(rds_name,
#                          p_path = p_path_internal(),
#                          ...) {
#   
#   new_item   <- tibble(...)
#   
#   rds_path   <- make_rds_path(rds_name, p_path)
#   rds_tibble <- get_rds(rds_path)
#   
#   if(all(is.na(changes[[1]]))) {
#     
#     changes[[1]] <- max(rds_tibble$id, 0L) + 1L
#     
#     if(changes[[1]] > 9999) {
#       if(!all(1:9999 %in% rds_tibble$id)) {
#         changes[[1]] <- min(setdiff(1:9999, rds_tibble$id))
#       }
#       else {
#         stop("Maximum number of ", rds_name, " reached.")
#       }
#     }
#   }
#   else {
#     if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
#                                    any.missing = FALSE, len = 1L)) {
#       stop("id must be an integer")
#     }
#     changes[[1]] <- as.integer(changes[[1]])
#     
#     if(any(changes[[1]] %in% rds_tibble[[1]])) {
#       stop('id already taken. Try a different one or leave the argument ',
#            'blank for automatic selection.')
#     }
#   }
#   
#   rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
# }
# 
# 
# 
# 
# # @importFrom rlang .data
# # @importFrom tibble tibble
# edit_rds_item <- function(rds_name,
#                           p_path = p_path_internal(),
#                           ...) {
#   changes    <- tibble(...)
#   
#   rds_path   <- make_rds_path(rds_name, p_path)
#   rds_tibble <- get_rds(rds_path)
#   
#   if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
#                                  any.missing = FALSE, len = 1L)) {
#     stop("id must be an integer")
#   }
#   changes[[1]] <- as.integer(changes[[1]])
#   
#   if(!all(changes[[1]] %in% rds_tibble$id)) {
#     stop('id not found. Double-check item(s) exist using ', rds_name,
#          '() or create the record')
#   }
#   
#   changes <-
#     tidyr::replace_na(
#       data    = changes,
#       replace = rds_tibble %>% dplyr::filter(.data$id == changes[[1]]))
#   
#   rds_tibble <-
#     dplyr::bind_rows(changes,
#                      rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])])
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
# }
# 
# 
# # @importFrom tibble tibble
# delete_rds_item <- function(rds_name,
#                             p_path = p_path_internal(),
#                             ...) {
#   changes    <- tibble(...)
#   
#   rds_path   <- make_rds_path(rds_name, p_path)
#   rds_tibble <- get_rds(rds_path)
#   
#   if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
#                                  any.missing = FALSE, len = 1L)) {
#     stop("id must be an integer")
#   }
#   changes[[1]] <- as.integer(changes[[1]])
#   
#   if(!all(changes[[1]] %in% rds_tibble$id)) {
#     stop('id not found. Double-check item(s) exist using ', rds_name,
#          '() or create the record')
#   }
#   
#   deleted_items <- rds_tibble[  rds_tibble[[1]] %in% changes[[1]] ]
#   
#   rds_tibble    <- rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])]
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   return(deleted_items)
# }
