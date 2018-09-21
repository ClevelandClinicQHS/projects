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

# Used by all the new_*() and edit_*() functions
# @importFrom rlang .data
# set_rds <- function(rds_name,
#                     p_path = p_path_internal(),
#                     edit   = FALSE,
#                     ...) {
#   
#   new_rows   <- tibble::as_tibble(list(...))
#   browser()
#   rds_path   <- make_rds_path(rds_name, p_path)
#   rds_tibble <- get_rds(rds_path)
#   
#   if(ncol(new_rows) == 1) {
#     rds_tibble <- rds_tibble[!(rds_tibble[[1]] %in% new_rows[[1]])]
#   }
#   
#   ###########################################################################
#   if("id" %in% colnames(new_rows)) {
# 
#     if(is.na(new_rows$id)) {
# 
#       # If there are no projects, id will be 1.
#       # Otherwise, id will be 1 + the highest existing project id.
#       # HOWEVER: if project id 9999 is taken, id will be the
#       # lowest available id in 1:9999.
#       max_id <- max(rds_tibble$id, 0L)
#       if(max_id < 9999L) {
#         new_rows$id <- max_id + 1L
#       }
#       else if(!all(1L:9999L %in% rds_tibble$id)) {
#         new_rows$id <- min(setdiff(1L:9999L, rds_tibbleid))
#       }
#       else {
#         stop("Maximum number of ", rds_name, " reached.")
#       }
#     }
#     else {
# 
#       if(isFALSE(checkmate::test_integerish(new_rows$id,
#                                             lower = 1L, upper = 9999L,
#                                             any.missing = FALSE, len = 1L))) {
#         stop("id must be an integer")
#       }
# 
#       new_rows$id <- as.integer(new_rows$id)
# 
#       if(new_rows$id %in% rds_tibble$id) {
#         if(edit) {
#           new_rows <-
#             new_rows %>%
#             tidyr::replace_na(
#               rds_tibble %>% dplyr::filter(.data$id == new_rows$id))
# 
#           rds_tibble <-
#             rds_tibble %>% dplyr::filter(.data$id != new_rows$id)
#         }
#         else {
#           stop('id already taken. Try a different one or leave the ',
#                'argument blank for automatic selection.')
#         }
#       }
#       else if(edit) {
#         stop('Not found. Double-check item exists with ', rds_name, '() or ',
#              'create the record')
#       }
#     }
#   }
#   else {
#     if(edit){
#       rds_tibble <- rds_tibble[rds_tibble[[1]] != new_rows[[1,1]]]
#     }
#     if(delete)
#   }
#   #
#   ###########################################################################
#   
#   rds_tibble <- dplyr::bind_rows(new_rows, rds_tibble)
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   # This returns all rows in which the first id column's ids match its first
#   # value.
#   # In effect, this returns all rows associated with the newly created item
#   return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
# }


#' @importFrom tibble tibble
new_rds_item <- function(rds_name,
                         p_path = p_path_internal(),
                         ...) {
  
  new_item   <- tibble(...)
  
  rds_path   <- make_rds_path(rds_name, p_path)
  rds_tibble <- get_rds(rds_path)
  
  if(all(is.na(changes[[1]]))) {
    
    changes[[1]] <- max(rds_tibble$id, 0L) + 1L
    
    if(changes[[1]] > 9999) {
      if(!all(1:9999 %in% rds_tibble$id)) {
        changes[[1]] <- min(setdiff(1:9999, rds_tibble$id))
      }
      else {
        stop("Maximum number of ", rds_name, " reached.")
      }
    }
  }
  else {
    if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
                                   any.missing = FALSE, len = 1L)) {
      stop("id must be an integer")
    }
    changes[[1]] <- as.integer(changes[[1]])
    
    if(any(changes[[1]] %in% rds_tibble[[1]])) {
      stop('id already taken. Try a different one or leave the argument ',
           'blank for automatic selection.')
    }
  }
  
  rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
  
  saveRDS(rds_tibble, rds_path)
  
  return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
}




#' @importFrom rlang .data
#' @importFrom tibble tibble
edit_rds_item <- function(rds_name,
                          p_path = p_path_internal(),
                          ...) {
  changes    <- tibble(...)
  
  rds_path   <- make_rds_path(rds_name, p_path)
  rds_tibble <- get_rds(rds_path)
  
  if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
                                 any.missing = FALSE, len = 1L)) {
    stop("id must be an integer")
  }
  changes[[1]] <- as.integer(changes[[1]])
  
  if(!all(changes[[1]] %in% rds_tibble$id)) {
    stop('id not found. Double-check item(s) exist using ', rds_name,
         '() or create the record')
  }
  
  changes <-
    tidyr::replace_na(
      data    = changes,
      replace = rds_tibble %>% dplyr::filter(.data$id == changes[[1]]))
  
  rds_tibble <-
    dplyr::bind_rows(changes,
                     rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])])
  
  saveRDS(rds_tibble, rds_path)
  
  return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
}


#' @importFrom tibble tibble
delete_rds_item <- function(rds_name,
                            p_path = p_path_internal(),
                            ...) {
  changes    <- tibble(...)
  
  rds_path   <- make_rds_path(rds_name, p_path)
  rds_tibble <- get_rds(rds_path)
  
  if(!checkmate::test_integerish(changes[[1]], lower = 1L, upper = 9999L,
                                 any.missing = FALSE, len = 1L)) {
    stop("id must be an integer")
  }
  changes[[1]] <- as.integer(changes[[1]])
  
  if(!all(changes[[1]] %in% rds_tibble$id)) {
    stop('id not found. Double-check item(s) exist using ', rds_name,
         '() or create the record')
  }
  
  deleted_items <- rds_tibble[rds_tibble[[1]] %in% changes[[1]]]
  
  rds_tibble <- rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])]
  
  saveRDS(rds_tibble, rds_path)
  
  return(deleted_items)
}

# Used by all the new_*() and edit_*() functions
# @importFrom rlang .data
# set_rds <- function(rds_name,
#                     p_path = p_path_internal(),
#                     new,
#                     edit,
#                     ...) {
#   
#   changes    <- tibble::as_tibble(list(...))
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
#     id_match_vector <- changes[[1]] %in% rds_tibble$id
#     if(!new) {
#       if(!all(id_match_vector)) {
#         stop('At least one Not found. Double-check item(s) exist using ',
#              rds_name, '() or create the record')
#       }
#       if(edit) {
#         changes <-
#           tidyr::replace_na(
#             data    = changes,
#             replace = rds_tibble %>% dplyr::filter(.data$id == changes[[1]]))
#       }
#       else {
#         deleted_items <- rds_tibble[rds_tibble[[1]] %in% changes[[1]]]
#       }
#       rds_tibble <- rds_tibble[!(rds_tibble[[1]] %in% changes[[1]])]
#     }
#     else {
#       if(any(id_match_vector)) {
#         stop('id already taken. Try a different one or leave the argument ',
#              'blank for automatic selection.')
#       }
#     }
#   }
#   
#   if(new || edit) {
#     rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
#   }
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   # This returns all rows in which the first id column's ids match its first
#   # value.
#   # In effect, this returns all rows associated with the newly created item
#   
#   if(new || edit) {
#     return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
#   }
#   else {
#     return(deleted_items)
#   }
# }