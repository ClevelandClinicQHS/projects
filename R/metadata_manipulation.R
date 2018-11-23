################################################################################
################################################################################
# RDS helper functions

make_rds_path <- function(rds_name, p_path = p_path_internal()) {
  fs::path(p_path, ".metadata", rds_name, ext = "rds")
}

get_rds <- function(rds_path) {

  if(!fs::file_exists(rds_path)) {
    stop(fs::path_file(rds_path), " file not found at ", rds_path,
         ". Please restore the file or [re]run setup_projects()")
  }

  readRDS(rds_path)
}


################################################################################
# Used by all the new_*(), edit_*(), and delete_*() functions
#' @importFrom rlang .data
change_table <- function(action = c("new", "edit", "delete"),
                         rds_path,
                         rds_tibble,
                         ...) {

  action     <- match.arg(action)
  changes    <- list(...)

  if(action == "edit") {
    changes <-
      purrr::map2(
        .x = changes,
        .y = as.list(dplyr::filter(rds_tibble, .data$id == changes$id)),
        .f = function(new, old) {
          if(is.null(new)) {
            return(methods::as(NA, class(old)))
          }
          if(is.na(new)) {
            return(old)
          }
          return(new)
        })
  }

  rds_tibble <- dplyr::filter(rds_tibble, .data$id != changes$id)

  if(action != "delete") {
    rds_tibble <- dplyr::bind_rows(changes, rds_tibble)
    changes    <- tibble::as_tibble(changes)
  }

  saveRDS(rds_tibble, rds_path)

  return(changes)
}



################################################################################
#' @importFrom tibble tibble
change_assoc <- function(assoc_path,
                         assoc_tibble,
                         new,
                         ...) {

  assoc_change   <- tibble(...)

  if(new) {
    old_assoc <- dplyr::semi_join(assoc_tibble, assoc_change, by = "id1")

    # The use of head() and tail() ensures that new authors will be inserted
    # right before the last element on the list unless there had been only one
    # element in the list--in that case, elements are added to the end of the
    # list.
    assoc_tibble <- dplyr::bind_rows(
      utils::head(old_assoc, n = 1),
      utils::head(utils::tail(old_assoc, n = -1), n = -1),
      assoc_change,
      utils::tail(utils::tail(old_assoc, n = -1), n =  1),
      dplyr::anti_join(assoc_tibble, assoc_change, by = "id1"))
  }
  else {
    assoc_tibble <- suppressMessages(dplyr::anti_join(assoc_tibble,
                                                      assoc_change))
  }

  saveRDS(assoc_tibble, assoc_path)

  return(assoc_tibble)
}
