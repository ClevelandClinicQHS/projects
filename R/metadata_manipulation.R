################################################################################
################################################################################
# RDS helper functions

make_rds_path <- function(rds_name, p_path = p_path_internal()) {
  fs::path(p_path, ".metadata", rds_name, ext = "rds")
}

get_rds <- function(rds_path) {

  if(!fs::file_exists(rds_path)) {
    stop(fs::path_file(rds_path), " file not found at ", rds_path,
         ". Please restore the file or [re]run initialize()")
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

  if(isTRUE(changes$default) && nrow(rds_tibble) > 0) {
    rds_tibble$default <- FALSE
  }

  if(action == "edit") {
    changes <-
      purrr::map2(
        .x = changes,
        .y = as.list(dplyr::filter(rds_tibble, .data$id == changes$id)),
        .f = function(new, old) {
          if(is.null(new)) {
            return(as(NA, class(old)))
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
      head(old_assoc, n = 1),
      head(tail(old_assoc, n = -1), n = -1),
      assoc_change,
      tail(tail(old_assoc, n = -1), n =  1),
      dplyr::anti_join(assoc_tibble, assoc_change, by = "id1"))
  }
  else {
    assoc_tibble <- suppressMessages(dplyr::anti_join(assoc_tibble,
                                                      assoc_change))
  }

  saveRDS(assoc_tibble, assoc_path)

  return(assoc_tibble)
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
