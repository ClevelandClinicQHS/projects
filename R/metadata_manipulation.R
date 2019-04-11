
make_rds_path <- function(rds_name, p_path = get_p_path()) {

  fs::path(p_path, ".metadata", rds_name, ext = "rds")
}



get_rds <- function(rds_path, check = TRUE) {

  if (!fs::file_exists(rds_path)) {
    stop(fs::path_file(rds_path), " file not found at ", rds_path,
         ". Please restore the file or [re]run setup_projects()")
  }

  # rds <- readRDS(rds_path)
  #
  # if (check && !up_to_date(rds)) {
  #   update_notice()
  #   rds <- readRDS(rds_path)
  # }

  readRDS(rds_path)
}



projects_internal <- function(p_path = get_p_path(), archived = TRUE) {

  projects_table <- get_rds(make_rds_path("projects", p_path))

  if (archived) {
    projects_table
  } else {
    remove_archived(projects_table)
  }
}

authors_internal <- function(p_path = get_p_path()) {
  get_rds(make_rds_path("authors", p_path))
}

affiliations_internal <- function(p_path = get_p_path()) {
  get_rds(make_rds_path("affiliations", p_path))
}

pa_assoc_internal <- function(p_path = get_p_path()) {
  get_rds(make_rds_path("project_author_assoc", p_path))
}

aa_assoc_internal <- function(p_path = get_p_path()) {
  get_rds(make_rds_path("author_affiliation_assoc", p_path))
}



write_metadata <- function(table, table_path) {

  # attr(table, "projects_version") <- utils::packageVersion("projects")

  readr::write_rds(x = table, path = table_path)
}


################################################################################
# Used by all the new_*(), edit_*(), and delete_*() functions
# @importFrom rlang .data
# change_table <- function(action = c("new", "edit", "delete"),
#                          rds_path,
#                          rds_table,
#                          ...) {
#
#   action     <- match.arg(action)
#   changes    <- list(...)
#
#   if (action == "edit") {
#     changes <-
#       purrr::map2(
#         .x = changes,
#         .y = as.list(dplyr::filter(rds_table, .data$id == changes$id)),
#         .f = function(new, old) {
#           if (is.null(new)) {
#             methods::as(NA, class(old))
#           } else if (is.na(new)) {
#             old
#           } else {
#             new
#           }
#         }
#       )
#   }
#
#   rds_table <- dplyr::filter(rds_table, .data$id != changes$id)
#
#   if (action != "delete") {
#     changes   <- tibble::as_tibble(changes)
#     rds_table <- rbind(changes, rds_table)
#   }
#
#   # if (!is.null(rds_table$stage) && !inherits(rds_table$stage,
#   #                                            "projects_stage")) {
#   #   class(rds_table$stage) <- "projects_stage"
#   # }
#
#   write_metadata(table = rds_table, table_path = rds_path)
#
#   changes
# }



add_metadata <- function(table, new_row, table_path) {

  table <- rbind(table, new_row)

  write_metadata(
    table      = table,
    table_path = table_path
  )

  table[nrow(table), ]
}


edit_metadata <- function(table, row_id, ..., table_path) {

  changes <- list(...)

  row_number <- match(row_id, table$id)

  purrr::iwalk(
    changes,
    function(change, name) {
      if (!is.null(change)) table[row_number, name] <<- change
    }
  )

  write_metadata(
    table      = table,
    table_path = table_path
  )

  table[row_number, ]
}



delete_metadata <- function(table, row_id, table_path) {

  table <- table[table$id != row_id, ]

  write_metadata(
    table      = table,
    table_path = table_path
  )
}



add_assoc <- function(assoc_table, new_rows, assoc_path) {

  assoc_table <- rbind(assoc_table, new_rows)

  write_metadata(
    table      = assoc_table,
    table_path = assoc_path
  )

  assoc_table
}



delete_assoc <- function(assoc_table, ..., assoc_path) {

  assoc_to_delete <- tibble::tibble(...)

  assoc_table <-
    suppressMessages(
      dplyr::anti_join(assoc_table, assoc_to_delete)
    )

  write_metadata(
    table      = assoc_table,
    table_path = assoc_path
  )

  assoc_table
}



################################################################################
# change_assoc <- function(assoc_path,
#                          assoc_table,
#                          new,
#                          ...) {
#
#   assoc_change   <- tibble::tibble(...)
#
#   if (new) {
#     old_assoc <- dplyr::semi_join(assoc_table, assoc_change, by = "id1")
#
#     # The use of head() and tail() ensures that new authors will be inserted
#     # right before the last element on the list unless there had been only one
#     # element in the list--in that case, elements are added to the end of the
#     # list.
#     assoc_table <-
#       rbind(
#         utils::head(old_assoc, n = 1),
#         utils::head(utils::tail(old_assoc, n = -1), n = -1),
#         assoc_change,
#         utils::tail(utils::tail(old_assoc, n = -1), n =  1),
#         dplyr::anti_join(assoc_table, assoc_change, by = "id1")
#       )
#   } else {
#     assoc_table <- suppressMessages(dplyr::anti_join(assoc_table, assoc_change))
#   }
#
#   write_metadata(table = assoc_table, table_path = assoc_path)
#
#   assoc_table
# }
