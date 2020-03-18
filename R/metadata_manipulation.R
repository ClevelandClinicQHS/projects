
make_rds_path <- function(rds_name, p_path = get_p_path()) {
  fs::path(p_path, ".metadata", rds_name, ext = "rds")
}



get_rds <- function(rds_path, check = TRUE) {

  if (!fs::file_exists(rds_path)) {
    stop(
      fs::path_file(rds_path), " file not found at ", rds_path,
      ". Please restore the file or [re]run setup_projects()"
    )
  }

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



add_metadata <- function(table, new_row, table_path) {

  table <- vec_rbind(table, new_row)

  readr::write_rds(x = table, path = table_path)

  table[nrow(table), ]
}


edit_metadata <- function(table, row_id, ..., table_path) {

  changes <- list(...)

  row_number <- match(row_id, table$id)

  purrr::iwalk(
    changes,
    function(change, name) {
      if (!is.null(change)) {
        table[row_number, name] <<- change
      }
    }
  )

  readr::write_rds(x = table, path = table_path)

  table[row_number, ]
}



delete_metadata <- function(table, row_id, table_path) {

  table <- table[table$id != row_id, ]

  readr::write_rds(x = table, path = table_path)
}



add_assoc <- function(assoc_table, new_rows, assoc_path) {

  assoc_table <- rbind(assoc_table, new_rows)

  readr::write_rds(x = assoc_table, path = assoc_path)

  assoc_table
}



delete_assoc <- function(assoc_table, ..., assoc_path) {

  assoc_to_delete <- tibble::tibble(...)

  assoc_table <-
    suppressMessages(
      dplyr::anti_join(assoc_table, assoc_to_delete)
    )

  readr::write_rds(x = assoc_table, path = assoc_path)

  assoc_table
}



change_special_author <- function(author_id,
                                  new_value,
                                  projects_path,
                                  projects_table) {
  if (nrow(projects_table) > 0L) {
    special_author_cols <- c("current_owner", "creator", "corresp_auth")
    change_matrix <- projects_table[special_author_cols] == author_id
    if (isTRUE(any(change_matrix))) {
      projects_table[special_author_cols][change_matrix] <- new_value
      readr::write_rds(x = projects_table, path = projects_path)
    }
  }
}


#' @importFrom rlang .data
remove_archived <- function(projects_table) {
  dplyr::filter(
    projects_table,
    fs::path_file(fs::path_dir(.data$path)) != "archive"
  )
}
