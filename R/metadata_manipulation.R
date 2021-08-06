
make_rds_path <- function(rds_name, p_path = get_p_path()) {
  fs::path(p_path, ".metadata", rds_name, ext = "rds")
}



get_rds <- function(rds_path) {

  if (!fs::file_exists(rds_path)) {

    what <- fs::path_ext_remove(fs::path_file(rds_path))

    user_prompt(
      msg =
        paste0("\nThe ", what, " table was not found at\n", rds_path,
               "\n\nContinue with a blank ", what, " table?"),
      n_msg =
        paste0("\n\nRestore the ", what, " table to\n", rds_path,
               "\n\nOr, [re]run setup_projects()",
               "\n\nOr, just put 'y' next time.")
    )
    switch(
      what,
      projects = projects_ptype,
      authors = authors_ptype,
      affiliations = affiliations_ptype,
      tasks = tasks_ptype,
      assoc_ptype
    )
  } else {
    readRDS(rds_path)
  }
}



save_metadata <- function(x, path, .ptype) {
  readr::write_rds(vec_cast(x, .ptype), path)
}



projects_internal <- function(p_path = get_p_path(), archived = TRUE) {

  projects_table <- get_rds(make_rds_path("projects", p_path))

  if (archived) {
    projects_table
  } else {
    remove_archived(projects_table)
  }
}

tasks_internal <- function(p_path = get_p_path()) {
  get_rds(make_rds_path("tasks", p_path))
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



add_metadata <- function(table,
                         new_row,
                         table_path,
                         .ptype,
                         task = FALSE) {

  if (task) {
    table <- table %>%
      vec_rbind(new_row, .ptype = dplyr::mutate(.ptype, TID = double())) %>%
      sort_project_tasks(PID = new_row$PID)
  } else {
    table <- vec_rbind(table, new_row)
  }

  save_metadata(table, table_path, .ptype)

  table
}


edit_metadata <- function(table, row_spec_lgl, table_path, .ptype, ...) {

  changes <- list(...)

  purrr::iwalk(
    changes,
    function(new_value, colname) {
      if (!is.null(new_value)) {
        table[row_spec_lgl, colname] <<- new_value
      }
    }
  )

  if (!is.null(changes$TID)) {
    table <-
      sort_project_tasks(
        table,
        PID = table$PID[row_spec_lgl],
        tiebreaker = !row_spec_lgl
      )
  }

  save_metadata(table, table_path, .ptype)

  table
}



add_assoc <- function(assoc_table, new_rows, assoc_path) {

  assoc_table <- vec_rbind(assoc_table, new_rows, .ptype = assoc_ptype)

  save_metadata(assoc_table, assoc_path, assoc_ptype)

  assoc_table
}



delete_assoc <- function(assoc_table, ..., assoc_path) {

  assoc_to_delete <- tibble::tibble(...)

  assoc_table <-
    suppressMessages(
      dplyr::anti_join(assoc_table, assoc_to_delete)
    )

  save_metadata(assoc_table, assoc_path, assoc_ptype)

  assoc_table
}



change_special_author <- function(author_id,
                                  new_value,
                                  table,
                                  table_path,
                                  ptype) {
  if (nrow(table)) {
    special_author_cols <-
      table %>%
      dplyr::select(where(is_projects_author)) %>%
      names()
    change_matrix <- table[special_author_cols] == author_id
    if (isTRUE(any(change_matrix))) {
      table[special_author_cols][change_matrix] <- new_value
      save_metadata(table, table_path, ptype)
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
