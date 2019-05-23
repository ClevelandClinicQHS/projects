
make_project_name <- function(x, short_title = FALSE) {

  if (short_title) {
    fs::path_sanitize(x)
  } else {
    paste0("p", stringr::str_pad(x, width = 4, side = "left", pad = "0"))
  }
}

make_project_path <- function(project_name, path = get_p_path()) {
  fs::path(path, project_name) %>% unclass()
}


check_all_rds <- function() {
  purrr::walk(
    c(
      "affiliations",
      "author_affiliation_assoc",
      "authors",
      "project_author_assoc",
      "projects"
    ),
    function(x) {
      print(x)
      print(
        readRDS(
          fs::path(projects_folder(), ".metadata", x, ext = "rds")
        )
      )
    }
  )
}



user_prompt <- function(msg, y_msg, n_msg, error = TRUE) {

  if (isTRUE(getOption('knitr.in.progress'))) {
    prompt <- "y"
  } else {
    prompt <- NULL
  }

  while (is.null(prompt) || !(prompt %in% c("y", "n"))) {
    if (!is.null(prompt)) {
      message("\nInvalid input.\n")
    }
    message(msg)
    prompt <- tolower(readLines(con = stdin(), n = 1L))
  }

  if (prompt == "n") {
    if (error) {
      stop(n_msg, call. = FALSE)
    } else if (!missing(n_msg)) {
      message(n_msg)
    }
  } else if (!missing(y_msg)) {
    message(y_msg)
  }

  prompt == "y"
}



clear_special_author <- function(author, projects_path, projects_table) {

  if (nrow(projects_table) > 0L) {
    is.na(projects_table[c("current_owner", "creator", "corresp_auth")]) <-
      projects_table[c("current_owner", "creator", "corresp_auth")] == author
  }

  readr::write_rds(x = projects_table, path = projects_path)

  projects_table
}



remove_archived <- function(projects_table) {
  projects_table[
    fs::path_file(fs::path_dir(projects_table$path)) != "archive",
  ]
}
