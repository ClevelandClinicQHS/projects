

make_project_path <- function(project_name, path = get_p_path()) {
  unclass(fs::path(path, project_name))
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

  prompt <- if (isTRUE(getOption('knitr.in.progress'))) "y"

  while (is.null(prompt) || !any(c("y", "n") == prompt)) {
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
