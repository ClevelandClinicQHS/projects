
#' Update the project metadata
#'
#' Safely updates existing project metadata to be compatible with
#' \code{\link[=projects-package]{projects}} 1.X.X.
#'
#' Prior to \code{\link[=projects-package]{projects}} 1.X.X, the \code{stage},
#' \code{current_owner}, \code{corresp_auth}, and \code{creator} columns of the
#' \code{\link{projects}()} table were different.
#'
#' The \code{stage} column was a \link{factor}, and users had to type stage
#' names exactly, down to the integer, colon, and space. Now, this column is of
#' class \code{\link{projects_stage}}.
#'
#' The latter three columns were integers corresponding to \code{id}s in the
#' \code{\link{authors}()} table, so users would have to query that table if
#' they did not remember which author was denoted by the integer \code{id}.
#'
#' @param ask Logical, indicating whether or not the user would be asked at the
#'   command line whether or not to proceed. Defaults to \code{TRUE}.
#'
#' @seealso \code{\link{projects_stage}}; \code{\link{projects_author}}.
#'
#' @importFrom rlang .data
#' @export
update_metadata <- function(ask = TRUE) {

  if (ask) {
    user_prompt(
      "\nUpdate the project metadata? (y/n)",
      n_msg = "Answer with 'y' next time in order to proceed.",
      error = TRUE
    )
  }

  p_path           <- get_p_path()

  projects_path    <- make_rds_path("projects", p_path)

  projects_table   <- readRDS(projects_path)

  authors_path     <- make_rds_path("authors", p_path)

  authors_table    <- readRDS(authors_path)

  pa_assoc_path    <- make_rds_path("project_author_assoc", p_path)

  pa_assoc         <- readRDS(pa_assoc_path)

  if (nrow(projects_table) == 0) {
    projects_table <-
      tibble::tibble(
        id            = integer(),
        title         = character(),
        short_title   = character(),
        current_owner = new_projects_author(),
        status        = character(),
        deadline_type = character(),
        deadline      = as.POSIXct(character()),
        stage         = new_projects_stage(),
        path          = character(),
        corresp_auth  = new_projects_author(),
        creator       = new_projects_author()
      )
  } else {

    projects_table$stage <- validate_stage_column(projects_table)

    current_owner_results <-
      validate_sa_column(
        projects_table,
        "current_owner",
        authors_table,
        pa_assoc,
        replacements =
          tibble::tibble(old = character(), new = new_projects_author())
      )

    projects_table$current_owner <- current_owner_results$results

    corresp_auth_results <-
      validate_sa_column(
        projects_table,
        "corresp_auth",
        authors_table,
        pa_assoc,
        replacements = current_owner_results$replacements
      )

    projects_table$corresp_auth <- corresp_auth_results$results

    creator_results <-
      validate_sa_column(
        projects_table,
        "creator",
        authors_table,
        pa_assoc,
        replacements = corresp_auth_results$replacements
      )

    projects_table$creator <- creator_results$results
  }

  readr::write_rds(x = projects_table, path = projects_path)

  message("\nProjects metadata updated.")

  invisible(projects_table)
}



validate_stage_column <- function(projects_table) {

  replacements <- tibble::tibble(old = character(), new = new_projects_stage())

  purrr::imap(
    projects_table$stage,
    function(x, row) {

      match_attempt <- match(x, replacements$old)

      if (is.na(match_attempt)) {

        attempt <- try(validate_stage(x), silent = TRUE)

        if (inherits(attempt, "try-error")) {

          while (inherits(attempt, "try-error")) {
            print(projects_table[row, c("id", "stage", "title")])
            message(
              "\nThe stage of the above project could not be parsed.",
              "\nPlease re-enter it so that it matches the number or name",
              "\nof one of:\n",
              paste(eval(formals(new_project)$stage), collapse = "\n"),
              "\nor enter NA to make the stage NA"
            )
            attempt <- readLines(con = stdin(), n = 1L)
            if (identical(attempt, "NA")) {
              attempt <- new_projects_stage(NA)
            } else {
              attempt <- try(validate_stage(attempt), silent = TRUE)
            }
          }

          replacements <<-
            rbind(
              tibble::tibble(
                old = x,
                new = attempt
              ),
              replacements
            )
        }
      } else {
        attempt <- replacements$new[match_attempt]
      }

      attempt
    }
  ) %>%
    do.call("c", .)
}



validate_sa_column <- function(projects_table,
                               colname,
                               authors_table,
                               pa_assoc,
                               replacements) {
  purrr::map2(
    projects_table[[colname]],
    projects_table$id,
    function(x, id) {

      project_authors <-
        authors_table[
          match(pa_assoc$id2[pa_assoc$id1 == id], authors_table$id),
        ]

      match_replacement_attempt <- match(x, replacements$old)

      if (
        !is.na(match_replacement_attempt) &&
        (is_creator(replacements$new[match_replacement_attempt]) ||
         any(project_authors$id == replacements$new[match_replacement_attempt]))
      ) {
        attempt <- replacements$new[match_replacement_attempt]
      } else {
        attempt <- x
      }

      attempt <- try_author(attempt, project_authors)

      if (inherits(attempt, "try-error")) {

        original_x <- x

        while (inherits(attempt, "try-error")) {

          message(
            "\nThe ", colname, " '", x,
            "' was unable to be matched to one of the",
            "\nauthors in the author list of project ", id, ":"
          )

          print(project_authors[, 1:3])

          message(
            "\nPlease re-enter the id number or the author name",
            "\nof one of these authors to be the ", colname,
            " of project ", id, ",",
            "\nor enter NA to make the ", colname, " NA.",
            ifelse(
              colname == "creator",
              paste0(
                "\nAlternatively, enter 0 to make the creator:\n0: ",
                original_x
              ),
              ""
            )
          )

          attempt <- readLines(con = stdin(), n = 1L)

          if (identical(attempt, "NA")) {
            attempt <- new_projects_author(NA)
          } else if (identical(attempt, "0") && colname == "creator") {
            attempt <- new_projects_author(paste0("0: ", original_x))
          } else {
            x       <- attempt
            attempt <- try_author(attempt, project_authors)
          }
        }

        if (
          !any(replacements$old == original_x) &&
          inherits(try_author(original_x, authors_table), "try-error")
        ) {
          replacements <<-
            rbind(
              tibble::tibble(
                old = original_x,
                new = attempt
              ),
              replacements
            )
        }
      }
      attempt
    }
  ) %>%
    do.call("c", .) %>%
    list(replacements = replacements, results = .)
}



try_author <- function(x, project_authors) {
  try(
    validate_projects_author(
      x,
      authors_table = project_authors,
      na.ok = TRUE
    ),
    silent = TRUE
  )
}
