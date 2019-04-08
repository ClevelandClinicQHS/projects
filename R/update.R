
up_to_date <- function(table) {

  metadata_version <- attr(table, "projects_version")

  package_version <- utils::packageVersion("projects")

  if (is.null(metadata_version)) {
    return(FALSE)
  }

  if (metadata_version > package_version) {
    message(
      "A metadata item was found to be created with\n",
      "'projects' version ", as.character(metadata_version), ",\n",
      "whereas R is currently running 'projects' version ",
      as.character(package_version), ".\n\n",
      "Updating 'projects' with install.packages('projects') or\n",
      "remotes::install_github('NikKrieger/projects')\n",
      "is highly recommended before continuing.",
      "\n(Press [Enter] to continue anway or enter QUIT to quit)"
    )
    if (readline() =="QUIT") {
      stop(
        "\nRun one of these two: commands:\n\n",
        "install.packages('projects')\n\n",
        "remotes::install_github('NikKrieger/projects')",
        call. = FALSE
      )
    }
    TRUE
  } else {
    metadata_version == package_version
  }
}


update_notice <- function(addendum = ".") {

  message(
    "\nThe project metadata needs to be updated before proceeding",
    addendum,
    "\nNo data wil be lost."
  )

  update_metadata()
}








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

  p_path <- get_p_path()

  projects_path <- make_rds_path("projects", p_path)

  projects_table <- readRDS(projects_path)
    # tibble::tibble(
    #   id = 1:3,
    #   title = paste("title", id),
    #   stage = c(7, "ffff", NA),
    #   current_owner = c(1, 9999, "kriegen"),
    #   corresp_auth = c("nothing", "0: nothing", NA),
    #   creator = c("0: something", NA, "daltonj")
    # )

  projects_version <- attr(projects_table, "projects_version")

  authors_path <- make_rds_path("authors", p_path)

  authors_table <- readRDS(authors_path)

  pa_assoc_path <- make_rds_path("project_author_assoc", p_path)

  pa_assoc <- readRDS(pa_assoc_path)

  # if (identical(projects_version, utils::packageVersion("projects"))) {
  #   message("Metadata is already up to date.")
  #   return(invisible(projects_table))
  # }

  if (nrow(projects_table) == 0) {
    projects_table <-
      tibble(
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

    projects_table$current_owner <-
      validate_sa_column(
        projects_table,
        "current_owner",
        authors_table,
        pa_assoc
      )

    projects_table$corresp_auth <-
      validate_sa_column(
        projects_table,
        "corresp_auth",
        authors_table,
        pa_assoc
      )

    projects_table$creator <-
      validate_sa_column(projects_table, "creator", authors_table, pa_assoc)
  }

  write_metadata(authors_table, authors_path)

  "affiliations" %>%
    make_rds_path(p_path) %>%
    write_metadata(table = readRDS(.), table_path = .)

  write_metadata(pa_assoc, pa_assoc_path)

  "author_affiliation_assoc" %>%
    make_rds_path(p_path) %>%
    write_metadata(table = readRDS(.), table_path = .)

  write_metadata(projects_table, projects_path)

  message("\nProjects metadata updated.")

  invisible(projects_table)
}


validate_stage_column <- function(projects_table) {
  purrr::imap(
    projects_table$stage,
    function(x, row) {

      x <- try(validate_stage(x), silent = TRUE)

      while (inherits(x, "try-error")) {
        print(projects_table[row, c("id", "stage", "title")])
        message(
          "\nThe stage of the above project could not be parsed.",
          "\nPlease re-enter it so that it matches the number or name",
          "\nof one of:\n",
          paste(eval(formals(new_project)$stage), collapse = "\n"),
          "\nor enter NA to make the stage NA"
        )
        x <- readLines(con = stdin(), n = 1L)
        if (identical(x, "NA")) {
          x <- new_projects_stage(NA)
        } else {
          x <- try(validate_stage(x), silent = TRUE)
        }
      }

      x
    }
  ) %>%
    do.call("c", .)
}



try_author <- function(x, project_authors) {
  try(
    validate_projects_author(
      x,
      authors_table = project_authors,
      na.ok = TRUE,
    ),
    silent = TRUE
  )
}

validate_sa_column <- function(projects_table,
                               colname,
                               authors_table,
                               pa_assoc) {
  purrr::map2(
    projects_table[[colname]],
    projects_table$id,
    function(x, id) {
      project_authors <-
        authors_table[
          match(pa_assoc$id2[pa_assoc$id1 == id], authors_table$id),
        ]

      attempt <- try_author(x, project_authors)

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
            paste0("\nAlternatively, enter 0 to make the creator:\n0: ", x),
            ""
          )
        )

        attempt <- readLines(con = stdin(), n = 1L)

        if (identical(attempt, "NA")) {
          attempt <- new_projects_author(NA)
        } else if (identical(attempt, "0")) {
          attempt <- new_projects_author(paste0("0: ", x))
        } else {
          x       <- attempt
          attempt <- try_author(attempt, project_authors)
        }
      }

      attempt
    }
  ) %>%
    do.call("c", .)
}
