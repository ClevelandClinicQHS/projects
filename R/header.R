
#' Print project header to console
#'
#' Prints a project's header to the console to be copied and pasted into a
#' project protocol or manuscript.
#'
#' The project header consists of:
#'
#' \enumerate{
#'
#' \item the project title
#'
#' \item the author list
#'
#' \item the list of author affiliations
#'
#' \item corresponding author information
#'
#' }
#'
#' The \code{header()} function is helpful when after editing details of the
#' project (e.g., any of the above information) you want to update your R
#' Markdown files. The displayed markdown can be pasted directly in place of the
#' header within the R Markdown documents (specifically \emph{01_protocol.Rmd}
#' and \emph{04_report.Rmd}).
#'
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} table.
#' @param archived Logical, indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project} argument. \code{FALSE} by default.
#'
#'   See the \strong{Details} section of \code{\link{archive_project}()} for
#'   more information on the "archived" status of a project.
#'
#' @examples
#' # SETUP
#' old_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' setup_projects(path = tempdir(), .Renviron_path = fs::path_temp(".Renviron"))
#' new_affiliation(department_name = "Math Dept.",
#'                 institution_name = "Springfield College",
#'                 address = "123 College St, Springfield, AB")
#' new_affiliation(department_name = "Art Department",
#'                 institution_name = "Springfield College",
#'                 address = "321 University Boulevard, Springfield, AB",
#'                 id = 42)
#' new_affiliation(department_name = "Central Intelligence Agency",
#'                 institution_name = "United States Government",
#'                 address = "888 Classified Dr, Washington DC")
#' new_affiliation(department_name = "Pyrotechnics",
#'                 institution_name = "ACME")
#' new_author(given_names = "Rosetta", last_name = "Stone",
#'            affiliations = c(42, "Math"), degree = "PhD",
#'            email = "slab@rock.net", phone = "867-555-5309", id = 8888)
#' new_author(given_names = "Spiro", last_name = "Agnew", degree = "LLB",
#'            affiliations = "Art D", id = 13)
#' new_author(given_names = "Plato", id = 303)
#' new_project(title = "Test Project 1", authors = c(13, "303", "Stone"),
#'             corresp_auth = "Stone")
#' #############################################################################
#'
#' header(1)
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @name header
#' @export
header <- function(project, archived = FALSE) {

  p_path  <- get_p_path()

  projects_table <- projects_internal(archived = archived)

  project_row <-
    validate_unique_entry(project, table = projects_table, what = "project")

  print_header_internal(
    project_id  = project_row$id,
    p_path      = p_path,
    project_row = project_row
  )
}



#' @importFrom rlang .data
print_header_internal <- function(
  project_id,
  p_path                   = get_p_path(),
  project_row              = dplyr::filter(projects_internal(p_path, TRUE),
                                           .data$id == project_id),
  authors_table            = authors_internal(p_path),
  affiliations_table       = affiliations_internal(p_path),
  project_author_assoc     = pa_assoc_internal(p_path),
  author_affiliation_assoc = aa_assoc_internal(p_path))
{

  project_authors <-
    project_author_assoc$id2[which(project_author_assoc$id1 == project_id)]

  corresp_auth_row <-
    authors_table[match(project_row$corresp_auth, authors_table$id), ]

  taa_to_console(
    title  = project_row$title,
    header =
      aa_header(
        project_id               = project_id,
        corresp_auth_row         = corresp_auth_row,
        authors_table            = authors_table,
        affiliations_table       = affiliations_table,
        project_authors          = project_authors,
        author_affiliation_assoc = author_affiliation_assoc
      )
  )
}



taa_to_console <- function(title, header) {
  cat('\ntitle: "', title, '"', '\n\n\n', sep = "")
  cat(header, sep = '\n')
}



insert_aa <- function(vector,
                      project_id,
                      yaml_bounds,
                      corresp_auth_row,
                      authors_table,
                      affiliations_table,
                      project_authors,
                      author_affiliation_assoc) {

  if (is.null(project_authors)) {
    aa_header <- character()
  } else {
    aa_header <-
      aa_header(
        project_id               = project_id,
        corresp_auth_row         = corresp_auth_row,
        authors_table            = authors_table,
        affiliations_table       = affiliations_table,
        project_authors          = project_authors,
        author_affiliation_assoc = author_affiliation_assoc
      )
  }

  vector <-
    append(
      x      = vector,
      values = c("", aa_header, "", "\\pagebreak", ""),
      after  = yaml_bounds[2L]
    )

  vector
}



#' @importFrom rlang .data
#' @importFrom tibble tibble
aa_header <- function(project_id,
                      corresp_auth_row,
                      authors_table,
                      affiliations_table,
                      project_authors,
                      author_affiliation_assoc) {

  # The left_join/select/rename combo was used instead of semi_join so that the
  # order in project_author_assoc would be preserved
  project_authors <-
    dplyr::left_join(tibble(id = project_authors), authors_table, by = "id")

  # In effect, this is author_affiliations_assoc (1) filtered to only include
  # authors on the project who have at least one affiliation and (2) all
  # affiliation information filled in.
  # It is constructed using the command sequence below in order to preserve (1)
  # the order of authors on the project and (2) the order of the affiliations of
  # each author.
  aa_assoc_complete <-
    project_authors %>%
    dplyr::select("id1" = "id") %>%
    dplyr::inner_join(author_affiliation_assoc, by = "id1") %>%
    dplyr::left_join(affiliations_table, by = c("id2" = "id"))


  ############################################################
  # Construction of affiliations line to go in 01_protocol.Rmd

  if (nrow(aa_assoc_complete) > 0) {

    # A tibble of the unique affiliations associated with the project, with a
    # superscript assigned to each
    unique_affiliations <-
      aa_assoc_complete %>%
      dplyr::select(-"id1") %>%
      dplyr::distinct() %>%
      dplyr::mutate(superscript = 1L:nrow(.))

    # In effect this adds the superscripts created in the previous command to
    # aa_assoc_complete
    aa_assoc_complete <-
      unique_affiliations %>%
      dplyr::select(.data$id2, .data$superscript) %>%
      dplyr::right_join(aa_assoc_complete, by = "id2")


    affiliations_lines <- ""
    for (a in 1:nrow(unique_affiliations)) {

      affiliation_line <- paste0("| ^", a, "^ ",
                                 unique_affiliations$department_name[a])

      if (!is.na(unique_affiliations$institution_name[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$institution_name[a])
      }

      if (!is.na(unique_affiliations$address[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$address[a])
      }

      affiliations_lines <- append(affiliations_lines, affiliation_line)
    }
  }
  else {
    affiliations_lines <- character()
  }
  ######################################################
  ######################################################

  ######################################################
  # Construction of author line to go in 01_protocol.Rmd
  if (nrow(project_authors) > 0) {
    author_line         <- "**_"

    for (x in 1:nrow(project_authors)) {

      if (x != 1) {
        author_line <- paste0(author_line, " ")

        if (x == nrow(project_authors) && x > 1) {
          author_line <- paste0(author_line, "and ")
        }
      }

      first_piece <- TRUE

      if (!is.na(project_authors$given_names[x])) {
        author_line <- paste0(author_line, project_authors$given_names[x])
        first_piece <- FALSE
      }

      if (!is.na(project_authors$last_name[x])) {
        if (!first_piece) {
          author_line <- paste0(author_line, " ")
        }

        author_line <- paste0(author_line, project_authors$last_name[x])
      }

      if (!is.na(project_authors$degree[x])) {
        author_line <- paste0(author_line, ", ", project_authors$degree[x])
      }

      if (x != nrow(project_authors) && nrow(project_authors) > 2) {
        author_line <- paste0(author_line, ";")
      }

      x_affiliations <- dplyr::filter(aa_assoc_complete,
                                      .data$id1 == project_authors$id[x])

      if (nrow(x_affiliations) > 0) {
        author_line <-
          paste0(author_line,
                 "^",
                 paste(sort(x_affiliations$superscript), collapse = ","),
                 "^")
      }

      if (isTRUE(project_authors$id[x] == corresp_auth_row$id)) {
        author_line <- paste0(author_line, "\\*")
      }
    }

    author_line <- paste0(author_line, "_**")
  }
  else {
    author_line <- character()
  }

  if (is.null(corresp_auth_row)) {
    corresp_lines <- character()
  }
  else {
    corresp_affils   <-
      aa_assoc_complete[match(corresp_auth_row$id, aa_assoc_complete$id1), ]

    corresp_lines    <- c("", "| \\* Corresponding author")

    if (nrow(corresp_affils) > 0L &&
        length(stats::na.omit(corresp_affils$address)) > 0L) {
      corresp_lines <-
        append(corresp_lines,
               paste0("|   ", stats::na.omit(corresp_affils$address)[1L]))
    }

    if (!is.na(corresp_auth_row$phone)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$phone))
    }

    if (!is.na(corresp_auth_row$email)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$email))
    }
  }

  ######################################################
  ######################################################

  c(author_line, affiliations_lines, corresp_lines, "", "| Funding:")
}
