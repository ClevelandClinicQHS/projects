
#' Print project header to console
#'
#' Prints a header to the console to be copied and pasted into the YAML of a
#' project protocol or manuscript R Markdown file. These lines essentially
#' produce a title page when the R Markdown file is knitted.
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
#' #############################################################################
#' # SETUP
#' old_home <- Sys.getenv("HOME")
#' old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' temp_dir <- tempfile("dir")
#' dir.create(temp_dir)
#' Sys.unsetenv("PROJECTS_FOLDER_PATH")
#' Sys.setenv(HOME = temp_dir)
#' setup_projects(path = temp_dir)
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
#' Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
#' @name header
#' @export
header <- function(project,
                   archived = FALSE
                   # degree = TRUE,
                   # corresp_address = TRUE,
                   # corresp_phone = FALSE,
                   # corresp_email = TRUE
                   ) {

  p_path  <- get_p_path()

  projects_table <- projects_internal(archived = archived)

  project_row <-
    validate_unique_entry(project, table = projects_table, what = "project")

  print_header_internal(
    project_id  = project_row$id,
    p_path      = p_path,
    project_row = project_row
    # degree = degree,
    # corresp_address = corresp_address,
    # corresp_phone = corresp_phone,
    # corresp_email = corresp_email
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
  author_affiliation_assoc = aa_assoc_internal(p_path)
  # degree,
  # corresp_address,
  # corresp_phone,
  # corresp_email
  ) {

  project_authors <-
    project_author_assoc$id2[project_author_assoc$id1 == project_id]

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
        # degree = degree,
        # corresp_address = corresp_address,
        # corresp_phone = corresp_phone,
        # corresp_email = corresp_email
      )
  )
}



taa_to_console <- function(title, header) {
  cat('\ntitle: "', title, '"\n', sep = "")
  cat(header, sep = '\n')
}



#' @importFrom rlang .data
aa_header <- function(project_id,
                      corresp_auth_row,
                      authors_table,
                      affiliations_table,
                      project_authors,
                      author_affiliation_assoc
                      # degree,
                      # corresp_address,
                      # corresp_phone,
                      # corresp_email
                      ) {

  # The left_join/select/rename combo was used instead of semi_join so that the
  # order in project_author_assoc would be preserved
  project_authors <-
    dplyr::left_join(
      tibble::tibble(id = project_authors),
      authors_table,
      by = "id"
    )

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


  if (nrow(project_authors) > 0L) {
    ############################################################
    # Construction of affiliations line to go in 01_protocol.Rmd

    if (nrow(aa_assoc_complete) > 0L) {

      # A tibble of the unique affiliations associated with the project, with a
      # superscript assigned to each
      unique_affiliations <-
        aa_assoc_complete %>%
        dplyr::select(-"id1") %>%
        dplyr::distinct() %>%
        dplyr::mutate(superscript = dplyr::row_number())

      # In effect this adds the superscripts created in the previous command to
      # aa_assoc_complete
      aa_assoc_complete <-
        unique_affiliations %>%
        dplyr::select(.data$id2, .data$superscript) %>%
        dplyr::right_join(aa_assoc_complete, by = "id2")


      affiliations_lines <- character()
      for (a in seq_len(nrow(unique_affiliations))) {

        affiliation_line <- paste0("  - ^", a, "^ ",
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

    author_line         <- "  - "

    for (x in seq_len(nrow(project_authors))) {

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

      if (
        # degree &&
        !is.na(project_authors$degree[x])) {
        author_line <- paste0(author_line, ", ", project_authors$degree[x])
      }

      if (x != nrow(project_authors) && nrow(project_authors) > 2L) {
        author_line <- paste0(author_line, ";")
      }

      x_affiliations <-
        dplyr::filter(aa_assoc_complete, .data$id1 == project_authors$id[x])

      if (nrow(x_affiliations) > 0L) {
        author_line <-
          paste0(
            author_line,
            "^",
            paste(sort(x_affiliations$superscript), collapse = ","),
            "^"
          )
      }

      if (isTRUE(project_authors$id[x] == corresp_auth_row$id)) {
        author_line <- paste0(author_line, "\\*")
      }
    }

    if (is.null(corresp_auth_row)) {
      corresp_lines <- character()
    } else {
      corresp_affils   <-
        aa_assoc_complete[match(corresp_auth_row$id, aa_assoc_complete$id1), ]

      corresp_lines    <- "  - \\* Corresponding author"

      if (
        # corresp_address &&
          nrow(corresp_affils) > 0L &&
          length(stats::na.omit(corresp_affils$address)) > 0L) {
        corresp_lines <-
          append(
            corresp_lines,
            paste0(
              "  - ",
              corresp_affils$address[!is.na(corresp_affils$address)][1L]
            )
          )
      }

      if (
        # corresp_phone &&
        !is.na(corresp_auth_row$phone)) {
        corresp_lines <-
          append(corresp_lines, paste0("  - ", corresp_auth_row$phone))
      }

      if (
        # corresp_email &&
          !is.na(corresp_auth_row$email)) {
        corresp_lines <-
          append(corresp_lines, paste0("  - ", corresp_auth_row$email))
      }
    }

    c("author:", author_line, affiliations_lines, corresp_lines)
  } else {
    character()
  }
}
