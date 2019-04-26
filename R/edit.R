################################################################################
#' @include new.R
#' @rdname new_edit_delete
#' @export
edit_project <- function(project,
                         title          = NULL,
                         short_title    = NULL,
                         authors        = NULL,
                         current_owner  = NULL,
                         status         = NULL,
                         deadline_type  = NULL,
                         deadline       = NULL,
                         stage          = NULL,
                         corresp_auth   = NULL,
                         creator        = NULL,
                         archived       = FALSE) {

  p_path          <- get_p_path()

  projects_path   <- make_rds_path("projects", p_path)

  projects_table  <- get_rds(projects_path)

  if (!archived) {
    projects_table <- remove_archived(projects_table)
  }

  project <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )

  authors_table   <- authors_internal(p_path)

  assoc_path      <- make_rds_path("project_author_assoc", p_path)
  assoc_table     <- get_rds(assoc_path)

  filtered_assoc  <- assoc_table[assoc_table$id1 == project$id, ]

  title         <- validate_single_string(title, null.ok = TRUE)
  short_title   <- validate_single_string(short_title, null.ok = TRUE)
  status        <- validate_single_string(status, null.ok = TRUE)
  deadline_type <- validate_single_string(deadline_type, null.ok = TRUE)

  stage           <- validate_stage(stage, null.ok = TRUE)

  deadline        <- validate_deadline(deadline, null.ok = TRUE)

  ###########################################
  # Handling of adding or removing authors

  if (is.null(authors)) {
    authors <- list(add = list(), remove = list())
  } else {
    authors <-
      parse_formula(
        formula      = authors,
        what         = "author",
        what2        = "project",
        main_table   = authors_table,
        assoc_table  = filtered_assoc
      )
  }

  ###########################################
  # Handling of current_owner, corresp_auth, and creator
  if (is.null(current_owner)) {
    if (
      !is.na(project$current_owner) &&
      any(authors$remove == project$current_owner)
    ) {
      current_owner <- new_projects_author(NA)
    }
  } else if (!is.na(current_owner)) {
    current_owner <-
      validate_projects_author(
        x             = current_owner,
        authors_table = authors_table
      )

    if (any(authors$remove == current_owner)) {
      stop(
        'The value of current_owner must not be slated for removal in the ',
        '"authors" argument.'
      )
    }

    # If current_owner isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if (!any(authors$add == current_owner)) {
      validate_assoc(
        x          = current_owner,
        what       = "author",
        rds_table  = authors_table,
        what2      = "project",
        rds_table2 = filtered_assoc
      )
    }
  }


  if (is.null(corresp_auth)) {
    if (
      !is.na(project$corresp_auth) &&
      any(authors$remove == project$corresp_auth)
    ) {
      corresp_auth <- new_projects_author(NA)
    }
  } else if (!is.na(corresp_auth)) {
    corresp_auth <-
      validate_projects_author(
        x             = corresp_auth,
        authors_table = authors_table,
        na.ok         = FALSE
      )

    if (any(authors$remove == corresp_auth)) {
      stop("The value of corresp_auth must not be slated for removal in the ",
           '"authors" argument.')
    }

    # If corresp_auth isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if (!any(authors$add == corresp_auth)) {
      validate_assoc(
        x          = corresp_auth,
        what       = "author",
        rds_table  = authors_table,
        what2      = "project",
        rds_table2 = filtered_assoc
      )
    }
  }


  if (is.null(creator)) {
    if (!is.na(project$creator) && any(authors$remove == project$creator)) {
      creator <- new_projects_author(NA)
    }
  } else if (!is.na(creator)) {
    creator <-
      validate_projects_author(
        x             = creator,
        authors_table = authors_table,
        na.ok         = FALSE
      )
  }

  new_project_row <-
    edit_metadata(
      table = projects_table,
      row_id = project$id,

      title         = title,
      short_title   = short_title,
      current_owner = current_owner,
      status        = status,
      deadline_type = deadline_type,
      deadline      = deadline,
      stage         = stage,
      corresp_auth  = corresp_auth,
      creator       = creator,

      table_path = projects_path
    )

  if (length(authors$remove) > 0L) {
    assoc_table <-
      delete_assoc(
        assoc_table = assoc_table,
        id1 = project$id,
        id2 = authors$remove,
        assoc_path = assoc_path
      )
  }

  if (length(authors$add) > 0L) {
    add_assoc(
      assoc_table = assoc_table,
      new_rows = tibble::tibble(id1 = project$id, id2 = authors$add),
      assoc_path = assoc_path
    )
  }

  if (
    !is.null(title) ||
    !is.null(corresp_auth) ||
    !identical(authors, list(add = list(), remove = list()))
  ) {
    message("\nHeader has changed. Reprint it with:\nheader(", project$id, ")")
  }

  invisible(new_project_row)
}
################################################################################
################################################################################



################################################################################
#' @include new.R
#' @rdname new_edit_delete
#' @export
edit_author <- function(author,
                        given_names   = NULL,
                        last_name     = NULL,
                        affiliations  = NULL,
                        title         = NULL,
                        degree        = NULL,
                        email         = NULL,
                        phone         = NULL) {

  p_path             <- get_p_path()

  authors_path       <- make_rds_path("authors", p_path)

  authors_table      <- get_rds(authors_path)

  author             <-
    validate_unique_entry(
      x      = author,
      table  = authors_table,
      what   = "author"
    )$id

  affiliations_table <- affiliations_internal(p_path)

  assoc_path         <- make_rds_path("author_affiliation_assoc", p_path)
  assoc_table        <- get_rds(assoc_path)

  given_names <- validate_single_string(given_names, null.ok = TRUE)
  last_name   <- validate_single_string(last_name, null.ok = TRUE)
  title       <- validate_single_string(title, null.ok = TRUE)
  degree      <- validate_single_string(degree, null.ok = TRUE)
  email       <- validate_single_string(email, null.ok = TRUE, tolower = TRUE)
  phone       <- validate_single_string(phone, null.ok = TRUE)

  if (is.null(affiliations)) {
    affiliations <- list(add = list(), remove = list())
  } else {
    affiliations <-
      parse_formula(
        formula     = affiliations,
        what        = "affiliation",
        what2       = "author",
        main_table  = affiliations_table,
        assoc_table = assoc_table[assoc_table$id1 == author, ]
      )
  }

  new_author_row <-
    edit_metadata(
      table = authors_table,
      row_id = author,
      given_names = given_names,
      last_name   = last_name,
      title       = title,
      degree      = degree,
      email       = email,
      phone       = phone,
      table_path = authors_path
    )

  if (length(affiliations$remove) > 0) {
    assoc_table <-
      delete_assoc(
        assoc_table = assoc_table,
        id1         = author,
        id2         = affiliations$remove,
        assoc_path  = assoc_path
      )
  }

  if (length(affiliations$add) > 0) {
    add_assoc(
      assoc_table = assoc_table,
      new_rows = tibble::tibble(id1 = author, id2 = affiliations$add),
      assoc_path  = assoc_path
    )
  }

  invisible(new_author_row)
}
################################################################################



################################################################################
#' @include new.R
#' @rdname new_edit_delete
#' @export
edit_affiliation <- function(affiliation,
                             department_name  = NULL,
                             institution_name = NULL,
                             address          = NULL) {

  p_path             <- get_p_path()

  affiliations_path  <- make_rds_path("affiliations", p_path)

  affiliations_table <- get_rds(affiliations_path)

  affiliation        <-
    validate_unique_entry(
      x     = affiliation,
      table = affiliations_table,
      what  = "affiliation"
    )$id

  department_name  <- validate_single_string(department_name, null.ok = TRUE)
  institution_name <- validate_single_string(institution_name, null.ok = TRUE)
  address          <- validate_single_string(address, null.ok = TRUE)

  message("Edited affiliation:")

  edited_row <-
    edit_metadata(
      table = affiliations_table,
      row_id = affiliation,
      department_name  = department_name,
      institution_name = institution_name,
      address          = address,
      table_path = affiliations_path
    )

  invisible(edited_row)
}
################################################################################




#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
delete_project <- function(project, archived = FALSE) {

  p_path         <- get_p_path()

  projects_path  <- make_rds_path("projects", p_path)
  projects_table <- get_rds(projects_path)

  if (!archived) {
    projects_table <- remove_archived(projects_table)
  }

  project        <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )$id

  pa_assoc_path  <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_table <- get_rds(pa_assoc_path)

  project_row    <- dplyr::filter(projects_table, .data$id == project)

  print(project_row)

  if (!fs::dir_exists(project_row$path)) {
    user_prompt(
      msg   = paste0("Project folder not found at\n", project_row$path,
                     "\nDelete only its metadata? (y/n)"),
      n_msg = paste0("Deletion not completed. Restore folder to ",
                     project_row$path, ' or rerun this command, inputting "y" ',
                     'instead of "n" when asked whether or not to continue.'))
  } else {
    user_prompt(
      msg   =
        paste0(
          "\nAre you sure you want to delete the above project ",
          "and its entire folder, which is located at\n\n",
          project_row$path,
          "\n\n? (y/n)"
        ),
      n_msg =
        paste0(
          '\nDeletion not completed. If deletion is desired, try ',
          'again and enter "y".'
        )
    )
    fs::dir_delete(path = project_row$path)
  }


  delete_metadata(
    table = projects_table,
    row_id = project,
    table_path = projects_path
  )

  delete_assoc(
    assoc_table = pa_assoc_table,
    id1         = project,
    assoc_path  = pa_assoc_path
  )

  print(project_row)
  message("\nThe above project was deleted.")
  invisible(project_row)
}



#' @include new.R
#' @rdname new_edit_delete
#' @export
delete_author <- function(author) {

  p_path          <- get_p_path()

  projects_path   <- make_rds_path("projects", p_path)
  projects_table <- get_rds(projects_path)

  authors_path    <- make_rds_path("authors", p_path)
  authors_table  <- get_rds(authors_path)

  author_row          <-
    validate_unique_entry(
      x     = author,
      table = authors_table,
      what  = "author"
    )

  pa_assoc_path   <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_table <- get_rds(pa_assoc_path)

  aa_assoc_path   <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_table <- get_rds(aa_assoc_path)

  print(author_row)

  user_prompt(
    msg   = "\nAre you sure you want to delete the above author? (y/n)",
    n_msg = paste0('\nDeletion not completed. If deletion is desired, ',
                   'input "y" next time.'))

  delete_metadata(
    table = authors_table,
    row_id = author_row$id,
    table_path = authors_path
  )

  clear_special_author(author          = author_row$id,
                       projects_path   = projects_path,
                       projects_table = projects_table)

  delete_assoc(
    assoc_table  = pa_assoc_table,
    id2          = author_row$id,
    assoc_path   = pa_assoc_path
  )

  delete_assoc(
    assoc_table = aa_assoc_table,
    id1         = author_row$id,
    assoc_path  = aa_assoc_path
  )

  print(author_row)
  message("The above author was deleted.")

  invisible(author_row)
}



#' @include new.R
#' @rdname new_edit_delete
#' @export
delete_affiliation <- function(affiliation) {

  p_path             <- get_p_path()

  affiliations_path  <- make_rds_path("affiliations", p_path)
  affiliations_table <- get_rds(affiliations_path)

  aa_assoc_path      <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_table     <- get_rds(aa_assoc_path)

  affiliation_row        <-
    validate_unique_entry(
      x     = affiliation,
      table = affiliations_table,
      what  = "affiliation"
    )

  print(affiliation_row)

  user_prompt(
    msg   = "\nAre you sure you want to delete the above affiliation? (y/n)",
    n_msg = paste0('\nDeletion not completed. If deletion is desired, ',
                   'input "y" next time.'))

  delete_metadata(
    table = affiliations_table,
    row_id = affiliation_row$id,
    table_path = affiliations_path
  )

  delete_assoc(
    assoc_table = aa_assoc_table,
    id2         = affiliation_row$id,
    assoc_path  = aa_assoc_path
  )

  print(affiliation_row)
  message("The above affiliation was deleted.")

  invisible(affiliation_row)
}



# These functions are dedicated to dealing with the authors argument in
# edit_project()
################################################################################
parse_formula <- function(formula, what, what2, main_table, assoc_table) {

  if (!rlang::is_formula(formula)) {
    stop(what, "s argument must be a formula. See help(edit_", what2, ")")
  }

  # This converts all integers in the formula to backquoted names e.g. `5`.
  # This is necessary because the terms() formula that comes later does not
  # know how to process plain integers, but it does know how to process names.
  formula <- process_formula_numbers(formula)

  if (
    !all(setdiff(all.names(formula), all.vars(formula)) %in% c("~", "+", "-"))
  ) {
    stop(what, "s formula must begin with a single ~ and only contain plus ",
         "signs (+) for adding formula and minus signs (-) for removing ",
         "them. See help(edit_", what2, ")")
  }

  if (!is.null(rlang::f_lhs(formula))) {
    stop(what, "s formula must not have a left-hand side (i.e., must begin ",
         "with a single tilde (~)). See help(edit_", what2, ")")
  }

  # The "term.labels" attribute of the terms() output yields a character
  # vector of the elements of the formula that are added (i.e., having a plus
  # sign in front as opposed to a minus sign). Any ID numbers entered by the
  # user end up in the form e.g. "`5`", so the sapply piece removes the
  # backticks.
  add <-
    formula %>%
    stats::terms() %>%
    attr("term.labels") %>%
    vapply(
      FUN       = function(x) as.character(rlang::parse_expr(x)),
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    )

  remove <- setdiff(all.vars(formula), add)

  if (length(remove) > 0L) {

    # Validates the remove list against the main database, making sure that the
    # authors or affiliations actually exist in the database, first turning any
    # IDs into numbers (via the ifelse() statement)
    remove <-
      validate_unique_entry_list(x = remove, table = main_table, what = what)$id

    # Makes sure the elements to remove are actually listed as elements of
    # the user-specified project/author

    validate_assoc(
      x           = remove,
      what        = what,
      rds_table   = main_table,
      what2       = what2,
      rds_table2  = assoc_table
    )
  }

  if (length(add) > 0L) {

    # Validates the add list against the author tibble, first
    # turning any author IDs from characters to integers (via the ifelse()
    # statement)
    add <-
      validate_unique_entry_list(x = add, table = main_table, what = what)$id

    # Makes sure the formula in add are not already in the
    # user-specified project's author list
    if (any(add %in% assoc_table$id2)) {
      print(main_table[main_table$id %in% add[add %in% assoc_table$id2], ])
      stop("The above ", what, "s are already on the ", what2, "'s ",
           what, " list.")
    }
  }
  #################################################
  #################################################

  list(add = add, remove = remove)
}


process_formula_numbers <- function(formula) {
  stats::as.formula(recursive_number_namer(formula))
}

recursive_number_namer <- function(formula) {
  as.call(
    lapply(
      as.list(formula),
      function(x) {
        if (is.atomic(x) && length(x) == 1) {
          as.name(x)
        } else if (is.name(x)) {
          x
        } else if (is.call(x)) {
          if (!(as.list(x)[1] %in% list(quote(`+`), quote(`-`)))) {
            stop(
              "Computation with the function `", as.list(x)[[1]], "` is ",
              "not allowed in formula. Must begin with a single tilde (~)",
              " and elements may only be separated with plus signs and ",
              "minus signs."
            )
          }
          recursive_number_namer(x)
        } else {
          stop(
            "Don't know how to handle object of type ",
            typeof(x),
            ". Formulas must only contain author names, IDs, plus signs, ",
            "and minus signs."
          )
        }
      }
    )
  )
}
################################################################################
################################################################################



#' Reordering authors and affiliations
#'
#' These functions allow the user to reorder a project's authors or an author's
#' affiliations.
#'
#' The order of authors and affiliations affects the order in which these items
#' appear in project \link{header}s.
#'
#' When specifying explicit ranks, enter \code{...} as name-value pairs (e.g.,
#' Johnson = 2, "Baron Cohen" = 4). You can even enumerate authors/affiliations
#' by their corresponding (quoted) \code{id} numbers (e.g., `7` = 2, ACME = 4,
#' `22` = 6). If entering an integer greater than the total number of
#' authors/affiliations, the element will be put at the end. The \code{after}
#' argument will be ignored in this case.
#'
#' When not specifying explicit ranks, simply enter author/affiliations
#' \code{id}s or names in the order you want them, and the ones you entered will
#' be inserted after the position specified by the \code{after} argument. By
#' default (\code{after = 0}), the authors/affiliations in \code{...} will be
#' moved to the front. This behavior corresponds to that of
#' \code{\link{append}()} or \code{forcats::\link[forcats]{fct_relevel}()}.
#'
#' @param project,author The \code{id} or unambiguous names of a project/author
#'   whose authors/affiliations you want to reorder.
#' @param ... The \code{id}s or names of authors/affiliations you want to
#'   reorder, optionally with their new ranks explicitly stated. See
#'   \strong{Details}.
#' @param after If not specifying explicit ranks in \code{...}, the position you
#'   want the elements to come after. Works like the \code{after} argument in
#'   \code{\link[base]{append}} or \code{forcats::fct_relevel}.
#'
#'   Ignored if ranks are explicitly provided in \code{...}.
#' @param archived Logical indicating whether or not the function should
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
#' new_author(given_names = "Condoleezza", last_name = "Rice", degree = "PhD",
#'            affiliations = c(1, 42, "Agency", "ACME"), phone = "555-555-5555",
#'            email = "condoleeza@ri.ce")
#' new_author(given_names = "Jane", last_name = "Goodall", degree = "PhD",
#'            affiliations = 3, id = 5)
#' new_project(title = "Understanding the Construction of the United States",
#'             short_title = "USA",
#'             authors = c(13, "Stone", "zz", "303", "Jane Goodall"),
#'             stage = 4, deadline = "2055-02-28", deadline_type = "submission",
#'             path = "famous_studied/philosophers/rocks",
#'             corresp_auth = "Stone", current_owner = "agnew",
#'             make_directories = TRUE, use_bib = TRUE,
#'             status = "waiting on IRB")
#' #############################################################################
#'
#' # Reordering with unnamed arguments
#' reorder_affiliations(author = "RICE", "ACME", 42, after = 1)
#'
#'
#' # Project 1 header before reordering authors:
#' header(1)
#'
#' # Reordering with named arguments
#' reorder_authors(project = 1, "Rosetta" = 99, `303` = 2, "5" = 1)
#'
#' # Project 1 header after reordering authors:
#' header(1)
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @name reordering
#' @export
reorder_authors <- function(project, ..., after = 0L, archived = FALSE) {
  reorder_assoc(
    id       = project,
    ...,
    after    = after,
    rds1     = "project",
    rds2     = "author",
    assoc    = "project_author_assoc",
    archived = archived
  )
}

#' @rdname reordering
#' @export
reorder_affiliations <- function(author, ..., after = 0L) {
  reorder_assoc(
    id    = author,
    ...,
    after = after,
    rds1  = "author",
    rds2  = "affiliation",
    assoc = "author_affiliation_assoc"
  )
}
#########################################


#########################################
#########################################
#' @importFrom rlang .data
reorder_assoc <- function(id, ..., after, rds1, rds2, assoc, archived = TRUE) {

  p_path         <- get_p_path()

  rds1_path      <- make_rds_path(paste0(rds1, "s"), p_path)
  rds1_table     <- get_rds(rds1_path)

  rds1_row       <-
    validate_unique_entry(x = id, table = rds1_table, what = rds1)

  rds2_path      <- make_rds_path(paste0(rds2, "s"), p_path)
  rds2_table     <- get_rds(rds2_path)

  assoc_path     <- make_rds_path(assoc, p_path)
  assoc_table    <- get_rds(assoc_path)

  filtered_assoc <- dplyr::filter(assoc_table, .data$id1 == rds1_row$id)

  user_order     <- rlang::exprs(...)

  valid_names    <- rlang::have_name(user_order)

  if (any(valid_names)) {

    if (!all(rlang::have_name(user_order))) {
      stop("Elements in ... must either all be named or have no names at all.")
    }

    user_order <- unlist(user_order)

    if (
      !(
        rlang::is_integerish(user_order) &&
        all(!is.na(user_order)) &&
        all(user_order >= 1L) &&
        length(user_order) <= nrow(filtered_assoc) &&
        anyDuplicated(user_order) == 0L
      )
    ) {
      stop("Ranks must be integers 1 or greater, no two ranks may be the same,",
           " and the number of ranks given must not be greater than the ", rds1,
           "'s total number of ", rds2, "s.")
    }
    names(user_order) <- names(user_order) %>%
      validate_unique_entry_list(table = rds2_table, what = rds2) %>%
      `$`("id") %>%
      validate_assoc(
        what  = rds2,
        rds_table  = rds2_table,
        what2 = rds1,
        rds_table2 = filtered_assoc
      )

    user_order <- user_order[order(user_order)]
  } else {

    user_order <- user_order %>%
      vapply(as.character, character(1L)) %>%
      validate_unique_entry_list(table = rds2_table, what  = rds2) %>%
      `$`("id") %>%
      validate_assoc(
        what  = rds2,
        rds_table  = rds2_table,
        what2 = rds1,
        rds_table2 = filtered_assoc
      )

    user_order <-
      stats::setNames(object = seq_along(user_order) + after, nm = user_order)
  }

  reordered <- setdiff(filtered_assoc$id2, names(user_order))

  purrr::iwalk(
    user_order,
    function(new_rank, rds2_id) {
      reordered <<-
        append(
          x      = reordered,
          values = as.integer(rds2_id),
          after  = new_rank - 1L
        )
    }
  )

  assoc_table <-
    delete_assoc(
      assoc_table = assoc_table,
      id1 = rds1_row$id,
      id2 = filtered_assoc$id2,
      assoc_path = assoc_path
    )

  add_assoc(
    assoc_table = assoc_table,
    new_rows    = tibble::tibble(id1 = rds1_row$id, id2 = reordered),
    assoc_path  = assoc_path
  )

  if (rds1 == "project") {
    message("\nHeader has changed. Reprint it with:\nheader(", rds1_row$id, ")")
  }

  invisible(
    assoc_table %>%
      dplyr::filter(.data$id1 == rds1_row$id) %>%
      dplyr::left_join(rds2_table, by = c("id2" = "id")) %>%
      dplyr::select(-"id1") %>%
      dplyr::rename(!!paste0(rds2, "_id") := "id2")
  )
}
################################################################################
################################################################################
