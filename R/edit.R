################################################################################
#' @rdname new_edit_delete
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
edit_project <- function(project,
                         title          = NA,
                         short_title    = NA,
                         authors,
                         current_owner  = NA,
                         status         = NA,
                         deadline_type  = NA,
                         deadline       = NA,
                         stage          = c("1: design", "2: data collection",
                                            "3: analysis", "4: manuscript",
                                            "5: under review", "6: accepted"),
                         corresp_auth   = NA,
                         creator        = NA,
                         reprint_header = TRUE,
                         archived       = FALSE) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)

  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(project,
                                    what       = "project",
                                    rds_tibble = projects_tibble,
                                    max.length = 1,
                                    archived   = archived)
  old_project_row <- dplyr::filter(projects_tibble, .data$id == project)

  authors_tibble  <- "authors" %>% make_rds_path(p_path) %>% get_rds

  assoc_path      <- make_rds_path("project_author_assoc", p_path)
  assoc_tibble    <- get_rds(assoc_path)

  filtered_assoc  <- dplyr::filter(assoc_tibble, .data$id1 == project)

  stage <- validate_stage(stage, choices = eval(formals()$stage))

  ###########################################
  # Handling of adding or removing authors

  if(missing(authors)) {
    authors <- list(add = list(), remove = list())
  }
  else {
    authors <- parse_formula(formula      = authors,
                             what         = "author",
                             what2        = "project",
                             main_tibble  = authors_tibble,
                             assoc_tibble = filtered_assoc)
  }

  ###########################################
  # Handling of current_owner, corresp_auth, and creator
  if(is.na(current_owner)) {
    if(old_project_row$current_owner %in% authors$remove) {
      current_owner <- NULL
    }
  }
  else if(!is.null(current_owner)) {
    current_owner <- validate_entry(x          = current_owner,
                                    what       = "author",
                                    rds_tibble = authors_tibble,
                                    max.length = 1)

    if(current_owner %in% authors$remove) {
      stop("The value of current_owner must not be slated for removal in the",
           '"authors" argument.')
    }

    # If current_owner isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if(!(current_owner %in% authors$add)) {
      validate_assoc(x           = current_owner,
                     what        = "author",
                     rds_tibble  = authors_tibble,
                     what2       = "project",
                     rds_tibble2 = filtered_assoc)
    }
  }


  if(is.na(corresp_auth)) {
    if(old_project_row$corresp_auth %in% authors$remove) {
      corresp_auth <- NULL
    }
  }
  else if(!is.null(corresp_auth)) {
    corresp_auth <- validate_entry(x          = corresp_auth,
                                   what       = "author",
                                   rds_tibble = authors_tibble,
                                   max.length = 1)

    if(corresp_auth %in% authors$remove) {
      stop("The value of corresp_auth must not be slated for removal in the ",
           '"authors" argument.')
    }

    # If corresp_auth isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if(!(corresp_auth %in% authors$add)) {
      validate_assoc(x           = corresp_auth,
                     what        = "author",
                     rds_tibble  = authors_tibble,
                     what2       = "project",
                     rds_tibble2 = filtered_assoc)
    }
  }


  if(is.na(creator)) {
    if(suppressWarnings(as.integer(old_project_row$creator)) %in%
       authors$remove) {
      creator <- NULL
    }
  }
  else if(!is.null(creator)) {
    creator <- validate_entry(x          = creator,
                              what       = "author",
                              rds_tibble = authors_tibble,
                              max.length = 1)
  }
  ###########################################
  ###########################################

  new_project_row <- change_table(action        = "edit",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  id            = project,
                                  title         = title,
                                  short_title   = short_title,
                                  current_owner = current_owner,
                                  status        = status,
                                  deadline_type = deadline_type,
                                  deadline      = as.Date(deadline),
                                  stage         = stage,
                                  path          = NA_character_,
                                  corresp_auth  = corresp_auth,
                                  creator       = as.character(creator))

  if(length(authors$remove) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = FALSE,
                   id1          = project,
                   id2          = authors$remove)
  }

  if(length(authors$add) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = TRUE,
                   id1          = project,
                   id2          = authors$add)
  }

  filtered_assoc <- dplyr::filter(assoc_tibble, .data$id1 == project)

  message("Edited project info:")
  print(new_project_row)

  message("\nEdited project's authors:")
  if(nrow(filtered_assoc) == 0) {
    print("None")
  }
  else {
    print(filtered_assoc %>%
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-"id1") %>%
            dplyr::rename("author_id" = "id2"))
  }

  if(reprint_header) {
    print_header_internal(
      project_id           = project,
      p_path               = p_path,
      project_row          = new_project_row,
      authors_tibble       = authors_tibble,
      project_author_assoc = assoc_tibble
    )
  }
}
################################################################################
################################################################################



################################################################################
#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
edit_author <- function(author,            given_names   = NA,
                        last_name = NA,    affiliations,
                        title     = NA,    degree        = NA,
                        email     = NA,    phone         = NA) {

  p_path              <- p_path_internal()

  authors_path        <- make_rds_path("authors", p_path)

  authors_tibble      <- get_rds(authors_path)

  author              <- validate_entry(author,
                                        what        = "author",
                                        rds_tibble  = authors_tibble,
                                        max.length  = 1)

  affiliations_tibble <- get_rds(make_rds_path("affiliations", p_path))

  assoc_path          <- make_rds_path("author_affiliation_assoc", p_path)
  assoc_tibble        <- get_rds(assoc_path)

  if(missing(affiliations)) {
    affiliations <- list(add = list(), remove = list())
  }
  else {
    affiliations <-
      parse_formula(
        formula      = affiliations,
        what         = "affiliation",
        what2        = "author",
        main_tibble  = affiliations_tibble,
        assoc_tibble = dplyr::filter(assoc_tibble, .data$id1 == author))
  }

  new_author_row <- change_table(action      = "edit",
                                 rds_path    = authors_path,
                                 rds_tibble  = authors_tibble,
                                 id          = author,
                                 given_names = given_names,
                                 last_name   = last_name,
                                 title       = title,
                                 degree      = degree,
                                 email       = tolower(email),
                                 phone       = phone)

  if(length(affiliations$remove) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = FALSE,
                   id1          = author,
                   id2          = affiliations$remove)
  }
  if(length(affiliations$add) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = TRUE,
                   id1          = author,
                   id2          = affiliations$add)
  }


  filtered_assoc <- dplyr::filter(assoc_tibble, .data$id1 == author)

  message("Edited author:")
  print(new_author_row)

  message("\nEdited author's affiliations:")
  if(nrow(filtered_assoc) == 0) {
    print("None.")
  }
  else {
    print(filtered_assoc %>%
            dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
            dplyr::select(-"id1") %>%
            dplyr::rename("affiliation_id" = "id2"))
  }

}
################################################################################



################################################################################
#' @rdname new_edit_delete
#' @export
edit_affiliation <- function(affiliation,           department_name  = NA,
                             institution_name = NA, address          = NA) {

  p_path              <- p_path_internal()

  affiliations_path   <- make_rds_path("affiliations", p_path)

  affiliations_tibble <- get_rds(affiliations_path)

  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        rds_tibble = affiliations_tibble,
                                        max.length = 1)

  message("Edited affiliation:")
  print(change_table(action           = "edit",
                     rds_path         = affiliations_path,
                     rds_tibble       = affiliations_tibble,
                     id               = affiliation,
                     department_name  = department_name,
                     institution_name = institution_name,
                     address          = address))
}
################################################################################


#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
delete_project <- function(project, archived = FALSE) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(project,
                                    what       = "project",
                                    rds_tibble = projects_tibble,
                                    max.length = 1,
                                    archived   = archived)

  pa_assoc_path   <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble <- get_rds(pa_assoc_path)

  project_row     <- dplyr::filter(projects_tibble, .data$id == project)

  print(project_row)

  if(!fs::dir_exists(project_row$path)) {
    user_prompt(
      msg   = paste0("Project folder not found at\n", project_row$path,
                     "\nDelete only its metadata? (y/n)"),
      n_msg = paste0("Deletion not completed. Restore folder to ",
                     project_row$path, ' or rerun this command, inputting "y" ',
                     'instead of "n" when asked whether or not to continue.'))
  }
  else {
    user_prompt(
      msg   = paste0("\nAre you sure you want to delete the above project ",
                     "and its entire folder, which is located at\n\n",
                     project_row$path, "\n\n? (y/n)"),
      n_msg = paste0('\nDeletion not completed. If deletion is desired, try ',
                     'again and enter "y".'))
    fs::dir_delete(path = project_row$path)
  }

  change_table(action     = "delete",        rds_path   = projects_path,
               rds_tibble = projects_tibble, id         = project)

  change_assoc(assoc_path   = pa_assoc_path, assoc_tibble = pa_assoc_tibble,
               new          = FALSE,         id1          = project)

  print(project_row)
  message("\nThe above project was deleted.")
  return(invisible(project_row))
}


#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
delete_author <- function(author) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  authors_path    <- make_rds_path("authors", p_path)
  authors_tibble  <- get_rds(authors_path)

  author          <- validate_entry(author,
                                    what       = "author",
                                    rds_tibble = authors_tibble,
                                    max.length = 1)

  pa_assoc_path   <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble <- get_rds(pa_assoc_path)

  aa_assoc_path   <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_tibble <- get_rds(aa_assoc_path)

  author_row      <- dplyr::filter(authors_tibble, .data$id == author)

  print(author_row)

  user_prompt(
    msg   = "\nAre you sure you want to delete the above author? (y/n)",
    n_msg = paste0('\nDeletion not completed. If deletion is desired, ',
                   'input "y" next time.'))

  change_table(action     = "delete",
               rds_path   = authors_path,
               rds_tibble = authors_tibble,
               id         = author)

  clear_special_author(author          = author,
                       projects_path   = projects_path,
                       projects_tibble = projects_tibble)

  change_assoc(assoc_path   = pa_assoc_path,
               assoc_tibble = pa_assoc_tibble,
               new          = FALSE,
               id2          = author)

  change_assoc(assoc_path   = aa_assoc_path,
               assoc_tibble = aa_assoc_tibble,
               new          = FALSE,
               id1          = author)

  print(author_row)
  message("The above author was deleted.")
}


#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
delete_affiliation <- function(affiliation) {

  p_path              <- p_path_internal()

  affiliations_path   <- make_rds_path("affiliations", p_path)
  affiliations_tibble <- get_rds(affiliations_path)

  aa_assoc_path       <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_tibble     <- get_rds(aa_assoc_path)

  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        rds_tibble = affiliations_tibble,
                                        max.length = 1)

  affiliation_row     <- dplyr::filter(affiliations_tibble,
                                       .data$id == affiliation)

  print(affiliation_row)

  user_prompt(
    msg   = "\nAre you sure you want to delete the above affiliation? (y/n)",
    n_msg = paste0('\nDeletion not completed. If deletion is desired, ',
                   'input "y" next time.'))

  change_table(action     = "delete",
               rds_path   = affiliations_path,
               rds_tibble = affiliations_tibble,
               id         = affiliation)

  change_assoc(assoc_path   = aa_assoc_path,
               assoc_tibble = aa_assoc_tibble,
               new          = FALSE,
               id2          = affiliation)

  print(affiliation_row)
  message("The above affiliation was deleted.")
}



# These functions are dedicated to dealing with the authors argument in
# edit_project()
################################################################################
parse_formula <- function(formula, what, what2, main_tibble, assoc_tibble) {

  if(!rlang::is_formula(formula)) {
    stop(what, "s argument must be a formula. See help(edit_", what2, ")")
  }

  # This converts all integers in the formula to backquoted names e.g. `5`.
  # This is necessary because the terms() formula that comes later does not
  # know how to process plain integers, but it does know how to process names.
  formula <- process_formula_numbers(formula)

  if(!all(setdiff(all.names(formula),
                  all.vars(formula)) %in% c("~", "+", "-"))) {
    stop(what, "s formula must begin with a single ~ and only contain plus ",
         "signs (+) for adding formula and minus signs (-) for removing ",
         "them. See help(edit_", what2, ")")
  }

  if(!is.null(rlang::f_lhs(formula))) {
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
    sapply(FUN       = function(x) as.character(rlang::parse_expr(x)),
           USE.NAMES = FALSE)

  remove <- setdiff(all.vars(formula), add)

  if(length(remove) > 0) {

    # Validates the remove list against the main database, making sure that the
    # authors or affiliations actually exist in the database, first turning any
    # IDs into numbers (via the ifelse() statement)
    remove <- validate_entry(x = remove, what = what, rds_tibble = main_tibble)

    # Makes sure the elements to remove are actually listed as elements of
    # the user-specified project/author

    validate_assoc(x           = remove,
                   what        = what,
                   rds_tibble  = main_tibble,
                   what2       = what2,
                   rds_tibble2 = assoc_tibble)
  }

  if(length(add) > 0) {

    # Validates the remove list against the author tibble, first
    # turning any author IDs from characters to integers (via the ifelse()
    # statement)
    add <- validate_entry(x = add, what = what, rds_tibble = main_tibble)

    # Makes sure the formula in add are not already in the
    # user-specified project's author list
    if(any(add %in% assoc_tibble$id2)) {
      print(main_tibble %>% dplyr::filter(.data$id %in%
                                            add[add %in% assoc_tibble$id2]))
      stop("The above ", what, "s are already on the ", what2, "'s ",
           what, " list.")
    }
  }
  #################################################
  #################################################

  return(list(add = add, remove = remove))
}


process_formula_numbers <- function(formula) {

  stats::as.formula(recursive_number_namer(formula))

}

recursive_number_namer <- function(formula) {
  as.call(
    lapply(
      X   = as.list(formula),
      FUN = function(x) {
        if(is.atomic(x) && length(x) == 1) {
          return(as.name(x))
        }
        else if(is.name(x)) {
          return(x)
        }
        else if(is.call(x)) {
          if(!(as.list(x)[1] %in% list(quote(`+`), quote(`-`)))) {
            stop("Computation with the function `", as.list(x)[[1]], "` is ",
                 "not allowed in formula. Must begin with a single tilde (~)",
                 " and elements may only be separated with plus signs and ",
                 "minus signs.")
          }
          return(recursive_number_namer(x))
        }
        else {
          stop("Don't know how to handle object of type ", typeof(x),
               ". Formulas must only contain author names, IDs, plus signs, ",
               "and minus signs.")
        }})
  )
}
################################################################################
################################################################################



# Reordering functions
################################################################################

#' Reordering authors and affiliations
#'
#' These functions allow the user to reorder authors on a project or to reorder
#' an author's affiliations.
#'
#' The order of these affects the order of authors and affiliations in
#' \code{\link{header}}s.
#'
#' When specifying explicit ranks, enter \code{...} as name-value pairs, in
#' which the names are names of authors/affiliations (or even their [back]quoted
#' \code{id}s) and the values are integer ranks you want them to occupy. If
#' entering an integer greater than the total number of authors/affiliations,
#' the element will be put at the end. The \code{after} argument will be ignored
#' in this case.
#'
#' When not specifying explicit ranks, simply enter author/affiliations
#' \code{id}s or names in the order you want them, and the ones you entered will
#' be inserted after the position specified by the \code{after} argument. By
#' default (\code{after = 0}), the authors/affiliations in \code{...} will be
#' moved to the front.
#'
#' @param project,author The \code{id} or unambiguous names of a project/author
#'   whose authors/affiliations you want to reorder.
#' @param ... The \code{id}s or names of authors/affiliations you want to
#'   reorder, optionally with their new ranks explicitly stated. See details.
#' @param after If not specifying explicit ranks in \code{...}, the position you
#'   want the elements to come after. Works like the \code{after} argument in
#'   \code{\link[base]{append}} or \code{forcats::fct_relevel}.
#'
#'   Ignored if ranks are explicitly provided in \code{...}.
#' @param reprint_header Should the \code{project}'s header be printed to the
#'   console?
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
#' # Reordering with named arguments
#' reorder_authors(project = 1, "Rosetta" = 99, `303` = 2, "5" = 1)
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @name reordering
#' @export
reorder_authors <- function(project, ..., after = 0L, reprint_header = TRUE,
                            archived = FALSE) {

  reorder_assoc(id             = project, ...,
                after          = after,
                reprint_header = reprint_header,
                rds1           = "project",
                rds2           = "author",
                assoc          = "project_author_assoc",
                archived       = archived)
}

#' @rdname reordering
#' @export
reorder_affiliations <- function(author, ..., after = 0L) {

  reorder_assoc(id             = author, ...,
                after          = after,
                reprint_header = FALSE,
                rds1           = "author",
                rds2           = "affiliation",
                assoc          = "author_affiliation_assoc")
}
#########################################


#########################################
#########################################
#' @importFrom rlang .data
reorder_assoc <- function(id, ..., after, reprint_header, rds1, rds2, assoc,
                          archived = TRUE) {

  p_path         <- p_path_internal()

  rds1_path      <- make_rds_path(paste0(rds1, "s"), p_path)
  rds1_tibble    <- get_rds(rds1_path)

  id             <- validate_entry(x          = id,
                                   what       = rds1,
                                   rds_tibble = rds1_tibble,
                                   max.length = 1,
                                   archived   = archived)

  rds1_row       <- dplyr::filter(rds1_tibble, .data$id == !!id)

  rds2_path      <- make_rds_path(paste0(rds2, "s"), p_path)
  rds2_tibble    <- get_rds(rds2_path)

  assoc_path     <- make_rds_path(assoc, p_path)
  assoc_tibble   <- get_rds(assoc_path)

  filtered_assoc <- dplyr::filter(assoc_tibble, .data$id1 == !!id)

  user_order     <- rlang::exprs(...)

  if(rlang::is_named(user_order)) {

    user_order <- unlist(user_order)

    if(!checkmate::test_integerish(user_order, lower = 1L,
                                   max.len = nrow(filtered_assoc),
                                   any.missing = FALSE, unique = TRUE,
                                   null.ok = FALSE)) {
      stop("Ranks must be integers 1 or greater, no two ranks may be the same,",
           " and the number of ranks given must not be greater than the ", rds1,
           "'s total number of ", rds2, "s.")
    }
    names(user_order) <-
      names(user_order) %>%
      validate_entry(what  = rds2, rds_tibble  = rds2_tibble) %>%
      validate_assoc(what  = rds2, rds_tibble  = rds2_tibble,
                     what2 = rds1, rds_tibble2 = filtered_assoc)

    user_order <- user_order[order(user_order)]
  }

  else {

    user_order <-
      sapply(X = user_order, FUN = as.character) %>%
      validate_entry(what  = rds2, rds_tibble  = rds2_tibble) %>%
      validate_assoc(what  = rds2, rds_tibble  = rds2_tibble,
                     what2 = rds1, rds_tibble2 = filtered_assoc)

    user_order <- stats::setNames(object = seq_along(user_order) + after,
                                  nm     = user_order)
  }

  reordered <- setdiff(filtered_assoc$id2, names(user_order))
  purrr::iwalk(user_order,
               function(new_rank, rds2_id) {
                 reordered <<- append(x      = reordered,
                                      values = as.integer(rds2_id),
                                      after  = new_rank - 1)
               })

  assoc_tibble <- change_assoc(assoc_path   = assoc_path,
                               assoc_tibble = assoc_tibble,
                               new          = FALSE,
                               id1          = id,
                               id2          = filtered_assoc$id2)

  assoc_tibble <- change_assoc(assoc_path   = assoc_path,
                               assoc_tibble = assoc_tibble,
                               new          = TRUE,
                               id1          = id,
                               id2          = reordered)

  message(rds1, " info:")
  print(rds1_row)

  message("\nReordered ", rds1, " ", rds2, "s:")
  print(assoc_tibble %>%
          dplyr::filter(.data$id1 == !!id) %>%
          dplyr::left_join(rds2_tibble, by = c("id2" = "id")) %>%
          dplyr::select(-"id1") %>%
          dplyr::rename(!!paste0(rds2, "_id") := "id2"))

  if(reprint_header) {
    print_header_internal(
      project_id           = id,
      p_path               = p_path,
      project_row          = rds1_row,
      authors_tibble       = rds2_tibble,
      project_author_assoc = assoc_tibble
    )
  }
}
################################################################################
################################################################################
