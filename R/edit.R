################################################################################
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
edit_project <- function(project,               title         = NA,
                         authors,               current_owner = NA,
                         creator        = NA,   corresp_auth  = NA,
                         stage          = NA,   deadline_type = NA,
                         deadline       = NA,   status        = NA,
                         reprint_header = TRUE) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  
  projects_tibble <- get_rds(projects_path)
  
  project         <- validate_entry(project,
                                    what       = "project",
                                    max.length = 1,
                                    rds_tibble = projects_tibble)
  
  authors_tibble  <- "authors" %>% make_rds_path(p_path) %>% get_rds
  
  assoc_path      <- make_rds_path("project_author_assoc", p_path)
  assoc_tibble    <- get_rds(assoc_path)
  
  filtered_assoc  <- dplyr::filter(assoc_tibble, .data$id1 == project)
  
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
  if(!is.null(current_owner) && !is.na(current_owner)) {
    current_owner <- validate_entry(x = current_owner,
                                    what = "author",
                                    rds_tibble = authors_tibble)
    
    if(current_owner %in% authors$remove) {
      stop("The value of current_owner must not be in authors$remove")
    }
    
    # If current_owner isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if(!(current_owner %in% authors$add)) {
      validate_entry(
        x                = current_owner,
        what             = "author",
        rds_tibble       = filtered_assoc %>% dplyr::select(id = id2),
        message_addendum = paste0(" in project's author list. ",
                                  "Add to authors$add as well"))
    }
  }
  
  if(!is.null(corresp_auth) && !is.na(corresp_auth)) {
    corresp_auth <- validate_entry(x = corresp_auth,
                                    what = "author",
                                    rds_tibble = authors_tibble)
    
    if(corresp_auth %in% authors$remove) {
      stop("The value of corresp_auth must not be in authors$remove")
    }
    
    # If corresp_auth isn't in authors$add, makes sure it's already in the
    # author list of the user-specified project
    if(!(corresp_auth %in% authors$add)) {
      validate_entry(
        x                = corresp_auth,
        what             = "author",
        rds_tibble       = filtered_assoc %>% dplyr::select(id = id2),
        message_addendum = paste0(" in project's author list. ",
                                  "Add to authors$add as well"))
    }
  }
  
  if(!is.null(creator) && !is.na(creator)) {
    creator <- validate_entry(x = creator,
                                   what = "author",
                                   rds_tibble = authors_tibble)
  }
  ###########################################
  ###########################################
  
  new_project_row <- change_table(rds_name      = "projects",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  action        = "edit",
                                  id            = project,
                                  title         = title,
                                  current_owner = current_owner,
                                  corresp_auth  = corresp_auth,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = as.Date(deadline),
                                  status        = status)
  
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
  print(tibble::as_tibble(new_project_row))
  
  message("\nEdited project's authors:")
  if(nrow(filtered_assoc) == 0) {
    print("None")
  }
  else {
    print(filtered_assoc %>% 
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename("author_id" = "id2"))
  }
  
  if(reprint_header) {
    authors_affils_header(
      project_id               = project,
      authors_tibble           = authors_tibble,
      affiliations_tibble      = get_rds(make_rds_path("affiliations",
                                                       p_path)),
      project_author_assoc     = assoc_tibble,
      author_affiliation_assoc = get_rds(make_rds_path(
                                           "author_affiliation_assoc",
                                           p_path))) %>%
      taa_to_console(title = new_project_row$title, header = .)
  }
}
################################################################################
################################################################################


################################################################################
#' @importFrom rlang .data
#' @export
edit_author <- function(author,           last_name    = NA,
                        given_names = NA, affiliations,
                        title       = NA, degree       = NA,
                        email       = NA) {
  
  p_path             <- p_path_internal()
  
  authors_path       <- make_rds_path("authors", p_path)
  
  authors_tibble     <- get_rds(authors_path)
  
  author             <- validate_entry(author,
                                       what        = "author",
                                       max.length  = 1,
                                       rds_tibble  = authors_tibble)
  
  affiliations_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  assoc_path      <- make_rds_path("author_affiliation_assoc", p_path)
  assoc_tibble    <- get_rds(assoc_path)
  
  filtered_assoc  <- 
  
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
  
  new_author_row <- change_table(rds_name           = "authors", 
                                 rds_path           = authors_path,
                                 rds_tibble         = authors_tibble,
                                 action             = "edit",
                                 id                 = author,
                                 last_name          = last_name,
                                 given_names        = given_names,
                                 title              = title,
                                 degree             = degree,
                                 email              = email)
  
  if(length(affiliations$add) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = TRUE,
                   id1          = author,
                   id2          = affiliations$add)
  }
  if(length(affiliations$remove) > 0) {
    assoc_tibble <-
      change_assoc(assoc_path   = assoc_path,
                   assoc_tibble = assoc_tibble,
                   new          = FALSE,
                   id1          = author,
                   id2          = affiliations$remove)
  }
  
  filtered_assoc <- dplyr::filter(assoc_tibble, .data$id1 == author)
  
  message("Edited author:")
  print(tibble::as_tibble(new_author_row))

  message("\nEdited author's affiliations:")
  if(nrow(filtered_assoc) == 0) {
    print("None")
  }
  else {
    print(filtered_assoc %>%
            dplyr::left_join(affiliations_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>% 
            dplyr::rename("affiliation_id" = "id2"))
  }
  
}
################################################################################


################################################################################
#' @export
edit_affiliation <- function(affiliation,           department_name  = NA,
                             institution_name = NA, address          = NA) {
  
  p_path              <- p_path_internal()
  
  affiliations_path   <- make_rds_path("affiliations", p_path)
  
  affiliations_tibble <- get_rds(affiliations_path)
  
  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        max.length = 1,
                                        rds_tibble = affiliations_tibble)
  
  message("Edited affiliation:")
  
  change_table(rds_name         = "affiliations",
               rds_path         = affiliations_path,
               rds_tibble       = affiliations_tibble,
               action           = "edit",
               id               = affiliation,
               department_name  = department_name,
               institution_name = institution_name,
               address          = address)
}
################################################################################





parse_formula <- function(formula, what, what2, main_tibble, assoc_tibble) {
  
  if(!rlang::is_formula(formula)) {
    stop(what, "s argument must be a formula. See help(edit_", what2, ")")
  }
  
  # This converts all integers in the formula to backquoted names e.g. `5`.
  # This is necessary because the terms() formula that comes later does not
  # know how to process plain integers, but it does know how to process names.
  formula <- formula %>% process_formula_numbers %>% as.formula
  
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
    terms() %>% 
    attr("term.labels") %>% 
    sapply(FUN       = function(x) as.character(rlang::parse_expr(x)),
           USE.NAMES = FALSE)
  
  remove <- setdiff(all.vars(formula), add)
  
  if(length(remove) > 0) {
    
    # Validates the remove list against the main database, making sure that the
    # authors or affiliations actually exist in the database, first turning any
    # IDs into numbers (via the ifelse() statement)
    remove <-
      sapply(
        X   = remove,
        FUN = function(x) {
          validate_entry(
            x          = ifelse(test = is.na(suppressWarnings(as.numeric(x))),
                                yes  = x,
                                no   = as.numeric(x)),
            what       = what,
            max.length = 1,
            rds_tibble = main_tibble)
        },
        USE.NAMES = FALSE)
    
    if(anyDuplicated(remove) > 0) {
      stop("Duplicates detected in list of ", authors, " to remove. ",
           "Check ", what, " argument.")
    }
    
    # Makes sure the elements to remove are actually listed as elements of
    # the user-specified project/author
    validate_entry(
      x                = remove,
      what             = what,
      rds_tibble       = assoc_tibble %>% dplyr::select("id" = "id2"),
      message_addendum = paste0(" in ", what2, "'s ", what, " list")
    )
  }
  
  if(length(add) > 0) {
    
    # Validates the remove list against the formula tibble, first
    # turning any author IDs from characters to integers (via the ifelse()
    # statement)
    add <-
      sapply(
        X   = add,
        FUN = function(x) {
          validate_entry(
            x          = ifelse(test = is.na(suppressWarnings(as.numeric(x))),
                                y    = x,
                                no   = as.numeric(x)),
            what       = "author",
            max.length = 1,
            rds_tibble = main_tibble)
        },
        USE.NAMES = FALSE)
    
    if(anyDuplicated(add) > 0) {
      stop("Duplicates detected in list of formula to add. ",
           "Check formula argument.")
    }
    
    # Makes sure the formula in add are not already in the
    # user-specified project's author list
    if(any(add %in% assoc_tibble$id2)) {
      print(main_tibble %>% dplyr::filter(id %in%
                                            add[add %in% assoc_tibble$id2]))
      stop("The above ", what, "s are already on the ", what2, "'s ",
           what, " list.")
    }
  }
  #################################################
  #################################################
  
  return(list(add = add, remove = remove))
}
