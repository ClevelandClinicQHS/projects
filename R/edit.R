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
                                    rds_tibble = projects_tibble,
                                    max.length = 1)
  
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
    current_owner <- validate_entry(x          = current_owner,
                                    what       = "author",
                                    rds_tibble = authors_tibble)
    
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
  
  if(!is.null(corresp_auth) && !is.na(corresp_auth)) {
    corresp_auth <- validate_entry(x          = corresp_auth,
                                   what       = "author",
                                   rds_tibble = authors_tibble)
    
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
  
  if(!is.null(creator) && !is.na(creator)) {
    creator <- validate_entry(x          = creator,
                              what       = "author",
                              rds_tibble = authors_tibble)
  }
  ###########################################
  ###########################################
  
  new_project_row <- change_table(action        = "edit",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  id            = project,
                                  title         = title,
                                  current_owner = current_owner,
                                  corresp_auth  = corresp_auth,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = as.Date(deadline),
                                  status        = status,
                                  path          = NA_character_)
  
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
            dplyr::select(-.data$id1) %>% 
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
    # project %>% 
    #   
    #   authors_affils_header(
    #     corresp_auth             = corresp_auth,
    #     authors_tibble           = authors_tibble,
    #     affiliations_tibble      = 
    #       get_rds(make_rds_path("affiliations", p_path)),
    #     project_author_assoc     = assoc_tibble,
    #     author_affiliation_assoc =
    #       get_rds(make_rds_path("author_affiliation_assoc", p_path))) %>%
    #   
    #   taa_to_console(title = new_project_row$title, header = .)
  }
}
################################################################################
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
edit_author <- function(author,              last_name    = NA,
                        given_names = NA,    affiliations,
                        title       = NA,    degree       = NA,
                        email       = NA,    phone        = NA,
                        default     = FALSE) {
  
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
  
  if(!is.logical(default)) {
    stop("The value of the 'default' argument must be either TRUE or FALSE")
  }
  
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
                                 last_name   = last_name,
                                 given_names = given_names,
                                 title       = title,
                                 degree      = degree,
                                 email       = email,
                                 phone       = phone,
                                 default     = default)
  
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
    terms() %>% 
    attr("term.labels") %>% 
    sapply(FUN       = function(x) as.character(rlang::parse_expr(x)),
           USE.NAMES = FALSE)
  
  remove <- setdiff(all.vars(formula), add)
  
  if(length(remove) > 0) {
    
    # Validates the remove list against the main database, making sure that the
    # authors or affiliations actually exist in the database, first turning any
    # IDs into numbers (via the ifelse() statement)
    remove <- validate_mixed(vector = remove, what = what, tibble = main_tibble)
    
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
    add <- validate_mixed(vector = add, what = what, tibble = main_tibble)
      
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


process_formula_numbers <- function(formula) {
  
  as.formula(recursive_number_namer(formula))

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
#' @export
reorder_authors <- function(project, ..., after = 0L, reprint_header = TRUE) {
  
  reorder_assoc(id             = project, ...,
                after          = after,
                reprint_header = reprint_header,
                rds1           = "project",
                rds2           = "author",
                assoc          = "project_author_assoc")
}

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
reorder_assoc <- function(id, ..., after, reprint_header, rds1, rds2, assoc) {
  
  p_path         <- p_path_internal()
  
  rds1_path      <- make_rds_path(paste0(rds1, "s"), p_path)
  rds1_tibble    <- get_rds(rds1_path)
  
  id             <- validate_entry(x          = id,
                                   what       = rds1,
                                   rds_tibble = rds1_tibble,
                                   max.length = 1)
  
  rds1_row       <- dplyr::filter(rds1_tibble, .data$id == !!id)
  
  rds2_path      <- make_rds_path(paste0(rds2, "s"), p_path)
  rds2_tibble    <- get_rds(rds2_path)
  
  assoc_path     <- make_rds_path(assoc, p_path)
  assoc_tibble   <- get_rds(assoc_path)
  
  filtered_assoc <- dplyr::filter(assoc_tibble, .data$id1 == !!id)
  
  user_order     <- rlang::exprs(...)
  
  if(rlang::is_named(user_order)) {
    
    user_order <- unlist(user_order)
    
    checkmate::assert_integerish(user_order, lower = 1L,
                                 len = length(filtered_assoc), 
                                 any.missing = FALSE, unique = TRUE, 
                                 null.ok = FALSE)
    names(user_order) <- 
      names(user_order) %>% 
      validate_mixed(what  = rds2, tibble      = rds2_tibble) %>% 
      validate_assoc(what  = rds2, rds_tibble  = rds2_tibble,
                     what2 = rds1, rds_tibble2 = filtered_assoc)
    
    user_order <- user_order[order(user_order)]
  }
  
  else {
    
    user_order <- 
      sapply(X = user_order, FUN = as.character) %>% 
      validate_mixed(what  = rds2, tibble      = rds2_tibble) %>% 
      validate_assoc(what  = rds2, rds_tibble  = rds2_tibble,
                     what2 = rds1, rds_tibble2 = filtered_assoc)
    
    user_order <- setNames(object = seq_along(user_order) + after,
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
    # id %>% 
    #   
    #   authors_affils_header(
    #     authors_tibble           = rds2_tibble,
    #     
    #     affiliations_tibble      = 
    #       get_rds(make_rds_path("affiliations", p_path)),
    #     
    #     project_author_assoc     = assoc_tibble,
    #     
    #     author_affiliation_assoc =
    #       get_rds(make_rds_path("author_affiliation_assoc", p_path))) %>%
    #   
    #   taa_to_console(title = rds1_row$title, header = .)
  }
}
################################################################################
################################################################################



move_project <- function(project, path) {
  project <- validate_directory()
}