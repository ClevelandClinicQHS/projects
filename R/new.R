


################################################################################
#' @importFrom tibble tibble
#' @export
new_project <- function(title            = NA, authors,
                        current_owner    = NA, creator  = NA,
                        corresp_auth     = NA, stage    = NA,   
                        deadline_type    = NA, deadline = NA,
                        id               = NA, status   = "just created",
                        path             = NA,
                        protocol         = c("STROBE", "CONSORT"),
                        make_directories = FALSE) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)
  
  authors_tibble  <- get_rds(make_rds_path("authors", p_path))
  
  pa_assoc_path      <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble    <- get_rds(pa_assoc_path)
  
  affiliations_tibble <-
    get_rds(make_rds_path("affiliations", p_path))
  
  author_affiliation_assoc <-
    get_rds(make_rds_path("author_affiliation_assoc", p_path))
    
  #######################
  # Validation of id, path
  id              <- validate_new(id         = id,
                                  what       = "project",
                                  rds_tibble = projects_tibble)
  
  if(is.na(path)) {
    path <- p_path
  }
  else {
    path <- validate_directory(path             = fs::path(p_path, path),
                               make_directories = make_directories)
  }
  
  pXXXX_name      <- make_project_name(id)
  pXXXX_path      <- make_project_path(pXXXX_name, path)
  ########################
  ########################
  
  
  ########################
  # Validation of authors, creator, corresp_auth, current_owner
  
  if(nrow(authors_tibble) == 0 && !all(is.na(creator), is.na(corresp_auth),
                                       is.na(current_owner), missing(authors))){
    stop("Can't set authors, creator, corresp_auth, or current owner until an",
         " author is created. Run new_author()")
  }
  
  if(!is.na(corresp_auth)) {
    corresp_auth <- validate_entry(corresp_auth,
                                   what        = "author",
                                   max.length = 1,
                                   rds_tibble = authors_tibble)
  }
  
  # If creator is left blank, the default author is used. If the default author
  # is not set, it will be NA.
  if(is.na(creator)) {
    creator <- get_default_author_internal(authors_tibble = authors_tibble)
  }
  else {
    creator <- validate_entry(creator,
                              what       = "author",
                              max.length = 1,
                              rds_tibble = authors_tibble)
  }
  
  if(!is.na(current_owner)) {
    current_owner <- validate_entry(current_owner,
                                    what       = "author",
                                    max.length = 1,
                                    rds_tibble = authors_tibble)
  }
  
  if(missing(authors)) {
    if(is.na(current_owner)) {
      if(!is.na(creator)) {
        authors <- current_owner <- creator
      }
    }
    else {
      authors <- current_owner
    }
  }
  else {
    authors <- validate_entry(authors,
                              what = "author",
                              rds_tibble = authors_tibble,
                              max.length = 9999)
    if(is.na(current_owner)) {
      current_owner <- authors[1]
    }
    else if(!(current_owner %in% authors)) {
      authors <- c(authors[1],
                   authors[-c(1, length(authors))],
                   current_owner,
                   authors[-1][length(authors) - 1])
    }
  }
  ############################
  ############################
  
  
  ############################
  # Processing of protocol choice.
  protocol_upper   <- toupper(protocol)
  protocol_choices <- eval(formals()[["protocol"]])
  protocol_matches <- pmatch(protocol_upper, protocol_choices)
  
  # If the user did not leave protocol blank or choose "STROBE" or "CONSORT"
  if(identical(protocol_matches, NA_integer_)) {
    
    template_path <- fs::path(p_path, "templates", protocol)
    
    if(!fs::file_exists(template_path)) {
      stop("protocol does not match ", paste(protocol_choices, collapse = ", "), 
           ", and no custom template found at the file path ", protocol,
           " (check the case, and don't forget file extension).")
    }
    
    protocol  <- readr::read_lines(template_path)
  }
  
  else {
    protocol <- protocol_choices[protocol_matches[1]]
    
    if(protocol == "STROBE")  protocol <- STROBE_template
    else if(protocol == "CONSORT") protocol <- CONSORT_template
  }
  
  yaml_bounds <- grep("^---$", protocol)
  if(length(yaml_bounds) < 2) {
    stop("Custom template must have a yaml header. (Check that there are no ",
         "spaces before or after each ---)")
  }
  ###########################
  ###########################
  
  
  # Add new row to project list
  new_project_row <- change_table(action        = "new",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  corresp_auth  = corresp_auth,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = as.Date(deadline),
                                  status        = status,
                                  path          = pXXXX_path)
  
  # Add row(s) to project-author association table
  if(!missing(authors)) {
    pa_assoc_tibble <-
      change_assoc(assoc_path   = pa_assoc_path,
                   assoc_tibble = pa_assoc_tibble,
                   new          = TRUE,
                   id1          = id,
                   id2          = authors)
  }
  
  header <-
    authors_affils_header(project_id               = id,
                          corresp_auth             = corresp_auth,
                          authors_tibble           = authors_tibble,
                          affiliations_tibble      = affiliations_tibble,
                          project_author_assoc     = pa_assoc_tibble,
                          author_affiliation_assoc = author_affiliation_assoc)
  
  ######################################################
  # Construct vector that will become /progs/01_protocol.Rmd
  header   <- c("", header, "", "\\pagebreak", "")
  protocol <- append(protocol, header, after = yaml_bounds[2])

  title    <- tools::toTitleCase(title)
  protocol <- append(protocol,
                     paste0('title: "', title, '"'),
                     after = yaml_bounds[1])
  ######################################################
  ######################################################
  
  
  
  ######################################################
  # Actual creation of the folders and files
  fs::dir_create(fs::path(pXXXX_path, c("data", "data_raw", "progs",
                                        "manuscript", "figures")))
  
  readr::write_lines(protocol,
                     fs::path(pXXXX_path, "progs/01_protocol", ext = "Rmd"))
  
  readr::write_lines("Bibliography",
                     fs::path(pXXXX_path, pXXXX_name, ext = "bib"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  ######################################################
  ######################################################
  
  message("Project ", id, " has been created at ", pXXXX_path)
  print(dplyr::select(new_project_row,
                      -c("current_owner", "creator", "corresp_auth")))
  
  if(!missing(authors)) {
    message("\nNew project's authors:")
    print(pa_assoc_tibble %>% 
            dplyr::filter(.data$id1 == id) %>% 
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(author_id = id2))
  }
  
  if(!is.na(current_owner)) {
    message("\nCurrent owner:")
    print(dplyr::filter(authors_tibble, .data$id == current_owner))
  }
  
  if(!is.na(creator)) {
    message("\nCreator:")
    print(dplyr::filter(authors_tibble, .data$id == creator))
  }
  
  if(!is.na(corresp_auth)) {
    message("\nCorresponding author:")
    print(dplyr::filter(authors_tibble, .data$id == corresp_auth))
  }
}
################################################################################




################################################################################
#' @importFrom rlang .data
#' @export
new_author <- function(last_name = NA,    given_names  = NA,
                       title     = NA,    affiliations,
                       degree    = NA,    email        = NA,
                       phone     = NA,    id           = NA,
                       default   = FALSE) {
  
  p_path         <- p_path_internal()
  
  authors_path   <- make_rds_path("authors", p_path)
  authors_tibble <- get_rds(authors_path)
  
  id             <- validate_new(id         = id,
                                 what       = "author",
                                 rds_tibble = authors_tibble)
  
  if(!missing(affiliations)) {
    aa_assoc_path       <- make_rds_path("author_affiliation_assoc", p_path)
    aa_assoc_tibble     <- get_rds(aa_assoc_path)
    affiliations_tibble <- get_rds(make_rds_path("affiliations", p_path))
    
    if(nrow(affiliations_tibble) == 0) {
      stop("Can't set affiliations until an affiliation is created. ",
           "Run new_affiliation()")
    }
    
    affiliations        <- validate_entry(affiliations,
                                          what        = "affiliation",
                                          rds_tibble  = affiliations_tibble)
  }
  
  #######################
  # Handling of default
  if(!is.logical(default)) {
    stop("The argument 'default' must be either TRUE or FALSE")
  }
  
  if(nrow(authors_tibble) == 0 && !default) {
    default <-
      user_prompt(msg   = paste0("This is the first author. Do you want to ",
                                 "make this author the default author? (y/n)"),
                  error = FALSE)
  }
  ######################
  ######################
  
  new_author_row <- change_table(action      = "new",
                                 rds_path    = authors_path,
                                 rds_tibble  = authors_tibble,
                                 id          = id,
                                 last_name   = last_name,
                                 given_names = given_names,
                                 title       = title,
                                 degree      = degree,
                                 email       = email,
                                 phone       = phone,
                                 default     = default)
  
  message("New author:")
  print(new_author_row)
  
  message("\nNew author's affiliations:")
  if(!missing(affiliations)) {
    new_author_affiliations <- 
      change_assoc(assoc_path   = aa_assoc_path,
                   assoc_tibble = aa_assoc_tibble,
                   new          = TRUE,
                   id1          = id,
                   id2          = affiliations) %>% 
      dplyr::filter(.data$id1 == id)
    
    #message("\nNew author's affiliations:")
    print(new_author_affiliations %>%
            dplyr::filter(.data$id1 == id) %>%
            dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
            dplyr::select(-"id1") %>% 
            dplyr::rename("affiliation_id" = "id2"))
  }
  else {
    print("None.")
  }
}
################################################################################


################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA, id               = NA) {
  
  affiliations_path   <- make_rds_path("affiliations")
  affiliations_tibble <- get_rds(affiliations_path)
  
  id                  <- validate_new(id         = id,
                                      what       = "affiliations",
                                      rds_tibble = affiliations_tibble)
  
  message("New affiliation:")
  print(change_table(action           = "new",
                     rds_path         = affiliations_path,
                     rds_tibble       = affiliations_tibble,
                     id               = id,
                     department_name  = department_name,
                     institution_name = institution_name,
                     address          = address))
}
################################################################################
