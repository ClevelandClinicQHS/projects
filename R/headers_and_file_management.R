#' @importFrom rlang .data
#' @export
header <- function(project) {

  p_path  <- p_path_internal()

  projects_tibble <- get_rds(make_rds_path("projects", p_path))

  project <- validate_entry(project,
                            what       = "project",
                            max.length = 1,
                            rds_tibble = projects_tibble)

  print_header_internal(project_id  = project,
                        p_path      = p_path,
                        project_row = dplyr::filter(projects_tibble,
                                                    .data$id == project))
}



#' @importFrom rlang .data
print_header_internal <- function(
  project_id,
  p_path                   = p_path_internal(),
  project_row              = dplyr::filter(get_rds(make_rds_path("projects",
                                                                 p_path)),
                                           .data$id == project_id),
  authors_tibble           = get_rds(make_rds_path("authors", p_path)),
  affiliations_tibble      = get_rds(make_rds_path("affiliations", p_path)),
  project_author_assoc     = get_rds(make_rds_path("project_author_assoc",
                                                   p_path)),
  author_affiliation_assoc = get_rds(make_rds_path("author_affiliation_assoc",
                                                   p_path)))
{
  project_authors <-
    dplyr::filter(project_author_assoc, .data$id1 == project_id)$id2

  taa_to_console(
    title  = project_row$title,
    header = aa_header(project_id               = project_id,
                       corresp_auth             = project_row$corresp_auth,
                       authors_tibble           = authors_tibble,
                       affiliations_tibble      = affiliations_tibble,
                       project_authors          = project_authors,
                       author_affiliation_assoc = author_affiliation_assoc))
}


taa_to_console <- function(title, header) {
  cat('\ntitle: "', title, '"', '\n\n\n', sep = "")
  cat(header, sep = '\n')
}




build_protocol_report <- function(protocol, p_path, project_id, title,
                                  corresp_auth, authors_tibble,
                                  affiliations_tibble, project_authors,
                                  aa_assoc_tibble, use_bib, pXXXX_name) {

  protocol        <- protocol_selector(selection = protocol, p_path = pa_path)

  protocol_report <- list(protocol, report)

  yaml_bounds     <- lapply(X = protocol_report, FUN = yaml_bounds)

  if(!missing(project_authors)) {
    protocol_report <- purrr::map2(
      .x = protocol_report,
      .y = yaml_bounds,
      .f = ~insert_aa(vector                   = .x,
                      project_id               = project_id,
                      yaml_bounds              = .y,
                      corresp_auth             = corresp_auth,
                      authors_tibble           = authors_tibble,
                      affiliations_tibble      = affiliations_tibble,
                      project_authors          = project_authors,
                      author_affiliation_assoc = aa_assoc_tibble))
  }

  protocol_report <- purrr::map2(
    .x = protocol_report,
    .y = yaml_bounds,
    .f = ~protocol_report_yaml(vector      = .x,
                               title       = title,
                               yaml_bounds = .y,
                               use_bib     = use_bib,
                               pXXXX_name  = pXXXX_name)
    )

  return(setNames(protocol_report, c("protocol", "report")))
}



protocol_selector <- function(selection, p_path) {
  ############################
  # Processing of protocol choice.
  protocol_upper   <- toupper(selection)
  protocol_choices <- eval(formals("new_project")[["protocol"]])
  protocol_matches <- pmatch(protocol_upper, protocol_choices)

  # If the user did not leave protocol blank or choose "STROBE" or "CONSORT"
  if(identical(protocol_matches, NA_integer_)) {

    protocol_path <- fs::path(p_path, ".templates", selection)

    if(!fs::file_exists(protocol_path)) {
      stop("protocol does not match ", paste(protocol_choices, collapse = ", "),
           ", and no custom template found at the file path ", selection,
           " (check the case, and don't forget file extension).")
    }

    selection  <- readr::read_lines(protocol_path)
  }

  else {
    selection <- protocol_choices[protocol_matches[1]]

    if(selection == "STROBE") {
      selection <- STROBE_template
    }
    else if(selection == "CONSORT") {
      selection <- CONSORT_template
    }

    selection <- append(selection, protocol, after = 0L)
  }

  return(selection)
}


yaml_bounds <- function(vector) {

  yaml_bounds <- grep("^---$", vector)
  if(length(yaml_bounds) < 2) {
    stop("Custom template must have a yaml header. (Check that there are no ",
         "spaces before or after each ---)")
  }

  return(yaml_bounds)
}


insert_aa <- function(vector, project_id, yaml_bounds, corresp_auth,
                      authors_tibble, affiliations_tibble,
                      project_authors, author_affiliation_assoc) {

  aa_header   <- aa_header(project_id = project_id,
                           corresp_auth = corresp_auth,
                           authors_tibble = authors_tibble,
                           affiliations_tibble = affiliations_tibble,
                           project_authors          = project_authors,
                           author_affiliation_assoc = author_affiliation_assoc)

  vector <- append(x      = vector,
                   values = c("", aa_header, "", "\\pagebreak", ""),
                   after  = yaml_bounds[2])

  return(vector)
}



protocol_report_yaml <- function(vector, title, yaml_bounds, use_bib,
                                 pXXXX_name) {

  yaml <- c(paste0('title: "', title, '"'),
            "output:",
            "  word_document: default",
            "  html_document:",
            "    css: style.css")

  if(use_bib) {
    yaml <- append(yaml, paste0("bibliography: ", pXXXX_name, ".bib"))
  }

  vector <- append(x      = vector,
                     values = yaml,
                     after  = yaml_bounds[1])

  return(vector)
}




build_datawork_analysis <- function(template, what, p_path, pXXXX_name) {

  if(is.null(template)) {
    template <- get(what)
  }
  else {

    template_path <- fs::path(p_path, ".templates", template)

    if(fs::file_exists(path = template_path)) {
      template <- readr::read_lines(file = template_path)
    }
    else {
      stop(toupper(what), " template does not exist at:\n", template_path,
           "\nCheck case and file extensions.")
    }
  }

  yaml_bounds <- yaml_bounds(vector = template)

  template    <- append(x      = template,
                        values = paste0('title: "', pXXXX_name, ' ', what, '"'),
                        after  = yaml_bounds[1])

  return(template)
}



#' @importFrom rlang .data
#' @importFrom tibble tibble
aa_header <- function(project_id, corresp_auth, authors_tibble,
                      affiliations_tibble, project_authors,
                      author_affiliation_assoc) {

  # The left_join/select/rename combo was used instead of semi_join so that the
  # order in project_author_assoc would be preserved
  project_authors <-
    dplyr::left_join(tibble(id = project_authors), authors_tibble, by = "id")

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
    dplyr::left_join(affiliations_tibble, by = c("id2" = "id"))


  ############################################################
  # Construction of affiliations line to go in 01_protocol.Rmd

  if(nrow(aa_assoc_complete) > 0) {

    # A tibble of the unique affiliations associated with the project, with a
    # superscript assigned to each
    unique_affiliations <-
      aa_assoc_complete %>%
      dplyr::select(-"id1") %>%
      dplyr::distinct() %>%
      dplyr::mutate(superscript = 1:nrow(.))

    # In effect this adds the superscripts created in the previous command to
    # aa_assoc_complete
    aa_assoc_complete <-
      unique_affiliations %>%
      dplyr::select(.data$id2, .data$superscript) %>%
      dplyr::right_join(aa_assoc_complete, by = "id2")


    affiliations_lines <- ""
    for(a in 1:nrow(unique_affiliations)) {

      affiliation_line <- paste0("| ^", a, "^ ",
                                 unique_affiliations$department_name[a])

      if(!is.na(unique_affiliations$institution_name[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$institution_name[a])
      }

      if(!is.na(unique_affiliations$address[a])) {
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
  if(nrow(project_authors) > 0) {
    author_line         <- "**_"

    for(x in 1:nrow(project_authors)) {

      if(x != 1) {
        author_line <- paste0(author_line, " ")

        if(x == nrow(project_authors) && x > 1) {
          author_line <- paste0(author_line, "and ")
        }
      }

      first_piece <- TRUE

      if(!is.na(project_authors$given_names[x])) {
        author_line <- paste0(author_line, project_authors$given_names[x])
        first_piece <- FALSE
      }

      if(!is.na(project_authors$last_name[x])) {
        if(!first_piece) {
          author_line <- paste0(author_line, " ")
        }

        author_line <- paste0(author_line, project_authors$last_name[x])
      }

      if(!is.na(project_authors$degree[x])) {
        author_line <- paste0(author_line, ", ", project_authors$degree[x])
      }

      if(x != nrow(project_authors) && nrow(project_authors) > 2) {
        author_line <- paste0(author_line, ";")
      }

      x_affiliations <- dplyr::filter(aa_assoc_complete,
                                      .data$id1 == project_authors$id[x])

      if(nrow(x_affiliations) > 0) {
        author_line <-
          paste0(author_line,
                 "^",
                 paste(sort(x_affiliations$superscript), collapse = ","),
                 # ifelse(
                 #   test = isTRUE(project_authors$id[x] == corresp_auth),
                 #   yes  = "\\*",
                 #   no   = ""),
                 "^")
      }

      if(isTRUE(project_authors$id[x] == corresp_auth)) {
        author_line <- paste0(author_line, "\\*")
      }
    }

    author_line <- paste0(author_line, "_**")
  }
  else {
    author_line <- character()
  }

  if(is.na(corresp_auth)) {
    corresp_lines <- character()
  }
  else {
    corresp_auth_row <- dplyr::filter(project_authors, .data$id == corresp_auth)
    corresp_affils   <- dplyr::filter(aa_assoc_complete,
                                      .data$id1 == corresp_auth)
    corresp_lines    <- c(ifelse(length(affiliations_lines) == 0, "", "|"),
                          "| \\* Corresponding author")

    if(nrow(corresp_affils) > 0) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_affils$address[1]))
    }

    if(!is.na(corresp_auth_row$phone)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$phone))
    }

    if(!is.na(corresp_auth_row$email)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$email))
    }
  }

  ######################################################
  ######################################################

  return(c(author_line, affiliations_lines, corresp_lines, "", "Funding:"))
}



write_project_files <- function(pXXXX_path, protocol_report, datawork, analysis,
                                use_bib, pXXXX_name) {

  fs::dir_create(fs::path(pXXXX_path, c("data", "data_raw", "progs",
                                        "manuscript", "figures")))

  readr::write_lines(protocol_report$protocol,
                     fs::path(pXXXX_path, "progs/01_protocol", ext = "Rmd"))

  readr::write_lines(datawork,
                     fs::path(pXXXX_path, "progs/02_datawork", ext = "Rmd"))

  readr::write_lines(analysis,
                     fs::path(pXXXX_path, "progs/03_analysis", ext = "Rmd"))

  readr::write_lines(protocol_report$report,
                     fs::path(pXXXX_path, "progs/04_report", ext = "Rmd"))

  if(use_bib) {
    readr::write_lines("",
                       fs::path(pXXXX_path, "progs", pXXXX_name, ext = "bib"))
  }

  readr::write_lines(css,
                     fs::path(pXXXX_path, "progs/style", ext = "css"))

  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
}




#' @importFrom rlang .data
#' @export
new_project_group <- function(path) {

  p_path <- p_path_internal()

  path   <- validate_directory(path, p_path, make_directories = T)

  if(fs::path_ext(path) != "") {
    stop("\nMust be a directory and not a file (i.e., must not have a file ",
         "extension, which is ", fs::path_ext(path), " in this case).")
  }

  if(fs::dir_exists(path)) {
    stop("\nDirectory already exists.")
  }

  fs::dir_create(path)

  message("\nThe following directory was created:\n", path)
}



#' @importFrom rlang .data
#' @export
move_project <- function(project, path = "", make_directories = FALSE) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(x          = project,
                                    what       = "project",
                                    rds_tibble = projects_tibble,
                                    max.length = 1)

  path            <- validate_directory(path             = path,
                                        p_path           = p_path,
                                        make_directories = make_directories)

  project_row     <- dplyr::filter(projects_tibble, .data$id == project)

  print(project_row)

  if(fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("Are you sure you want to move this project folder so ",
                     "that its new file path is\n",
                     fs::path(path, fs::path_file(project_row$path)),
                     "\n? (y/n)"),
      n_msg = paste0('Move not completed. To move this project, try again ',
                     'and enter "y".')
    )
  }
  else {
    user_prompt(
        msg   = paste0("\nDirectory not found:\n", path,
                       "\n\nWould you like to create it and move the project ",
                       "folder there, so that its new file path will be\n",
                       fs::path(path, fs::path_file(project_row$path)),
                       "\n\n? (y/n)"),
        n_msg = paste0("\nMove not completed. To move this project, try again ",
                       'and enter "y"'))

    fs::dir_create(path)
  }

  fs::file_move(path = project_row$path, new_path = path)

  project_row$path <-
    fs::path(path, fs::path_file(project_row$path)) %>%
    unclass()

  do.call(what = change_table,
          args = c(list(action        = "edit",
                        rds_path      = projects_path,
                        rds_tibble    = projects_tibble),
                   as.list(project_row)))

  message("\nProject ", project, " moved so that its new path is\n",
          project_row$path)
}



#' @importFrom rlang .data
#' @export
copy_project <- function(project_to_copy, path = "", new_id = NA,
                         make_directories = FALSE) {

  p_path           <- p_path_internal()

  pa_assoc_path    <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble  <- get_rds(pa_assoc_path)

  projects_path    <- make_rds_path("projects", p_path)
  projects_tibble  <- get_rds(projects_path)

  project          <- validate_entry(x          = project_to_copy,
                                     what       = "project",
                                     rds_tibble = projects_tibble,
                                     max.length = 1)

  path             <- validate_directory(path             = path,
                                         p_path           = p_path,
                                         make_directories = make_directories)

  project_row      <- dplyr::filter(projects_tibble, .data$id == project)

  old_name         <- make_project_name(project)

  old_path         <- project_row$path

  print(project_row)

  project_row$id   <- validate_new(id         = new_id,
                                   what       = "project",
                                   rds_tibble = projects_tibble)

  pXXXX_name       <- make_project_name(project_row$id)

  project_row$path <- fs::path(path, pXXXX_name) %>% unclass()

  if(fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("\nAre you sure you want to copy this project into the ",
                     "new directory\n", project_row$path, "\n\n? (y/n)"),
      n_msg = paste0('Copy not completed. To copy this project, try again ',
                     'and enter "y".')
    )
  }
  else {
    user_prompt(
      msg   = paste0("\nDirectory not found:\n", path,
                     "\n\nWould you like to create it and copy the above ",
                     "project there, so that its file path will be\n",
                     project_row$path, "\n\n? (y/n)"),
      n_msg = paste0("\nCopy not completed. To copy this project, try again ",
                     'and enter "y"'))

    fs::dir_create(path)
  }

  fs::dir_copy(path = old_path, new_path = project_row$path)

  do.call(what = change_table,
          args = c(list(action     = "new",
                        rds_path   = projects_path,
                        rds_tibble = projects_tibble),
                   as.list(project_row)))

  old_assoc <- dplyr::filter(pa_assoc_tibble, .data$id1 == project)

  if(nrow(old_assoc) > 0) {
    change_assoc(assoc_path = pa_assoc_path,
                 assoc_tibble = pa_assoc_tibble,
                 new = TRUE,
                 id1 = project_row$id,
                 id2 = old_assoc$id2)
  }

  message("\nProject ", project_row$id, " below is a copy of project ", project,
          " and is located at\n", project_row$path)
  print(project_row)

  Rproj_path <- grep(pattern = ".Rproj$",
                     x       = fs::dir_ls(project_row$path, recursive = TRUE),
                     value   = TRUE)
  if(length(Rproj_path) == 1 && fs::file_exists(Rproj_path)) {

    new_path <- fs::path(fs::path_dir(Rproj_path), pXXXX_name, ext = "Rproj")
    fs::file_move(path = Rproj_path, new_path = new_path)
    message("\nThe .Rproj file\n", Rproj_path, "\nwas renamed to\n", new_path)
  }

  old_filenames <- grep(pattern = old_name,
                        x       = fs::dir_ls(project_row$path,
                                             recursive = TRUE),
                        value   = TRUE)
  purrr::walk(
    .x = old_filenames,
    .f = function(file) {

      if(fs::file_exists(file)) {
        new_path <- fs::path(fs::path_dir(file),
                             gsub(pattern     = old_name,
                                  replacement = pXXXX_name,
                                  x           = fs::path_file(file)))
        fs::file_move(path = file, new_path = new_path)
      }

      message("\nThe file\n", file, "\nwas renamed to\n", new_path)
    })

  message('\nBe sure to change all instances of \"', old_name, '\" to \"',
          pXXXX_name, '\" as necessary.\n')
}
