if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", ":="))

make_project_name <- function(project_id) {
  paste0("p", stringr::str_pad(project_id, width = 4, side = "left", pad = "0"))
}

make_project_path <- function(project_name, path = p_path_internal()) {
  fs::path(path, project_name) %>% unclass()
}


check_all_rds <- function() {
  purrr::walk(c("affiliations", "author_affiliation_assoc", "authors",
                "project_author_assoc", "projects"),
              function(x) {
                print(x)
                print(readRDS(fs::path(projects_folder(), ".metadata", x,
                                       ext = "rds")))
              })
}

user_prompt <- function(msg, y_msg, n_msg, error = TRUE) {

  if(isTRUE(getOption('knitr.in.progress'))) {
    prompt <- "y"
  }
  else {
    prompt <- NULL
  }

  while(is.null(prompt) || !(prompt %in% c("y", "n"))) {
    if(!is.null(prompt)) {
      message("\nInvalid input.\n")
    }
    message(msg)
    prompt <- tolower(readLines(con = stdin(), n = 1L))
  }
  if(prompt == "n") {
    if(error) {
      stop(n_msg, call. = FALSE)
    }
    else if(!missing(n_msg)) {
      message(n_msg)
    }
  }
  else if(!missing(y_msg)) {
    message(y_msg)
  }
  return(prompt == "y")
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



build_protocol_report <- function(vector, what, project_id, title,
                                  corresp_auth, authors_tibble,
                                  affiliations_tibble, project_authors,
                                  aa_assoc_tibble, use_bib, pXXXX_name) {

  yaml_bounds     <- yaml_bounds(vector = vector, what = what)

  vector <- vector %>%
    insert_aa(project_id               = project_id,
              yaml_bounds              = yaml_bounds,
              corresp_auth             = corresp_auth,
              authors_tibble           = authors_tibble,
              affiliations_tibble      = affiliations_tibble,
              project_authors          = project_authors,
              author_affiliation_assoc = aa_assoc_tibble) %>%
    protocol_report_yaml(title       = title,
                         yaml_bounds = yaml_bounds,
                         use_bib     = use_bib,
                         pXXXX_name  = pXXXX_name)

  return(vector)
}

insert_aa <- function(vector, project_id, yaml_bounds, corresp_auth,
                      authors_tibble, affiliations_tibble,
                      project_authors, author_affiliation_assoc) {

  if(is.null(project_authors)) {
    aa_header <- character()
  }
  else {
    aa_header <- aa_header(project_id               = project_id,
                           corresp_auth             = corresp_auth,
                           authors_tibble           = authors_tibble,
                           affiliations_tibble      = affiliations_tibble,
                           project_authors          = project_authors,
                           author_affiliation_assoc = author_affiliation_assoc)
  }

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


build_datawork_analysis <- function(vector, what, p_path, pXXXX_name) {

  yaml_bounds <- yaml_bounds(vector = vector, what = what)

  vector    <- append(x      = vector,
                      values = paste0('title: "', pXXXX_name, ' ', what, '"'),
                      after  = yaml_bounds[1])

  return(vector)
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
    corresp_lines    <- c("", #ifelse(length(affiliations_lines) == 0, "", "|"),
                          "| \\* Corresponding author")

    if(nrow(corresp_affils) > 0 &&
       length(stats::na.omit(corresp_affils$address)) > 0){
      corresp_lines <-
        append(corresp_lines,
               paste0("|   ", stats::na.omit(corresp_affils$address)[1]))
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

  return(c(author_line, affiliations_lines, corresp_lines, "", "| Funding:"))
}



write_project_files <- function(pXXXX_path, files, use_bib, pXXXX_name) {

  fs::dir_create(fs::path(pXXXX_path, c("data", "data_raw", "progs",
                                        "manuscript", "figures")))

  file_names <- c("01_protocol.Rmd", "02_datawork.Rmd", "03_analysis.Rmd",
                  "04_report.Rmd", "style.css", paste0(pXXXX_name, ".Rproj"))

  if(use_bib) {
    files      <- append(x      = files,
                         values = "",
                         after  = 0)
    file_names <- append(x      = file_names,
                         values = paste0(pXXXX_name, ".bib"),
                         after  = 0)
  }

  purrr::walk2(
    .x = files,
    .y = fs::path(pXXXX_path,
                  c(rep("progs", length(files) - 1), ""),
                  file_names),
    .f = readr::write_lines)
}


clear_special_author <- function(author, projects_path, projects_tibble) {

  if(nrow(projects_tibble) > 0) {

    is.na(projects_tibble[c("current_owner", "creator", "corresp_auth")]) <-
      projects_tibble %>%
      dplyr::select("current_owner", "creator", "corresp_auth") %>%
      dplyr::mutate(
        creator = suppressWarnings(as.integer(.data$creator))) == author
  }

  saveRDS(object = projects_tibble, file = projects_path)

  return(projects_tibble)
}

#' @importFrom rlang .data
remove_archived <- function(projects_tibble) {
  dplyr::filter(projects_tibble,
                fs::path_file(fs::path_dir(.data$path)) != "archive")
}
