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
      stop(n_msg)
    }
    else if(!missing(n_msg)) {
      message(n_msg)
    }
  }
  else if(!missing(y_msg)) {
    msg(y_msg)
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



build_protocol_report <- function(protocol, p_path, project_id, title,
                                  corresp_auth, authors_tibble,
                                  affiliations_tibble, project_authors,
                                  aa_assoc_tibble, use_bib, pXXXX_name) {

  protocol        <- protocol_selector(selection = protocol, p_path = p_path)

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

  return(stats::setNames(protocol_report, c("protocol", "report")))
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

      protocol_path <- fs::path(p_path, ".templates/STROBE_template.Rmd")

      if(fs::file_exists(protocol_path)) {
        selection <- readr::read_lines(protocol_path)
      }

      else {

        if(user_prompt(
          msg   = paste0("STROBE protocol template not found at\n",
                         protocol_path, "\n\nDo you want the default STROBE ",
                         "template to be restored? (y/n)"),
          y_msg = paste0("\nThe default STROBE template was restored at\n",
                         protocol_path),
          error = FALSE)) {
          readr::write_lines(STROBE_template, protocol_path)
        }

        selection <- STROBE_template
      }
    }
    else if(selection == "CONSORT") {

      protocol_path <- fs::path(p_path, ".templates/CONSORT_template.Rmd")

      if(fs::file_exists(protocol_path)) {
        selection <- readr::read_lines(protocol_path)
      }

      else {

        if(user_prompt(
          msg   = paste0("CONSORT protocol template not found at\n",
                         protocol_path, "\n\nDo you want the default CONSORT ",
                         "template to be restored? (y/n)"),
          y_msg = paste0("\nThe default CONSORT template was restored at\n",
                         protocol_path),
          error = FALSE)) {
          readr::write_lines(CONSORT_template, protocol_path)
        }

        selection <- CONSORT_template
      }
    }
    else {
      stop("Invalid protocol selection: ", selection)
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
      stop(what, " template does not exist at:\n", template_path,
           "\n\nCheck case and file extensions.")
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

      affiliation_line <- paste0("| <sup>", a, "</sup> ",
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
                 "<sup>",
                 paste(sort(x_affiliations$superscript), collapse = ","),
                 "</sup>")
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

    if(nrow(corresp_affils) > 0 && length(na.omit(corresp_affils$address)) > 0){
      corresp_lines <-
        append(corresp_lines,
               paste0("|   ", na.omit(corresp_affils$address)[1]))
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

  return(c(author_line, affiliations_lines, corresp_lines, "| ", "| Funding:"))
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



delete_all_rds <- function() {
  old_knit <- options('knitr.in.progress' = TRUE)
  on.exit(options(old_knit))

  purrr::walk(authors()$id, delete_author)
  purrr::walk(affiliations()$id, delete_affiliation)
  purrr::walk(projects(archived = TRUE)$id, delete_project)
}


# @importFrom rlang .data
# @export
# fix_metadata <- function(path = ".metadata") {
#   p_path <- p_path_internal()
#   purrr::walk(
#     .x = c("projects", "authors", "affiliations",
#            "project_author_assoc", "author_affiliation_assoc"),
#     .f =
#       function(x) {
#         tibble <- readRDS(file = fs::path(p_path, path, x, ext = "rds"))
#         if(x == "projects") {
#           tibble$id <- as.integer(tibble$id)
#
#           tibble$deadline <- as.Date(tibble$deadline)
#
#           tibble$stage <- factor(tibble$stage,
#                                  levels = c("1: design", "2: data collection",
#                                             "3: analysis", "4: manuscript",
#                                             "5: under review", "6: accepted"))
#
#           if(is.null(tibble$creator)) {
#             tibble$creator <- NA_character_
#           }
#           else {
#             tibble$creator <- as.character(tibble$creator)
#           }
#
#           if(is.null(tibble$current_owner)) {
#             tibble$current_owner <- NA_integer_
#           }
#           else {
#             tibble$current_owner <- as.integer(tibble$current_owner)
#           }
#
#           if(is.null(tibble$corresp_auth)) {
#             tibble$corresp_auth <- NA_integer_
#           }
#           else {
#             tibble$corresp_auth <- as.integer(tibble$corresp_auth)
#           }
#
#           if(is.null(tibble$path)) {
#             tibble <-
#               dplyr::mutate(
#                 tibble,
#                 path = fs::path(p_path, make_project_name(.data$id)))
#           }
#
#           if(is.null(tibble$short_title)) {
#             tibble$short_title <- NA_character_
#           }
#
#           tibble <- dplyr::select(
#             tibble,
#             id, title, short_title, current_owner, status, deadline_type,
#             deadline, stage, path, corresp_auth, creator)
#         }
#
#         if(x == "authors") {
#           tibble$id <- as.integer(tibble$id)
#           if(is.null(tibble$phone)) {
#             tibble$phone <- NA_character_
#           }
#           tibble <-
#             dplyr::select(
#               tibble,
#               id, given_names, last_name, title, degree, email, phone)
#         }
#
#         if(x == "affiliations") {
#           tibble$id <- as.integer(tibble$id)
#         }
#
#         if(x == "project_author_assoc") {
#
#           if(fs::file_exists(fs::path(p_path, path, "project_PI_assoc",
#                                       ext = "rds"))) {
#             PI <-
#               readRDS(file = fs::path(p_path, path, "project_PI_assoc",
#                                       ext = "rds")) %>%
#               dplyr::mutate(id1 = as.integer(.data$id1),
#                             id2 = as.integer(.data$id2))
#
#             tibble <- dplyr::mutate(tibble,
#                                     id1 = as.integer(.data$id1),
#                                     id2 = as.integer(.data$id2))
#             tibble <- dplyr::bind_rows(PI, tibble)
#           }
#
#         }
#
#         if(x == "author_affiliation_assoc") {
#           tibble <- dplyr::mutate(tibble,
#                                   id1 = as.integer(.data$id1),
#                                   id2 = as.integer(.data$id2))
#         }
#
#         saveRDS(tibble, file = fs::path(p_path, ".metadata", x, ext = "rds"))
#       })
# }
