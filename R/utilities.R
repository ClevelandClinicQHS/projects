
make_project_name <- function(x, short_title = FALSE) {

  if (short_title) {
    fs::path_sanitize(x)
  } else {
    paste0("p", stringr::str_pad(x, width = 4, side = "left", pad = "0"))
  }
}

make_project_path <- function(project_name, path = get_p_path()) {
  fs::path(path, project_name) %>% unclass()
}


check_all_rds <- function() {
  purrr::walk(
    c(
      "affiliations",
      "author_affiliation_assoc",
      "authors",
      "project_author_assoc",
      "projects"
    ),
    function(x) {
      print(x)
      print(
        readRDS(
          fs::path(projects_folder(), ".metadata", x, ext = "rds")
        )
      )
    }
  )
}



user_prompt <- function(msg, y_msg, n_msg, error = TRUE) {

  if (isTRUE(getOption('knitr.in.progress'))) {
    prompt <- "y"
  } else {
    prompt <- NULL
  }

  while (is.null(prompt) || !(prompt %in% c("y", "n"))) {
    if (!is.null(prompt)) {
      message("\nInvalid input.\n")
    }
    message(msg)
    prompt <- tolower(readLines(con = stdin(), n = 1L))
  }

  if (prompt == "n") {
    if (error) {
      stop(n_msg, call. = FALSE)
    } else if (!missing(n_msg)) {
      message(n_msg)
    }
  } else if (!missing(y_msg)) {
    message(y_msg)
  }

  prompt == "y"
}







build_protocol_report <- function(vector,
                                  what,
                                  project_id,
                                  title,
                                  corresp_auth_row,
                                  authors_table,
                                  affiliations_table,
                                  project_authors,
                                  aa_assoc_table,
                                  use_bib,
                                  pXXXX_name) {

  yaml_bounds <- yaml_bounds(vector = vector, what = what)

  vector      <- vector %>%
    insert_aa(
      project_id               = project_id,
      yaml_bounds              = yaml_bounds,
      corresp_auth_row         = corresp_auth_row,
      authors_table            = authors_table,
      affiliations_table       = affiliations_table,
      project_authors          = project_authors,
      author_affiliation_assoc = aa_assoc_table
    ) %>%
    protocol_report_yaml(
      title       = title,
      yaml_bounds = yaml_bounds,
      use_bib     = use_bib,
      pXXXX_name  = pXXXX_name
    )

  vector
}



protocol_report_yaml <- function(vector, title, yaml_bounds, use_bib,
                                 pXXXX_name) {

  yaml <- c(paste0('title: "', title, '"'),
            "output:",
            "  word_document: default",
            "  html_document:",
            "    css: style.css")

  if (use_bib) {
    yaml <- append(yaml, paste0("bibliography: ", pXXXX_name, ".bib"))
  }

  vector <- append(x      = vector,
                   values = yaml,
                   after  = yaml_bounds[1])

  vector
}



build_datawork_analysis <- function(vector, what, p_path, pXXXX_name) {

  yaml_bounds <- yaml_bounds(vector = vector, what = what)

  vector    <- append(x      = vector,
                      values = paste0('title: "', pXXXX_name, ' ', what, '"'),
                      after  = yaml_bounds[1])

  vector
}



write_project_files <- function(pXXXX_path, files, use_bib, pXXXX_name) {

  fs::dir_create(fs::path(pXXXX_path, c("data", "data_raw", "progs",
                                        "manuscript", "figures")))

  file_names <-
    c("01_protocol.Rmd", "02_datawork.Rmd", "03_analysis.Rmd",
      "04_report.Rmd", "style.css", paste0(pXXXX_name, ".Rproj")) %>%
    utils::tail(n = length(files))

  if (use_bib) {
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



clear_special_author <- function(author, projects_path, projects_table) {

  if (nrow(projects_table) > 0) {
    is.na(projects_table[c("current_owner", "creator", "corresp_auth")]) <-
      projects_table[c("current_owner", "creator", "corresp_auth")] == author
  }

  write_metadata(table = projects_table, table_path = projects_path)

  projects_table
}



#' @importFrom rlang .data
remove_archived <- function(projects_table) {
  projects_table[
    which(fs::path_file(fs::path_dir(projects_table$path)) != "archive"),
  ]
}
