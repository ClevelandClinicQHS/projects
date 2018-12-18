validate_new <- function(id, what, rds_tibble) {

  if(nrow(rds_tibble) > 9999L) {
    stop("Maximum number of ", what, "s reached.")
  }

  id <- validate_int(x = id, what = what, rds_tibble = NULL)

  if(is.na(id)) {
    id <- min(setdiff(1L:9999L, rds_tibble$id))
  }
  else if(id %in% rds_tibble$id) {
    stop('id number already taken. Try a different one or leave the ',
         'argument blank for automatic selection ',
         '(the lowest counting number still available).')
  }

  return(id)
}


validate_entry <- function(x, what, rds_tibble = NULL, max.length = 9999,
                           allow_dups = FALSE, archived = TRUE) {

  if(!checkmate::test_vector(x = x, any.missing = FALSE, min.len = 1,
                             max.len = max.length, unique = TRUE)) {
    stop("Must enter ", what, " as a vector of minimum length 1, maximum ",
         "length ", max.length, " and no missing values or duplicate values.")
  }

  if(!archived) {
    rds_tibble <- remove_archived(rds_tibble)
  }

  ids <-
    lapply(
      x,
      function(char) {
        num <- suppressWarnings(as.numeric(char))
        if(is.na(num)) {
          validate_char(x = char, what = what, rds_tibble = rds_tibble,
                        allow_dups = allow_dups, archived = archived)
        }
        else {
          validate_int(x = num, what = what, rds_tibble = rds_tibble,
                       archived = archived)
        }
      }) %>%
    unlist()

  if(!allow_dups && anyDuplicated(ids) > 0) {
    stop("The following ", what, "s are repeats of other entered", what, "s: ",
         paste(x[duplicated(ids)], collapse = ", "), ". Check all names/",
         "titles and id numbers.")
  }

  return(ids)
}



validate_int <- function(x, what, rds_tibble, archived) {

  if(!checkmate::test_integerish(x, lower = 1, upper = 9999)) {
    stop(what, " id number must be a single integer between 1 and 9999.")
  }

  x <- as.integer(x)

  if(!is.null(rds_tibble)) {

    id_checks <-
      purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble$id)

    if(!all(id_checks)) {
      stop("The following ", what, " id(s) not found:",
           paste(x[!id_checks], collapse = ", "),
           ifelse(archived, "", "\nTry setting archived = TRUE"))
    }
  }

  return(x)
}



validate_char <- function(x, what, rds_tibble, allow_dups, archived) {

  if(!checkmate::test_character(x, min.chars = 1)) {
    stop("Each ", what, " entered as a character strings must contain at ",
         "least 1 character")
  }

  matches <- grep(pattern     = x,
                  x           = paste(rds_tibble[[2]], rds_tibble[[3]]),
                  ignore.case = TRUE)

  if(length(matches) != 1) {

    if(length(matches) == 0) {
      stop("No ", what, " found containing '", x, "' in the ",
           colnames(rds_tibble)[2], "+", colnames(rds_tibble)[3],
           ifelse(archived, "", "\nTry setting archived = TRUE"))
    }
    else if(length(matches) > 1 && !allow_dups) {
      print(rds_tibble[matches, ])
      stop(x, " matches the ", colnames(rds_tibble)[2], "+",
           colnames(rds_tibble)[3], " of all of the above ", what,
           "s. Be more specific to differentiate or enter their ", what,
           " id numbers instead.",
           ifelse(archived, "\nTry setting archived = FALSE", ""))
    }
  }

  return(rds_tibble$id[matches])
}



#' @importFrom rlang .data
validate_assoc <- function(x, what, rds_tibble, what2, rds_tibble2) {
  id_checks <-
    purrr::map_lgl(x, checkmate::test_choice, choices = rds_tibble2$id2)

  if(!all(id_checks)) {
    print(dplyr::filter(rds_tibble, .data$id %in% x[!id_checks]))
    stop("The above ", what, "(s) not found in ", what2, "'s ", what, " list.")
  }

  return(x)
}


validate_directory <- function(path,
                               p_path = p_path_internal(),
                               make_directories = FALSE) {

  path <- fs::path_tidy(path)

  # This only occurs during setup_projects()
  if(is.null(p_path)) {
    if(tolower(fs::path_file(path)) == "projects") {
      path <- fs::path_dir(path)
    }
  }

  else {
    path <- fs::path_abs(path = path, start = p_path)
    if(!fs::path_has_parent(path, p_path)) {
      path <- fs::path(p_path, path)
    }
  }

  if(!make_directories && !fs::dir_exists(path)) {
    stop("The directory\n", path, "\n\ndoes not exist. Create it or set ",
         "make_directories = TRUE.")
  }

  return(unclass(path))
}


validate_stage <- function(stage, choices) {

  if(length(stage) == 1 && is.na(stage)) {

    return(choices[1])

  }
  else {

    if(identical(stage, choices)) {
      stage <- choices[1]
    }
    else {
      stage <- as.character(stage)

      checkmate::assert_character(stage, min.chars = 1, any.missing = FALSE,
                                  len = 1)
    }

    stage <- factor(x      = grep(pattern     = stage,
                                  x           = choices,
                                  ignore.case = TRUE,
                                  value       = TRUE),
                    levels = choices)

    if(length(stage) != 1) {
      stop("User input for the 'stage' argument must match one of ",
           "the following:\n", paste(choices, collapse = "\n"))
    }

    return(stage)
  }
}

validate_protocol <- function(protocol, choices) {

  protocol_match <- try(match.arg(protocol, choices), silent = TRUE)

  if(class(protocol_match) == "try-error") {
    return(protocol)
  }

  else {
    return(protocol_match)
  }
}

validate_template <- function(file_name, what, default_name, default_template,
                              p_path) {

  path <- fs::path(p_path, ".templates", file_name)

  if(fs::file_exists(path)) {
    return(readr::read_lines(path))
  }

  else {

    if(file_name %in% default_name) {
      return(restore_default_template(file_name, what, default_name,
                                      default_template, p_path))
    }

    else {

      default_path <- fs::path(p_path, ".templates", default_name[1])

      user_prompt(msg   = paste0("Custom template not found at:\n", path,
                                 "\n\nUse the default at:\n", default_path,
                                 "\n\n? (y/n)"),
                  n_msg = paste0("\nChoose an existing ", what,
                                 " template in\n",
                                 fs::path(p_path, ".templates")))

      if(fs::file_exists(default_path)) {
        message("\n\nUsing default template.")
        return(readr::read_lines(default_path))
      }

      else {
        return(restore_default_template(file_name, what, default_name,
                                        default_template, p_path))
      }
    }
  }
}

restore_default_template <- function(file_name, what, default_name,
                                     default_template, p_path) {

  user_prompt(msg   = paste0("\n\nDefault template was not found at:\n",
                             fs::path(p_path, ".templates", file_name),
                             "\n\nRestore it and use for this project? (y/n)"),
              y_msg = paste0("\nDefault restored at:\n",
                             fs::path(p_path, ".templates", file_name),
                             "\n\nUsing it as the ", what,
                             " template for this project"),
              n_msg = paste0("Choose an existing ", what, " template in\n",
                             fs::path(p_path, ".templates"), "\n\nor respond ",
                             '"y" to restoring the default next time.'))

  return(
    readr::write_lines(
      x    = default_template[[which(default_name == file_name)]],
      path = fs::path(p_path, ".templates", file_name)))

}

yaml_bounds <- function(vector, what) {

  yaml_bounds <- grep("^---$", vector)

  if(length(yaml_bounds) < 2) {
    stop(what, " template must have a yaml header even if it's empty, as in:",
         "\n\n---\n---",
         "\n\nCheck that there are no spaces before or after each ---")
  }

  return(yaml_bounds)
}

