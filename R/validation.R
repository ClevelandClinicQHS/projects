

validate_entry <- function(x,
                           table,
                           what,
                           na.ok   = FALSE,
                           zero.ok = FALSE) {

  if (!rlang::is_scalar_vector(x)) {
    stop("Each entry identifying a ", what, "must be of length 1")
  }

  if (is.na(x)) {
    if (na.ok) {
      return(table[NULL, ])
    }
    stop(what, " may not be missing (NA)")
  }

  x_int <- as.integer(stringr::str_extract(x, "\\d+"))

  if (is.na(x_int)) {

    matches <-
      table[grep(x, paste(table[[2]], table[[3]]), ignore.case = TRUE), ]

  } else if (x_int == 0 && is_creator(x) && zero.ok) {

    table <- table[NULL, ]
    table[1L, c("id", "last_name")] <- list(0L, substr(x, 4L, nchar(x)))
    return(table)

  } else {

    matches <- table[table$id == x_int, ]

  }

  if (nrow(matches) == 0L) {
    stop(
      what,
      ifelse(
        rlang::is_integerish(x),
        paste0(" with the id ", x),
        paste0(" matching '", x, "'")
      ),
      " not found"
    )
  } else {
    matches
  }
}



is_creator <- function(x) {
  stringr::str_detect(x, "0: .+")
}




validate_unique_entry <- function(x,
                                  table,
                                  what,
                                  na.ok = FALSE,
                                  zero.ok = FALSE) {
  match <-
    validate_entry(
      x       = x,
      table   = table,
      what    = what,
      na.ok   = na.ok,
      zero.ok = zero.ok
    )

  if (nrow(match) > 1L) {
    print(match)
    stop("\nThe entry ", x, " matches multiple ", what, "s, seen above.")
  } else {
    match
  }
}



validate_entry_list <- function(x,
                                table,
                                what,
                                na.ok = FALSE,
                                zero.ok = FALSE) {
  x %>%
    lapply(
      validate_entry,
      table = table,
      what = what,
      na.ok = na.ok,
      zero.ok = zero.ok
    ) %>%
    do.call(rbind, .) %>%
    dplyr::distinct()
}




validate_unique_entry_list <- function(x,
                                       table,
                                       what,
                                       na.ok = FALSE,
                                       zero.ok = FALSE) {
  x_valid <-
    lapply(
      x,
      validate_unique_entry,
      table = table,
      what = what,
      na.ok = na.ok,
      zero.ok = zero.ok
    ) %>%
    do.call(rbind, .)

  if (anyDuplicated(x_valid)) {
    dup_vector <- duplicated(x_valid) | duplicated(x_valid, fromLast = TRUE)
    print(x_valid[dup_vector, ])
    stop(
      "\nThere are identical matches among the entries:\n",
      paste(x[dup_vector], collapse = "\n"),
      "\nCollectively, they match the ", what, "s above."
    )
  } else {
    x_valid
  }
}




validate_new <- function(id, what, rds_table) {

  if (nrow(rds_table) > 9999L) {
    stop("Maximum number of ", what, "s reached.")
  }

  if (!is.na(id) &&
      (!rlang::is_scalar_integerish(id) || !dplyr::between(id, 1L, 9999L))) {
    stop(what, " id number must be a single integer between 1 and 9999.")
  }

  id <- as.integer(id)

  if (is.na(id)) {
    id <- min(setdiff(1L:9999L, rds_table$id))
  }
  else if (any(rds_table$id == id)) {
    stop(
      'id number already taken. Try a different one or leave the ',
      'argument blank for automatic selection ',
      '(the lowest counting number still available).'
    )
  }

  id
}




validate_authors <- function(general_authors,
                             current_owner,
                             corresp_auth,
                             creator,
                             authors_table) {
  if (
    nrow(authors_table) == 0L &&
    (!is.null(general_authors) || !is.na(current_owner) ||
     !is.na(corresp_auth) || !is.na(creator))
  ) {
    stop(
      "Can't set authors until an author is created. Run new_author()"
    )
  }

  special_authors <-
    validate_special_authors(
      current_owner,
      corresp_auth,
      creator,
      authors_table
    )

  general_authors <- validate_general_authors(general_authors, authors_table)

  general_authors <-
    c(general_authors, special_authors[!is.na(special_authors)]) %>%
    unique()

  if (is.na(special_authors$current_owner) && length(general_authors) > 0L) {
    special_authors$current_owner <- general_authors[[1L]]
  }

  if (is.na(special_authors$creator)) {
    special_authors$creator <-
      new_projects_author(paste0("0: ", Sys.info()["user"]))
  }

  if (!is.na(special_authors$corresp_auth)) {
    corresp_auth_row <-
      authors_table[match(special_authors$corresp_auth, authors_table$id), ]
  } else {
    corresp_auth_row <- NULL
  }

  c(
    list("general_authors" = vapply(general_authors, as.integer, integer(1L))),
    special_authors,
    list(corresp_auth_row = corresp_auth_row)
  )
}



validate_special_authors <- function(current_owner,
                                     corresp_auth,
                                     creator,
                                     authors_table) {
  sapply(
    list(
      "current_owner" = current_owner,
      "corresp_auth"  = corresp_auth,
      "creator"       = creator
    ),
    validate_projects_author,
    authors_table = authors_table,
    na.ok         = TRUE,
    simplify      = FALSE
  )
}



validate_general_authors <- function(authors, authors_table) {

  if (is.null(authors)) {
    list()
  } else {

    if (!rlang::is_vector(authors)) {
      stop("authors argument must be a vector of author ids and/or names")
    }

    lapply(
      authors,
      validate_projects_author,
      authors_table = authors_table,
      na.ok = FALSE
    )
  }
}



validate_assoc <- function(x, what, rds_table, what2, rds_table2) {

  id_checks <- x %in% rds_table2$id2
    #purrr::map_lgl(x, checkmate::test_choice, choices = rds_table2$id2)

  if (!all(id_checks)) {
    print(rds_table[rds_table$id %in% x[!id_checks], ])
    stop("The above ", what, "(s) not found in ", what2, "'s ", what, " list.")
  }

  x
}




validate_single_string <- function(x, null.ok = FALSE, tolower = FALSE) {

  user_input <- rlang::as_label(rlang::enexpr(x))

  if (is.null(x) && null.ok) {
    return(NULL)
  }

  x    <- as.character(x)
  if (!rlang::is_scalar_character(x)) {
    stop(
      user_input,
      " must be coercible to a single character string"
    )
  }

  if (tolower) {
    x <- tolower(x)
  }

  x
}




validate_deadline <- function(x, null.ok = FALSE) {

  if (is.null(x) && null.ok) {
    return(NULL)
  }

  x <- as.POSIXct(x)
  if (length(x) != 1) {
    stop("deadline must be coercible to a POSIXct object of length 1")
  }
  x
}



validate_directory <- function(path,
                               p_path = get_p_path(),
                               make_directories = FALSE) {

  path <- fs::path_tidy(path)

  # setup_projects() is the only place where p_path is NULL
  if (!is.null(p_path)) {
    path <- fs::path_abs(path = path, start = p_path)
    if (!fs::path_has_parent(path, p_path)) {
      path <- fs::path(p_path, path)
    }
  }

  if (!make_directories && !fs::dir_exists(path)) {
    stop("The directory\n", path, "\n\ndoes not exist. Create it or set ",
         "make_directories = TRUE.")
  }

  unclass(path)
}



validate_protocol <- function(protocol, choices) {

  protocol_match <- try(match.arg(protocol, choices), silent = TRUE)

  if (inherits(protocol_match, "try-error")) {
    protocol
  } else {
    protocol_match
  }
}




validate_template <- function(file_name,
                              what,
                              default_name,
                              default_template,
                              p_path) {

  path <- fs::path(p_path, ".templates", file_name)

  if (fs::file_exists(path)) {
    return(readr::read_lines(path))
  } else {

    if (any(default_name == file_name)) {
      return(
        restore_default_template(
          file_name,
          what,
          default_name,
          default_template,
          p_path
        )
      )
    } else {

      default_path <- fs::path(p_path, ".templates", default_name[1])

      user_prompt(
        msg   =
          paste0(
            "Custom template not found at:\n",
            path,
            "\n\nUse the default at:\n",
            default_path,
            "\n\n? (y/n)"
          ),
        n_msg =
          paste0(
            "\nChoose an existing ",
            what,
            " template in\n",
            fs::path(p_path, ".templates")
          )
      )

      if (fs::file_exists(default_path)) {
        message("\n\nUsing default template.")
        return(readr::read_lines(default_path))
      } else {
        return(
          restore_default_template(
            file_name,
            what,
            default_name,
            default_template,
            p_path
          )
        )
      }
    }
  }
}

restore_default_template <- function(file_name,
                                     what,
                                     default_name,
                                     default_template,
                                     p_path) {
  user_prompt(
    msg   =
      paste0(
        "\n\nDefault template was not found at:\n",
        fs::path(p_path, ".templates", file_name),
        "\n\nRestore it and use for this project? (y/n)"
      ),
    y_msg =
      paste0(
        "\nDefault restored at:\n",
        fs::path(p_path, ".templates", file_name),
        "\n\nUsing it as the ",
        what,
        " template for this project"
      ),
    n_msg =
      paste0(
        "Choose an existing ",
        what,
        " template in\n",
        fs::path(p_path, ".templates"),
        "\n\nor respond ",
        '"y" to restoring the default next time.'
      )
  )

  readr::write_lines(
    x    = default_template[[which(default_name == file_name)]],
    path = fs::path(p_path, ".templates", file_name)
  )
}



validate_docx <- function(docx, p_path) {
  if (
    docx != "styles.docx" &&
    !fs::file_exists(fs::path(p_path, ".templates", docx))
  ) {
    user_prompt(
      msg =
        paste0(
          "docx template not found at\n", fs::path(p_path, ".templates", docx),
          "\n\nUse the default template?"
        ),
      n_msg =
        paste0(
          "\nChoose an existing docx template in\n",
          fs::path(p_path, ".templates")
        )
    )
    docx <- "styles.docx"
  }

  if (
    docx == "styles.docx" &&
    !fs::file_exists(fs::path(p_path, ".templates", "styles.docx"))
  ) {
    user_prompt(
      msg   =
        paste0(
          "\n\nDefault template was not found at:\n",
          fs::path(p_path, ".templates", "styles.docx"),
          "\n\nRestore it and use for this project? (y/n)"
        ),
      y_msg =
        paste0(
          "\nDefault restored at:\n",
          fs::path(p_path, ".templates", "styles.docx"),
          "\n\nUsing it as the .docx template for this project."
        ),
      n_msg =
        paste0(
          "Choose an existing .docx template in\n",
          fs::path(p_path, ".templates"),
          "\n\nor respond ",
          '"y" to restoring the default next time.'
        )
    )

    fs::file_copy(
      system.file("templates", "styles.docx", package = "projects"),
      fs::path(p_path, ".templates", "styles.docx")
    )
  }

  docx
}

restore_default_docx_template <- function(p_path) {
  user_prompt(
    msg   =
      paste0(
        "\n\nDefault template was not found at:\n",
        fs::path(p_path, ".templates", "styles.docx"),
        "\n\nRestore it and use for this project? (y/n)"
      ),
    y_msg =
      paste0(
        "\nDefault restored at:\n",
        fs::path(p_path, ".templates", "styles.docx"),
        "\n\nUsing it as the .docx template for this project."
      ),
    n_msg =
      paste0(
        "Choose an existing .docx template in\n",
        fs::path(p_path, ".templates"),
        "\n\nor respond ",
        '"y" to restoring the default next time.'
      )
  )

  fs::file_copy(
    system.file("templates", "styles.docx", package = "projects"),
    fs::path(p_path, ".templates", "styles.docx")
  )
}




yaml_bounds <- function(vector, what) {

  yaml_bounds <- grep("^---$", vector)

  if (length(yaml_bounds) < 2) {
    stop(
      what, " template must have a yaml header even if it's empty, as in:",
      "\n\n---\n---",
      "\n\nCheck that there are no spaces before or after each ---"
    )
  }

  yaml_bounds
}

