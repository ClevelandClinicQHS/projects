

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
      table[grep(x, paste(table[[3]], table[[2]]), ignore.case = TRUE), ]

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
    do.call(vec_rbind, .) %>%
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
    do.call(vec_rbind, .)

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
  lapply(
    list(
      "current_owner" = current_owner,
      "corresp_auth"  = corresp_auth,
      "creator"       = creator
    ),
    validate_projects_author,
    authors_table = authors_table,
    na.ok         = TRUE
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



validate_single_string <- function(x,
                                   null.ok = FALSE,
                                   na.ok   = TRUE,
                                   zero.chars.ok = TRUE,
                                   tolower = FALSE) {

  user_input <- rlang::as_label(rlang::enexpr(x))

  if (is.null(x) && null.ok) {
    return(NULL)
  }

  x    <- as.character(x)

  if (!rlang::is_scalar_character(x)) {
      stop(user_input, " must be coercible to a single character string")
  }

  if (is.na(x) && !na.ok) {
    stop(user_input, " must not be missing")
  }

  if (nchar(x) == 0L && !zero.chars.ok) {
    stop(user_input, " must be at least one character long")
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

  x <- lubridate::as_datetime(x)
  if (length(x) != 1L) {
    stop(
      "deadline must be coercible to a POSIXct object of length 1",
      "\n(via lubridate::as_datetime())"
    )
  }
  x
}



validate_directory <- function(path,
                               p_path = get_p_path(),
                               make_directories = FALSE) {

  path <- path %>% fs::path_tidy() %>% fs::path_expand()

  # setup_projects() and
  # move_projects_folder() is the only place where p_path is NULL
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



validate_Renviron <- function(.Renviron_path,
                              action = c("setting up", "moving", "renaming")) {
  action <- match.arg(action)

  pt_action <- switch(action, moving = "moved", renaming = "renamed", "created")

  if (is.null(.Renviron_path)) {
    FALSE
  } else if (fs::path_file(.Renviron_path) != ".Renviron") {
    user_prompt(
      msg =
        paste0(
          "\nThe provided .Renviron_path:\n",
          .Renviron_path,
          "\n\nis not a .Renviron file. Continue ", action, " projects folder",
          "\nwithout saving its path? (y/n)"
        ),
      n_msg = paste0("\nProjects folder not ", pt_action, ".")
    )
    FALSE
  } else if (fs::file_exists(.Renviron_path)) {

    if (fs::file_access(.Renviron_path, "write")) {
      TRUE
    } else {
      user_prompt(
        msg =
          paste0(
            "\nPermission to edit the .Renviron file at:\n",
            .Renviron_path,
            "\n\nwas denied. Continue ", action, " projects folder",
            "\nwithout saving its path? (y/n)"
          ),
        n_msg = paste0("\nProjects folder not ", pt_action, ".")
      )
      FALSE
    }

  } else if (fs::file_access(fs::path_dir(.Renviron_path), "write")) {
    TRUE
  } else {
    user_prompt(
      msg =
        paste0(
          "\nPermission to create a .Renviron file at:\n",
          .Renviron_path,
          "\n\nwas denied. Continue ", action, " projects folder",
          "\nwithout saving its path? (y/n)"
        ),
      n_msg = paste0("\nProjects folder not ", pt_action, ".")
    )
    FALSE
  }
}
