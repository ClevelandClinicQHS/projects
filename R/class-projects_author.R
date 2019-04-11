
methods::setClass("projects_author")


new_projects_author <- function(x = character()) {
  structure(x, class = "projects_author")
}



validate_projects_author <- function(x,
                                     authors_table = authors_internal(p_path),
                                     p_path        = get_p_path(),
                                     na.ok         = TRUE) {
  x_valid <-
    validate_unique_entry(
      x       = x,
      table   = authors_table,
      what    = "author",
      na.ok   = na.ok,
      zero.ok = TRUE
    )

  if (nrow(x_valid) == 0) {
    new_projects_author(NA)
  } else {
    new_projects_author(paste0(x_valid$id, ": ", x_valid$last_name))
  }
}



# Numeric coercion-------------------------------------------------------------
as.integer.projects_author <- function(x) {
  as.integer(stringr::str_extract(x, "^\\d+"))
}

as.double.projects_author  <- function(x) {
  as.double(stringr::str_extract(x, "^\\d+"))
}

as.numeric.projects_author <- as.double.projects_author



# Subsetting methods, per ?`[.data.frame` -------------------------------------
as.data.frame.projects_author <- as.data.frame.vector

`[.projects_author` <- function(x, i, ...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}




# Method for c
c.projects_author <- function(...) {
  new_projects_author(c(unlist(lapply(list(...), unclass))))
}



# Math operator methods handle the authors according to their integer ids
Ops.projects_author <- function(e1, e2) {
  if (!any(c("==", "!=") == .Generic)) {
    stop(gettextf("%s not meaningful for projects_authors", sQuote(.Generic)))
  }

  if (rlang::is_integerish(e1) || rlang::is_integerish(e2)) {
    e1 <- as.integer(e1)
    e2 <- as.integer(e2)
  } else {
    if (inherits(e1, "projects_author")) {
      e1 <- unclass(e1)
      e2 <-
        as.character(
          lapply(
            e2,
            validate_projects_author,
            authors_table = authors_internal()
          )
        )
    } else {
      e2 <- unclass(e2)
      e1 <-
        as.character(
          lapply(
            e1,
            validate_projects_author,
            authors_table = authors_internal()
          )
        )
    }
  }

  get(.Generic)(e1, e2)
}




# Generic methods for match() and `%in%`---------------------------------------

match.projects_author <- function(x,
                                  table,
                                  nomatch       = NA_integer_,
                                  incomparables = NULL) {

  if (rlang::is_integerish(x) || rlang::is_integerish(table)) {
    x     <- as.integer(x)
    table <- as.integer(table)
    if (!is.null(incomparables)) {
      if (!rlang::is_integerish(incomparables)) {
        incomparables <- lapply(incomparables, validate_projects_author)
      }
      incomparables <- as.integer(incomparables)
    }
  } else {
    x     <- lapply(x, validate_projects_author)
    table <- lapply(table, validate_projects_author)
    if (!is.null(incomparables)) {
      incomparables <- lapply(incomparables, validate_projects_author)
    }
  }

  base::match(x, table, nomatch, incomparables)
}



#' @include class-projects_stage.R
methods::setMethod(
  "match",
  methods::signature(x = "projects_author"),
  match.projects_author
)

#' @include class-projects_stage.R
methods::setMethod(
  "match",
  methods::signature(table = "projects_author"),
  match.projects_author
)



`%in%.projects_author` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

#' @include class-projects_stage.R
methods::setMethod(
  "%in%",
  methods::signature(x = "projects_author"),
  `%in%.projects_author`
)

#' @include class-projects_stage.R
methods::setMethod(
  "%in%",
  methods::signature(table = "projects_author"),
  `%in%.projects_author`
)
