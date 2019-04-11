
methods::setClass("projects_stage")


new_projects_stage <- function(x = character()) {
  structure(x, class = "projects_stage")
}



validate_stage <- function(stage, na.ok = TRUE, null.ok = FALSE) {

  if (is.null(stage) && null.ok) {
    return(NULL)
  }

  choices <- eval(formals(new_project)$stage)

  if (identical(stage, choices)) {
    stage <- stage[1L]
  } else {

    stage <- trimws(tolower(as.character(stage)))

    if (!rlang::is_scalar_character(stage)) {
      stop("stage must be a single integer or character string")
    }

    if (is.na(stage)) {
      if (!na.ok) {
        stop("stage must not be missing (NA)")
      }
    } else {

      match_attempt <- pmatch(stage, choices)

      if (is.na(match_attempt)) {

        match_attempt <- pmatch(stage, substr(choices, 4, nchar(choices)))

        if (is.na(match_attempt)) {
          stop(
            "\nTo match a stage, user input must either:\n\n",
            "- exactly match the integer\n",
            "- partially match the text\n\n",
            "of one of:\n",
            paste(choices, collapse = "\n"),
            "\n\n'", stage, "' did not match."
          )
        }
      }

      stage <- choices[match_attempt]
    }
  }

  new_projects_stage(stage)
}




# Numeric coercion-------------------------------------------------------------
as.integer.projects_stage <- function(x) as.integer(substr(unclass(x), 1, 1))

as.double.projects_stage  <- function(x) as.double(substr(unclass(x), 1, 1))

as.numeric.projects_stage <- as.double.projects_stage



# Subsetting methods, per ?`[.data.frame` -------------------------------------
as.data.frame.projects_stage <- as.data.frame.vector

`[.projects_stage` <- function(x, i, ...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}



# Method for c
c.projects_stage <- function(...) {
  new_projects_stage(c(unlist(lapply(list(...), unclass))))
}


# Mathematical operator methods handle the stages as integers------------------
Ops.projects_stage <- function(e1, e2) {
  get(.Generic)(as.integer(validate_stage(e1)), as.integer(validate_stage(e2)))
}




# Generic methods for match() and `%in%`---------------------------------------
methods::setGeneric("match")

match.projects_stage <- function(x,
                                 table,
                                 nomatch       = NA_integer_,
                                 incomparables = NULL) {

  x     <- lapply(x, validate_stage)

  table <- lapply(table, validate_stage)

  if (!is.null(incomparables)) {
    incomparables <- lapply(incomparables, validate_stage)
  }

  base::match(x, table, nomatch, incomparables)
}

methods::setMethod(
  "match",
  signature(x = "projects_stage"),
  match.projects_stage
)
methods::setMethod(
  "match",
  signature(table = "projects_stage"),
  match.projects_stage
)

methods::setGeneric("%in%")

`%in%.projects_stage` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

methods::setMethod(
  "%in%",
  signature(x = "projects_stage"),
  `%in%.projects_stage`
)
methods::setMethod(
  "%in%",
  signature(table = "projects_stage"),
  `%in%.projects_stage`
)
