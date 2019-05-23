
methods::setClass("projects_stage")


#' \code{projects_stage} class and its methods
#'
#' Objects of this class are merely a character string containing a number and a
#' name of one of seven project development stages.
#'
#' A \code{projects_stage} object is either a missing value (\code{NA}) or one
#' of:
#'
#' \code{0: idea}\cr \code{1: design}\cr \code{2: data collection}\cr \code{3:
#' analysis}\cr \code{4: manuscript}\cr \code{5: under review}\cr \code{6:
#' accepted}
#'
#' \code{new_projects_stage()} merely coerces the object's class attribute to
#' \code{projects_stage}.
#'
#' @section Numeric coercion methods: \code{\link{as.integer}()},
#'   \code{\link{as.double}()}, and \code{\link{as.numeric}()} return the stage
#'   number of the \code{projects_author} object as an integer/double. The
#'   methods for the comparison and value matching functions described below
#'   make use of these numeric coercion methods. Users desiring to apply value
#'   matching functions other than the ones described below may similarly take
#'   advantage of these.
#'
#' @section Comparison and value matching methods: Methods for the
#'   \link{Comparison} operators as well as \code{\link{match}()} and
#'   \code{\link{\%in\%}} enable users to test equality and to value match among
#'   \code{projects_stage} objects and as well as between \code{projects_stage}
#'   objects and unclassed numbers/characters. When comparing or value matching
#'   against a numeric vector, the \code{projects_stage} object is first coerced
#'   to an integer with the \code{as.integer()} method described above. When
#'   testing or value matching against a character vector, the character vector
#'   is validated against the list of project stages enumerated above.
#'
#' @section \code{c()} method: A method for \code{\link{c}()} was also written
#'   so that the class attribute is not lost.
#'
#' @param x For \code{new_projects_stage()}, any object. For
#'
#'   For the \code{as.*()} methods, a \code{projects_stage} object.
#'
#'   For \code{\link{match}()} and \code{\link{\%in\%}}, an integer, a character
#'   string, or a \code{projects_author} object. See \code{\link{match}()} and
#'   \strong{Equality and value matching methods} below.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @param table An integer number, a character string, or a
#'   \code{projects_author} object. See \code{\link{match}()} and
#'   \strong{Equality and value matching methods} below.
#'
#' @param nomatch See \code{\link{match}()}.
#'
#' @param incomparables An integer number, a character string, or a
#'   \code{projects_author} object. See \code{\link{match}()}.
#'
#' @seealso \code{\link{Ops}}; \code{\link[methods]{Methods_for_Nongenerics}}.
#'   For other S3 class-retention strategies, see \code{\link{Extract}} and
#'   \code{\link{[.data.frame}}.
#'
#' @examples
#' stage <- new_projects_stage("4: manuscript")
#'
#' as.integer(stage) # 4
#'
#' stage == 4       # TRUE
#' stage != 4       # FALSE
#' stage <  6       # TRUE
#'
#' stage %in% c(3:6)  # TRUE
#' match(stage, 0:4)  # 5
#'
#' stage %in% c("design", "manusc", "idea")  # TRUE
#'
#' more_stages <- new_projects_stage(c("0: idea", "4: manuscript", "1: design"))
#'
#' match("MAnuscRIPT", more_stages)      # 2
#'
#' x <- structure("7: redacted", class = "dummyclass")
#' class(c(x))     # Does not retain class
#' class(c(stage, more_stages)) # Retains class
#' @name projects_stage
#' @export
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
#' @rdname projects_stage
#' @export
as.integer.projects_stage <- function(x, ...) {
  as.integer(substr(unclass(x), 1L, 1L), ...)
}

#' @rdname projects_stage
#' @export
as.double.projects_stage  <- function(x, ...) {
  as.double(substr(unclass(x), 1L, 1L), ...)
}

#' @rdname projects_stage
#' @export
as.numeric.projects_stage <- as.double.projects_stage



# Subsetting methods, per ?`[.data.frame` -------------------------------------

#' @export
as.data.frame.projects_stage <- as.data.frame.vector

#' @export
`[.projects_stage` <- function(x, i, ...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}


#' @export
c.projects_stage <- function(...) {
  new_projects_stage(c(unlist(lapply(list(...), unclass))))
}


#' @export
Ops.projects_stage <- function(e1, e2) {
  get(.Generic)(as.integer(validate_stage(e1)), as.integer(validate_stage(e2)))
}




# Generic methods for match() and `%in%`---------------------------------------

#' @method match projects_stage
#' @rdname projects_stage
#' @export
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

#' @include set_generics.R
methods::setMethod(
  "match",
  signature(x = "projects_stage"),
  match.projects_stage
)

#' @include set_generics.R
methods::setMethod(
  "match",
  signature(table = "projects_stage"),
  match.projects_stage
)



#' @method %in% projects_stage
#' @rdname projects_stage
#' @export
`%in%.projects_stage` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

#' @include set_generics.R
methods::setMethod(
  "%in%",
  signature(x = "projects_stage"),
  `%in%.projects_stage`
)

#' @include set_generics.R
methods::setMethod(
  "%in%",
  signature(table = "projects_stage"),
  `%in%.projects_stage`
)
