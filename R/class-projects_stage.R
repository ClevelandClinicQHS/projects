
#' \code{projects_stage} vector
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
#' \code{projects_stage()} validates and coerces a vector of the above integers or strings to a \code{projects_stage} S3 vector.
#'
#' @section Numeric coercion methods: \code{\link{as.integer}()},
#'   \code{\link{as.double}()}, and \code{\link{as.numeric}()} return the stage
#'   number of the \code{projects_stage} object as an integer/double. The
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
#' @param x For \code{projects_stage()}, an integer or character vector. For
#'
#'   For \code{\link{match}()} and \code{\link{\%in\%}}, an integer, a character
#'   string, or a \code{projects_stage} object. See \code{\link{match}()} and
#'   \strong{Comparison and value matching methods} below.
#'
#' @param table An integer number, a character string, or a
#'   \code{projects_stage} object. See \code{\link{match}()} and
#'   \strong{Comparison and value matching methods} below.
#'
#' @param nomatch See \code{\link{match}()}.
#'
#' @param incomparables An integer number, a character string, or a
#'   \code{projects_stage} object. See \code{\link{match}()}.
#'
#' @return For \code{projects_stage()}, an S3 vector of class
#'   \code{projects_stage}.
#'
#' @seealso \code{\link{Ops}}; \code{\link[methods]{Methods_for_Nongenerics}}.
#'
#' @examples
#' stage <- projects_stage("4: manuscript")
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
#' more_stages <- projects_stage(c("0: idea", "4: manuscript", "1: design"))
#'
#' match("MAnuscRIPT", more_stages)      # 2
#' @export
projects_stage <- function(x = character()) {
  x <- vec_cast(x, character())
  validate_stage(x)
}


new_projects_stage <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "projects_stage")
}


#' @rdname projects_stage
#' @export
methods::setClass("projects_stage")


#' @export
vec_ptype_abbr.projects_stage <- function(x, ...) "prjstg"


validate_stage <- function(stage, na.ok = TRUE, null.ok = FALSE, n = NULL) {

  if (is.null(stage) && null.ok) {
    return(NULL)
  }

  choices <- eval(formals(new_project)$stage)

  stage <- trimws(tolower(as.character(stage)))

  if (!rlang::is_atomic(stage) || !is.null(n) && length(stage) != n) {
    stop("\nstage must be coercible to a character vector of length ", n)
  }

  stage <-
    vapply(
      stage,
      function(stage) {
        if (is.na(stage)) {
          if (!na.ok) {
            stop("stage must not be missing (NA)")
          }
        } else {

          match_attempt <- pmatch(stage, choices)

          if (is.na(match_attempt)) {

            match_attempt <- pmatch(stage, substr(choices, 4L, nchar(choices)))

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
      },
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    )

  new_projects_stage(stage)
}



#' @rdname projects_stage-vctrs
#' @method vec_ptype2 projects_stage
#' @export
#' @export vec_ptype2.projects_stage
vec_ptype2.projects_stage <- function(x, y, ...)
  UseMethod("vec_ptype2.projects_stage", y)

#' @method vec_ptype2.projects_stage default
#' @export
vec_ptype2.projects_stage.default <- function(x, y, ...,
                                              x_arg = "x", y_arg = "y")
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)

#' @method vec_ptype2.projects_stage projects_stage
#' @export
vec_ptype2.projects_stage.projects_stage <- function(x, y, ...)
  new_projects_stage()

#' @method vec_ptype2.projects_stage character
#' @export
vec_ptype2.projects_stage.character <- function(x, y, ...) character()

#' @method vec_ptype2.character projects_stage
#' @export
vec_ptype2.character.projects_stage <- function(x, y, ...) character()

#' @method vec_cast projects_stage
#' @export vec_cast.projects_stage
#' @export
#' @rdname projects_stage-vctrs
vec_cast.projects_stage <- function(x, to, ...)
  UseMethod("vec_cast.projects_stage")

#' @method vec_cast.projects_stage default
#' @export
vec_cast.projects_stage.default <- function(x, to, ...)
  vec_default_cast(x, to)

#' @method vec_cast.projects_stage projects_stage
#' @export
vec_cast.projects_stage.projects_stage <- function(x, to, ...) x

#' @method vec_cast.projects_stage character
#' @export
vec_cast.projects_stage.character <- function(x, to, ...) validate_stage(x)

#' @method vec_cast.character projects_stage
#' @export
vec_cast.character.projects_stage <- function(x, to, ...) vec_data(x)

#' @method vec_cast.projects_stage integer
#' @export
vec_cast.projects_stage.integer <- function(x, ...) validate_stage(x)

#' @method vec_cast.integer projects_stage
#' @export
vec_cast.integer.projects_stage <- function(x, ...)
  as.integer(substr(vec_data(x), 1L, 1L))

#' @method vec_cast.double projects_stage
#' @export
vec_cast.double.projects_stage <- function(x, ...)
  as.double(substr(vec_data(x), 1L, 1L))

#' @method vec_cast.projects_stage double
#' @export
vec_cast.projects_stage.double <- function(x, ...) validate_stage(x)


#' @export
Ops.projects_stage <- function(e1, e2) {
  get(.Generic)(
    vapply(validate_stage(e1), as.integer, 0L),
    vapply(validate_stage(e2), as.integer, 0L)
  )
}




# Generic methods for match() --------------------------------------------------

#' @rdname projects_stage
#' @export
match.projects_stage <- function(x,
                                 table,
                                 nomatch       = NA_integer_,
                                 incomparables = NULL) {

  x <- validate_stage(x)
  table <- validate_stage(table)
  if (!is.null(incomparables)) {
    incomparables <- validate_stage(incomparables)
  }

  base::match(x, table, nomatch, incomparables)
}

#' @include set_generics.R
#' @rdname projects_stage
#' @export
methods::setMethod(
  "match",
  methods::signature(x = "projects_stage"),
  match.projects_stage
)

#' @include set_generics.R
#' @rdname projects_stage
#' @export
methods::setMethod(
  "match",
  methods::signature(table = "projects_stage"),
  match.projects_stage
)

#' @include set_generics.R
#' @rdname projects_stage
#' @export
methods::setMethod(
  "match",
  methods::signature(x = "projects_stage", table = "projects_stage"),
  match.projects_stage
)





# Generic methods for %in% -----------------------------------------------------

#' @rdname projects_stage
#' @export
`%in%.projects_stage` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

#' @include set_generics.R
#' @rdname projects_stage
#' @export
methods::setMethod(
  "%in%",
  methods::signature("projects_stage"),
  `%in%.projects_stage`
)

#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name projects_stage-vctrs
NULL
