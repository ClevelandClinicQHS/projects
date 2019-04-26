
methods::setClass("projects_author")

#' \code{projects_author} class and its methods
#'
#' Objects of this class contain both the \code{id} and the \code{last_name} of
#' an author so that the package and the user, respectively, can easily identify
#' the author.
#'
#' Essentially, this is a character string of the form:
#'
#' \code{id: last_name}
#'
#' \code{new_projects_author()} merely coerces the object's class attribute to
#' \code{projects_author}.
#'
#' @section Numeric coercion methods: \code{\link{as.integer}()},
#'   \code{\link{as.double}()}, and \code{\link{as.numeric}()} return the
#'   \code{id} portion of the \code{projects_author} object as an
#'   integer/double. The methods for the equality and value matching functions
#'   described below make use of these numeric coercion methods. Users desiring
#'   to apply value matching functions other than the ones described below may
#'   similarly take advantage of these.
#'
#' @section Equality and value matching methods: Methods for \code{\link{==}},
#'   \code{\link{!=}}, \code{\link{match}()}, and \code{\link{\%in\%}} enable
#'   users to test equality and to value match among \code{projects_author}
#'   objects and as well as between \code{projects_author} objects and unclassed
#'   numbers/characters. When testing or matching against a numeric vector, the
#'   \code{projects_author} object is first coerced to an integer with the
#'   \code{as.integer()} method described above. When testing or matching
#'   against a character vector, the character vector is validated against the
#'   \code{\link{authors}()} table.
#'
#' @section \code{c()} method: A method for \code{\link{c}()} was also written
#'   so that the class attribute is not lost.
#'
#' @param x For \code{new_projects_author()}, any object. For
#'
#'   For the \code{as.*()} methods, a \code{projects_author} object.
#'
#'   For \code{\link{match}()} and \code{\link{\%in\%}}, an integer, a character
#'   string, or a \code{projects_author} object. See \code{\link{match}()} and
#'   \strong{Equality and value matching methods} below.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @param table An integer number, a character string, or a
#'   \code{projects_author} object. See \code{\link{match}()}  and
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
#' jones <- new_projects_author("33: Jones")
#'
#' as.integer(jones) # 33
#'
#' jones == 33       # TRUE
#' jones == 10       # FALSE
#' jones != 33       # FALSE
#'
#' jones %in% c(20:40)     # TRUE
#' match(jones, c(31:40))  # 3
#'
#' \dontrun{
#' # Not run because no authors() table is created within this example code.
#' jones == "jOnES"
#' # TRUE, assuming that the authors() table contains an author with an id of 3
#' # and a last_name beginning with "jones" (not case sensitive).
#' }
#'
#' x <- structure("32: Clinton", class = "dummyclass")
#' class(c(x))     # Does not retain class
#' class(c(jones)) # Retains class
#' @name projects_author
#' @export
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
#' @rdname projects_author
#' @export
as.integer.projects_author <- function(x, ...) {
  as.integer(stringr::str_extract(x, "^\\d+"), ...)
}


#' @rdname projects_author
#' @export
as.double.projects_author  <- function(x, ...) {
  as.double(stringr::str_extract(x, "^\\d+"), ...)
}


#' @rdname projects_author
#' @export
as.numeric.projects_author <- as.double.projects_author



# Subsetting methods, per ?`[.data.frame` -------------------------------------

#' @export
as.data.frame.projects_author <- as.data.frame.vector

#' @export
`[.projects_author` <- function(x, i, ...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}

#' @export
c.projects_author <- function(...) {
  new_projects_author(c(unlist(lapply(list(...), unclass))))
}

#' @export
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
      if (inherits(e2, "projects_author")) {
        e2 <- unclass(e2)
      } else {
        e2 <-
          as.character(
            lapply(
              e2,
              validate_projects_author,
              authors_table = authors_internal()
            )
          )
      }
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
#' @method match projects_author
#' @rdname projects_author
#' @export
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
    if (inherits(x, "projects_author")) {
      if (!inherits(table, "projects_author")) {
        table <- lapply(table, validate_projects_author)
      }
    } else {
      x <- lapply(x, validate_projects_author)
    }
    if (!is.null(incomparables)) {
      incomparables <- lapply(incomparables, validate_projects_author)
    }
  }

  base::match(x, table, nomatch, incomparables)
}

#' @include set_generics.R
methods::setMethod(
  "match",
  methods::signature(x = "projects_author"),
  match.projects_author
)

#' @include set_generics.R
methods::setMethod(
  "match",
  methods::signature(table = "projects_author"),
  match.projects_author
)



#' @method %in% projects_author
#' @rdname projects_author
#' @export
`%in%.projects_author` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

#' @include set_generics.R
methods::setMethod(
  "%in%",
  methods::signature(x = "projects_author"),
  `%in%.projects_author`
)

#' @include set_generics.R
methods::setMethod(
  "%in%",
  methods::signature(table = "projects_author"),
  `%in%.projects_author`
)
