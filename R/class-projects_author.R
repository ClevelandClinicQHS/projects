
#' \code{projects_author} vector
#'
#' Objects of this class contain both the \code{id} and the \code{last_name} of
#' an author so that the package and the user, respectively, can easily identify
#' the author.
#'
#' Essentially, this is a character string of the form:
#'
#' \code{id: last_name}
#'
#' \code{projects_author()} coerces an integer or character vector to a
#' \code{projects_author} object, validating each element against the existing
#' \code{\link{authors}()} table.
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
#' @param x For \code{projects_author()}, an integer or character vector. For
#'
#'   For \code{\link{match}()} and \code{\link{\%in\%}}, an integer, a character
#'   string, or a \code{projects_author} object. See \code{\link{match}()} and
#'   \strong{Equality and value matching methods} below.
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
#'
#' @examples
#' #############################################################################
#' # SETUP
#' old_home <- Sys.getenv("HOME")
#' old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' temp_dir <- tempfile("dir")
#' dir.create(temp_dir)
#' Sys.unsetenv("PROJECTS_FOLDER_PATH")
#' Sys.setenv(HOME = temp_dir)
#' setup_projects(path = temp_dir)
#' new_author("chuck", "jonesman", id = 33)
#' new_author("Hattie", "Hatsman", id = 45)
#' #############################################################################
#'
#' jones <- projects_author("33: Jones")
#'
#' jones
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
#' # Comparing a projects_author object to a character vector results in the
#' # character strings being validated against the authors() table. Then, the id
#' # numbers are compared.
#' jones == c("jOnES", "hat")   # TRUE FALSE
#'
#' #############################################################################
#' # Cleanup (or just restart R)
#' Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
#' @import vctrs
#' @export
projects_author <- function(x = character()) {
  x <- as.character(x)
  validate_projects_author(x)
}


new_projects_author <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "projects_author")
}


#' @rdname projects_author
#' @export
methods::setClass("projects_author")

#' @export
vec_ptype_abbr.projects_author <- function(x, ...) "prjaut"

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

  if (nrow(x_valid) == 0L) {
    new_projects_author(NA_character_)
  } else {
    new_projects_author(paste0(x_valid$id, ": ", x_valid$last_name))
  }
}




#' @rdname projects_author-vctrs
#' @method vec_ptype2 projects_author
#' @export
#' @export vec_ptype2.projects_author
vec_ptype2.projects_author <- function(x, y, ...)
  UseMethod("vec_ptype2.projects_author", y)

#' @method vec_ptype2.projects_author default
#' @export
vec_ptype2.projects_author.default <- function(x, y, ...,
                                               x_arg = "x", y_arg = "y")
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)

#' @method vec_ptype2.projects_author projects_author
#' @export
vec_ptype2.projects_author.projects_author <- function(x, y, ...)
  new_projects_author()

#' @method vec_ptype2.projects_author character
#' @export
vec_ptype2.projects_author.character <- function(x, y, ...) character()

#' @method vec_ptype2.character projects_author
#' @export
vec_ptype2.character.projects_author <- function(x, y, ...) character()

#' @method vec_cast projects_author
#' @export vec_cast.projects_author
#' @export
#' @rdname projects_author-vctrs
vec_cast.projects_author <- function(x, to, ...)
  UseMethod("vec_cast.projects_author")

#' @method vec_cast.projects_author default
#' @export
vec_cast.projects_author.default <- function(x, to, ...)
  vec_default_cast(x, to)

#' @method vec_cast.projects_author projects_author
#' @export
vec_cast.projects_author.projects_author <- function(x, to, ...) x

#' @method vec_cast.projects_author character
#' @export
vec_cast.projects_author.character <- function(x, to, ...)
  validate_projects_author(x)

#' @method vec_cast.character projects_author
#' @export
vec_cast.character.projects_author <- function(x, to, ...) vec_data(x)

#' @method vec_cast.projects_author integer
#' @export
vec_cast.projects_author.integer <- function(x, ...)
  validate_projects_author(x)

#' @method vec_cast.integer projects_author
#' @export
vec_cast.integer.projects_author <- function(x, ...)
  as.integer(stringr::str_extract(vec_data(x), "^\\d+"))

#' @method vec_cast.double projects_author
#' @export
vec_cast.double.projects_author  <- function(x, ...)
  as.double(stringr::str_extract(vec_data(x), "^\\d+"))


#' @export
Ops.projects_author <- function(e1, e2) {

  if (!any(c("==", "!=") == .Generic)) {
    stop(gettextf("%s not meaningful for projects_authors", sQuote(.Generic)))
  }

  if (rlang::is_integerish(e1) || rlang::is_integerish(e2)) {
    e1 <- as.integer(e1)
    e2 <- as.integer(e2)
  } else if (inherits(e1, "projects_author")) {
    e1 <- as.integer(e1)
    if (inherits(e2, "projects_author")) {
      e2 <- as.integer(e2)
    } else {
      e2 <-
        vapply(
          e2,
          function(x) as.integer(validate_projects_author(x)),
          integer(1L),
          USE.NAMES = FALSE
        )
    }
  } else {
    e1 <-
      vapply(
        e1,
        function(x) as.integer(validate_projects_author(x)),
        integer(1L),
        USE.NAMES = FALSE
      )
    e2 <- as.integer(e2)
  }

  get(.Generic)(e1, e2)
}



# Generic methods for match() --------------------------------------------------

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
#' @rdname projects_author
#' @export
methods::setMethod(
  "match",
  methods::signature(x = "projects_author"),
  match.projects_author
)

#' @include set_generics.R
#' @rdname projects_author
#' @export
methods::setMethod(
  "match",
  methods::signature(table = "projects_author"),
  match.projects_author
)

#' @include set_generics.R
#' @rdname projects_author
#' @export
methods::setMethod(
  "match",
  methods::signature(x = "projects_author", table = "projects_author"),
  match.projects_author
)




# Generic methods for %in% -----------------------------------------------------

#' @rdname projects_author
#' @export
`%in%.projects_author` <- function(x, table) {
  match(x, table, nomatch = 0L) > 0L
}

#' @include set_generics.R
#' @rdname projects_author
#' @export
methods::setMethod(
  "%in%",
  methods::signature("projects_author"),
  `%in%.projects_author`
)

#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name projects_author-vctrs
NULL
