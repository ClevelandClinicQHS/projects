#' projects: A project infrastructure for researchers.
#'
#' The \code{projects} package provides a project infrastructure with a focus on
#' manuscript creation. It creates a project folder with a single command,
#' containing subdirectories for specific components, templates for manuscripts,
#' and so on.
#'
#' @section Knitting: There are several functions that require interactive user
#'   confirmation via the console. Since interactive console input is
#'   incompatible with knitting via R Markdown files, the \code{projects}
#'   package was coded such that user confirmation is bypassed when
#'   \code{isTRUE(getOption('knitr.in.progress')) == TRUE}. Therefore, all
#'   \code{projects} package functions are usable when knitting. \strong{Knit
#'   with caution!}
#'
#' @section Acknowledgements: The authors of this package acknowledge the
#'   support provided by members of the Northeast Ohio Cohort for
#'   Atherosclerotic Risk Estimation (NEOCARE) investigative team: Claudia
#'   Coulton, Douglas Gunzler, Darcy Freedman, Neal Dawson, Michael Rothberg,
#'   David Zidar, David Kaelber, Douglas Einstadter, Alex Milinovich, Monica
#'   Webb Hooper, Kristen Hassmiller-Lich, Ye Tian (Devin), Kristen Berg, and
#'   Sandy Andrukat.
#'
#' @section Funding: This work was supported by The National Institute on Aging
#'   of the National Institutes of Health under award number R01AG055480. The
#'   content is solely the responsibility of the authors and does not
#'   necessarily represent the official views of the National Institutes of
#'   Health.
#'
#' @seealso \code{\link{setup_projects}()} for getting started.
#'
#' @docType package
#' @name projects-package
NULL
