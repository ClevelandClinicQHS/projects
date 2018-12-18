## Resubmission 4
This is the fourth resubmission. In this version I have:

* Overhauled examples so that they all take place entirely in tempdir() and no longer touch files in the home directory.
* Added a name to "Acknowledgements" section of the package Rd file.
* Minor changes to README.md as well as the location of files used in its creation: some are now in man/README
* Fixed bug in which new_project() write a file in the wrong directory.
* Added the logical argument "archived" to several functions.

## Resubmission 3
This is the third resubmission. In this version I have:

* Changed the `setup_projects()` example from:
```
setup_projects(tempdir())
```
* to:
```
# Not run so that developers' home .Renviron file will be left alone during
# package checking.
\dontrun{
setup_projects(fs::path_home())
}
```

* Added `tidyverse` and `here` to the suggests.


## Resubmission 2
This is the second resubmission. In this version I have:

* Removed the funding comment from the LICENSE file.
* Changed the comment in the last `person()` entry in the DESCRIPTION file to include the funding comment.
* Added the file projects.R, which only contains documentation of the package itself (including the funding statement).
* Changed the internal function `aa_header()` so that it uses carats (`^`) instead of html (`<sup>` and `</sup>`) to create superscripts when writing .Rmd files or writing to the console.
* Made `setup_projects()` to not throw an error (i.e., `stop()`) if user tries to change the projects folder path without specifying `overwrite = TRUE`. Now it simply sends a `message()`.
* Made `setup_projects()` return the path of the projects folder no matter how the function ended.


## Resubmission 1
This is a resubmission. In this version I have:

* Changed the `setup_projects()` example from:

```
\dontrun{
setup_projects("C:/Users/Loretta/")
}
```

* to:

```
setup_projects(tempdir())
```
* Changed the internal function `setup_projects()` so that it will only require a user prompt if the user is changing the projects folder path (i.e., if `.Renviron` already contains a `PROJECTS_FOLDER_PATH` value)
* Added more authors in DESCRIPTION
* Added Github URL in DESCRIPTION
* Added funding comment to LICENSE

## Test environments
* Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: CentOS Linux 7 (Core)
  R 3.5.0
* Platform: i386-w64-mingw32/i386 (32-bit)
  Running under: Windows >= 8 (build 9200)
  R 3.5.1
* Platform: x86_64-w64-mingw32 (64-bit)
  R Under development (unstable) (2018-11-26 r75682)
  using session charset: ISO8859-1

* New submission.


## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.
