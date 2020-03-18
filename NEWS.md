
# projects 2.1.0

## Major updates:
- Addition of `rename_projects_folder()` and `move_projects_folder()`
- Addition of `path` argument to `projects()`, allowing users to filter by subdirectory
- Incorporation of `vctrs` package for `projects_stage` and `projects_author` classes

## Minor updates:
- `projects()` now prints a maximum of 100 rows by default.
- `open_project()` attempts to locate projects on shared servers when the local `projects_folder()` value differs from the server's `projects_folder()` value
- Backslash inserted before any single quotation marks in projects folder path before being written to .Renviron
- Bug, documentation, README improvements

# projects 2.0.0

## Major updates:
- `new_project()` now copies a single template project folder within *.templates* instead of assembling multiple template components into a file. This allows users to customize project folders to a very high degree. Thus, `new_project()` has fewer arguments, having only `template_folder` insofar as template-related arguments.
- Consequently, header/title page YAML text is NOT written automatically into .Rmd files when they are created by `new_project()`. Users must run `header()` and copy the resulting text into desired *.Rmd* files.
- Added the function email_authors(), which opens a new email for the currently open project (or, a specified project).

## Minor updates:
- There are now separate arguments in `new_project()` and `copy_project()` for specifying the new project's folder name. It can be distinct from a project's `short_title` and its `id` number.
- Updated documentation, 
- Tilde expansion is performed in `setup_projects()`
- Added tests

# projects 1.3.0

## Major updates
- `save_session_info()` function for saving `sessioninfo::session_info()` to a text file.
- `export_project()` for zipping project folders

## Minor updates
- Improvements to default .Rmd files.
- Code improvements

# projects 1.2.0.9000

## Major updates
- Moved all header material to YAML when generating 01_protocol.Rmd and 04_report.Rmd.
- YAML Output options no longer written in function; rather, they are expected to already be in the templates.
- Addition of .docx style template.

## Minor updates
- Other minor tweaks to default .Rmd files.

# projects 1.1.4.9000

## Major updates
- Bug fix: exported all methods for the `projects_stage` and `projects_author` S3 classes so that the class would not be stripped.

## Minor updates
- Made `reorder_authors()` and all `edit_*()` functions to stop printing metadata after successful editing.
- Removed the `reprint_header` argument from `reorder_authors()` function, but added a message beckoning user to run `header()`, as in `edit_project()`.

# projects 1.1.3.9000

## Minor updates:
- Changed printing of projects table so it displays projects in descending order by stage before sorting them by id.


# projects 1.1.2.9000
 
## Minor updates:
- Required a later rlang version so that as_label() is available.


# projects 1.1.1

## Major updates:
- Fix of show-stopping bugs with update_metadata() and edit_project()


# projects 1.1.0

## Minor updates:
- Documentation updates for functions created in version 1.0.0 and 1.0.1.


# projects 1.0.1.9000

## Major updates:
- Fixed bug in internal acquisition of metadata tables. 
- Fixed bug in open_projects()

## Minor update:
- Changed number of rows that projects(), authors(), and affiliations() tibbles will print



# projects 1.0.0.9000

## Major update:
- Created S3 classes implemented for each of stage and special authors (current_owner, corresp_auth, creator).
- Added update_metadata() function to assist with updating metadata from projects version 0.X.X to 1.X.X
- Addition of ideas(), new_idea(), and manuscripts().
- Overhaul of underlying code.

## Minor updates:
- More customizable print options for projects(), 
- Minor improvements to printing of projects at the end of new_project() and edit_project()


# projects 0.2.1.9000

## Major update:
- Instances of fs::path_home() were changed to fs::path_home_r() so that .Renviron files would be put in the correct place (i.e., the directory that R considers to be the home directory, which is where R actually looks for .Renviron files).



# projects 0.2.0.9000

## Major updates

Users can now create custom names for project folders, linking the name to `short_title`:
- Added the function `rename_folder()`, which enables the user to rename project folders. Added an example of this.
- Added the logical argument `stitle_as_folder` to `new_project()`, which if `TRUE` makes the folder name of new projects the same as its `short_title`.
- Added the argument `new_short_title` to `copy_project()`, which enables users to change the project copy's folder name and/or `short_title`.
- Made the `path` argument in `copy_project()` second instead of fourth.
  
## Minor updates:
- Made `open_project()` better handle instances of missing/multiple .Rproj files.
- Documentation updates



# projects 0.1.1.9000

## Minor updates:
- Updated README.md so that it properly reflects how to install the `projects` package.
- Updated README.md so that it contains CRAN version and download count badges.



# projects 0.1.0

## Major updates:
- Release.
