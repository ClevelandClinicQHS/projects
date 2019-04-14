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
