projects_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#projects_path <- "dummy"

# @export
# refresh_projects_path <- function() {
#   
#   readRenviron(fs::path(Sys.getenv("HOME"), ".Renviron"))
#   
#   assign("projects_path", Sys.getenv("PROJECTS_FOLDER_PATH"),
#          asNamespace("projects"))
# }

# .onLoad <- function(libname, pkgname) {
#   refresh_projects_path()
#   if(projects_path == "") {
#     message('"projects" folder not found. Please run setup_projects_folder()')
#   }
# }

#   projects_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#   
#   if(check && projects_path == "") {
#     check_message <-
#       '"projects" folder not found. Please run setup_projects_folder()'
#     if(error) {
#       return(check_message)
#     }
#     else {
#       stop(check_message)
#     }
#   }
#   return(projects_path)
# }
# 
# 
# project_list <- NULL
# 
# .onLoad <- function(libname, pkgname) {
#   projects_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#   
#   if(projects_path == "") {
#     '"projects" folder not found. Please run setup_projects_folder()'
#   }
#   assign("projects_path", projects_path, pos = asNamespace("projects"))
# }

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(check_projects_path(error = FALSE))
}
#   
#   if(projects_path(check = FALSE) == "") {
#     packageStartupMessage(
#       paste0('"projects" folder not found. Please run setup_projects_folder()'))
#   }
#   else {
#     assign("project_list",
#            readRDS(fs::path(projects_path, "project_list", ext = "rds")),
#            pos = asNamespace("projects"))
#     packageStartupMessage(paste0('"projects" folder located at ',
#                                  projects_path(check = FALSE)))
#   }
# }
