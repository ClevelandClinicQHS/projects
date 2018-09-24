################################################################################
################################################################################
# Internal helper functions

# To be scrapped. Currently only used by archive_delete()
# project_data <- function() {
#   
#   p_path <- p_path_internal()
#   
#   rds_paths <- 
#     purrr::map(c("projects", "authors", "affiliations"),
#                make_rds_path,
#                p_path = p_path) %>% 
#     setNames(c("list_path", "authors_path", "affiliations_path"))
#   
#   return(c(list(folder_path = p_path),
#            rds_paths,
#            purrr::map(rds_paths, get_rds) %>%
#              setNames(c("list", "authors", "affiliations"))))
# }

make_project_name <- function(project_id) {
  paste0("p", stringr::str_pad(project_id, width = 4, side = "left", pad = "0"))
}

make_project_path <- function(project_name, p_path = p_path_internal()) {
  fs::path(p_path, project_name) %>%
    unclass()
}



check_all_rds <- function() {
  purrr::walk(c("affiliations", "author_affiliation_assoc", "authors",
                "project_investigator_assoc", "project_PI_assoc", "projects"),
              function(x) {
                print(x)
                print(readRDS(fs::path(projects_folder(), "metadata", x,
                                       ext = "rds")))
              })
}