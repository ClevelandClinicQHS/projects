

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
