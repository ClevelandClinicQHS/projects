################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA, id = NA) {
  
  message("New affiliation:")
  
  set_rds(rds_name         = "affiliations",
          new              = TRUE,
          edit             = FALSE,
          id               = id,
          department_name  = department_name,
          institution_name = institution_name,
          address          = address)
}
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
new_author <- function(last_name = NA, given_names  = NA,
                       title     = NA, affiliations = NA,
                       degree    = NA, email        = NA,
                       id        = NA) {
  
  p_path         <- p_path_internal()
  new_author_row <- set_rds(rds_name     = "authors", 
                            p_path       = p_path,
                            new          = TRUE,
                            edit         = FALSE,
                            id           = id,
                            last_name    = last_name,
                            given_names  = given_names,
                            title        = title,
                            degree       = degree,
                            email        = email)
  
  author_affiliation_tibble <- 
    set_rds(rds_name       = "author_affiliation_assoc",
            p_path         = p_path,
            edit           = FALSE,
            author_id      = new_author_row$id,
            affiliation_id = affiliations)
  
  affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  message("New author:")
  print(new_author_row)

  message("\nNew author's affiliations:")
  print(author_affiliation_tibble %>%
          dplyr::filter(.data$author_id == new_author_row$id) %>%
          dplyr::left_join(affiliation_tibble,
                           by = c("affiliation_id" = "id")) %>%
          dplyr::select(-.data$author_id))
  
  # print_association("author", new_author_row, "affiliation", affiliation_tibble)
  # invisible(return(TRUE))
}
################################################################################



################################################################################
#' @export
new_project <- function(title         = NA,   current_owner = NA,   
                        PI            = NA,   investigators = NA,   
                        creator       = NA,   stage         = NA,   
                        deadline_type = NA,   deadline      = as.Date(NA),
                        id            = NA,   status        = "just created",
                        
                        checklist = c("STROBE", "CONSORT", "PRIMA")) {
  
  p_path          <- p_path_internal()
  new_project_row <- new_rds_item(rds_name      = "projects",
                                  p_path        = p_path,
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  
  project_PI_assoc_tibble <-
    set_rds(rds_name   = "project_PI_assoc",
            p_path     = p_path,
            edit  = FALSE,
            project_id = new_project_row$id,
            PI_id      = PI)
  
  project_investigator_assoc_tibble <-
    set_rds(rds_name        = "project_investigator_assoc",
            p_path          = p_path,
            edit       = FALSE,
            project_id      = new_project_row$id,
            investigator_id = investigators)
  
  author_tibble <- get_rds(make_rds_path("authors", p_path))
  
  pXXXX_name     <- make_project_name(new_project_row$id)
  pXXXX_path     <- make_project_path(pXXXX_name, p_path)
  
  fs::dir_create(fs::path(pXXXX_path, c("data", "progs", "manuscript",
                                        "figures")))
  
  fs::file_copy(path     = system.file("extdata", "pXXXX_protocol.docx",
                                       package = "projects"),
                new_path = fs::path(pXXXX_path, paste0(pXXXX_name, "_protocol"),
                                    ext = "docx"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  
  message("Project ", new_project_row$id, " has been created at ", pXXXX_path)
  print(new_project_row)
  
  message("\nNew project's PI(s):")
  print(project_PI_assoc_tibble %>% 
          dplyr::filter(.data$project_id == new_project_row$id) %>% 
          dplyr::left_join(author_tibble,
                           by = c("PI_id" = "id")) %>% 
          dplyr::select(-.data$project_id))
  
  message("\nNew project's investigators:")
  print(project_investigator_assoc_tibble %>% 
          dplyr::filter(.data$project_id == new_project_row$id) %>%
          dplyr::left_join(author_tibble,
                           by = c("investigator_id" = "id")) %>% 
          dplyr::select(-.data$project_id))
}
################################################################################



################################################################################
################################################################################
# The internal function that prints associations
# print_association <- function(main_tibble_name, main_tibble,
#                               assoc_name, assoc_tibble) {
# 
#   message("New ", main_tibble_name, ":")
#   print(main_tibble)
#   
#   message("\nNew ", main_tibble_name, "'s ", assoc_name, "s:")
#   print(assoc_tibble %>%
#           dplyr::filter(parse(text = paste0(".data$",
#                                             main_tibble_name, "_id %in% ",
#                                             main_tibble, "$id"))) %>%
#           dplyr::left_join(parse(text = paste0(assoc_tibble, ", by = c(\"",
#                                                assoc_name,
#                                                "_id\" = \"id\")"))) %>%
#           dplyr::select(parse(text = paste0("-.data$",
#                                             main_tibble_name, "_id"))))
# }
