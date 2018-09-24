################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA, id               = NA) {
  
  test_id_entry(id, what = "affiliation", max.length = 1, any.missing = TRUE)
  
  message("New affiliation:")
  
  change_table(rds_name         = "affiliations",
               action           = "new",
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
                       title     = NA, affiliations,
                       degree    = NA, email        = NA,
                       id        = NA) {
  
  p_path         <- p_path_internal()
  
  test_id_entry(id = id, what = "author", max.length = 1, any.missing = TRUE)
  
  if(!missing(affiliations)) {
    test_id_entry(id = affiliations, what = "affiliation")
  }
  
  new_author_row <- change_table(rds_name     = "authors", 
                                 p_path       = p_path,
                                 action       = "new",
                                 id           = id,
                                 last_name    = last_name,
                                 given_names  = given_names,
                                 title        = title,
                                 degree       = degree,
                                 email        = email)
  
  message("New author:")
  print(new_author_row)
  
  if(!missing(affiliations)) {
    new_author_affiliations <- 
      change_assoc(assoc_name = "author_affiliation_assoc",
                   p_path     = p_path,
                   new        = TRUE,
                   id1        = new_author_row$id,
                   id2        = affiliations)
    
    affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
    
    message("\nNew author's affiliations:")
    print(new_author_affiliations %>%
            dplyr::filter(.data$id1 == new_author_row$id) %>%
            dplyr::left_join(affiliation_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(affiliation_id = "id2"))
  }
  
  # print_association("author", new_author_row, "affiliation", affiliation_tibble)
  # invisible(return(TRUE))
}
################################################################################



################################################################################
#' @export
new_project <- function(title         = NA,   current_owner = NA,   
                        PI,                   investigators,   
                        creator       = NA,   stage         = NA,   
                        deadline_type = NA,   deadline      = as.Date(NA),
                        id            = NA,   status        = "just created",
                        
                        checklist = c("STROBE", "CONSORT", "PRIMA")) {
  
  p_path          <- p_path_internal()
  
  test_id_entry(id = id, what = "project", max.length = 1, any.missing = TRUE)
  if(!missing(PI)) {
    test_id_entry(id = PI, what = "PI")
  }
  if(!missing(investigators)) {
    test_id_entry(id = investigators, what = "investigator")
  }
  
  new_project_row <- change_table(rds_name      = "projects",
                                  p_path        = p_path,
                                  action        = "new",
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  if(!missing(PI)) {
    new_project_PI_assoc <-
      change_assoc(assoc_name = "project_PI_assoc",
                   p_path   = p_path,
                   new      = TRUE,
                   id1      = new_project_row$id,
                   id2      = PI)
  }
  
  if(!missing(investigators)) {
    new_project_investigator_assoc <-
      change_assoc(assoc_name        = "project_investigator_assoc",
                   p_path          = p_path,
                   new             = TRUE,
                   id1             = new_project_row$id,
                   id2             = investigators)
  }
  
  author_tibble  <- get_rds(make_rds_path("authors", p_path))
  
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
  
  if(!missing(PI)) {
    message("\nNew project's PI(s):")
    print(new_project_PI_assoc %>% 
            dplyr::filter(.data$id1 == new_project_row$id) %>% 
            dplyr::left_join(author_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(PI_id = id2))
  }
  
  if(!missing(investigators)) {
    message("\nNew project's investigators:")
    print(new_project_investigator_assoc %>% 
            dplyr::filter(.data$id1 == new_project_row$id) %>%
            dplyr::left_join(author_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(investigator_id = id2))
  }
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
