################################################################################
#' @export
new_author <- function(last_name = NA, given_names  = NA,
                       title     = NA, affiliations = NA,
                       degree    = NA, email        = NA) {
  
  set_rds(rds_name     = "authors", 
          last_name    = last_name,
          given_names  = given_names,
          title        = title,
          affiliations = list(affiliations),
          degree       = degree,
          email        = email)
}
################################################################################



################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA) {
  set_rds(rds_name         = "affiliations",
          department_name  = department_name,
          institution_name = institution_name,
          address          = address)
}
################################################################################



################################################################################
#' @export
new_project <- function(title         = NA,   current_owner = NA,   
                        PI            = NA,   investigators = NA,   
                        creator       = NA,   stage         = NA,   
                        deadline_type = NA,   deadline      = as.Date(NA),
                        id            = NULL, status        = "just created",
                        
                        checklist = c("STROBE", "CONSORT", "PRIMA")) {
  
  # fields <- list(id            = id,
  #                title         = title,
  #                current_owner = current_owner,
  #                PI            = list(as.character(PI)),
  #                investigators = list(as.character(investigators)),
  #                creator       = creator,
  #                stage         = stage,
  #                deadline_type = deadline_type,
  #                deadline      = deadline,
  #                status        = status)
  
  p_path         <- p_path_internal()
  project_tibble <- set_rds(rds_name      = "projects",
                            p_path        = p_path,
                            id            = id,
                            title         = title,
                            current_owner = current_owner,
                            PI            = list(as.character(PI)),
                            investigators = list(as.character(investigators)),
                            creator       = creator,
                            stage         = stage,
                            deadline_type = deadline_type,
                            deadline      = deadline,
                            status        = status)
  
  pXXXX_name     <- make_project_name(project_tibble[1,1])
  pXXXX_path     <- make_project_path(pXXXX_name, p_path)
  
  fs::dir_create(fs::path(pXXXX_path, c("data", "progs", "manuscript",
                                        "figures")))
  
  fs::file_copy(path     = system.file("extdata", "pXXXX_protocol.docx",
                                       package = "projects"),
                new_path = fs::path(pXXXX_path, paste0(pXXXX_name, "_protocol"),
                                    ext = "docx"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  
  message("Project ", pXXXX_name, " has been created at ", pXXXX_path)
  
  project_tibble
}
################################################################################



################################################################################
################################################################################
# The internal function that adds a row to one of the rds objects

set_rds <- function(rds_name, p_path = p_path_internal(), ...) {
  
  new_row    <- list(...)
  
  rds_path   <- make_rds_path(rds_name, p_path)
  rds_tibble <- get_rds(rds_path)
  
  ######################################################################
  # This section is entirely dedicated to creating an id for the new row
  if(rds_name == "projects") {
    if(is.null(new_row$id)) {
      
      # If there are no projects, id will be 1.
      # Otherwise, id will be 1 + the highest existing project id.
      # HOWEVER: if project id 9999 is taken, id will be the
      # lowest available id in 1:9999.
      max_id <- max(rds_tibble$id, 0L)
      if(max_id < 9999L) {
        new_row$id <- max_id + 1L
      }
      else if(!all(1L:9999L %in% rds_tibble$id)) {
        new_row$id <- min(setdiff(1L:9999L, rds_tibbleid))
      }
      else {
        stop("projects folder is full. Delete or archive one or more of them.")
      }
    }
    else {
      
      if(isFALSE(checkmate::test_integerish(new_row$id,
                                            lower = 1L, upper = 9999L,
                                            any.missing = FALSE, len = 1L))) {
        stop("id must be an integer; not ")
      }
      
      new_row$id <- as.integer(new_row$id)
      
      if(new_row$id %in% rds_tibble$id) {
        stop('id already taken. Try a different one or leave the ',
             'argument blank for automatic selection.')
      }
    }
  }
  else {
    new_row$id <- sample(x = setdiff(10:99, rds_tibble$id), size = 1)
  }
  ######################################################################
  
  rds_tibble <- dplyr::bind_rows(new_row, rds_tibble)
  
  saveRDS(rds_tibble, rds_path)
  
  return(rds_tibble)
}
