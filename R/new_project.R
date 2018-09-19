#' @importFrom rlang .data
#' @export
new_project <- function(number,                        title    = NA_character_,
                        current_owner = NA_character_, PI       = NA_character_,
                        investigators = NA_character_, creator  = NA_character_,
                        stage         = NA_character_, path     = NA_character_,
                        deadline_type = NA_character_, deadline = as.Date(NA),
                        status        = "just created",
                        checklist     = c("STROBE", "CONSORT", "PRIMA")){

  p <- projects()
  
  if(missing(number)) {
    
    # If there are no projects, number will be 1.
    # Otherwise, number will be 1 + the highest existing project number.
    # HOWEVER: if project number 9999 is taken, number will be the
    # lowest available number in 1:9999.
    if(nrow(p$list) == 0) {
      number <- 1L
    }
    else if(max(p$list$number) < 9999L) {
      number <- max(p$list$number) + 1L
    }
    else if(!all(1L:9999L %in% p$list$number)){
      number <- min(setdiff(1L:9999L, p$list$number))
    }
    else {
      stop("projects folder is full. Delete or archive one or more of them.")
    }
  }
  else {
    
    if(isFALSE(checkmate::test_integerish(number, lower = 1L,
                                          upper = 9999L, any.missing = FALSE,
                                          len = 1L))) {
      stop("number must be an integer")
    }
    
    number <- as.integer(number)
    
    if(number %in% p$list$number) {
      stop('number already taken. Try a different one or leave the ',
           'argument blank for automatic selection.')
    }
  }

  pXXXX <- make_project(number, p$folder_path)
  
  fs::dir_create(fs::path(pXXXX$path, c("data", "progs", "manuscript",
                                        "figures")))
  
  fs::file_copy(path     = system.file("extdata", "pXXXX_protocol.docx",
                                       package = "projects"),
                new_path = fs::path(pXXXX$path, paste0(pXXXX$name, "_protocol"),
                                    ext = "docx"))

  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX$path, pXXXX$name, ext = "Rproj"))

  #p$list <-
  dplyr::bind_rows(
    p$list,
    tibble::tibble(number        = number,              title    = title,
                   current_owner = current_owner,       PI       = list(PI),
                   investigators = list(investigators), creator  = creator,
                   stage         = stage,               path     = pXXXX$path,
                   deadline_type = deadline_type,       deadline = deadline,
                   status        = status)) %>%
    dplyr::arrange(.data$number) %>% 
    saveRDS(file = p$list_path)

  message("Project ", pXXXX$name, " has been created at ", pXXXX$path)
}
