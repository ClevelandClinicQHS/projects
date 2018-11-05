

make_project_name <- function(project_id) {
  paste0("p", stringr::str_pad(project_id, width = 4, side = "left", pad = "0"))
}

make_project_path <- function(project_name, p_path = p_path_internal()) {
  fs::path(p_path, project_name) %>% unclass()
}


check_all_rds <- function() {
  purrr::walk(c("affiliations", "author_affiliation_assoc", "authors",
                "project_author_assoc", "project_PI_assoc", "projects"),
              function(x) {
                print(x)
                print(readRDS(fs::path(projects_folder(), "metadata", x,
                                       ext = "rds")))
              })
}


process_formula_numbers <- function(x) {
  
  as.call(
      lapply(
        X   = as.list(x),
        FUN = function(y) {
          if(is.atomic(y) && length(y) == 1) {
            return(as.name(y))
          }
          else if(is.name(y)) {
            return(y)
          }
          else if(is.call(y)) {
            return(process_formula_numbers(y))
          }
          else {
            stop("Don't know how to handle object of type ", typeof(y),
                 ". Formulas must only contain author names, IDs, plus signs, ",
                 "and minus signs.")
          }})
  )
}



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
