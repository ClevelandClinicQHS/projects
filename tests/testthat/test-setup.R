context("Setup")

test_that("Setup works", {

  old_home <- Sys.getenv("HOME")
  old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
  temp_dir <- tempfile("dir")
  dir.create(temp_dir)
  Sys.setenv(HOME = temp_dir)
  Sys.unsetenv("PROJECTS_FOLDER_PATH")

  expect_equal(
    fs::path_tidy(setup_projects(temp_dir)),
    fs::path_tidy(fs::path(temp_dir, "projects"))
  )

  expect_message(
    setup_projects(temp_dir, folder_name = "projects2"),
    "The .Renviron file at"
  )

  expect_equal(
    sort(fs::dir_ls(projects_folder(), all = TRUE, recurse = TRUE)),
    sort(
      fs::path(
        projects_folder(),
        c(
          ".metadata",
          ".metadata/affiliations.rds",
          ".metadata/author_affiliation_assoc.rds",
          ".metadata/authors.rds",
          ".metadata/project_author_assoc.rds",
          ".metadata/projects.rds",
          ".templates",
          ".templates/CONSORT_protocol.Rmd",
          ".templates/STROBE_protocol.Rmd",
          ".templates/default_folder",
          ".templates/default_folder/data",
          ".templates/default_folder/data_raw",
          ".templates/default_folder/figures",
          ".templates/default_folder/manuscript",
          ".templates/default_folder/pXXXX.Rproj",
          ".templates/default_folder/progs",
          ".templates/default_folder/progs/01_protocol.Rmd",
          ".templates/default_folder/progs/02_datawork.Rmd",
          ".templates/default_folder/progs/03_analysis.Rmd",
          ".templates/default_folder/progs/04_report.Rmd",
          ".templates/default_folder/progs/citations.bib",
          ".templates/default_folder/progs/style.css",
          ".templates/default_folder/progs/styles.docx"
        )
      )
    )
  )

  expect_error(
    new_affiliation(
      department_name = "Math Dept.",
      institution_name = "Springfield College",
      address = "123 College St, Springfield, AB"
    ),
    NA
  )

  expect_error(
    new_affiliation(
      department_name = "Art Department",
      institution_name = "Springfield College",
      address = "321 University Boulevard, Springfield, AB",
      id = 42
    ),
    NA
  )

  expect_error(
    edit_affiliation("Math Dept", department_name = "Mathematics Department"),
    NA
  )

  expect_error(
    new_author(
      given_names = "Rosetta",
      last_name = "Stone",
      affiliations = c(42, "Math"),
      degree = "PhD",
      email = "slab@rock.net",
      phone = "867-555-5309",
      id = 8888
    ),
    NA
  )

  expect_error(
    new_author(id = 8888, given_names = "Johhhhhn")
  )

  expect_error(
    new_author(
      given_names = "Spiro",
      last_name = "Agnew",
      degree = "LLB",
      affiliations = "Art D",
      id = 13
    ),
    NA
  )

  expect_error(
    new_author(given_names = "Plato", id = 303),
    NA
  )

  expect_error(
    edit_author(
      author = 303,
      given_names = NA,
      last_name = "Plato",
      title = "Nut"
    ),
    NA
  )

  expect_error(
    edit_author("Spiro", affiliations = ~ -"Art D" + Math),
    NA
  )


  expect_error(
    new_project(
      title = "Understanding the Construction of the United States",
      short_title = "USA",
      authors = c(13, "Stone"),
      stage = 4,
      deadline = "2055-02-28",
      deadline_type = "submission",
      parent_directory = "famous_studied/philosophers/rocks",
      corresp_auth = "Stone",
      current_owner = "agnew",
      make_directories = TRUE,
      status = "waiting on IRB"
    ),
    NA
  )


  expect_error(
    edit_project(
      "Understanding",
      short_title = "usa1",
      authors = ~ + "303" - Stone,
      corresp_auth = "plato"
    ),
    NA
  )

  expect_error(
    new_idea(title = "Boiling the Ocean"),
    NA
  )

  expect_identical(
    affiliations(),
    tibble::tribble(
      ~id, ~department_name,         ~institution_name,     ~address,
      1L,  "Mathematics Department", "Springfield College", "123 College St, Springfield, AB",
      42L, "Art Department",         "Springfield College", "321 University Boulevard, Springfield, AB"
    )
  )



  expect_identical(
    authors(),
    tibble::tribble(
      ~id,   ~given_names, ~last_name, ~title, ~degree, ~email,          ~phone,
      13L,   "Spiro",      "Agnew",    NA,     "LLB",   NA,              NA,
      303L,  NA,           "Plato",    "Nut",  NA,      NA,              NA,
      8888L, "Rosetta",    "Stone",    NA,     "PhD",   "slab@rock.net", "867-555-5309"
    )
  )

  expect_identical(
    projects(verbose = TRUE, all_stages = TRUE),
    tibble::tribble(
      ~id, ~title,                                                ~short_title, ~current_owner,                                    ~status,          ~deadline_type, ~deadline,                             ~stage,                                               ~path,                                                                              ~corresp_auth,                                      ~creator,
      1L,  "Understanding the Construction of the United States", "usa1",       structure("13: Agnew", class = "projects_author"), "waiting on IRB", "submission",   lubridate::as_datetime("2055-02-28"),  structure("4: manuscript", class = "projects_stage"), unclass(fs::path(projects_folder(), "famous_studied/philosophers/rocks", "p0001")), structure("303: Plato", class = "projects_author"), structure(paste0("0: ", Sys.info()["user"]), class = "projects_author"),
      2L,  "Boiling the Ocean",                                   NA,           structure(NA, class = "projects_author"),          "just an idea",   NA,             lubridate::as_datetime(NA_character_), structure("0: idea",       class = "projects_stage"), unclass(fs::path(projects_folder(), "p0002")),                                      structure(NA, class = "projects_author"),           structure(paste0("0: ", Sys.info()["user"]), class = "projects_author")
    )
  )

  expect_identical(
    get_rds(make_rds_path("project_author_assoc")),
    tibble::tibble(id1 = c(1L, 1L), id2 = c(13L, 303L))
  )

  expect_identical(
    get_rds(make_rds_path("author_affiliation_assoc")),
    tibble::tibble(id1 = c(8888L, 8888L, 13L), id2 = c(42L, 1L, 1L))
  )

  Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
})
