test_that("Everything works", {

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
    "\nThe environment variable PROJECTS_FOLDER_PATH indicates"
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
          ".metadata/tasks.rds",
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
    ),
    ignore_attr = "names"
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
      corresp_auth = "plato",
      impact = Inf
    ),
    NA
  )

  expect_error(
    new_idea(title = "Boiling the Ocean", impact = pi),
    NA
  )

  expect_identical(
    affiliations(),
    dplyr::tribble(
      ~id, ~department_name,         ~institution_name,     ~address,
      1L,  "Mathematics Department", "Springfield College", "123 College St, Springfield, AB",
      42L, "Art Department",         "Springfield College", "321 University Boulevard, Springfield, AB"
    ) %>% structure(class = c("projects_metadata_tbl", class(.)))
  )

  expect_identical(
    authors(),
    dplyr::tribble(
      ~id,   ~last_name, ~given_names, ~title, ~degree, ~email,          ~phone,
      13L,   "Agnew",    "Spiro",      NA,     "LLB",   NA,              NA,
      303L,  "Plato",    NA,           "Nut",  NA,      NA,              NA,
      8888L, "Stone",    "Rosetta",    NA,     "PhD",   "slab@rock.net", "867-555-5309"
    ) %>% structure(class = c("projects_metadata_tbl", class(.)))
  )

  expect_identical(
    projects(verbose = TRUE, all_stages = TRUE),
    dplyr::tibble(
      id = 1L:2L,
      title = c("Understanding the Construction of the United States",
                "Boiling the Ocean"),
      short_title = c("usa1", NA),
      current_owner = new_projects_author(c("13: Agnew", NA)),
      status = c("waiting on IRB", "just an idea"),
      deadline_type = c("submission", NA),
      deadline = lubridate::as_datetime(c("2055-02-28", NA)),
      stage = new_projects_stage(c("4: manuscript", "0: idea")),
      impact = c(Inf, pi),
      path = unclass(c(fs::path(projects_folder(),
                                "famous_studied/philosophers/rocks/p0001"),
                       fs::path(projects_folder(), "p0002"))),
      corresp_auth = new_projects_author(c("303: Plato", NA)),
      creator = new_projects_author(rep(paste0("0: ", Sys.info()["user"]), 2L))
    ) %>% structure(class = c("projects_metadata_tbl", class(.)))
  )

  expect_identical(
    get_rds(make_rds_path("project_author_assoc")),
    dplyr::tibble(id1 = c(1L, 1L), id2 = c(13L, 303L))
  )

  expect_identical(
    get_rds(make_rds_path("author_affiliation_assoc")),
    dplyr::tibble(id1 = c(8888L, 8888L, 13L), id2 = c(42L, 1L, 1L))
  )

  expect_error(
    new_task(project = 2,
             task = "put the horse before the cart",
             lead = "spiro",
             timing = NaN),
    NA
  )

  expect_error(
    new_task(project = 1,
             task = "learn something",
             effort = pi,
             lead = "Stone",
             status = "foobar",
             timing = Inf),
    NA
  )

  expect_error(
    new_task(project = 1,
             task = "take a break",
             TID = 600.66,
             effort = -100,
             status = "throw it all away",
             lead = "303"),
    NA
  )

  expect_error(
    new_task(
      project = 1,
      task = "tie your shoes",
      TID = 2,
      lead = 303),
    NA
  )

  expect_error(
    new_task(
      project = 1,
      task = "put out the fire",
      TID = .5,
      lead = "stone"),
    NA
  )

  expect_error(
    edit_task(
      project = "understanding",
      TID = 3,
      new_TID = 2,
      lead = "agnew",
      done = 1,
      effort = 77L,
      status = "make dinner"
    ),
    NA
  )

  expect_equal(
    tasks(),
    dplyr::tibble(
      PID = c(1, 1, 1, 1, 2),
      project = c(rep("Understanding the Construction of the United States", 4), "Boiling the Ocean"),
      PI = new_projects_author(c(rep("303: Plato", 4), NA)),
      impact = c(rep(Inf, 4), pi),
      TID = c(1, 2, 3, 4, 1),
      done = c(0, 1, 0, 0, 0),
      task = c("put out the fire", "tie your shoes", "learn something", "take a break", "put the horse before the cart"),
      effort = c(NA, 77, pi, -100, NA),
      timing = c(NA, NA, Inf, NA, NaN),
      lead = new_projects_author(c("8888: Stone", "13: Agnew", "8888: Stone", "303: Plato", "13: Agnew")),
      status = c(NA, "make dinner", "foobar", "throw it all away", NA)
    )
  )

  finish("construction", 4)

  expect_equal(
    tasks("construction", c(13, "plato")),
    dplyr::tibble(
      PID = c(1, 1),
      project = rep("Understanding the Construction of the United States", 2),
      PI = new_projects_author(rep("303: Plato", 2)),
      impact = c(Inf, Inf),
      TID = c(2, 4),
      done = c(1, 1),
      task = c("tie your shoes", "take a break"),
      effort = c(77, -100),
      timing = rep(NA_real_, 2),
      lead = new_projects_author(c("13: Agnew", "303: Plato")),
      status = c("make dinner", "throw it all away")
    )
  )

  Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
})
