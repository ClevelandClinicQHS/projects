'projects' R package demonstration
================

The user can install the `projects` package from the Comprehensive R Archive Network (CRAN):

``` r
install.packages("projects")
```

The user then loads the package:

``` r
library(projects)
```

    ## projects_folder() location:
    ## projects folder not found. Please run setup_projects()

As the startup message indicates, the user must run `setup_projects()` in order to initialize the */projects/* folder. The function accepts the path of the directory wherein the user would like for the */projects/* folder to dwell. Here, the user establishes the */projects/* folder in the home directory:

``` r
setup_projects("~")
```

    ## projects folder created at
    ## ~/projects
    ## 
    ## Add affiliations with new_affiliation(),
    ## then add authors with new_author(),
    ## then create projects with new_project()

This mock *~/projects/* folder has now been created in the user's home directory, and it is also available for viewing in the [demonstration](https://github.com/NikKrieger/projects/tree/master/demonstration) folder on the `projects` GitHub.

The message suggests adding affiliations and authors before creating projects because it takes marginally more user effort to change an existing project's authorship than to specify authorship upon a project's creation.

``` r
new_affiliation(
  department_name  = "Department of Physics",
  institution_name = "University of North Science",
  address = "314 Newton Blvd, Springfield CT 06003"
)
new_affiliation(
  department_name  = "Impossibles Investigation Team",
  institution_name = "Creekshirebrook Academy of Thinks",
  address = "Let Gade 27182, 1566 Copenhagen, Denmark"
)
new_affiliation(
  department_name  = "Statistical Consulting Unit",
  institution_name = "Creekshirebrook Academy of Thinks",
  address = "196 Normal Ave, Columbus, OH ",
  id = 50
)
new_author(
  given_names = "Scott",
  last_name = "Bug",
  title = "Professor",
  affiliations = c(2, "Physics"),
  degree = "PhD",
  email = "scottbug@imPOSSible.net",
  phone = "965-555-5556"
)
new_author(
  given_names = "Marie",
  last_name = "Curie",
  title = "Chemist",
  affiliations = "Unit",
  phone = "553-867-5309",
  id = 86
)
new_author(
  given_names = "George Washington",
  last_name = "Carver",
  title = "Botanist",
  degree = "MS",
  affiliations = c(1, 2, 50),
  id = 1337
)
new_author(
  given_names = "Leonardo",
  last_name = "da Vinci",
  title = "Mathematician"
)
new_author(
  last_name = "Wu",
  given_names = "Chien-Shiung",
  title = "Physicist",
  affiliations = c("of North", "Statistical Consulting"),
  degree = "PhD",
  email = "wu@WU.wU"
)
## Output suppressed in knitted file for brevity
```

``` r
affiliations()
```

    # A tibble: 3 x 4
         id department_name        institution_name        address             
      <int> <chr>                  <chr>                   <chr>               
    1     1 Department of Physics  University of North Sc… 314 Newton Blvd, Sp…
    2     2 Impossibles Investiga… Creekshirebrook Academ… Let Gade 27182, 156…
    3    50 Statistical Consultin… Creekshirebrook Academ… "196 Normal Ave, Co…

``` r
authors()
```

    # A tibble: 5 x 7
         id given_names     last_name title      degree email          phone   
      <int> <chr>           <chr>     <chr>      <chr>  <chr>          <chr>   
    1     1 Scott           Bug       Professor  PhD    scottbug@impo… 965-555…
    2     2 Leonardo        da Vinci  Mathemati… <NA>   <NA>           <NA>    
    3     3 Chien-Shiung    Wu        Physicist  PhD    wu@wu.wu       <NA>    
    4    86 Marie           Curie     Chemist    <NA>   <NA>           553-867…
    5  1337 George Washing… Carver    Botanist   MS     <NA>           <NA>    

We will now create a project called "Creating Cold Fusion":

``` r
new_project(
  title = "Achieving Cold Fusion",
  short_title = "ACF",
  authors = list("Bug", "Chien-Shiung", "Leonardo", 1337, 86),
  current_owner = "Carver",
  corresp_auth = "Bug",
  creator = "Curie",
  stage = "1: design",
  deadline_type = "Pilot study",
  deadline = "2020-12-31",
  use_bib = TRUE,
  id = 12
)
```


    Project 12 has been created at
    ~/projects/p0012

    # A tibble: 1 x 6
         id title        stage       status   deadline_type deadline           
      <int> <chr>        <S3: proje> <chr>    <chr>         <dttm>             
    1    12 Achieving C… 1: design   just cr… Pilot study   2020-12-31 00:00:00


    New project's authors:

    # A tibble: 5 x 7
      author_id given_names    last_name title     degree email        phone   
          <int> <chr>          <chr>     <chr>     <chr>  <chr>        <chr>   
    1         1 Scott          Bug       Professor PhD    scottbug@im… 965-555…
    2         3 Chien-Shiung   Wu        Physicist PhD    wu@wu.wu     <NA>    
    3         2 Leonardo       da Vinci  Mathemat… <NA>   <NA>         <NA>    
    4      1337 George Washin… Carver    Botanist  MS     <NA>         <NA>    
    5        86 Marie          Curie     Chemist   <NA>   <NA>         553-867…
    # A tibble: 1 x 3
      current_owner         corresp_auth          creator              
      <S3: projects_author> <S3: projects_author> <S3: projects_author>
    1 1337: Carver          1: Bug                86: Curie            

This creates the project folder at *~/projects/p0012/*. The */projects/* folder would be structured as follows:

    ~/projects
    ├── .metadata
    │   ├── affiliations.rds
    │   ├── author_affiliation_assoc.rds
    │   ├── authors.rds
    │   ├── project_author_assoc.rds
    │   └── projects.rds
    ├── .templates
    │   ├── 01_protocol.Rmd
    │   ├── 02_datawork.Rmd
    │   ├── 03_analysis.Rmd
    │   ├── 04_report.Rmd
    │   ├── CONSORT_protocol.Rmd
    │   ├── STROBE_protocol.Rmd
    │   ├── pXXXX.Rproj
    │   ├── style.css
    │   └── styles.docx
    └── p0012
        ├── data
        ├── data_raw
        ├── figures
        ├── manuscript
        ├── p0012.Rproj
        └── progs
            ├── 01_protocol.Rmd
            ├── 02_datawork.Rmd
            ├── 03_analysis.Rmd
            ├── 04_report.Rmd
            ├── p0012.bib
            ├── style.css
            └── styles.docx

The user may now begin work on this project. The [demonstration](https://github.com/NikKrieger/projects/tree/master/demonstration) folder on the `projects` GitHub contains this project folder, with a mock project completed in order to demonstrate workflow from 01\_protocol.Rmd to 04\_report.Rmd.
