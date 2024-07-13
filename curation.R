{
  
  list.of.packages <- c(
    'here',
    'rio',
    'tidyverse',
    'janitor',
    'feather',
    'esquisse',
    'skimr',
    'timetk'
    )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  rm(list.of.packages, new.packages)
  
  # Packages----
  library(here)
  library(rio)
  library(tidyverse)
  library(janitor)
  library(feather)
  library(esquisse)
  library(skimr)
  library(timetk)

  # Custom func------

  `%ni%` <- Negate(`%in%`)

  ## for printing of colnames for selection
  prettylist <- function(x) {
    paste0("'", x, "',", "\n") %>% cat()
  }

  skim_count <- skim_with(numeric = sfl(n = length, median = ~ median(.x, na.rm = T)))
}

# Test for previous work --------------------------------------------------

fl <- list.files(pattern = "disc_raw_")

if (file.exists(fl) != TRUE) {
  # Downloads the FF db----
  temp <- tempfile()
  options(timeout = 300)
  download.file("https://www.fracfocusdata.org/digitaldownload/FracFocusCSV.zip", temp)

  if (dir.exists(here("temp"))) {
    message("Temp directory found!")
    unlink(here("temp"), recursive = TRUE)
    message("Created temp dir")
    dir.create(here("temp"))
  } else {
    message("Created temp dir")
    dir.create(here("temp"))
  }

  unzip(temp, exdir = here("temp"))

  # creates folders for the files----

  {
    ## Disclosure----
    if (dir.exists(here("disc"))) {
      unlink(here("disc"), recursive = TRUE)
      dir.create(here("disc"))
    } else {
      dir.create(here("disc"))
    }

    files_to_copy <- list.files(path = here("temp"), pattern = "DisclosureList_[[:digit:]].csv$", full.names = TRUE)
    file.copy(files_to_copy, here("disc"))
    file.remove(files_to_copy)

    ## Water-----
    if (dir.exists(here("water"))) {
      unlink(here("water"), recursive = TRUE)
      dir.create(here("water"))
    } else {
      dir.create(here("water"))
    }

    files_to_copy <- list.files(path = here("temp"), pattern = "WaterSource_[[:digit:]].csv$", full.names = TRUE)
    file.copy(files_to_copy, here("water"))
    file.remove(files_to_copy)

    ## readme----
    if (dir.exists(here("readme"))) {
      unlink(here("readme"), recursive = TRUE)
      dir.create(here("readme"))
    } else {
      dir.create(here("readme"))
    }

    files_to_copy <- list.files(path = here("temp"), pattern = "readme csv.txt$", full.names = TRUE)
    file.copy(files_to_copy, here("readme"))
    file.remove(files_to_copy)

    ## frac records----
    if (dir.exists(here("frac"))) {
      unlink(here("frac"), recursive = TRUE)
      dir.create(here("frac"))
    } else {
      dir.create(here("frac"))
    }

    files_to_copy <- list.files(path = here("temp"), pattern = "FracFocusRegistry_\\d+.csv", full.names = TRUE)
    file.copy(files_to_copy, here("frac"))
    file.remove(files_to_copy)

    rm(files_to_copy)
    rm(temp)
    unlink(here("temp"), recursive = T)
  }


  # Water -------------------------------------------------------------------

  file_list <- list.files(path = here("water"), pattern = "^WaterSource_\\d+.csv", full.names = TRUE)

  water_raw <- map(file_list,
    ~ readr::read_csv(.x,
      col_types = cols(
        APINumber = col_character(),
        ClaimantCompany = col_character()
      ),
      na = c("", "NA", " ")
    ),
    .id = "id",
    .progress = TRUE
  ) %>%
    list_rbind() %>%
    clean_names() %>%
    as_tibble()

  write_feather(water_raw,
    path = paste0(here(), "/water_raw", "_", Sys.Date())
  )

  # water_raw %>%
  #   group_by(state_name, description) %>%
  #   reframe(., n()) %>%
  #   print(n = Inf)

  # Frac records ------------------------------------------------------------

  # This should take a moment!

  file_list <- list.files(path = here("frac"), pattern = "^FracFocusRegistry_\\d+.csv", full.names = TRUE)

  frac_raw <- map(file_list,
    ~ readr::read_csv(.x,
      col_types = cols(
        APINumber = col_character(),
        ClaimantCompany = col_character()
      ),
      na = c("", "NA", " ")
    ),
    .id = "id",
    .progress = TRUE
  ) %>%
    list_rbind() %>%
    clean_names() %>%
    as_tibble()

  write_feather(frac_raw,
    path = paste0(here(), "/frac_raw", "_", Sys.Date())
  )

  # Disclosure --------------------------------------------------------------------

  file_list <- list.files(path = here("disc"), pattern = "^DisclosureList_\\d+.csv", full.names = TRUE)

  disc_raw <- map(file_list,
    ~ readr::read_csv(.x,
      col_types = cols(
        APINumber = col_character()
      ),
      na = c("", "NA", " ")
    ),
    .id = "id",
    .progress = TRUE
  ) %>%
    list_rbind() %>%
    clean_names() %>%
    as_tibble()

  write_feather(disc_raw,
    path = paste0(here(), "/disc_raw", "_", Sys.Date())
  )

  rm(file_list)
} else {
  ### ELSE --------------------------------------------------------------------

  disc_raw <- read_feather(list.files(pattern = "disc_raw"))
  frac_raw <- read_feather(list.files(pattern = "frac_raw"))
  water_raw <- read_feather(list.files(pattern = "water_raw"))
}

rm(fl)

# Cleaning ----------------------------------------------------------------

## Disclosure --------------------------------------------------------------

disc <- disc_raw %>%
  distinct(api_number, job_start_date, .keep_all = T) %>%
  mutate(
    across(c(job_start_date, job_end_date), mdy_hms),
    job_diff = difftime(job_end_date, job_start_date, units = "days")
  )


### Bad data ----------------------------------------------------------------

{
  disc_bad <- list()

  disc_bad$job_diff <- disc %>%
    filter(job_diff < 0) # removes bad dates

  disc_bad$date <- disc %>%
    filter(job_start_date > Sys.Date()) # removes future jobs

  disc_bad <- disc_bad %>%
    list_rbind(names_to = "type") %>%
    distinct()
}

# Good data!
disc <- disc %>%
  filter(disclosure_id %ni% disc_bad$disclosure_id)

rm(disc_bad, disc_raw)

### Plot --------------------------------------------------------------------

disc %>%
  mutate(year = year(job_start_date), .keep = "unused") %>%
  filter(., year >= 2010) %>% # Removes suspect data
  group_by(state_name, year) %>%
  reframe(., count = n()) %>%
  print(n = Inf) %>%
  ggplot(.) +
  aes(x = year, y = count, color = state_name) +
  geom_point() +
  # scale_y_log10() +
  geom_smooth(se = FALSE) +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_classic() +
  facet_wrap(vars(state_name))

## Frac --------------------------------------------------------------------

frac <- frac_raw %>%
  distinct(api_number, job_start_date, .keep_all = T) %>%
  mutate(
    across(c(job_start_date, job_end_date), mdy_hms),
    job_diff = difftime(job_end_date, job_start_date, units = "days"),
    year = year(job_start_date)
  ) %>%
  filter(disclosure_id %in% disc$disclosure_id)

tx <- frac %>%
  filter(., year >= 2010 & state_name == "Texas") %>%
  filter(!is.na(ingredient_common_name))


### EDA ---------------------------------------------------------------------

tx_eda <- tx %>%
  select(
    #  "disclosure_id",
    #  "job_start_date",
    #  "job_end_date",
    #  "api_number",
    #  "state_name",
    #  "county_name",
    #  "operator_name",
    #  "well_name",
    #  "latitude",
    #  "longitude",
    #  "projection",
    #  "tvd",
    #  "total_base_water_volume",
    # "total_base_non_water_volume",
    #  "ff_version",
    #  "federal_well",
    #  "indian_well",
    #  "purpose_id",
    #  "trade_name",
    #  "supplier",
    #  "purpose",
    #  "ingredients_id",
    #  "cas_number",
    #  "ingredient_name",
    "ingredient_common_name",
    #  "percent_high_additive",
    "percent_hf_job",
    #  "ingredient_comment",
    #  "ingredient_msds",
    #  "mass_ingredient",
    #  "claimant_company",
    #  "job_diff",
    #  "year"
  ) %>%
  group_by(ingredient_common_name) %>%
  skim_count() %>%
  arrange(desc(numeric.n))


### compound ----------------------------------------------------------------

#### ts ----------------------------------------------------------------------

tx %>%
  filter(ingredient_common_name == "Hydrotreated light petroleum distillate") %>%
  timetk::summarize_by_time(
    .date_var = job_start_date,
    .by = "month",
    n = n()
  ) %>%
  plot_time_series(.,
    .date_var = job_start_date,
    .value = n,
    .interactive = TRUE
  )


##### ts, a few compounds -----------------------------------------------------

#Single plots

tx_eda %>%
  select(ingredient_common_name) %>%
  as.list() %>%
  unlist() %>%
  unname() %>%
  .[1:4] %>%
  map(., ~ {
    cat(.x, '\n')
    title <- .x
    tx %>%
      filter(ingredient_common_name == .x) %>%
      timetk::summarize_by_time(
        .date_var = job_start_date,
        .by = "month",
        n = n()
      ) %>%
      plot_time_series(.,
        .title = title,
        .date_var = job_start_date,
        .value = n,
        .interactive = FALSE
      )
  })

#Facet plots
{
tx_compounds <- tx_eda %>%
  select(ingredient_common_name,numeric.n) %>%
  as_tibble() %>% 
  slice_head(n = 4) 

tx %>%
  filter(ingredient_common_name %in% tx_compounds$ingredient_common_name) %>%
  group_by(ingredient_common_name) %>%
  timetk::summarize_by_time(
    .date_var = job_start_date,
    .by = "month",
    n = n()
  ) %>%
  plot_time_series(.,
                   .date_var = job_start_date,
                   .value = n,
                   .facet_ncol = 2,
                   # .facet_scales = "free",
                   .facet_scales = "fixed",
                   .interactive = TRUE
  )
}


#Further exploration

tx_compounds <- tx_eda %>%
  select(ingredient_common_name,numeric.n) %>%
  as_tibble() %>% 
  slice_head(n = 30) %>% 
  slice_sample(n = 3)

tx %>%
  filter(ingredient_common_name %in% c('Water', tx_compounds$ingredient_common_name)) %>%
  group_by(ingredient_common_name) %>%
  timetk::summarize_by_time(
    .date_var = job_start_date,
    .by = "month",
    n = n()
  ) %>%
  plot_time_series(.,
    .date_var = job_start_date,
    .value = n,
    .facet_ncol = 2,
   # .facet_scales = "free",
    .facet_scales = "fixed",
    .interactive = TRUE
  )


#### Seasonal ----------------------------------------------------------------

tx %>%
  #filter(ingredient_common_name == "Hydrotreated light petroleum distillate") %>%
  filter(ingredient_common_name == "Water") %>%
  select(
    "job_start_date",
    "ingredient_common_name",
    "percent_hf_job"
  ) %>%
  timetk::summarize_by_time(
    .date_var = job_start_date,
    .by = "month",
    val = mean(percent_hf_job, na.rm = T)
  ) %>%
  plot_seasonal_diagnostics(.,
    .date_var = job_start_date,
    .value = val,
    .feature_set = c("month.lbl", "year"),
    .interactive = TRUE
  )


# Mapping -----------------------------------------------------------------

library(sf)
library(mapview)
#library(tigris)
#library(tidycensus)


tx_sf <- tx %>% 
  filter(ingredient_common_name %in% tx_compounds$ingredient_common_name) %>%
  select(ingredient_common_name, year, latitude, longitude) %>% 
  sf::st_as_sf(., coords = c('longitude', 'latitude'), crs = 'EPSG:4269')

mapview(tx_sf, zcol = c(
  'ingredient_common_name'
  #,'year'
  ), burst = FALSE, legend = TRUE)

  
  
  
  
  
  
  
  
  
  