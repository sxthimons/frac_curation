#Packages----
{
  library(here) 
  library(rio) 
  library(tidyverse)
  library(janitor)
  library(feather)
  library(esquisse)
}

#Custom func------

`%ni%` <- Negate(`%in%`)

#Downloads the FF db----
temp <- tempfile()
options(timeout = 300)
download.file('https://www.fracfocusdata.org/digitaldownload/FracFocusCSV.zip', temp)

if(dir.exists(here('temp'))){
  message('Temp directory found!')
  unlink(here('temp'), recursive = TRUE)
  message('Created temp dir')
  dir.create(here('temp'))
}else{
  message('Created temp dir')
  dir.create(here('temp'))
}

unzip(temp, exdir = here('temp'))

# creates folders for the files----

{
  
  ##Disclosure----
  if(dir.exists(here('disc'))){
    unlink(here('disc'), recursive = TRUE)
    dir.create(here('disc'))
  }else{
    dir.create(here('disc'))
  }
  
  files_to_copy <- list.files(path = here('temp'), pattern = 'DisclosureList_[[:digit:]].csv$', full.names = TRUE)
  file.copy(files_to_copy, here('disc'))
  file.remove(files_to_copy)
  
  ##Water-----
  if(dir.exists(here('water'))){
    unlink(here('water'), recursive = TRUE)
    dir.create(here('water'))
  }else{
    dir.create(here('water'))
  }
  
  files_to_copy <- list.files(path = here('temp'), pattern = 'WaterSource_[[:digit:]].csv$', full.names = TRUE)
  file.copy(files_to_copy, here('water'))
  file.remove(files_to_copy)
  
  ##readme----
  if(dir.exists(here('readme'))){
    unlink(here('readme'), recursive = TRUE)
    dir.create(here('readme'))
  }else{
    dir.create(here('readme'))
  }
  
  files_to_copy <- list.files(path = here('temp'), pattern = 'readme csv.txt$', full.names = TRUE)
  file.copy(files_to_copy, here('readme'))
  file.remove(files_to_copy)
  
  ##frac records----
  if(dir.exists(here('frac'))){
    unlink(here('frac'), recursive = TRUE)
    dir.create(here('frac'))
  }else{
    dir.create(here('frac'))
  }
  
  files_to_copy <- list.files(path = here('temp'), pattern = 'FracFocusRegistry_\\d+.csv', full.names = TRUE)
  file.copy(files_to_copy, here('frac'))
  file.remove(files_to_copy)
  
  rm(files_to_copy)
  rm(temp)
  unlink(here('temp'), recursive = T)
}


# Water -------------------------------------------------------------------

file_list <- list.files(path = here('water'), pattern = '^WaterSource_\\d+.csv', full.names = TRUE)

water_raw <- map(file_list, 
                ~readr::read_csv(.x,
                                 col_types = cols(
                                   APINumber = col_character(),
                                   ClaimantCompany = col_character()
                                 ),
                                 na = c("", "NA", " ")),
                .id = "id",
                .progress = TRUE
) %>%
  list_rbind() %>%
  clean_names() %>% 
  as_tibble()

write_feather(water_raw,
              path = paste0(here(), '/water_raw','_', Sys.Date())
)

# water_raw %>%
#   group_by(state_name, description) %>%
#   reframe(., n()) %>% 
#   print(n = Inf)

# Frac records ------------------------------------------------------------

file_list <- list.files(path = here('frac'), pattern = '^FracFocusRegistry_\\d+.csv', full.names = TRUE)

frac_raw <- map(file_list, 
                ~readr::read_csv(.x,
                                 col_types = cols(
                                   APINumber = col_character(),
                                   ClaimantCompany = col_character()
                                 ),
                                 na = c("", "NA", " ")),
                .id = "id",
                .progress = TRUE
) %>%
  list_rbind() %>%
  clean_names() %>% 
  as_tibble()

write_feather(frac_raw,
              path = paste0(here(), '/frac_raw','_', Sys.Date())
)

# Disclosure --------------------------------------------------------------------

file_list <- list.files(path = here('disc'), pattern = '^DisclosureList_\\d+.csv', full.names = TRUE)

disc_raw <- map(file_list, 
                ~readr::read_csv(.x,
                                 col_types = cols(
                                   APINumber = col_character()
                                 ),
                                 na = c("", "NA", " ")),
                .id = "id",
                .progress = TRUE
) %>%
  list_rbind() %>%
  clean_names() %>% 
  as_tibble()

write_feather(disc_raw,
              path = paste0(here(), '/disc_raw','_', Sys.Date())
)

rm(file_list)

# Cleaning ----------------------------------------------------------------

## Disclosure --------------------------------------------------------------

disc <- disc_raw %>% 
  distinct(api_number, job_start_date, .keep_all = T) %>% 
  mutate(
    across(c(job_start_date,job_end_date), mdy_hms),
    job_diff = difftime(job_end_date,job_start_date, units = 'days'),
    jd = as.numeric(job_diff),
    jd_fix = case_when(
      jd >= 0 ~ TRUE,
      jd < 0 ~ FALSE)
  )


### Bad data ----------------------------------------------------------------

{
  disc_bad <- list()
  
  disc_bad$job_diff <- disc %>%
    filter(job_diff < 0) #removes bad dates
  
  disc_bad$date <- disc %>%
    filter(job_start_date > Sys.Date()) #removes future jobs
  
  disc_bad <- disc_bad %>%
    list_rbind(names_to = 'type') %>% 
    distinct()
}

disc <- disc %>% 
  filter(disclosure_id %ni% disc_bad$disclosure_id)

disc %>% 
  mutate(year = year(job_start_date), .keep = 'unused') %>% 
  filter(., year > 2010) %>% 
  group_by(state_name, year) %>%
  reframe(., count = n()) %>% 
  print(n = Inf) %>% 
  ggplot(.) +
  aes(x = year, y = count, color = state_name) +
  geom_point() +
  #scale_y_log10() +
  geom_smooth(se = FALSE) +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_classic() +
  facet_wrap(vars(state_name))
  


