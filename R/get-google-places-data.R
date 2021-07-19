
###

# WIP (last edited 07/05/2021)

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(googledrive)
library(googlesheets4)
library(janitor)
library(googleway)
library(glue)
library(here)
library(mapview)
library(tigris)

# lazy stuff
g <- dplyr::glimpse


# Note: your personal google maps api key must be set for this script to work.
googleway::set_key(Sys.getenv("GOOGLEWAY_GOOGLE_MAPS_API"))  

# GET DATA ----------------------------------------------------------------


drive_id <- as_id("1W7hKReDRg9gMZP8gnnZ2c01nN2odYxEGhKvkTihgdFI")

places_names <- drive_get(drive_id) %>% 
  read_sheet(sheet = 1, skip = 5) %>% 
  clean_names("snake")

permits <- drive_get(drive_id) %>% 
  read_sheet(sheet = 2) %>% 
  clean_names("snake") 

# HIT GOOGLE PLACES API ---------------------------------------------------

place_names_all <- places_names %>% 
  mutate(business_name = str_split(business_name,", ")) %>% 
  unnest(cols = c(business_name)) %>% 
  mutate(search_string = as.character(glue("{business_name}, {address}, Seattle, WA"))) 

place_names_unique <- place_names_all %>% 
  group_by(business_name, address) %>% 
  slice(1) %>% 
  ungroup()

download_places_data <- function(dat){
  
  # browser()
  
  hit_google_places_api <- function(x){
    
    # browser()
    
    search_string <- x[1,"search_string"][[1]]
    
    res <- google_places(search_string)
    
    names(res$results) <- str_c("google_",names(res$results))
    
    names(x) <- str_c("permit_", names(x))
    
    res$results <- cbind(res$results, x)
    
    print(res)
    
    return(res)
    
  }
  
  list_of_rows <- pmap(dat,tibble)
  
  res <- map(
    list_of_rows,
    possibly(hit_google_places_api,
             list()
    )
  )
  
  return(res)
  
}


if(!file.exists(here("data/places-data-raw.rds"))){
  places_data_raw <- download_places_data(dat = place_names_unique)
  
  write_rds(places_data_raw, here("data/places-data-raw.rds"),)
}



# CLEAN PLACES DATA -------------------------------------------------------


places_raw <- read_rds(here("data/places-data-raw.rds"))


remove_zero_results <- function(x){
  
  ids <- map(x,"status") %in% "OK"
  
  res <- x[ids]
  
  return(res)
}


places_ok <- places_raw %>% 
  remove_zero_results() %>% 
  map_dfr("results") %>% 
  as_tibble()

places_open <- places_ok %>% 
  filter(google_business_status %in% c("OPERATIONAL", "CLOSED_TEMPORARILY"))

# de-duplicate by selecting the closest address match using string matching

p_nest <- places_open %>% 
  mutate(grp = permit_search_string) %>% 
  mutate(g_address_short = str_extract(google_formatted_address,
                                       ".*(?=, Seattle)")) %>% 
  group_by(grp) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(n = map_int(data, ~ nrow(.x)))


filter_fuzzy_address <- function(x){
  # browser()
  
  if(nrow(x) == 1L){return(x)}
  
  res <- x %>%  
    rowwise() %>%  
    mutate(dist = stringdist(permit_address,
                             g_address_short,
                             method = "jw")) %>%  
    ungroup() %>% 
    arrange(desc(dist))
  
  res <- res %>% 
    slice_min(dist) %>% 
    select(-dist)
  
  return(res)
}

places_deduplicated <- p_nest %>%  
  mutate(data = map(data, filter_fuzzy_address)) %>% 
  unnest(cols = c(data))

places_with_permit_info <- places_deduplicated %>% 
  left_join(permits, by = "permit_address" )

places_sf <- places_with_permit_info %>% 
  transmute(name = google_name, 
            formatted_address = google_formatted_address,
            permit_address,
            business_status = google_business_status,
            permit_number,
            permit_type_alias,
            permit_project_name = project_name,
            permit_company = contact_company,
            lat = pluck(google_geometry,"location","lat"),
            lng = pluck(google_geometry,"location","lng"),
            types = map_chr(google_types, ~str_c(.x, collapse = "; ")),
            place_id = google_place_id
  ) %>% 
  st_as_sf(coords = c("lng","lat")) %>% 
  st_set_crs(4326)


# WRITE CLEANED DATA ------------------------------------------------------

if(!file.exists(here("data/places-data-cleaned.gpkg"))){
  st_write(places_sf,
           here("data/places-data-cleaned.gpkg"), 
           append = FALSE)
}

# mapview(places_sf)



# HIT GOOGLE PLACES API AGAIN ---------------------------------------------




