
###

# WIP (last edited 07/05/2021)

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)
library(googleway)
library(glue)
library(here)

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

place_names_ready <- places_names %>% 
  mutate(business_name = str_split(business_name,", ")) %>% 
  unnest(cols = c(business_name)) %>% 
  transmute(search_string = as.character(glue("{business_name}, {address}, Seattle, WA"))) %>% 
  pluck(1)

download_places_data <- function(dat){
  
  # browser()
  
  hit_google_places_api <- function(x){
    
    # browser()
    
    res <- google_places(x)
    
    print(res)
    
    return(res)
    
  }
  
  res <- map(
    dat,
    possibly(hit_google_places_api,
             list()
    )
  )
  
  return(res)
  
}


if(!file.exists(here("data/places-data-raw.rds"))){
  places_data_raw <- download_places_data(dat = place_names_ready)
  
  write_rds(places_data_raw, here("data/places-data-raw.rds"))
}


# CLEAN PLACES DATA -------------------------------------------------------


places_raw <- read_rds(here("data/places-data-raw.rds"))

remove_zero_results <- function(x){
  
  ids <- map(x,"status") %in% "OK"
  
  res <- x[ids]
  
  return(res)
}

places_ok <- remove_zero_results(places_raw) %>% 
  map_dfr("results") %>% 
  as_tibble()

places_open <- places_ok %>% 
  filter(business_status %in% c("OPERATIONAL", "CLOSED_TEMPORARILY"))

places_ready <- places_open %>% 
  transmute(name, 
            formatted_address,
            business_status,
            lat = pluck(geometry,"location","lat"),
            lng = pluck(geometry,"location","lng"),
            types,
            place_id
  ) %>% 
  st_as_sf(coords = c("lng","lat")) %>% 
  st_set_crs(4326)




