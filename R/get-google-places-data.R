
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

tmp <- place_names_ready[sample(1:length(place_names_ready),size = 5,replace = F)]

places_data_raw <- download_places_data(dat = place_names_ready)

write_rds(places_data_raw, here("data/places-data-raw.rds"))


