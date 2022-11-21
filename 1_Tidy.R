## Data processing for school warehouse app
## First created September 2022
## Created by Mike McCarthy, PhD, Radical Research LLC

library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)

#Warehouse locations and polygons are from Warehouse CITY v1.08 @https://radicalresearch.shinyapps.io/WarehouseCITY/)

#School locations and polygons are from the [California School Campus
                                            #Database](https://www.californiaschoolcampusdatabase.org) Version 2021.

wd <- getwd()
school_wd <- paste0(wd, '/CSCD_2021.gdb')
warehouse_wd <- paste0(wd, '/warehouse')
app_wd <- paste0(wd, '/WarehouseCITY_school/')



load(paste0(warehouse_wd, '/.RData'))

counties <- c('Riverside', 'San Bernardino')

inland_parcels <- final_parcels %>% 
  mutate(county = ifelse(county == 'San Bernadino', 'San Bernardino', county)) %>% 
  filter(county %in% counties)

rm(ls = final_parcels)

schools <- sf::st_read(dsn = school_wd, quiet = TRUE, type = 3) %>%
  filter(County %in% counties) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  select(School, District, City, Status, GradesServed, Shape)

URL.path <- 'https://raw.githubusercontent.com/RadicalResearchLLC/EDVcourse/main/CalEJ4/CalEJ.geoJSON'
SoCalEJ <- st_read(URL.path) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(County %in% counties)

setwd(app_wd)
save.image('.RData')
#getwd()
setwd(wd)
