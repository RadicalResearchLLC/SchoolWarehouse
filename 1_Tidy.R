## Data processing for school warehouse app
## First created September 2022
## Created by Mike McCarthy, PhD, Radical Research LLC

library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(janitor)

#Warehouse locations and polygons are from Warehouse CITY v1.08 @https://radicalresearch.shinyapps.io/WarehouseCITY/)

#School locations and polygons are from the [California School Campus
                                            #Database](https://www.californiaschoolcampusdatabase.org) Version 2021.

wd <- getwd()
school_wd <- paste0(wd, '/CSCD_2021.gdb')
app_wd <- paste0(wd, '/WarehouseCITY_school/')


WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/warehouse.geoJSON'
warehouses <- st_read(WH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  select(shape_area, county, geometry) %>% 
  unique() 

counties <- c('Riverside', 'San Bernardino', 'Los Angeles', 'Orange')

schools <- sf::st_read(dsn = school_wd, quiet = TRUE, type = 3) %>%
 # clean_names() %>% 
  filter(County %in% counties) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  filter(CDSCode != 33670330131607) %>% 
  filter(CDSCode != 33672310114066) %>% 
  filter(Status == 'Active') %>% 
  select(School, District, City, GradesServed, Level, Shape) %>% 
  st_make_valid()

URL.path <- 'https://raw.githubusercontent.com/RadicalResearchLLC/EDVcourse/main/CalEJ4/CalEJ.geoJSON'
SoCalEJ <- st_read(URL.path) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(County %in% counties)

crs <- st_crs(4326)
sf_use_s2(FALSE)


buffSchool1000 <- schools %>% 
  st_transform(crs = 7801) %>% 
 #default units are meters, 304 m = 1000 ft
  st_buffer(dist = 304) %>% 
  st_transform(crs = 4326) #%>% 


setwd(app_wd)
save.image('.RData')
#getwd()
setwd(wd)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = buffSchool1000,
              color = 'gray',
              fillOpacity = 0.3,
              stroke = FALSE) %>% 
  addPolygons(data = schools,
              color = 'purple',
              fillOpacity = 0.5,
              weight=2)

big100kWH <- warehouses %>% 
  filter(shape_area > 150000)

schoolsNearWH <- buffSchool1000 %>% 
  st_join(big100kWH, left = FALSE) %>% 
  st_set_geometry(value = NULL) %>% 
  group_by(School,District,City, Level, GradesServed) %>% 
    summarize(count = n(), footprint = sum(shape_area), .groups = 'drop') %>%
  arrange(desc(footprint)) #%>% 

SchoolsNearWH2 <- schoolsNearWH %>% 
left_join(schools) %>%  
  st_as_sf() %>% 
  distinct()

SchoolsNearWH1000 <- schoolsNearWH %>% 
  left_join(buffSchool1000) %>%  
  st_as_sf() %>% 
  distinct()
  


setwd(app_wd)
save.image('.RData')
#getwd()
setwd(wd)

  