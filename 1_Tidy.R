## Data processing for school warehouse app
## First created September 2022
## Created by Mike McCarthy, PhD, Radical Research LLC
## last modified April 2024 for Horseman article

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


WH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/comboFinal.geojson'
warehouses <- st_read(WH.url) |> 
  st_transform(crs = 4326) |> 
  select(shape_area, apn, class, year_built, county, geometry, category) |> 
  unique() 

counties <- c('Riverside', 'San Bernardino', 'Los Angeles', 'Orange')

schools <- sf::st_read(dsn = school_wd, quiet = TRUE, type = 3) %>%
 # clean_names() |> 
  filter(County %in% counties) |> 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  filter(CDSCode != 33670330131607) |> 
  filter(CDSCode != 33672310114066) |> 
  filter(Status == 'Active') |> 
  select(School, District, City, GradesServed, Level, Shape) |> 
  st_make_valid()

URL.path <- 'https://raw.githubusercontent.com/RadicalResearchLLC/EDVcourse/main/CalEJ4/CalEJ.geoJSON'
SoCalEJ <- st_read(URL.path) |> 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") |> 
  filter(County %in% counties)

crs <- st_crs(4326)
sf_use_s2(FALSE)


buffSchool1000 <- schools |> 
  st_transform(crs = 7801) |> 
 #default units are meters, 304 m = 1000 ft
  st_buffer(dist = 304) |> 
  st_transform(crs = 4326) #|> 


big100kWH <- warehouses |> 
  filter(shape_area > 200000)

sf_use_s2(FALSE)

schools1000WH <- buffSchool1000 |>
  st_filter(big100kWH) |> 
  st_set_geometry(value = NULL) |> 
  left_join(schools) |> 
  st_as_sf()

buff1000 <- schools1000WH |> 
  st_transform(crs = 7801) |> 
  st_buffer(dist = 304) |> 
  st_transform(crs = 4326)

buff300 <- schools1000WH |> 
  st_transform(crs = 7801) |> 
  st_buffer(dist = 91.2) |> 
  st_transform(crs = 4326)

schools300WH <- buff300 |> 
  st_filter(big100kWH)

palWH <- colorFactor(domain = warehouses$category,
                     palette = c('red', 'black'))

leaflet() |> 
  addTiles() |> 
  addPolygons(data = schools1000WH,
              color = 'purple',
              weight = 1) |> 
  addPolygons(data = buff1000,
              color = 'grey80',
              fillOpacity = 0.2,
              weight = 1) |> 
  addPolygons(data = buff300, 
              color = 'grey40',
              weight = 1) |> 
  addPolygons(data = warehouses,
              color = ~palWH(category),
              fillOpacity = 0.6,
              weight = 0.3)
  

schoolsNearWH <- buffSchool1000 |> 
  st_join(big100kWH, left = FALSE) |> 
  st_set_geometry(value = NULL) |> 
  mutate(shape_area2 = ifelse(category == 'Existing', shape_area,
                              ifelse(shape_area > 500000, 500000, shape_area))) |> 
  group_by(School,District,City, Level, GradesServed) |> 
    summarize(count = n(), footprint = sum(shape_area2), .groups = 'drop') %>%
  arrange(desc(footprint)) |>
  rename(Grades = GradesServed)

SchoolsNearWH2 <- schoolsNearWH |> 
left_join(schools) |>  
  st_as_sf() |> 
  distinct()

SchoolsNearWH1000 <- schoolsNearWH |> 
  left_join(buffSchool1000) |>  
  st_as_sf() |> 
  distinct()


setwd(app_wd)
save.image('.RData')
#getwd()
setwd(wd)

  