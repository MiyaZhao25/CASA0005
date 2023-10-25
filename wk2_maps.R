#Load Packages (ignore any error messages about being built under a 
#different R version):
#library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
#library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(terra)

#download geojson 
# this will take a few minutes
# geojson in local folder
#EW <- st_read(here::here("week 2",
 #                        "Local_Authority_Districts__December_2015__Boundaries.geojson"))

# shapefile in local folder
EW <- st_read(here::here("week 2",
                         "Local_Authority_Districts_(December_2015)_Boundaries",
                         "Local_Authority_Districts_(December_2015)_Boundaries.shp"))

LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

qtm(LondonMap)


LondonData <- clean_names(LondonDataOSK)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)


BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))


library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")


tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))


Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "new_code"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

#map again 
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))
