install.packages('benchmarkme')
library(benchmarkme)
Sys.info()
update.packages( ask = FALSE)

install.packages('ggplot2movies')
data(movies, package = "ggplot2movies")
head(movies)
colnames(movies)
ratings = movies[,7:16]
popular = apply(ratings, 1, which.max)
popular

#Tidy Data Criteria
#1. Each Variable forms a column
#2. Each observation forms a row
# Each type of observatinal unit forms a table

head(movies)
movies %>% filter(budget != 'NA' & year > 2000) %>%
  select(title, year, length , rating)  %>% 
  arrange(-rating, year) %>% 
  filter(length > 100) %>%
  group_by(year) %>% 
  summarise(mean(length),mean(rating))

# Spatial Data Stuff
install.packages('ggmap')
library(ggmap)
library(data.table)
library(dplyr)
head(crime)
data = as.data.table(crime)
colnames(data)
data = data %>% filter(lon != "NA" & lat != "NA") %>% 
  mutate( offense = as.factor(offense), day = as.factor(day), month = as.factor(month))

library(sp)
coordinates(data) <- c("lon","lat")
proj4string(data) <- CRS("+proj=longlat +ellps=WGS84")
saveRDS(data, "/Users/luajiongwei/Documents/GitHub/R tutorials/crime_spatial_df.rds")
texas_shapefile <- readOGR('/Users/luajiongwei/Documents/GitHub/R tutorials/tl_2017_48_tract/tl_2017_48_tract.shp')
install.packages('rgdal')
library(rgdal)
?readOGR
plot(texas_shapefile, col="grey",axes = TRUE)
plot(data, pch = 21, bg = "red", cex = .5, add = TRUE)


map_dat <- get_map(location = c(-95.3698, 29.7604), zoom = 14, source = "google")
houstonMap <- ggmap(map_dat, extent = "device", legend = "topleft")
downtown_crime = as.data.table(data)
houstonMap2 <- houstonMap + geom_point(aes(x = lon, y = lat), data = downtown_crime, alpha = 0.5, color="darkred", size = 3)
houstonMap2


## Encoding Hawker Centre Locations:
library(data.table)
library(sp)
library(rgeos)
library(geosphere)
library(rgdal)

hawker_centres <- readOGR('/Users/luajiongwei/Downloads/hawker-centres/hawker-centres-kml.kml')
planning_area <- readOGR('/Users/luajiongwei/Downloads/master-plan-2014-planning-area-boundary-web/master-plan-2014-planning-area-boundary-web-shp/MP14_PLNG_AREA_WEB_PL.shp')
hc_coordinates <- as.data.table(hawker_centres@coords[,1:2])
coordinates(hc_coordinates) <- c('coords.x1', 'coords.x2')
proj4string(hc_coordinates) <- CRS("+proj=longlat +ellps=WGS84")
hc_coordinates <- spTransform(hc_coordinates, proj4string(planning_area))
hc_pln_area_mapping <- over(hc_coordinates, planning_area)
hc_pln_area_mapping <- as.data.table(hc_pln_area_mapping)
library(dplyr)
no_of_hc_by_pa <- hc_pln_area_mapping %>% group_by(PLN_AREA_N) %>% summarise(number_of_hc = n())
fwrite(no_of_hc_by_pa,'/Users/luajiongwei/Downloads/hc.csv' )