install.packages("ggmap")
library(tidyverse)
library(ggmap)

# this is registering my API key
register_stadiamaps('b87b6ac8-7e0b-4ccd-a579-f3c525aa1a15', FALSE)

# this creates a map of the us defining the latitude and longitude borders
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stadiamap(us, zoom = 5) %>% ggmap()

# this creates a map of the uk as shown with terrain
uk <- c(left = -10, bottom = 49, right = 2, top = 59)
get_stadiamap(uk, zoom = 6, maptype = "stamen_terrain") %>% ggmap()

# this creates a map of sheffield city centre
sheffield <- c(left = -1.49, bottom = 53.37, right = -1.45, top = 53.39)
get_stadiamap(sheffield, zoom = 15, maptype = "stamen_toner_lite") %>% ggmap()

# this reads the dataset downloaded
sheffieldCameras <- read_csv("Sheffield_CCTV_locations.csv")

# this creates a map of Sheffield centre with plotted CCTV locations in red
get_stadiamap(sheffield, zoom = 15, maptype = "stamen_toner_lite") %>% ggmap() +
  geom_point(data = sheffieldCameras, aes(x = lon, y = lat), colour = "red") +
  labs(title = "Position of CCTV cameras in Sheffield city centre in 2017",
       caption = "Data: Sheffield city council, 2017")

# this creates a map of cathedral quarter with plotted CCTV in blue, using coordinates gained from Open street maps
cathedral <- c(left = -1.47772, bottom = 53.38060, right = -1.46245, top = 53.38660)
get_stadiamap(cathedral, zoom = 18, maptype = "stamen_toner_lite") %>% ggmap() +
  geom_point(data = sheffieldCameras, aes(x = lon, y = lat), colour = "blue") +
  labs(title = "Position of CCTV cameras in the Cathedral Quarter in 2017",
       caption = "Data: Sheffield city council, 2017")

# this installs the OSM package
install.packages("osmdata")

# this runs the package and then is a query looking for Sheffield city centre and filtering to pubs & restaurants, and downloads the data
library(osmdata)
SCC_pubs_restaurants <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "amenity",
                   value = c("restaurant", "pub")) %>%
  osmdata_sf()

View(SCC_pubs_restaurants$osm_points)

# this installs the sf package
install.packages("sf")

base_map <- get_stadiamap(getbb("Sheffield City Centre"), source = "stadia")

# this creates a map of sheffield centre with pubs and restaurants plotted in blue
ggmap(base_map) +
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = "blue",
          fill = "blue",
          alpha = .5,
          size = 1,
          shape = 21) +
  labs(x = "", y = "")

# this downloads highways
SCC_highways <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

# this downloads streets
SCC_streets <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

# this downloads small streets
SCC_small_streets <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

# this downloads rivers
SCC_river <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "waterway",
                  value = "river") %>%
  osmdata_sf()

# this downloads railways
SCC_railway <- getbb("Sheffield city centre") %>%
  opq() %>%
  add_osm_feature(key = "railway",
                  value = "rail") %>%
  osmdata_sf()

View(SCC_railway$osm_lines)

# this creates a map plotted with all the data we just downloaded
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          colour = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .2,
          linetype = "dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE)

# this adds plots of pubs and restaurants onto the previous map
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          colour = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .2,
          linetype = "dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .5,
          alpha = .6) +
geom_sf(data = SCC_pubs_restaurants$osm_points,
        inherit.aes = FALSE,
        colour = "blue",
        fill = "blue",
        size = 1,
        alpha = .5,
        shape = 21) +
  theme_void() +
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE) +
  theme() + labs(title = "Restaurants and pubs in Sheffield city centre")

# this adds plots of CCTV with pubs and restaurants onto the previous map
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          colour = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .2,
          linetype = "dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = "blue",
          fill = "blue",
          size = 1,
          alpha = .5,
          shape = 21) +
  theme_void() +
  geom_point(data = sheffieldCameras,
             colour = "red",
             aes(x = lon, y = lat)) +
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE) +
  theme(legend.position = "none") + labs(title = "CCTV coverage, restaurants and pubs in Sheffield city centre")

# this pulls the crime data I downloaded
sheffieldcrime <- read_csv("2022-11-south-yorkshire-street.csv")

View(sheffieldcrime)

# this adds to our last plot by adding crime location data
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          colour = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .2,
          linetype = "dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          colour = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          colour = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = "blue",
          fill = "blue",
          size = 1,
          alpha = .5,
          shape = 21) +
  theme_void() +
  geom_point(data = sheffieldCameras,
             colour = "green",
             aes(x = lon, y = lat)) +
  geom_point(data = sheffieldcrime,
             colour = "red",
             aes(x = Longitude, y = Latitude)) +
  coord_sf(xlim = c(-1.49, -1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE) +
  theme(legend.position = "none") + labs(title = "Crime, CCTV coverage, restaurants and pubs in Sheffield city centre")

# this creates a light gray world map
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", colour = "white") +
  theme(panel.background = element_blank()) +
  labs(title = "World map", caption = "maps package, R")

# this creates a value of eu countries
eu.countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany","Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# this creates a data frame for an eu map
eu.map <- map_data("world", region = eu.countries)

View(eu.map)

# this creates a map of the eu
ggplot(eu.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey", colour = "black") +
  labs(title = "EU map", caption = "maps package, R") +
  coord_map()

# this loads the WDI dataset
library(WDI)

# this creates a new cache
new_wdi_cache <- WDIcache()

# this downloads data in relation to our chosen indicators
countryDataWDI <- WDI(indicator = c("NY.GDP.PCAP.KD",
                                    "NY.GDP.PCAP.KD.ZG",
                                    "SP.POP.TOTL",
                                    "SP.DYN.LE00.IN"),
                      start = 2019,
                      end = 2019,
                      extra = TRUE,
                      cache = new_wdi_cache)

View(countryDataWDI)

# this mutates the values to be more uniform
countryDataWDI <- countryDataWDI %>%
  mutate(country = recode(str_trim(country), "United States" = "USA",
                          "United Kingdom" = "UK"))

# left joining countryDataWDI with world_map
countryDataWDIMap <- left_join(world_map, countryDataWDI, by = c("region" = "country"))

# this creates a world map that shows the GDP per capita growth of each country in 2019
ggplot(countryDataWDIMap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = NY.GDP.PCAP.KD), colour = "white") +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "GDP per \ncapita growth",
       title = "World map coloured by GDP per capita grown in 2019",
       caption = "Data source: World Development Indicators")

                                    

