

library(geodata)
library(terra)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(tidyterra)
library(leaflet)
# webshot::install_phantomjs()
library(mapview)

#.........................................................
# Provincial shapefiles ----
#.........................................................

#download boundaries
canada <- gadm(country="Canada", level=1, path = tempdir()) #include provinces border

#subset to provincial boundaries
ab <- canada[canada$NAME_1 %in% "Alberta", ]
bc <- canada[canada$NAME_1 %in% "British Columbia", ]
sk <- canada[canada$NAME_1 %in% "Saskatchewan", ]
mb <- canada[canada$NAME_1 %in% "Manitoba", ]

#define the crs projection system
# nad83 <- "EPSG:4269"  
latlon <- "EPSG:4326"  

#reproject
ab <- project(ab, latlon)
bc <- project(bc, latlon)
sk <- project(sk, latlon)
mb <- project(mb, latlon)

# combine both into one
absk <- rbind(ab, sk)
prov <- rbind(absk, mb)

# convert into sf object
absk_sf <- st_as_sf(absk)
prov_sf <- st_as_sf(prov)

# plot provinces
plot(prov_sf)


#.........................................................
# National Park shapefiles ----
#.........................................................


# import downloaded shapefile
npab <- st_read("data/shapefiles/CLAB_AB_2023-09-08/CLAB_AB_2023-09-08.shp")
npsk <- st_read("data/shapefiles/CLAB_SK_2023-09-08/CLAB_SK_2023-09-08.shp")
npmb <- st_read("data/shapefiles/CLAB_MB_2023-09-08/CLAB_MB_2023-09-08.shp")

# convert crs
npab <- st_transform(npab, crs = latlon)
npsk <- st_transform(npsk, crs = latlon)
npmb <- st_transform(npmb, crs = latlon)

# extract national parks geometry
elk <- st_geometry(npab[npab$CLAB_ID == "ELKI", ])
banff <- st_geometry(npab[npab$CLAB_ID == "BANF", ])
wood <- st_geometry(npab[npab$CLAB_ID == "WOOD", ])
waterton <- st_geometry(npab[npab$CLAB_ID == "WATE", ])

prince <- st_geometry(npsk[npsk$CLAB_ID == "PALB", ])
grassland <- st_geometry(npsk[npsk$CLAB_ID == "GRAS", ])

riding <- st_geometry(npmb[npmb$CLAB_ID == "RIDM", ])

np7 <- c(elk, banff, wood, waterton, prince, grassland, riding)
np3 <- c(elk, banff, prince)

#.........................................................
# Plot study area ----
#.........................................................

# get colour friendly colours
display.brewer.all(colorblindFriendly = TRUE) # visalise as a plot
# extract palette
col_palette <- brewer.pal(n = 9, name = "Paired")
# visualise palette with hex codes as a plot
scales::show_col(col_palette) 


ggplot() +
  geom_sf(data = absk) +
  geom_sf(data = elk, fill = "#33a02c") +
  geom_sf(data = banff, fill = "#1f78b4") +
  geom_sf(data = prince, fill = "#e31a1c") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),)
  





#.........................................................
# Plot map study area ----
#.........................................................

# 
# 
# # plot using leaflet
# # m <-
# leaflet(absk) %>%
#   addTiles() %>%  # base map tiles
#   addPolylines(color = "black", weight = 2, fillOpacity = 0.3, label = ~NAME_1) %>%
#   addPolygons(data = elki, fillColor = "#33a02c", fillOpacity = 0.9, color = "#33a02c", weight = 1) %>%  
#   addPolygons(data = banff, fillColor = "#1f78b4", fillOpacity = 0.9, color = "#1f78b4", weight = 1) %>%  
#   addPolygons(data = prince, fillColor = "#e31a1c", fillOpacity = 0.9, color = "#e31a1c", weight = 1) %>% 
#   setView(lng = mean(st_bbox(absk_sf)[c("xmin", "xmax")]), 
#           lat = mean(st_bbox(absk_sf)[c("ymin", "ymax")]), 
#           zoom = 5)
# 
# 
# # save as a static plot
# mapshot(m, file = "figures/leaflet_map.png")


# plot using leaflet
m <-
  leaflet(absk_sf) %>%
  addTiles() %>%  # base map tiles
  # addPolylines(color = "black", weight = 2, fillOpacity = 0.3, label = ~NAME_1) %>%
  addPolygons(data = elk, fillColor = "purple", fillOpacity = 0.9, color = "purple", weight = 1) %>% 
  addPolygons(data = banff, fillColor = "#1f78b4", fillOpacity = 0.9, color = "#1f78b4", weight = 1) %>% 
  addPolygons(data = prince, fillColor = "#e31a1c", fillOpacity = 0.9, color = "#e31a1c", weight = 1) %>% 
    setView(lng = mean(st_bbox(absk_sf)[c("xmin", "xmax")]), 
          lat = mean(st_bbox(absk_sf)[c("ymin", "ymax")]), 
          zoom = 6)


# save as a static plot
mapshot(m, file = "images/map_np_sites.png")






# PLOT ALL NATIONAL PARKS ----

# plot using leaflet
m <-
  leaflet(prov_sf) %>%
  addTiles() %>%  # base map tiles
  # addPolylines(color = "black", weight = 2, fillOpacity = 0.3, label = ~NAME_1) %>%
  addPolygons(data = np7, fillColor = "brown", fillOpacity = 0.9, color = "brown", weight = 1) %>% 
  setView(lng = mean(st_bbox(prov_sf)[c("xmin", "xmax")]), 
          lat = mean(st_bbox(prov_sf)[c("ymin", "ymax")]), 
          zoom = 5)


# save as a static plot
mapshot(m, file = "images/leaflet_np_map.png")



