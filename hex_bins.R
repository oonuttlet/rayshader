# baltimore_scooters #
# harrison deford -- @oonuttlet -- hdeford1@umbc.edu #
# this script gathers micromobility data in the gbfs format,
# then bins it to hexes.

library(sf)
library(mapbaltimore)
library(tidyverse)
library(httr)
library(tmap)

balt_b <- mapbaltimore::baltimore_city_detailed %>%#load in baltimore shape from mapbaltimore
  st_transform(32618) %>%
  st_make_valid()
  
baltimore_urls <- list('https://gbfs.spin.pm/api/gbfs/v2/baltimore/free_bike_status',
                       'https://mds.bird.co/gbfs/v2/public/baltimore/free_bike_status.json',
                       'https://mds.linkyour.city/gbfs/2.2/us_md_baltimore/free_bike_status.json')

fbs_raw <- lapply(baltimore_urls, function(x) content(GET(x)))
fbs_bikes <- lapply(fbs_raw, function(x){
  bikes <- as.data.frame(do.call(rbind, x$data$bikes)) %>%
    select('bike_id','lat','lon')
})
fbs_bikes_bind <- as.data.frame(do.call(rbind,fbs_bikes))
fbs_gdf <- st_as_sf(fbs_bikes_bind, coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(32618)
plot(st_geometry(fbs_gdf)) #check for distribution

hex <- st_as_sf(st_make_grid(x = balt_b, cellsize = 200, what = 'polygons', square = FALSE))
hex.intersects <- st_intersects(st_union(balt_b), hex)
hex.subset <- hex[hex.intersects[[1]],]



