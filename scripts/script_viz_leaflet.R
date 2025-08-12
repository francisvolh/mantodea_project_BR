locs_sf <- sf::st_as_sf(main_data[,c("longitude", "latitude")], 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) ## I am not reprojecting because this is the tropics

leaflet::leaflet()|>
  leaflet::addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
    options = leaflet::tileOptions(variant='stamen_toner_lite', apikey = 'hidden') )|>
  leaflet::addMarkers(data = locs_sf)|>
  leaflet::addCircleMarkers( col ="red",
                             data = sf::st_as_sf(dplyr::filter(locs_sf, selected == 1), coords = c('lon', 'lat'), crs = 4326)
  )
