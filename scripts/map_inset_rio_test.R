system.time({
  
  
  
  geo_pol0 <- terra::vect("data/gadm_brazil/gadm41_BRA_1.shp")
  
  geo_pol0_crop_alt <- terra::crop(geo_pol0, c(-45.0656538667, -40.0297000036, -23.3413619722, -20.575620752))
  
  ## Load full dataset
  
  # old file, will change eventually
  main_raw <- read.csv("data/Data de ocurrencia_ad_n_ninf.csv")
  
  
  
  
  main_data <- main_raw|>
    dplyr::mutate(
      #longitude = as.character(longitude),
      #latitude = as.character(latitude),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      time_date = as.POSIXct(time_observed_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      month = lubridate::month(time_date)
    )
  
  
  
  vect_1 <- terra::vect(main_data[,c("longitude", "latitude")], 
                        geom = c("longitude", "latitude"),
                        crs= "EPSG:4326")
  
  rast.crs <-"EPSG:4326"
  #prep elevation
  elevation <-terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/rj.tif")   
  terra::crs(elevation) <- "EPSG:4618"
  
    
  #terra::project(terra::vect(NA_CEC_Eco_Level2_short),  "EPSG:4326" )
  
  
  xy_sfrep<-vect_1
  
  
  
  elevationCrop <-  terra::crop(elevation, terra::project(geo_pol0_crop_alt, elevation ))
  
  elevationCrop2 <-  terra::project(elevationCrop, "EPSG:4326" )  
  
  
  mdt2 <-  elevationCrop2
  #terra::plot(mdt2)
  #mdtdf <- as.data.frame(mdt, xy = TRUE)
  #head(mdtdf)
  #names(mdtdf)[3] <- "alt"
  
  mdt2<- terra::aggregate(mdt2, 6)
  sl2 <- terra::terrain(mdt2, "slope", unit = "radians")
  #terra::plot(sl)
  asp2 <- terra::terrain(mdt2, "aspect", unit = "radians")
  #terra::plot(asp)
  
  #need to write this raster, if not I will not produce layers, due to the size of the file it seems
  h2<-terra::shade(sl2, asp2, angle = c(45, 45, 45, 80), direction = c(225, 270, 315, 135) ,  
                   filename = 'plots/alt2_shade2.tif', overwrite = TRUE, wopt = list(memmax = 0.6, verbose = TRUE))
  
  
  hill_single2 <- Reduce(mean, h2)
  
  
  
  #hill_single<- terra::mask(hill_single, bcr) ###masking hillshade to keep the geom of maps white around
  
  
  hilldf_single2 <- as.data.frame(hill_single2, xy = TRUE)
  names(hilldf_single2)<-c("x", "y", "hillshade")
  gc()
  
  #NA_CEC_Eco_Level2_shortRP<- terra::project(terra::vect(NA_CEC_Eco_Level2_short), rast.crs)
  
  
  
  #ecoregions reprojected and cropped to rast.crs
  NA_CEC_Eco_Level2crop <-  geo_pol0_crop_alt
  
  #create mask for ocean elevation
  maskedTest <- terra::mask(  elevationCrop2 , NA_CEC_Eco_Level2crop  )
  #terra::plot(maskedPATest)
  #terra::plot(maskedPATest)
  er <- terra::rast(terra::ext(maskedTest), resolution=terra::res(maskedTest), crs = rast.crs)
  terra::values(er) <- 1
  #terra::plot(er)
  
  xx <- terra::ifel( maskedTest > 0, NA, er ) # use this to cover the raster
  
  #terra::plot(xx)
  gc()
  xxx <- terra::as.polygons(xx) #make a polygon out of the NAing raster for PAs
  
  #terra::plot(xxx, col = "red")
  
  
  ###
###################################  definir en base a data de rio de janerio
  ####
  
 terra::ext(geo_pol0_crop_alt)
  
  rect <- data.frame(
    x =c(-45.0656538667 , -40.363923402 , -40.363923402, -45.0656538667)
    ,
    y =  c(-23.3413619722, -23.3413619722, -20.575620752  ,  -20.575620752  ))
  gc()
})




{
  plottopo <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = hilldf_single2,
                       ggplot2::aes(x, y, fill = hillshade),
                       show.legend = FALSE) +  # plot hillshade
    ggplot2::scale_fill_distiller(palette = "Greys")+
    
    ggnewscale::new_scale_fill()+
    
    tidyterra::geom_spatraster(data = terra::project(mdt2,  rast.crs ))+  # plot elevation with an alpha
    tidyterra::scale_fill_hypso_tint_c(alpha =0.4,
                                       na.value = "transparent", name="Elevation (m)")+
    tidyterra::geom_spatvector(data = NA_CEC_Eco_Level2crop, linewidth = 1.25,alpha = 0)+  # plot ecoregions
    
    tidyterra::geom_spatvector(
      data = xxx, fill = "white"#ggplot2::aes(color = "white")
    )+
    tidyterra::geom_spatvector(data = terra::project(xy_sfrep,  rast.crs ),  size=0.5)+ # plot add or remove points
    
    ggplot2::coord_sf(
                    expand = FALSE)+# crop the plotting+
    ggplot2::xlab(NULL)+
    ggplot2::ylab(NULL)+
    ggplot2::theme_bw()+
    ggplot2::theme(
      legend.position = "none")+
    ggspatial::annotation_north_arrow(location= "br",
                                        pad_y = grid::unit(0.25, "in"),
                                        width = grid::unit(1, "cm"))+
    ggspatial::annotation_scale(location= "br")
  
 # ggplot2::ggsave(plottopo, filename = "topotest07.png", path = "plots/", units = "in", width = 10, height = 10, dpi = 300, bg = "white")
  
  
  
  
  country <- ggplot2::ggplot() +
    ggplot2::xlab("Longitude")+
    ggplot2::ylab("Latitude")+  
    tidyterra::geom_spatvector(data = geo_pol0, ggplot2::aes(), fill = "plum",  linewidth=0.5 ,color = "darkgrey")+
    ggplot2::xlab(NULL)+
    ggplot2::ylab(NULL)+
    ggplot2::theme_bw()+
    ggplot2::coord_sf(
      xlim=c(as.numeric(terra::ext(geo_pol0)[1]), 
                            as.numeric(terra::ext(geo_pol0)[2]-5)), #crop to extent of points
               ylim = c(as.numeric(terra::ext(geo_pol0)[3]), 
                               as.numeric(terra::ext(geo_pol0)[4])),
                      expand = FALSE)+
    ggplot2::theme(
      plot.margin = ggplot2::margin(0,,0,0, "cm"),
      legend.position = "none", 
     axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
     plot.background = ggplot2::element_blank(),
     panel.grid.major = ggplot2::element_blank(),
     panel.grid.minor = ggplot2::element_blank(),
     panel.border = ggplot2::element_blank()
     )+
    ggplot2::geom_polygon(data = rect, ggplot2::aes(x, y, group = 1), 
                          colour = "black", fill = "transparent", linewidth = 2)
  
  ecoregions_inset<- cowplot::ggdraw(plottopo) +
    cowplot::draw_plot(
      { country 
        #theme(panel.border=element_blank())
      },
      # The distance along a (0,1) x-axis to draw the left edge of the plot
      x = 0.07, 
      # The distance along a (0,1) y-axis to draw the bottom edge of the plot
      y = 0.55,
      # The width and height of the plot expressed as proportion of the entire ggdraw object
      width = 0.3, 
      height = 0.3)
  
  #ecoregions_inset
  
  ggplot2::ggsave(ecoregions_inset, filename = "ecoregions_insetfinalv1.png",
                  path = "plots/", units = "in", width = 10, height = 8, dpi = 300, bg = "white")
  
  gc()
  
}
