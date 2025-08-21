#load base layers

mapa_olson <- terra::vect("data/official WWF/wwf_terr_ecos.shp")

geo_pol <- terra::vect("data/gadm_brazil/gadm41_BRA_1.shp")


## Load full dataset

# old file, will change eventually
main_raw <- read.csv("data/Data de ocurrencia_ad_n_ninf.csv")



#load dataset with high accuracy for extraction of env variables


#head(mapa_olson)
#names(mapa_olson)
#terra::plot(mapa_olson)

head(main_raw)

# Format data frame, lon and lat, posixct Month label, 

main_data <- main_raw|>
  dplyr::mutate(
    #longitude = as.character(longitude),
    #latitude = as.character(latitude),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    time_date = as.POSIXct(time_observed_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    month = lubridate::month(time_date)
  )

#######################
## Map 1. ecoregion in Rio
#######################



#head(main_data)

#main_data[which(is.na(main_data$latitude)),]

#main_data[420,]
#main_data[973,]

#produce vector of all locations
# to use for cropping later the ecoregions and political map

vect_1 <- terra::vect(main_data[,c("longitude", "latitude")], 
            geom = c("longitude", "latitude"),
            crs= "EPSG:4326")

#terra::plot(vect_1)
extent_samp <-terra::ext(vect_1)

# crop olson to raw obs with a small buffer
olson_sub <- terra::crop(mapa_olson, c(-45.0656538667, -40.0297000036, -23.3413619722, -20.575620752))
#terra::plot(olson_sub)

# crop geo political map to raw obs with a small buffer

#now is crop to only rio
geo_pol_crop <- terra::subset(geo_pol, 
                              geo_pol$NAME_1 == "Rio de Janeiro", names(geo_pol))

geo_pol_crop_alt <- terra::crop(geo_pol, c(-45.0656538667, -40.0297000036, -23.3413619722, -20.575620752))


uno <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data=geo_pol_crop)
dos <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data=geo_pol_crop_alt)+
  ggplot2::coord_sf(expand = FALSE)

cowplot::plot_grid(uno, dos, nrow = 1)


basic_eco_geopol <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt)+ #extra base layer for white background
  tidyterra::geom_spatvector(data = olson_sub, ggplot2::aes(fill = ECO_NAME), color = "transparent",  alpha = 0.45)+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, alpha=0)+
  ggplot2::theme_bw()+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::scale_fill_discrete(name="Ecoregion (Olsen, 2001)" )+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")

ggplot2::ggsave(basic_eco_geopol, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/basic_eco_geopolv1.png",
                height =  3.75,
                width = 8,
                dpi = 300, 
                units = "in", 
                bg = "white")


eco_map_01 <- ggplot2::ggplot()+
 tidyterra::geom_spatvector(data = olson_sub, ggplot2::aes(fill = ECO_NAME), color = "transparent",  alpha = 0.45)+
  tidyterra::geom_spatvector(data = vect_1  )+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, #ggplot2::aes(fill =NAME_1 ), 
                             alpha = 0)+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::theme_bw()+
  ggplot2::scale_fill_discrete(name="Ecoregion (Olsen, 2001)" )+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")


ggplot2::ggsave(eco_map_01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/eco_map_01v2.png",
                height =  3.75,
                width = 8,
                dpi = 300, 
                units = "in", 
                bg = "white")

## can modify title of legend!!!!!!!!!!!!!!!!






plot_phenology01 <- main_data |>
  dplyr::filter(!is.na(month))|>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species)
  )|>
  dplyr::distinct(month, sp_name)|>
  dplyr::group_by(month)|>
  dplyr::summarise(
    Total_spp = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=as.factor(month), y = Total_spp))+
  ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  ggplot2::ylab("Species richness")+
  ggplot2::xlab("Month")+
  ggplot2::theme_bw()

ggplot2::ggsave(plot_phenology01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/plot_phenology01.png",
                height =  3.75,
                width = 8,
                dpi = 300, 
                units = "in", 
                bg = "white")



phenology_grid01 <- main_data |>
  dplyr::filter(!is.na(month))|>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species)
  )|>
  #dplyr::distinct(month, sp_name)|>
  dplyr::group_by(month, sp_name)|>
  dplyr::summarise(
    Total_spp = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=as.factor(month), y = Total_spp))+
  ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  ggplot2::ylab("Species richness")+
  ggplot2::xlab("Month")+
  ggplot2::theme_bw()+
  ggplot2::facet_wrap(~sp_name, scales="free_y", # dos versions, una con scale free y otro no
                      ncol = 5)


ggplot2::ggsave(phenology_grid01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/phenology_grid02.png",
                height =  12,
                width = 8.5,
                dpi = 300, 
                units = "in", 
                bg = "white")
summary(main_data)
summary(main_raw)


##########################
#get elevation for points
##########################


###option 1
#examp_sf <- sf::st_as_sf(main_data, coords = c("longitude", "latitude"), crs = 4326)

#df_elev_epqs <- elevatr::get_elev_point(examp_sf, prj = 4326, src = "aws")

#df_elev_epqs_df <- terra::as.data.frame(df_elev_epqs)
#write.csv(df_elev_epqs_df, file = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/df_elev_epqs_df.csv")
#df_elev_epqs_df
#summary(df_elev_epqs)

#hist(df_elev_epqs$elevation)

#raster
#elevation <- elevatr::get_elev_raster(examp_sf, z = 9)

#terra::plot(terra::rast(elevation))
#elev_rast <-terra::rast(elevation)   
## continue below

#option 2
elev_rast <-terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/rj.tif")   

terra::crs(elev_rast) <- "EPSG:4618"


terra::plot(elev_rast)
terra::plot( add = TRUE, terra::project(geo_pol_crop, elev_rast))

###########################################
elev_rast_repoj <- terra::project(elev_rast, "EPSG:31983")

elev_rast_repoj<- terra::aggregate(elev_rast_repoj, 6)

elev_map01 <- ggplot2::ggplot()+
  tidyterra::geom_spatraster(data = elev_rast_repoj)+
  ggplot2::scale_fill_viridis_c(option = "E","Elevation")+
  tidyterra::geom_spatvector(
    data = vect_1, alpha = 0.5
  )+
  #ggnewscale::new_scale_fill()+
  tidyterra::geom_spatvector(data = geo_pol_crop, #ggplot2::aes(fill =NAME_1 ), 
                             alpha = 0)+
  ggplot2::coord_sf(expand = FALSE)

ggplot2::ggsave(elev_map01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_map02v1.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

#elevation map can be clipped of for ocean

#points with each elev value colored

# for option 2


df_elev_epqs_elevs <- terra::extract(elev_rast_repoj, terra::project(vect_1, elev_rast_repoj))

df_elev_epqs <- main_data
df_elev_epqs$elevation <- df_elev_epqs_elevs[,2]

##


elev_points_map1 <- df_elev_epqs|>
  #dplyr::filter(elevation >1000)|>
  terra::vect(geom=c("longitude", "latitude"), crs = terra::crs(vect_1))|>
  ggplot2::ggplot()+
  tidyterra::geom_spatvector(data =geo_pol_crop, alpha = 0)+
  tidyterra::geom_spatvector(ggplot2::aes(color = elevation))+
  ggplot2::scale_color_viridis_c()



ggplot2::ggsave(elev_points_map1, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_points_map2v1.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

############################
# bin species with elevation
############################

elev_bin_plot <- df_elev_epqs |>
  dplyr::mutate(
     elev_bin = dplyr::case_when(elevation < 250 ~ "A", .default = "ucat"),
     elev_bin = dplyr::case_when(elevation >= 250 &  elevation < 500 ~ "B", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 500 &  elevation < 750 ~ "C", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 750 &  elevation < 1000 ~ "D", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 1000 &  elevation < 1250 ~ "E", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 1250 &  elevation < 1500 ~ "F", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 1500 &  elevation < 1750 ~ "G", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 1750 &  elevation < 2000 ~ "H", .default = elev_bin),
     elev_bin = dplyr::case_when(elevation >= 2000 ~ "I", .default = elev_bin),
     sp_name = paste0(Genus     ,"_", Species)
     )|>
  dplyr::distinct(elev_bin, sp_name)|>
  dplyr::group_by(elev_bin)|>
  dplyr::summarise(
    Total_elev = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=as.factor(elev_bin), y = Total_elev))+
  ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  ggplot2::ylab("Species richness")+
  ggplot2::xlab("Elevation class")

ggplot2::ggsave(elev_bin_plot, 
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_bin_plot02_v2.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

elev_means_01 <- df_elev_epqs |>
  dplyr::mutate(
   sp_name = paste0(Genus     ,"_", Species)
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = sp_name, color = sp_name,  y = elevation ))+
  ggplot2::ylab("Elevation")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(elev_means_01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_means_02_v2.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")


elev_means_01_fams <- df_elev_epqs |>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species)
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = sp_name, color = sp_name,  y = elevation ))+
  ggplot2::ylab("Elevation")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")+
  ggplot2::facet_wrap(~Family#, scales = "free"
                      )

ggplot2::ggsave(elev_means_01_fams, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_means_02_famsv4.png",
                height =  10,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

vector_order <- read.csv("C:/Users/franc/Documents/Research/Brazil - julio insectos/Species ordered by family - Hoja 1.csv")
head(vector_order)
vector_order$spp <- paste0(vector_order$Species,"_",vector_order$X )
vector_order<-vector_order$spp



species_raw <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species)#,
    #sp_name = factor(sp_name, levels = vector_order)
  )|>
  dplyr::pull(sp_name)|>
  unique()


elev_means_02 <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = sp_name, color = sp_name,  y = elevation ))+
  ggplot2::ylab("Elevation")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(elev_means_02, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/elev_means_02_v3.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

# reproject
# EPSG:31983 and the administrative region in EPSG:29193


############################
# make rasters of each species
############################
spp_df <-main_data |>
  dplyr::mutate(
   sp_name = paste0(Genus     ,"_", Species) )

rast_list <- NULL
list_plots <- NULL
for (i in 1:length(unique(spp_df$sp_name))) {
  spp <- unique(spp_df$sp_name)[i]
  one_spp <- spp_df |>
    dplyr::filter(sp_name == spp)
  
  vect_1_spp <- terra::vect(one_spp[,c("longitude", "latitude")], 
                        geom = c("longitude", "latitude"),
                        crs= "EPSG:4326")
  
  vect_1_spp <- terra::project(vect_1_spp, elev_rast_repoj)
  #print(vect_1_spp)
  rast1spp <- terra::rasterize(vect_1_spp, elev_rast_repoj
                               , fun = "count")
  
  rast1spp <- terra::ifel(rast1spp >0 ,1, 0) 
  rast1spp <- terra::ifel(is.na(rast1spp), 0, rast1spp) 
  #terra::plot(rast1spp)
  #title(paste(spp, i))
 names(rast1spp) <- spp
  rast_list[[i]] <- rast1spp
  
  one_plot <- ggplot2::ggplot()+
                tidyterra::geom_spatraster(data = rast1spp)+
                ggplot2::scale_fill_viridis_c(direction = -1)+
                ggplot2::coord_sf(expand = FALSE)+
                ggplot2::theme(legend.position = "none")+
                ggplot2::ggtitle(spp)
  
  list_plots[[i]] <- one_plot
  
  #readline('next')
  
  }


rasters_all <- terra::rast(rast_list)

#terra::plot(rasters_all)

plot_grid_br <- cowplot::plot_grid( plotlist =list_plots, nrow = 7, ncol = 5)

ggplot2::ggsave(plot_grid_br,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/plot_grid1.png",
                height = 15, width = 10, 
                bg =  "white",
                dpi = 300)

sum_rasts <-terra::app(rasters_all, fun = "sum")

sum_rasts <- terra::ifel(sum_rasts == 0, NA, sum_rasts)



geo_pol_crop2<- terra::project(geo_pol,"EPSG:31983")

geo_pol_crop2 <- terra::crop(geo_pol_crop2, sum_rasts)

basic_density <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = terra::project(geo_pol_crop2, terra::crs(sum_rasts)))+  
  tidyterra::geom_spatraster(data = sum_rasts)+
  ggplot2::scale_fill_viridis_c(direction = -1, na.value = "transparent")+
  ggplot2::coord_sf(expand = FALSE)
ggplot2::ggsave(basic_density,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/basic_density.png",
                height = 5, width = 7, 
                bg =  "white",
                dpi = 300)

density_v1 <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = terra::project(geo_pol_crop2, terra::crs(sum_rasts)))+ 
  ggnewscale::new_scale_fill()+
  tidyterra::geom_spatvector(data = olson_sub, 
                             ggplot2::aes(fill = ECO_NAME), 
                             color = "transparent",  alpha = 0.45)+
  ggnewscale::new_scale_fill()+
  
  tidyterra::geom_spatraster(data = sum_rasts, alpha = 0.5)+
  ggplot2::scale_fill_viridis_c(direction = -1, na.value = "transparent")+
  ggplot2::coord_sf(expand = FALSE)

  
ggplot2::ggsave(density_v1,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/density_v1.png",
                height = 4.5, width = 8, 
                bg =  "white",
                dpi = 300)





summary(terra::values(sum_rasts))

table(terra::values(sum_rasts))

##################################
# land cover
###################################



raw_landuse_cat<-terra::vect( "C:/Users/franc/Documents/Research/Brazil - julio insectos/RJ - Uso de Tierra (250mil)/Rio_de_Janeiro.shp")

terra::crs(raw_landuse_cat) <- "PROJCS[\"Albers_Conic_Equal_Area\",
       GEOGCS[\"GCS_Geographic Coordinate System\",
              DATUM[\"D_SIRGAS_2000\",
                    SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],
              PRIMEM[\"Greenwich\",0.0],
              UNIT[\"Degree\",0.0174532925199433]],
       PROJECTION[\"Albers\"],
       PARAMETER[\"false_easting\",0.0],
       PARAMETER[\"false_northing\",0.0],
       PARAMETER[\"central_meridian\",-54.0],
       PARAMETER[\"standard_parallel_1\",-1.0],
       PARAMETER[\"standard_parallel_2\",-28.0],
       PARAMETER[\"latitude_of_origin\",5.0],
       UNIT[\"Meter\",1.0]
       ]"

#rast_full <- terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/land_use_timeseries/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tiff")
#rast_full_crop <- terra::crop(rast_full, terra::project(geo_pol_crop2, rast_full))
#terra::plot(rast_full_crop)


land_use_map01 <-ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = raw_landuse_cat, ggplot2::aes(fill =DESC_NIII), color = "transparent" )+
  tidyterra::geom_spatvector(data = terra::project(vect_1, raw_landuse_cat))+
  ggplot2::theme(legend.position = "none")

unique(
raw_landuse_cat$DESC_NIII)


ggplot2::ggsave(land_use_map01,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/land_use_map01.png",
                height = 5, width = 8, 
                bg =  "white",
                dpi = 300)



municip<-terra::vect(file.choose())
municip <-terra::subset(municip, 
                        municip$NAME_1 == "Rio de Janeiro", names(municip))

land_use_map02 <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = raw_landuse_cat, ggplot2::aes(fill =DESC_NIII), color = "transparent" )+
  tidyterra::geom_spatvector(data = terra::project(municip, raw_landuse_cat), alpha = 0)+
  tidyterra::geom_spatvector(data = terra::project(vect_1, raw_landuse_cat))+
  
  ggplot2::theme(legend.position = "none")


ggplot2::ggsave(land_use_map02,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/land_use_map02.png",
                height = 5, width = 8, 
                bg =  "white",
                dpi = 300)

landuse_L1 <-raw_landuse_cat[,"DESC_NI"]


landuse_L1_rast <- terra::rasterize(terra::project(landuse_L1, elev_rast), elev_rast, "DESC_NI" )

ocurr_landuse_L1 <- terra::extract(  landuse_L1_rast, vect_1 )

land_use_plot01 <- ocurr_landuse_L1|>
  ggplot2::ggplot(ggplot2::aes(x = DESC_NI) ) +
  ggplot2::geom_bar( fill =  "#009E73")+ 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                                       legend.position = "none")
ggplot2::ggsave(land_use_plot01,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/land_use_plot01.png",
                height = 5, width = 10, 
                bg =  "white",
                dpi = 300)

landuse_L2 <-raw_landuse_cat[,"DESC_NII"]


landuse_L2_rast <- terra::rasterize(terra::project(landuse_L2, elev_rast), elev_rast, "DESC_NII" )

ocurr_landuse_L2 <- terra::extract(  landuse_L2_rast, vect_1 )

land_use_plot02 <- ocurr_landuse_L2|>
  ggplot2::ggplot(ggplot2::aes(x = DESC_NII, fill = DESC_NII) ) +
  ggplot2::geom_bar( )+
  ggplot2::scale_color_viridis_b()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                 legend.position = "none")
ggplot2::ggsave(land_use_plot02,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/land_use_plot02.png",
                height = 5, width = 10, 
                bg =  "white",
                dpi = 300)

vect_2<-cbind(vect_1, ocurr_landuse_L2)
unique(vect_2$DESC_NII)

#check which fall in water
ggplot2::ggplot( ) +
  tidyterra::geom_spatvector(data = terra::project(geo_pol_crop2,vect_2))+
  tidyterra::geom_spatvector(data = terra::subset(vect_2, vect_2$DESC_NII %in% c("Águas Continentais", "Águas Costeiras"),
                                                 "DESC_NII"), 
                             ggplot2::aes(color = DESC_NII))+
  ggplot2::scale_color_viridis_d()


### temperature viz

temp_rast <- terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/CHELSA_bio1_1981-2010_V.2.1.tiff")
ggplot2::ggplot( ) +
  tidyterra::geom_spatraster(data = terra::crop(temp_rast, terra::project(raw_landuse_cat, temp_rast)))+
  tidyterra::geom_spatvector(data = vect_1, alpha = 0.25)+
  ggplot2::scale_colour_viridis_c()


temp_rast_repoj <- terra::project(temp_rast, "EPSG:31983")
temp_rast_repoj<-terra::crop(temp_rast_repoj, terra::project(vect_1, temp_rast_repoj))
#temp_rast_repoj<- terra::aggregate(temp_rast_repoj, 70)

temp_map01 <- ggplot2::ggplot()+
  tidyterra::geom_spatraster(data = temp_rast_repoj)+
  ggplot2::scale_fill_viridis_c(option = "E")+
  tidyterra::geom_spatvector(
    data = vect_1, alpha = 0.5
  )+
  #ggnewscale::new_scale_fill()+
  tidyterra::geom_spatvector(data = geo_pol_crop, #ggplot2::aes(fill =NAME_1 ), 
                             alpha = 0)+
  ggplot2::coord_sf(expand = FALSE)

ggplot2::ggsave(temp_map01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/temp_map01.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")


############################
# bin species with elevation
############################
vect_sf<-sf::st_as_sf(terra::project(vect_1, temp_rast_repoj) )
temp_points <- terra::extract(  temp_rast_repoj,terra::project(vect_1, temp_rast_repoj) )

df_elev_epqs$temp <- temp_points[[2]]
summary(df_elev_epqs$temp )
hist((df_elev_epqs$temp ))
hist(df_elev_epqs$elevation)
#temp_bin_plot <- df_elev_epqs |>
  #dplyr::mutate(
    
   # temp_bin = dplyr::case_when(temp < 12.5 ~ "A", .default = "ucat"),
    #temp_bin = dplyr::case_when(temp >= 12.5 &  elevation < 15 ~ "B", .default = temp_bin),
    #temp_bin = dplyr::case_when(temp >= 15 &  elevation < 17.5 ~ "C", .default = temp_bin),
    #temp_bin = dplyr::case_when(temp >= 17.5 &  elevation < 20 ~ "D", .default = temp_bin),
    #temp_bin = dplyr::case_when(temp >= 20 &  elevation < 22.5 ~ "E", .default = temp_bin),
    #temp_bin = dplyr::case_when(temp >= 22.5  ~ "F", .default = temp_bin),
    #sp_name = paste0(Genus     ,"_", Species)
  #)|>
  #dplyr::distinct(temp_bin, sp_name)|>
  #dplyr::group_by(temp_bin)|>
  #dplyr::summarise(
   # Total_temp = dplyr::n()
  #)|>
  #ggplot2::ggplot(ggplot2::aes(x=as.factor(temp_bin), y = Total_temp))+
  #ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  #ggplot2::ylab("Species richness")+
  #ggplot2::xlab("Temperature class")

temp_bin_plot <- df_elev_epqs |>
  ggplot2::ggplot()+
  ggplot2::geom_histogram(ggplot2::aes(temp), fill = "lightblue")+
  ggplot2::theme_bw()


ggplot2::ggsave(temp_bin_plot, 
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/temp_bin_plot01.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

temp_means_01 <- df_elev_epqs |>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species)
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = sp_name, color = sp_name,  y = temp ))+
  ggplot2::ylab("Temperature")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(temp_means_01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/temp_means_01.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

temp_means_02 <- df_elev_epqs |>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species)
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = Family, color = Family,  y = temp ))+
  ggplot2::ylab("Temperature")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(temp_means_02, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/outputs/temp_means_02.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")
