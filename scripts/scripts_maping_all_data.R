#### Analisis con toda la data (Sin filtrar high accuracy of 500m)

# Mapas de diversidad, fenologia y distribucion general


#load base layers

mapa_olson <- terra::vect("data/official WWF/wwf_terr_ecos.shp")

geo_pol <- terra::vect("data/gadm_brazil/gadm41_BRA_1.shp")


## Load full dataset

# RAw file with high a low accuray, only for general stuff, not elevation or temp calcs
main_raw <- read.csv("data/Data de ocurrencia_ad_n_ninf.csv")


vector_order <- read.csv("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/Species ordered by family - Hoja 1.csv")

vector_order$spp <- paste0(vector_order$Species,"_",vector_order$X )
vector_order<-vector_order$spp


# Format data frame, lon and lat, posixct Month label, 

main_data <- main_raw|>
  dplyr::mutate(
    #longitude = as.character(longitude),
    #latitude = as.character(latitude),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    time_date = as.POSIXct(time_observed_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    month = lubridate::month(time_date),
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )


#####################################################################
##  1. Map of ecoregions in Rio
#####################################################################



#head(main_data)

#main_data[which(is.na(main_data$latitude)),]

#main_data[420,]
#main_data[973,]

#produce vector of all locations
# to use for cropping later the ecoregions and political map

vect_1 <- terra::vect(main_data[,c("sp_name","longitude", "latitude", "Family")], 
                      geom = c("longitude", "latitude"),
                      crs= "EPSG:4326")

#terra::plot(vect_1)
extent_samp <-terra::ext(vect_1)

# crop olson to raw obs with a small buffer
olson_sub <- terra::crop(mapa_olson, c(-45.0656538667, -40.0297000036, -23.3413619722, -20.575620752))
#terra::plot(olson_sub)

# crop geo political map to raw obs with a small buffer

#now is crop to only rio
#geo_pol_crop <- terra::subset(geo_pol, 
#                             geo_pol$NAME_1 == "Rio de Janeiro", names(geo_pol))

geo_pol_crop_alt <- terra::crop(geo_pol, c(-45.0656538667, -40.0297000036, -23.3413619722, -20.575620752))


#uno <- ggplot2::ggplot()+
# tidyterra::geom_spatvector(data=geo_pol_crop)
#dos <- ggplot2::ggplot()+
# tidyterra::geom_spatvector(data=geo_pol_crop_alt)+
#ggplot2::coord_sf(expand = FALSE)

#cowplot::plot_grid(uno, dos, nrow = 1)


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

#ggplot2::ggsave(basic_eco_geopol, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/basic_eco_geopolv1.png",
#height =  3.75,width = 8,dpi = 300, units = "in",bg = "white")


#####################################################################
##  2. Map of ecoregions in Rio and full dataset as points
#####################################################################

eco_map_01 <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt)+ #extra base layer for white background
  tidyterra::geom_spatvector(data = olson_sub, ggplot2::aes(fill = ECO_NAME), color = "transparent",  alpha = 0.45)+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, alpha=0)+
  tidyterra::geom_spatvector(data = vect_1  )+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::theme_bw()+
  ggplot2::scale_fill_discrete(name="Ecoregion (Olsen, 2001)" )+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")


#ggplot2::ggsave(eco_map_01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/eco_map_01v2.png",
#                height =  3.75,width = 8,dpi = 300, units = "in",bg = "white")




ecoreg_ras <- terra::rasterize(terra::project(olson_sub, elev_rast), elev_rast, "ECO_NAME" )

occur_ecoregions <- terra::extract(  ecoreg_ras, terra::project(vect_1,  ecoreg_ras))


vect_1<-cbind(vect_1, occur_ecoregions)

unique(vect_1$ECO_NAME)

ecoreg_spp<-vect_1|>
  terra::as.data.frame()|>
  dplyr::group_by(sp_name, ECO_NAME)|>
  dplyr::distinct(ECO_NAME, sp_name)|>
  dplyr::arrange(ECO_NAME)

write.csv(ecoreg_spp, "data/ecoreg_spp.csv") 

###################################################################
##  X. Grid of species maps and their respective data points
###################################################################

plot_grip_spp_points<- main_data |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  dplyr::select("longitude", "latitude", "sp_name")|>
  terra::vect(
    geom = c("longitude", "latitude"),
    crs= "EPSG:4326")|>
  ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt)+ #extra base layer for white background
  tidyterra::geom_spatvector(data = olson_sub, ggplot2::aes(fill = ECO_NAME), color = "transparent",  alpha = 0.45)+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, alpha=0)+
  tidyterra::geom_spatvector( )+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::theme_bw()+
  ggplot2::scale_fill_discrete(name="Ecoregion (Olsen, 2001)" )+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")+
  ggplot2::facet_wrap(~sp_name)+
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave(plot_grip_spp_points, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/eco_map_raw_points01_spp.png",
                height =  15,width = 18,dpi = 300, units = "in",bg = "white")


########################################################################
### 3) Map of Species Richness map and ecoregions (as cells of richness)
########################################################################

##########################
# get elevation raster for base density grid and points
##########################


###option 1
#examp_sf <- sf::st_as_sf(main_data, coords = c("longitude", "latitude"), crs = 4326)

#df_elev_epqs <- elevatr::get_elev_point(examp_sf, prj = 4326, src = "aws")

#df_elev_epqs_df <- terra::as.data.frame(df_elev_epqs)
#write.csv(df_elev_epqs_df, file = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/df_elev_epqs_df.csv")
#df_elev_epqs_df
#summary(df_elev_epqs)

#hist(df_elev_epqs$elevation)

#raster
#elevation <- elevatr::get_elev_raster(examp_sf, z = 9)

#terra::plot(terra::rast(elevation))
#elev_rast <-terra::rast(elevation)   
## continue below

#option 2
# Source
# https://www.ufrgs.br/labgeo/index.php/downloads/dados-geoespaciais/modelos-digitais-de-elevacao-dos-estados-brasileiros-obtidos-a-partir-do-srtm-shuttle-radar-topography-mission/modelos-digitais-de-elevacao-do-srtm-no-formato-geotiff/
# UFRGS IB Centro de Ecologia Laboratório de Geoprocessamento
## WEBER, E.; HASENACK, H.; FERREIRA, C.J.S. 2004. Adaptação do modelo digital de elevação do SRTM para o sistema de referência oficial brasileiro e recorte por unidade da federação. Porto Alegre, UFRGS Centro de Ecologia. ISBN 978-85-63843-02-9. Disponível em http://www.ufrgs.br/labgeo.
##
elev_rast <-terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/rj.tif")   

terra::crs(elev_rast) <- "EPSG:4618"

###########################################

# do not reproject raster yet, only at the end for plotting
#elev_rast_repoj_raw <- terra::project(elev_rast, "EPSG:4326")

#elev_rast_repoj_raw <- terra::project(elev_rast, "EPSG:31983")


# changed project of elevation to wgs84 
#because having issues with 1 spp falling in an edge when rasterizing


#elev_rast_repoj<- terra::aggregate(elev_rast_repoj_raw, 6)

# dens version is to plot the density map with a large pixel/cell size
elev_rast_repoj_dens <- terra::aggregate(elev_rast, 42)

############################
# make rasters of each species
############################
spp_df <- main_data |>
  dplyr::mutate(
    sp_name = paste0(Genus,"_", Species) )

#spp_df|>
# dplyr::group_by(sp_name)|>
#dplyr::summarise(
# Total = dplyr::n()
#)|>
#dplyr::arrange((Total))


rast_list <- NULL
list_plots <- NULL
for (i in 1:length(unique(spp_df$sp_name))) {
  spp <- unique(spp_df$sp_name)[i]
  one_spp <- spp_df |>
    dplyr::filter(sp_name == spp)
  
  vect_1_spp <- terra::vect(one_spp[,c("longitude", "latitude")], 
                            geom = c("longitude", "latitude"),
                            crs= "EPSG:4326")
  
  vect_1_spp <- terra::project(vect_1_spp, elev_rast_repoj_dens)
  #print(vect_1_spp)
  # to dens rast for plotting
  rast1spp <- terra::rasterize(vect_1_spp, elev_rast_repoj_dens
                               , fun = "count")
  
  rast1spp <- terra::ifel(rast1spp > 0 ,1, 0) 
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

#ggplot2::ggsave(plot_grid_br,
#               filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_grid_spp_densag42.png",
#              height = 15, width = 15, 
#             bg =  "white",
#            dpi = 300)

sum_rasts <-terra::app(rasters_all, fun = "sum")

sum_rasts <- terra::ifel(sum_rasts == 0, NA, sum_rasts)


#geo_pol_crop2<- terra::project(geo_pol,"EPSG:31983")

# reproject geo pol shape into original elev rast, and crop
geo_pol_crop2 <- terra::crop(terra::project(geo_pol_crop_alt, sum_rasts), sum_rasts)


#ggplot2::ggsave(basic_density,
#               filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/basic_density_v3.png",
#              height = 5, width = 7, 
#             bg =  "white",
#            dpi = 300)


#final product
density_v1 <- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt)+  
  tidyterra::geom_spatvector(data = olson_sub, 
                             ggplot2::aes(fill = ECO_NAME), 
                             color = "transparent",  alpha = 0.45)+
  ggplot2::scale_fill_discrete(name="Ecoregion (Olsen, 2001)" )+
  ggnewscale::new_scale_fill()+
  tidyterra::geom_spatraster(data = terra::crop(sum_rasts, geo_pol_crop_alt, snap = "in"))+
  ggplot2::scale_fill_viridis_c(name="Species Richness",direction = -1, na.value = "transparent")+
  ggplot2::theme_bw()+
  ggplot2::coord_sf(crs ="EPSG:4326", expand = FALSE)+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")

#ggplot2::ggsave(density_v1,
#               filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/density_v1.png",
#              height = 5.5, width = 10.5, 
#             bg =  "white",
#            dpi = 300)






###############################
# 8) Phenology cummulative adults plots (monthly richness)
###############################

plot_phenology01 <- main_data |>
  dplyr::filter(!is.na(month))|>
  dplyr::filter(Instar == "Adult")|>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species), 
    Month = lubridate::month(time_date, label = TRUE, abbr = FALSE)
  )|>
  dplyr::distinct(Month, sp_name)|>
  dplyr::group_by(Month)|>
  dplyr::summarise(
    Total_spp = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=as.factor(Month), y = Total_spp))+
  ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  ggplot2::ylab("Species richness (adults)")+
  ggplot2::xlab("Month")+
  ggplot2::theme_bw()+  
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

#ggplot2::ggsave(plot_phenology01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_phenology01.png",
#               height =  3.75,
#              width = 8,
#             dpi = 300, 
#            units = "in", 
#           bg = "white")


# All cummulative observations: dif of instar, not informative as it dilutes species differences

#plot_phenology_all_instar <- main_data |>
# dplyr::filter(!is.na(month))|>
#dplyr::mutate(
# sp_name = paste0(Genus     ,"_", Species), 
#Month = lubridate::month(time_date, label = TRUE, abbr = FALSE)
#)|>
#  dplyr::group_by(sp_name,Month, Instar)|>
# dplyr::summarise(
#  Total_spp = dplyr::n()
#  )|>
# ggplot2::ggplot(ggplot2::aes(x=as.factor(Month), y = Total_spp, fill =  Instar))+
#ggplot2::geom_bar( stat='identity')+
#ggplot2::ylab("Observations")+
#ggplot2::xlab("Month")+
#ggplot2::theme_bw()+  
#ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)
#              ,legend.position = "top"
#  )

#ggplot2::ggsave(plot_phenology_all_instar, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_phenology_all_instar_01.png",
#               height =  5.75,
#              width = 8,
#            dpi = 300, 
#           units = "in", 
#          bg = "white")

####################################################### 
### 9) Phenology adults and nymphs per species
####################################################### 



# plots of phenology only nymph and adult spp

both_inst <- main_data |>
  dplyr::filter(!is.na(month))|>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species), 
    Month = lubridate::month(time_date, label = TRUE, abbr = FALSE)
  )|>
  #dplyr::distinct(Month, sp_name, Instar)|>
  dplyr::select(sp_name, Instar)|>
  
  dplyr::group_by(sp_name, Instar)|>
  dplyr::summarise(
    Total_spp = dplyr::n()
  )|>
  tidyr::pivot_wider(names_from = Instar, values_from =  Total_spp)|>
  dplyr::filter(!is.na(Nymph))|>
  dplyr::pull(sp_name)# nymphs are rare



plot_both_instar_phen <- main_data |>
  dplyr::filter(!is.na(month))|>
  dplyr::mutate(
    sp_name = paste0(Genus     ,"_", Species), 
    Month = lubridate::month(time_date, label = TRUE, abbr = FALSE)
  )|>
  dplyr::filter(  sp_name %in% both_inst)|>
  
  #dplyr::distinct(Month, sp_name, Instar)|>
  dplyr::group_by(sp_name,Month, Instar)|>
  dplyr::summarise(
    Total_spp = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=as.factor(Month), y = Total_spp, fill =  Instar))+
  ggplot2::geom_bar( stat='identity')+
  ggplot2::ylab("Observations")+
  ggplot2::xlab("Month")+
  ggplot2::theme_bw()+  
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)
                 ,legend.position = "top"
  )+
  ggplot2::facet_wrap(~sp_name, scales="free_y", # dos versions, una con scale free y otro no
                      ncol = 3)

ggplot2::ggsave(plot_both_instar_phen, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_both_instar_phen_01.png",
                height =  6,
                width = 8,
                dpi = 300, 
                units = "in", 
                bg = "white")






############################################  
#### 15) Richness per municipality
###################### ###################### 

plot_munucip <- spp_df |>
  dplyr::group_by(municipality)|>
  dplyr::distinct(sp_name)|>
  dplyr::summarise(
    Total.spp = dplyr::n()
  )|>
  dplyr::arrange(Total.spp)|>
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x = Total.spp, y =  reorder(municipality, Total.spp)))+
  ggplot2::coord_cartesian(expand = FALSE)+
  ggplot2::xlab("Species Richness")+
  ggplot2::ylab("Municipality")

ggplot2::ggsave(plot_munucip, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_munucip.png",
                height =  8,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

#######################################################
## Areas de conservacion
######################################################
privadas <- terra::vect("data/RJ_priv/RJ.shp")


wwf <- terra::vect("data/RJ - Unidades de Conservacion/limites_ucs_federais_27022025_a.shp")
head(wwf)
unique(wwf$SiglaCateg)
names(wwf)
terra::plot(privadas)

#Rebio-UNA - Reserva Biológica de Una
unique(wwf$BiomaIBGE)
#[1] "REBIO" "ESEC"  "PARNA" "RESEX" "REVIS" "MONA"  "ARIE"  "APA"   "FLONA"
#[10] "RDS"

#REBIO Reserva biologica
#Esec– Estação Ecológica
# PARNA parque nacional o parque nacional natural?
#Resex – Reserva Extrativista
#Revis- Refúgio de Vida Silvestre
#MONA Monumento Natural? 
#Arie – Área de Relevante Interesse Ecológico
#APA – Área de Proteção Ambiental
#Flona – Floresta Nacional
#RDS Reserva de Desarrollo Sostenible
wwf_rpj <- terra::crop(wwf,geo_pol_crop_alt )

wwf_rpj <- terra::project(wwf_rpj, geo_pol_crop_alt)

privadas_rpj <- terra::project(privadas, geo_pol_crop_alt)


unique(wwf_rpj$BiomaIBGE)
unique(wwf_rpj$SiglaCateg)
unique(wwf_rpj$NomeUC)

unique(privadas_rpj$nome)
names(privadas_rpj)

wwf_rpj <- wwf_rpj[,c("abrev", "SiglaCateg")]


privadas_rpj <- privadas_rpj[,c("nome", "antigo")]

privadas_rpj$antigo <- "RPPN"
names(privadas_rpj) <- c("abrev","SiglaCateg" )

"Reserva Particular do Patrimônio Natural Reserva Ecológica de Guapiaçu"

wkt <-  "POLYGON((-42.70677255 -22.44598985,-42.70682136 -22.44605116,-42.70690845 -22.4460859,-42.70709534 -22.4461077,-42.70733932 -22.44610638,-42.70758293 -22.44609996,-42.7079476 -22.4460674,-42.70810013 -22.4460617,-42.70825206 -22.44609216,-42.70833601 -22.44613582,-42.70846749 -22.44620774,-42.70861215 -22.44629143,-42.70872925 -22.44633044,-42.70887334 -22.44635879,-42.70909628 -22.44640116,-42.70943487 -22.44646242,-42.70949974 -22.44646916,-42.70955806 -22.44649141,-42.70962441 -22.44653518,-42.70970077 -22.44660826,-42.7098733 -22.4468248,-42.70988941 -22.44686889,-42.70988163 -22.4469504,-42.70986724 -22.4470378,-42.7098758 -22.44706913,-42.70989733 -22.44709464,-42.70993699 -22.4471192,-42.70998122 -22.44713745,-42.71008034 -22.44715542,-42.71030312 -22.44716922,-42.71037051 -22.4471761,-42.71054473 -22.44719869,-42.71091455 -22.44725196,-42.71102786 -22.44699745,-42.71290217 -22.44278749,-42.71248304 -22.44263045,-42.71224262 -22.44254036,-42.71164988 -22.44219112,-42.71117691 -22.44191149,-42.7107852 -22.44180043,-42.71048363 -22.4417485,-42.70965891 -22.44160959,-42.70928909 -22.44091061,-42.70852668 -22.4407736,-42.70844273 -22.44044881,-42.70826409 -22.44016944,-42.70812225 -22.44007524,-42.70783656 -22.44022868,-42.70743269 -22.44026181,-42.70564745 -22.4441231,-42.70555159 -22.44433038,-42.70569341 -22.44442472,-42.70582355 -22.44450216,-42.70595939 -22.44458102,-42.70612753 -22.44469083,-42.70629486 -22.44481316,-42.70645232 -22.4449426,-42.70657429 -22.44505888,-42.70668449 -22.4452165,-42.7067111 -22.44527744,-42.70673271 -22.44532934,-42.70674765 -22.4454262,-42.7067479 -22.44546777,-42.70673586 -22.44560404,-42.70671399 -22.44574696,-42.70671811 -22.445854,-42.70673256 -22.44591591,-42.70677255 -22.44598985))"

v <- terra::vect(wkt, crs = "EPSG:4326")  # assuming WGS84 coordinate system

v$abrev <- "RPPN Reserva Ecológica de Guapiaçu"

v$SiglaCateg <-"RPPN"
  
wwf_rpj<- rbind(wwf_rpj, privadas_rpj, v)

cons_areas_map01<- ggplot2::ggplot()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, alpha = 0)+  
  tidyterra::geom_spatvector(data = v, 
                             ggplot2::aes(fill = SiglaCateg ), 
                             color = "transparent",  alpha = 0.45)+
  ggplot2::scale_fill_discrete(name="Unidad de Conservacion" )+
  ggnewscale::new_scale_fill()+
  tidyterra::geom_spatraster(data = terra::crop(sum_rasts, geo_pol_crop_alt, snap = "in"))+
  ggplot2::scale_fill_viridis_c(name="Species Richness",direction = -1, na.value = "transparent")+
  ggplot2::theme_bw()+
  ggplot2::coord_sf(#crs ="EPSG:4326", 
    expand = FALSE)+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")


ggplot2::ggsave(cons_areas_map01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/plot_munucip.png",
                height =  5,
                width = 10,
                dpi = 300, 
                units = "in", 
                bg = "white")



wwf_cons_area <-wwf_rpj
#wwf_cons_area2 <-wwf_rpj[,"SiglaCateg"]


wwf_cons_area_rast <- terra::rasterize(terra::project(wwf_cons_area, elev_rast), elev_rast, "abrev" )
#if rasterizing directly from shapefile, can get double areas (maybe there are polygons contained insided others)
wwf_cons_area_rast2 <- terra::rasterize(terra::project(wwf_cons_area, elev_rast), elev_rast, "SiglaCateg" )


### only to check why REGUA is missing in the selection
all_cons <- ggplot2::ggplot()+
  tidyterra::geom_spatraster(data=wwf_cons_area_rast)+
  
  ggplot2::scale_fill_viridis_d(na.value = "transparent")+
  tidyterra::geom_spatvector(data=terra::project(wwf_cons_area,wwf_cons_area_rast) , alpha =0)+
  
  tidyterra::geom_spatvector(data=vect_1, alpha =0.15)+
  ggplot2::theme(legend.position = "none")
  
ggplot2::ggsave(all_cons, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/all_cons.png",
                height =  20,
                width = 20,
                dpi = 500, 
                units = "in", 
                bg = "white")

ocurr_cons_area <- terra::extract(method = "simple", x =wwf_cons_area_rast , y=terra::project(vect_1, elev_rast)  )
ocurr_cons_area2 <- terra::extract(x =wwf_cons_area_rast2 , y=terra::project(vect_1, elev_rast)  )

vect_1_ext<-cbind(vect_1, ocurr_cons_area[,"abrev"], ocurr_cons_area2["SiglaCateg"])


vect_1_ext|>
  terra::as.data.frame()|>
  ggplot2::ggplot(ggplot2::aes(x = SiglaCateg) ) +
  ggplot2::geom_bar( fill =  "#009E73")+
  ggplot2::theme_bw()+ 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                 legend.position = "none")+
  ggplot2::xlab("Conservation area")+
  ggplot2::ylab("Observations")

vect_cons <- vect_1_ext|>
  terra::as.data.frame()|>
  dplyr::pull(y)|>
  unique()|>
  droplevels()|>
  as.character()





obs_plot_conserv <- vect_1_ext|>
  terra::as.data.frame()|>
  #dplyr::filter(!is.na(NomeUC))|>
  dplyr::mutate(
    y = factor(y, levels = vect_cons)
   # = dplyr::case_when(DESC_NI =="Água"~ "Outras Áreas",  TRUE~ DESC_NI)
  )|>
  ggplot2::ggplot(ggplot2::aes(x = y) ) +
  ggplot2::geom_bar( fill =  "#009E73")+ 
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust=1),
                                                     plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 1), "cm"))+
                   
  ggplot2::xlab("Conservation area")+
  ggplot2::ylab("Observations")
ggplot2::ggsave(obs_plot_conserv, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/obs_plot_conserv.png",
                height =  7,
                width = 7,
                dpi = 300, 
                units = "in", 
                bg = "white")

sp_rich_consv_area <- vect_1_ext|>
  terra::as.data.frame()|>
    dplyr::mutate(
      y = factor(y, levels = vect_cons)
      # = dplyr::case_when(DESC_NI =="Água"~ "Outras Áreas",  TRUE~ DESC_NI)
    )|>
  #dplyr::mutate(
   # y = dplyr::case_when(is.na(y)~ "Not federally protected",  TRUE~ y)
  #)|>
  dplyr::group_by(sp_name, y)|>
  dplyr::distinct(y, sp_name)|>
  ggplot2::ggplot(ggplot2::aes(x = y) ) +
  ggplot2::geom_bar( fill =  "#D55E00")+
  ggplot2::theme_bw()+ 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust=1),
                 legend.position = "none", 
                 plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 2), "cm"))+
  ggplot2::xlab("Conserved Area")+
  ggplot2::ylab("Species Richness")

ggplot2::ggsave(sp_rich_consv_area, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/sp_rich_consv_area.png",
                height =  7,
                width = 7,
                dpi = 300, 
                units = "in", 
                bg = "white")

cons_area_list_pp <- vect_1_ext|>
  terra::as.data.frame()|>
  #dplyr::mutate(
   # DESC_NII = dplyr::case_when(DESC_NII =="Águas Costeiras"~ "Outras Áreas",  TRUE~ DESC_NII)
  #)|>
  dplyr::group_by(sp_name, y)|>
  dplyr::distinct(y, sp_name)|>
  dplyr::arrange(y)

write.csv(cons_area_list_pp, "data/cons_area_list_pp.csv") 



