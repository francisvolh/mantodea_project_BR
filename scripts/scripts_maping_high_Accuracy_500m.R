# Analisis con data con high Accuracy 500m

# Elevacion y Temperatura



#load base layers

mapa_olson <- terra::vect("data/official WWF/wwf_terr_ecos.shp")

geo_pol <- terra::vect("data/gadm_brazil/gadm41_BRA_1.shp")
geo_pol_crop_alt <- terra::crop(geo_pol, c(-45.0656538667, -40.0297000036, -23.4, -20.575620752))


## Load dataset high accuracy

main_accurate <- read.csv("data/Final Dataset with accuracy _500m  - Adultos+Ninfas.csv")

#load dataset with high accuracy for extraction of env variables
#
#

##############################################################
### Elevation outputs only with HIGH accuracy data
##############################################################

###############################
## First elevation map and data is replaced with the hillshade version in a separate script
###############################

# for option 2

vector_order <- read.csv("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/Species ordered by family - Hoja 1.csv")
vector_order$spp <- paste0(vector_order$Species,"_",vector_order$X )
vector_order<-vector_order$spp


main_data_acc <- main_accurate|>
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

vect_2 <- terra::vect(main_data_acc[,c("sp_name", "longitude", "latitude", "Family")], 
                      geom = c("longitude", "latitude"),
                      crs= "EPSG:4326")


# Extract elevations
# Source
# https://www.ufrgs.br/labgeo/index.php/downloads/dados-geoespaciais/modelos-digitais-de-elevacao-dos-estados-brasileiros-obtidos-a-partir-do-srtm-shuttle-radar-topography-mission/modelos-digitais-de-elevacao-do-srtm-no-formato-geotiff/
# UFRGS IB Centro de Ecologia Laboratório de Geoprocessamento
## WEBER, E.; HASENACK, H.; FERREIRA, C.J.S. 2004. Adaptação do modelo digital de elevação do SRTM para o sistema de referência oficial brasileiro e recorte por unidade da federação. Porto Alegre, UFRGS Centro de Ecologia. ISBN 978-85-63843-02-9. Disponível em http://www.ufrgs.br/labgeo.
##
elev_rast <-terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/rj.tif")   

terra::crs(elev_rast) <- "EPSG:4618"

#reproject in UTM to check spatial resolution and match with accuray of iNat
elev_rast_repoj <- terra::aggregate(elev_rast, 5)


df_elev_epqs_elevs <- terra::extract(terra::aggregate(elev_rast_repoj, 5), terra::project(vect_2, elev_rast ))

#do this to follow previous pipeline with the name of dataframes
df_elev_epqs <- main_data_acc
df_elev_epqs$elevation <- df_elev_epqs_elevs[,2]

##

# to observe only point with elevs (this map call dataframe only, no reproj needed)
#elev_points_map1 <- df_elev_epqs|>
  #####dplyr::filter(elevation >1000)|>
  #terra::vect(geom=c("longitude", "latitude"), crs = terra::crs(vect_1))|>
  #ggplot2::ggplot()+
  #tidyterra::geom_spatvector(data =geo_pol_crop_alt, alpha = 0)+
  #tidyterra::geom_spatvector(ggplot2::aes(color = elevation))+
  #ggplot2::scale_color_viridis_c()



#ggplot2::ggsave(elev_points_map1, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/elev_points_map2v1.png",
#               height =  5,
#              width = 7.5,
#             dpi = 300, 
#            units = "in", 
#           bg = "white")

############################
# bin species with elevation
############################

elev_vect_ord <- c(
  "0-250",
  "250-500",
  "500-750",
  "750-1000",
  "1000-1250",
  "1250-1500",
  "1500-1750",
  "1750-2000",
  ">2000"
)


elev_bin_plot <- df_elev_epqs |>
  dplyr::mutate(
    elev_bin = dplyr::case_when(elevation < 250 ~ "0-250", .default = "ucat"),
    elev_bin = dplyr::case_when(elevation >= 250 &  elevation < 500 ~ "250-500", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 500 &  elevation < 750 ~ "500-750", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 750 &  elevation < 1000 ~ "750-1000", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 1000 &  elevation < 1250 ~ "1000-1250", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 1250 &  elevation < 1500 ~ "1250-1500", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 1500 &  elevation < 1750 ~ "1500-1750", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 1750 &  elevation < 2000 ~ "1750-2000", .default = elev_bin),
    elev_bin = dplyr::case_when(elevation >= 2000 ~ ">2000", .default = elev_bin),
    sp_name = paste0(Genus     ,"_", Species), 
    elev_bin = factor(elev_bin, levels = elev_vect_ord)
    
  )|>
  dplyr::distinct(elev_bin, sp_name)|>
  dplyr::group_by(elev_bin)|>
  dplyr::summarise(
    Total_elev = dplyr::n()
  )|>
  ggplot2::ggplot(ggplot2::aes(x=(elev_bin), y = Total_elev))+
  ggplot2::geom_bar( stat='identity',fill = "#56B4E9")+
  ggplot2::ylab("Species richness")+
  ggplot2::xlab("Elevation class")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1),
                 legend.position = "none")+
  ggplot2::scale_x_discrete(drop = FALSE) ## avoid to skip empty categories 

#ggplot2::ggsave(elev_bin_plot, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/elev_bin_plot02_accurate_v3.png",
 #               height =  5,width = 7.5,dpi = 300, units = "in",bg = "white")





####
### version final Ordenada por familias con eje X alturas!!!!
####

elev_means_02 <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(y = sp_name, color = sp_name,  x = elevation ))+
  ggplot2::ylab("Elevation")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::scale_x_continuous(breaks = c(0,250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250,2500, 2750))+
  
  ggplot2::theme(#axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none")

#ggplot2::ggsave(elev_means_02, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/elev_means_02_v4.png",
 #               height =  5,width = 7.5,dpi = 300, units = "in",bg = "white")



family_elves <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(y = Family, color = Family,  x = elevation ))+
  ggplot2::ylab("Family")+
  ggplot2::xlab("Elevation")+
  ggplot2::theme_bw()+
  ggplot2::scale_x_continuous(breaks = c(0,250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250,2500, 2750))+
  
  ggplot2::theme(#axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none")

#ggplot2::ggsave(family_elves, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/family_elves.png",
              #  height =  5,width = 7.5,dpi = 300, units = "in",bg = "white")



##################################
# Map 11. land cover per municipality
###################################

raw_landuse_cat<-terra::vect( "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/RJ - Uso de Tierra (250mil)/Rio_de_Janeiro.shp")

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

raw_landuse_cat|>
  terra::as.data.frame()|>
  dplyr::select(DESC_NI, DESC_NII)|>
  dplyr::distinct(DESC_NI, DESC_NII)

#land_use_map01 <-ggplot2::ggplot()+
 # tidyterra::geom_spatvector(data = dplyr::filter(raw_landuse_cat,DESC_NI!="Água") , ggplot2::aes(fill =DESC_NI), color = "transparent" )+
  #tidyterra::geom_spatvector(data = terra::project(vect_2, raw_landuse_cat))+
  #ggplot2::coord_sf(crs ="EPSG:4326", expand = FALSE)+
  #ggspatial::annotation_north_arrow(location= "br",
   #                                 pad_y = grid::unit(0.25, "in"),
    #                                width = grid::unit(1, "cm"))+
  #ggspatial::annotation_scale(location= "br")
  


#ggplot2::ggsave(land_use_map01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/land_use_map01v2.png",
 #               height = 5, width = 8, bg =  "white",dpi = 300)



municip<-terra::vect("data/gadm_brazil/gadm41_BRA_2.shp")
municip <-terra::subset(municip, 
                        municip$NAME_1 == "Rio de Janeiro", names(municip))

raw_landuse_cat|> 
  dplyr::mutate(  DESC_NI = dplyr::case_when(DESC_NI =="Água"~ "Litoral",  TRUE~ DESC_NI))|>
  as.data.frame()|>
  dplyr::pull(DESC_NI)|>
  unique()

coar_cats <- c("Áreas de Vegetação Natural",
               "Áreas Antrópicas Não Agrícolas",
               "Áreas Antrópicas Agrícolas" ,   
               "Outras Áreas",
               "Litoral")

#palette.colors(palette = "Okabe-Ito")
##         black        orange       skyblue   bluishgreen        yellow 
##     "#000000"     "#E69F00"     "#56B4E9"     "#009E73"     "#F0E442" 
##          blue    vermillion reddishpurple          gray 
##     "#0072B2"     "#D55E00"     "#CC79A7"     "#999999" 

land_use_map02 <- raw_landuse_cat|>
  dplyr::filter(DESC_NI!="Água") |> 
  dplyr::mutate(
    DESC_NI = factor(DESC_NI, levels = coar_cats)
  )|># get rid of ocean altogether
  #dplyr::mutate(DESC_NI = dplyr::case_when(DESC_NI =="Água"~ "Outras Áreas",  TRUE~ DESC_NI))|>
  ggplot2::ggplot()+
  tidyterra::geom_spatvector(#data = raw_landuse_cat, 
    ggplot2::aes(fill =DESC_NI), color = "transparent" )+
  tidyterra::geom_spatvector(data = terra::project(geo_pol_crop_alt, raw_landuse_cat), alpha = 0)+
  tidyterra::geom_spatvector(data = terra::project(municip, raw_landuse_cat), alpha = 0)+
  tidyterra::geom_spatvector(data = terra::project(vect_2, raw_landuse_cat), alpha = 0.25)+
  ggplot2::coord_sf(ylim= c(terra::ext(geo_pol_crop_alt)[3]-0.1, terra::ext(geo_pol_crop_alt)[4]),crs ="EPSG:4326", expand = FALSE)+
  ggplot2::theme_bw()+
  #ggplot2::theme(legend.position = "none")+
  ggspatial::annotation_north_arrow(location= "br",
                                    pad_y = grid::unit(0.25, "in"),
                                    width = grid::unit(1, "cm"))+
  ggspatial::annotation_scale(location= "br")+
  ggplot2::scale_fill_manual( name = "Land Use / Habitat",
                              values = c( 
     "Áreas de Vegetação Natural" =  "#009E73",
     "Áreas Antrópicas Agrícolas"    =   "#E69F00"  ,
     "Áreas Antrópicas Não Agrícolas" = "#F0E442" ,
       "Outras Áreas" =  "#CC79A7"
  ))

ggplot2::ggsave(land_use_map02,filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/land_use_map02v3.png",
                height = 8, width = 16,bg =  "white",dpi = 300)



###################################################
## Plots de frecuencia de Observaciones y especies
###################################################

# extract for coarse category of land use
landuse_L1 <-raw_landuse_cat[,"DESC_NI"]
landuse_L1_rast <- terra::rasterize(terra::project(landuse_L1, elev_rast), elev_rast, "DESC_NI" )
ocurr_landuse_L1 <- terra::extract(  landuse_L1_rast, terra::project(vect_2,landuse_L1_rast ) )
ocurr_landuse_L1<-dplyr::mutate(ocurr_landuse_L1,
  DESC_NI = dplyr::case_when(DESC_NI =="Água"~ "Outras Áreas",  TRUE~ DESC_NI)
)

#extract fine categories for land use
landuse_L2 <-raw_landuse_cat[,"DESC_NII"]
landuse_L2_rast <- terra::rasterize(terra::project(landuse_L2, elev_rast), elev_rast, "DESC_NII" )
ocurr_landuse_L2 <- terra::extract(  landuse_L2_rast, terra::project(vect_2,  landuse_L2_rast))


#merge both coarse and fine into vector file to match with species
vect_2 <- cbind(vect_2, ocurr_landuse_L1[,2], ocurr_landuse_L2[,2] )
names(vect_2) <- c("sp_name","Family", "DESC_NI", "DESC_NII")

vect_2<-dplyr::mutate(vect_2,
                  DESC_NII = dplyr::case_when(DESC_NII =="Áreas Descobertas"~ "Outras Áreas",  TRUE~ DESC_NII))

vect_2<-dplyr::mutate(vect_2,    
              DESC_NII = dplyr::case_when(DESC_NII =="Águas Costeiras"~ "Outras Áreas",  TRUE~ DESC_NII)
)

unique(vect_2$DESC_NII)



### stacked plot of fine habitat classes, for observations, within coarse land use class

land_use_plot01 <- vect_2|>
  terra::as.data.frame()|>
  dplyr::mutate(
    DESC_NI = factor(DESC_NI, levels = coar_cats)
  )|>
  ggplot2::ggplot( ) +
  ggplot2::geom_bar( ggplot2::aes(x = DESC_NI, fill =  DESC_NII))+ 
  
  ggplot2::xlab("Habitat (coarse)")+
  ggplot2::ylab("Observations")+
  ggplot2::theme_bw()+  
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                 #legend.position = "none"
                 )+
  ggplot2::scale_fill_manual( name = "Land Use / Habitat",
                              values = c( 
                                "Área Florestal" =  "#009E73",
                                "Culturas Temporárias" = "#E69F00",
                                "Áreas Urbanizadas"= "#F0E442",
                                "Pastagens"= "#999999"  ,
                                "Área Campestre" = "#D55E00"    , 
                                "Outras Áreas" =  "#CC79A7",
                                "Litoral" =  "#56B4E9"
                                
                                #"Áreas de Vegetação Natural" =  "#009E73",
                                #"Áreas Antrópicas Agrícolas"    =   "#E69F00"  ,
                                #"Áreas Antrópicas Não Agrícolas" = "#F0E442" ,
                                #"Outras Áreas" =  "#CC79A7"#,
                                #"Litoral" =  "#56B4E9" 
                              ))


ggplot2::ggsave(land_use_plot01,filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/land_use_plot01_COARSEv3.png",
              height = 5, width = 8, bg =  "white",dpi = 300)








### RICHNESS in land use Coarse Categories
rich_landuse_Coarse <- xx|>
  terra::as.data.frame()|>
 
  dplyr::group_by(sp_name, DESC_NI)|>
  dplyr::distinct(DESC_NI, sp_name)|>
  dplyr::mutate(
    DESC_NI = factor(DESC_NI, levels = coar_cats)
    
  )|>
      ggplot2::ggplot( ) +
        ggplot2::geom_bar( ggplot2::aes(x = DESC_NI, fill =  DESC_NI))+
        ggplot2::xlab("Habitat (coarse)")+
        ggplot2::ylab("Species Richness")+
        ggplot2::theme_bw()+ 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                 legend.position = "none"
                 )+
  ggplot2::scale_fill_manual( name = "Land Use / Habitat",
                              values = c( 
                                
                               # "Área Florestal" =  "#009E73",
                                #"Culturas Temporárias" = "#E69F00",
                                #"Áreas Urbanizadas"= "#F0E442",
#                                "Pastagens"= "#999999"  ,
 #                               "Área Campestre" = "#D55E00"    , 
  #                              "Outras Áreas" =  "#CC79A7",
   #                             "Litoral" =  "#56B4E9"
                                
                              "Áreas de Vegetação Natural" =  "#009E73",
                              "Áreas Antrópicas Agrícolas"    =   "#E69F00"  ,
                              "Áreas Antrópicas Não Agrícolas" = "#F0E442" ,
                              "Outras Áreas" =  "#CC79A7"#,
                              #"Litoral" =  "#56B4E9" 
                              ))

ggplot2::ggsave(rich_landuse_Coarse,filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/rich_landuse.png",
                height = 5, width =8, bg =  "white",dpi = 300)


#check which fall in water
#ggplot2::ggplot( ) +
 # tidyterra::geom_spatvector(data = terra::project(geo_pol_crop2,vect_2))+
  #tidyterra::geom_spatvector(data = terra::subset(vect_2, vect_2$DESC_NII %in% c("Águas Continentais", "Águas Costeiras"),
                                        #          "DESC_NII"), 
   #                          ggplot2::aes(color = DESC_NII))+
  #ggplot2::scale_color_viridis_d()

## RICHNESS species fine land use categories

vect_2|> 
  #dplyr::mutate(  DESC_NI = dplyr::case_when(DESC_NI =="Água"~ "Litoral",  TRUE~ DESC_NI))|>
  as.data.frame()|>
  dplyr::pull(DESC_NII)|>
  unique()

fine_cats <- c("Área Florestal", #1
               "Área Campestre" ,#1
               "Áreas Urbanizadas" ,#2 
               "Culturas Temporárias",#3
               "Pastagens", #3
               "Outras Áreas")


land_use_RICH_plot02 <- vect_2|>
  terra::as.data.frame()|>
  dplyr::mutate(
    # DESC_NI = dplyr::case_when(DESC_NI =="Água"~ "Outras Áreas",  TRUE~ DESC_NI)
    DESC_NII = factor(DESC_NII, levels = fine_cats),
    DESC_NI = factor(DESC_NI, levels = coar_cats)
    
  )|>
  dplyr::group_by(sp_name, DESC_NII, DESC_NI)|>
  dplyr::distinct(DESC_NII, sp_name)|>
  ggplot2::ggplot( ) +
  ggplot2::geom_bar(ggplot2::aes(x = DESC_NII, fill = DESC_NI) )+
  ggplot2::scale_color_viridis_b()+
 
  ggplot2::ylab("Species Richness")+
  ggplot2::xlab("Habitat (fine)")+
  ggplot2::scale_fill_manual( name = "Land Use / Habitat (coarse)",
                              values = c( 
                                
                                # "Área Florestal" =  "#009E73",
#                                "Culturas Temporárias" = "#E69F00",
 #                               "Áreas Urbanizadas"= "#F0E442",
  #                                 "Pastagens"= "#999999"  ,
   #                                 "Área Campestre" = "#D55E00"    , 
    #                                "Outras Áreas" =  "#CC79A7"
                                #"Litoral" =  "#56B4E9"
                               "Áreas de Vegetação Natural" =  "#009E73",
                                "Áreas Antrópicas Agrícolas"    =   "#E69F00"  ,
                                "Áreas Antrópicas Não Agrícolas" = "#F0E442" ,
                                "Outras Áreas" =  "#CC79A7"#,
                                #"Litoral" =  "#56B4E9" 
                              ))+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                 #legend.position = "none"
                 )

ggplot2::ggsave(land_use_RICH_plot02,
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/land_use_RICH_FINEv03.png",
                height = 5, width = 8, 
                bg =  "white",
                dpi = 300)


land_use_sppFINE_COARSE <- vect_2|>
  terra::as.data.frame()|>
  #dplyr::mutate(
   # DESC_NII = dplyr::case_when(DESC_NII =="Águas Costeiras"~ "Outras Áreas",  TRUE~ DESC_NII)
  #)|>
  dplyr::group_by(DESC_NI,  DESC_NII)|>
  dplyr::distinct(DESC_NI, DESC_NII)|>
  dplyr::arrange(DESC_NII)

write.csv(land_use_sppFINE_COARSE, "data/land_use_sppFINE_COARS.csv") 

#two_plot_landuse <- cowplot::plot_grid(land_use_plot02, 
#                              land_use_map02,
#                             ncol = 2, nrow = 1
#)

#second_plot_landuse <- cowplot::plot_grid(NULL, land_use_plot01, NULL,
#                             nrow = 1, ncol=3, 
#                            rel_widths = c(0.25, 0.75, 0.25)
#                           )




### ### ### ### ### ### ### ### ### PENDING
### temperature viz
### ### ### ### ### ### ### ### ### ### ### ### 



temp_rast <- terra::rast("C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/data/chelsa_brasil/CHELSA_bio1_1981-2010_V.2.1.tiff")
ggplot2::ggplot( ) +
  tidyterra::geom_spatraster(data = terra::crop(temp_rast, terra::project(raw_landuse_cat, temp_rast)))+
  tidyterra::geom_spatvector(data = vect_2, alpha = 0.25)+
  ggplot2::scale_colour_viridis_c()


temp_rast_repoj <- terra::project(temp_rast, "EPSG:31983")
temp_rast_repoj<-terra::crop(temp_rast_repoj, terra::project(vect_2, temp_rast_repoj))
#temp_rast_repoj<- terra::aggregate(temp_rast_repoj, 70)

temp_map01 <- ggplot2::ggplot()+
  tidyterra::geom_spatraster(data = temp_rast_repoj)+
  ggplot2::scale_fill_viridis_c(option = "E", na.value = "transparent")+
  tidyterra::geom_spatvector(
    data = vect_2, alpha = 0.5
  )+
  #ggnewscale::new_scale_fill()+
  tidyterra::geom_spatvector(data = geo_pol_crop_alt, #ggplot2::aes(fill =NAME_1 ), 
                             alpha = 0)+
  ggplot2::coord_sf(crs ="EPSG:4326", expand = FALSE)


  
ggplot2::ggsave(temp_map01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/temp_map01v2.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")







############################
# bin species with Temperature
############################
temp_points <- terra::extract(  temp_rast_repoj,terra::project(vect_2, temp_rast_repoj) )

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
  ggplot2::theme_bw()+
  ggplot2::xlab("Temperature")+
  ggplot2::ylab("Frequency of Observations")


ggplot2::ggsave(temp_bin_plot, 
                filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/temp_bin_plot01v2.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

temp_means_01 <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(y = sp_name, color = sp_name,  x = temp ))+
  ggplot2::ylab("Species")+
  ggplot2::xlab("Temperature")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(temp_means_01, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/temp_means_01.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")

temp_means_02 <- df_elev_epqs |>
  dplyr::mutate(
    Genus = as.factor(Genus),
    Species = as.factor(Species),
    sp_name = paste0(Genus,"_", Species),
    sp_name = factor(sp_name, levels = vector_order)
  )|>
  
  ggplot2::ggplot()+
  ggplot2::geom_boxplot( ggplot2::aes(x = Family, color = Family,  y = temp ))+
  ggplot2::ylab("Temperature")+
  ggplot2::xlab("Species")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                 legend.position = "none")

ggplot2::ggsave(temp_means_02, filename = "C:/Users/franc/Documents/Research/Brazil - julio insectos/mantodea_project_BR/plots/temp_means_02.png",
                height =  5,
                width = 7.5,
                dpi = 300, 
                units = "in", 
                bg = "white")