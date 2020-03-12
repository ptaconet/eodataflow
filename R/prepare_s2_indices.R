#' @name s2_create_rast_indices_one_date
#' @aliases s2_create_rast_indices_one_date
#' @title Create spectral indices from S2 data downloaded with get_url. Invalid pixels are set to NA.
#' @description fonction pour calculer les indices spectraux (spectral_indices) pour 1 date donnée, à partir d'un vecteur de fichiers de destination (destfiles)
#'
#' @param destfiles character vector. vector of destination files (e.g. c("korhogo/S2L2A/S2L2A_20170212_9_SCENE_CLASSIFICATION__p1.tif","korhogo/S2L2A/S2L2A_20170212_B04__p1.tif"))
#' @param variables character vector. vector of bands (e.g. c("BO4","B08"))
#' @param spectral_indices character vector. vector of spectral indices to calculate (e.g. c("ndvi","mndvi"))
#'
#' @return
#' a list with the spectral indices for the date, where invalid pixels (e.g. cloud, shadow etc.) have been set to NA
#'
#' @details
#' param destfile must contain the band "9_SCENE_CLASSIFICATION"
#' param variables must contain the useful bands to compute the spectral indices provideds
#'
#'
#' @noRd

## fonction pour calculer les indices spectraux (spectral_indices) pour 1 date donnée, à partir d'un vecteur de fichiers de destination (destfiles)
s2_create_rast_indices_one_date <- function(destfiles,variables,spectral_indices){

  variables <- setdiff(variables,"9_SCENE_CLASSIFICATION")
  # create the rasters (1/band)
  raw_rasts <- variables %>%
    map(.,~grep(.,destfiles,value = T)) %>%
    set_names(variables) %>%
    map(.,~map(.,~raster(.))) %>%
    map(.,~raster::merge(x=.[[1]],y=.[[2]],tolerance=0.15))

  # set bad quality pixels in the scene classification to NA
  brick_bad_q_pixels <- "9_SCENE_CLASSIFICATION" %>%
    grep(.,destfiles,value = T) %>%
    map(.,~brick(.)) %>%
    raster::merge(x=.[[1]],y=.[[2]],tolerance=0.15)

  NAvalue(brick_bad_q_pixels) <- -1 # by default R set value 65535 to NA when reading the raster. We set it to -1 because we need to use the 65535 value (above)

  cloud_high_proba <- c(65535,65535,65535) # combinaison de pixels dans les bandes 1, 2 et 3 pour les pixels de mauvaise qualité
  cloud_medium_proba <- c(49344,49344,49344)
  thin_cirrus <- c(25700,51400,65535)
  dark_feature_shadow <- c(12079,12079,12079)
  cloud_shadow <- c(25700,12850,0)
  unclassifed <- c(32896,32896,32896)

  # on set à NA les pixels de mauvaise qualité (cloud high proba, cloud medium proba, etc.)
  # workaround... quite ugly but working
  rast_bad_q_pixels <- sum(brick_bad_q_pixels)
  bad_q_pix_val <- c(sum(cloud_high_proba),sum(cloud_medium_proba),sum(thin_cirrus),sum(dark_feature_shadow),sum(cloud_shadow),sum(unclassifed))
  rast_bad_q_pixels[rast_bad_q_pixels %in% bad_q_pix_val] <- NA

  # calculate indices and remove bad pixels
  list_indices <- list()
  if("ndvi" %in% spectral_indices){
    ind <- (raw_rasts$B08 - raw_rasts$B04) / (raw_rasts$B08 + raw_rasts$B04) %>% raster::mask(rast_bad_q_pixels) %>% magrittr::set_names("ndvi")
    list_indices <- c(list_indices,ind)
    names(list_indices) <- c(names(list_indices[1:length(list_indices)-1]),"ndvi")
  }
  if("mndvi" %in% spectral_indices){
    ind <- (raw_rasts$B08 - raw_rasts$B11) / (raw_rasts$B08 + raw_rasts$B11) %>% raster::mask(rast_bad_q_pixels) %>% magrittr::set_names("mndvi")
    list_indices <- c(list_indices,ind)
    names(list_indices) <- c(names(list_indices[1:length(list_indices)-1]),"mndvi")
  }
  if("ndwi" %in% spectral_indices){
    ind <- (raw_rasts$B03 - raw_rasts$B08) / (raw_rasts$B03 + raw_rasts$B08) %>% raster::mask(rast_bad_q_pixels) %>% magrittr::set_names("ndwi")
    list_indices <- c(list_indices,ind)
    names(list_indices) <- c(names(list_indices[1:length(list_indices)-1]),"ndwi")
  }
  if("mndwi" %in% spectral_indices){
    ind <- (raw_rasts$B03 - raw_rasts$B11) / (raw_rasts$B03 + raw_rasts$B11) %>% raster::mask(rast_bad_q_pixels) %>% magrittr::set_names("mndwi")
    list_indices <- c(list_indices,ind)
    names(list_indices) <- c(names(list_indices[1:length(list_indices)-1]),"mndwi")
  }
  if("bri" %in% spectral_indices){ # bri stands for brightness
    ind <- sqrt(raw_rasts$B03^2 + raw_rasts$B04^2 + raw_rasts$B08^2 + raw_rasts$B11^2) %>% raster::mask(rast_bad_q_pixels) %>% magrittr::set_names("bri") # formule ici : http://agritrop.cirad.fr/583973/1/remotesensing-09-00259.pdf
    list_indices <- c(list_indices,ind)
    names(list_indices) <- c(names(list_indices[1:length(list_indices)-1]),"bri")
  }

  return(list_indices)

}


#' @name prepare_s2_indices
#' @aliases prepare_s2_indices
#' @title Create valid spectral indices from S2 data averaged over a given time period.
#' @description
#'
#' @param th_list_of_url data.frame, output of shub4r::shr_get_url()
#' @param variables character vector. vector of bands (e.g. c("BO4","B08"))
#' @param spectral_indices character vector. vector of spectral indices to compute (e.g. c("ndvi","mndvi"))
#' @param verbose verbose
#'
#' @return
#' a rasterBrick with the spectral indices (1 band / indice).
#'
#' @details
#' param th_list_of_url is the output of shub4r::shr_get_url. It must contain the band "9_SCENE_CLASSIFICATION"
#'
#' @import furrr
#' @export

prepare_s2_indices <- function(th_list_of_url,variables,spectral_indices){

  # pour 1 élément de list_of_url, retourne les indices spectraux pour chaque date puis flatten la liste
  rasts_indices <- th_list_of_url %>%
   dplyr::group_split(time_start) %>% #split th_list_of_url par date
   future_map(~s2_create_rast_indices_one_date(.$destfile,variables,spectral_indices)) %>% # applique la fonction pour calculer les indices spectraux
   flatten()

 # une autre manière de faire la même chose (mais cette fois ci en conservant les dates)
 # list_of_url_split <- th_list_of_url %>%
 #   dplyr::group_split(time_start)
 #
 # rasts_indices <- list_of_url_split %>%
 #   map(~s2_create_rast_indices_one_date(.,variables)) %>%
 #   set_names(map_chr(list_of_url_split,~unique(.$time_start)))

 #  pour 1 élément de list_of_url : indices spectraux sous forme de rasterBrick, moyennés sur toute la période
  indices_rasterstack_period <- spectral_indices %>%
    map(.,~keep(rasts_indices,names(rasts_indices)==.)) %>%
    map(.,~raster::stack(.))

  if(nlayers(indices_rasterstack_period[[1]]) > 1){
  indices_rasterstack_period <- indices_rasterstack_period %>%
    map(.,~calc(., fun = mean, na.rm = T))
  }

  indices_rasterstack_period <- indices_rasterstack_period %>%
    raster::brick(.) %>%
    magrittr::set_names(spectral_indices)

  return(indices_rasterstack_period)

}
