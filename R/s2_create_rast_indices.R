


## fonction pour calculer les indices spectraux pour 1 date, à partir de l'objet th_list_of_urls

s2_create_rast_indices_one_date <- function(th_list_of_url_th_time,time,variables){

  # filter th_list_of_urls to keep only this time start
  #th_list_of_urls_th_time <- th_list_of_urls %>%
  #  filter(time_start == time)

  variables <- setdiff(variables,"9_SCENE_CLASSIFICATION")
  # create the rasters (1/band)
  raw_rasts <- variables %>%
    map(.,~grep(.,th_list_of_urls_th_time$destfile,value = T)) %>%
    set_names(variables) %>%
    map(.,~map(.,~raster(.))) %>%
    map(.,~raster::merge(x=.[[1]],y=.[[2]],tolerance=0.15))

  # calculate indices
  ndvi <- (raw_rasts$B04 - raw_rasts$B08) / (raw_rasts$B04 + raw_rasts$B08)
  mndvi <- (raw_rasts$B08 - raw_rasts$B11) / (raw_rasts$B08 + raw_rasts$B11)

  # set bad quality pixels in the scene classification to NA
  brick_bad_q_pixels <- "9_SCENE_CLASSIFICATION" %>%
    grep(.,th_list_of_urls_th_time$destfile,value = T) %>%
    map(.,~brick(.)) %>%
    raster::merge(x=.[[1]],y=.[[2]],tolerance=0.15)

  cloud_high_proba <- c(65535,65535,65535) # combinaison de pixels dans les bandes 1, 2 et 3 pour les pixels de mauvaise qualité
  cloud_medium_proba <- c(49344,49344,49344)
  thin_cirrus <- c(25700,51400,65535)
  dark_feature_shadow <- c(12079,12079,12079)
  cloud_shadow <- c(25700,12850,0)
  unclassifed <- c(32896,32896,32896)

  bad_q_pix_list <- list(cloud_high_proba,cloud_medium_proba,thin_cirrus,dark_feature_shadow,cloud_shadow,unclassifed)

  rast_bad_q_pixels <- brick_bad_q_pixels[[1]] #initialisation du raster
  # on set à NA les pixels de mauvaise qualité (cloud high proba, cloud medium proba, etc.)
  for(i in 1:length(bad_q_pix_list)){
      rast_bad_q_pixels[brick_bad_q_pixels[[1]] == bad_q_pix_list[[i]][1] & brick_bad_q_pixels[[2]] == bad_q_pix_list[[i]][2] & brick_bad_q_pixels[[3]] == bad_q_pix_list[[i]][3]] <- NA
    }

  # remove bad pixels
  ndvi <- mask(ndvi,rast_bad_q_pixels)
  mndvi <- mask(mndvi,rast_bad_q_pixels)

  return(list(ndvi=ndvi,mndvi=mndvi))

}


### pour toutes les dates :

