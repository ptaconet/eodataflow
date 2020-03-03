#' @name .getUTMepsg
#' @title Get UTM epsg
#'
#' @import sf
#' @noRd

.getUTMepsg<-function(roi){

  bbox<-sf::st_bbox(roi)
  utm_zone_number<-(floor((bbox$xmin + 180)/6) %% 60) + 1
  if(bbox$ymin>0){ # if latitudes are North
    epsg<-as.numeric(paste0("326",utm_zone_number))
  } else { # if latitude are South
    epsg<-as.numeric(paste0("325",utm_zone_number))
  }

  return(epsg)
}

#' @name .convertMetersToDegrees
#' @title Convert length in meters to length in degrees
#'
#' @noRd
.convertMetersToDegrees<-function(length_meters,
                                  latitude_4326){

  length_degrees <- length_meters / (111.32 * 1000 * cos(latitude_4326 * ((pi / 180))))

  return(length_degrees)
}

#' @name .resample_rast
#' @title spatially resample a raster (using bilinear interpolation)
#'
#' @noRd
.resample_rast<-function(rast,resample_output_res){
  resample_output_res<-.convertMetersToDegrees(resample_output_res,latitude_4326=mean(c(extent(rast)[3],extent(rast)[4])))
  r<-rast
  res(r)<-resample_output_res
  rast<-raster::resample(rast,r,method='bilinear')
}
