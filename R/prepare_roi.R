#' @name prepare_roi
#' @aliases prepare_roi
#' @title Prepare the roi for the rest of the workflow
#'
#' @param df_points_metadata data.frame. Contains the input sampling points. Expected format is given in the details
#' @param buffer_sizes vector of buffer sizes
#'
#' @import sf
#' @importFrom magrittr %>%
#' @export

prepare_roi <- function(df_points_metadata,buffer_sizes){

  roi <- sf::st_as_sf(df_points_metadata, coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_bbox()

  mean_roi_latitude<-mean(c(roi$ymin,roi$ymax))
  roi[1]<-roi[1]-0.05-.convertMetersToDegrees(max(buffer_sizes),mean_roi_latitude) #xmin
  roi[2]<-roi[2]-0.05-.convertMetersToDegrees(max(buffer_sizes),mean_roi_latitude) #ymin
  roi[3]<-roi[3]+0.05-.convertMetersToDegrees(max(buffer_sizes),mean_roi_latitude) #xmin
  roi[4]<-roi[4]+0.05-.convertMetersToDegrees(max(buffer_sizes),mean_roi_latitude) #ymin

  roi <- roi %>%
    sf::st_as_sfc() %>%
    sf::st_sf()

  return(roi)
}
