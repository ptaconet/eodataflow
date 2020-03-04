#' @name prepare_df_points
#' @aliases prepare_df_points
#' @title Prepare the input data.frame of sampling points for the rest of the workflow
#'
#' @param df_points_metadata data.frame. Contains the input sampling points. Expected format is given in the details
#' @param lag_time integer. lagged time (in days) for which data will be extracted from the time series collections
#'
#' @details
#'
#' Expected format for input data.frame \code{df_points_metadata} is the following :
#' \describe{
#' \item{id}{character string. unique identifier of the sampling point}
#' \item{date}{sampling date (character string) }
#' \item{longitude}{longitude}
#' \item{latitude}{latitude}
#' }
#'
#' @importFrom tidyr nest
#' @importFrom sf st_as_sf
#' @import dplyr
#' @import purrr
#'
#' @export

prepare_df_points <- function(df_points_metadata){

  sampling_points<-df_points_metadata %>%
    mutate(date=as.Date(date)) %>%
    group_by(date) %>%
    arrange(date) %>%
    tidyr::nest(coords=c(latitude,longitude,id)) %>%
    mutate(sf_points=map(coords,~sf::st_as_sf(.,coords = c("longitude", "latitude"), crs = 4326))) %>%
    dplyr::select(-coords) %>%
    mutate(date_numeric=as.integer(date))

  return(sampling_points)

}
