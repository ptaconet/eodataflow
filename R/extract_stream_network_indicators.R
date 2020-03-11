#' @name extract_stream_network_indicators
#' @aliases extract_stream_network_indicators
#' @title
#' @description
#'
#' @param path_to_stream_network
#' @param path_to_accumulation_raster
#' @param sf_points_metadata
#' @param buffer_sizes
#'
#' @details
#'
#' Extract indices related to the stream network : WAD (Average distance to stream), WMD (Distance to closest stream), WLS (Total length of stream), WAL (Accumulation / distance to sampling point - that is, distance from the hydrographic network to the HLC point weighted by the acculumation value at the stream's location)
#'
#' @importFrom  rgeos gCentroid
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @import sf raster furrr purrr dplyr
#' @export
#'
extract_stream_network_indicators <- function(path_to_stream_network, path_to_accumulation_raster, sf_points_metadata, buffer_sizes){

# Static (non time-series) data: function to extract a summary of line (vector) variable over 1 buffer
.extractVar_line_singleBuff<-function(network_sf,sfPoints,buffer_size){

  buffer<-st_buffer(sfPoints,dist=buffer_size) # Create the buffer around each point
  sfPoints <- sfPoints %>%
    rename(geometry_pt=geometry) # rename point geometry column, else an error is sent back on the pipe flow

  res<-st_join(buffer,network_sf, join = st_intersects,left = TRUE) %>% # Get the lines inside each buffer
    left_join(as.data.frame(network_sf)) %>%  # get the line geometry
    rename(geometry_line=geom) %>% # rename line geometry column to be more clear
    left_join(as.data.frame(sfPoints),by="id")  %>%  # get the point geometry
    mutate(dist_line_to_pt=st_distance(geometry_pt,geometry_line,by_element = T)) %>% # get the distance between each point and line
    st_drop_geometry() %>%
    dplyr::select(id,length_stream,dist_line_to_pt,accumulation) %>%
    group_by(id) %>%
    summarise(mean_dist_to_stream=as.numeric(mean(dist_line_to_pt)),min_dist_to_stream=as.numeric(min(dist_line_to_pt)),length_stream=as.numeric(sum(length_stream)),mean_acc_by_dist=as.numeric(mean(accumulation/dist_line_to_pt))) %>% # compute stats related to hydrographic network
    mutate(mean_dist_to_stream=na_if(mean_dist_to_stream,0)) %>%
    mutate(min_dist_to_stream=na_if(min_dist_to_stream,0)) %>%
    mutate(length_stream = if_else(is.na(length_stream), 0, length_stream)) %>%
    mutate(mean_acc_by_dist = if_else(is.na(mean_acc_by_dist), 0, mean_acc_by_dist))


  res <- res %>%
    mutate(buffer=buffer_size) %>%
    gather(var,val,-c(id,buffer))
  return(res)
}

# open stream network
streams_network_sf<-sf::read_sf(path_to_stream_network) %>%
  st_zm() %>%
  mutate(length_stream=st_length(.)) %>%
  mutate(DN=seq(1,nrow(.),1))  %>%
  dplyr::select(DN,length_stream,geom)

# Get the accumulation value on each piece of the network. For this, take the centroid of each piece of network and calculate the accumulation on this piece.
accumulation<-raster(path_to_accumulation_raster)
accumulation<-accumulation*res(accumulation)[1]*res(accumulation)[2]/10000 # convert accumulation from number of pixels to surface in ha

sp_cent <- rgeos::gCentroid(as(streams_network_sf, "Spatial"), byid=TRUE)
streams_network_sf<-raster::extract(accumulation,sp_cent,df=TRUE) %>%
  dplyr::select(-ID) %>%
  mutate(DN=streams_network_sf$DN) %>%
  full_join(streams_network_sf,.)

# Convert the points to UTM projection
sfPoints_utm<-st_transform(sf_points_metadata,.getUTMepsg(sf_points_metadata))

# Calculate the stats for all buffers
WAD_WMD_WLS_WAL<-buffer_sizes %>%
  purrr::set_names() %>%
  future_map_dfr(~.extractVar_line_singleBuff(streams_network_sf,sfPoints_utm,.))

WAD_WMD_WLS_WAL <- WAD_WMD_WLS_WAL %>%
  mutate(var=stringr::str_replace_all(var,c("mean_dist_to_stream"="WAD","min_dist_to_stream"="WMD","length_stream"="WLS","mean_acc_by_dist"="WAL"))) %>%
  mutate(qval=1)

return(WAD_WMD_WLS_WAL)

}
