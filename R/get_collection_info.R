#' @name .get_collection_info
#' @title Get the information for a given collection
#' @noRd


.get_collection_info <- function(collection){

  odr_coll_info <- shr_coll_info <- grd_coll_info <- NULL

  odr_coll_info <- opendapr:::opendapMetadata_internal %>% dplyr::select(collection,source,provider,login) %>% dplyr::mutate(package="opendapr")
  shr_coll_info <- shub4r:::shrMetadata_internal %>% dplyr::select(collection,source,provider,login) %>% dplyr::mutate(package="shub4r")
  grd_coll_info <- getremotedata:::grdMetadata_internal %>% dplyr::select(collection,source,provider,login) %>% dplyr::mutate(package="getremotedata")

  coll_info <- rbind(odr_coll_info,shr_coll_info,grd_coll_info)
  coll_info <- coll_info[which(coll_info$collection==collection),]

  if(nrow(coll_info)==0){stop("the collection that you specified does not exist")}

  return(coll_info)
}
