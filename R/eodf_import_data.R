#' @name eodf_import_data
#' @aliases eodf_import_data
#' @title Import the data in R
#' @description Function to import a data / time series that was downloaded with one of the packages \code{opendapr}, \code{getremotedata} or \code{shub4r}
#'
#' @param df_data_to_import data.frame. Contains the path to the data that were downloaded. Typically output of \code{odr_get_url}, \code{grd_get_url} or \code{shr_get_url}. See Details for the structure
#' @param collection character string the name of a collection
#' @param variable character string the name of a variables
#' @param roi sf the ROI. Mandatory for SMAP collection, else, not necessary
#' @param output character string the output. "RasterBrick" or "stars". At not stars is implemented only for MODIS and VIIRS collections
#'
#' @import raster stars ncdf4 purrr magrittr
#'
#' @export

eodf_import_data <- function(df_data_to_import,
                            collection,
                            variable = NULL,
                            roi = NULL,
                            output = "RasterBrick"){

  rasts <- . <- smap_sp_bound <- NULL

  odap_coll_info <- opendapr:::opendapMetadata_internal[which(opendapr:::opendapMetadata_internal$collection==collection),]

  if(nrow(odap_coll_info)>0){ # data imported with opendapr
  if(output=="stars" && (collection=="GPM" || collection=="SMAP")){stop("stars output is not yet available for SMAP and GPM collections")}
  if(is.null(roi) && collection=="SMAP"){stop("for SMAP collections you must provide the ROI")}
  if(is.null(variable) && output=="RasterBrick"){stop("for RasterBrick output you must provide one variables")}
  if(!is.null(variable) && length(variable)>1){stop("you must provide only one variable")}
  if(!("destfile" %in% colnames(df_data_to_import))){stop("df_data_to_import must contain a 'destfile' column")}
  if(!("time_start" %in% colnames(df_data_to_import)) && (collection=="GPM" || collection=="SMAP")){stop("for SMAP and GPM collections, df_data_to_import must contain a 'time_start' column")}

  # get the variables
  #variables_available <- names(ncdf4::nc_open(df_data_to_import$destfile[1])$var)
  #if(is.null(variable)){
  #  variable <- variables_available
  #}


  if(odap_coll_info$source %in% c("MODIS","VIIRS")){

    if (odap_coll_info$provider=="NASA USGS LP DAAC"){

      if(output=="RasterBrick"){
        if(length(df_data_to_import$destfile)==1){
          #rasts <- expand.grid(df_data_to_import$destfile,variables,stringsAsFactors = F) %>%    # to import multiple variables at once
          #  purrr::map2(.x=.$Var1,.y=.$Var2, .f=~brick(.x,varname=.y,crs=odap_coll_info$crs)) %>%
          #  set_names(variables)
          rasts <- df_data_to_import$destfile %>%    # multiple variables
            brick(.,varname=variable,crs=odap_coll_info$crs)
        } else {  # case of multiple modis tiles
          rasts <- df_data_to_import$destfile %>%
            purrr::map(~raster::brick(.,varname=variable,crs=odap_coll_info$crs)) %>%
            do.call(merge,.)
          }
      } else if (output=="stars"){
        if(length(df_data_to_import$destfile)==1){
          rasts <- df_data_to_import$destfile %>%
            stars::read_stars(.) %>%
            st_set_crs(odap_coll_info$crs)

        } else {
          stop("stars output is not implemented yet for multiple tiles")
          #TODO : stars output format for several modis tiles
          #rasts <- df_data_to_import$destfile %>%
          #  map(~stars::read_stars(.)) %>%
          #  map(~st_set_crs(.,odap_coll_info$crs)) %>%
          #  do.call(c,.)
        }

    }
      } else if (odap_coll_info$provider=="NASA LAADS DAAC"){
      if(output=="RasterBrick"){
        rasts <- df_data_to_import$destfile %>%
          purrr::map(~raster(., varname = variable,crs = odap_coll_info$crs)) %>%
          raster::brick()
        names(rasts) <- df_data_to_import$time_start
      } else if (output=="stars"){
        stop("stars output is not implemented yet for this collection")
      }
    }

  } else if (odap_coll_info$source=="GPM"){
    rasts <- df_data_to_import$destfile %>%
      purrr::map(~raster(., varname = variable,crs = odap_coll_info$crs)) %>%
      raster::brick() %>%
      raster::t() %>%
      raster::flip("y") %>%
      raster::flip("x")
    names(rasts) <- df_data_to_import$time_start

    #a<-stars::read_stars(df_data_to_import$destfile) %>%
    #  st_set_crs(odap_coll_info$crs) %>% t() %>% flip("y")

  } else if (odap_coll_info$source=="SMAP"){
    smap_sp_bound <- opendapr::odr_get_opt_param(roi = roi, collection = collection)$roiSpatialBound$`1`
    rasts <- df_data_to_import$destfile %>%
      purrr::map(~ncdf4::nc_open(.)) %>%
      purrr::map(~ncdf4::ncvar_get(., variable)) %>%
      purrr::map(~raster(t(.), ymn=smap_sp_bound[1], ymx=smap_sp_bound[2], xmn=smap_sp_bound[3], xmx=smap_sp_bound[4], crs=odap_coll_info$crs)) %>%  # EPSG : 6933
      raster::brick()
    names(rasts) <- df_data_to_import$time_start

}
  } else {  # data imported with getremotedata


   if (collection=="SRTM"){
     rasts <- df_data_to_import$destfile %>%
      map(~unzip(., exdir = dirname(.))) %>%
      map(~raster(.)) %>%
      do.call(merge,.) %>%
      crop(roi)
  } else if (collection=="TAMSAT"){
    rasts <- df_data_to_import$destfile %>%
      map(~raster(.)) %>%
      map(~crop(.,roi)) %>%
      brick()
    names(rasts) <- df_data_to_import$time_start
  } else if (collection=="VIIRS_DNB_MONTH"){
    rasts <- df_data_to_import$destfile %>%
      map(~raster(.)) %>%
      brick(.)
    names(rasts) <- df_data_to_import$time_start
  } else if (collection=="ERA5"){
    rasts <- df_data_to_import$destfile %>%
      map(~raster(., varname = variable)) %>%
      brick(.)
    names(rasts) <- df_data_to_import$time_start
  } else if (collection=="MIRIADE"){
    rasts <- df_data_to_import$destfile %>%
      map(~read.csv(.,skip=10))
  }

  }


  return(rasts)


}
