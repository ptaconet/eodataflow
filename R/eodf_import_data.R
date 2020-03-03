#' @name eodf_import_data
#' @aliases eodf_import_data
#' @title Import the data in R
#' @description Function to import a data / time series that was downloaded with one of the packages \code{opendapr}, \code{getremotedata} or \code{shub4r}
#'
#' @param df_data_to_import data.frame. Contains the path to the data that were downloaded. Typically output of \code{odr_get_url}, \code{grd_get_url} or \code{shr_get_url}. See Details for the structure
#' @param collection character string the name of a collection
#' @param variable character string the name of a variables
#' @param roi sf the ROI. Mandatory for SMAP collection, else, not necessary
#' @param output character string the output. "RasterBrick" or "stars". At now stars is implemented only for MODIS and VIIRS collections
#'
#' @export

eodf_import_data <- function(df_data_to_import,
                            collection,
                            variable = NULL,
                            roi = NULL,
                            output = "RasterBrick"){

  rasts <- coll_info <- NULL

  coll_info <- .get_collection_info(collection)

  if(coll_info$package=="opendapr"){
    rast <- opendapr::odr_import_data(df_data_to_import, collection, variable, roi, output)
  } else if (coll_info$package=="getremotedata"){
    rast <- getremotedata::grd_import_data(df_data_to_import, collection, variable, roi, output)
  } else if (coll_info$package=="shub4r"){
    rast <- shub4r::shr_import_data(df_data_to_import, collection, variable, roi, output)
  }

  return(rasts)


}
