#' @name eodf_geturl_dl
#' @aliases eodf_geturl_dl
#' @title Get URL and download
#' @description
#'
#' @param sampling_points
#' @param roi
#' @param roi_name
#' @param collection
#' @param variables
#' @param verbose
#'
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @export


eodf_geturl_dl <- function(sampling_points,
                           roi,
                           roi_name,
                           collection,
                           variables,
                           verbose=FALSE){

  if(verbose){cat("Retrieving URLs for collection ",collection,"\n")}

  coll_info <- .get_collection_info(collection)

  # get optional opendap param if data from opendap
  if(coll_info$package=="opendapr"){

   odap_opt_param <- opendapr::odr_get_opt_param(collection,roi,verbose = F)
  list_of_urls <- map2(.x = sampling_points$date , .y = sampling_points$lag_time,
            ~opendapr::odr_get_url(
              collection = collection,
              variables = variables,
              roi = roi,
              time_range = as.Date(c(.x-.y,.x)),
              opt_param = odap_opt_param,
              verbose = verbose)
            )

  } else if (coll_info$package=="getremotedata"){

    list_of_urls <- map2(.x = sampling_points$date , .y = sampling_points$lag_time,
                         ~getremotedata::grd_get_url(
                           collection = collection,
                           variables = variables,
                           roi = roi,
                           time_range = as.Date(c(.x-.y,.x)),
                           verbose = verbose)
                         )

    } else if (coll_info$package=="shub4r"){

      list_of_urls <- map2(.x = sampling_points$date , .y = sampling_points$lag_time,
                           ~shub4r::shr_get_url(
                             collection = collection,
                             variables = variables,
                             roi = roi,
                             time_range = as.Date(c(.x-.y,.x)),
                             verbose = verbose)
      )
    }

  for(i in 1:length(list_of_urls)){
  list_of_urls[[i]]$destfile<- file.path(roi_name,list_of_urls[[i]]$destfile)
  }

  df_to_dl <- list_of_urls %>%
    do.call(rbind.data.frame, .) %>%
    unique() %>%
    dplyr::filter(!is.na(time_start))

  if(collection=="ERA5"){
    dl_res <- getremotedata::grd_download_data_era5(df_to_dl)
  } else {
    if(is.na(coll_info$login)){source=NULL} else {source="earthdata"}
    dl_res <- opendapr::odr_download_data(df_to_dl, parallel = TRUE, source = source, verbose = verbose)
  }

  return(list(list_of_urls=list_of_urls,dl_res=dl_res))

}
