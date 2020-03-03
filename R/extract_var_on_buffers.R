#' @name extract_var_on_buffers
#' @aliases extract_var_on_buffers
#' @title Extract variables given a vector of buffer sizes
#' @description
#'
#' @param rastBrick list of raster bricks (1 to n)
#' @param sf_points list of sampling sf_points (1 to n)
#' @param buffer_sizes vector of buffers
#' @param fun_summarize
#' @param is_timeseries
#' @param timeseries_date_sampling vector or list of sampling dates (1 to n)
#' @param code_indice
#' @param verbose
#'
#' @details
#'
#' Argument RastBrick is either :
#' - a Raster object
#' - a rasterBrick object
#'
#'
#'
#' @import velox sf dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr pmap_dfr
#'
#' @export


extract_var_on_buffers <- function(rastBrick,
                           sf_points,
                           buffer_sizes,
                           fun_summarize = "mean",
                           is_timeseries = FALSE,
                           timeseries_date_sampling = NULL,
                           code_indice = NULL,
                           verbose = FALSE){

  extract_var_on_buffer <- function(rastBrick,sf_points,buffer_size,fun_summarize,is_timeseries,timeseries_date_sampling){

    utm_epsg <- .getUTMepsg(sf_points)
    if(verbose){cat("UTM zone is ",utm_epsg,"\n")}
    buffers <- st_buffer(st_transform(sf_points,.getUTMepsg(sf_points)),buffer_size) %>% st_transform(st_crs(rastBrick))
    rastVx <- velox(rastBrick)

    ex.mat <- rastVx$extract(buffers, small = T, fun = function(x) eval(parse(text=fun_summarize))(x, na.rm = TRUE))
    ex.mat <- as.data.frame(ex.mat)
    colnames(ex.mat) <- names(rastBrick)

    if(is_timeseries){
      ex.mat2 <- ex.mat
      colnames(ex.mat2) <- seq(ncol(ex.mat2)-1,0,-1)

      ex.mat2 <- ex.mat2 %>%
        mutate(id = sf_points$id) %>%
        tidyr::pivot_longer(-id,names_to = "lag_n", values_to = "val") %>%
        dplyr::select(lag_n)

      ex.mat <- ex.mat %>%
        mutate(id = sf_points$id) %>%
        tidyr::pivot_longer(-id,names_to = "date", values_to = "val") %>%
        mutate(val = ifelse(is.nan(val),NA,val)) %>%
        mutate(date = gsub("X","",date)) %>%
        mutate(date = gsub("\\.","-",date)) %>%
        mutate(date = as.Date(date)) %>%
        mutate(lag_time = timeseries_date_sampling-date) %>%
        mutate(lag_time = as.numeric(lag_time)) %>%
        mutate(buffer = buffer_size) %>%
        mutate(lag_n = ex.mat2$lag_n) %>%
        dplyr::select(id,buffer,date,lag_time,lag_n,val)
      } else {
    #TODO non time series
        }
    return(ex.mat)
    }

  df_var <- buffer_sizes %>%
    map_dfr(.,~pmap_dfr(list(rastBrick, sf_points, ., timeseries_date_sampling),
                        ~extract_var_on_buffer(..1,..2,..3,fun_summarize,is_timeseries,..4))) %>%
    mutate(var = code_indice)

  return(df_var)
}
