#' @name extract_var_on_buffers
#' @aliases extract_var_on_buffers
#' @title Extract variables given a vector of buffer sizes
#' @description
#'
#' @param rastBrick list of rasterBricks or velox objects (1 to n)
#' @param sf_points list of sampling sf_points (1 to n)
#' @param buffer_sizes vector of buffers (1 to z)
#' @param fun_summarize summarizing function
#' @param is_timeseries
#' @param date_sampling list of sampling dates (1 to n)
#' @param na_max_perc integer. if, in a given buffer, there are more than na_max_perc cells that are NA, the overall value for this buffer will be NA. default is 0 (i.e. as long as there is one cell with a value, the overall value for the buffer will be computed)
#' @param code_indice character vector. code indice (only for timeseries)
#' @param verbose verbose
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
#' @importFrom magrittr set_names %>%
#' @export


extract_var_on_buffers <- function(rastBrick,
                           sf_points,
                           buffer_sizes,
                           fun_summarize = "mean",
                           is_timeseries = FALSE,
                           date_sampling = NA,
                           na_max_perc = 100,
                           code_indice = NULL,
                           verbose = FALSE){

  extract_var_on_buffer <- function(rastBrick,sf_points,buffer_size,fun_summarize,is_timeseries,date_sampling,na_max_perc,code_indice,verbose){

    utm_epsg <- .getUTMepsg(sf_points)
    if(verbose){cat("UTM zone is ",utm_epsg,"\n")}
    buffers <- st_buffer(st_transform(sf_points,utm_epsg),buffer_size) %>% st_transform(st_crs(rastBrick))
    rastVx <- velox(rastBrick)

    if(na_max_perc<100){ # get percentages of NA cells in each buffer

      fun_perc_na <- function(x) {
        if(sum(!is.na(x))==0){ 100 } # buffers with only NA pixels will be set to inf (as sum(!is.na(x))==0). We replace them by 100
         else {
             sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x)))*100
          }
        }

      ex.mat.vals.nas <- rastVx$extract(buffers, small = T, fun = NULL) # return the raw values in each buffer
      names(ex.mat.vals.nas) <- sf_points$id
      ex.mat.vals.nas <- ex.mat.vals.nas %>%
        map(.,~as.data.frame(.)) %>%
        map(.,~map_dbl(., ~fun_perc_na(.))) %>%
        map(.,~magrittr::set_names(.,names(rastBrick))) %>%
        do.call(rbind,.) %>%
        as.data.frame() %>%
        mutate(id=rownames(.))

    }

    ex.mat <- rastVx$extract(buffers, small = T, fun = function(x) eval(parse(text=fun_summarize))(x, na.rm = TRUE))
    ex.mat <- as.data.frame(ex.mat)
    colnames(ex.mat) <- names(rastBrick)

    if(is_timeseries){
      ex.mat2 <- ex.mat
      colnames(ex.mat2) <- seq(ncol(ex.mat2)-1,0,-1)

      ex.mat2 <- ex.mat2 %>%
        mutate(id = sf_points$id) %>%
        tidyr::pivot_longer(-id,names_to = "lag_n", values_to = "val") %>%
        dplyr::select(lag_n) %>%
        mutate(lag_n = as.numeric(lag_n))

      ex.mat <- ex.mat %>%
        mutate(id = sf_points$id) %>%
        tidyr::pivot_longer(-id,names_to = "date", values_to = "val") %>%
        mutate(val = ifelse(is.nan(val),NA,val)) %>%
        mutate(date = gsub("X","",date)) %>%
        mutate(date = gsub("\\.","-",date)) %>%
        mutate(date = as.Date(date)) %>%
        mutate(lag_time = date_sampling-date) %>%
        mutate(lag_time = as.numeric(lag_time)) %>%
        mutate(buffer = buffer_size) %>%
        mutate(lag_n = ex.mat2$lag_n) %>%
        mutate(var = code_indice) %>%
        dplyr::select(id,buffer,date,lag_time,lag_n,val,var)

      if(na_max_perc<100){ # set to NAs buffers with to many NA values (treshold is na_max_perc)

        ex.mat.vals.nas <- ex.mat.vals.nas %>%
          tidyr::pivot_longer(-id,names_to = "date", values_to = "na_perc") %>%
          mutate(date = gsub("X","",date)) %>%
          mutate(date = gsub("\\.","-",date)) %>%
          mutate(date = as.Date(date))

        ex.mat <- ex.mat %>%
          left_join(ex.mat.vals.nas,by=c("id","date")) %>%
          mutate(val=ifelse(na_perc <= na_max_perc,val,NA)) #%>%
        #select(-na_perc)
      }

      } else {

        ex.mat <- ex.mat %>%
          mutate(id = sf_points$id) %>%
          tidyr::pivot_longer(-id,names_to = "code_indice", values_to = "val") %>%
          mutate(buffer = buffer_size) %>%
          dplyr::select(id,buffer,val,code_indice)

        if(na_max_perc<100){ # set to NAs buffers with to many NA values (treshold is na_max_perc)

          ex.mat.vals.nas <- ex.mat.vals.nas %>%
            tidyr::pivot_longer(-id,names_to = "code_indice", values_to = "na_perc")

          ex.mat <- ex.mat %>%
            left_join(ex.mat.vals.nas,by=c("id","code_indice")) %>%
            mutate(val=ifelse(na_perc <= na_max_perc,val,NA)) #%>%
            #select(-na_perc)
        }

      }

    return(ex.mat)

    }

  if(!is.list(rastBrick)){
    rastBrick <- list(rastBrick)
    sf_points <- list(sf_points)
  }

  df_var <- buffer_sizes %>%
    map_dfr(.,~pmap_dfr(list(rastBrick, sf_points, ., date_sampling,na_max_perc),
                        ~extract_var_on_buffer(..1,..2,..3,fun_summarize,is_timeseries,..4,..5,code_indice,verbose)))

  return(df_var)
}
