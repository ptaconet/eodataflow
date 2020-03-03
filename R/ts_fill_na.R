# Impute NAs and setup quality flag
# qval=1 is for values that where retrieved using the base raster layer products (best quality)
# qval=2 is for values that where retrieved using the value(s) the upper(s) buffer(s) for the same day / weekd
# qval=3 is for values that where retrieved using the mean value of the previous and the following day / weekd
# qval=4 is for values that where retrieved using the 8-days product rather than the 1-day product
# qval=5 is for values that where retrieved using the mean value of the previous and the following 8-days product
# qval=0 is for NAs remaining after these imputings


#' @name ts_fillna_l1tol5
#' @title fill NAs in time series - level 1 to level 5
#' @importFrom magrittr %>%
#' @export

ts_fillna_l1tol5 <- function(df_var_1day,df_var_8day){

  df_var_8day_nafill <- .ts_fillNA_l1(df_var_8day,1)

  df_var_1day_nafill <- df_var_1day %>%
    .ts_fillNA_l1(.,1) %>%
    .ts_fillNA_l2(.,2) %>%
    .ts_fillNA_l3(.,3) %>%
    .ts_fillNA_l4(.,df_var_8day_nafill,4)

  df_var_8day_nafill <- df_var_8day_nafill %>%
    .ts_fillNA_l2(.,2) %>%
    .ts_fillNA_l3(.,3)

  df_var_1day_nafill<-df_var_1day_nafill %>%
    .ts_fillNA_l4(.,df_var_8day_nafill,5)

  return(list(df_var_1day_nafill = df_var_1day_nafill, df_var_8day_nafill = df_var_8day_nafill))

}

#' @name ts_fillna_l1tol3
#' @title fill NAs in time series - level 1 to level 3
#' @importFrom magrittr %>%
#' @export

ts_fillna_l1tol3 <- function(df_var){

  df_var_nafill <- df_var %>%
    .ts_fillNA_l1(.,1) %>%
    .ts_fillNA_l2(.,2) %>%
    .ts_fillNA_l3(.,3)

  return(df_var_nafill)
}

#' @name .ts_fillNA_l1
#' @title fill NAs in time series - level 1
#' @import dplyr
#' @importFrom magrittr %>%
#' @noRd

# qval=1
.ts_fillNA_l1<-function(df,qvalue){
  df <- df %>%
    dplyr::mutate(qval=if_else(!is.na(val),qvalue,0))
  return(df)
}

# qval=2
#' @name .ts_fillNA_l2
#' @title fill NAs in time series - level 2
#' @import dplyr
#' @importFrom magrittr %>%
#' @noRd
.ts_fillNA_l2<-function(df,qvalue){
  df <- df %>%
    arrange(id,lag_n,buffer) %>%
    group_by(id,lag_n) %>%
    fill(val, .direction = "up") %>%
    mutate(qval=if_else((qval==0 & !is.na(val)),qvalue,qval)) %>%
    as.data.frame()
  return(df)
}

# qval=3
#' @name .ts_fillNA_l3
#' @title fill NAs in time series - level 3
#' @import dplyr
#' @importFrom magrittr %>%
#' @noRd
.ts_fillNA_l3<-function(df,qvalue){
  df <- df %>%
    arrange(id,buffer,lag_n) %>%
    mutate(val_ahead=lead(val)) %>%
    mutate(val_lag=lag(val)) %>%
    mutate(val_mean=map2_dbl(.x=val_ahead,.y=val_lag,~mean(c(.x,.y),na.rm=T))) %>%
    mutate(val_mean=replace(val_mean, which(is.nan(val_mean)), NA)) %>%
    mutate(val=if_else(!is.na(val),val,val_mean)) %>%
    mutate(qval=if_else((qval==0 & !is.na(val)),qvalue,qval))  %>%
    dplyr::select(-c(val_ahead,val_lag,val_mean))
  return(df)
}

# qval=4
#' @name .ts_fillNA_l4
#' @title fill NAs in time series - level 4
#' @import dplyr
#' @importFrom magrittr %>%
#' @noRd
.ts_fillNA_l4<-function(df,df_8day,qvalue){
  df8day_as_1day <- df_8day %>%
    slice(rep(1:n(), each = 7)) %>%
    group_by(id,buffer) %>%
    mutate(lag_n=seq(0,n()-1,1))

  df<-left_join(df,df8day_as_1day,by=c("id","buffer","lag_n")) %>%
    mutate(val.x=if_else(!is.na(val.x),val.x,val.y)) %>%
    dplyr::select(-c(val.y,var.y,qval.y,date.y,lag_n.y)) %>%
    rename(val=val.x,var=var.x,qval=qval.x,date=date.x,lag_n=lag_n.x) %>%
    mutate(qval=if_else((qval==0 & !is.na(val)),qvalue,qval))
  return(df)
}
