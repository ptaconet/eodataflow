#' @name prepare_mixed_products
#' @aliases prepare_mixed_products
#' @title Prepare mixed products (e.g. MODIS TERRA and AQUA)
#' @description
#'
#' @param rasts_terra
#' @param rasts_aqua
#' @param fun_summarize
#'
#' @import raster
#'
#' @export


prepare_mixed_products <- function(rasts_terra,rasts_aqua,fun_summarize){


   if(fun_summarize=="max"){
   fun_to_summarize <- Vectorize(function(x, y){
     if(is.na(x) && is.na(y)) { NA } else if (is.na(x) && !is.na(y)) { y } else if (!is.na(x) && is.na(y)) { x } else { max(x, y, na.rm=TRUE)}
   })
   } else if (fun_summarize=="min"){
     fun_to_summarize <- Vectorize(function(x, y){
       if(is.na(x) && is.na(y)) { NA } else if (is.na(x) && !is.na(y)) { y } else if (!is.na(x) && is.na(y)) { x } else { min(x, y, na.rm=TRUE)}
     })
   } else if (fun_summarize=="mean"){ # the "mean" function does not work...
     fun_to_summarize <- Vectorize(function(x, y){
       if(is.na(x) && is.na(y)) { NA } else if (is.na(x) && !is.na(y)) { y } else if (!is.na(x) && is.na(y)) { x } else {(x+y)/2}
     })
   } else if (fun_summarize=="sqrt_squared"){
      fun_to_summarize <- Vectorize(function(x, y){
         (x^2+y^2)^0.5
      })
   } else if (fun_summarize=="atan2"){
      fun_to_summarize <- Vectorize(function(x, y){
         atan2(x,y)
      })
   }


    # not working :
    # fun_to_summarize <- Vectorize(function(x, y){
    #   if(is.na(x) && is.na(y)) { NA } else { eval(parse(text=fun_summarize))(x, y, na.rm=TRUE)}
    # })

   rasts_combined <- list()
   for(i in 1:length(rasts_terra)){
     th_rast <- overlay(rasts_terra[[i]], rasts_aqua[[i]], fun = fun_to_summarize)
     names(th_rast) <- names(rasts_terra[[i]])
     rasts_combined<-c(rasts_combined, th_rast)
   }

   return(rasts_combined)
 }
