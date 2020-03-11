#' @name prepare_topography_indices
#' @aliases prepare_topography_indices
#' @title Prepare the topography indices from the DEM
#' @description Create the topography datasets from the DEM with the rgrass7 package : slope, aspect, accumulation, tci, twi
#'
#' @param path_to_input_dem_files
#' @param roi
#' @param path_to_grassApplications_folder
#' @details
#'
#' param path_to_grassApplications_folder : Path to the GRASS application folder, for the setup of the rgrass7 package. Can be retrieved in the terminal with grass74 --config path . More info on the use of rgrass7 at https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7
#' @import rgrass7 sf
#' @importFrom magrittr %>%
#'
#' @export


prepare_topography_indices <- function(path_to_input_dem_files, roi, path_to_grassApplications_folder){

  output_dir <- dirname(path_to_input_dem_files$destfile)[1]

  # Setup GRASS environment (mandatory to use the `rgrass7` package).
  loc <- rgrass7::initGRASS(path_to_grassApplications_folder, home=output_dir, gisDbase="GRASS_TEMP", override=TRUE,mapset = "PERMANENT" )
  rgrass7::execGRASS("g.proj",flags="c",parameters = list(proj4=sf::st_crs(st_transform(roi,.getUTMepsg(roi)))$proj4string))

  # set path to output directories
  dem_output_path<-file.path(output_dir,"DEM.tif")
  dem_depressionless_output_path<-file.path(output_dir,"DEM_depressionless.tif")
  slope_output_path<-file.path(output_dir,"slope.tif")
  aspect_output_path<-file.path(output_dir,"aspect.tif")
  accumulation_output_path<-file.path(output_dir,"accumulation.tif")
  tci_output_path<-file.path(output_dir,"tci.tif")
  twi_output_path<-file.path(output_dir,"twi.tif")

  # import dem
  dem <- getremotedata::grd_import_data(path_to_input_dem_files,"SRTMGL1.003",roi = roi)

  # reproject DEM to UTM projection and save to disk
  dem <- dem %>%
    raster::projectRaster(crs = sf::st_crs(st_transform(roi,.getUTMepsg(roi)))$proj4string) %>%
    raster::writeRaster(dem_output_path, NAflag=0, overwrite=TRUE)

  #Create the topography datasets from the DEM with the `rgrass7` package : slope, aspect, accumulation, tci, twi ;
  rgrass7::execGRASS("r.external", flags="o", parameters=list(input=dem_output_path, output="tmprast",band=1))
  rgrass7::execGRASS("g.region", parameters=list(raster="tmprast"))

  # Filters and generates a depressionless elevation map
  rgrass7::execGRASS("r.fill.dir", flags="overwrite", parameters=list(input="tmprast", output="DEM",direction="dir"))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="DEM", output=dem_depressionless_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))

  # Compute slope and aspect and save to disk
  rgrass7::execGRASS("r.slope.aspect", flags="overwrite", parameters=list(elevation="DEM", slope="slope",aspect="aspect",format="percent", precision="FCELL",zscale=1,min_slope=0))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="slope", output=slope_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="aspect", output=aspect_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))

  # Compute hydrograpy indices and save to disk
  rgrass7::execGRASS("r.terraflow", flags="overwrite", parameters=list(elevation="DEM", accumulation="accumulation", tci="tci"))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="accumulation", output=accumulation_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="tci", output=tci_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))

  # Compute TWI indice
  rgrass7::execGRASS("r.topidx", flags="overwrite", parameters=list(input="DEM", output="twi"))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="twi", output=twi_output_path, format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))


  return(list(dem_depressionless_output_path,slope_output_path,aspect_output_path,accumulation_output_path,tci_output_path,twi_output_path))


}
