#' @name prepare_streams_network
#' @aliases prepare_streams_network
#' @title Prepare the stream network from the accumulation raster (itself derived from the DEM)
#' @description
#'
#' @param path_to_accumulation_raster
#' @param path_to_grassApplications_folder
#' @param threshold_accumulation_raster
#' @details
#'
#' Create the hydrographic network from the accumulation raster file. We threshold the accumulation raster file : consider that all cells with a value superior to a given threshold (provided as input parameter threshold_accumulation_raster) are the hydrographic network and all the cells with a value inferior to this threshold are not part of it. The threshold was determined visually by overlaying the accumulation raster file with a very high resolution satellite image.
#' For additional information see the section 'example' of the article provided here: <https://grass.osgeo.org/grass76/manuals/r.thin.html>
#'
#' param path_to_grassApplications_folder : Path to the GRASS application folder, for the setup of the rgrass7 package. Can be retrieved in the terminal with grass74 --config path . More info on the use of rgrass7 at https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7
#' @import rgrass7 sf
#' @importFrom magrittr %>%
#'
#' @export

prepare_streams_network <- function(path_to_accumulation_raster, path_to_grassApplications_folder, threshold_accumulation_raster){

  output_dir <- dirname(path_to_accumulation_raster)

  # Setup GRASS environment (mandatory to use the `rgrass7` package).
  loc <- rgrass7::initGRASS(path_to_grassApplications_folder, home=output_dir, gisDbase="GRASS_TEMP", override=TRUE,mapset = "PERMANENT" )
  rgrass7::execGRASS("g.proj",flags="c",parameters = list(proj4=sf::st_crs(st_transform(roi,.getUTMepsg(roi)))$proj4string))

  streams_network_path<-file.path(output_dir,"streams_network.gpkg")

  # Create accumulation threshold raster
  # NB : Grass has a algorithm 'r.stream.extract' that can do the job (see https://grass.osgeo.org/grass76/manuals/addons/r.accumulate.htmlsœ)
  acc_raster<-raster(path_to_accumulation_raster) %>%
    raster::reclassify(c(-Inf,threshold_accumulation_raster,NA, threshold_accumulation_raster,Inf,1)) %>%
    raster::writeRaster(accumulation_threshold_output_path,datatype='INT2S',overwrite=TRUE)

  # skeletonization (thinning extraction) and vectorization of stream network from flow accumulation map. See https://grass.osgeo.org/grass76/manuals/r.thin.html
  rgrass7::execGRASS("r.external", flags="overwrite", parameters=list(input=accumulation_threshold_output_path, output="acc_threshold",band=1))
  rgrass7::execGRASS("g.region", parameters=list(raster="acc_threshold"))
  rgrass7::execGRASS("r.thin", flags="overwrite", parameters=list(input="acc_threshold",output="acc_thin"))
  rgrass7::execGRASS("r.out.gdal", flags=c("t","m","overwrite"), parameters=list(input="acc_thin", output=file.path(output_dir,"accumulation_thin.tif"), format="GTiff",  createopt="TFW=YES,COMPRESS=LZW" ))
  rgrass7::execGRASS("r.to.vect", flags="overwrite", parameters=list(input="acc_thin", output="acc_thin_vect", type="line"))
  # Next we need to execute the v.split algo. With v.split we split the stream network into pieces of 20 meters. This will be used then to measure the mean length between the HLC point and the streams within the buffer
  # the step v.split does not give the appropriate results when executed in R, although it does not send back any error... It is weird because it works in QGIS (through GRASS plugin). For now we need to do it by hand in QGIS. To do it by hand execute:
  rgrass7::execGRASS("v.out.ogr", flags=c("m","overwrite"), parameters=list(input="acc_thin_vect", type="line", output=file.path(output_dir,"acc_thin_vect.gpkg")))
  # and than manually execute the algo v.split in the GRASS plugin of QGIS. Save the output file under streams_network_path
  # To do it via R it should be :
  #rgrass7::execGRASS("v.split", flags=c("overwrite","verbose"), parameters=list(input="acc_thin_vect", output="acc_thin_vect_split", length=20, units="map"))
  #rgrass7::execGRASS("v.out.ogr", flags=c("m","overwrite"), parameters=list(input="acc_thin_vect_split", type="line", output=streams_network_path))

  return(streams_network_path)

}
