% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_streams_network.R
\name{prepare_streams_network}
\alias{prepare_streams_network}
\title{Prepare the stream network from the accumulation raster (itself derived from the DEM)}
\usage{
prepare_streams_network(path_to_accumulation_raster,
  path_to_grassApplications_folder, threshold_accumulation_raster)
}
\description{

}
\details{
Create the hydrographic network from the accumulation raster file. We threshold the accumulation raster file : consider that all cells with a value superior to a given threshold (provided as input parameter threshold_accumulation_raster) are the hydrographic network and all the cells with a value inferior to this threshold are not part of it. The threshold was determined visually by overlaying the accumulation raster file with a very high resolution satellite image.
For additional information see the section 'example' of the article provided here: <https://grass.osgeo.org/grass76/manuals/r.thin.html>

param path_to_grassApplications_folder : Path to the GRASS application folder, for the setup of the rgrass7 package. Can be retrieved in the terminal with grass74 --config path . More info on the use of rgrass7 at https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7
}
