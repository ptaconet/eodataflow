% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eodf_import_data.R
\name{eodf_import_data}
\alias{eodf_import_data}
\title{Import the data in R}
\usage{
eodf_import_data(df_data_to_import, collection, variable = NULL,
  roi = NULL, output = "RasterBrick")
}
\arguments{
\item{df_data_to_import}{data.frame. Contains the path to the data that were downloaded. Typically output of \code{odr_get_url}, \code{grd_get_url} or \code{shr_get_url}. See Details for the structure}

\item{collection}{character string the name of a collection}

\item{variable}{character string the name of a variables}

\item{roi}{sf the ROI. Mandatory for SMAP collection, else, not necessary}

\item{output}{character string the output. "RasterBrick" or "stars". At now stars is implemented only for MODIS and VIIRS collections}
}
\description{
Function to import a data / time series that was downloaded with one of the packages \code{opendapr}, \code{getremotedata} or \code{shub4r}
}
