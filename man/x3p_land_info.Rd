% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upload.R
\name{x3p_land_info}
\alias{x3p_land_info}
\title{Read metadata from x3p and fill in land information}
\usage{
x3p_land_info(path)
}
\arguments{
\item{path}{path to x3p file}
}
\value{
tibble with land information
}
\description{
x3p header information is concatenated into a long comment (containing serial
number, version, calibration date, probing system, creator, creation date,
and md5), and other relevant information (microscope brand, model,
resolution) is stored in individual fields corresponding to fields on NBTRD.
}
