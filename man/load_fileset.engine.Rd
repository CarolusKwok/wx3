% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_fileset_engine.R
\name{load_fileset.engine}
\alias{load_fileset.engine}
\title{System tool: Engine of the load_fileset function}
\usage{
load_fileset.engine(data_download, tmp_dir, threshold, ...)
}
\arguments{
\item{data_download}{(data.frame) Data to be downloaded, refer to \code{data} in \code{wx3::load_fileset}}

\item{tmp_dir}{(character) Temporary directory of the main \code{R} session}

\item{threshold}{(numeric) The minimum file size in bytes, similar to \code{threshold} in \code{wx3::load_fileset}}

\item{...}{Additional parameters to be passed to \code{RCurl::getBinaryURL}}
}
\value{
NA
}
\description{
The engine to download filesets from a site, using parallel computation.
}
\keyword{internal}
