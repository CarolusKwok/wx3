% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_help.R
\name{load}
\alias{load}
\alias{load_file.format}
\title{Download data}
\usage{
load(url, dir, unpw, token)

load_file.format(data)
}
\arguments{
\item{url}{(character) URL of the download file}

\item{dir}{(character) Directory of the downloaded file}

\item{unpw}{(character) Username and Password, to be passed to \code{httr2::req_auth_basic}}

\item{token}{(character) Token, to be passed to \code{httr2::req_auth_basic}}

\item{data}{(data.frame) Dataframe containing columns "URL", "DIR", "Info"}
}
\description{
Download data using \code{httr2}, with safety in mind. All the downloaded data will provide a status code of the following
\itemize{
\item #FAIL: The download failed completely, and there is no successful request attempted
\item #FINE: The download was performed, but an error code appeared or the internet connection was lost between the download attempt.
\item #GOOD: The download is successful
}

Provides \code{Exist} and \code{Size} columns to \code{load_file} and \code{load_fileset} functions
}
\examples{
wx3:::load_file.format(data)
}
\keyword{internal}
