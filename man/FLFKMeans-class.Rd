% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLFKMeans.R
\docType{class}
\name{FLFKMeans-class}
\alias{FLFKMeans-class}
\title{An S4 class to represent FLFKMeans}
\arguments{
\item{object}{retrieves the cluster vector}

\item{object}{retrieves the coordinates of the centroids}

\item{object}{overloads the print function}

\item{object}{total within sum of squares}

\item{object}{within sum of squares}

\item{object}{between sum of squares}

\item{object}{total sum of squares}

\item{object}{size vector}
}
\description{
An S4 class to represent FLFKMeans
}
\section{Slots}{

\describe{
\item{\code{centers}}{A numeric vector containing the number of clusters, say k}

\item{\code{AnalysisID}}{A character output used to retrieve the results of analysis}

\item{\code{connection}}{ODBC connectivity for R}

\item{\code{table}}{FLTable object given as input on which analysis is performed}

\item{\code{resultsfetched}}{A logical vector describing what components are fetched}

\item{\code{results}}{A list of all fetched components}

\item{\code{deeptablename}}{A character vector containing a deeptable(either conversion from a widetable or input deeptable)}
}}

