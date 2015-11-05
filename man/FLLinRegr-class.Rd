% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/FLLinRegr.R
\docType{class}
\name{FLLinRegr-class}
\alias{FLLinRegr-class}
\title{An S4 class to represent FLLinRegr}
\arguments{
\item{object}{contains: call,coefficients}

\item{object}{a named vector of coefficients}

\item{object}{contains: call,residuals,coefficients,significant codes note and statistical output.}
}
\description{
An S4 class to represent FLLinRegr
}
\section{Slots}{

\describe{
\item{\code{formula}}{an object of class 'formula': Model Formulae}

\item{\code{table_name}}{A character}

\item{\code{deeptablename}}{A character vector containing name of the deeptable on conversion from a widetable}

\item{\code{AnalysisID}}{An output character ID from CALL FLLinRegr}

\item{\code{dataprepID}}{An output character ID from CALL FLRegrDataPrep}

\item{\code{datatable}}{An object of class FLTable}
}}
