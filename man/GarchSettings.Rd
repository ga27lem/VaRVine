% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{GarchSettings}
\alias{GarchSettings}
\title{class: GARCH Settings Class.}
\description{
Class for the GARCH models settings.
}
\section{Slots}{

\describe{
\item{\code{train.size}}{The training window size.}

\item{\code{refit.every}}{Determines every how many periods the garch models are re-estimated.}

\item{\code{specs}}{A named list (stock ticker => \code{\link[rugarch:uGARCHspec-class]{rugarch::uGARCHspec}} class).}
}}

