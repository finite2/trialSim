% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00classes.R
\docType{class}
\name{simulateOutcome-class}
\alias{simulateOutcome}
\alias{simulateOutcome-class}
\title{simulateOutcome}
\description{
A class and generator function to store a function which simulates outcome data and adds it to the current data. This function may rely on the current \code{\link{Decision}} object.
}
\section{Slots}{

\describe{
\item{\code{fun}}{Does the simulation and adds the new data. This function should allow any number of arguments to be passed to it using \dots. This function must return the data.

To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.}
}}

