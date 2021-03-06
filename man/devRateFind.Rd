% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devRate_find.R
\name{devRateFind}
\alias{devRateFind}
\title{Find models for species}
\usage{
devRateFind(orderSP = "", familySP = "", species = "")
}
\arguments{
\item{orderSP}{Find models by Order.}

\item{familySP}{Find models by Family.}

\item{species}{Find models by species (Genus species).}
}
\value{
A data.frame with the name of the equations, the number of
  occurrences in the database, and the number of parameters for each
  equation.
}
\description{
Find models for species
}
\details{
The function looks for the species in the database and returns
  the number of occurrences for each model.
}
\examples{
devRateFind(orderSP = "Lepidoptera")
devRateFind(familySP = "Gelechiidae")
## detailed example:
devRateFind(species = "Tuta absoluta")
## campbell_74 model has been used for T. absoluta
## Parameters from the campbell equation can be accessed by:
## campbell_74$startVal[campbell_74$startVal["genSp"] == "Tuta absoluta",]
}
