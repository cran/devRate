% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hilbertLogan_83}
\alias{hilbertLogan_83}
\title{Holling type III equation of development rate as a function of temperature.}
\format{
A list of eight elements describing the equation.
\describe{
  \item{eq}{The equation (formula object).}
  \item{eqAlt}{The equation (string).}
  \item{name}{The name of the equation.}
  \item{ref}{The equation reference.}
  \item{refShort}{The equation reference shortened.}
  \item{startVal}{The parameters found in the literature with their references.}
  \item{com}{An optional comment about the equation use.}
  \item{id}{An id to identify the equation.}
}
}
\usage{
hilbertLogan_83
}
\description{
Hilbert, DW, y JA Logan (1983) Empirical model of nymphal development for the migratory
grasshopper, Melanoplus sanguinipes (Orthoptera: Acrididae).
Environmental Entomology 12(1): 1-5.
}
\details{
Equation:
\deqn{rT = phi * ((\frac{(T-Tb)^{2}}{(T-Tb)^{2} + aa^{2}}) - e^{-\frac{Tmax - (T-Tb)}{deltaT}})}{%
      rT = phi * (((T-Tb)^2 / ((T-Tb)^2 + aa^2)) - exp(-(Tmax - (T-Tb))/deltaT))}

where rT is the development rate, T the temperature, Tb the minimum
temperature for development, deltaT the width of high temperature boundary area,
Tmax the maximum temperature, and aa a constant.
}
\references{
\doi{10.1093/ee/12.1.1}
}
\keyword{datasets}
