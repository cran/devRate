% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{stinner_74}
\alias{stinner_74}
\title{Stinner et al equation of development rate as a function of temperature.}
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
stinner_74
}
\description{
Stinner, R., Gutierrez, A. & Butler, G. (1974) An algorithm for temperature-dependent growth
rate simulation. The Canadian Entomologist, 106, 519-524.
}
\details{
Equation:
\deqn{rT = \frac{C}{1 + e^{k1 + k2 * T}}}{%
      rT = C / (1 + exp(k1 + k2 * T))}
and
\deqn{rT = \frac{C}{1 + e^{k1 + k2 * (2 * Topt - T)}}}{%
      rT = C / (1 + exp(k1 + k2 * (2 * Topt - T)))}

where rT is the development rate, T the temperature, Topt the optimum temperature,
k1 and k2 constants. "[...] the relationship [is] inverted when the temperature is above an
optimum [...] T = 2 * Topt - T for T >= Topt." Stinner et al. 1974.
}
\references{
\doi{10.4039/Ent106519-5}
}
\keyword{datasets}
