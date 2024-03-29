% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devRate_qualityFits.R
\name{devRateQlStat}
\alias{devRateQlStat}
\title{Statistical indices of the nls goodness-of-fit}
\usage{
devRateQlStat(nlsDR)
}
\arguments{
\item{nlsDR}{A list of \code{nls} objects.}
}
\value{
A \code{data.frame} with statistical indices
  in columns (RSS, RMSE, AIC, BIC) and nls objects in rows.
}
\description{
Return a table of multiple statistical indices of goodness-of-fit
}
\details{
NULL is returned when nlsDR is not of type list.
  AIC and BIC are calculated using the RSS (Burnham and Anderson, 2002).
}
\examples{
myDf <- data.frame(
  temp = seq(from = 0, to = 50, by = 10),
  rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
damos_08Fit <- devRateModel(
  eq = damos_08,
  dfData = myDf,
  startValues = list(aa = 1, bb = 1, cc = 1),
  algo = "LM")
kontodimas_04Fit <- devRateModel(
  eq = kontodimas_04,
  dfData = myDf,
  startValues = list(aa = 1, Tmin = 7, Tmax = 40),
  algo = "LM")
poly2Fit <- devRateModel(
  eq = poly2,
  dfData = myDf,
  startValues = list(a0 = 1, a1 = 1, a2 = 1),
  algo = "LM")
devRateQlStat(
  nlsDR = list(damos_08Fit, kontodimas_04Fit, poly2Fit)
)
}
