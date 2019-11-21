\name{region_ethnicity}
\alias{region_ethnicity}
\docType{data}
\title{
Ethnic makeup of New Zealand, by region
}
\description{
Number of people, by ethnicity, for the New Zealand regions, based on the 2013 Estimated Resident Population.
}
\usage{data("region_ethnicity")}
\format{
  A data frame with 16 observations on the following 6 variables.
  \describe{
    \item{\code{Area}}{a factor with level \code{Auckland region} \code{Bay of Plenty region} \code{Canterbury region} \code{Gisborne region} \code{Hawke\'s Bay region} \code{Manawatu-Wanganui region} \code{Marlborough region} \code{Nelson region} \code{Northland region} \code{Otago region}  \code{Southland region} \code{Taranaki region} \code{Tasman region}  \code{Waikato region} \code{Wellington region} \code{West Coast region}}
    \item{\code{Asian}}{number of people}
    \item{\code{Euro_Other}}{number of people}
    \item{\code{Maori}}{number of people}
    \item{\code{MELAA}}{number of people}
    \item{\code{Pacific}}{number of people}
  }
}

\source{
\url{https://figure.nz/table/foip3RBvo69sO3J3}, originally from Statistics New Zealand
}

\examples{
data(region_ethnicity)
summary(region_ethnicity)
}
\keyword{datasets}
