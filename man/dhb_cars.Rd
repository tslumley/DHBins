\name{dhb_cars}
\alias{dhb_cars}
\docType{data}
\title{
Cars per household in New Zealand
}
\description{
The distribution of cars per household across New Zealand District Health Boards, based on data from the 2013 Census
}
\usage{data("dhb_cars")}
\format{
  A data frame with 20 observations on the following 5 variables.
  \describe{
    \item{\code{dhb}}{District Health Board name}
    \item{\code{none}}{Number of households with no cars}
    \item{\code{one}}{Number of households with one car}
    \item{\code{two}}{Number of households with two cars}
    \item{\code{more}}{Number of households with three or more cars}
  }
}

\source{
Statistics New Zealand \url{http://archive.stats.govt.nz/Census/2013-census/data-tables/dhb-tables.aspx}
}

\examples{
data(dhb_cars)
tris<-tri_alloc(dhb_cars[,-1], c("green","gold","orange","goldenrod"), names=dhb_cars$dhb )

dhtri(tri_colours=tris,
  legend=list(fill=c("green","gold","orange","goldenrod"),
         border=NA,
         legend=c("0","1","2","3+"),
         title="Cars/Household")
)
}
\keyword{datasets}
