\name{immune}
\alias{immune}
\docType{data}
\title{
Immunisation coverage 
}
\description{
The report measures the number of children who turned the milestone age of 5 years between 01-Jul-2019 and 30-Sep-2019 and who have completed their age appropriate immunisations by the time they turned the milestone age. The target is 95\%. Children are only counted once, according to 'prioritised ethnicity': the priority order is Māori, Pacific, Asian, other, NZ European.
}
\usage{data("immune")}
\format{
  A data frame with 20 observations on the following 19 variables.
  \describe{
    \item{\code{DHB}}{name of District Health Board}
    \item{\code{Ntotal}}{Number of children}
    \item{\code{Vaxtotal}}{Number who completed their vaccinations}
    \item{\code{Pcttotal}}{Proportion}
    \item{\code{NNZE},\code{VaxNSE},\code{PctNZE}}{the same, for children of NZ European ethnicity}
    \item{\code{NMaori},\code{VaxMaori},\code{PctMaori}}{the same, for children of Māori ethnicity}
     \item{\code{NPacific},\code{VaxPacific},\code{PctPacific}}{the same, for children of Pacific ethnicities}
    \item{\code{NAsian},\code{VaxAsian},\code{PctAsian} }{the same, for children of Asian ethnicities}
    \item{\code{NOther},\code{VaxOther},\code{PctOther}}{the same, for children of other ethnicities}
  }
}

\source{
New Zealand Health Indicators, 2019
}
\references{
Statistics New Zealand. Health Indicators.
\url{http://archive.stats.govt.nz/browse_for_stats/snapshots-of-nz/nz-social-indicators/Home/Health/childhood-immunisation.aspx}
}
\examples{
data(immune)
summary(immune)

immune$vax_pct<-cut(immune$Pcttotal,c(0,.80,.85,.90,.95,1))
ggplot(immune)+
	geom_dhb(aes(fill=vax_pct,map_id=dhb_fixname(DHB)))+
	scale_fill_viridis_d(drop=FALSE)+
	geom_label_dhb(short=TRUE,colour="white")

cols<-c("goldenrod","red","orange","gold","springgreen")
with(immune, dhbin(hex_colours=cols[vax_pct], legend_opts=list(fill=cols,
legend=c("<80","80-85","85-90","90-95","95+"), title="Immunisation coverage (pct)")
 ))

## Add some transparency
with(immune, dhbin(hex_colours=adjustcolor(cols[vax_pct],alpha.f=.5), legend_opts=list(fill=cols,
legend=c("<80","80-85","85-90","90-95","95+"), title="Immunisation coverage (pct)")
 ))

}
\keyword{datasets}
