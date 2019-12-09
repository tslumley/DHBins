\name{dhbin}
\alias{dhbin}
\alias{dhtri}
\alias{regionbin}
\alias{regiontri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hexmaps of New Zealand Adminstrative Units
}
\description{
The 20 District Health Boards are the main administrative and funding units of the NZ national health system. The Regions are the second-level government divisions . These functions draw stylised maps of the DHBs and regions, along the lines of "statebins" for the US states. The hexagon for each location can be coloured and sized to show numeric data, and it can also be split into six triangles to show proportion or composition data. 
}
\usage{
dhbin(radius = NULL, hex_colours = "lightskyblue", DHB_names=NULL, text_colour = "black",
legend_opts = NULL, border = NULL, short = FALSE,cex=1)
dhtri(radius = NULL, tri_colours, DHB_names=NULL, text_colour = "black", legend_opts = NULL,
 short=FALSE,cex=1)
regionbin(radius = NULL, hex_colours = "lightskyblue", DHB_names=NULL, text_colour = "black",
legend_opts = NULL, border = NULL, short = FALSE, tasman=TRUE, cex=1)
regiontri(radius = NULL, tri_colours, DHB_names=NULL, text_colour = "black", legend_opts = NULL,
 short=FALSE, tasman=TRUE, cex=1)
 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{radius{
Vector of radius measurements for the coloured part of each hexagon, with 1 as a full hexagon. If any are greater than 1, the vector is scaled to have maximum value 0.95, which is also the default value for all hexagons.  If it has names, they are matched to the DHB names or common alternative names.
}
  \item{hex_colours}{
Vector of colours as names or hashtag colours.  If it has names, they are matched to the DHB names or common alternative names.
}
\item{tri_colours}{
20 x 6 matrix of colours for the six triangles in each hexagon.  If it has row names, they are matched to the DHB names or common alternative names.
}
\item{DHB_names}{Vector of DHB names. If \code{NULL}, the function will look for this as the names of \code{radius} then as the names of \code{hex_colours} or the rownames of \code{tri_colours}}
  \item{text_colour}{
Colour for the label on all the hexagons.
}
  \item{legend_opts}{
a list with arguments to \code{graphics::legend}, not including \code{x} and \code{y}, which are supplied, and \code{bty}, which is forced to \code{"n"}.  If not \code{NULL} a legend is drawn. 
}
  \item{border}{
 \code{NULL} or a colour name to draw a border around each hex.
}
  \item{short}{
If \code{TRUE}, label each hexagon with a 1-3 character short abbreviation rather than the full name. Especially useful for faceting.
}
\item{tasman}{The Tasman District and Nelson City are considered as separate regions in most contexts, but as forming the Nelson Region in some contexts. Use \code{tasman=FALSE} when they are combined}
\item{cex}{
character expansion factor
}

}

\value{
Called for its side-effect
}

\seealso{
\code{\link{tri_alloc}} for the Sainte-Laguë method to allocate triangles based on counts
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


households<-rowSums(dhb_cars[,-1])
names(households)<-dhb_cars$dhb
dhbin(radius=sqrt(households))
title(main="Number of households in private dwellings")


opar<-par(mfrow=c(2,3),mar=c(1,1,1,1))
z<-rnorm(20)
for(i in 1:6){
  z<-(rnorm(20)+z)/sqrt(2)
  z1<- (z+4)/8
  col_z<-rgb(colorRamp(c("blue", "white","red"))(z1),max=255)
  dhbin(hex_colours=col_z,border="grey",short=TRUE,
        legend_opts=list(fill=c("red","white","blue"),
                         legend=c("High","Medium","Low"),
                         title=paste("Thing",i))
  ) 
}
par(opar)


data(region_ethnicity)
cols<-tri_alloc(as.matrix(region_ethnicity[,-1]),
	colours=hcl.colors(5,"Set2"),names=region_ethnicity$Area)

regiontri(tri_colours=cols,text_colour="white",
	legend_opts=list(fill=hcl.colors(5,"Set2"),
			 title="Ethnicity",
			 legend=names(region_ethnicity)[2:6]))
title(main="New Zealand regions")


}
