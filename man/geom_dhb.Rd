\name{geom_dhb}
\alias{geom_dhb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geom for DHB hexmap
}
\description{
A \code{ggplot2} geom for the District Health Board hexmap: \code{geom_dhb} wraps \code{geom_map} and \code{geom_label_dhb} wraps \code{geom_text}. 
}
\usage{
geom_dhb(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,coord=TRUE)
geom_label_dhb(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
geom_dhbtri(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,coord=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. For \code{geom_dhb} you must specify \code{map_id} with the unit names, and for \code{geom_dhbtri} additionally specify \code{class_id} giving the order of triangles within a hex.  The \code{radius} aesthetic controls the size of the hexes}
  \item{data}{
The data to be displayed in this layer. Usually inherited from the plot data}
  \item{\dots}{
Other arguments passed on to layer(), such as aesthetics, used to set an aesthetic to a fixed value, like \code{colour = "white"} or \code{cex = 3}. }
  \item{na.rm}{
If \code{FALSE}, warn when missing values are removed
}
  \item{show.legend}{

logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{inherit.aes}{
  If \code{FALSE}, overrides the default aesthetics, rather than combining with them.}
  \item{coord}{If \code{TRUE}, set the aspect ratio and plot limits.  The main reason to have \code{coord=FALSE} is if you are stacking two \code{geom_dhb} layers }
}

\value{
The geoms
}

\seealso{
\code{\link{dhb_fixname}}
}
\examples{
data(immune)
summary(immune)

immune$vax_pct<-cut(immune$Pcttotal,c(0,.80,.85,.90,.95,1))
ggplot(immune)+
	geom_dhb(aes(fill=vax_pct,map_id=dhb_fixname(DHB)))+
	scale_fill_viridis_d(drop=FALSE)+
	geom_label_dhb(short=TRUE,colour="white")

## size variation, plus stacking to show the full-size hexes underneath,
## plus no labels
ggplot(immune)+
	geom_dhb(aes(map_id=dhb_fixname(DHB)),fill="white",colour="grey",coord=FALSE)+
	geom_dhb(aes(fill=vax_pct,map_id=dhb_fixname(DHB),radius=sqrt(Ntotal)),alpha=0.5)+
	scale_fill_viridis_d(drop=FALSE)


data(immune_long)
ggplot(immune_long) + 
   	geom_dhb(aes(map_id=dhb_fixname(DHB),fill=pct_vax),
		alpha=0.5,colour="lightgrey") +
   	scale_fill_viridis_d(drop=FALSE)+
   	geom_label_dhb(short=TRUE,colour="black",cex=3)+
   	facet_wrap(~ethnicity)


data(dhb_cars)
tris<-tri_alloc(dhb_cars[,-1], c("0","1","2","3+"), names=dhb_cars$dhb )
tri_data<-data.frame(DHB=rep(rownames(tris),6), cars=as.vector(tris), tri_id=rep(1:6,each=nrow(tris)))

ggplot(tri_data)+
	geom_dhbtri(aes(map_id=DHB,class_id=tri_id, fill=cars))+
	scale_fill_viridis_d()
}
