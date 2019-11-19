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
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping at the top level of the plot.  The \code{radius} aesthetic controls the size of the hexes}
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

## size variation, plus stacking to show the full-size hexes underneath
ggplot(immune)+
	geom_dhb(aes(map_id=dhb_fixname(DHB)),fill="white",colour="grey",coord=FALSE)+
	geom_dhb(aes(fill=vax_pct,map_id=dhb_fixname(DHB),radius=sqrt(Ntotal)),alpha=0.5)+
	scale_fill_viridis_d(drop=FALSE)+
	geom_label_dhb(short=TRUE,colour="black")


data(immune_long)
ggplot(immune_long) + 
   	geom_dhb(aes(map_id=dhb_fixname(DHB),fill=pct_vax),
		alpha=0.5,colour="lightgrey") +
   	scale_fill_viridis_d(drop=FALSE)+
   	geom_label_dhb(short=TRUE,colour="black",cex=3)+
   	facet_wrap(~ethnicity)
}
