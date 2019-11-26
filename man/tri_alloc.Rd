\name{tri_alloc}
\alias{tri_alloc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Allocate triangles within hexagons
}
\description{
This implements the Webster/Sainte-Laguë method to allocate six triangles in each hexagon, in proportion to counts in two or more classes. 
}
\usage{
tri_alloc(countmatrix, colours, names = rownames(countmatrix))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{countmatrix}{
A matrix of counts with a column for each class and a row for each hexagon. 
}
  \item{colours,classes}{
A vector of class names, or colour names with one for each class
}
  \item{names}{
A vector of names, for each row of \code{countmatrix}, to be applied to the output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Matrix of class names, with six columns.  The rownames will be set to \code{names} if provided.
}

\examples{
data(dhb_cars)
tri_alloc(dhb_cars[,-1], c("green","gold","orange","goldenrod"), names=dhb_cars$dhb )

}
