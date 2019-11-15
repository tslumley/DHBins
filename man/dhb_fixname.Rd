\name{dhb_fixname}
\alias{dhb_fixname}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardise DHB names
}
\description{
Converts popular variants of the DHB names into the versions used as keys in this package 
}
\usage{
dhb_fixname(names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
vector of strings with DHB names
}
}

\value{
vector of strings with standardised DHB names

}


\examples{
dhb_fixname( c("Hawkes Bay","Capital & Coast","Counties"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
