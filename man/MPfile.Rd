\name{MPfile}
\alias{MPfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Multiset Permutation, store in file }
\description{
  Label switching algorithm, allow multiple groups.
}
\usage{
MPfile(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{ A vector of integer as group sizes }
}
\details{
  The output is stored in "output.txt" in the current working directory.
}
\value{
Null
}

\author{ Kun Liang }

\examples{
MPfile(c(2,3))
}
