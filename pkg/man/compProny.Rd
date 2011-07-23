\name{compProny}
\alias{compProny}
\title{
Comparison of several Prony decompositions.
}
\description{Comparison of several Prony decompositions.}
\usage{
compProny(x, dt = 1e-08, M = seq(4, 20, 4), clean = TRUE)
}

\arguments{
  \item{x}{numeric, a vector with a partial discharge signal.}
  \item{dt}{numeric, the sampling time.}
  \item{M}{numeric, a set of numbers for different decompositions}
  \item{clean}{logical, should the signal be cleaned first with no0?}
}

\value{A trellis graphic.}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
\code{layer}, \code{\link{prony}}, \code{\link{xyplot.prony}}
}
