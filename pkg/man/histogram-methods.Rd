\name{histogram-methods}
\docType{methods}
\alias{histogram-methods}
\alias{histogram,PDCluster,missing-method}
\alias{histogram,PD,missing-method}
\title{Methods for Function \code{histogram} in Package \pkg{pdR}}
\description{
Methods for function \code{histogram} in package \pkg{pdR}
}
\usage{
\S4method{histogram}{PD}(x, vars)
\S4method{histogram}{PD}(x, vars, clusters)
}
\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object}
  \item{vars}{character, variables to be drawn.}
  \item{clusters}{numeric, clusters to be drawn.}

}
\value{A trellis object}
\details{
}
\references{
}
\seealso{\code{\link{lattice::histogram}}, \code{\link{useOuterStrips}},
  \code{\link{combineLimits}}, \code{\link{PD2Long}}
}
\examples{
}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
