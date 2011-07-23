\name{histogram-methods}
\docType{methods}
\alias{histogram}
\alias{histogram,PDCluster,missing-method}
\alias{histogram,PD,missing-method}
\title{Methods for Function \code{histogram} in Package \pkg{pdCluster}}
\description{
Methods for function \code{histogram} in package \pkg{pdCluster}
}
\usage{
\S4method{histogram}{PD,missing}(x, vars, \dots)
\S4method{histogram}{PDCluster,missing}(x, vars, clusters, \dots)
}
\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object}
  \item{vars}{character, variables to be drawn.}
  \item{clusters}{numeric, clusters to be drawn.}
  \item{\dots}{Addtional arguments for \code{histogram}}

}
\value{A trellis object}
\details{
}
\references{
}
\seealso{\code{\link{histogram}}, \code{\link{useOuterStrips}},
  \code{\link{combineLimits}}, \code{\link{PD2Long-methods}}
}
\examples{
}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
