\name{densityplot-methods}
\docType{methods}
\alias{densityplot}
\alias{densityplot,PDCluster,missing-method}
\alias{densityplot,PD,missing-method}
\title{Methods for Function \code{densityplot} in Package \pkg{pdCluster}}
\description{Methods for function \code{densityplot} in package \pkg{pdCluster}}
\usage{
\S4method{densityplot}{PD,missing}(x, vars, \dots)
\S4method{densityplot}{PDCluster,missing}(x, vars, clusters, \dots)
}
\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object}
  \item{vars}{character, variables to be drawn.}
  \item{clusters}{numeric, clusters to be drawn.}
  \item{\dots}{Additional arguments for \code{densityplot}.}
}

\seealso{\code{\link{densityplot}}, \code{\link{glayer}}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
