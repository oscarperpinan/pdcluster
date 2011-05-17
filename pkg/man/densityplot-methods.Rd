\name{densityplot-methods}
\docType{methods}
\alias{densityplot-methods}
\alias{densityplot,PDCluster,missing-method}
\alias{densityplot,PD,missing-method}
\title{Methods for Function \code{densityplot} in Package \pkg{pdR}}
\description{Methods for function \code{densityplot} in package \pkg{pdR}}
\usage{
\S4method{densityplot}{PD}(x, vars)
\S4method{densityplot}{PDCluster}(x, vars, clusters)
}
\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object}
  \item{vars}{character, variables to be drawn.}
  \item{clusters}{numeric, clusters to be drawn.}
}

% \section{Methods}{
% \describe{

% \item{\code{signature(x = "PDCluster", data = "missing")}}{
% }

% \item{\code{signature(x = "PD", data = "missing")}}{
% }
% }}
\seealso{\code{\link{lattice::densityplot}}, \code{\link{glayer}}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
