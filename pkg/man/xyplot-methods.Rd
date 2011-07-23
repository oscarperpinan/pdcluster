\name{xyplot-methods}
\docType{methods}
\alias{xyplot}
\alias{xyplot,PDCluster,missing-method}
\alias{xyplot,PD,missing-method}
\title{Methods for Function \code{xyplot} in Package \pkg{pdCluster}}
\description{
Methods for function \code{xyplot} in package \pkg{pdCluster}
}
\usage{
\S4method{xyplot}{PD,missing}(x, plot.refl = TRUE, \dots)
\S4method{xyplot}{PDCluster,missing}(x, distances, clusters,
                 plot.refl = TRUE, panelClust = TRUE, \dots)
}
\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object.}
  \item{distances}{numeric, set of distances to be plot.}
  \item{clusters}{numeric, set of clusters to be plot.}
  \item{plot.refl}{logical, should be the reflections be drawn in
    separate panels?}
  \item{panelClust}{logical, if TRUE each cluster is drawn in a separate
  panel.}
  \item{\dots}{Additional arguments for \code{\link{xyplot}}}
}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
