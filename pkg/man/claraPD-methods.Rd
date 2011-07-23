\name{claraPD-methods}
\docType{methods}
\alias{claraPD}
\alias{claraPD-methods}
\alias{claraPD,PD-method}
\title{Clustering of Partial Discharge signals}
\description{Clustering of Partial Discharge signals with the clara method}
\usage{\S4method{claraPD}{PD}(object, vars, nSims=25, nCl=7, metric='manhattan',
            noise.level=0.7, noise.rm=TRUE,
            seed=421)
}
\arguments{
  \item{object}{A \code{PD} or \code{PDCluster} object.}
  \item{vars}{Character vector with the names of the variables to be
    used.}
  \item{nSims}{numeric, number of \code{\link{clara}} executions.}
    \item{nCl}{numeric, number of clusters.}
    \item{metric}{character, metric for the \code{\link{clara}}
      function.}
    \item{noise.level}{numeric, percentage threshold to consider a
      classification result as noise.}
    \item{noise.rm}{logical, should the noise results be removed?}
    \item{seed}{numeric, \code{\link{set.seed}}}
}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
