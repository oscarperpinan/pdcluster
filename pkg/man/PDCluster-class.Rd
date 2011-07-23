\name{PDCluster-class}
\Rdversion{1.1}
\docType{class}
\alias{PDCluster-class}
\alias{show,PDCluster-method}

\title{Class \code{"PDCluster"}}
\description{Partial discharge clusters class.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PDCluster", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{cluster}:}{numeric, number of cluster to which each row
      belongs.}
    \item{\code{nSims}}{numeric, number of \code{\link{clara}} executions.}
    \item{\code{nClusters}}{numeric, number of clusters.}
    \item{\code{metric}}{character, metric for the \code{\link{clara}}
      function.}
    \item{\code{noise.level}}{numeric, percentage threshold to consider a
      classification result as noise.}
    \item{\code{noise.rm}}{logical, have the noisy results been removed?}
    \item{\code{dist}:}{Object of class \code{"data.frame"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{PD}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "PDCluster")}: ... }
    \item{densityplot}{\code{signature(x = "PDCluster", data = "missing")}: ... }
    \item{histogram}{\code{signature(x = "PDCluster", data = "missing")}: ... }
    \item{PD2Long}{\code{signature(object = "PDCluster")}: ... }
    \item{show}{\code{signature(object = "PDCluster")}: ... }
    \item{xyplot}{\code{signature(x = "PDCluster", data = "missing")}: ... }
	 }
}

\author{Oscar Perpiñán Lamigueiro}

\keyword{classes}
