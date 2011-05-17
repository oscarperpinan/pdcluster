\name{PDCluster-class}
\Rdversion{1.1}
\docType{class}
\alias{PDCluster-class}

\title{Class \code{"PDCluster"}}
\description{Partial discharge clusters class.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PDCluster", ...)}.
%% describe objects here
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
    %\item{\code{angle, data, refl, filtered, transfomed, refl.rm,
%	description}:}{See \code{\linkS4class{PD}}}
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
\references{
%% put references to the literature/web site here~~
}
\author{Oscar Perpiñán Lamigueiro}
\note{
%% further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% objects to See Also as \code{\link{~~fun~~}},~
%% or \code{\linkS4class{CLASSNAME}} for links to other classes~
}
\examples{
showClass("PDCluster")
}
\keyword{classes}
