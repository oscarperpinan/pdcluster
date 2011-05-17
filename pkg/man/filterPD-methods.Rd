\name{filterPD-methods}
\docType{methods}
\alias{filterPD}
\alias{filterPD-methods}
\alias{filterPD,PD-method}
\title{Simple filtering of a partial discharge signal}
\description{Simple filtering of a partial discharge signal.}
\usage{
\S4method{filterPD}{PD}(object,
         filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7,
         refl.rm=TRUE)
}
\arguments{
    \item{object}{A \code{PD} or \code{PDCluster} object.}
\item{filter}{logical expression indicating elements to keep (see
  \code{\link{subset}})}
  \item{refl.rm}{logical, should be reflections be removed?}
}
% \section{Methods}{
% \describe{

% \item{\code{signature(object = "PD")}}{

% }
% }}
\seealso{\code{\link{transformPD-methods}}, \code{\link{subset}}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
