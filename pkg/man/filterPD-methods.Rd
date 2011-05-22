\name{filterPD-methods}
\docType{methods}
\alias{filterPD}
\alias{filterPD-methods}
\alias{filterPD,PD-method}
\title{Simple filtering of a partial discharge signal}
\description{Simple filtering of a partial discharge signal. This
  function is a wrapper of \code{\link{subset,PD-method}} with a
predefined filter passed to the \code{subset} condition.}
\usage{
\S4method{filterPD}{PD}(object,
         filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
}
\arguments{
    \item{object}{A \code{PD} or \code{PDCluster} object.}
    \item{filter}{logical expression indicating elements to keep (see
  \code{\link{subset}})}
}
% \section{Methods}{
% \describe{

% \item{\code{signature(object = "PD")}}{

% }
% }}
\seealso{\code{\link{transformPD-methods}}, \code{\link{subset,PD-method}}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
