\name{filterPD-methods}
\docType{methods}
\alias{filterPD}
\alias{filterPD-methods}
\alias{filterPD,PD-method}
\title{Simple filtering of a partial discharge signal}
\description{Filtering of a partial discharge signal.}
\usage{
\S4method{filterPD}{PD}(object,
         filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl,
         select)
}
\arguments{
    \item{object}{A \code{PD} or \code{PDCluster} object.}
    \item{filter}{logical expression indicating elements to keep. This
      expression will be evaluated in the \code{data} slot of the
      \code{object} (as in \code{subset}).}
    \item{select}{Variables to be selected. This expression will be
      evaluated in the \code{data} of the object (as in \code{subset}).}
}

\seealso{\code{\link{transformPD-methods}}, \code{subset}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
