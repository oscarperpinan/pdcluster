\name{subset}
\alias{subset-methods}
\alias{subset,PD-method}
\title{Subset of a Partial Discharge object.}
\description{Subset of a partial discharge object.
}

\usage{\S4method{subset}{PD}(x, subset, select, drop=FALSE,\dots)
}
\arguments{
  \item{x}{A \code{PD} object.}
  \item{subset}{A condition based on variables of the \code{PD} object,
    including \code{angle}, \code{refl}, \code{cluster} and
    \code{dist}.}
  \item{select}{Variables of the \code{data} slot to be selected. The
    variable \code{energy} must be included if you wish to use the
    \code{\link{xyplot}} method.}
  \item{drop}{Not used.}
  \item{\dots}{Not used.}
}
\value{A \code{PD} or \code{PDCluster} object.}


\seealso{\code{\link{subset}}, \code{\link{filterPD-methods}}
}
\examples{
}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}

