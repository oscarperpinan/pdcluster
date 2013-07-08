\name{hexbinplot-methods}
\docType{methods}
\alias{hexbinplot}
\alias{hexbinplot,PD,missing-method}
\alias{hexbinplot,PDCluster,missing-method}

\title{Hexbinplot methods in the \code{pdCluster} package.}
\description{Hexbinplot for \code{PD} and \code{PDCluster} classes.
}
\usage{
\S4method{hexbinplot}{PD,missing}(x, plot.refl=TRUE, \dots)
\S4method{hexbinplot}{PDCluster,missing}(x, clusters, panelClust=TRUE, plot.refl=TRUE, \dots)
}

\arguments{
  \item{x}{A \code{PD} or \code{PDCluster} object}
  \item{clusters}{numeric, clusters to be drawn.}
  \item{panelClust}{logical, if TRUE each cluster is drawn in a separate
    panel.}
  \item{plot.refl}{logical, should be the reflections be drawn
    in separate panels?}
  \item{\dots}{Additional arguments for \code{hexbinplot}.}

}
\value{A trellis graphic.
}

\seealso{\code{\link{hexbinplot}}
}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}
