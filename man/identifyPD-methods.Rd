\name{identifyPD-methods}
\docType{methods}
\alias{identifyPD}
\alias{identifyPD-methods}
\alias{identifyPD,PD-method}
\title{Methods for Function \code{identifyPD} }
\description{
Methods for function \code{identifyPD} 
}

\usage{
\S4method{identifyPD}{PD}(object, label='energy', column=1, row=1, pch=13, cex=0.6, col='darkgreen',...)
}
\arguments{
    \item{object}{A \code{PD} or \code{PDCluster} object.}
  \item{label}{character, feature to be used for identification.}
  \item{column}{numeric, see \code{\link{trellis.focus}}}
  \item{row}{numeric, see \code{\link{trellis.focus}}}
  \item{pch, cex, col, ...}{See \code{\link{panel.identify}}}
}

\value{A data.frame, a subset of the object}

\seealso{\code{\link{choosePoints}}, \code{\link{trellis.focus}}, \code{\link{panel.identify}}
}

\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
