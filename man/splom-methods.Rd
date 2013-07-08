\name{splom-methods}
\docType{methods}
\alias{splom}
\alias{splom,PD,missing-method}
\title{Methods for Function \code{splom} in Package \pkg{pdCluster}}
\description{
Methods for function \code{splom} in package \pkg{pdCluster}
}
\usage{\S4method{splom}{PD,missing}(x, data,\dots)}
\arguments{
  \item{x}{A \code{\linkS4class{PD}} object}
  \item{data}{Not used.}
  \item{\dots}{Additional arguments for \code{splom}}
  }
\section{Methods}{
\describe{
\item{\code{signature(x = "PD", data = "missing")}}{See
  \code{\link{splom}}. \code{\link{panel.hexbinplot}} is used for the
  upper panels and \code{\link{panel.loess}} for the lower panels. The
  diagonal panels include a density plot por each variable.
}
}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
