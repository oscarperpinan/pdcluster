\name{splom-methods}
\docType{methods}
\alias{splom-methods}
\alias{splom,PD,missing-method}
\title{Methods for Function \code{splom} in Package \pkg{pdR}}
\description{
Methods for function \code{splom} in package \pkg{pdR}
}
\usage{\S4method{splom}{PD}(x, ...)}
\arguments{
  \item{x}{A \code{\linkS4class{PD}} object}
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
