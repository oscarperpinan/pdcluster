\name{as.data.frame-methods}
\docType{methods}
\alias{as.data.frame-methods}
\alias{as.data.frame,PD-method}
\alias{as.data.frame,PDCluster-method}
\title{Methods for Function \code{as.data.frame} in Package \pkg{pdR}}
\description{
Methods for function \code{as.data.frame} in package \pkg{pdR}}
\section{Methods}{
\describe{
\item{\code{signature(x = "PD")}}{
A data.frame with the content of \code{data}, \code{angle} and
\code{refl} slots.
}

\item{\code{signature(x = "PDCluster")}}{
A data.frame with the content of \code{data}, \code{angle} and
\code{refl}, \code{cluster} and \code{dist} slots.

}
}}
\author{Oscar Perpiñán Lamigueiro}
\keyword{methods}
