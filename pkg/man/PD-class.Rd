\name{PD-class}
\Rdversion{1.1}
\docType{class}
\alias{PD-class}
\alias{as.data.frame,PD-method}
\alias{claraPD,PD-method}
\alias{densityplot,PD,missing-method}
\alias{filterPD,PD-method}
\alias{histogram,PD,missing-method}
\alias{identifyPD,PD-method}
\alias{isFiltered,PD-method}
\alias{isTransformed,PD-method}
\alias{PD2Long,PD-method}
\alias{show,PD-method}
\alias{splom,PD,missing-method}
\alias{transformPD,PD-method}
\alias{xyplot,PD,missing-method}

\title{Class \code{"PD"}}
\description{
%% A concise (1-5 lines) description of what the class is.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{\link{df2PD(x, ...)}}
}
\section{Slots}{
  \describe{
    \item{\code{angle}:}{numeric, reference angle (0:360)}
    \item{\code{data}:}{data.frame, result of the \code{\link{analysis}} function.}
    \item{\code{refl}:}{logical, is the signal of this row a reflection?}
    \item{\code{filtered}:}{logical, has been filtered with \code{\link{filterPD}}?}
    \item{\code{transformed}:}{logical, has been filtered with \code{\link{filterPD}}?}
    \item{\code{refl.rm}:}{logical, TRUE if the reflections were erased
      be \code{\link{filterPD}}}
    \item{\code{description}:}{character}
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "PD")}: ... }
    \item{claraPD}{\code{signature(object = "PD")}: ... }
    \item{densityplot}{\code{signature(x = "PD", data = "missing")}: ... }
    \item{filterPD}{\code{signature(object = "PD")}: ... }
    \item{histogram}{\code{signature(x = "PD", data = "missing")}: ... }
    \item{identifyPD}{\code{signature(object = "PD")}: ... }
    \item{isFiltered}{\code{signature(object = "PD")}: ... }
    \item{isTransformed}{\code{signature(object = "PD")}: ... }
    \item{PD2Long}{\code{signature(object = "PD")}: ... }
    \item{show}{\code{signature(object = "PD")}: ... }
    \item{splom}{\code{signature(x = "PD", data = "missing")}: ... }
    \item{transformPD}{\code{signature(object = "PD")}: ... }
    \item{xyplot}{\code{signature(x = "PD", data = "missing")}: ... }
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
}
\examples{
showClass("PD")
}
\keyword{classes}
