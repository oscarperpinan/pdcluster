\name{PD-class}
\Rdversion{1.1}
\docType{class}
\alias{PD-class}
\alias{show,PD-method}


\title{Class \code{"PD"}}
\description{Partial discharge features class.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{df2PD(x, ...)}
}
\section{Slots}{
  \describe{
    \item{\code{angle}:}{numeric, reference angle (0:360)}
    \item{\code{data}:}{data.frame, result of the \code{\link{analysis}} function.}
    \item{\code{refl}:}{logical, is the signal of this row a
      reflection?}
    \item{\code{filter}:}{\code{call}, condition used to filter the object
      with \code{\link{filterPD-methods}}.}
    \item{\code{filtered}:}{logical, has been filtered with \code{\link{filterPD-methods}}?}
    \item{\code{transformed}:}{logical, has been filtered with \code{\link{filterPD-methods}}?}
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

\author{Oscar Perpiñán Lamigueiro}

\keyword{classes}
