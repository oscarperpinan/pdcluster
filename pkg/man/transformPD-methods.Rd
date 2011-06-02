\name{transformPD-methods}
\docType{methods}
\alias{transformPD}
\alias{transformPD-methods}
\alias{transformPD,PD-method}
\title{Methods for Function \code{transformPD} }
\description{
This method apply a transformation to each variable of a \code{PD}
object. If a function is not supplied, the method selects a suitable
transformation from the Box-Cox family.
}

\usage{\S4method{transformPD}{PD}(object, fun)}
\arguments{\itemize{
    \item{object: }{A \code{PD} or \code{PDCluster} object.}
    \item{fun: }{A function to be applied. If it is missing, then a
      suitable Box-Cox transformation is selected for each variable. See
      \code{\link{powerTransform}}}
    }
}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
