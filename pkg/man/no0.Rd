\name{no0}
\alias{no0}

\title{Supression of zeros.}
\description{Suppress head and tail of a vector if the values are below a threshold.
}
\usage{
no0(x, threshold = 0.008)
}

\arguments{
  \item{x}{numeric vector}
  \item{threshold}{numeric, relative value. The threshold is calculated
  with the product of this value and the absolute maximum value of \code{x}}
}

\value{A numeric vector}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
\code{\link{rle}}
}

