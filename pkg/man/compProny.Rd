\name{compProny}
\alias{compProny}
\title{
Comparison of several Prony decompositions.
}
\description{Comparison of several Prony decompositions.}
\usage{
compProny(x, dt = 1e-08, M = seq(4, 20, 4), clean = TRUE)
}

\arguments{
  \item{x}{numeric, a vector with a partial discharge signal.}
  \item{dt}{numeric, the sampling time.}
  \item{M}{numeric, a set of numbers for different decompositions}
  \item{clean}{logical, should the signal be cleaned first with no0?}
}
\details{
}
\value{A trellis graphic.}
\references{

}
\author{Oscar Perpiñán Lamigueiro}
\note{

}

\seealso{
\code{layer}, \code{\link{prony}}, \code{\link{plot.prony}}
}
\examples{
## setwd('~/Investigacion/PD/Datos/20100922')##Con estas he hecho todo el desarrollo inicial
## load('DescargasRAW.RData')

## x=lista[[3]]

## pron1 <- prony(x, M=10)
## plot(pron1, all=FALSE)

## compProny(x, M=c(10, 20,  30, 40))

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
