\name{prony}
\alias{prony}

\title{
Prony's method
}
\description{Prony's method
}
\usage{
prony(x, M = 6, dt = 1e-08, clean = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric
}
  \item{M}{numeric, number of complex exponentials
}
  \item{dt}{numeric, sampling time of \code{x}
}
  \item{clean}{logical, should x be cleaned with \code{\link{no0}} before the decomposition?
}
}
\details{

}
\value{A \code{list} of class \code{prony} with:
  \item{damp}{numeric, damping factors of each component.}
  \item{freq}{numeric, frequency of each component.}
  \item{energy}{numeric, relative energy of each component.}
  \item{comp}{\code{data.frame}, each column being one of the components
    of the signal.}
  
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Oscar Perpiñán Lamigueiro}
\note{The result is ordered by the relative energy of the components.
}

\seealso{
\code{\link{plot.prony}}, \code{\link{compProny}}, \code{\link{hankel}}, \code{\link{solve}}
\code{\link{polyroot}}, \code{\link{qr.solve}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
