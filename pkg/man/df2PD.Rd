\name{df2PD}
\alias{df2PD}

\title{Conversion from \code{data.frame} to a  \code{\linkS4class{PD}} object.}
\description{
}
\usage{
df2PD(x,
 select = c("RefMax", 
 "W1", "W2", "W3", "W4",
 "range", "N",
 "energy", "nZC",
 "freq1", "damp1"),
 angle = "angulo", refl = "separacionOriginal",
 description = "")
}

\arguments{
  \item{x}{
A data.frame.
}
  \item{select}{
character, columns of the \code{data.frame} to be selected.
}
  \item{angle}{character, name of the column where the information of
  angle is stored.
}
  \item{refl}{character, name of the column where the separation of a
  signal from its original. It the value in this column is 0, the signal
  is regarded as the original one.
}
  \item{description}{character}
}

\details{
%% If necessary, more details than the description above
}
\value{A \code{\linkS4class{PD} object.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{Oscar Perpiñán Lamigueiro}

\note{
%% further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{analysis}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
