\name{df2PD}
\alias{df2PD}

\title{Conversion from \code{data.frame} to a  \code{\linkS4class{PD}} object.}
\description{Conversion from \code{data.frame} to a  \code{\linkS4class{PD}} object.
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


\value{A \code{\linkS4class{PD} object.}}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
\code{\link{analysis}}
}
