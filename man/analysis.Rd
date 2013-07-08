\name{analysis}
\alias{analysis}

\title{
Analysis of partial discharge signals.
}
\description{
Analysis of partial discharge signals.
}
\usage{
analysis(x, ...)
}

\arguments{
  \item{x}{numeric, a partial discharge signal.}
  \item{\dots}{additional arguments for \code{\link{no0}} and \code{\link{wavVarPD}}
}
}
\details{
This function calculate the wavelet variance (\code{\link{wavVarPD}}),
maximum location, range, length, energy, zero-cross ratio
(\code{\link{nZeroCross}}) and the frequency and damping factor via the
Prony's method (\code{\link{prony}}).
}
\value{A \code{data.frame} with:
  \item{RefMax}{Maximum location.}
  \item{W1, W2, W3 and W4}{Wavelet variances of the first 4 levels.}
  \item{range}{Range of the signal.}
  \item{N}{Number of samples of the signal.}
  \item{energy}{Energy of the signal calculated as the sum of the
    squared samples.}
  \item{nZC}{Zero-cross rate.}
  \item{freq1, freq2, damp1, damp2}{Frequencies and damping factors of
    the most energetic components of the Prony's analysis.}
}

\author{Oscar Perpiñán Lamigueiro}


\seealso{
\code{\link{no0}}, \code{\link{wavVarPD}}, \code{\link{prony}},
\code{lapply}, \code{do.call}.
}
