\name{analysis}
\alias{analysis}

\title{
Analysis of partial discharge signals.
}
\description{
Analysis of partial discharge signals.
}
\usage{
analysis(x, threshold = 8e-3, n.levels = 4, ...)
}

\arguments{
  \item{x}{numeric, a partial discharge signal.}
 \item{threshold}{numeric, relative value. The threshold is calculated
  with the product of this value and the absolute maximum value of
  \code{x}. See \code{\link{no0}}}
\item{n.levels}{numeric, number of wavelet levels}
  \item{\dots}{additional arguments for \code{\link{wavVarPD}}
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
  \item{W1, W2, ...}{Wavelet variances of the first \code{n.levels} levels.}
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
