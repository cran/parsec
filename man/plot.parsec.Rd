\name{plot.parsec}
\alias{plot.parsec}

\title{
Plot outouts of PARSEC function \code{\link{evaluation}}.
}

\description{
Different representation of the results obtained by the evaluation function.
}

\usage{
\method{plot}{parsec}(
    x,
    which = c("Hasse", "threshold", "identification", "rank", "gap"),
    ask = dev.interactive(),
    shape = c("square", "circle", "equispaced"),
    noise = FALSE,
    ...
)
}

\arguments{
  \item{x}{
  an object of S3 class \code{parsec}, output of the \code{\link{evaluation}} function.
}
  \item{which}{
the names of the graphs to be plotted (all by default), the user can choose between
\itemize{
\item{\code{Hasse}, the Hasse diagram of the poset, see \code{\link{plot.cover}} for details,}
\item{\code{threshold}, the plot relative frequencies of the times a profile is used as threshold
in the sampled linear extensions. This result is useful for posterior valuation of the poset threshold,}
\item{\code{rank}, representation of the rank distribution for each profile (X-axis), through bar-plots, where the heights of
the blocks represents the relative frequencies (total height equal to one for all the profiles) and the color represents the rank:
white rank one, black the higher rank and a gray scale for the middle ranks,}
\item{\code{gap}, a unified representation of the point relative poverty gap (from zero downward) and of the point relative wealth gap (from zero upward).
The horizontal lines represents the average poverty gap and the average wealth gap. The darker vertical dashed lines represent
the threshold profiles.}
}
}
  \item{ask}{
  boolean value indicating whether the system has to ask before changing the plot.
}
  \item{shape}{
  the shape of the Hasse diagram, see \code{\link{plot.cover}} for details.
  }
  \item{noise}{
  noise in the shape of the Hasse diagram. See \code{\link{vertices}}.
}
  \item{\dots}{
  further arguments for the \code{\link{plot.cover}} function.
}
}

\seealso{
\code{\link{evaluation}}, \code{\link{plot.cover}}
}
\examples{
profiles <- var2prof(varlen = c(3, 2, 4))
threshold <- c("311", "112")

res <- evaluation(profiles, threshold, nit = 10^3)

plot(res)
}