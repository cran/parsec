\name{plot.parsec}
\alias{plot.parsec}

\title{
Plot the outputs of the PARSEC function \code{\link{evaluation}}.
}

\description{
Several representations of the results provided by the evaluation function.
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
the names of the graphs to be plotted (\code{all}, by default); the user can choose among
\itemize{
\item{\code{Hasse}, the Hasse diagram of the poset, see \code{\link{plot.cover}} for details,}
\item{\code{threshold}, the relative frequencies of the times a profile is used as threshold
in the sampled linear extensions.}
\item{\code{rank}, barplot providing the rank distribution of each profile (X-axis). The heights of
the blocks represent relative frequencies (the sum of the heights over profiles is equal to 1) and the color represents the rank:
white for rank one, black for the highest rank and a gray scale for intermediate ranks.}
\item{\code{gap}, a unified representation of the relative (e.g. poverty) gap and of the relative (e.g. wealth) gap.
The horizontal lines represent the average (e.g. poverty) gap and the average (e.g. wealth gap). The darker vertical dashed lines represent
the threshold profiles.}
}
}
  \item{ask}{
  boolean value indicating whether the system has to ask users before changing the plot.
}
  \item{shape}{
  the shape of the Hasse diagram, see \code{\link{plot.cover}} for details.
  }
  \item{noise}{
  jittering in the shape of the Hasse diagram. See \code{\link{vertices}}.
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