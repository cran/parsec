\name{plot.average_ranks}
\alias{plot.average_ranks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Method of function \code{plot} for objects of class \code{average_ranks}
}
\description{
From the output of the function \code{average_ranks}, the function plots the average rank and the associated rank interval, for each element of the poset.
}
\usage{
\method{plot}{average_ranks}(x,
    range.first = TRUE, range.col = "black", range.lty = 1,
    range.lwd = 1, type = "p", ylim = c(max(x$sup), 1),
    xlab = "", ylab = "Average rank", pch = c(16, 3, 3),
    col = "black", cex = c(1, 1, 1), ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{average_ranks}.
}
  \item{range.first}{
A boolean attribute to specify whether the interval is plotted in background (\code{TRUE}) or in foreground (\code{FALSE}).
}
  \item{range.col}{
Color of the interval.
}
  \item{range.lty}{
The line type to represent the range; the values are the same of the attribute \code{lty} in the \code{\link{plot.default}} function.
}
  \item{range.lwd}{
Width of the lines representing the range.
}
  \item{type}{
Attribute of the function \code{\link{plot.default}}, here \code{"p"} by default.
}
  \item{ylim}{
Attribute of function \code{\link{plot.default}}, here \code{c(max(x$sup), 1)} by default (this way, the Y-axis
is reversed, so that rank 1 corresponds to "best").
}
  \item{xlab}{
Attribute of the function \code{\link{plot.default}}, here \code{""} by default.
}
  \item{ylab}{
Attribute of the function \code{\link{plot.default}}, here \code{"Average rank"} by default.
}
  \item{pch}{
Attribute of the function \code{\link{plot.default}}, here \code{c(16, 3, 3)} by default. This method uses the \code{\link{matplot}} function to plot the average ranks and their range. The first value refers to the point character of the average rank, the other two to the point characters of the range.
}
  \item{col}{
Attribute of the function \code{\link{plot.default}}, here \code{"black"} by default. The average ranks and their ranges are of the same color, but similarly to \code{pch}, users can provide a vector of different colors.
}
  \item{cex}{
Attribute of the function \code{\link{plot.default}}, here \code{c(1, 1, 1)} by default.
}
  \item{\dots}{
Other arguments of the function \code{\link{plot.default}}.
}
}

\seealso{
\code{\link{average_ranks}}, \code{\link{plot.default}}, \code{\link{matplot}}
}
\examples{
profiles <- var2prof(varlen = c(3, 2, 4))
Z <- getzeta(profiles)
res <- average_ranks(Z)
plot(res)
}