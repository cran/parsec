\name{plot.average_ranks}
\alias{plot.average_ranks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Method od function \code{plot} for objects of class \code{average_ranks}
}
\description{
Starting from the output of the function \code{average_ranks}, it provides a plot that for each each element the poset shows the average rank and the rank interval.
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
A boolean attribute that indicates if the interval is plotted in background (\code{TRUE}) or in foreground (\code{FALSE}).
}
  \item{range.col}{
Color of the interval.
}
  \item{range.lty}{
The type of the lines representing the range, the values are the same of the attribute \code{lty} of the \code{\link{plot.default}} function.
}
  \item{range.lwd}{
Width of the lines representing the range.
}
  \item{type}{
Attribute of the function \code{\link{plot.default}}, here \code{"p"} by default.
}
  \item{ylim}{
Attribute of the function \code{\link{plot.default}}, here \code{c(max(x$sup), 1)} by default, that means that the axis of the ordinates is reversed, since rank 1 usually means that the element of the poset is "more important".
}
  \item{xlab}{
Attribute of the function \code{\link{plot.default}}, here \code{""} by default.
}
  \item{ylab}{
Attribute of the function \code{\link{plot.default}}, here \code{"Average rank"} by default.
}
  \item{pch}{
Attribute of the function \code{\link{plot.default}}, here \code{c(16, 3, 3)} by default. This method uses the \code{\link{matplot}} function to plot the average ranks and their range. The first first value is referred to the point character of the average rank, the other two are referred to the point characters of the range.
}
  \item{col}{
Attribute of the function \code{\link{plot.default}}, here \code{""black""} by default. The average ranks and their ranges are of the same colors, but similarly to the \code{pch}, it is possible to provide a vector to differentiate their colors.
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