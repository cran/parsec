\name{drawedges}
\alias{drawedges}

\title{
drawedges
}
\description{
Graphical function called by \code{\link{plot.cover}} to draw the edges of the
Hasse diagram representing the input cover matrix \code{C}.
}
\usage{
drawedges(C, vertices, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{
cover matrix.
}
  \item{vertices}{
coordinates of the vertices obtained by function \code{\link{vertices}}.
}
  \item{\dots}{
line parameters, see \code{\link{graphics}{lines}}.
}
}
\seealso{
\code{\link{plot.cover}}, \code{\link{vertices}}, \code{\link{graphics}{lines}}
}
