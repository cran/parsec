\name{vertices}
\alias{vertices}

\title{
Coordinates of the vertices of the Hasse diagram, representing the input cover relation.
}
\description{
The function computes the coordinates of the vertices of the Hasse diagram.
}
\usage{
vertices(C, shape = c("square", "circle", "equispaced"), noise = FALSE)
}

\arguments{
  \item{C}{
cover matrix, an object of class S3 \code{cover}.
}
  \item{shape}{
shape of the diagram. See details.
}
  \item{noise}{
some jittering on the x axis, so as to improve readability. Values can be boolean or positive values, to get different jittering intensities.
  }
}
\details{
Possible Hasse diagram shapes: \code{square}; \code{circle}; \code{equispaced}. The last option is suggested when the poset has more than one maximal or minimal elements. The function is used by the plot methods defined in the package (see \code{\link{plot.cover}}).
}

\seealso{
\code{\link{plot.cover}}
}