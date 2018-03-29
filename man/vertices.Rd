\name{vertices}
\alias{vertices}

\title{
Coordinates of the vertices of the Hasse diagram representing the input cover relation.
}
\description{
The function returns the coordinates of the vertices of the Hasse diagram.
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
some noise on the axis of ascites in order to distinguish possible overlapping relations. Values can be boolean or positive values to obtain different noise intensities.
  }
}
\details{
Possible shapes of the diagram are: \code{square}; \code{circle}; \code{equispaced}. The last option is suggested when the poset has more than one maximal or minimal elements. The function is used by the plot methods defined in the package (see \code{\link{plot.cover}}).
}

\seealso{
\code{\link{plot.cover}}
}