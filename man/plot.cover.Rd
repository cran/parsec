\name{plot.cover}
\alias{plot.cover}
\alias{plot.incidence}
\alias{plot.wprof}

\title{
Hasse diagram
}
\description{
\code{plot} methods to draw Hasse diagrams, for objects of S3 classes \code{wprof}, \code{incidence}, \code{cover}, 
}
\usage{
\method{plot}{wprof}(x,  shape = c("square", "circle", "equispaced"), noise = FALSE, ...)
\method{plot}{incidence}(x, shape = c("square", "circle", "equispaced"), noise = FALSE, ...)
\method{plot}{cover}(x, shape = c("square", "circle", "equispaced"), noise = FALSE,
    pch = 21, cex = max(nchar(rownames(x))) + 2, bg = "white", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of S3 class \code{wprof}, an object of S3 class \code{incidence} or an object of S3 class \code{cover}.
}
  \item{shape}{
shape of the Hasse diagram. See \code{\link{vertices}}.
},
  \item{noise}{
jittering in the shape of the Hasse diagram. See \code{\link{vertices}}.
}
  \item{pch}{
graphical parameter. See \code{\link[graphics]{plot.default}}.
}
  \item{cex}{
graphical parameter. See \code{\link[graphics]{plot.default}}.
}
  \item{bg}{
graphical parameter. See \code{\link[graphics]{plot.default}}.
}
  \item{\dots}{
further optional graphical parameters. See \code{\link[graphics]{plot.default}}.
}
}

\examples{
prf <- var2prof(varlen = c(5, 5, 5))
prf$freq <- sample(c(rep(0, 20), 1, 2, 3), 5*5*5, replace = TRUE)
prf <- obsprof(prf)

z <- getzeta(prf)

plot(z, shape = "equispaced", col = prf$freq, lwd = 2)
}

