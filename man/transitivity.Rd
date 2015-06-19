\name{transitivity}
\alias{transitivity}

\title{
transitivity
}
\description{
The function checks whether the boolean square matrix \code{m} representes a transitive binary relation.
}
\usage{
transitivity(m)
}

\arguments{
  \item{m}{
a boolean square matrix.
}
}
\seealso{
\code{\link{binary}}, \code{\link{reflexivity}}, \code{\link{antisymmetry}},

\code{\link{is.preorder}}, \code{\link{is.partialorder}},

\code{\link{validate.partialorder.incidence}}
}
\examples{
M <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
FALSE, TRUE, TRUE, TRUE, TRUE)
M <- matrix(M, 4, 4)
rownames(M) <- colnames(M) <- LETTERS[1:4]

transitivity(M)
}