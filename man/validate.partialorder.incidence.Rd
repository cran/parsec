\name{validate.partialorder.incidence}
\alias{validate.partialorder.incidence}

\title{
validate.partialorder.incidence
}
\description{
The function checks whether the boolean square matrix \code{m} represents a partial order. If this is the case, the function returns the same input matrix as a S3 class object \code{incidence}. Otherwise, the unfulfilled poset theoretical properties of matrix \code{m} are returned.
}
\usage{
validate.partialorder.incidence(m)
}

\arguments{
  \item{m}{
a boolean square matrix.
}
}
\seealso{
\code{\link{transitivity}}, \code{\link{binary}}, \code{\link{reflexivity}},

\code{\link{antisymmetry}}, \code{\link{is.preorder}}, \code{\link{is.partialorder}}
}
\examples{
M <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
FALSE, TRUE, TRUE, TRUE, TRUE)
M <- matrix(M, 4, 4)
rownames(M) <- colnames(M) <- LETTERS[1:4]

M <- validate.partialorder.incidence(M)

plot(M)
}
