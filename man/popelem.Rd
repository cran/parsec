\name{popelem}
\alias{popelem}
\alias{popelem.wprof}

\title{
popelem
}
\description{
The function identifies in a matrix \code{y}, profiles in \code{prof}. 
For each row of matrix \code{y}, the function returns the location of the
corresponding profile in object \code{prof}.
}
\usage{
popelem(prof, ...)
\method{popelem}{wprof}(prof, y, ...)
}

\arguments{
  \item{prof}{
an object of S3 class \code{wprof}.
}
  \item{y}{
a \code{matrix} or \code{data.frame} representing a set of observations with
variables (the same contained in \code{prof}) by columns.
}
  \item{\dots}{
any of the above.
}
}
\examples{
vl <- c(2, 3, 2)
prf <- var2prof(varlen = vl)
pop <- matrix(c(2, 1, 1, 1, 2, 1, 2, 3, 1), 3, 3)
rownames(pop) <- LETTERS[1:3]

v <- popelem(prof = prf, y = pop)
v
prf$profiles[v,]
}