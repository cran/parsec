\name{proFreq}
\alias{proFreq}

\title{
Observed profile frequencies
}
\description{
The function computes profile frequencies, by counting the number of times a profile appears in the population.
}
\usage{
proFreq(profiles, population)
}

\arguments{
  \item{profiles}{
an object of S3 class \code{wprof}.
}
  \item{population}{
a \code{matrix} or \code{data.frame} representing a set of observations with
variables (the same contained in \code{prof}) by columns.
}
}

\value{
An object of class \code{wprof} with the same profiles of the argument but with different frequencies.
}

\author{
Alberto Arcagni
}

\examples{
vl <- c(2, 3, 2)
prf <- var2prof(varlen = vl)
pop <- matrix(c(2, 1, 1, 1, 2, 1, 2, 3, 1), 3, 3)
rownames(pop) <- LETTERS[1:3]

proFreq(profiles = prf, population = pop)
}
\seealso{
\code{\link{popelem}}
}
