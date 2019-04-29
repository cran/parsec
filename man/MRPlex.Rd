\name{MRPlex}
\alias{MRPlex}

\title{
Mutual ranking probabilities on the lexicographic linear extensions set
}
\description{
The function returns the mutual ranking probabilities matrix evaluated considering only the lexicographic linear extensions. Results are obtained by exact formula.
}
\usage{
MRPlex(profiles, selection = NULL)
}

\arguments{
  \item{profiles}{
an object of S3 class \code{wprof}.
}
  \item{selection}{
  a vector of \code{string} indicating a subset of profiles to evaluate the mutual ranking probabilities. If \code{NULL} the mutual ranking probabilities are evaluated for all profiles.
}
}

\value{
The MRP matrix of the selected profiles.
}
\examples{
  prf <- var2prof(varlen = c(2, 2, 2))
  MRPlex(prf)
}