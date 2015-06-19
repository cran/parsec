\name{linzeta}
\alias{linzeta}

\title{
linzeta
}
\description{
The function computes the incidence matrix of the linear order defined by the rank vector \code{lin}. It returns an object of S3 class \code{incidence}.
}
\usage{
linzeta(lin)
}

\arguments{
  \item{lin}{
a  of profile ranks.
}
}
\examples{
ranks <- c(5, 3, 4, 2, 1)
names(ranks) <- LETTERS[1:5]
linzeta(ranks)
plot(linzeta(ranks))
}