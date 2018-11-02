\name{gen.upset}
\alias{gen.upset}

\title{
Antichain generating a given upset
}
\description{
The function returns the antichain generating the input upset \code{Q}, given the incidence matrix \code{z} of the poset.
}
\usage{
gen.upset(z, Q = 1)
}

\arguments{
  \item{z}{
an incidence matrix.
}
  \item{Q}{
a vector (\code{boolean}, \code{numeric} indexing elements,
or \code{character} with elements' names) identifying the input upset.
}
}
\value{
A \code{boolean} vector.
}

\seealso{
\code{\link{gen.downset}}
}
\examples{
lv <- c(2, 3, 2)
prof <- var2prof(varlen = lv)

z <- getzeta(prof)
up <- c("221", "131", "231", "222", "132", "232")
gen <- gen.upset(z, up)

plot(z, lwd = 1 + (rownames(prof$profiles)\%in\%up), col = 1 + gen,
sub = "bold = the upset, red = the antichain generating the upset")
}