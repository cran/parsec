\name{gen.downset}
\alias{gen.downset}

\title{
Antichain generating a given downset
}
\description{
The function returns the antichain generating the input downset \code{Q}, given the incidence matrix \code{z} of the poset.
}
\usage{
gen.downset(z, Q = 1)
}

\arguments{
  \item{z}{
an incidence matrix.
}
  \item{Q}{
a vector (\code{boolean}, \code{numeric} indexing profiles,
or \code{character} with profile names) identifying the input downset.
}
}
\value{
A \code{boolean} vector.
}

\seealso{
\code{\link{gen.upset}}
}
\examples{
lv <- c(2, 3, 2)
prof <- var2prof(varlen = lv)

z <- getzeta(prof)
down <- c("111", "211", "112", "212")
gen <- gen.downset(z, down)

plot(z, lwd = 1 + (rownames(prof$profiles)\%in\%down), col = 1 + gen,
sub = "bold = the downset, red = the antichain generating the downset")
}