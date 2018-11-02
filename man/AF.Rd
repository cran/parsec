\name{AF}
\alias{AF}
\alias{AF.default}
\alias{AF.wprof}

\title{
OPHI counting approach
}
\description{
The function implements the OPHI
counting approach, in a single call. The implementation is limited to ordinal attributes.
}
\usage{
AF(y, ...)
\method{AF}{default}(y, z, w=rep(1, ncol(y)), k=sum(w), freq=rep(1, nrow(y)), ...)
\method{AF}{wprof}(y, ...)
}

\arguments{
  \item{y}{
matrix of profiles, possibly substituted by an object of class \code{wprof}.
}
  \item{z}{
vector of attribute cutoffs.
}
  \item{w}{
variables' weights.
}
  \item{k}{
overall cutoff.
}
  \item{freq}{
profiles' frequencies; the argument can be omitted if \code{y} is an object of class \code{wprof}.
.
}
  \item{\dots}{
any of the above.
}
}
\value{
An object of S3 class \code{ophi} containing all the outputs related to the OPHI counting approach. The object is a \code{list}
comprising:
\item{y}{
matrix of profiles,
}
\item{freq}{
profiles' frequencies,
}
\item{d}{
number of variables
}
\item{n}{
number of observations (sum of frequencies),
}
\item{z}{
vector of cutoffs,
}
\item{k}{
overall cutoff,
}
\item{rho}{
function comparing profiles to the vector of cutoffs,
}
\item{rho_k}{
function comparing profiles to the overall cutoff, by weighting
variables,
}
\item{g0}{
profile-variable matrix reporting the output of function
\code{rho},
}
\item{c}{
censored vector of deprivation counts,
}
\item{Z_k}{
boolean vector identifying deprived profiles, according to the specified cutoffs,
}
\item{q}{
number of poor statstical units in the population,
}
\item{H}{
headcount ratio, i.e. \code{q/n}, where \code{n} is the number of statistical units in the population,
}
\item{A}{
average deprivation share,
}
\item{M0}{
adjusted headcount ratio.
}
}
\references{
Alkire S., Foster J. (2011), Counting and multidimensional poverty measurement, Journal of Public Economics, 96(7-8), 476-487.
}
\examples{
vl <- c(2, 3, 3, 2)
prof <- var2prof(varlen = vl)

res <- AF(prof, z = c(1, 2, 1, 1), k = 1)

res
}