\name{merge.wprof}
\alias{merge.wprof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge two sets of profiles.
}
\description{
Method of the function \code{merge} of package \code{base} to merge two objects of class \code{wprof} generated through functions \code{var2prof} or \code{pop2prof}.
}
\usage{
\method{merge}{wprof}(x, y, support = FALSE, FUN = "+", all = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
objects of class \code{wprof} to be coerced to one.
}
  \item{support}{
boolean variables specifying whether \code{y} is the support of \code{x} (\code{FALSE} by default).
}
  \item{FUN}{
function to be applied to the profiles' frequencies (by default, FUN = \code{sum}). It is ignored if \code{support} is \code{TRUE}.
}
  \item{all}{
same argument of function \code{\link{merge}}, by default set to \code{TRUE}, to get all possible profiles. If a profile is not observed in the data, its frequency is set to 0.
}
  \item{\dots}{
additional arguments to be passed to method \code{\link{merge.data.frame}} of the package \code{base}.
}
}
\details{
Objects of class \code{wprof} are composed of a \code{data.frame} of profiles and a vector of frequencies. This method applies method \code{\link{merge.data.frame}} to the profiles and applies function \code{FUN} to the frequencies.

If \code{support} is \code{TRUE}, function \code{\link{merge.data.frame}} is not used and the output corresponds to the object \code{y}, but with its frequencies modified. These are set equal to the frequencies of the corresponding profiles in \code{x}, or to 0 for profiles not contained in \code{x}.
}

\author{
Arcagni A.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{merge}}, \code{\link{var2prof}}, \code{\link{pop2prof}}
}
\examples{
n <- 5
v1 <- as.ordered(c("a", "b", "c", "d"))
v2 <- 1:3
set.seed(0)
pop <- data.frame(
	v1 = sample(v1, n, replace = TRUE),
	v2 = sample(v2, n, replace = TRUE)
)

survey_weights <- round(runif(5)*10)

prof1 <- pop2prof(pop, weights = survey_weights)
prof2 <- var2prof(varmod = list(v1 = as.ordered(c("a", "b", "c", "d")), v2 = 1:3))

# prof2 is the support of prof1
merge(prof1, prof2, support = TRUE)

# union between the two sets of profiles and their frequencies are added
merge(prof1, prof2)

# intersection of the sets of profiles with the assumption
# that the minimum number of observations is shared
# between the two distributions
merge(prof1, prof2, all = FALSE, FUN = min)

prof2$freq <- prof2$freq*10
# to remove from prof2 the observations in prof1
distribution <- merge(prof2, prof1, FUN = "-"); distribution
}