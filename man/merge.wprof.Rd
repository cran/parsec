\name{merge.wprof}
\alias{merge.wprof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge Two Sets of Profiles
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
boolean variables to indicate  if \code{y} is the support of \code{x}, \code{FALSE} by default.
}
  \item{FUN}{
function to be applied to the profiles frequencies, by default the frequencies are added. It is ignored if \code{support} id \code{TRUE}.
}
  \item{all}{
same argument of the function \code{\link{merge}}, here set \code{TRUE} by default to get all the possible profiles. If a profile is not observed its frequency is equal to 0, but it does not mean that it is not possible to observe it.
}
  \item{\dots}{
additional arguments to be passed to the method \code{\link{merge.data.frame}} of the package \code{base}.
}
}
\details{
Objects of class \code{wprof} are composed by a \code{data.frame} of profiles and a vector of frequencies. This method applies the method \code{\link{merge.data.frame}} to the first the profiles and applies the function \code{FUN} to the frequencies.

If \code{support} is \code{TRUE} function \code{\link{merge.data.frame}} is not used and the output corresponds to the object \code{y} but its frequencies are modified. Frequencies are equal to the frequencies of the corresponding profiles in \code{x} or are equal to 0 if the corresponding profile is not contained in \code{x}.
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