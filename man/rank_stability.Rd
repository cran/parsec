\name{rank_stability}
\alias{rank_stability}
\alias{rank_stability.FODposet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank stability analysis in posetic FOD
}
\description{
The function computes the average ranks and the positions in the ranking of the elements of the alpha-cuts.
}
\usage{
rank_stability(x, ...)
\method{rank_stability}{FODposet}(x,
    selection = 1:length(x$covers),
    coverage_probability = 0.9,
    error = 10^(-5), ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class \code{FODposet} generated by function \code{\link{FOD}}.
}
  \item{selection}{
numeric vector or a vector of names to select the cover matrices in argument \code{x}.
}
  \item{coverage_probability}{
least coverage probability of the rank intervals with extremes \code{lower_ranks} and \code{upper_ranks}.
}
  \item{error}{
the "distance" from uniformity in the sampling distribution of linear extensions used to evaluate the average ranks. See \code{\link{idn}} for details.
}
  \item{\dots}{
any of above.
}
}
\value{
\item{alpha}{vector of the alpha values defining the alpha-cuts.}
\item{average_ranks}{data frame of average ranks of the poset elements (by columns) at different alpha values (by rows).}
\item{lower_ranks}{data frame of the lower bounds of the rank interval, of each poset element (by columns) at different alpha values (by rows).}
\item{upper_ranks}{data frame of the upper bounds of the rank interval, of each poset element (by columns) at different alpha values (by rows).}
\item{ranking}{data frame of the positions of poset elements  (by columns), in the ranking extracted from the posets associated to alpha-cuts (by rows).}
\item{resolution}{number of elements of the posets associated to the alpha-cuts.}
}
\author{
Fattore M., Arcagni A.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FFOD}}, \code{\link{idn}}
}
\examples{
v1 <- as.ordered(c("a", "b", "c", "d"))
v2 <- 1:3
prof <- var2prof(varmod = list(v1 = as.ordered(c("a", "b", "c", "d")), v2 = 1:3))
np <- nrow(prof$profiles)

k <- 10 # number of populations
set.seed(0)
populations <- as.data.frame(lapply(1:k, function(x) round(runif(np)*100)))
rownames(populations) <- rownames(prof$profiles)
names(populations) <- paste0("P", 1:k)

x <- FFOD(profiles = prof, distributions = populations)

res <- rank_stability(x)
res
}
