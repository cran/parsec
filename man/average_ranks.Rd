\name{average_ranks}
\alias{average_ranks}
\alias{average_ranks.incidence}
\alias{average_ranks.cover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Ranks
}
\description{
The function evaluates the average rank, and other distribution details, for each element of the poset.
}
\usage{
average_ranks(x, ...)
\method{average_ranks}{cover}(x, level = 0.9, error = 10^(-5), ...)
\method{average_ranks}{incidence}(x, level = 0.9, error = 10^(-5), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an incidence or cover matrix representing a partial order.
}
  \item{level}{
coverage probability of the rank intervals.
}
  \item{error}{
the "distance" from uniformity in the sampling distribution of linear extensions used to evaluate the average ranks. See \code{\link{idn}} for details.
}
  \item{\dots}{
any of above.
}
}
\details{
The function computes the rank distribution for each element of the poset, through function \code{\link{idn}}. Next, it checks whether there are any equivalent profiles, using function \code{\link{equivalences}}, and makes their rank distribution equal. Finally it provides a dataframe comprising, for each element of the poset: the average rank \code{avrg}, the extremes \code{inf} and \code{sup} of the rank interval, the effective coverage probability of the rank interval \code{prob}, the estimated minimum and maximum rank values (\code{min} and \code{max}) and the rank \code{range}.

The output is a dataframe of class \code{average_ranks} /for which a method of function \code{plot} is available. See \code{\link{plot.average_ranks}} for details).
}
\value{
A dataframe of class \code{average_ranks} whose columns are:
\item{avrg}{the average rank;}
\item{inf}{the lower extreme of the rank interval;}
\item{sup}{the upper extreme of the rank interval;}
\item{prob}{the effective coverage probability of the rank interval;}
\item{min}{the minimum rank;}
\item{max}{the maximum rank;}
\item{range}{the rank range.}
}
\author{
Fattore M., Arcagni A.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{idn}}, \code{\link{equivalences}}, \code{\link{plot.average_ranks}}
}
\examples{
profiles <- var2prof(varlen = c(3, 2, 2))
Z <- getzeta(profiles)
res <- average_ranks(Z)
plot(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.