\name{pop2prof}
\alias{pop2prof}

\title{
Population to profiles
}
\description{
Extract observed profiles and their frequencies out of statistical population.
}
\usage{
pop2prof(y, labtype = c("profiles", "progressive"), sep = "/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
a dataset, used to count profile frequencies. See details.
}
  \item{labtype}{
the user can choose the type of labels to assign to profiles. See details.
}
  \item{sep}{
variables separator in the profiles labels.
  }
}
\details{
\code{y} is a data.frame of observation of the ordinal or numeric variables. The order must be defined into the object type, so as to build the incidence matrix (see \code{\link{getzeta}}).

The function extracts variables and their observed modalities from the population; it builds all possible profiles and assigns the corresponding frequency to each of them. If some modalities are not observed in the population, they will not be used to build profiles. If one is interested in the set of all possible profiles from a given set of variables, function \code{\link{var2prof}} is to be used.

Users can choose the type label to assign to profiles. When the names of modalities are too long, it is suggested to choose \code{progressive}.
}
\value{
The function returns a S3 class object \code{wprof}, "weighted profiles", containing the \code{data.frame} named \code{profiles} and the frequency vector \code{freq}.
}
\seealso{
\code{\link{var2prof}}, \code{\link{getzeta}}
}
\examples{
n <- 5
v1 <- as.ordered(c("a", "b", "c", "d"))
v2 <- 1:3
pop <- data.frame(
    v1 = sample(v1, n, replace = TRUE),
    v2 = sample(v2, n, replace = TRUE)
)
pop2prof(pop)
}