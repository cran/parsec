\name{pop2prof}
\alias{pop2prof}

\title{
Population to profiles
}
\description{
Extract the observed profiles and the corresponding frequencies, out of the statistical population.
}
\usage{
pop2prof(
  y,
  labtype = c("profiles", "progressive", "rownames"),
  sep = "",
  weights = rep(1, nrow(y))
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
a dataset, used to count profile frequencies. See details.
}
  \item{labtype}{
users can choose the type of labels to assign to profiles. See details.
}
  \item{sep}{
variables separator in the profiles labels.
  }
  \item{weights}{
a vector of length equal to the number of observations in \code{y}, representing the survey weights of each observation.
  }
}
\details{
\code{y} is a data.frame of observations on the ordinal or numeric variables. The partial order must be defined within the object type, so as to build the incidence matrix of the order relation (see \code{\link{getzeta}}).

The function extracts variables and their observed modalities from the population; it builds all possible profiles and assigns to them the corresponding frequency. If some modalities are not observed in the population, they will not be used to build the profiles. If one is interested in the set of all possible profiles from a given set of variables, function \code{\link{var2prof}} is to be used.

Users can choose the label type to assign to profiles. Accepetd types are: \code{profiles} the variabiles modalities, \code{progressive} a progressive numeration, \code{rownames} the rownames in the dataset.
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