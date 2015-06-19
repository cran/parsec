\name{var2prof}
\alias{var2prof}
\alias{wprof}

\title{
Variables to profiles
}
\description{
The function produces the list of all of the profiles from a list of input ordinal variables. See details for how to define variables.
}
\usage{
var2prof(varmod = lapply(as.list(varlen), function(x) 1:x),
    varlen = sapply(varmod, length), freq = NULL,
    labtype = c("profiles", "progressive"), y=NULL)
}

\arguments{
  \item{varmod}{
list of variables and their modalities. See details. 
}
  \item{varlen}{
a vector of number of modalities of each variable. See details.
}
  \item{freq}{
profiles frequency distribution. By default, the frequencies are set equal to 1.
}
  \item{labtype}{
type of labels to assign to  profiles. See details.
}
  \item{y}{
a matrix of observations, used to count profiles frequencies. See details.
}
}
\details{
Variables can be defined through their names and modalities, using a list as argument \code{varmod}. The names of the objects in the list are taken as variable names. The objects in the list must be ordered vectors or numeric vectors.

A faster way to define variables is through a vector with the number of modalities of each variable, as argument \code{varlen}. This way,  variables and ttheir modalities are assigned arbitrary names. In particular, modalities are identified by their ranks in variable definition.

The user can choose the type label to assign to profiles. \code{profiles} is the combination of modailties identifying the profiles. When the names of the modalities are too long, it is suggested to choose \code{progressive}.

\code{y} is a matrix of observation on the ordinal variables (observations by rows and variables by columns). Variables must be ordered as defined in the previous parameters. The names of variable modalities must match their definition. By this parameter, the function counts the number of times a profile is observed in the population, assigning the result to the \code{freq} output parameter. This method should be used when the variables and their modalities are known, otherwise the function \code{\link{pop2prof}} is available.
}
\value{
The function returns a S3 class object \code{wprof}, "weighted profiles", comprising the \code{data.frame} \code{profiles} and the vector of frequencies \code{freq}.
}

\seealso{
\code{\link{pop2prof}}, \code{\link{getzeta}}
}
\examples{
# 2 variables with 2 modalities, frequencies detected from population
pop <- matrix(sample(1:2, 200, replace=TRUE), 50, 2)
var2prof(varlen=c(2, 2))

# 2 variables:
# - mood: 2 modalities
# - weather: 3 modalities
# 2*3*2 profiles and frequencies sampled from a Binomial distribution n = 10, p = 0.5
var <- list(
    mood = as.ordered(c("bad", "good")),
    weather = as.ordered(c("rainy", "cloudy", "sunny"))
)
var2prof(var, freq = rbinom(2*3, 10, 0.5), labtype = "progressive")
}