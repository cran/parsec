\name{LE2incidence}
\alias{LE2incidence}
\alias{LE2incidence.default}
\alias{LE2incidence.list}

\title{
Convert lexicographic linear extensions into incidence matrices of profiles
}
\description{
The function generates the incidence matrices of the profiles of lexicographic linear extensions starting from the variables definition (argument \code{varmod} or \code{varlen}) and a list of complete orders between the varaibles (argiment \code{lst}).
}
\usage{
LE2incidence(
    lst,
    varmod = lapply(as.list(varlen), function(lst) 1:lst),
    varlen = sapply(varmod, length)
)
\method{LE2incidence}{default}(
    lst,
    varmod = lapply(as.list(varlen), function(lst) 1:lst),
    varlen = sapply(varmod, length)
)
\method{LE2incidence}{list}(
    lst,
    varmod = lapply(as.list(varlen), function(x) 1:x),
    varlen = sapply(varmod, length)
)
}

\arguments{
  \item{lst}{
a vector of \code{characters}, or a list of vectors of \code{characters}. Each vector indicates the names of the variables in increasing order. See details.
}
  
  \item{varmod}{
list of variables and their modalities. See details. 
}
  \item{varlen}{
a vector of number of modalities of each variable. See details.
}
}
\details{
The argument \code{lst} is a list of vectors of characters. Each vector lists the variables names in increasing order.

The list \code{varmod} and the vector \code{varlen} must be named in order to identify the variables they refer to. The profiles are generated by the combinations of the varaibles modalities. The names of the profiles are the modelities of the variables concatenated observing the varaibles order in \code{varmod}/\code{varlen}. See \code{\link{var2prof}} for more details about these arguments.
}
\value{
an object of S3 class \code{incidence} or a list of objects of S3 class \code{incidence}.
}
\author{
Alberto Arcagni
}

\seealso{
\code{\link{var2prof}}
}
\examples{
Lambda <- getlambda(A < B, C < D)
plot(Lambda)
lst <- LE(Lambda)
vl <- c(A = 2, B = 2, C = 2, D = 2)
lstZeta <- LE2incidence(lst, varlen = vl)
for (x in lstZeta)
    plot(x)
}