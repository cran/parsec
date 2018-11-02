\name{LE2incidence}
\alias{LE2incidence}
\alias{LE2incidence.default}
\alias{LE2incidence.list}

\title{
Computes the incidence matrices of lexicographic linear extensions of a profile poset.
}
\description{
The function generates the incidence matrices of the lexicographic linear extensions of a profile poset, given the variables (argument \code{varmod} or \code{varlen}) and a list of complete orders on them (argument \code{lst}).
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
a vector of \code{characters}, or a list of specifies the names of the variables in increasing order. See details.
}
  
  \item{varmod}{
list of variables and their grades. See details. 
}
  \item{varlen}{
a vector with the number of grades of each variable. See details.
}
}
\details{
Argument \code{lst} is a list of chracter vectors. Each vector lists variable names in increasing order.

List \code{varmod} and vector \code{varlen} must be named so as to identify the variables they refer to. Profiles are generated as combinations of the variables' grades. The names of the profiles are the grades of the variables concatenated, after the variables order in \code{varmod}/\code{varlen}. See \code{\link{var2prof}} for more details about these arguments.
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