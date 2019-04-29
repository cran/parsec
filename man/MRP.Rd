\name{MRP}
\alias{MRP}
\alias{MRP.incidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mutial ranking probability matrix
}
\description{
Function to evaluate Mutial Ranking Probability (MRP) matrix based on \code{netrankr} package.
}
\usage{
MRP(Z, method = c("exact", "mcmc", "approx"), error = 10^(-3), nit = NULL)
\method{MRP}{incidence}(Z, method = c("exact", "mcmc", "approx"), error = 10^(-3), nit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
an incidence matrix, an object of class \code{incidence}.
}
  \item{method}{
a string to choose the method applied to evaluate the MRP matrix. The default value is \code{"exact"}. See section 'Details' below.
}
  \item{error}{
considered only if \code{mcmc} method is selected. The "distance" from uniformity in the sampling distribution of linear extensions.
}
  \item{nit}{
considered only if \code{mcmc} method is selected. Number of ITerations in the Bubley-Dyer algorithm, by default evaluated indicated in Bubley and Dyer (1999) depending on the value of \code{error}.
}
}

\details{
Package \code{netrankr} provides three functions to evaluate MRP matrix. Note that MRP matrix definition in \code{netrankr} is a little different from the one used in Fattore and Arcagni (2018), therefore this function unifies the results to the second definition.

Parameter \code{method} allows the selection of which function of package \code{netrankr} to use: \code{"exact"} runs the function \code{exact_rank_prob} that provides the exact results, \code{"mcmc"} the function \code{mcmc_rank_prob} that provide the estimated results through the Bubley Dyer algorithm and \code{"approx"} runs the function \code{approx_rank_relative} that provide the Bruggemann and Carlsen (2011) approximated results. For small posets it is possible to evaluate the exact MRP matrix, for larger posets it is necessary to use the appoximated results.
}
\value{
An object of class \code{matrix} representing the MRP matrix. Dimensions names are equal to incidence matrix ones.
}
\seealso{
\code{\link[netrankr]{exact_rank_prob}}, \code{\link[netrankr]{mcmc_rank_prob}}, \code{\link[netrankr]{approx_rank_relative}}
}
\references{
Bruggemann R., Carlsen L., (2011). An improved estimation of averaged ranks of partial orders. MATCH Commun. Math. Comput. Chem., 65(2):383-414.

Bubley R., Dyer M. (1999), Faster random generation of linear extensions, Discrete Math., 201, 81-88.

Fattore M., Arcagni A. (2018). Using mutual ranking probabilities for dimensionality reduction and ranking extraction in multidimensional systems of ordinal variables. Advances in Statistical Modelling of Ordinal Data, 117.
}
\examples{
L <- getlambda(A < B, C < B, B < D)
MRP(L)
}
