\name{parsec-package}
\alias{parsec-package}
\alias{parsec}
\docType{package}
\title{
PARtial orders in Socio-EConomics
}
\description{
The package implements tools for basic partial order analysis and for multidimensional
evaluation (e.g. of poverty and material deprivation) with ordinal variables.
Its main goal is to provide socio-economic scholars with an integrated
set of elementary functions for multidimensional evaluation,
based on ordinal information. The package is organized in four main parts.
The first two comprise functions for data management and basic partial order analysis;
the third and the fourth are devoted to evaluation and implement both
the poset-based approach and a more classical counting procedure.
}
\details{
\tabular{ll}{
Package: \tab parsec\cr
Type: \tab Package\cr
Version: \tab 1.2.0\cr
Date: \tab 2018-04-01\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
A, Arcagni
M, Fattore

Maintainer: A, Arcagni <alberto.arcagni@unimib.it>
}
%\references{
%~~ Literature or other references for background information ~~
%}
\keyword{package, poset, partial order, multidimensional poverty, ordinal variables, counting approach}

\examples{
# a simple example of package application to obtain first poset-based results

# definition of the variables by their number of modalities
variables <- c(2, 2, 2)

# definition of the threshold
threshold <- c("112", "211")

# extraction of all of the possible profiles from variables; the
# function returns an object of class "wprof", weighted profiles: by default,
# wheigths/frequencies are set equal to 1
profiles <- var2prof(varlen = variables)

# the following function creates matrices describing the poset, and
# provide all the results related to it
eval <- evaluation(profiles, threshold, nit = 10^5, maxint = 10^3)

# the results can be summarized, the method returns a data.frame object that
# describes the profiles and you can also summarize
summary(summary(eval))

# a method of the plot function returns the Hasse diagram, a frequency 
# distribution of the threshold, the identification function, the rank
# distribution of each profile through a barplot, and the relative gap.
plot(eval)
}
