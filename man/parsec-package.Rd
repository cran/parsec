\name{parsec-package}
\alias{parsec-package}
\alias{parsec}
\docType{package}
\title{
PARtial orders in Socio-EConomics
}
\description{
The package implements tools for the analysis of partially ordered data, with a particular focus on the evaluation of multidimensional systems of indicators and on the analysis of poverty.

Its main objective is to provide socio-economic scholars with an integrated set of elementary functions for multidimensional evaluation, based on ordinal information. In particular, it provides functions for data management and basic analysis of partial orders as well as other functions for the evaluation and application of both the poset-based approach and a more classic counting method.
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
\keyword{package, poset, partial order, multidimensional poverty, ordinal variables, counting approach, firs order dominance}

\examples{
###########################################
# a simple example of package application #
###########################################

# definition of the variables by their number of grades
variables <- c(2, 2, 2)

# definition of the threshold
threshold <- c("112", "211")

# extraction of all of the possible profiles from variables; the
# function returns an object of class "wprof", weighted profiles: by default,
# weigths/frequencies are set equal to 1
profiles <- var2prof(varlen = variables)

# the following function creates matrices describing the poset, and
# provides all the results related to it
eval <- evaluation(profiles, threshold, nit = 10^5, maxint = 10^3)

# The results can then be summarized

summary(summary(eval))

# a method of the plot function returns the Hasse diagram, a frequency 
# distribution of the threshold, the identification function, the rank
# distribution of each profile through a barplot, and the relative gap.
plot(eval)

#########################################################
# a second example of new functions recently introduced #
#########################################################

# definition of the variables and of the corresponding profiles
v1 <- as.ordered(c("a", "b", "c", "d"))
v2 <- 1:3
prof <- var2prof(varmod = list(v1 = as.ordered(c("a", "b", "c", "d")), v2 = 1:3))
np <- nrow(prof$profiles)

# definition of different distributions over the set of profiles
k <- 10 # number of populations
set.seed(0)
populations <- as.data.frame(lapply(1:k, function(x) round(runif(np)*100)))
rownames(populations) <- rownames(prof$profiles)
names(populations) <- paste0("P", 1:k)

prof
populations

# evaluation of the fuzzy first order dominance
res <- FFOD(profiles = prof, distributions = populations)
res

# rank stablity analysis
res <- rank_stability(res)
res

# graphical representation
plot(res)
}
