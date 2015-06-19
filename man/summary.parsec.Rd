\name{summary.parsec}
\alias{summary.parsec}

\title{
Summary of outputs of the evaluation function
}

\description{
S3 method of function \code{summary} reporting main information for an object of class \code{parsec},
obtained from function \code{\link{evaluation}}.
In particular, it produces a table showing, for each profile:
\itemize{
\item{}{the modalities of the variables identifying the profile (if they are
returned by \code{\link{evaluation}},}
\item{the assigned weight,}
\item{whether or not it belongs to the threshold,}
\item{the corresponding value of the identification function,}
\item{the average rank,}
\item{the different gap measures (see \code{\link{evaluation}} for details).}
}
If the number of profiles is higher than ten, the shown table gets cut, but
the method returns a \code{data.frame} providing the complete output.

}
\usage{
\method{summary}{parsec}(object, ...)
}

\arguments{
  \item{object}{
  an object of S3 class \code{parsec}, output of the \code{\link{evaluation}} function.
}
  \item{\dots}{
added for consistency with the generic method.
}
}

\seealso{
\code{\link{evaluation}}
}
\examples{
profiles <- var2prof(varlen = c(3, 2, 4))
threshold <- c("311", "112")

res <- evaluation(profiles, threshold, nit = 10^3)

sm <- summary(res)
summary(sm)
}
