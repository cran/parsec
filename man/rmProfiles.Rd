\name{rmProfiles}
\alias{rmProfiles}
\alias{rmProfiles.wprof}

\title{
Removing profiles
}
\description{
Function to remove profiles from an object of class \code{wprof}.
}
\usage{
rmProfiles(y, ...)
\method{rmProfiles}{wprof}(y, v, ...)
}

\arguments{
  \item{y}{
object of class \code{wprof}.
}
  \item{v}{
a vector pointing to the profiles to be removed. The vector can be of type:
\itemize{
\item{\code{numeric} whose components refer to the positions of profiles in
\code{y};}
\item{\code{logical} of the same length as the number of profiles in \code{y};}
\item{\code{character}, referring to profile names in \code{y}.}
}
}
  \item{\dots}{
any of the above.
}
}

\value{
The function returns an \code{wprof} object equal to \code{y} but without the
profiles in \code{v}.
}

\examples{
vl <- c(3, 3, 3)
prof <- var2prof(varlen = vl)
rownames(prof$profiles)

prof <- rmProfiles(prof, c("123", "321"))

plot(prof)
}