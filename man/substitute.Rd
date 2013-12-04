\name{s}
\alias{s}
\title{Simple expression substitution}
\description{
  Evaluate R expressions and substitute their values into a string.
}
\usage{
s(string, round = NULL, signif = NULL, envir = parent.frame())
}
\arguments{
  \item{string}{The string to substitute into, containing any number of \code{"#{}"} blocks.}
  \item{round}{\code{NULL} or a single integer, giving the number of decimal digits for rounding numeric expressions. This argument takes priority over \code{signif}.}
  \item{signif}{\code{NULL} or a single integer, giving the number of significant decimal digits to use for numeric expressions. The \code{round} argument takes priority over this one and will be used if not \code{NULL}.}
  \item{envir}{The environment to evaluate expressions in.}
}
\details{
  Each part of the string surrounded by \code{"#{}"} is extracted, evaluated as R code in the specified environment, and then its value is substituted back into the string. The literal string \code{"#{}"} can be obtained by escaping the hash character, viz. \code{"\\\\#{}"}. The block may contain multiple R expressions, separated by semicolons, but may not contain additional braces.
  
  If the \code{reportrNumericRepresentation} option is defined and is a list, its \code{round} and \code{signif} elements provide defaults for the arguments of the same name.
}
\value{
  The final string, with expressions substituted into it.
}
\author{Jon Clayden}
\seealso{\code{\link{report}}}
\examples{
s("pi is #{pi}")
s("pi is \\\\#{pi}")
s("The square-root of pi is approximately #{sqrt(pi)}", signif=4)
s("1/(1+x) for x=3 is #{x <- 3; 1/(1+x)}")
}
