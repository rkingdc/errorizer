\name{errorize}
\alias{errorize}
\title{
Function to create "Errorized" version of existing R functions.
}
\description{
This function accepts an existing R function as its first argument and returns a new R function with enhanced error and warning logging capabilities.
}
\usage{
errorize(FUN, stopOnError = TRUE, onErrorReturn = NULL, ...)
}
\arguments{
  \item{FUN}{
The function to "errorize". If you are making a drop-in replacement, include the namespace (see Details).
}
  \item{stopOnError}{
Logical. If the function errors out, do you want to stop the script (the default) or issue a warning and proceed.
}
  \item{onErrorReturn}{
If \code{stopOnError} is \code{FALSE}, what value should the function return if it errors out? Default is \code{NULL}.
}
\item{...}{
Additional arguments to \code{\link{saveRDS}}.
}
}
\details{
If you encounter an error or warning, an "errorized" function will save the error or warning, the time at which the error or warning occurred, the function that threw the error, and all the objects entering the "errorized" function in a list.

If you want to create a drop-in replacement of an existing R function (i.e. create a new function of the same name as an existing function in the global environment), you'll need to include the namespace in the call to \code{errorize} in the form \code{namespace::function}.
}
\value{
A function identical to the function passed to \code{errorize} but with enhanced error and warning logging capabilities.
}


\author{
Ian K. Kyle
}
\seealso{
\code{\link{saveRDS}}, \code{\link{readRDS}} for reading the saved error or warning information back in
}
\examples{
\dontrun{
lm <- errorize(stats::lm)
data("cars")
lm(speed ~ dist, data = cars)             # works
lm(speed ~ missing_variable, data = cars) # this errors out and saves the data to a .Rda file

# read the saved data back in (timestamp on filename will change based on when the above is run)
er <- readRDS('./lm_error_1478361734.Rds')

# use do.call to recreate the error
do.call(er$fxn, er$arglst)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
