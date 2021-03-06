% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-vector.R
\name{whichMax}
\alias{whichMax}
\title{whichMax}
\usage{
whichMax(x)
}
\arguments{
\item{x}{numeric vector}
}
\value{
numeric vector that contains the indexes of *all* max elements, not just the *first*
}
\description{
behaves like which.max(x) but returns multiple indexes if required.
}
\examples{
which.max( c(87, presidents[1:30], 87) );
whichMax( c(87, presidents[1:30], 87) );

}
\seealso{
Other Vectors: 
\code{\link{doUnique}()},
\code{\link{findAllIndexesWithValueInVector}()},
\code{\link{freqMax}()},
\code{\link{freqMin}()},
\code{\link{notDuplicated}()},
\code{\link{whichMaxFreq}()},
\code{\link{whichMinFreq}()},
\code{\link{whichMin}()}
}
\concept{Vectors}
