% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-matrix.R
\name{rotateMatrixAngle}
\alias{rotateMatrixAngle}
\title{rotateMatrixAngle}
\usage{
rotateMatrixAngle(x, a = 0, clockwise = TRUE)
}
\arguments{
\item{x}{matrix}

\item{a}{angle, must be a multiple of 90 degrees, can be 630}

\item{clockwise}{direction of rotation, default is clockwise}
}
\value{
matrix, updated
}
\description{
rotateMatrixAngle
}
\examples{
m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));

rotateMatrixAngle(m,0);
rotateMatrixAngle(m,90);
rotateMatrixAngle(m,180);
rotateMatrixAngle(m,270);
rotateMatrixAngle(m,-90); # we reverse the direction of clockwise if negative

rotateMatrixAngle(m,270,FALSE);
rotateMatrixAngle(m,-90,FALSE); # we reverse the direction of clockwise if negative

#rotateMatrixAngle(m,33);
rotateMatrixAngle(m,630);

}
