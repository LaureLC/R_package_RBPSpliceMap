\name{intervalSelect}
\alias{intervalSelect}

\title{intervalSelect}

\description{
This function returns the part of GRanges object in the input ranges.
}

\usage{
intervalSelect(objectGRanges, from, to)
}

\arguments{
  \item{objectGRanges}{
GRanges object from where we want to select the part concerning the interval of interest.
}
  \item{from}{
Coordinate from where you want the coverage.
}
  \item{to}{
Coordinate where you want the coverage to end.
}
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}\cr
\code{\link{chromosomeSelect}}\cr
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Select the interval of interest
intervalSelect(mapReadsGRanges, from = 134621217, to = 134681857)
}

\keyword{ranges}
\keyword{subset}
