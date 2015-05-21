\name{intervalSelect}
\alias{intervalSelect}

\title{intervalSelect}

\description{
This function return the part of the GRanges object concerning the input ranges
}

\usage{
intervalSelect(objectGRanges, from, to)
}

\arguments{
  \item{objectGRanges}{
GRanges object from where we want to select the part concerning the strand of interest
}
  \item{from}{
Coordinate from where you want the coverage
}
  \item{to}{
Coordinate where you want the coverage to end
}
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}
\code{\link{chromosomeSelect}}
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Select the interval of interest
intervalSelect(mapReadsGRanges, from = 134621217, to = 134681857)
}

\keyword{ranges}
\keyword{subset}
