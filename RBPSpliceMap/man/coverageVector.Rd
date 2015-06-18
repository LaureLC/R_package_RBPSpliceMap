\name{coverageVector}
\alias{coverageVector}

\title{coverageVector}

\description{
This function transforms a coverage Rle (package S4Vector) into a coverage vector on a specified region of the Rle.
}

\usage{
coverageVector(cover, from = 0, to)
}

\arguments{
  \item{cover}{
Rle (package S4Vector) with the coverage (typically \code{\link{coverageChr}})
}
  \item{from}{
from = coordinate from where you want the coverage\cr
DEFAULT = 0
}
  \item{to}{
The copordinate of the last read you want on the coverage.
}
}

\details{
The returned vector contains the number of reads at each position (nucleotide).
}

\value{Vector}

\author{Laure Le Calvez <laure.le-calvez@laposte.net}

\seealso{
\code{\link{coverageChr}}\cr
\code{\link{coverage}}\cr
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Calculate the coverage on a specified chromosome
cover = coverageChr(mapReadsGRanges, chromosome = "chr7")
#Transforme coverage Rle into a coverage vector
coverageVector(cover, from = 810695, to = 810796)
}

\keyword{coverage}
\keyword{Rle}
