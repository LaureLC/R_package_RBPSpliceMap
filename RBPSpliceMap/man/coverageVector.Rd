\name{coverageVector}
\alias{coverageVector}

\title{coverageVector}

\description{
This function transform a coverage Rle (package S4Vector) into a coverage vector
}

\usage{
coverageVector(cover, from = 0, to)
}

\arguments{
  \item{cover}{
Rle (package S4Vector) with the coverage
}
  \item{from}{
If there is a part without any reads, from where do the reads appear. from = coordinate of the position just before the one with the first read
DEFAULT = 0
}
  \item{to}{
The coordinate of the last read you want on the coverage
}
}

\details{
/!\ ERROR if the from given correspond to a position with or after some reads of the input Rle
from: only if you know how much nucleotides at the beggining of the sequence are not covered by reads
The return vector contains the number of reads at each position (nucleotide)
}

\value{Vector}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{coverageChr}}
\code{\link{coverage}}
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Calculate the coverage on a specified chromosome
cover = coverageChr(mapReadsGRanges, chromosome = "chr7")
#Transforme coverage Rle into a coverage vector
coverageVector(cover, from = 810695, to = 810796)
}

\keyword{coverage}
\keyword{Rle}
