\name{mapReadsTreatment}
\alias{mapReadsTreatment}

\title{mapReadsTreatment}

\description{
This function, from a GRanges object, computes the coverage (number of reads/nucleotide) for a region defined by "chromosome", "strand" and a range of interest ("from" and "to") for a GRanges object.
}

\usage{
mapReadsTreatment(mapReadsGRanges, chromosome, strand, from, to)
}

\arguments{
  \item{mapReadsGRanges}{
GRanges object containing mapped reads (generally from a bam file)
(see \code{\link{bamToGRanges}})
}
  \item{chromosome}{
chromosome of interest as a string (ex: "7" or "chr7")\cr
/!\  It should conform to the GRanges object notation
}
  \item{strand}{
strand of interest as a string ("+", "-", "*")\cr
"*" correspond to both strands
}
  \item{from}{
Coordinate from where you want the coverage.
}
  \item{to}{
Coordinate where you want the coverage to end.
}
}

\details{
The returned coverage vector indicates the number of reads per nucleotide along the region of interest.
}

\value{Coverage Vector}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}\cr
\code{\link{intervalSelect}}\cr
\code{\link{coverageChr}}\cr
\code{\link{coverageVector}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Treatment of the GRanges object with mapped reads
mapReadsTreatment(mapReadsGRanges, chromosome = "chr7", strand = "+", from = 810695, to = 810796)
}

\keyword{coverage}
\keyword{reads}
\keyword{mapping}
