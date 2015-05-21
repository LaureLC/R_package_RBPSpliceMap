\name{mapReadsTreatment}
\alias{mapReadsTreatment}

\title{mapReadsTreatment}

\description{
This function, from a GRanges object, give the coverage (number of reads) on a "chromosome", a "strand" and a range of interest ("from" and "to")
}

\usage{
mapReadsTreatment(mapReadsGRanges, chromosome, strand, from, to)
}

\arguments{
  \item{mapReadsGRanges}{
GRanges object containing mapped reads (generally from a bam file)
(see bamToGRanges())
}
  \item{chromosome}{
chromosome of interest as a string (ex: "7" or "chr7")
/!\  Write it like it is written in the GRanges object
}
  \item{strand}{
strand of interest as a string ("+", "-", "*")
"*" correspond to the 2 strands
}
  \item{from}{
Coordinate from where you want the coverage
}
  \item{to}{
Coordinate where you want the coverage to end
}
}

\details{
The return coverage vector indicate the number of reads per nucleotide
}

\value{Coverage Vector}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}
\code{\link{intervalSelect}}
\code{\link{coverageChr}}
\code{\link{coverageVector}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Treatment of the GRanges with mapped reads
mapReadsTreatment(mapReadsGRanges, chromosome = "chr7", strand = "+", from = 810695, to = 810796)
}

\keyword{coverage}
\keyword{reads}
\keyword{mapping}

