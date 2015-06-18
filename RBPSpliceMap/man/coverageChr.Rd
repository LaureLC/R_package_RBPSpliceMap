\name{coverageChr}
\alias{coverageChr}

\title{coverageChr}

\description{
This function, from a GRanges object, computes the coverage (number of reads/nucleotide) for the chromosome of interest.
}

\usage{
coverageChr(mapReadsGRanges, chromosome)
}

\arguments{
  \item{mapReadsGRanges}{
GRanges object from where we want to select the part concerning the chromosome of interest.
}
  \item{chromosome}{
chromosome of interest as a character (ex: "7" or "chr7")\cr
/!\ It should conform to the GRanges object notation.
}
}

\details{
The returned Rle (package S4Vectors) represents the coverage (number of reads/nucleotide) of the analysed chromosome.\cr
No strand can be selected here (see \code{\link{strandSelect}})
}

\value{Rle (package S4Vectors}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{coverageVector}}\cr
\code{\link{coverage}}\cr
\code{\link{mapReadsTreatment}}
}
\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Calculate the coverage on a specified chromosome
coverageChr(mapReadsGRanges, chromosome = "chr7")
}

\keyword{chromosome}
\keyword{coverage}
