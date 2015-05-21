\name{coverageChr}
\alias{coverageChr}

\title{coverageChr}

\description{
This function, from the GRanges, give the coverage (number of reads) concerning the chromosome of interest
}

\usage{
coverageChr(mapReadsGRanges, chromosome)
}

\arguments{
  \item{mapReadsGRanges}{
GRanges object from where we want to select the part concerning the strand of interest
}
  \item{chromosome}{
chromosome of interest as a string (ex: "7" or "chr7")
			/!\  Write it like it is written in the GRanges object
}
}

\details{
The return Rle (package S4Vectors) represent the coverage (number of reads) of the input chromosome
}

\value{Rle (package S4Vectors)}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{coverageVector}}
\code{\link{coverage}}
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Calculate the coverage on a specified chromosome
coverageChr(mapReadsGRanges, chromosome = "chr7")
}

\keyword{chromosome}
\keyword{coverage}
