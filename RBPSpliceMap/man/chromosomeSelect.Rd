\name{chromosomeSelect}
\alias{chromosomeSelect}

\title{chromosomeSelect}

\description{
This function return the part of the GRanges object concerning the input "chromosome"
}

\usage{
chromosomeSelect(objectGRanges, chromosome)
}

\arguments{
\item{objectGRanges}{
GRanges object from where we want to select the part concerning the strand of interest
}
  \item{chromosome}{
chromosome of interest as a string (ex: "7" or "chr7")
/!\  Write it like it is written in the GRanges object
}
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}
\code{\link{intervalSelect}}
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Select the chromosome of interest
chromosomeSelect(mapReadsGRanges, chromosome = "chr7")

regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Select the chromosome of interest
chromosomeSelect(mapReadsGRanges, chromosome = "chr7")
}

\keyword{chromosome}
\keyword{subset}
