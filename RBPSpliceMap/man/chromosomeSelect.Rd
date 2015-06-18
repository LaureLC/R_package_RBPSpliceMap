\name{chromosomeSelect}
\alias{chromosomeSelect}

\title{chromosomeSelect}

\description{
This function subsets a GRanges object by the requested chromosome.
}

\usage{
chromosomeSelect(objectGRanges, chromosome)
}

\arguments{
  \item{objectGRanges}{
GRanges object from which we want to subset by chromosome.
}
  \item{chromosome}{
chromosome of interest as a character (ex: "7" or "chr7")\cr
/!\ It should conform to the GRanges object notation.
}
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{strandSelect}}\cr
\code{\link{intervalSelect}}\cr
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges object from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Select the chromosome of interest
chromosomeSelect(mapReadsGRanges, chromosome = "chr7")

regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges object from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Select the chromosome of interest
chromosomeSelect(mapReadsGRanges, chromosome = "chr7")
}

\keyword{chromosome}
\keyword{subset}
