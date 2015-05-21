\name{strandSelect}
\alias{strandSelect}

\title{strandSelect}

\description{
This function return the part of the GRanges object about the input "strand"
}

\usage{
strandSelect(objectGRanges, strand)
}

\arguments{
  \item{objectGRanges}{
GRanges object from where we want to select the part concerning the strand of interest
}
  \item{strand}{
strand of interest as a string ("+", "-", "*")
"*" correspond to the 2 strands -> return the entire GRanges object
}
}

\value{GRanges object}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{intervalSelect}}
\code{\link{chromosomeSelect}}
\code{\link{mapReadsTreatment}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Select the strand of interest
strandSelect(mapReadsGRanges, strand = "+")

regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Select the strand of interest
strandSelect(regulatedExons, strand = "+")
}

\keyword{strand}

\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
