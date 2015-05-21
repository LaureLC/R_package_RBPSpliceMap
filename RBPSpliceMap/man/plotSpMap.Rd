\name{plotSpMap}
\alias{plotSpMap}

\title{plotSpMap}

\description{
This function is a way to represent the coverage in reads of a splice Site and its surrondings
}

\usage{
plotSpMap(spMapList)
}

\arguments{
  \item{spMapList}{
List of 4 vectors
	spMapList[[1]] : coverage vector
	spMapList[[2]] : splice site ("3SS" or "5SS")
	spMapList[[3]] : paddingExon (number)
	spMapList[[4]] : paddingIntron (number)
}
}

\details{
padding : - paddingExon and paddingIntron correspond to the number of nucleotides wanted around the splice site on the exon and the intron
- This function create a representation of the coverage of a "splice site" (x=0) between paddingIntron (x= pI-1) and paddingExon (x=pE)
- paddingIntron is used with -1 because x=0 is the splice site and it is included in the Intron
- The return graph represent the coverage of the part of interest of the exon (or multiple exon if you used spMap() with multiple regions)
}

\value{Graph}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{spMap}}
\code{\link{coverageVector}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Create a GRanges from a bam file
mapReadsGRanges = bamToGRanges(iCLIPData)
#Normalized mean of coverages of ranges of interest on the 3' splice site
spMapList3SS = spMap(exonInterest, mapReadsGRanges, spSite = "3SS", padding =  c(40, 200), goal = "normMean")
#Mean of coverages of ranges of interest on the 5' splice site
spMapList5SS = spMap(exonInterest, mapReadsGRanges, spSite = "5SS", padding =  c(100, 50), goal = "mean")
#3' splice site representation
plotSpMap(spMapList3SS)
#5' splice site representation
plotSpMap(spMapList5SS)
}

\keyword{mapping}
\keyword{reads}
\keyword{splice site}
