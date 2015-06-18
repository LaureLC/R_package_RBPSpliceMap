\name{plotSpMap}
\alias{plotSpMap}

\title{plotSpMap}

\description{
This function take as input the output of (\code{\link{spMap}}) and plot a RNA splice map around the chosen splice site.
}

\usage{
plotSpMap(spMapList, ylim = c(0, max(spMapList[[1]])), type = "s", color = "black")
}

\arguments{
  \item{spMapList}{
List of 4 vectors \cr
	spMapList[[1]] : coverage vector\cr
	spMapList[[2]] : splice site ("3SS" or "5SS")\cr
	spMapList[[3]] : paddingExon (number)\cr
	spMapList[[4]] : paddingIntron (number)\cr
	(typically comming from \code{\link{spMap}})
}
  \item{ylim}{
numeric vector of length 2, giving the y coordinates ranges.
}
  \item{type}{
what type of plot should be drawn.
}
  \item{color}{
default color of all points and lines
}
}

\details{
padding : - paddingExon and paddingIntron correspond to the number of nucleotides to be analysed around the splice site in the exon and the intron\cr
- This function creates a representation of the coverage of a "splice site" (x=0) between paddingIntron (x= pI-1) and paddingExon (x=pE)\cr
- paddingIntron is used with -1 because x=0 is the splice site and part of the Intron\cr
- The returned graph represents the coverage of the region of interest
}

\value{Graph}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{spMap}}\cr
\code{\link{coverageVector}}
}

\examples{
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
#Create GRanges object from a bedFile with exons of interest
data <- read.table(regulatedExons, header = TRUE)
colnames(data) = c('chr', 'start', 'end', 'ProbeID', 'pval', 'strand')
exonInterest = with(data, GRanges(chr, IRanges(start, end), strand))
#Create a GRanges object from a bam file
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
