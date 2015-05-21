\name{spMap}
\alias{spMap}

\title{spMap}

\description{
This function return a coverage calculate from all coverage vectors of ranges described in the GRanges object "exonInterest" given.
}

\usage{
spMap(exonInterest, mapReadsGRanges, spSite, padding, goal)
}

\arguments{
  \item{exonInterest}{
GRanges object containing coordinates of interest (these coordinates match with exon ones)
}
  \item{mapReadsGRanges}{
GRanges object containing mapped reads (generally from a bam file)
(see bamToGRanges())
}
  \item{spSite}{
Splice Site of interest ("5SS" or "3SS")
}
  \item{padding}{
Vector with the padding exon (pE) and the padding intron (pI) in the form of c(pE,pI)
(pE and pI correspond to the number of nucleotides wanted around the splice site on the exon and the intron)
}
  \item{goal}{
Function, applied between the different coverage vectors to get the wanted result into the return coverage vector.
This function can be "normMean" to get a normalized mean or a basic R function.
	- "normMean" : normalized mean
	- "sum"
	- "mean"
	- "median"
	- "max"
	- ...
}
}

\details{
The coverage vector represent the number of reads (depending on the chosen function "goal") at each nucleotide.
This function return a list containing the coverage vector, splice site, padding exon (pE) and padding intron (pI) in this order
You can access differents elements with :
<listName>[[i]] (i = 1, 2, 3 or 4)
}

\value{
list(coverage vector, splice site, pE, pI)
}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{exonInterestTreatment}}
\code{\link{mapReadsTreatment}}
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
}

\keyword{splice map}
\keyword{reads}
\keyword{coverage}
