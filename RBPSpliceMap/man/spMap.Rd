\name{spMap}
\alias{spMap}

\title{spMap}

\description{
This function returns a summarized coverage calculated from all coverage vectors provided in the GRanges object "exonInterest".
}

\usage{
spMap(exonInterest, mapReadsGRanges, spSite, padding, goal, window = 50)
}

\arguments{
  \item{exonInterest}{
GRanges object containing coordinates of interest (typically exon coordinates)
}
  \item{mapReadsGRanges}{
GRanges object containing mapped reads (generally from a bam file)
(see \code{\link{bamToGRanges}}) (typically from a CLIPSeq experiment)
}
  \item{spSite}{
Splice Site of interest 5' or 3' splice site("5SS" or "3SS")
}
  \item{padding}{
A vector with the padding exon (pE) and the padding intron (pI) in the form of c(pE,pI)
(pE and pI correspond to the number of nucleotides required in the exon and in the intron)
}
  \item{goal}{
Function, applied between the different coverage vectors to get the wanted result into the return coverage vector.
This function can be "licatalosi" to get a normalized complexity, "normMean" to get a normalized mean or a basic R function.\cr
	- "licatalosi" : normalized complexity (see Details)\cr
	- "normMean" : normalized mean\cr
	- r-base function (ex: "sum", "mean")
}
  \item{window}{
Only necessary with licatalosi function. Size chosen for the window.\cr
DEFAULT = 50
}
}

\details{
The coverage vector represents the number of reads (depending on the chosen function "goal") at each nucleotide.\cr
This function returns a list containing the calculated coverage vector, splice site, padding exon (pE) and padding intron (pI). You can access differents elements with :
<listName>[[i]] (i = 1, 2, 3 or 4 respectively)\cr
"licatalosi" : The function is based on the "normalized complexity map of PTB-RNA" described by Licatalosi et al. (HITS-CLIP yields genome-wide insights into brain alternative RNA processing).
It takes as input a matrix of reads coverage where each row correspond to a different genomic region.
Each row is normalized by dividing by the sum of the read coverage for the row.The gene region is splitted  in N windows of length ("window") and for each window, the sum of the normalized coverage is calculated to obtain one normalized coverage vector representing the matrix.
In licatalosi et al. complexity is defined as "the number of different transcripts with a CLIP tag in the window".
The complexity vector correspond to the number of transcripts containing reads in each window.
The normalized complexity is obtained by multiplying the complexity vector by the normalized coverage vector.
The result, a normalized complexity coverage vector is returned by the function spMap.
}

\value{list(coverage vector, splice site, pE, pI)}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\seealso{
\code{\link{exonInterestTreatment}}\cr
\code{\link{mapReadsTreatment}}
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
}

\keyword{splice map}
\keyword{reads}
\keyword{coverage}
