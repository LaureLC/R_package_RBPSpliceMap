\name{RBPSpliceMap-package}
\alias{RBPSpliceMap-package}
\alias{RBPSpliceMap}
\docType{package}

\title{
Summarize CLIPSeq data
}

\description{
This package is intended to manipulate CLIPSeq data and summarize them along genomic regions. Typically it can be use to draw RNA splice map such as those found in licatalosi et al. or using other summarize functions. It allows to select a chromosome, strand and interval of interest. It provides numerical and graphical output about the coverage of splice sites and their surroundings.
}

\details{
\tabular{ll}{
Package: \tab RBPSpliceMap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-06-18\cr
License: \tab GPL(>=2.0)\cr
}
}

\author{Laure Le Calvez <laure.le-calvez@laposte.net>}

\keyword{package}

\examples{
regulatedExons = system.file("extdata", "regulatedExons.bed", package="RBPSpliceMap", mustWork=TRUE)
iCLIPData = system.file("extdata", "iCLIPData.bam", package="RBPSpliceMap", mustWork=TRUE)
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
