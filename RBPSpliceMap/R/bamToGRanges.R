bamToGRanges <-
function(bamFile){
	# Function: transform a bam file into GRanges object

	#library(GenomicAlignments)	# Import
	bamFileA <- readGAlignments(bamFile)
	# Transformation into GRanges Object
	bamGRanges = as(bamFileA, "GRanges")
	
return (bamGRanges) }
