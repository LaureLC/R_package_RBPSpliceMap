chromosomeSelect <-
function(objectGRanges, chromosome) {
	#Function: Select, in the GRanges object, what concern the chosen chromosome
	# OUT : GRanges object // package: GenomicRanges
	
	chromosomeGR = subset(objectGRanges, seqnames == chromosome)
	
	# If there is no more line in the GRanges (no read matches to interest ranges)
	# In this case, we return 0 to create a vector of zeros at the last step
	if (length(objectGRanges) == 0)	# if no read matches to this strand
	chromosomeGR = 0		# will be transformed into a vector of zeros at the end
	
	return(chromosomeGR)
}
