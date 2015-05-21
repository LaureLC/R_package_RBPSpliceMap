strandSelect <-
function(objectGRanges, strand) {
	# Function: Select, in the GRanges object, what concern the chosen strand
	# IN : strand ("+","-","*")
	# OUT : GRanges object // package: GenomicRanges
	
	# If there is no more line in the GRanges (no read matches to interest ranges)
	# In this case, we return 0 to create a vector of zeros at the last step
	if (length(objectGRanges) == 0)	# if no read matches to this strand
		strandGR = 0		# will be transformed into a vector of zeros at the end
		
	else {
		if(strand == "+")
			strandGR = subset(objectGRanges, strand != "-")	# selection of the part of interest
		else if(strand == "-")
			strandGR = subset(objectGRanges, strand != "+")
		else
			strandGR = objectGRanges 
}
return(strandGR) }
