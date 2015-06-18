coverageVector <-
function(cover, from = 0, to) {
	# Function: to recover cover datas into a single vector
	# IN : cover (Objet Rle  //  package: S4Vectors)
	# OUT: vector
	if (to > length(cover))
		cover = cover[from:length(cover)]
	else
		cover = cover[from:to]
	lengths = cover@lengths
	values = cover@values
	# Creation of a real rle
	testRle = list(lengths,values)
	names(testRle) = c("lengths","values")
	# Inversion of the rle to obtain a vector of the cover nucleotide by nucleotide
	coverVector = inverse.rle(testRle)
	# Sometimes reads don't go to the end. Before, we controlled that the begginning begin at the same point
	# To have the same vector length at the end, we add 0 (for 0 reads) at the end of the vector
	# to - from + 1 because to is included
	if (length(coverVector) < to - from + 1){
		
		for (i in (length(coverVector) + 1):(to - from + 1)){
			coverVector[i] = 0 
		}
	}
	
	coverVector = coverVector[1:(to - from + 1)]
	return(coverVector) }
