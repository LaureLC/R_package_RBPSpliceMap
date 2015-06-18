spMap <-
function(exonInterest, mapReadsGRanges, spSite, padding, goal, window = 50) {
	# Function: calculate a cover vector from all cover vector described in the given "exonInterest" GRanges object
	# IN : padding = c(pE, pI), spSite = "5SS" ou "3SS", goal = (ex: "normMean", "licatalosi", "sum", "mean", ...)
	# OUT: list with a cover vector, the splice site and the padding
	
	# padding treatment
	pE = padding[1]  	# padding exon
	pI = padding[2]
	
	# Modification of coordinates of the GRanges according to the padding and the splice site
	modifiedExonInterest = exonInterestTreatment(exonInterest, spSite, pE, pI)

	# for each coordinates (strand, chromosome, from and to) we calculate the cover
	# We put each vector, one by one in a matrix
	for (i in 1:length(modifiedExonInterest)){
		# Chromosome into the seqnames column of the GRanges
		chromosome = toString(seqnames(modifiedExonInterest[i]))
		# Strand into the strand column of the GRanges
		strand = toString(strand(modifiedExonInterest[i]))
		from = start(modifiedExonInterest[i])
		to = end(modifiedExonInterest[i])
		coverVector = mapReadsTreatment(mapReadsGRanges, chromosome, strand, from, to)
		if (i == 1)
			tableCoverVector = matrix(coverVector, nrow = 1, byrow = TRUE)
		else
			tableCoverVector = rbind(tableCoverVector, coverVector)
	}
	
	# Application of the "goal" (function asked by the user)
	# Normalized mean
	if (goal == "normMean") {
		normVectors = apply(tableCoverVector, MARGIN=1, function(x) x/max(x))
		# Some vectors can only contain zeros so the max is 0
		# A division by 0 give here a "NaN" in the matrix, we replace it by 0
		normVectors[normVectors == "NaN"] = 0
		result = apply(normVectors, MARGIN=1, mean)
	}
	
	#licatalosi function
	else if (goal == "licatalosi") {
		if ((pE + pI)%%window != 0)
			return("for licatalosi function, padding(pE + pI) has to be a multiple of window")
		else{
			sumLine = apply(tableCoverVector, MARGIN=1, function(x) x/sum(x)) # /!\ INVERSE MATRIX
			# Some vectors can only contain zeros so the sum is 0
			# A division by 0 give here a "NaN" in the matrix, we replace it by 0
			sumLine[sumLine == "NaN"] = 0	
			
			#Creation of complexity matrix
			complexity = sumLine
			complexity[complexity > 0] = 1
			
			#Application of licatalosiComplexity function to each column
			complexityMatrix = apply(complexity, MARGIN=1, function(x) licatalosiComplexity(x, window))
			
			#Sum of each line
			complexityResult = apply(complexityMatrix, MARGIN=1, sum)
			
			#Application of sumLineMatrix function to each column
			sumLineMatrix = apply(sumLine, MARGIN=2, function(x) sumWindow(x, window))
			
			#Sum of each line
			sumLineResult = apply(sumLineMatrix, MARGIN=1, sum)
			
			result = complexityResult * sumLineResult
		}
	}
	
	# Other functions (ex: sum, mean, ...) -> functions already existing in R
	# If the user want to use another function between cover vectors
	else
		result = apply(tableCoverVector, MARGIN=2, FUN=goal) #MARGIN = 2 -> calculate by column
	
		spMapList = list(cover = result, spSite = spSite, paddingExon = pE, paddingIntron = pI)
		#pI+1 because at the beginning of the function : pI = pI - 1  -> It's to give back the good pI
	
return(spMapList)
}
