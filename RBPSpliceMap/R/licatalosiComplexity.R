licatalosiComplexity <-
function(x, window){
	#Convert vector x into a matrix with "window" columns
	windowMatrix = matrix(x, ncol = window, byrow = TRUE)
	
	#max/row -> vector size is divided by "window"
	littleComplexityVector = apply(windowMatrix, MARGIN=1, max)
	
	#Repete the vector "window" time
	complexityVectorRep = rep(littleComplexityVector, window)
	
	#Convert vector into a matrix with length(littleComplexityVector) columns
	complexityVectorRepMatrice = matrix(complexityVectorRep, ncol = length(littleComplexityVector), byrow = TRUE)
	
	#Convert matrix into a vector
	complexityVector = as.vector(complexityVectorRepMatrice)
	return(complexityVector)
}
