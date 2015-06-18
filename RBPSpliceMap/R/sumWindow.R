sumWindow <-
function(x, window){
	#Convert vector x into a matrix with "window" columns
	windowMatrix = matrix(x, ncol = window, byrow = TRUE)
	
	#max/row -> vector size is divided by "window"
	littleSumVector = apply(windowMatrix, MARGIN=1, sum)
	
	#Repete the vector "window" time
	sumVectorRep = rep(littleSumVector, window)
	
	#Convert vector into a matrix with length(littleComplexityVector) columns
	sumVectorRepMatrice = matrix(sumVectorRep, ncol = length(littleSumVector), byrow = TRUE)
	
	#Convert matrix into a vector
	sumVector = as.vector(sumVectorRepMatrice)
	return(sumVector)
}
