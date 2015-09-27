#####################################################
# This function creates a special "matrix" object
# that can cache its inverse.
# Rather like a class, except it's a function. 
# R is an odd language. 
#####################################################
makeCacheMatrix <- function(x = matrix()) {

        # This is the inverse matrix
        inverse <- NULL

	# Sets the value of the matrix, if y is a square matrix.  
        set <- function(y) {
		if (nrow(x) != ncol(x)) {
			stop("Inverses can only be computed for square matrices!")
		}
         	x <<- y
                inverse <<- NULL
		
        }
      
	# Returns the matrix.
	get <- function() x
      
	# Sets the inverse.
	setInverse <- function(i) inverse <<- i
      
	# Returns the inverse.
	getInverse <- function() inverse

        # Returns all functions.
 	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}



##########################################
# Finds the inverse of the special matrix created with the above function. 
# First checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setInverse function.
##########################################
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
