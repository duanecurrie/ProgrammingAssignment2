
# The functions in this file implement a wrapper for caching the 
# value of the inverses of matrices.  It implements two base functions:
#     - makeCacheMatrix(mat): 
#           Creates a wrapper object for the supplied matrix.  The wrapper
#           provides get() and set() member functions to retrieve and set
#           the value of the wrapped matrix.
#     - cacheSolve(cacheMatrix):
#           Returns the inverse of the provide cacheMatrix, computing and
#           caching the inverse if necessary

# Synopsis:
#     # Use makeCacheMatrix(matrix) to create a wrapper for a matrix
#     m <- matrix(c(1,2,2,1), 2, 2)
#     cached_m <- makeCacheMatrix(m)
#
#     # Use cacheSolve to find the inverse of a matrix
#     m_inv <- cacheSolve(cached_m)
#
#     # Use $get() to retrieve the source matrix for use with other operations
#     m_mean <- mean(cached_m$get())
#
#     # Use $set(m) to set a new value for the matrix
#     cached_m$set( cached_m$get() * 2 )
#     new_inv <- cacheSolve(cached_m)



## Function:  makeCacheMatrix
##     Creates a wrapper for a matrix which allows caching of the inverse.
## Usage:
##     m <- matrix(c(1,2,2,1), 2, 2)
##     cached_m <- makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
    # Variable to store the cached inverse
	x_inv <- NULL
	
	# Set the new value of the matrix and invalidate the cache
	set <- function(new_matrix) {
		x <<- new_matrix
		x_inv <<- NULL
	}
	
	# Return the wrapped matrix
	get <- function() x
	
	# Set the inverse to the provided value
	setinv <- function(new_inv) x_inv <<- new_inv
	
	# Return the cached inverse, or NULL if the cache is invalid
	getinv <- function() x_inv

	# Return list object with the get and set functions for the matrix 
	# and cached inverse.
	list(set=set,       get=get, 
	     setinv=setinv, getinv=getinv)
}



# Function:  cacheSolve
#     Return the inverse of a cacheMatrix.  Computes and caches the
#     inverse if not already cached.
# Usage:
#     m_inv <- cacheSolve(cached_m)

cacheSolve <- function(x, ...) {
    # Retrieve the matrix's cached inverse
	x_inv <- x$getinv()
	
	# If the inverse is valid, then return it, and note the use of the cache.
	if (!is.null(x_inv)) {
		message("getting cached data")
		return(x_inv)
	}
	
	# Otherwise, compute and set the inverse
	data <- x$get()
	data_inv <- solve(data, ...)
	x$setinv(data_inv)
	
	# Return the newly computed inverse
	data_inv
}
