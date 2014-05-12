## Creating the inverse of a matrix is often a very expensive operation.
## So it make sense to cache the result of a matrix inversion
## even if the result of this specific inverse will be used frequently.

## A set of function to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverted_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverted_matrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverted_matrix <<-inverse
    getInverse <- function() inverted_matrix
    list(set = set,
		     get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function will return the inverse of the matrix created by makeCacheMatrix.
## If one will be found it returns the cached inverse.
## Otherwise it will be computed then cached and finally returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted_matrix <- x$getInverse()
    if ( ! is.null(inverted_matrix)) {
        print("Cached inverse matrix found.")
        return(inverted_matrix)
    }
	else {
		inverted_matrix <- solve(x$get())
		x$setInverse(inverted_matrix)
		return(inverted_matrix)
	}
}
