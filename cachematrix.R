## These function implements the concept of cache to save costly computation again and again, in this case the inverse of the matrix
## makeCacheMatrix create a matrix which holds both the matrix data as well as its inverse as well
## cacheSolve class check before computing the inverse that if the inverse of current matrix has been computed already or not
## if the inverse has not been computed, it computes the inverse and saves with matrix and uses it in future to save time

## This function is similar to the example
## makeCacheMatrix holds the matrix and its inverse
## Initially and whenever new matrix is set through set function
## then i (inverse of matrix) is made zero as it needed to be recalculated
## it exposes setrer and getter for both matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inv) i <<- inv
	
	getinverse <- function() i
	
	list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## To compute the inverse of matrix it first tries to get the inverse of matrix from the passed x matrix
## if found it prints the message and returns the inverse otherwise it computes the inverse, saves it with matrix and returns the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	i <- x$getinverse()
    
	if(!is.null(i)) {
		message("getting cached data")
        return(i)
	}
	
	data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
	i
}
