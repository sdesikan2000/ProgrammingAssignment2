## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix :- This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize
        m <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## Set the inverse of the matrix 
        setinverse <- function(inverse) m <<- inverse
        
        ## Get the inverse of the matrix
        getinverse <- function() m
        
        ## Return a list of the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve :- This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()
       
       ## Return the inverse if it already exists in cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from the object
        data <- x$get()
        
        ## Calcaute the inverse using matrix
        m <- solve (data, ...)
        
        ## set the inverse 
        x$setinverse(m)
        
        ## return the matrix
        m

}
