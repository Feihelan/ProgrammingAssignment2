###########################################
##Assignment2: Caching the Inverse of a Matrix
############################################
## create functions  that cache the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialized inverse 
        inv <- NULL
        ## set matrix 
        set <- function(y=matrix()) {
                x <<- y
                inv <<- NULL }
        ## get the matrix 
        get <- function() {
                x
        }
        ## set the inverse the matrix 
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        ## get the inverse  the matrix 
        getinverse <- function() {
                inv
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## return the  inverse if its already set 
        if(!is.null(inv) && is.matrix(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if it is not set 
        ##get the matrix 
        data <- x$get()
        ## calculate the inverse of the matix 
        inv <-solve(data,...)
        ##inv <- solve(data)%*%data
        
        ## set the inverse 
        x$setinverse(inv)
        ## get the inverse matrix 
        inv
}
