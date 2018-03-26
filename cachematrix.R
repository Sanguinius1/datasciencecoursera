
## We want to write two functions. The first, makeCacheMatrix, calculates a special matrix, inverts and caches it,
## while the second function, chacheSolve, returns the inversed matrix.

## makeCacheMatrix is a function which enables to calculate the inverse of a matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {

        ## mi stands for "matrix inverse"
        mi <- NULL
        
       
        set <- function(y){
                x <<- y
                mi <<- NULL
        }
     
        get <- function() x
        
     
        setinverse <- function(solve) mi <<- solve
        
    
        getinverse <- function() mi

        
    
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## chachSolve is a function that returns the inversed matrix of input "x".

cacheSolve <- function(x, ...) {
        
     
        mi <- x$getinverse()
        
        
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        
        data <- x$get()
        mi <- solve(data)
        x$setinverse(mi)
        
        mi
        }

