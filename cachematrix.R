makeCacheMatrix <- function(x = matrix()) {

                ## set the matrix
                ## get the matrix
                ## set the inverse
                ## get the inverse
        
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
                ## caches a list for cacheSolve    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
                  inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                  
                ## check if inverse has been computed
                  
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
              return(inv)
        
        ## Return a matrix that is the inverse of 'x'
}
