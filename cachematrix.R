
## `makeCacheMatrix` creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        ## inv holds the cached value or NULL if nothing is cached
        inv <- NULL
        
        ## set stores the matrix
        set <- function(y) {
                x <<- y
                
                ## setting cache to NULL because of the new Matrix
                inv <<- NULL
        }
        
        ## gets the stored matrix
        get <- function() {x
        }
        
        ## caches the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        ## get the cached value
        getInverse <- function() {
                inv
        }
        
        list(set = set,
             get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## gets the cached inverse
        inv <- x$getInverse()
        
        ## when a cached inverse exists, it will get returned
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## else get matrix, calculate inverse and store it in cache
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        
        ## return inverse
        inv
}
