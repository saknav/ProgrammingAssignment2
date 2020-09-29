## Pairs of functionthat cahe the inverse of a matrix
## functions do

## create special matrix object tha can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## setting matrix
        set <- function(y){
                x <<- y
                m <<- NULL

}
        ## to get matrix
get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list (set=set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }


## Compute  inverse of special matrix returned by "makeCacheMatrix" function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cacheSolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
        if (!is.NULL(m)) {
                message ("getting chached data")
                return(m)
  }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        
