## Caching the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   get <- function() x
   setInv <- function(solve) m <<- solve
   getInv <- function() m
   list(set=set, get=get,
        setInv=setInv,
        getInv=getInv)
}


## Computes inverse of special "matrix" returned by makeCacheMatrix. Retrieves inverse from cache if already calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getInv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   
   data <- x$get()
   m <- solve(data, ...)
   x$setInv(m)
   m
}
