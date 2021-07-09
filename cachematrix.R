 ## The function "makeCachematrix" creates a special "matrix" object that will be able to cache the inverse for its input
 
 makeCacheMatrix <- function(x = matrix()) {
     
     jeo <- NULL
     set <- function(y) {
         x <<- y
         jeo <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) jeo <<- inverse
     getInverse <- function() jeo
     list(set = set, get = get, setInverse = setInversev, getInverse = getInverse)
 }
 
 
 ## The cacheSolve function is the function that solves the inverse of the special "matrix" that was created by the makeCachematrix function.

 cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     jeo <- x$getInverse()
     if(!is.null(jeo)) {
         message("getting cached result")
         return(jeo)
     }
     data <- x$get()
     jeo <- solve(data, ...)
     x$setInverse(jeo)
     jeo
 }
