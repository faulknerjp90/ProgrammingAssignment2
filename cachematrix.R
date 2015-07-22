#makeCacheMatrix is a function that contains functions within it:
# - setMatrix, getMatrix, cacheInverse & getInverse

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        getInverse <- function() {
                cache
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

#cacheSolve gives the inverse of a matrix that is created by using the "makeCacheMatrix" function
cacheSolve <- function(y, ...) {
        inverse <- y$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        inverse
}
