## This is an adapted version of the vector-mean function provided by Prof Peng.
## This R function was written as a requirement of the R Programming course from
## Coursera's Data Science Specialization (programming assignment 2). The
## purpose of the exercise was to develop a function that could cache the
## inverse of a matrix, allowing for improved code performance, i.e. faster
## matrix inverse operations.

## Sets up a list that allows for the caching of the inverse of a matrix.
### The makeCacheMatrix function makes a special ``vector'' (list) that enables
### the caching of the x-matrix inverse. The list contains functions that: (1)
### set the matrix; (2) get the matrix; (3) set the inverse of the matrix; and
### (4) get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
### First try to get the cached inverse of the matrix. If inverse has been
### calculated, the cache is accessed and returned. Otherwise, the inverse is
### calculated and then cached, which takes longer than accessing a cached
### result.
cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinv()

    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
