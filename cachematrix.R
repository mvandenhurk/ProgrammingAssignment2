## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The special "matrix" is actually a list containing functions to
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse of the matrix
## 4. get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates or retrieves from cache the inverse of the special "matrix" created by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ## If the inverse has already been calculated, the i operand is not NULL
        ## in which case cacheSolve skips computation and retrieves (returns) the inverse from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the inverse has not yet been computed, i is NULL
        ## in which case cacheSolve stores the matrix into the data variable,
        ## computes the inverse (i) of the data,
        ## and stores the calculated values into cache via the setinverse function
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}