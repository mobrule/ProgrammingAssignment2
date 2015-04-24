makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {    #Set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x#Get the value of the matrix
        setinv <- function(solve) inv <<- solve #Set the value of the matrix inverse
        getinv <- function() inv #Get the value of the matrix inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the matrix returned by above function. If the inverse has been calculated and the matrix is unchanged,
#function retrieves inverse from previous function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
