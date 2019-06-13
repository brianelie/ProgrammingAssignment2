## Compute inverse and store it globally so it does not need to be computed
## every time

## Compute the inverse of a matrix and assign the inverse globally

makeCacheMatrix <- function(x = matrix()) {
    # Reset value of mean
    inv <- NULL
    # Define set function: globally assign x and ensure inverse is NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Return value of x
    get <- function() x
    # Define function "setinv" to globablly assign the inverse 
    setinv <- function(inverse) inv <<- inverse
    # Define function "getinv" to retrieve value of inverse
    getinv <- function() inv
    # Output is a list with named columns and our 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Test if the inverse has been previously computed.
## If yes, return value of inverse
## If not, compute inverse

cacheSolve <- function(x, ...) {
    # Attempt to retrieve inverse from previous calculation
    inv <- x$getinv()
    # Test if inverse exists
    if(!is.null(inv)) {
        # If yes, return inverse and leave function
        message("getting cached data")
        return(inv)
    }
    # Get matrix data from object
    data <- x$get()
    # Calculate the inverse of matrix
    inv <- solve(data)
    # Set inverse of object
    x$setinv(inv)
    # Return inverse
    inv
}
