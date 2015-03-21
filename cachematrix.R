## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
}


# this function creates four functions: set, get, setInv
# and getInv. it uses <<- assignment operator so that

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL # storing inversion here
        set <- function(y) {
                x <<- y
                xinv <<- NULL # xinv to null
        }
        
        get <- function() x # return the input matrix
        setInv <- function(inv) xinv <<- inv # inversed matrix 
        getInv <- function() xinv # getting the inversed matrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


cacheSolve <- function(x, ...) {
        m <- x$getInv() # here we get the inversed matrix from object x
        if(!is.null(m)) { # if the inversion result is there
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get() # if not, we do x$get to get the matrix object
        m <- solve(data) # solve it
        x$setInv(m) # we then set it to the object
        m # return the solved result
}