## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()){
#creates a special "matrix" object that can cache its inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ##set and save original matrix
        
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        ##set and save inverse matrix
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##see if there has already calculated the inversed matrix,if not, then go on
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        #save the inversed matrix
        m
}