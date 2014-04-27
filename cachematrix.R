## The functions makeCacheMatrix and cacheSolve are used to create a special object 
## that stores a matrix and cache's its inverse. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## The function creates a special "matrix", which is really a list 
        ## containing a function to
        ## set the value of the matrix (set), get the value of the matrix (get), 
        ## set the value of the inverse (setsolve),  get the value of the inverse (getsolve)
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## cacheSolve returns a matrix that is the inverse of 'x'
        ## checks to see if the inverse has already been calculated. 
        # If so, it gets the inverse from the cache and skips the computation. 
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise, it calculates the inverse of the data and sets the value 
        ## of the inverse in the cache via the setsolve function.
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
