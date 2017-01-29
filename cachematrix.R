## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix( numeric() )) {
        # creates a new matrix for  the inverse.
        m <- matrix(numeric()  ,ncol=ncol(x), nrow=nrow(x))

        # creates a set function for setting a new matrix and its inverse  
        set <- function(y) {
                x <<- y
                m <<- matrix(numeric()  ,ncol = ncol(x), nrow = nrow(x))  
        }

        # Read the matrix
        get <- function() x
        # function for setting the inverse
        setinv <- function(solve) m <<- solve
        # function for getting the inverse    
        getinv <- function() m
        # list returning functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # getting the inverse matrix from the getinverse function
        m <- x$getinv()
        #Checking if the inverse matrix is set or not. Returns inverse if available in the catche.
        if(!all(is.na(m))) {
                message("getting cached data")
                return(m)
        }
        # Reading the matrix if its inverse is not available.
        data <- x$get()
        # solving the inverse.
        m <- solve(data)
        # setting the inverse in the setinv function.
        x$setinv(m)
        # Return inverse
        m

}

