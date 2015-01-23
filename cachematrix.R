## The makeCacheMatrix and cacheSolve functions provide a diffetent way to save a matrix and
## its inverse as a single R object. Both the functions check for NULL values before working
## on the input R object.

## The makeCacheMatrix accepts a matrix and defines four functions inside. It returns
## a list of those four functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        if(is.null(x)){
                message("Wrong Input!\nReturning NULL")
                return(NULL)
        }
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The cacheSolve function checks if the inverse has already been calculated, if yes then it 
## returns the inverse and if not then it calculates the inverse, updates the object with the
## calculated inverse and returns the inverse

cacheSolve <- function(x) {
        if(is.null(x)){
                message("x is not an acceptable R object\nReturning NULL")
                return(NULL)
        }
        inv <- x$getInv()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(x$get())
        x$setInv(inv)
        inv
}