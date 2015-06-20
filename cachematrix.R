## makeCacheMatrix stores of 4 functions, which in final result enable to invert matrix
## set - change the value of variable x
## get - show matrix x
## setsolve - store inverted matrix x under variable s
## getsolve - show variable (matrix x)

## sample matrix
x <- matrix(sample(9),3,3)

makeCacheMatrix <- function(x = matrix()) {
        
## s <- inverted matrix
## clean up s value, in case any values are stored from earlier calculations
        s <- NULL
        
## enables changing matrix x with matrix y, so it would be different than
## provided in the input
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
## show matrix x
        get <- function() x
        
## store value of inverted matrix x (that means of matrix s)
        setsolve <- function(solve) s <<- solve
        s <- solve(x)
## show matrix s (that means show inverted matrix x)
        getsolve <- function() s
        
## list of all functions used in makeCacheMatrix()
        list(set = set, get = get, setsolve = setsolve,
             getsolve= getsolve)        
}


## cacheSolve calculates inverted matrix x

cacheSolve <- function(x, ...) {
## assign value to s
## it uses getsolve() function from makeCacheMatrix()
        s <- x$getsolve()
        
## check if matrix s is not empty, so that inverting can be done
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
## create variable storing matrix x
        data <- x$getmatrix()
        
## store solved data (means solved matrix x) under s variable
        s <- solve(data)
                
        x$setsolve(s)
        
## show matrix x (means show inverted matrix x)
        s
}

