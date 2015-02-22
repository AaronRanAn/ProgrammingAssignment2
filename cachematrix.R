## The makeCacheMatrix function creats a list that nested 4 functions:
## set, get, setinv and getinv. 

## The <<- operator assign a value to an object in an enviroment that is
## different from current one. 

## The key is to modify the example's code so that numeric -> matrix, mean -> inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ## This step stored the result of the matrix inversion. 
        
        set <- function(y) {
                x <<- y
                m <<- NULL  # Here we initialize the m to null. 
        }
        
        ## first function is a setter that set martix to object from makeCacheMatrix.
        
        get <- function() x  
        
        ## second function returns the input matrix.
        
        setinv <- function(inv) m <<- inv 
        
        ## third function sets the inversed matrix.
        
        getinv <- function() m 
        
        ## Fourth function returns the inverted matrix as a list 
        
        list (set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## The cacheSolve function inversed the speical matrix created by the makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinv() ## here we get the inversed matrix from x, which will be null if uncalculated.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }  ## If the result is not null, return the calculated inversed matrix. 
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinv(m)
        m  ## Return a matrix that is the inverse of 'x'
}

## Now let's see if this thing works!

test <- matrix(runif(4,1,50),2,2)

## Generate a random 2x2 matrix to test

testcached <- makeCacheMatrix(test)

testinv <- cacheSolve(testcached)

testinv