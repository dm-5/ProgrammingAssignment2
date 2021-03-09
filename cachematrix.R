## functions to demonstrate how to do caching in R

## the function makeCacheMatrix() uses R Scoping behavior to cache the inverse of a matrix
## it's somehow like a static variable in C/C++ 
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL

    setMat <- function(y = matrix())
    {
        x <<- y
        inv <<- NULL
    }
    
    getMat <- function(){
        x
    }

    getInv <- function(){
        inv
    }
    
    setInv <- function(i){
        inv <<- i
    }
    
    list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


## the function cacheSolve() uses the object provided by makeCacheMatrix
## to return the inverse of a matrix by calculting it or getting it from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInv()
    
    if( ! is.null(i) )
    {
        print("cacheSolve(): retrieving inverse from cache")
        return(i)
    }

    print("cache is empty - calculating inverse")
    i <- solve(x$getMat(), ...)
    x$setInv(i)
    
    i
}

# Testing the functions makeCacheMatrix / cacheSolve

# one matrix
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

m <- makeCacheMatrix(A)

print("1. call of cacheSolve(A)")
s1 <- cacheSolve(m)
print(s1)

print("2. call cacheSolve(A)")
s2 <- cacheSolve(m)
print(s2)


# another matrix
B <- matrix( c(3, 3, 6,
               3,-1, 2,
               4, 1,-1), nrow=3, byrow=TRUE)


m$setMat(B)

print("1. call of cacheSolve(B)")
s1 <- cacheSolve(m)
print(s1)

print("2. call cacheSolve(B)")
s2 <- cacheSolve(m)
print(s2)

