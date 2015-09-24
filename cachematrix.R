## Put comments here that give an overall description of what your
## functions do
## The functions calculate the inverse of matrix and cache the result.

## Write a short comment describing this function
## The first function creates an object to store the calculated inverse of matrix.

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The second one checks whether the inverse of the object is already calculated.
## If yes, the stored value is returned; otherwise, the inverse is calculated, stored and returned.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

nx <- 3
ny <- 3
row_names <- c('r1','r2','r3')
col_names <- c('c1','c2','c3')

x = c(2,1,1,3,2,1,2,1,2)
dim(x) <- c(nx, ny)
dimnames(x) <- list(row_names, col_names)

cx = makeCacheMatrix()
cx$set(x)

y = cacheSolve(cx)
z = cacheSolve(cx)
