## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    cx = makeCacheMatrix(x)
    if (is.null(cx$getinverse()))
    {
        cx$setinverse(solve(x))
    }
    cx$getinverse()
}

nx <- 3
ny <- 3
row_names <- c('r1','r2','r3')
col_names <- c('c1','c2','c3')

x = c(2,1,1,3,2,1,2,1,2)
dim(x) <- c(nx, ny)
dimnames(x) <- list(row_names, col_names)

xi = c(3,-1,-1,-4,2,1,-1,0,1)
dim(xi) <- c(nx, ny)
dimnames(xi) <- list(row_names, col_names)

y = cacheSolve(x)
z = cacheSolve(x)
