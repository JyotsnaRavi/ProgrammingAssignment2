
## Computing inverse of big matrices involves huge compuatation. Hence, it would be better
## to take it from Cache if the inverse of the same matrix has to be calculated repeatedly.
##"makeCacheMatrix" and "cacheSolve" are two functions written below which will calculate inverse of matrix 
## and also can retrieve the inverse if it has already been calcuated once.

## "makeCachematrix" creates and returns list of functions to set a matrix, get the matrix, set the inverse of matrix and get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" computes inverse of matrix. It first checks if the inverse is already calcuated.
## If so, it displays"getting cached data" and then prints the inverse of the matrix stored already.
## If not, it  calcuates the inverse and prints it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)## Calcualtes inverse of Matrix
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}


##xy<-matrix(1:4,2,2)
##a<-makeCacheMatrix(xy)
##cacheSolve(a)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## cacheSolve(a)
## getting cached data
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5