## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list containing four functions,
#1) set the value of the matrix
#2) get the value of the matrix
#3) set the inverse of the matrix
#4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}


## The follwing function cacheSolve calculates the inverse of the matrix. It first checks
## to see if the inverse has been calcuated; if not, it will calcuate it otherwise it will 
## fetch the value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
test1<-matrix(c(1,2,3,4),nrow=2,ncol=2)
a<-makeCacheMatrix(test1)
cacheSolve(a)