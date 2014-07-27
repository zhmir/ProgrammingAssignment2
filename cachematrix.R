## Put comments here that give an overall description of what your
## functions do

## Overall, this program calculates the inverse of a given matrix. It avoid re-computing the matrix inverse by mean 
## of the caching the already computed result, thus saving the computational resources.

## Write a short comment describing this function

## This funtion declares unique matInverse variable which holds the inverse of a given matrix.
## It also defines/constructs four member funtions namely set(),get(),setInverse() and getInverse() to handle matInverse.


## sample 3x3 matrix:  mdat <- matrix(c(1,1,4, 0,3,1, 4,4,0), nrow = 3, ncol = 3)


makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  
  ## It seems '<<-' instructs R to search free variable outside the construction function's body and in parent's environment.
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(mInv) matInverse <<- mInv
  getInverse <- function() matInverse
  
  ## Commenting out following line and calling getInverse() results in Error: object of type 'closure' is not subsettable.
  ## Google search reveals that this list 'reserves' a list of function names so that their use as object names is avoided.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function actaully calculates the inverse of the matrix, only if it has not been already calculated. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInverse <- x$getInverse()
  if(!is.null(matInverse)) {
    message("getting cached inverse of the matrix")
    return(matInverse)
  }
  data <- x$get()
  matInverse <- solve(data)
  x$setInverse(matInverse)
  matInverse
}
