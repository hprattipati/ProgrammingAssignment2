## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix -  Function creates a special "matrix" object that can cache its inverse.
## cacheSolve - Function computes the inverse of the special "matrix" returned by makeCacheMatrix

## Write a short comment describing this function

##makeCacheMatrix returns a list of functions to 
   ##set the matrix
   ##get the matrix  
   ## set the square inverse matrix
   ## get the square inverse matrix
makeCacheMatrix <- function(x = matrix()) {
     inve <- NULL
     set <- function(y)
                     {
                       x <<- y
                       inve <<- NULL
                     }
     get <- function() x
     setinv <- function(inverse) inve <<- inverse
     getinv <- function() inve
     list(set=set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve returns a matrix that is the inverse of 'x' which was input to makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inve <- x$getinv()
   if(!is.null(inve))
   {
     message("getting cached inverse matrix")
     return(inve)
   }
   calin <- x$get()
   inve <- solve(calin,...)
   x$setinv(inve)
   inve
   }
