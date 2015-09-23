## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set<- function(y){
               x<<-y
               m<<-NULL #restores to null the value of Inv, because the old Inv of the old matrix isn't needed anymore
       }
       get <- function() x #returns the vector x stored in the main function. Doesn't require any input.
       setInv <- function(solve) m <<- solve #stores a value (matrix) that should be Inv(x)
       getInv <- function () m
       list(set = set, get = get, #store the 4 functions in the function makeCacheMatrix
            setInv = setInv,
            getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#Input of cacheSolve is the object where makeVector is stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv() #verify if the value m, stored previously with getmean, exists and is not NULL
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get() #gets the matrix stored with makeCacheMatrix
        m <-solve(mat) #calculates de Inv of the matrix
        x$setInv(m) #stores it in the object generated assigned with makeCacheMatrix
        m
}
