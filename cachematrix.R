## These functions allow to reduce the computation time of the inverse of a matrix by retrieving the inverse in the cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set<- function(y){
               x<<-y
               m<<-NULL #restores to null the value of Inv, because the old Inv of the old matrix isn't needed anymore
       }
       get <- function() x #returns the matrix x stored in the main function. Doesn't require any input.
       setInv <- function(solve) m <<- solve #stores a value (matrix) that should be Inv(x)
       getInv <- function () m
       list(set = set, get = get, #stores the 4 functions in the function makeCacheMatrix
            setInv = setInv,
            getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve should retrieve the inverse from the cache.
#Input of cacheSolve is the object where makeCacheMatrix is stored.
#The first time the functions are runned, there is no value in the cache, so Inv(x) is calculated

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
