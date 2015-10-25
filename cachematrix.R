## Put comments here that give an overall description of what your
## functions do

## This function returns a list which holds 4 functions and a variable (which will be evaluated)
## as per R rules (Parent frame and its parent frame and so on)
makeCacheMatrix <- function(x = matrix()) 
{
        #local variable per the list returned which will hold the matrix inverse
        inverseofx = NULL
        
        #function to set the value of the matrix
        set <- function(y) 
                {
                        x <- y
                        inverseofx <- NULL
                }
        
        #get the value of the matrix
        get <- function() 
                {
                        x
                }
        
        #set the inverse of the matrix to re use later if the same object is used
        setinverse <- function(inverse)
                {
                        inverseofx <<- inverse
                }
        
        #returns the cache inverse of the matrix
        getinverse <- function()
                {
                        inverseofx
                }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function which will utilize the list returned by the makeCacheMatrix function
## to not to re compute the inverse if it is already computed
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse = x$getinverse()
        if(!is.null(inverse)) 
        {
                message("getting cached data")
                return(inverse)
        }
        
        inverse = solve(x$get())
        x$setinverse(inverse)
        inverse
}
