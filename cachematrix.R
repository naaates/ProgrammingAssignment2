## This code is for the creation of two functions, makeCacheMatrix() and cacheSolve
## Basically, these two functions are used to cache the inverse of a matrix

## makeCacheMatrix() function is used to create a special matrix object
## This function: 
## (1) sets the value of the matrix, 
## (2) gets the value of the matrix...
## (3) Set the value of the matrix inverse and finally, 
## (4) get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve() function then computes for the Inverse of the... 
## Special matrix created by the function makeCacheMatrix in 7 steps total.
## Step by step: 
## First, (1) Get the inverse from the cache
## Then (2) Check if the inverse has already been calculated. If yes, return inverse.
## Then (3) If no inverse was calculated yet, it gets the matrix
## Then (4) Calculates the inverse of the matrix and store in variable I
## Then (6) Sets the value of the inverse via the setInverse() function
## Lastly (7) This function will return a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
        
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached matrix")
                return(I)
        }
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setInverse(I)
        I
}


## To check if the functions are working I compared the solve() function output
## and the cacheSolve(makeCacheMatrix()) function output.
s1 = c(1,2) #1st row creation
s2 = c(3,4) #2nd row creation
S = rbind(s1,s2) #binding the two rows
solve(S)
cacheSolve(makeCacheMatrix(S))

