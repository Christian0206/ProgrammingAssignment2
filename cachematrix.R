##These functions will be used to calculate the inverse of a matrix and cache it.


##                      makeCacheMatrix Description
##                      ---------------------------
## this is a faction that returns a list with four elements each one of them
## will be calculated later.
## the function checks the provided argument whether it is a squared 
## invertible matrix or not. 

makeCacheMatrix <- function(x = matrix())
{
        if(is.matrix(x))
        {
                if(!(dim(x)[1] == dim(x)[2]) | (det(x)==0))
                {
                        message("The provided Matrix is not a square one or it is singular")
                        invisible(return)
                }
                invCache<-NULL
                setM<-function(val)
                {
                        x <<- val
                        invCache <<- NULL
                }
                getM <- function() 
                {
                        x
                }
                setI <- function(val)
                {
                        invCache <<- val
                }
                getI <- function() 
                {
                        invCache
                }
                list(Mat = getM, Inv = getI, NewM = setM , NewInv = setI)
        }
        else 
        {
                message("argument x must be a matrix")
        }
}


##                              cacheSolve Description
##                              ----------------------
## This function will check if the inverse is already calculated and if
## that's true then it will check if the matrix has changed and if that is
## also true the cached value will be returned; otherwise the inverse
## would be calculated.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        invCache <- x$Inv()
        orMatrix <- x$Mat()
        if(!is.null(invCache)) 
        {
                if(identical((invCache %*% orMatrix),diag(dim(orMatrix)[1])))
                {
                        message("getting cached data")
                        invCache
                }                
                else
                {
                        invCache <- solve(orMatrix)
                        x$NewInv(invCache)
                        invCache
                }
        }
        else
        {
                invCache <- solve(orMatrix)
                x$NewInv(invCache)
                invCache
        }
}