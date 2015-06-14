## This file contains code for 2 functions:
        ##makeCacheMatrix creates the list Subfunctions to:
        ##1. create a special matrix object
        ##2. cache a copy of the special matrix object for comparison
        ##3. retrieve a cache of the inverse of the special matrix object
        ##4. calculate and cache the inverse of the special matrix object

        ##cacheSolve uses the functions created by makeCacheMatrix to
                ## check to see if
                        ##Condition1 matrix has not changed, AND
                        ##Condition2 the inverse cache contains a matrix
                ##and if both conditions 1 and 2 are true,
                        ## retrieves inverse from cache
                        ## for fast and efficient result
                ##but if either condition is false,
                        ##calculates inverse from the new matrix
                                ## using solve
                                ## (and outputting solve error messages
                                ## for nonsquare or singular matrices)
                        ## caches the new matrix and new inverse
                        ## reports the inverse


makeCacheMatrix <- function() {
        #INITIALIZATIONS OF WORKING OBJECTS
        #initialize special matrix object, SMO
        SMO<<-matrix(NA)
        #initialize special matrix object cache, SMOCache
        SMOCache<<-SMO
        #initialize an empty inverse cache, SMOInverseCache
        SMOInverseCache<<-matrix(NA,nrow(SMO),ncol(SMO))
        #initialize a working matrix to hold the inverse, SMOInverse
        SMOInverse<<-SMOInverseCache

        #set up four subfunctions to retrieve from and write to the caches for
        #both SMO and SMOInverse
        #Subfunction SMORetrieve to call up the cached SMO into the SMO
        SMORetrieve<-function() {
                SMO<-SMOCache
                SMO
        }
        #Subfunction SMOStore to move the current SMO into the cache
        #and to reinitialize the SMOInverseCache to NAs (since SMO has been changed)
        #Also warns user when matrix change happens
        SMOStore<-function(SMO){
                        SMOCache<<-SMO
                        SMOInverseCache<<-matrix(NA,nrow(SMO),ncol(SMO))
                        message("matrix has changed,
caching new matrix and
reinitializing inverse")
        }

        #Subfunction SMOInverseRetrieve to retrieve the previously cached
        #SMOInverse from the cache
       SMOInverseRetrieve<-function(){
                SMOInverse<<-SMOInverseCache
                message("Retrieving inverse from cache")
                SMOInverse
        }

       #Subfunction SMOInverseStore writes SMOInverse to its cache
        SMOInverseStore<-function(SMOInverse){
                SMOInverseCache<<-SMOInverse
                message("new inverse has been cached")
        }
       #Output list of subfunctions makes them available for the cacheSolve
       #function later (and other functions as well)
       Subfunctions<<-list(SMORetrieve=SMORetrieve,
                        SMOStore=SMOStore,
                        SMOInverseRetrieve=SMOInverseRetrieve,
                        SMOInverseStore=SMOInverseStore)
}



## In order to return a matrix that is the inverse of SMO efficiently,
## cacheSolve checks to see if:
        ## SMO has changed AND if there are  already values in SMOInverseCache
                ##if yes to both questions, moves xInverseCache to xInverse
                ##if no to either question,
                        ##computes xInverse from the new x using solve
                        ##and caches xInverse in xInverseCache

cacheSolve <- function(SMO, ...) {
        ## Checks to see if SMO has changed and SMOInverseCache is non-empty
        ##and if both conditions met,
        ## executes SMOInverseRetrieve for fast and efficient result
        if(identical(SMO,SMOCache)&&!is.na(SMOInverseCache)){
                message("unchanged matrix from previous")
                Subfunctions$SMOInverseRetrieve()
        ## But if either condition above failed, computes SMOInverse
        ## and loads SMOInverse into the cache
        } else {
                ##Computes SMOInverse using solve
                SMOInverse<<-solve(SMO)
                ##and caches SMOInverse in SMOInverseCache
                Subfunctions$SMOStore(SMO)
                Subfunctions$SMOInverseStore(SMOInverse)
        }
        SMOInverse
}
