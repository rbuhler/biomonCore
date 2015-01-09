# -- CONSTRUCTOR
global <- function(arg){
  return(new(Class = "Global", arguments = as.list(arg)))
}
#' Class Global
#' 
#' The Global class is responsible to set the Global Environment with the expected variables. The analyst will consider what is defined by this class in time of defining the CSV analysis file.
#' 
#' @param arg A list of arguments to be used by the class initializer.
#' @slot arguments A list of arguments informed in time of creating the object.
#' 
setClass( Class="Global",
          representation( arguments = "list" ),
          # -- INSPECTOR 
          validity=function(object){ 
            if(TRUE){
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
)
# -- INITIALIZER
setMethod( f="initialize",
           signature="Global",
           definition=function( .Object, 
                                arguments ){ 
             #--- INITIALIZER
             vSepDir <- .Platform$file.sep
             
             #--- Ensure the DATA folder/directory is there
             vDataDir <- paste0(getwd(), vSepDir, "data")
             if (!file.exists(vDataDir)){
              dir.create(vDataDir, mode = "0777")
             }else{ TRUE }

             #--- Check whether gbl.RData exists or not
             rData_dir    <- "gbl.RData"
             tryCatch({load( rData_dir, .GlobalEnv )},
                      warning = function(w){
                        #--- Analysis repository
                        vAnalysis_dir <- paste0(vDataDir, vSepDir,"analysis", vSepDir)
                        if (!file.exists(vAnalysis_dir)){
                          dir.create(vAnalysis_dir, mode = "0777")
                        }else{ TRUE }
                        
                        #--- Matrices repository
                        vMatrices_dir <- paste0(vDataDir, vSepDir,"matrices", vSepDir)
                        if (!file.exists(vMatrices_dir)){
                          dir.create(vMatrices_dir, mode = "0777")
                        }else{ TRUE }
                        
                        gbl.active   <-  "X"
                        gbl.analysis <- vAnalysis_dir
                        gbl.matrices <- vMatrices_dir
                        
                        save( gbl.active,
                              gbl.analysis,
                              gbl.matrices,
                              file = rData_dir)
                        save.image()
                      })
             
             load( rData_dir, .GlobalEnv )
            
            if(length(arguments[1]) == 0){
              .Object@arguments[1] <- "NoContext"
            }else{
              .Object@arguments[1] <- toupper(arguments)
            }
            
            # -- Load variables for environment "BIOMON"
            if(.Object@arguments[1] == "BIOMON" ){
              
              rMatrices <- "mtx.RData"
              gbl.mAnalysis    <- matrix()
              gbl.mEnvironment <- matrix()
              gbl.mSpecies     <- matrix()
              gbl.mAttributes  <- matrix()
              gbl.mFactor      <- matrix()
              gbl.mSpace       <- matrix()
              save( gbl.mAnalysis,
                    gbl.mEnvironment, 
                    gbl.mSpecies,
                    gbl.mAttributes,
                    gbl.mFactor,
                    gbl.mSpace,
                    file = rMatrices )
              save.image()
              load( rMatrices, .GlobalEnv )
              
            }
            # -- Class inspection
            validObject(.Object)
            return(.Object) 
           }
)         
 #' Method Global.getMatrices
 #' 
 #' Description.
 #' 
 #' @param object        Description.
 #' @return gbl.matrices Description.
 #' @examples
 #' Global.getMatrices()
 #' @export
 #' 
 setGeneric("Global.getMatrices",
            function(object){standardGeneric("Global.getMatrices")})
 setMethod("Global.getMatrices",
           "Global",
           function(object){ return(gbl.matrices) }
 )
 
 #' Method Global.getAnalysis
 #' 
 #' Description.
 #' 
 #' @param object        Description.
 #' @return gbl.analysis Description.
 #' @examples
 #' Global.getAnalysis()
 #' @export
 #' 
 setGeneric("Global.getAnalysis",
            function(object){standardGeneric("Global.getAnalysis")})
 setMethod("Global.getAnalysis",
           "Global",
           function(object){ return(gbl.analysis) }
 )

#' Method Global.getmAnalysis
#' 
#' Description.
#' 
#' @param object        Description.
#' @return gbl.mAnalysis Description.
#' @examples
#' Global.getmAnalysis()
#' @export
#' 
setGeneric("Global.getmAnalysis",
           function(object){standardGeneric("Global.getmAnalysis")})
setMethod("Global.getmAnalysis",
          "Global",
          function(object){ return(gbl.mAnalysis) }
)

#' Method Global.setmAnalysis
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmAnalysis()
#' @export
#' 
setGeneric("Global.setmAnalysis",
           function(object, path ){standardGeneric("Global.setmAnalysis")})
setMethod("Global.setmAnalysis",
          "Global",
          function(object){ 
            gbl.mAnalysis <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                              error   = function(e) message(path, '\n', e),
                              warning = function(w) message(path, '\n', w)
                              )
          }
)

#' Method Global.getmEnvironment
#' 
#' Description.
#' 
#' @param object            Description.
#' @return gbl.mEnvironment Description.
#' @examples
#' Global.getmEnvironment()
#' @export
#' 
setGeneric("Global.getmEnvironment",
           function(object){standardGeneric("Global.getmEnvironment")})
setMethod("Global.getmEnvironment",
          "Global",
          function(object){ return(gbl.mEnvironment) }
)

#' Method Global.setmEnvironment
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmEnvironment()
#' @export
#' 
setGeneric("Global.setmEnvironment",
           function(object, path ){standardGeneric("Global.setmEnvironment")})
setMethod("Global.setmEnvironment",
          "Global",
          function(object){ 
            gbl.mEnvironment <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                                error   = function(e) message(path, '\n', e),
                                warning = function(w) message(path, '\n', w)
                                )
          }
)

#' Method Global.getmSpecies
#' 
#' Description.
#' 
#' @param object        Description.
#' @return gbl.mSpecies Description.
#' @examples
#' Global.getmSpecies()
#' @export
#' 
setGeneric("Global.getmSpecies",
           function(object){standardGeneric("Global.getmSpecies")})
setMethod("Global.getmSpecies",
          "Global",
          function(object){ return(gbl.mSpecies) }
)

#' Method Global.setmSpecies
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmSpecies()
#' @export
#' 
setGeneric("Global.setmSpecies",
           function(object, path){standardGeneric("Global.setmSpecies")})
setMethod("Global.setmSpecies",
          "Global",
          function(object){ 
            gbl.mSpecies <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                            error   = function(e) message(path, '\n', e),
                            warning = function(w) message(path, '\n', w)
                            )
          }
)

#' Method Global.getmAttributes
#' 
#' Description.
#' 
#' @param object           Description.
#' @return gbl.mAttributes Description.
#' @examples
#' Global.getmAttributes()
#' @export
#' 
setGeneric("Global.getmAttributes",
           function(object){standardGeneric("Global.getmAttributes")})
setMethod("Global.getmAttributes",
          "Global",
          function(object){ return(gbl.mAttributes) }
)

#' Method Global.setmAttributes
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmAttributes()
#' @export
#' 
setGeneric("Global.setmAttributes",
           function(object, path){standardGeneric("Global.setmAttributes")})
setMethod("Global.setmAttributes",
          "Global",
          function(object){ 
            gbl.mAttributes <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                                error   = function(e) message(path, '\n', e),
                                warning = function(w) message(path, '\n', w)
                                )
          }
)

#' Method Global.getmFactor
#' 
#' Description.
#' 
#' @param object       Description.
#' @return gbl.mFactor Description.
#' @examples
#' Global.getmFactor()
#' @export
#' 
setGeneric("Global.getmFactor",
           function(object){standardGeneric("Global.getmFactor")})
setMethod("Global.getmFactor",
          "Global",
          function(object){ return(gbl.mFactor) }
)

#' Method Global.setmFactor
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmFactor()
#' @export
#' 
setGeneric("Global.setmFactor",
           function(object, path){standardGeneric("Global.setmFactor")})
setMethod("Global.setmFactor",
          "Global",
          function(object){ 
            gbl.mFactor <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                            error   = function(e) message(path, '\n', e),
                            warning = function(w) message(path, '\n', w)
                            )
          }
)

#' Method Global.getmSpace
#' 
#' Description.
#' 
#' @param object      Description.
#' @return gbl.mSpace Description.
#' @examples
#' Global.getmSpace()
#' @export
#' 
setGeneric("Global.getmSpace",
           function(object){standardGeneric("Global.getmSpace")})
setMethod("Global.getmSpace",
          "Global",
          function(object){ return(gbl.mSpace) }
)

#' Method Global.setmSpace
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @examples
#' Global.setmSpace()
#' @export
#' 
setGeneric("Global.setmSpace",
           function(object, path){standardGeneric("Global.setmSpace")})
setMethod("Global.setmSpace",
          "Global",
          function(object){ 
            gbl.mSpace <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                          error   = function(e) message(path, '\n', e),
                          warning = function(w) message(path, '\n', w)
            )  
          }
)