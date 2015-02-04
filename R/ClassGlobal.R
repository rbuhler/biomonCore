# -- CONSTRUCTOR
global <- function(arg){
  return(new(Class = "Global", arguments = as.list(arg)))
}
#' Class Global
#' 
#' @title Class to set the Global Environment for the application Biomon.
#' 
#' @param arg A list of arguments to be used by the class initializer.
#' @slot arguments A list of arguments informed in time of creating the object.
#' 
#' @import methods
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
              vDataDir <- paste0(Sys.getenv("HOME"), vSepDir, "Biomon")
             if (!file.exists(vDataDir)){
               dir.create(vDataDir, mode = "0777")
             }else{ TRUE }
             
             #--- Check whether gbl.RData exists or not
             rData_dir    <- paste0(vDataDir, vSepDir, "gbl.RData")
             tryCatch({load( rData_dir, .GlobalEnv )},
                      warning = function(w){
                        #--- Analysis repository
                        vAnalysis_dir <- paste0(vDataDir, vSepDir,"analysis", vSepDir)
                        if (!file.exists(vAnalysis_dir)){
                          dir.create(vAnalysis_dir, mode = "0777")
                          sampleAnalysis(vAnalysis_dir)
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
               
               rMatrices <- paste0(vDataDir, vSepDir, "mtx.RData")
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
#' @export
#' @docType methods
#' @rdname Global.getMatrices-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_MatricesPth <- Global.getMatrices(obj)
#' @export
#' 
setGeneric("Global.getMatrices",
           function(object){standardGeneric("Global.getMatrices")})
#' @rdname Global.getMatrices-methods
#' @aliases Global.getMatrices,Global,Global-method
setMethod("Global.getMatrices",
          "Global",
          function(object){ return(get('gbl.matrices', envir=.GlobalEnv)) }
)

#' Method Global.getAnalysis
#' 
#' Description.
#' 
#' @param object        Description.
#' @return gbl.analysis Description.
#' @export
#' @docType methods
#' @rdname Global.getAnalysis-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_AnalysisPth <- Global.getAnalysis(obj)
#' @export
#' 
setGeneric("Global.getAnalysis",
           function(object){standardGeneric("Global.getAnalysis")})
#' @rdname Global.getAnalysis-methods
#' @aliases Global.getAnalysis,Global,Global-method
setMethod("Global.getAnalysis",
          "Global",
          function(object){ return(get('gbl.analysis', envir=.GlobalEnv)) }
)

#' Method Global.getmAnalysis
#' 
#' Description.
#' 
#' @param object        Description.
#' @return gbl.mAnalysis Description.
#' @export
#' @docType methods
#' @rdname Global.getmAnalysis-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_AnalysisMtx <- Global.getmAnalysis(obj)
#' @export
#' 
setGeneric("Global.getmAnalysis",
           function(object){standardGeneric("Global.getmAnalysis")})
#' @rdname Global.getmAnalysis-methods
#' @aliases Global.getmAnalysis,Global,Global-method
setMethod("Global.getmAnalysis",
          "Global",
          function(object){ return(get('gbl.mAnalysis', envir=.GlobalEnv)) }
)

#' Method Global.setmAnalysis
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmAnalysis-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_AnalysisPth <- character()
#' Global.setmAnalysis(obj, v_AnalysisPth)
#' @export
#' 
setGeneric("Global.setmAnalysis",
           function(object, path ){standardGeneric("Global.setmAnalysis")})
#' @rdname Global.setmAnalysis-methods
#' @aliases Global.setmAnalysis,Global,Global-method
setMethod("Global.setmAnalysis",
          "Global",
          function(object, path){ 
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
#' @export
#' @docType methods
#' @rdname Global.getmEnvironment-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_EnvMatrix <- Global.getmEnvironment(obj)
#' @export
#' 
setGeneric("Global.getmEnvironment",
           function(object){standardGeneric("Global.getmEnvironment")})
#' @rdname Global.getmEnvironment-methods
#' @aliases Global.getmEnvironment,Global,Global-method
setMethod("Global.getmEnvironment",
          "Global",
          function(object){ return(get('gbl.mEnvironment', envir=.GlobalEnv)) }
)

#' Method Global.setmEnvironment
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmEnvironment-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_EnvPth <- character()
#' Global.setmEnvironment(obj,v_EnvPth)
#' @export
#' 
setGeneric("Global.setmEnvironment",
           function(object, path ){standardGeneric("Global.setmEnvironment")})
#' @rdname Global.setmEnvironment-methods
#' @aliases Global.setmEnvironment,Global,Global-method
setMethod("Global.setmEnvironment",
          "Global",
          function(object, path){ 
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
#' @export
#' @docType methods
#' @rdname Global.getmSpecies-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_SpsMatrix <- Global.getmSpecies(obj)
#' @export
#' 
setGeneric("Global.getmSpecies",
           function(object){standardGeneric("Global.getmSpecies")})
#' @rdname Global.getmSpecies-methods
#' @aliases Global.getmSpecies,Global,Global-method
setMethod("Global.getmSpecies",
          "Global",
          function(object){ return(get('gbl.mSpecies', envir=.GlobalEnv)) }
)

#' Method Global.setmSpecies
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmSpecies-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_SpeciesPth <- character()
#' Global.setmSpecies(obj, v_SpeciesPth)
#' @export
#' 
setGeneric("Global.setmSpecies",
           function(object, path){standardGeneric("Global.setmSpecies")})
#' @rdname Global.setmSpecies-methods
#' @aliases Global.setmSpecies,Global,Global-method
setMethod("Global.setmSpecies",
          "Global",
          function(object, path){ 
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
#' @export
#' @docType methods
#' @rdname Global.getmAttributes-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_AttMatrix <- Global.getmAttributes(obj)
#' @export
#' 
setGeneric("Global.getmAttributes",
           function(object){standardGeneric("Global.getmAttributes")})
#' @rdname Global.getmAttributes-methods
#' @aliases Global.getmAttributes,Global,Global-method
setMethod("Global.getmAttributes",
          "Global",
          function(object){ return(get('gbl.mAttributes', envir=.GlobalEnv)) }
)

#' Method Global.setmAttributes
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmAttributes-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_AttributesPth <- character()
#' Global.setmAttributes(obj, v_AttributesPth)
#' @export
#' 
setGeneric("Global.setmAttributes",
           function(object, path){standardGeneric("Global.setmAttributes")})
#' @rdname Global.setmAttributes-methods
#' @aliases Global.setmAttributes,Global,Global-method
setMethod("Global.setmAttributes",
          "Global",
          function(object, path){ 
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
#' @export
#' @docType methods
#' @rdname Global.getmFactor-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_FctMatrix <- Global.getmFactor(obj)
#' @export
#' 
setGeneric("Global.getmFactor",
           function(object){standardGeneric("Global.getmFactor")})
#' @rdname Global.getmFactor-methods
#' @aliases Global.getmFactor,Global,Global-method
setMethod("Global.getmFactor",
          "Global",
          function(object){ return(get('gbl.mFactor', envir=.GlobalEnv)) }
)

#' Method Global.setmFactor
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmFactor-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_FactorPth <- character()
#' Global.setmFactor(obj, v_FactorPth)
#' @export
#' 
setGeneric("Global.setmFactor",
           function(object, path){standardGeneric("Global.setmFactor")})
#' @rdname Global.setmFactor-methods
#' @aliases Global.setmFactor,Global,Global-method
setMethod("Global.setmFactor",
          "Global",
          function(object, path){
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
#' @export
#' @docType methods
#' @rdname Global.getmSpace-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_SpcMatrix <- Global.getmSpace(obj)
#' @export
#' 
setGeneric("Global.getmSpace",
           function(object){standardGeneric("Global.getmSpace")})
#' @rdname Global.getmSpace-methods
#' @aliases Global.getmSpace,Global,Global-method
setMethod("Global.getmSpace",
          "Global",
          function(object){ return(get('gbl.mSpace', envir=.GlobalEnv)) }
)

#' Method Global.setmSpace
#' 
#' Description.
#' 
#' @param object Description.
#' @param path   Description.
#' @export
#' @docType methods
#' @rdname Global.setmSpace-methods
#' 
#' @examples
#' obj <- new("Global", "Biomon")
#' v_SpacePth <- character()
#' Global.setmSpace(obj, v_SpacePth)
#' @export
#' 
setGeneric("Global.setmSpace",
           function(object, path){standardGeneric("Global.setmSpace")})
#' @rdname Global.setmSpace-methods
#' @aliases Global.setmSpace,Global,Global-method
setMethod("Global.setmSpace",
          "Global",
          function(object, path){ 
            gbl.mSpace <- tryCatch(read.csv(path, header = TRUE, row.names = 1),
                                   error   = function(e) message(path, '\n', e),
                                   warning = function(w) message(path, '\n', w)
            )  
          }
)