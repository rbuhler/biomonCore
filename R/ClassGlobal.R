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
#' Method for returning the matrices files folder path.
#' 
#' @param object Object instance.
#' @return gbl.matrices Path to the matrices files folder.
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
#' Method for returning the analysis files folder path.
#' 
#' @param object Object instance.
#' @return gbl.analysis Path to the analysis files folder.
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