# -- CONSTRUCTOR
global <- function(arg){
  return(new(Class = "Global", context = arg))
}
# -- CLASS
setClass( Class="Global",
          representation( context   = "character",
                          analysis  = "character",
                          matrixE   = "character"),
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
           definition=function(.Object, 
                                context, 
                                analysis, 
                                matrixE){ 
             #--- INITIALIZER
             # -- Load the defaults in case it is not already loaded
            if(!exists("gbl.active", mode="character")){
#               load("../data/gbl.RData")
              load("data/gbl.RData")
            }else{

            }
            # -- Set the attibutes with the defaults
            .Object@analysis <- gbl.analysis

            if(length(context) == 0){
             .Object@context <- "NoContext"
            }else{
              .Object@context <- toupper(context)
            }

            # -- Load variables for environment "BIOMON"
            if(.Object@context == "BIOMON" ){
              .Object@matrixE  <- gbl.matrixE  
            }else{
              .Object@matrixE <- " "
            }
            # -- Class inspection
            validObject(.Object)
            return(.Object) 
           }
)
# -- GETTER
setGeneric("Global.getAnalysis",
           function(object){standardGeneric("Global.getAnalysis")})
setMethod("Global.getAnalysis",
          "Global",
          function(object){return(object@analysis)}
          )
setGeneric("Global.getMatrixE",
           function(object){standardGeneric("Global.getMatrixE")})
setMethod("Global.getMatrixE",
          "Global",
          function(object){return(object@matrixE)}
          )