# -- CONSTRUCTOR
technique <- function(){
  return(new("Technique"))
}
# -- CLASS
setClass( Class="Technique",
          representation( technique = "character",
                          parameters = "list" ),
          contains = "Analyz",
                     "Constants",
          # -- INSPECTOR 
          validity = function(object){ 
            if(TRUE){
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
)
# -- INITIALIZER
setMethod( f="initialize",
           signature="Technique",
           definition=function(.Object, 
                               technique, 
                               parameters){ 
             # -- Load the defaults in case it is not already loaded
             if(TRUE){
              return(TRUE)
             }else{
              return(FALSE)
             }
             # -- INITIALIZER
             # -- Set the attibutes with the defaults
             .Object@technique <- NULL
             .Object@parameters <- list()
             # -- Class inspection
             validObject(.Object)
             return(.Object) }
)
# -- GETTER
setGeneric("Technique.getTechnique",
           function(object){standardGeneric("Technique.getTechnique")})
setMethod("Technique.getTechnique",
          "Technique",
          function(object){return(object@technique)}
)
setGeneric("Technique.getParameters",
           function(object){standardGeneric("Technique.getParameters")})
setMethod("Technique.getParameters",
          "Technique",
          function(object){return(object@parameters)}
)
# -- SETTER
setGeneric("Technique.setTechnique<-",
           function(object, line){standardGeneric("Technique.setTechnique<-")})
setReplaceMethod(f="Technique.setTechnique",
                 signature="Technique",
                 definition=function(object, line){
                    # -- Get a specific line of the Steps matrix
                    object@technique <- steps[line, object@POStechnique]
                    return(object)
                 }
)
setGeneric("Technique.setParameters<-",
          function(object, line){standardGeneric("Technique.setParameters<-")})
setReplaceMethod(f="Technique.setParameters",
                signature="Technique",
                definition=function(object, line){
                  # -- Get a specific line of the Steps matrix
                  for (i in object@POSparameters: object@noColumns){
# -- I stopped here !
                    object@parameter[] <- steps[line, i] 
                  }
                  return(object)
                }                 
)