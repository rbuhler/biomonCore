# -- CONSTRUCTOR
analyz <- function(){
  return(new("Analyz"))
}
# -- CLASS
setClass( Class="Analyz",
          representation( steps     = "list",
                          nrLines   = "numeric",
                          nrColumns = "numeric",
                          stepLine  = "list",
                          results   = "data.frame"),
          validity=function(object){ 
            #--- INSPECTOR
           if(TRUE){
             return(TRUE)
           }else{
             return(FALSE)
           }
          }
)
setMethod( f="initialize",
           signature="Analyz",
           definition=function(.Object, 
                               steps, 
                               nrLines, 
                               nrColumns,
                               stepLine,
                               results){ 
            #--- INITIALIZER
            # -- Set the attibutes with the defaults
            .Object@steps     <- list()
            .Object@nrLines   <- 0
            .Object@nrColumns <- 0
            .Object@stepLine  <- list()
            .Object@results   <- data.frame()
            # -- Class inspection
            validObject(.Object)
            return(.Object) }
)
# -- GETTER
setGeneric("Analyz.getStep",
           function(object, index){standardGeneric("Analyz.getStep")})
setMethod("Analyz.getStep",
          "Analyz",
          function(object, index){
          
            stepList <- list()
            for(i in 1:object@nrColumns){
              
              if (length(stepList) == 0){
                stepList <- object@steps[index, i]
              }else{
                stepList <- c(stepList, object@steps[index, i])
              }
            
            }
            return(stepList)
          }
)

setGeneric("Analyz.getResult",
           function(object, index){standardGeneric("Analyz.getResult")})
setMethod("Analyz.getResult",
          "Analyz",
          function(object, index){ return(object@results[index, 1]) }
)

setGeneric("Analyz.getNrColumns",
           function(object){standardGeneric("Analyz.getNrColumns")})
setMethod("Analyz.getNrColumns",
          "Analyz",
          function(object){return(object@nrColumns)}
)

setGeneric("Analyz.getNrLines",
           function(object){standardGeneric("Analyz.getNrLines")})
setMethod("Analyz.getNrLines",
          "Analyz",
          function(object){return(object@nrLines)}
)

setGeneric("Analyz.getStepLine",
           function(object){standardGeneric("Analyz.getStepLine")})
setMethod("Analyz.getStepLine",
          "Analyz",
          function(object){
            line["title"]      <- object@stepLine["title"]
            line["command"]    <- object@stepLine["command"]
            line["parameters"] <- object@stepLine["parameters"]
            return(line)}
)
# -- SETTER
setGeneric("Analyz.setResult<-",
           function(object, value){standardGeneric("Analyz.setResult<-")})
setReplaceMethod( f="Analyz.setResult",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- BODY
                    object@results<-rbind(object@results, value)
                    return(object)                      
                  }
)
setGeneric("Analyz.setStepLine<-",
           function(object, value){standardGeneric("Analyz.setStepLine<-")})
setReplaceMethod( f="Analyz.setStepLine",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- 
                    object@stepLine["title"]      <- value[1]
                    object@stepLine["command"]    <- value[2]
                    # -- From position 3 on the values are paramters
                    for(i in 3:length(value)){
                      # -- If the variable is empty create the first entry
                      if(length(object@stepLine) == 0 ){
                        object@stepLine["parameters"] <- list(c(value[i]))
                      # -- Otherwise add the new entry to the list  
                      }else{
                        object@stepLine["parameters"] <- list(c(object@stepLine["parameters"], value[i]))
                      }
                    }
                    return(object)                      
                  }
)

setGeneric("Analyz.loadSteps<-",
           function(object, value){standardGeneric("Analyz.loadSteps<-")})
setReplaceMethod( f="Analyz.loadSteps",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- Constants   
                    HEADER  <- FALSE
                    FACTORS <- FALSE
                    ROWNMS  <- 1
                    steps <- NULL
                    # -- BODY
                    file  <- value
                    tryCatch({
                      steps <- read.csv (file = file, header=HEADER, stringsAsFactors = FACTORS, row.names = ROWNMS)
                      object@nrLines   <- length(steps[[1]])
                      object@nrColumns <- length(steps)
                      
                      object@steps<-steps
                    },
                    error   = function(e) FALSE,
                    warning = function(w) FALSE
                    )
                    return(object)                      
                  }
)
# -- OTHERS
setGeneric("Analyz.coerceType",
           function(object, variable, type){standardGeneric("Analyz.coerceType")})
setMethod("Analyz.coerceType",
          "Analyz",
          function(object, variable, type){
            l_variable <- variable
            l_type     <- type
            l_return   <- FALSE
            # -- Coerce a variable to a defined type
            switch(type,
                     numeric={
                       tryCatch(l_return <- as.numeric(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     double={
                       tryCatch(l_return <- as.double(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     character={
                       tryCatch(l_return <- as.character(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     logical={
                       tryCatch(l_return <- as.logical(variable),
                           error = function(e) FALSE,
                           warning = function(w) FALSE)
                     },
                     vector={
                       tryCatch(l_return <- list(c(variable)),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     list={
                       tryCatch(l_return <- list(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     l_return <- variable
            )
            return( l_return ) 
          }
)