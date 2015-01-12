# -- CONSTRUCTOR
action <- function(){
  return(new("Action"))
}
#' Action
#'
#' @title Class to handle requests form the UI of Biomon.
#' 
#' @slot success A numeric attribute meaning the status of the execution, where 0 is success any other number a message ID.
#' @slot msgType A character meaning 'S' success, E' error, 'W' warning or 'I' information.
#' @slot msgText A character string with a message.
#' @slot anlz An instance of the class Analyz.
#' 
setClass( Class="Action",
          representation( success     = 'numeric',
                          msgType     = 'character',
                          msgText     = 'character',
                          anlz        = 'Analyz' ),
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
           signature="Action",
           definition=function(.Object, 
                               success, 
                               msgType, 
                               msgText,
                               anlz){ 
             #--- INITIALIZER
             # -- Set the attibutes with the defaults
             .Object@success <- 1
             .Object@msgType <- 'E'
             .Object@msgText <- 'Not Defined.'
             .Object@anlz    <- new("Analyz")

             # -- Class inspection
             validObject(.Object)
             return(.Object) 
           }
)
#--- SETTER
#' Action.setSuccess 
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   The infomed value states the result of an execution, where 0 is success and any other number means that an issue happens.
#' @return object Description.
#' @export
#' @docType methods
#' @rdname Action.setSuccess-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_success <- numeric()
#' Action.setSuccess(obj) <- v_success
#' @export
setGeneric("Action.setSuccess<-",
           function(object, value){standardGeneric("Action.setSuccess<-")})
#' @rdname Action.setSuccess-methods
#' @aliases Action.setSuccess,Action,Action-method
setReplaceMethod( f="Action.setSuccess",
                  signature="Action",
                  definition=function(object, value){
                    # -- BODY
                    object@success <- value
                    return(object)                       
                  }
)
#' Action.setMsgType 
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   The infomed type defines the type of the execution message, where 'S' is success, 'E' error, 'W' warning and 'I' information.
#' @return object Description.
#' @export
#' @docType methods
#' @rdname Action.setMsgType-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_msgType <- character()
#' Action.setMsgType(obj) <- v_msgType
#' @export
setGeneric("Action.setMsgType<-",
           function(object, value){standardGeneric("Action.setMsgType<-")})
#' @rdname Action.setMsgType-methods
#' @aliases Action.setMsgType,Action,Action-method
setReplaceMethod( f="Action.setMsgType",
                  signature="Action",
                  definition=function(object, value){
                    # -- BODY
                    object@msgType <- value
                    return(object)                       
                  }
)
#' Action.setMsgText
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   The informed string describes the result of an execution.
#' @return object Description.
#' @export
#' @docType methods
#' @rdname Action.setMsgText-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_msg <- character()
#' Action.setMsgText(obj) <- v_msg
#' @export
setGeneric("Action.setMsgText<-",
           function(object, value){standardGeneric("Action.setMsgText<-")})
#' @rdname Action.setMsgText-methods
#' @aliases Action.setMsgText,Action,Action-method
setReplaceMethod( f="Action.setMsgText",
                  signature="Action",
                  definition=function(object, value){
                    # -- BODY
                    object@msgText <- value
                    return(object)                       
                  }
)
#--- GETTER
#' Action.getSuccess 
#' 
#' Description.
#' 
#' @param object    Description.
#' @return success The returned value states the result of an execution, where 0 is success and any other number means that an issue happens.
#' @export
#' @docType methods
#' @rdname Action.getSuccess-methods
#' @examples
#' obj <- new("Action")
#' Action.getSuccess(obj)
#' @export
setGeneric("Action.getSuccess",
           function(object){standardGeneric("Action.getSuccess")})
#' @rdname Action.getSuccess-methods
#' @aliases Action.getSuccess,Action,Action-method
setMethod("Action.getSuccess",
          "Action",
          function(object){ return(object@success) }
)
#' Action.getMsgType 
#' 
#' Description.
#' 
#' @param object    Description.
#' @return msgType The returned type defines the type of the execution message, where 'S' is success, 'E' error, 'W' warning and 'I' information.
#' @export
#' @docType methods
#' @rdname Action.getMsgType-methods
#' 
#' @examples
#' obj <- new("Action")
#' Action.getMsgType(obj)
#' @export
setGeneric("Action.getMsgType",
           function(object){standardGeneric("Action.getMsgType")})
#' @rdname Action.getMsgType-methods
#' @aliases Action.getMsgType,Action,Action-method
setMethod("Action.getMsgType",
          "Action",
          function(object){ return(object@msgType) }
)
#' Action.getMsgText 
#' 
#' Description.
#' 
#' @param object    Description.
#' @return msgText The returned string describes the result of an execution.
#' @export
#' @docType methods
#' @rdname Action.getMsgText-methods
#' 
#' @examples
#' obj <- new("Action")
#' Action.getMsgText(obj)
#' @export
setGeneric("Action.getMsgText",
           function(object){standardGeneric("Action.getMsgText")})
#' @rdname Action.getMsgText-methods
#' @aliases Action.getMsgText,Action,Action-method
setMethod("Action.getMsgText",
          "Action",
          function(object){ return(object@msgText) }
)

#' Action.btnExecute
#' 
#' Description.
#' 
#' @param object Description.
#' @param pathList A list with the paths of the CSV files.
#' @return result The return states the number of results reached or zero when something went wrong.
#' @export
#' @docType methods
#' @rdname Action.btnExecute-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_pthList <- list()
#' Action.btnExecute(obj, v_pthList)
#' @export
setGeneric("Action.btnExecute",
          function(object, pathList){standardGeneric("Action.btnExecute")})
#' @rdname Action.btnExecute-methods
#' @aliases Action.btnExecute,Action,Action-method
setMethod("Action.btnExecute",
          "Action",
          function(object, pathList){ 
            result   <- 0
            cols     <- 0
            lins     <- 0
            exec     <- list()
            result   <- data.frame
#--- Load the analysis CSV file
            anlzFile = pathList['analysis']
            Analyz.loadSteps(object@anlz) <- anlzFile
              cols <- Analyz.getNrColumns(object@anlz)
              lins <- Analyz.getNrRows(object@anlz)           

              #--- Success test
              if(cols > 0 & lins > 0){
                #--- Read each line of the analysis
                #--- Preapare the line in items
                #--- Read the items
                for(x in 1 : lins){
                  Analyz.setStepItems(object@anlz) <- Analyz.getStep(object@anlz, x)
                  exec <- Analyz.getStepItems(object@anlz)
                  
                  #--- Execute the command
                  temp <- unlist(exec["parameters"])
                  temp2 <- as.character(unlist(exec[["command"]]))
                  
                  Analyz.setResult(object@anlz) <- Analyz.runAnalysis(object@anlz, temp2, list(temp))
                }
                result <- Analyz.getResult(object@anlz, lins)
#                 result <- lins
              }else{
                result <- 0
              }
            #--- Return
            return(result)
          }
)

#' Action.btnExecuteEcho
#' 
#' Description.
#' 
#' @param object Description.
#' @param string A string to be "echoed".
#' @return result The return is the string sent in the parameter string.
#' @export
#' @docType methods
#' @rdname Action.btnExecuteEcho-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_anySTR <- character()
#' Action.btnExecuteEcho(obj, v_anySTR)
#' @export
setGeneric("Action.btnExecuteEcho",
           function(object, string){standardGeneric("Action.btnExecuteEcho")})
#' @rdname Action.btnExecuteEcho-methods
#' @aliases Action.btnExecuteEcho,Action,Action-method
setMethod("Action.btnExecuteEcho",
          "Action",
          function(object, string){ 
            param <- string
            
            result   <- param
            return(result)
          }
)