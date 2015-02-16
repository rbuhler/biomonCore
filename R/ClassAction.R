# -- CONSTRUCTOR
action <- function(){
  return(new("Action"))
}
#' Action
#'
#' @title Class to handle requests form the UI of Biomon.
#' 
#' @slot anlz An instance of the class Analyz.
#' 
#' @import methods
#' @import analyz
#' 
setClass( Class="Action",
          representation( anlz = 'Analyz' ),
          validity=function(object)
  { 
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
                               anlz)
  { 
   #--- INITIALIZER
   #--- Load the necessary libraries
   # Install and load required a package
   install_load <- function (package)
   {   
     # if package is installed locally, load
     if(package %in% rownames(installed.packages()))
       do.call('library', list(package))
     # if package is not installed locally, download, then load
     else {
       install.packages(package)
       do.call('library', list(package))
     }
   } 
   install_load('analyz')
   
   .Object@anlz    <- new("Analyz")
   
   # -- Class inspection
   validObject(.Object)
   return(.Object) 
 }
)
#' Action.btnExecute
#' 
#' Description.
#' 
#' @param object Object instance.
#' @param analysisFl Path and analysis file name.
#' @return result Result of the execution of the last step described on the analysis file.
#' @export
#' @docType methods
#' @rdname Action.btnExecute-methods
#' 
#' @examples
#' obj <- new("Action")
#' v_analysisFl <- c()
#' Action.btnExecute(obj, v_analysisFl)
#' @export
setGeneric("Action.btnExecute",
          function(object, analysisFl){standardGeneric("Action.btnExecute")})
#' @rdname Action.btnExecute-methods
#' @aliases Action.btnExecute,Action,Action-method
setMethod("Action.btnExecute",
          "Action",
          function(object, analysisFl)
  { 
      vCols     <- 0
      vRows     <- 0
      vExec     <- list()
      vResult   <- c()
# STEP 1 lodad the analysis steps
        object@anlz <- Analyz.loadSteps(object@anlz, analysisFl)
# STEP 2 get the number of read coluns
        vCols <- Analyz.getNrColumns(object@anlz)
# STEP 3 get the numbert of read rows
        vRows <- Analyz.getNrRows(object@anlz)           
# So far so good?
        if(vCols > 0 && vRows > 0){
          for(x in 1 : vRows){
# STEP 4 load the steps one-by-one
            object@anlz <- Analyz.setStepItems(object@anlz, x)
# STEP 5 get the current command
            vCommand <- Analyz.getStepCommand(object@anlz)
# STEP 6 get the current parameters
            vParms <- Analyz.getStepParameters(object@anlz)
# STEP 7 run the command with the parameters > do.call(command(paramters))
            vResult <- Analyz.runAnalysis(object@anlz, vCommand, (vParms))
# STEP 8 store the result
            Analyz.setResult(object@anlz) <- vResult
          }
        }else{
          vResult <- NULL
        }
# Return the result of the last execution
      return(print(vResult))
    }
)

#' Action.btnExecuteEcho
#' 
#' Method for conneciton testing purposes.
#' 
#' @param object Object instance.
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
          function(object, string){ return(print(string)) }
)