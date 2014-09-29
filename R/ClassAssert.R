# -- CONSTRUCTOR
assert <- function(){
  return(new("Assert"))
# -- CLASS
}
setClass( Class = "Assert",
          representation( result = "data.frame" ),
          validity = function(object){ 
             if(TRUE){
              return(TRUE)
             }else{
              return(FALSE)
             }
          }
)
# -- INITIALIZER
setMethod( f = "initialize",
           signature = "Assert",
           definition = function(.Object, 
                                result){ 
             # -- Set the attibutes with the defaults
             .Object@result <- data.frame()
             # -- Class inspection
             validObject(.Object)
             return(.Object) }
)
# -- SETTER
setGeneric("Assert.setResult<-",
          function(object, value){standardGeneric("Assert.setResult<-")})
setReplaceMethod(f = "Assert.setResult",
                signature = "Assert",
                definition = function(object, value){
                # -- Add a new line to the results
                  object@result<-rbind(object@result, value)
                  return(object) }
)
# -- OTHERS
setGeneric("Assert.summary",
           function(object){standardGeneric("Assert.summary")})
setMethod("Assert.summary",
          "Assert",
          function( object ){
            result <- object@result
            
            # -- Check the methods
            meth <- table(result$method)
            exec_methods <- length(meth)
            # -- Check the status
            status <- table(result$status)
            # -- Count the Success and Failures
            exec_success  <- status["S"]
            exec_fail     <- status["F"]
            # -- Avoid NA values
            if( is.na(exec_success) ) {
              exec_success <- 0
            }else{}
            # --
            if( is.na(exec_fail) ){
              exec_fail <- 0 
            }else{}

            exec_total    <- nrow(result)
            # -- Print summary
            cat("<<< UNIT TESTING SUMMARY >>>", "\n")
            cat("You tested ", exec_methods,"different methods. \n")
            cat("The final result of the execution was:", "\n")
            cat("Succeed [", exec_success," | ", exec_fail, "] Failed", "\n")
            cat("Total   [", exec_total  ,"]", "\n")
            
            cat("<<< UNIT TESTING REPORT >>>", "\n")
            if(exec_fail <= 0){
              message("\\o/ Congrats, all tests successfully executed.")
              return(TRUE)
            }else{
              # -- Failure report
              cat("Status Method"  , "\n")
              for( r in 1: nrow(result)){
                cat(result[r,2], "....",result[r,1], "\n")
                # -- Details only for status F (Fail)
                if(result[r,2] == "F"){
                  cat("...... - ",  result[r,5] , "\n")
                  cat("...... - ", "Expected [", result[r,3], " | ", result[r,4], "] Actual", "\n")  
                }else{
                  cat("...... + ", "\n")
                }
              }
            }
            cat("<<< END OF UNIT TESTING >>>", "\n")
          }
) 
setGeneric("Assert.equals",
           function(object, meth, act, exp){standardGeneric("Assert.equals")})
setMethod("Assert.equals",
          "Assert",
          function( object,
                    meth,
                    act,
                    exp ){
                      result  <- data.frame()
                      success <- TRUE
                      msg     <- ""
                      stat    <- ""
                      supported <- TRUE
                      
                      typ_act <- typeof(act)
                      typ_exp <- typeof(exp)
                                            
                      # -- Threatment for NA values
                      if(is.na(act)){act = ""}else{TRUE}
                      if(is.na(exp)){exp = ""}else{TRUE}

                      # -- Threatment for unsupported comparisons
                      # -- lists are not supported
                      if(typ_act == "list"){ 
                        supported <- FALSE 
                        success   <- FALSE
                        msg       <- "Check not supported."
                        stat      <- "F"
                        act <- exp <- ""
                      }else{supported <- TRUE}
                      
#                       # -- Threatment for characters whith spaces
#                       act <- str_trim(act, side = "both")
#                       exp <- str_trim(exp, side = "both") 
                      
                      # -- Execute the comparison
                      # -- Check the types
                      if(supported == TRUE){
                        if(typ_act == typ_exp){
                          # -- Check the sizes
                          if(length(act) == length(exp)){
                            # --- Check content
                            if(act == exp){
                              success <- TRUE # -- Success
                              stat <- "S"
                            }else{
                              success <- FALSE # -- Content
                              msg     <- "Not the same value/content."
                              stat    <- "F"
                            } # -- Content
                          }else{
                            success <- FALSE # -- Type
                            msg     <- "Not the same size."
                            stat    <- "F"
                          } # -- Size
                        }else{
                          success <- FALSE # -- Type
                          msg     <- "Not the same type."
                          stat    <- "F"
                        } # -- Type
                      }else{ }
                      # -- Store the current result
                      result <- data.frame(method  = meth,
                                          status   = stat,
                                          expected = exp,
                                          actual   = act,
                                          message  = msg,
                                          stringsAsFactors = FALSE)
                      return(result)
          }
)

setGeneric("Assert.differs",
           function(object, meth, act, exp){standardGeneric("Assert.differs")})
setMethod("Assert.differs",
          "Assert",
          function( object,
                    meth,
                    act,
                    exp ){
            result  <- data.frame()
            success <- FALSE
            msg     <- ""
            stat    <- ""
            supported <- TRUE
          
            typ_act <- typeof(act)
            typ_exp <- typeof(exp)
            
            # -- Threatment for unsupported comparisons
            # -- lists are not supported
            if(typ_act == "list"){ 
              supported <- FALSE 
              success   <- FALSE
              msg       <- "Check not supported."
              stat      <- "F"
              act <- exp <- ""
            }else{supported <- TRUE}            
            
           # -- Threatment for characters whith spaces            
#          act <- str_trim(act, side = "both") 
#          exp <- str_trim(exp, side = "both")             
            
            # -- Execute the comparison            
            # -- Check the types
            if(supported == TRUE){
              if(typ_act == typ_exp){
                # --- Check content
                if(act != exp){
                  success <- TRUE # -- Success
                  stat <- "S"
                }else{
                  success <- FALSE # -- Content
                  msg     <- "Same value/content."
                  stat <- "F"                
                }
              }else{
                success <- FALSE # -- Type
                msg     <- "Not the same type."
                stat <- "F"                
              }
            } else {}
            # -- Store the current result
            result <- data.frame(method   = meth,
                                 status   = stat,
                                 expected = exp,
                                 actual   = act,
                                 message  = msg,
                                 stringsAsFactors = FALSE)
            return(result)
          }
)