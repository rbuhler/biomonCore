# -- CLASS
setClass( Class="Constants",
          representation( POINTERtechnique = "numeric",
                          POINTERparamters = "numeric" )
)
# -- INITIALIZER
setMethod( f="initialize",
           signature="Constants",
           definition=function(.Object, 
                               POINTERtechnique, 
                               POINTERparamters){ 
             # -- INITIALIZER
             # -- Set the attibutes with the defaults
             .Object@POINTERtechnique <- 2
             .Object@POINTERparmaters <- 3
             return(.Object) 
           }
)