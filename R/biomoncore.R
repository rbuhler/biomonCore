# ***************************************
# BIOMON core
# ***************************************
# Author: Rodrigo Bühler
# July 2014
# Version 1.51
# ***************************************

# **************************************
# MAIN METHOD
# **************************************
biomon_run <- function(i_analysis) {
  
  # **************************************
  # METHOD - Data load
  # **************************************
  data_load <- function(i_file, i_header){  
    file   <- i_file
    header <- i_header
    data   <- NA
    #---
    path   <- file
      tryCatch({
        data   <- read.csv (file = path, header=header, stringsAsFactors = FALSE, row.names = 1)},
        error = function( e ){
            message('ERROR >> ', e)
            stop},
        warning = function( w ){
            message('WARNING >> ', w)
            stop}) 
    #---
  o_data <- data
  return(o_data)
  }  
  # **************************************
  # METHOD - Coerce
  # **************************************
  coerce <- function(i_type, i_variable){
    type     <- i_type
    variable <- i_variable
    ret   <- c()
    switch(type,
           numeric={
             ret <- as.numeric(variable)
           },
           double={
             ret <- as.double(variable)
           },
           character={
             ret <- as.character(variable)
           },
           logical={
             ret <- as.logical(variable)
           },
           vector={
             ret <- list(c(variable))
           },
           list={
             ret <- list(variable)
           },
           ret <- variable
    )
    #
    o_return <- ret
    return(o_return)
  }
# **************************************
# CONSTANTS
# **************************************
  AT   <- "@"
  NUM  <- "numeric"
  DBL  <- "double"
  CHAR <- "character"
  LOG  <- "logical"
  VEC  <- "vector"
  LIS  <- "list"
  PARM_POINTER <- 3 # fist position of the parameters
  # **************************************
  # PRE LOAD CALLS
  # **************************************
#     pck_chk() # Check for necessary packages
  # **************************************
  # MAIN BODY
  # **************************************
  analysis  <- i_analysis
  techniques <- data_load(analysis, FALSE)
# --  
  obs_size  <- length(techniques[[1]]) # Lines
  var_size  <- length(techniques) # Columns
  result    <- list() # Execution results
# Lines of methods
  for (x in 1: obs_size) { 
    shorttext  <- c()
    method     <- c()
    parameters <- list()
    param_idx  <- 0
# Get the short text - 1st position    
    tryCatch(
      shorttext  <- techniques[x,1],
        
      error = function( err ){ message('ERROR >> ', err) 
                               stop(call. = FALSE)},
      warning = function( w ){ message('WARNING >> ', w)
                               stop(call. = FALSE)}
    ) 
# Get the method     - 2nd position    
    method     <- techniques[x,2]  
# Columns of parameters
    for (i in PARM_POINTER: var_size) { 
# Get the parameters - 3rd and others
      typ = str_trim(techniques[x,i])
      var = str_trim(techniques[x,i+1])
# Parameter is an already calculated value typ='@'
      if(typ == AT && !is.na(typ)){
        index <- coerce(DBL, var) # var is a position not a value
        typ  <- result[[index]][[3]]
        var <- result[[index]][[2]]
      }
# Coerce a value into a determined type      
      if((typ == NUM | typ == CHAR | typ == DBL | typ == LOG) && !is.na(typ)){
        param_idx = param_idx +1
        parameters[param_idx] <- coerce(typ, var)
      }
      if((typ == VEC | typ == LIS) && !is.na(typ)){
        param_idx = param_idx +1
        parameters[param_idx] <- coerce(typ, var)
      }
    }
        do_what   <- method
        do_args   <- parameters
        ret_val <- c()
        ret_typ <- NULL
# Check the need for do.call
      switch(do_what,
             as.vector={
               ret_val  <- unlist(do_args)  
               ret_typ <- VEC
             },
             ret_val  <- do.call(do_what, do_args)
      )
# If type still not assigned      
      if(length(ret_typ) == 0){
          ret_typ <- typeof(ret_val)
      }
# Put the result in a stack
      ret_lst <- list(shorttext, ret_val, ret_typ)
      result[x] <- list(ret_lst)
    }
return(ret_val)
}