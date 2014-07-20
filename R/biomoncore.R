# ***************************************
# BIOMON core
# ***************************************
# Author: Rodrigo Bühler
# July 2014
# Version 1.0
# ***************************************

# FUNCTIONS
# **************************************
# Testing results
# **************************************

# <- resum from here


# **************************************
# Coertion of variable types
# **************************************
coerce <- function(i_type, i_variable){
 result <- c()

 switch(i_type,
        numeric={
          result <- as.numeric(i_variable)
        },
        double={
          result <- as.double(i_variable)
        },
        character={
          result <- as.character(i_variable)
        },
        result <- i_variable
 )
  return(result)
}
# **************************************
# Execution of methods
# **************************************
execute <- function( i_path ) {
  # Variables
  path <- i_path
  analysis  <- read.csv (file = path, header=FALSE, stringsAsFactors = FALSE)
# --  
  obs_size  <- length(analysis[[1]]) # Lines
  var_size  <- length(analysis) # Columns
  result    <- list() # Execution results
  #Constants  
  AT <- "@"
  NUM <- "numeric"
  DBL <- "double"
  CHAR <- "character"
  PARM_POINTER <- 3 # fist position of the parameters

# Lines of methods
  for (x in 1: obs_size) { 
    shorttext  <- c()
    technique  <- c()
    parameters <- c()
    # Get the short text - 1st position    
    shorttext <- analysis[x,1]
    # Get the technique  - 2nd position    
    technique <- analysis[x,2]  
# Columns of parameters
    for (i in PARM_POINTER: var_size) { 
      # Get the parameters - 3rd and others
      ty  = analysis[x,i]
      var = analysis[x,i+1]
      # Parameter is an already calculated value ty='@'
      if(ty == AT && !is.na(ty)){
        index <- coerce(DBL, var) # var is a position not a value
        ty  <- result[[index]][[3]]
        var <- result[[index]][[2]]
      }
# Coerce a value into a determined type      
      if((ty == NUM | ty == CHAR | ty == DBL) && !is.na(ty)){
        parameters <- c(parameters, coerce(ty, var))
      }
    }
    what   <- technique;
    args   <- list(parameters)
    
    ret_val  <- do.call(what, c(args))
    ret_typ <- typeof(ret_val)
    ret_lst <- list(shorttext, ret_val, ret_typ)
    result[x] <- list(ret_lst)
    print(c(x, shorttext, "<-",ret_lst[[2]]), quote=FALSE)
    }
print("end.")
}

# ***************************************
# MAIN
# source("./R/biomoncore.R")
# ***************************************
path <- ("/Users/rodrigobuhler/Dev/R/biomon_core/Data/Methods_Def.csv")
execute(path)