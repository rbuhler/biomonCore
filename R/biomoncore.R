# ***************************************
# BIOMON core
# ***************************************
# Author: Rodrigo Bühler
# July 2014
# Version 1.0
# ***************************************

# FUNCTIONS
# **************************************
# Coertion of variable types
# **************************************
coerce <- function(i_type, i_variable){
 result <- c()
  
 if(i_type == "I"){
   result <- as.numeric(i_variable)
 }
 if(i_type == "D"){
   result <- as.double(i_variable)
 }
 if(i_type == "C"){
   result <- as.character(i_variable)
 }
# as.character(), as.double(), as.integer(), or as.logical()
  return(result)
}
# **************************************
# Execution of methods
# **************************************
execute <- function( i_path ) {
  path <- i_path
  analysis <- read.csv (file = path, header=FALSE, stringsAsFactors = FALSE)

# Get the number of observations - lines
# Get the number of variables - columns
  obs_size <- length(analysis[[1]])
  var_size <- length(analysis)
  
# Get the short text - 1st position
# Get the technique  - 2nd position
# Get the parameters - 3rd and others
  for (x in 1: obs_size) {
    shorttext  <- c()
    technique  <- c()
    parameters <- c()
    
    shorttext <- analysis[x,1]
    technique <- analysis[x,2]
    for (i in 3: var_size) {
# Coercion      
      ty = analysis[x,i]
      var = analysis[x,i+1]
# Integer # Character # Double
      if((ty == 'I' | ty == 'C' | ty == 'D') && !is.na(ty)){
        parameters <- c(parameters, coerce(ty, var))
      }
    }
    what   <- technique;
    args   <- list(parameters)
    result <- do.call(what, c(args))
    print(c(shorttext,": ",result))
    }
}

# ***************************************
# MAIN
# source("./R/biomoncore.R")
# ***************************************
path <- "/Users/rodrigobuhler/Dev/R/biomon_core/Data/Methods_Def.csv"
execute(path)