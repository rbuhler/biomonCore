# -- Test Initilization
test <- assert()
# -- Class/method initialization
analyze <- analyz()
# -- Test the implemented coercion
method   <- "Analyz.coerceType"
expected <- as.numeric("1")
actual   <- Analyz.coerceType(analyze, "1", "numeric")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.double("1.2")
actual   <- Analyz.coerceType(analyze, "1.2", "double")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.character("x")
actual   <- Analyz.coerceType(analyze, "x", "character")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.logical(1)
actual   <- Analyz.coerceType(analyze, 1, "logical")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected   <- "list"
actual_tmp <- Analyz.coerceType(analyze, "a,b,c", "vector")
actual     <- typeof(actual_tmp)
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected   <- "list"
actual_tmp <- Analyz.coerceType(analyze, "1", "list")
actual     <- typeof(actual_tmp)
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

# -- Force coercion error
expected <- FALSE
actual   <- Analyz.coerceType(analyze, "a", "numeric")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

# --- Test the load of an Analysis file
glbl <- global("biomon")
path <- Global.getAnalysis(glbl)

# -- Class/method initialization
analyze <- analyz()
Analyz.loadSteps(analyze) <- paste0(path,"PCA.csv")

method   <- "Analyz.getNrLines"
actual   <- Analyz.getNrLines(analyze)
expected <- 8L
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

method   <- "Analyz.getNrColumns"
actual   <- Analyz.getNrColumns(analyze)
expected <- 14L
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

# -- Test of handling errors
analyze_e <- analyz()
Analyz.loadSteps(analyze_e) <- paste0(path,"PCA.cs")

method   <- "Analyz.getNrLines"
expected <- 0
actual   <- Analyz.getNrLines(analyze_e)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

method   <- "Analyz.getNrColumns"
expected <- 0
actual   <- Analyz.getNrColumns(analyze_e)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

# -- Test the Get steps
actual_tmp <- Analyz.getStep(analyze, 2)
method     <- "Analyz.getStep"
actual     <- actual_tmp[1]
expected   <- "Header"
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

actual     <- actual_tmp[2]
expected   <- "as.logical"
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

actual     <- actual_tmp[3]
expected   <- "logical"
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

actual     <- actual_tmp[4]
expected   <- "TRUE"
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

actual     <- actual_tmp[5]
expected   <- ""
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

# -- Test Set and Get a Result
# -- Class/method initialization
analyze <- analyz()

method <- "Analyz.getResult"

do_what <- "mean"
do_args <- list(10,20)
expected <- do.call(do_what, do_args)
Analyz.setResult(analyze)<- expected
actual <- Analyz.getResult(analyze, 1)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

do_what <- "median"
do_args <- list(c(10,20,50))
expected <- do.call(do_what, do_args)
Analyz.setResult(analyze)<- expected
actual <- Analyz.getResult(analyze, 2)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

expected <- 20
actual <- Analyz.getResult(analyze, 2)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

# -- Test Set and Get a Step
line <- list("MEAN CALCULATION", "mean", 1,2,3)
Analyz.setStepLine(analyze)<-line
actual_tmp <- Analyz.getStepLine(analyze)

method   <- "Analyz.getStepLine"
expected <- "MEAN CALCULATION"
actual   <- as.character(actual_tmp["title"])
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

expected <- "mean"
actual   <- as.character(actual_tmp["command"])
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

# ---
Assert.summary(test)