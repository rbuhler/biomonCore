# -- Class/method initialization
act <- action()

# -- Test 
method   <- "Action.setSuccess"
expected <- 0
actual   <- Action.setSuccess(act)<-0
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

method   <- "Action.getSuccess"
actual   <- Action.getSuccess(act)
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

method   <- "Action.setMsgType"
expected <- "S"
actual   <- Action.setMsgType(act)<-"S"
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

method   <- "Action.getMsgType"
actual   <- Action.getMsgType(act)
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

method   <- "Action.setMsgText"
expected <- "Message from attribute."
actual   <- Action.setMsgText(act)<-expected
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

method   <- "Action.getMsgText"
actual   <- Action.getMsgText(act)
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

# ---
method   <- "Action.btnExecute"
expected <- mean(c(3,5,6))
actual   <- Action.btnExecute(act, "data/mean.csv")
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)

# ---
method   <- "Action.btnExecuteTest"
# expected <- format(Sys.time(), "%a %b %d %X %Y")
expected <- "data/mean.csv"
actual   <- Action.btnExecuteTest(act, "data/mean.csv")
Assert.setResult(chk) <- Assert.equals(chk, method, actual, expected)
# ---
Assert.summary(chk)