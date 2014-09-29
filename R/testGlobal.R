test <- assert()
emptyChar <- ""
# ---
glob <- global("biomon")

exec_result <- Global.getAnalysis(glob)
test_result <- Assert.differs(test, "Global.getAnalysis", exec_result, emptyChar)
Assert.setResult(test)<-test_result

exec_result <- Global.getMatrixE(glob)
test_result <- Assert.differs(test, "Global.getMatrixE", exec_result, emptyChar)
Assert.setResult(test)<-test_result

# ---
glob2 <- global("biom")
exec_result <- Global.getAnalysis(glob2)
test_result <- Assert.differs(test, "Global.getAnalysis", exec_result, emptyChar)
Assert.setResult(test)<-test_result

exec_result <- Global.getMatrixE(glob2)
test_result <- Assert.equals(test, "Global.getMatrixE", exec_result, emptyChar)
Assert.setResult(test)<-test_result

# ---
Assert.summary(test)