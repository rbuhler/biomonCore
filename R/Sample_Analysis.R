sampleAnalysis<-function(path=Sys.getenv("HOME")){

  d <- data.frame()
  
  l1 <- list("v_A"    ,"as.numeric","numeric",7,NA,NA)
  d <- rbind(l1)
  l2 <- list("v_B"    ,"as.numeric","numeric",3,NA,NA)
  d <- rbind(d,l2)
  l3 <- list("v_Vect" ,"c"         ,"@"      ,1,"@",2)
  d <- rbind(d,l3)
  l4 <- list("Mean"   ,"mean"      ,"@"      ,3,NA,NA)
  d <- rbind(d,l4)
  
  vFile <- paste0(path, "Sample_Mean.csv")
  write.table(d, vFile, sep=",", row.names=FALSE, col.names=FALSE)  

}