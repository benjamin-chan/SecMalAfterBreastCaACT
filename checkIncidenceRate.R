D4 <- D2[malType == "AML or MDS", .(id, authorYear, arm, regimen, malType, malN, nITT, medianFU, py, rate, scale)]
setwd("H:/Projects/SecMalAfterBreastCaACT")
f <- "checkIncidenceRate.csv"
write.csv(D4, file = f, row.names = FALSE)
file.info(f)
