setwd("~/Projects/SecMalAfterBreastCaACT")

if(!require(checkpoint)){
  install.packages("checkpoint")
  library(checkpoint)
}
checkpoint("2016-08-17", use.knitr = TRUE)

sink("make.log")

files <- c("preamble.Rmd",
           "tidyData.Rmd",
           "summarize.Rmd",
           "makeAppendix1StudyCharacteristics.Rmd",
           "metaRegression.Rmd")

f <- file("master.Rmd", open="w")
for (i in 1:length(files)) {
    x <- readLines(files[i])
    writeLines(x, f)
    if (i < length(files)) {writeLines("\n---\n", f)}
}
close(f)

library(knitr)
library(rmarkdown)
knit("master.Rmd", output = "index.md")
file.remove("master.Rmd")

sink("session.log")
list(completionDateTime = Sys.time(),
     sessionInfo = sessionInfo())
sink()

sink()
