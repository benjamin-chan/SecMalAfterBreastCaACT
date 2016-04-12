setwd("~/GitHub repositories/AEAfterBreastCaACT/SecondaryMalignancies")
list.files()
f <- "SecondaryMalignancies_Data_BG_3_4.xlsx"
library(openxlsx)
library(data.table)
D <- read.xlsx(f, startRow=3, colNames=FALSE)
D <- data.table(D)
oldnames <- names(D)
newnames <- c("id",
              "authorYear",
              "trial",
              "quality",
              "arm",
              "nRandomized",
              "nITT",
              # "pctCompletingTx",
              "medianFU",
              "regimen",
              "anthracyclineType",
              "anthracyclineTotalDose",
              "anthracyclineDuration",
              "anthracyclineCourses",
              "cyclophosphamideDose",
              "cyclophosphamideDuration",
              "cyclophosphamideCourses",
              "taxaneType",
              "taxaneTotalDose",
              "taxaneDuration",
              "taxaneCourses",
              "fluoroucilTotalDose",
              "fluoroucilDuration",
              "fluoroucilCourses",
              "otherTxDetails",
              "malAML",
              "malMDS",
              "malAMLOrMDS",
              "malNonBreastSolid",
              "malNonBreastSolidType",
              "malOtherBloodCancers",
              "malSMRelatedDeaths",
              "malSecondPrimary",
              "NOTES")
setnames(D, oldnames, newnames[1:33])


# Tidy data

# Input missing data for Bergh (2000); row was split
col <- grep("^cyclophosphamide", names(D), invert=TRUE)
D[11, col] <- D[10, col, with=FALSE]
# D[11, ]
# D[10, ]

