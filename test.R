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

# Recode mal* values of "NR" and "-" to NA
recode <- function (x) {
  missval <- c("-", "NR", " ")
  vec <- D[, get(x)]
  vec[vec %in% missval] <- NA
  vec
}
col <- grep("^mal", names(D), value=TRUE)
for (i in 1:length(col)) {
  D[, col[i]] <- recode(col[i])
}

# Substitute non-numeric text in mal* values
substitute <- function (x) {
  vec <- D[, get(x)]
  vec <- gsub("[[:alpha:]]", "", vec)
  vec
}
col <- grep("^mal", names(D), value=TRUE)
for (i in 1:4) {  # Only substitute a subset of mal* columns
  D[, col[i]] <- substitute(col[i])
}

# Convert mal* values to numeric
for (i in 1:4) {  # Only convert a subset of mal* columns
  D <- D[, col[i] := as.numeric(get(col[i]))]
}

# Remove text from nITT column
D <- D[authorYear == "Misset (1996)" & arm == 2, nITT := "137"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 1, nITT := "210"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 2, nITT := "197"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 3, nITT := "195"]
D <- D[, nITT := as.integer(nITT)]

# Clean up the regimen and dose columns
D <- D[, regimen := gsub("\\s$", "", regimen)]
D <- D[, regimen := gsub("\\r\\n", " ", regimen)]
doseVar <- grep("dose", names(D), ignore.case=TRUE, value=TRUE)
for (i in 1:length(doseVar)) {
  D <- D[, doseVar[i] := gsub("\\r\\n", " ", get(doseVar[i]))]
}
D[, .N, c("regimen", doseVar), with=TRUE][order(regimen)]
write.table(D[, .N, c("regimen", doseVar), with=TRUE][order(regimen)],
            file="regimens.md",
            sep=" | ", quote=FALSE,
            row.names=FALSE)

# Save to RData
save(D, file="SecMal.RData")
file.info("SecMal.RData")
load("SecMal.RData")
