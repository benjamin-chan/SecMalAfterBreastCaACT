setwd("~/GitHub repositories/AEAfterBreastCaACT/SecondaryMalignancies")
list.files()

library(openxlsx)
library(data.table)

pathBox <- "C:/Users/chanb/Box Sync/NCI Systematic Review - Secondary Malignancies"
f <- file.path(pathBox, "SecondaryMalignancies_Data_BG_3_4.xlsx")
sheets <- getSheetNames(f)
sheets

D <- read.xlsx(f, sheet = sheets[1], startRow = 3, colNames = FALSE)
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

# Create a rowid variable
# This will be handy later
D[, rowid := .I]

# Input missing data for Bergh (2000); row was split
col <- grep("^cyclophosphamide", names(D), invert = TRUE)
D[11, col] <- D[10, col, with = FALSE]
# D[11, ]
# D[10, ]

# Fix trial column for Romond (2005)
D[grep("Romond", authorYear), trial := gsub("&N", "& N", trial)]

# Make medianFU numeric
D <- D[grep("-", medianFU, invert = TRUE), medianFUNum := round(as.numeric(medianFU), digits = 2)]
x <- D[grep("-", medianFU), medianFU]
x <- as.numeric(unlist(strsplit(x, "-")))
x1 <- x[seq(1, length(x), 2)]
x2 <- x[seq(2, length(x), 2)]
xbar <- rowMeans(cbind(x1, x2))
D <- D[grep("-", medianFU), medianFUNum := xbar]
D <- D[,
       `:=` (medianFUChar = medianFU,
             medianFU = medianFUNum,
             medianFUNum = NULL)]
unique(D[, .(medianFU, medianFUChar)])
D <- D[, medianFUChar := NULL]

# Recode mal* values of "NR" and "-" to NA
recode <- function(x) {
    missval <- c("-", "NR", " ")
    vec <- D[, get(x)]
    vec[vec %in% missval] <- NA
    vec
}
col <- grep("^mal", names(D), value = TRUE)
for (i in 1:length(col)) {
    D[, col[i]] <- recode(col[i])
}

# Substitute non-numeric text in mal* values
substitute <- function(x) {
    vec <- D[, get(x)]
    vec <- gsub("[[:alpha:]]", "", vec)
    vec
}
col <- grep("^mal", names(D), value = TRUE)
for (i in 1:4) {
    # Only substitute a subset of mal* columns
    D[, col[i]] <- substitute(col[i])
}

# Convert mal* values to numeric
for (i in 1:4) {
    # Only convert a subset of mal* columns
    D <- D[, col[i] := as.numeric(get(col[i]))]
}

# The malAMLOrMDS column, as entered, captures number of AML or MDS malignancies if the study reported them grouped as opposed to separately.
# If the study reported them as separately, then these counts are captures in the malAML and malMDS columns.
# I.e., the malAML, malMDS, and malAMLOrMDS columns are mutually exclusive counts.
# We want to create another column, malAMLOrMDSTotal, to be non-mutually exclusive from malAML, malMDS, and malAMLOrMDS.
# If none of the malAML, malMDS, and malAMLOrMDS values are populated, then set malAMLOrMDSTotal to NA, also.
# NOTE: Couldn't get this to work: http://stackoverflow.com/a/16513949/1427069
s <- sample(D[, rowid], 12)
malAMLOrMDSTotal <- rowSums(D[, .(malAML, malMDS, malAMLOrMDS)], na.rm=TRUE)
D <- D[, malAMLOrMDSTotal := malAMLOrMDSTotal]
D <- D[is.na(malAML) & is.na(malMDS) & is.na(malAMLOrMDS), malAMLOrMDSTotal := NA]
D[s, .(rowid, malAML, malMDS, malAMLOrMDS, malAMLOrMDSTotal)]

# Remove text from nITT column
D <- D[authorYear == "Misset (1996)" & arm == 2, nITT := "137"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 1, nITT := "210"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 2, nITT := "197"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 3, nITT := "195"]
D <- D[, nITT := as.integer(nITT)]

# Clean up the regimen and dose columns
D <- D[, regimen := gsub("\\s$", "", regimen)]
D <- D[, regimen := gsub("\\r\\n", " ", regimen)]
doseVar <- grep("dose", names(D), ignore.case = TRUE, value = TRUE)
for (i in 1:length(doseVar)) {
    D <- D[, doseVar[i] := gsub("\\r\\n", " ", get(doseVar[i]))]
}
D[, .N, c("regimen", doseVar), with = TRUE][order(regimen)]
write.table(D[, .N, c("regimen", doseVar), with = TRUE][order(regimen)],
            file = "regimens.md",
            sep = " | ", quote = FALSE,
            row.names = FALSE)


D[,
  `:=`(isAnthra = !is.na(anthracyclineTotalDose),
        isCyclo = !is.na(cyclophosphamideDose),
        isTaxane = !is.na(taxaneTotalDose),
        isFluoro = !is.na(fluoroucilTotalDose))]
prec <- "%.3g"
summaryRegimens <- D[,
                     .(sumNITT = sum(nITT, na.rm = TRUE),
                       sumAML = sum(malAML, na.rm = TRUE),
                       #meanPctAML = sprintf(prec, mean(malAML / nITT, na.rm = TRUE) * 100),
                       medianPctAML = sprintf(prec, median(malAML / nITT, na.rm = TRUE) * 100),
                       sumMDS = sum(malMDS, na.rm = TRUE),
                       #meanPctMDS = sprintf(prec, mean(malMDS / nITT, na.rm = TRUE) * 100),
                       medianPctMDS = sprintf(prec, median(malMDS / nITT, na.rm = TRUE) * 100),
                       sumAMLOrMDS = sum(malAMLOrMDS, na.rm = TRUE),
                       #meanPctAMLOrMDS = sprintf(prec, mean(malAMLOrMDS / nITT, na.rm = TRUE) * 100),
                       medianPctAMLOrMDS = sprintf(prec, median(malAMLOrMDS / nITT, na.rm = TRUE) * 100),
                       sumNonBreastSolid = sum(malNonBreastSolid, na.rm = TRUE),
                       #meanPctNonBreastSolid = sprintf(prec, mean(malNonBreastSolid / nITT, na.rm = TRUE) * 100),
                       medianPctNonBreastSolid = sprintf(prec, median(malNonBreastSolid / nITT, na.rm = TRUE) * 100)),
                     .(isAnthra,
                       isCyclo,
                       isTaxane,
                       isFluoro)]
summaryRegimens <- summaryRegimens[order( - isAnthra, - isCyclo, - isTaxane, - isFluoro)]
write.table(summaryRegimens,
            file = "summaryRegimens.md",
            sep = " | ", quote = FALSE,
            row.names = FALSE)

# Hold the 1st sheet
D1 <- D

# Read the 2nd sheet
D <- read.xlsx(f, sheet = sheets[2], startRow = 1, colNames = TRUE)
D <- data.table(D)
oldnames <- names(D)
newnames <- c("id",
              "authorYear",
              "trial",
              "quality",
              "arm",
              "nRandomized",
              "nITT",
              "age",
              "menopausalStatus",
              "surgery",
              "tamoxifen",
              "tumorStage",
              "tumorSize",
              "nodalStatus",
              "hrStatus")
setnames(D, oldnames, newnames)


# Tidy data

# Input missing data for Bergh (2000); year is missing a digit
D[, authorYear := gsub("Bergh \\(200\\)", "Bergh \\(2000\\)", authorYear)]

# Recode values of "NR" and "-" to NA
D[, "menopausalStatus"] <- recode("menopausalStatus")
D[, "surgery"] <- recode("surgery")
D[, "tamoxifen"] <- recode("tamoxifen")
D[, "tumorStage"] <- recode("tumorStage")
D[, "tumorSize"] <- recode("tumorSize")
D[, "nodalStatus"] <- recode("nodalStatus")
D[, "hrStatus"] <- recode("hrStatus")


# Hold the 2nd sheet
D2 <- D


# Merge sheets
keyVar <- c("id", "authorYear", "arm")
D1 <- D1[, in1 := TRUE]
D2 <- D2[, in2 := TRUE]
setkeyv(D1, keyVar)
setkeyv(D2, keyVar)
D2 <- D2[,
         `:=` (trial = NULL,
               quality = NULL,
               nRandomized = NULL,
               nITT = NULL)]
D <- merge(D1, D2, all = TRUE)
D[is.na(in1) | is.na(in2), .(id, authorYear, arm, trial, in1, in2)]


# Save to RData
save(D, file = "SecMal.RData")
file.info("SecMal.RData")
load("SecMal.RData")
