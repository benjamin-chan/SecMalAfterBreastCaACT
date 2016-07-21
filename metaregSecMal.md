# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy
[Benjamin Chan](http://careers.stackoverflow.com/benjaminchan)  
`r Sys.time()`  


Load packages.



```r
if (!require(openxlsx)) {install.packages("openxlsx", dependencies=TRUE, repos="https://cloud.r-project.org")}
```

```
## Loading required package: openxlsx
```

```r
library(openxlsx)
if (!require(data.table)) {install.packages("data.table", dependencies=TRUE, repos="https://cloud.r-project.org")}
```

```
## Loading required package: data.table
```

```r
library(data.table)
```

# Load data

Load the data from [GitHub](https://github.com/benjamin-chan/SecMalAfterBreastCaACT).



```r
repo <- "https://github.com/benjamin-chan/SecMalAfterBreastCaACT"
url <- paste0(repo, "/raw/26115dab1eb2b075d40f545514a03c3b30cde6b4/SecondaryMalignancies_Data_BG_3_4.xlsx")
f <- tempfile()
download.file(url, f, mode="wb")
file.info(f)[c("size", "mtime")]
```

```
##                                                                          size
## C:\\Users\\chanb\\AppData\\Local\\Temp\\1\\RtmpGKr9BS\\file1ee471883556 59619
##                                                                                       mtime
## C:\\Users\\chanb\\AppData\\Local\\Temp\\1\\RtmpGKr9BS\\file1ee471883556 2016-07-21 16:19:40
```

```r
sheets <- getSheetNames(f)
# sheets
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
```

# Tidy data

Create a `rowid` variable.
This will be handy later.



```r
D <- D[, rowid := .I]
```

Fix some minor data entry inconsistencies.



```r
D <- D[grep("Nitz", authorYear), authorYear := gsub("2014$", "2014)", authorYear)]
D <- D[grep("Wolmark", authorYear), authorYear := gsub(", 2001$", " (2001)", authorYear)]
```

Input missing data for Bergh (2000); row was split.



```r
col <- grep("^cyclophosphamide", names(D), invert = TRUE)
D[11, col] <- D[10, col, with = FALSE]
# D[11, ]
# D[10, ]
```

Fix trial column for Romond (2005).



```r
D <- D[grep("Romond", authorYear), trial := gsub("&N", "& N", trial)]
```

Make `medianFU` numeric.



```r
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
unique(D[as.character(medianFU) != medianFUChar, .(medianFU, medianFUChar)])
```

```
##    medianFU       medianFUChar
## 1:     38.3 38.299999999999997
## 2:     73.0              71-75
## 3:     99.6 99.600000000000009
## 4:     28.8 28.799999999999997
```

```r
D <- D[, medianFUChar := NULL]
```

Recode mal* values of `NR` and `-` to `NA`.



```r
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
```

Substitute non-numeric text in mal* values.



```r
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
```

Convert mal* values to numeric.



```r
for (i in 1:4) {
    # Only convert a subset of mal* columns
    D <- D[, col[i] := as.numeric(get(col[i]))]
}
```

The `malAMLOrMDS` column, as entered, captures number of AML or MDS malignancies if the study reported them grouped as opposed to separately.
If the study reported them as separately, then these counts are captures in the `malAML` and `malMDS` columns.
I.e., the `malAML`, `malMDS`, and `malAMLOrMDS` columns are mutually exclusive counts.
We want to create another column, `malAMLOrMDSTotal`, to be non-mutually exclusive from `malAML`, `malMDS`, and `malAMLOrMDS`.
If none of the `malAML`, `malMDS`, and `malAMLOrMDS` values are populated, then set `malAMLOrMDSTotal` to `NA`, also.
NOTE: Couldn't get this to work: [http://stackoverflow.com/a/16513949/1427069](http://stackoverflow.com/a/16513949/1427069).



```r
s <- sample(D[, rowid], 12)
malAMLOrMDSTotal <- rowSums(D[, .(malAML, malMDS, malAMLOrMDS)], na.rm=TRUE)
D <- D[, malAMLOrMDSTotal := malAMLOrMDSTotal]
D <- D[is.na(malAML) & is.na(malMDS) & is.na(malAMLOrMDS), malAMLOrMDSTotal := NA]
D[s, .(rowid, malAML, malMDS, malAMLOrMDS, malAMLOrMDSTotal)]
```

```
##     rowid malAML malMDS malAMLOrMDS malAMLOrMDSTotal
##  1:    56      1      1          NA                2
##  2:    33      0      0          NA                0
##  3:    84     NA     NA           0                0
##  4:    37      3     NA          NA                3
##  5:    15     NA     NA           2                2
##  6:    75      0      0          NA                0
##  7:     5     NA     NA          NA               NA
##  8:    80     NA     NA          NA               NA
##  9:    10      0      0          NA                0
## 10:     2     NA     NA          NA               NA
## 11:    89     NA     NA          NA               NA
## 12:    68     NA     NA          NA               NA
```

Remove text from `nITT` column.



```r
D <- D[authorYear == "Misset (1996)" & arm == 2, nITT := "137"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 1, nITT := "210"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 2, nITT := "197"]
D <- D[authorYear == "Fumoleu (2003)" & arm == 3, nITT := "195"]
D <- D[, nITT := as.integer(nITT)]
```

Clean up the `regimen` and `dose` columns.
Output to [regimens.md](regimens.md) for checking.



```r
D <- D[, regimen := gsub("\\s$", "", regimen)]
D <- D[, regimen := gsub("\\r\\n", " ", regimen)]
doseVar <- grep("dose", names(D), ignore.case = TRUE, value = TRUE)
for (i in 1:length(doseVar)) {
    D <- D[, doseVar[i] := gsub("\\r\\n", " ", get(doseVar[i]))]
}
write.table(D[, .N, c("regimen", doseVar), with = TRUE][order(regimen)],
            file = "regimens.md",
            sep = " | ", quote = FALSE,
            row.names = FALSE)
file.info("regimens.md")
```

```
##             size isdir mode               mtime               ctime
## regimens.md 2221 FALSE  666 2016-07-21 16:19:41 2016-06-29 07:25:19
##                           atime exe
## regimens.md 2016-07-21 16:19:41  no
```

Calculate cumulative dose: $\text{total dose} \times \text{number of courses}$.



```r
D <- D[, anthracyclineTotalDose := as.numeric(anthracyclineTotalDose)]
```

```
## Warning: NAs introduced by coercion
```

```r
D <- D[authorYear == "Bergh (2000)" & regimen == "FEC\nTailored", anthracyclineTotalDose := 75]
D <- D[authorYear == "Henderson (2003)", anthracyclineTotalDose := 75]
D <- D[authorYear == "Colleoni (2009)" & regimen == "AC-CMF", anthracyclineTotalDose := 75]
D <- D[, cyclophosphamideDose := as.numeric(cyclophosphamideDose)]
```

```
## Warning: NAs introduced by coercion
```

```r
D <- D[authorYear == "Colleoni (2009)" & regimen == "EC", cyclophosphamideDose := 600]
D <- D[authorYear == "Bergh (2000)" & regimen == "FEC\nTailored", cyclophosphamideDose := 900]
D <- D[authorYear == "Henderson (2003)", anthracyclineTotalDose := 75]
D <- D[, taxaneCourses := as.numeric(taxaneCourses)]
```

```
## Warning: NAs introduced by coercion
```

```r
D <- D[authorYear == "Shulman (2014)" & regimen == "T" & arm == 3, taxaneCourses := 4]
D <- D[authorYear == "Shulman (2014)" & regimen == "T" & arm == 4, taxaneCourses := 6]
D <- D[, fluoroucilTotalDose := as.numeric(fluoroucilTotalDose)]
```

```
## Warning: NAs introduced by coercion
```

```r
D <- D[authorYear == "Bergh (2000)" & regimen == "FEC\nTailored", fluoroucilTotalDose := 600]
D <- D[authorYear == "Joensuu (2012)" & regimen == "TX-CEX", fluoroucilTotalDose := NA]
D <- D[!is.na(anthracyclineTotalDose) & !is.na(anthracyclineTotalDose),
       anthracyclineCumulDose := anthracyclineTotalDose * anthracyclineCourses]
D <- D[!is.na(cyclophosphamideDose) & !is.na(cyclophosphamideCourses),
       cyclophosphamideCumulDose := cyclophosphamideDose * cyclophosphamideCourses]
D <- D[!is.na(as.numeric(taxaneTotalDose)) & !is.na(taxaneCourses),
       taxaneCumulDose := as.numeric(taxaneTotalDose) * taxaneCourses]
D <- D[!is.na(fluoroucilTotalDose) & !is.na(fluoroucilCourses),
       fluoroucilCumulDose := fluoroucilTotalDose * fluoroucilCourses]
# D <- D[is.na(anthracyclineCumulDose), anthracyclineCumulDose := 0]
# D <- D[is.na(cyclophosphamideCumulDose), cyclophosphamideCumulDose := 0]
# D <- D[is.na(taxaneCumulDose), taxaneCumulDose := 0]
# D <- D[is.na(fluoroucilCumulDose), fluoroucilCumulDose := 0]
```

Check.



```r
head(D[!is.na(anthracyclineCumulDose),
       .(authorYear, regimen, anthracyclineCumulDose, anthracyclineTotalDose, anthracyclineCourses)])
```

```
##       authorYear regimen anthracyclineCumulDose anthracyclineTotalDose
## 1: Fisher (1990)      AC                    240                     60
## 2: Fisher (1990)  AC-CMF                    240                     60
## 3: Misset (1996)    AVCF                    360                     30
## 4: Fisher (1999)      AC                    240                     60
## 5: Fisher (1999)      AC                    240                     60
## 6: Fisher (1999)      AC                    240                     60
##    anthracyclineCourses
## 1:                    4
## 2:                    4
## 3:                   12
## 4:                    4
## 5:                    4
## 6:                    4
```

```r
head(D[!is.na(cyclophosphamideCumulDose),
       .(authorYear, regimen, cyclophosphamideCumulDose, cyclophosphamideDose, cyclophosphamideCourses)])
```

```
##       authorYear regimen cyclophosphamideCumulDose cyclophosphamideDose
## 1: Fisher (1990)      AC                      2400                  600
## 2: Fisher (1990)  AC-CMF                      2400                  600
## 3: Misset (1996)    AVCF                     14400                 1200
## 4: Fisher (1999)      AC                      4800                 1200
## 5: Fisher (1999)      AC                      4800                 2400
## 6: Fisher (1999)      AC                      9600                 2400
##    cyclophosphamideCourses
## 1:                       4
## 2:                       4
## 3:                      12
## 4:                       4
## 5:                       2
## 6:                       4
```

```r
head(D[!is.na(taxaneCumulDose),
       .(authorYear, regimen, taxaneCumulDose, taxaneTotalDose, taxaneCourses)])
```

```
##        authorYear regimen taxaneCumulDose taxaneTotalDose taxaneCourses
## 1:  Citron (2003)   A-T-C             700             175             4
## 2:  Citron (2003)   A-T-C             700             175             4
## 3:  Citron (2003)    AC-T             700             175             4
## 4:  Citron (2003)    AC-T             700             175             4
## 5: Francis (2008) A-T-CMF             300             100             3
## 6: Francis (2008)  AT-CMF             300              75             4
```

```r
head(D[!is.na(fluoroucilCumulDose),
       .(authorYear, regimen, fluoroucilCumulDose, fluoroucilTotalDose, fluoroucilCourses)])
```

```
##          authorYear       regimen fluoroucilCumulDose fluoroucilTotalDose
## 1:    Misset (1996)          AVCF               19200                1600
## 2:     Bergh (2000) FEC\nTailored                5400                 600
## 3: Venturini (2005)           FEC                3600                 600
## 4: Venturini (2005)           FEC                3600                 600
## 5:    Martin (2010)           FAC                3000                 500
## 6:     Roche (2006)           FEC                3000                 500
##    fluoroucilCourses
## 1:                12
## 2:                 9
## 3:                 6
## 4:                 6
## 5:                 6
## 6:                 6
```

## Manual tidying

After an early round of analysis, we realized there were some `nITT` data entry errors in the commit [26115da](https://github.com/benjamin-chan/SecMalAfterBreastCaACT/raw/26115dab1eb2b075d40f545514a03c3b30cde6b4/SecondaryMalignancies_Data_BG_3_4.xlsx) (Jun 1, 2016) spreadsheet.



```r
D <- D[authorYear == "Mamounas (2005)" & nITT == 1528, `:=` (nITTOld = nITT, nITT = 1529, isFixed = TRUE)]
D <- D[authorYear == "Fumoleu (2003)" & nITT == 210, `:=` (nITTOld = nITT, nITT = 212, isFixed = TRUE)]
D <- D[authorYear == "Fumoleu (2003)" & nITT == 197, `:=` (nITTOld = nITT, nITT = 209, isFixed = TRUE)]
D <- D[authorYear == "Fumoleu (2003)" & nITT == 195, `:=` (nITTOld = nITT, nITT = 200, isFixed = TRUE)]
D <- D[authorYear == "Fargeot (2004)" & nITT == 155, `:=` (nITTOld = nITT, nITT = 164, isFixed = TRUE)]
D <- D[authorYear == "Fargeot (2004)" & nITT == 163, `:=` (nITTOld = nITT, nITT = 174, isFixed = TRUE)]
D <- D[authorYear == "Kerbrat (2007)" & nITT == 235, `:=` (nITTOld = nITT, nITT = 241, isFixed = TRUE)]
D <- D[authorYear == "Kerbrat (2007)" & nITT == 236, `:=` (nITTOld = nITT, nITT = 241, isFixed = TRUE)]
D <- D[authorYear == "Fisher (1997)" & nITT == 763, `:=` (nITTOld = nITT, nITT = 767, isFixed = TRUE)]
D <- D[authorYear == "Fisher (1997)" & nITT == 771, `:=` (nITTOld = nITT, nITT = 772, isFixed = TRUE)]
D <- D[authorYear == "Bonneterre (2005)" & nITT == 271, `:=` (nITTOld = nITT, nITT = 289, isFixed = TRUE)]
D <- D[authorYear == "Bonneterre (2005)" & nITT == 266, `:=` (nITTOld = nITT, nITT = 276, isFixed = TRUE)]
D <- D[authorYear == "Roche (2006)" & nITT == 163, `:=` (nITTOld = nITT, nITT = 164, isFixed = TRUE)]
D <- D[authorYear == "Roche (2006)" & nITT == 168, `:=` (nITTOld = nITT, nITT = 169, isFixed = TRUE)]
D <- D[authorYear == "Shulman (2014)" & nITT == 1107, `:=` (nITTOld = nITT, nITT = 1142, isFixed = TRUE)]
D <- D[authorYear == "Shulman (2014)" & nITT ==  766, `:=` (nITTOld = nITT, nITT =  789, isFixed = TRUE)]
D <- D[authorYear == "Shulman (2014)" & nITT == 1119, `:=` (nITTOld = nITT, nITT = 1151, isFixed = TRUE)]
D <- D[authorYear == "Shulman (2014)" & nITT ==  782, `:=` (nITTOld = nITT, nITT =  789, isFixed = TRUE)]
D <- D[authorYear == "Swain (2013)" & nITT == 1617, `:=` (nITTOld = nITT, nITT = 1630, isFixed = TRUE)]
D <- D[authorYear == "Swain (2013)" & nITT == 1624, `:=` (nITTOld = nITT, nITT = 1634, isFixed = TRUE)]
D <- D[authorYear == "Swain (2013)" & nITT == 1618, `:=` (nITTOld = nITT, nITT = 1630, isFixed = TRUE)]
D[isFixed == TRUE, .(authorYear, regimen, nITTOld, nITT)]
```

```
##            authorYear   regimen nITTOld nITT
##  1:   Mamounas (2005)        AC    1528 1529
##  2:    Fumoleu (2003)    FEC 50     210  212
##  3:    Fumoleu (2003)  3 FEC 50     197  209
##  4:    Fumoleu (2003)    FEC 75     195  200
##  5:    Fargeot (2004) Tamoxifen     155  164
##  6:    Fargeot (2004)   EPI-Tam     163  174
##  7:    Kerbrat (2007)       FEC     235  241
##  8:    Kerbrat (2007)     E+Vnr     236  241
##  9:     Fisher (1997)        AC     763  767
## 10:     Fisher (1997)        AC     771  772
## 11: Bonneterre (2005)    FEC 50     271  289
## 12: Bonneterre (2005)   FEC 100     266  276
## 13:      Roche (2006) Tamoxifen     163  164
## 14:      Roche (2006)       FEC     168  169
## 15:    Shulman (2014)        AC    1107 1142
## 16:    Shulman (2014)        AC     766  789
## 17:    Shulman (2014)         T    1119 1151
## 18:    Shulman (2014)         T     782  789
## 19:      Swain (2013)       ACT    1617 1630
## 20:      Swain (2013)      AC-T    1624 1634
## 21:      Swain (2013)     AC-TG    1618 1630
##            authorYear   regimen nITTOld nITT
```

```r
D <- D[, `:=` (nITTOld = NULL, isFixed = NULL)]
```

We also made the decision to use the AML/MDS outcomes reported in Perez (2011) as the outcomes for Romond (2005) since they study the same cohort. Other edits to *AML or MDS* and *non-breast solid* malignancy outcome were needed, as well.



```r
D <- D[authorYear == "Martin (2010)" & regimen == "TAC",
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 0, isFixed = TRUE)]
D <- D[authorYear == "Martin (2010)" & regimen == "FAC",
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 0, isFixed = TRUE)]
D <- D[authorYear == "Romond (2005)" & regimen == "AC-T",
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 2, isFixed = TRUE)]
D <- D[authorYear == "Romond (2005)" & regimen == "ACT-T-Trast",
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 1, isFixed = TRUE)]
D <- D[authorYear == "Del Mastro (2015)",
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 0, isFixed = TRUE)]
D <- D[authorYear == "Del Mastro (2015)" & regimen == "EC-T" & nITT == 502,
      `:=` (malAMLOrMDSTotalOld = malAMLOrMDSTotal, malAMLOrMDSTotal = 2, isFixed = TRUE)]

D <- D[authorYear == "Bernard-Marty (2003)" & regimen == "EC" & nITT == 267,
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 4, isFixed = TRUE)]
D <- D[authorYear == "Bonneterre (2005)" & regimen == "FEC 50",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 11, isFixed = TRUE)]
D <- D[authorYear == "Bonneterre (2005)" & regimen == "FEC 100",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 15, isFixed = TRUE)]
D <- D[authorYear == "Citron (2003)" & regimen == "A-T-C" & nITT == 493,
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 11, isFixed = TRUE)]
D <- D[authorYear == "Citron (2003)" & regimen == "AC-T" & nITT == 495,
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 11, isFixed = TRUE)]
D <- D[authorYear == "Eiermann (2011)" & regimen == "AC-T",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 34, isFixed = TRUE)]
D <- D[authorYear == "Eiermann (2011)" & regimen == "TAC",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 31, isFixed = TRUE)]
D <- D[authorYear == "Romond (2005)" & regimen == "AC-T",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 13, isFixed = TRUE)]
D <- D[authorYear == "Romond (2005)" & regimen == "ACT-T-Trast",
      `:=` (malNonBreastSolidOld = malNonBreastSolid, malNonBreastSolid = 4, isFixed = TRUE)]

D[isFixed == TRUE,
  .(authorYear, regimen, nITT, malAMLOrMDSTotalOld, malAMLOrMDSTotal, malNonBreastSolidOld, malNonBreastSolid)]
```

```
##               authorYear     regimen nITT malAMLOrMDSTotalOld
##  1: Bernard-Marty (2003)          EC  267                  NA
##  2:        Citron (2003)       A-T-C  493                  NA
##  3:        Citron (2003)        AC-T  495                  NA
##  4:        Martin (2010)         TAC  539                  NA
##  5:        Martin (2010)         FAC  521                  NA
##  6:    Bonneterre (2005)      FEC 50  289                  NA
##  7:    Bonneterre (2005)     FEC 100  276                  NA
##  8:      Eiermann (2011)        AC-T 1649                  NA
##  9:      Eiermann (2011)         TAC 1649                  NA
## 10:        Romond (2005)        AC-T  872                  NA
## 11:        Romond (2005) ACT-T-Trast  864                  NA
## 12:    Del Mastro (2015)        EC-T  545                  NA
## 13:    Del Mastro (2015)       FEC-T  544                  NA
## 14:    Del Mastro (2015)        EC-T  502                   0
## 15:    Del Mastro (2015)       FEC-T  500                  NA
##     malAMLOrMDSTotal malNonBreastSolidOld malNonBreastSolid
##  1:                0                    3                 4
##  2:                3                   10                11
##  3:                2                   10                11
##  4:                0                   NA                 9
##  5:                0                   NA                16
##  6:                1                    9                11
##  7:                1                   12                15
##  8:                2                   NA                34
##  9:                4                   NA                31
## 10:                2                   NA                13
## 11:                1                   NA                 4
## 12:                0                   NA                NA
## 13:                0                   NA                NA
## 14:                2                   NA                NA
## 15:                0                   NA                NA
```

```r
D <- D[, `:=` (malAMLOrMDSTotalOld = NULL, malNonBreastSolidOld = NULL, isFixed = NULL)]
```

Deduplicate.



```r
n0 <- nrow(D)
setkey(D, authorYear, trial, arm)
D <- unique(D)
message(sprintf("Removed %d duplicate row(s) (%.03g%%)", n0 - nrow(D), (n0 - nrow(D)) / n0 * 100))
```

```
## Removed 1 duplicate row(s) (1.03%)
```

# Summarize

Define some functions.



```r
scale <- 1e4
calcPct <- function (x, n) {
  prec <- "%.3g"
  sprintf(paste0(prec, "%%"),
          median(x / n, na.rm = TRUE) * 100)
}
calcRate <- function (x, n, y) {
  prec <- "%.3g"
  py <- scale
  sprintf(paste(prec, "per %s p-y"),
          median(x / (n * (y / 12)), na.rm=TRUE) * py,
          py)
}
```

Summarize the regimens.
Output to [summaryRegimens.md](summaryRegimens.md) for checking.



```r
D <- D[,
       `:=` (isAnthra = !is.na(anthracyclineTotalDose),
             isCyclo = !is.na(cyclophosphamideDose),
             isTaxane = !is.na(taxaneTotalDose),
             isFluoro = !is.na(fluoroucilTotalDose))]
D1 <- melt(D,
           id.vars=c("id", "authorYear", "arm",
                     "isAnthra", "anthracyclineCumulDose",
                     "isCyclo", "cyclophosphamideCumulDose",
                     "isTaxane", "taxaneCumulDose",
                     "isFluoro", "fluoroucilCumulDose",
                     "nITT", "medianFU"),
           measure.vars=c("malAML", "malMDS", "malAMLOrMDSTotal", "malNonBreastSolid"),
           value.name="malN",
           variable.name="malType")

D1 <- D1[, malType := gsub("^mal", "", malType)]
D1 <- D1[, malType := factor(malType,
                             levels=c("AML", "MDS", "AMLOrMDSTotal", "NonBreastSolid"),
                             labels=c("AML", "MDS", "AML or MDS", "Non-Breast Solid"))]
D1 <- D1[, py := nITT * (medianFU / 12)]
D1 <- D1[, rate := malN / py * scale]
summaryRegimens <- D1[,
                      .(totalN = sum(nITT, na.rm = TRUE),
                        totalPersonYears = round(sum(py, na.rm = TRUE)),
                        totalMalignancies = sum(malN, na.rm = TRUE),
                        medianPct = calcPct(malN, nITT),
                        medianRate = calcRate(malN, nITT, medianFU)),
                      .(isAnthra,
                        isCyclo,
                        isTaxane,
                        isFluoro,
                        malType)]
summaryRegimens <- summaryRegimens[order(-isAnthra, -isCyclo, -isTaxane, -isFluoro, malType)]
write.table(summaryRegimens,
            file = "summaryRegimens.md",
            sep = " | ", quote = FALSE,
            row.names = FALSE)
file.info("summaryRegimens.md")
```

```
##                    size isdir mode               mtime               ctime
## summaryRegimens.md 2916 FALSE  666 2016-07-21 16:19:42 2016-06-29 07:25:19
##                                  atime exe
## summaryRegimens.md 2016-07-21 16:19:42  no
```

# Meta-regression

Estimate meta-regression models for log transformed incidence rate.
Model is

$$\frac{y_i}{\text{py}_i} = \beta x_i + \sigma_\text{study}$$

Or

$$\frac{y_i}{\text{py}_i} = \beta I_{\text{high dose}, i} + \sigma_\text{study}$$

Models were estimated using the `rma.mv()` function from the metafor` package for R.



```r
library(metafor)
```

```
## Loading required package: Matrix
```

```
## Loading 'metafor' package (version 1.9-8). For an overview 
## and introduction to the package please type: help(metafor).
```

```r
citation("metafor")
```

```
## 
## To cite the metafor package in publications, please use:
## 
##   Viechtbauer, W. (2010). Conducting meta-analyses in R with the
##   metafor package. Journal of Statistical Software, 36(3), 1-48.
##   URL: http://www.jstatsoft.org/v36/i03/
## 
## A BibTeX entry for LaTeX users is
## 
##   @Article{,
##     title = {Conducting meta-analyses in {R} with the {metafor} package},
##     author = {Wolfgang Viechtbauer},
##     journal = {Journal of Statistical Software},
##     year = {2010},
##     volume = {36},
##     number = {3},
##     pages = {1--48},
##     url = {http://www.jstatsoft.org/v36/i03/},
##   }
```

Define meta-regression functions.



```r
pvalToChar <- function (p) {
  if (p < 0.001) {
    pvalue <- "p < 0.001"
  } else {
    pvalue <- sprintf("p = %.03f", p)
  }
  pvalue
}
metareg <- function (D) {
  require(metafor)
  D <- D[!(is.na(x) | is.na(rate) | is.na(nITT) | is.na(malType))]
  xData <- unique(D[, .(drug, x, xHighDose, malType)])
  D <- escalc("IRLN", xi=malN, ti=py, data=D)
  randomEffect <- list(~ 1 | id)
  MLin <- rma.mv(yi ~ x, vi, random=randomEffect, data=D)
  MBin <- rma.mv(yi ~ xHighDose, vi, random=randomEffect, data=D)
  pvalueLin <- MLin$pval[which(row.names(MLin$b) == "x")]
  pvalueBin <- MBin$pval[which(row.names(MBin$b) == "xHighDoseTRUE")]
  predLin <- predict(MLin, xData[, x], transf = exp)[["pred"]] * scale
  confLowerLin <- predict(MLin, xData[, x], transf = exp)[["cr.lb"]] * scale
  confUpperLin <- predict(MLin, xData[, x], transf = exp)[["cr.ub"]] * scale
  predBin <- predict(MBin, xData[, xHighDose], transf = exp)[["pred"]] * scale
  confLowerBin <- predict(MBin, xData[, xHighDose], transf = exp)[["cr.lb"]] * scale
  confUpperBin <- predict(MBin, xData[, xHighDose], transf = exp)[["cr.ub"]] * scale
  pred <- data.table(xData, predLin, confLowerLin, confUpperLin, predBin, confLowerBin, confUpperBin, scale)
  setorder(pred, x)
  list(rmaLin = MLin,
       rmaBin = MBin,
       pvalueLin = pvalToChar(pvalueLin),
       pvalueBin = pvalToChar(pvalueBin),
       pred = pred)
}
plotreg <- function (M, D, title, xlab, xbreaks, xscale) {
  require(ggplot2)
  require(RColorBrewer)
  require(tools)
  pvalues <- c(M$pvalueLin, M$pvalueBin)
  x <- seq(min(M$rmaLin$X[, "x"]), max(M$rmaLin$X[, "x"]), length.out=100)
  yhat1 <- unique(data.table(malType = mal,
                             x,
                             yhat = predict(M$rmaLin, x, transf = exp)[["pred"]] * scale))
  xHighDose <- as.logical(M$rmaBin$X[, "xHighDoseTRUE"])
  yhat2 <- unique(data.table(malType = mal,
                             xHighDose,
                             yhat = predict(M$rmaBin, xHighDose, transf = exp)[["pred"]] * scale))
  D <- D[!(is.na(x) | is.na(rate) | is.na(nITT) | is.na(malType))]
  D <- D[, malType := droplevels(malType)]
  steps <- merge(D[, .(min = min(x), max = max(x)), .(malType, xHighDose)],
                 yhat2,
                 by=c("malType", "xHighDose"))
  steps <- melt(steps, id.vars=c("malType", "xHighDose", "yhat"), measure.vars=c("min", "max"), value.name="x")
  annoLin <- data.frame(x=Inf, y=1.2, label=pvalues[c(1)], malType=levels(D[, malType]))
  annoBin <- data.frame(x=Inf, y=1.4, label=pvalues[c(2)], malType=levels(D[, malType]))
  pal <- brewer.pal(4, name="RdBu")
  G <- ggplot(D, aes(x=x * xscale, y=rate + 1/2, size=nITT / min(nITT, na.rm=TRUE)))
  G <- G + geom_point(alpha=1/3, position="jitter")
  G <- G + geom_line(data=yhat1, aes(x=x * xscale, y=yhat), inherit.aes=FALSE, color=pal[4])
  G <- G + geom_step(data=steps, aes(x=x * xscale, y=yhat), inherit.aes=FALSE, color=pal[1])
  G <- G + geom_text(data=annoLin,
                     aes(x, y, label=label), inherit.aes=FALSE, hjust=1, color=pal[4])
  G <- G + geom_text(data=annoBin,
                     aes(x, y, label=label), inherit.aes=FALSE, hjust=1, color=pal[1])
  G <- G + scale_x_log10(xlab, breaks=xbreaks)
  G <- G + scale_y_log10(sprintf("Rate per %s person-years", format(scale, big.mark=",")))
  G <- G + labs(title=title)
  G <- G + theme_bw()
  G <- G + theme(legend.position="none")
  filename <- gsub("\\s+",
                   "",
                   toTitleCase(paste(gsub("[[:punct:]]", "", title),
                                     gsub("cumulative dose", "", xlab),
                                     sep="_")))
  ggsave(filename=sprintf("%s.png", filename))
  write.csv(D, file=sprintf("%s.csv", filename), row.names=FALSE, quote=FALSE)
  M$pred[, x := x * xscale]
  write.csv(M$pred, file=sprintf("%s_Pred.csv", filename), row.names=FALSE, quote=FALSE)
  show(file.info(c(sprintf("%s.png", filename), sprintf("%s.csv", filename)))[c("size", "mtime")])
  G
}
```

Dichotomize cumulative doses

* Cyclophosphamide
    * $\lt 2400$
    * $\ge 2400$
* Taxane
    * $\lt 500$
    * $\ge 500$



```r
D2 <- D1[,
         .(id = factor(id),
           authorYear,
           isCyclo,
           xCyc = cyclophosphamideCumulDose / 1e3,
           xCycHighDose = cyclophosphamideCumulDose >= 2400,
           xTax = taxaneCumulDose / 1e2,
           xTaxHighDose = taxaneCumulDose >= 500,
           isAnthra,
           isTaxane,
           isFluoro,
           nITT,
           medianFU,
           malType,
           malN,
           py,
           rate)]
```

## AML/MDS

* Fit the models
* Show model summaries
* Plot the predicted values



```r
mal <- "AML or MDS"
D3 <- D2[malType == mal]
```



```r
D3 <- D3[, `:=` (drug = "Cyclophosphamide", x = xCyc, xHighDose = xCycHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.3175  0.5634     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 55) = 95.6084, p-val = 0.0006
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 12.3389, p-val = 0.0004
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -7.8323  0.2463  -31.7969  <.0001  -8.3151  -7.3495  ***
## x          0.1975  0.0562    3.5127  0.0004   0.0873   0.3078  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.5878  0.7667     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 55) = 135.0328, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.7092, p-val = 0.1911
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -7.7559  0.4686  -16.5504  <.0001  -8.6744  -6.8374  ***
## xHighDoseTRUE    0.6096  0.4662    1.3074  0.1911  -0.3043   1.5234     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p < 0.001"
## 
## $pvalueBin
## [1] "p = 0.191"
## 
## $pred
##                 drug    x xHighDose    malType   predLin confLowerLin
##  1: Cyclophosphamide 0.90     FALSE AML or MDS  4.738847     1.460555
##  2: Cyclophosphamide 1.20     FALSE AML or MDS  5.028162     1.561695
##  3: Cyclophosphamide 1.50     FALSE AML or MDS  5.335139     1.668353
##  4: Cyclophosphamide 2.00     FALSE AML or MDS  5.888979     1.858791
##  5: Cyclophosphamide 2.40      TRUE AML or MDS  6.373168     2.022926
##  6: Cyclophosphamide 3.00      TRUE AML or MDS  7.175108     2.289492
##  7: Cyclophosphamide 3.60      TRUE AML or MDS  8.077956     2.581317
##  8: Cyclophosphamide 4.00      TRUE AML or MDS  8.742122     2.790305
##  9: Cyclophosphamide 4.80      TRUE AML or MDS 10.238766     3.243888
## 10: Cyclophosphamide 6.64      TRUE AML or MDS 14.726459     4.474304
## 11: Cyclophosphamide 7.20      TRUE AML or MDS 16.449013     4.902518
## 12: Cyclophosphamide 8.10      TRUE AML or MDS 19.649407     5.645432
## 13: Cyclophosphamide 9.60      TRUE AML or MDS 26.426040     7.040370
##     confUpperLin  predBin confLowerBin confUpperBin scale
##  1:     15.37544 4.282064    0.7358653     24.91770 10000
##  2:     16.18909 4.282064    0.7358653     24.91770 10000
##  3:     17.06097 4.282064    0.7358653     24.91770 10000
##  4:     18.65733 4.282064    0.7358653     24.91770 10000
##  5:     20.07848 7.877381    1.6796486     36.94412 10000
##  6:     22.48628 7.877381    1.6796486     36.94412 10000
##  7:     25.27910 7.877381    1.6796486     36.94412 10000
##  8:     27.38937 7.877381    1.6796486     36.94412 10000
##  9:     32.31687 7.877381    1.6796486     36.94412 10000
## 10:     48.46979 7.877381    1.6796486     36.94412 10000
## 11:     55.19001 7.877381    1.6796486     36.94412 10000
## 12:     68.39144 7.877381    1.6796486     36.94412 10000
## 13:     99.19019 7.877381    1.6796486     36.94412 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Cyclophosphamide cumulative dose",
        1e3 * c(0.5, 1, 2, 4, 8, 16),
        1e3)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: RColorBrewer
```

```
## Loading required package: tools
```

```
## Saving 7 x 5 in image
```

```
##                                size               mtime
## AMLorMDS_Cyclophosphamide.png 46338 2016-07-21 16:19:44
## AMLorMDS_Cyclophosphamide.csv  7433 2016-07-21 16:19:44
```

![](metaregSecMal_files/figure-html/unnamed-chunk-25-1.png)<!-- -->



```r
D3 <- D3[, `:=` (drug = "Taxane", x = xTax, xHighDose = xTaxHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2494  0.4994     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 25) = 30.4549, p-val = 0.2077
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 2.5512, p-val = 0.1102
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -8.2329  0.5566  -14.7925  <.0001  -9.3238  -7.1421  ***
## x          0.1455  0.0911    1.5972  0.1102  -0.0331   0.3241     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1712  0.4137     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 25) = 26.2243, p-val = 0.3957
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 4.7966, p-val = 0.0285
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -7.7251  0.2433  -31.7527  <.0001  -8.2019  -7.2482  ***
## xHighDoseTRUE    0.6598  0.3013    2.1901  0.0285   0.0693   1.2503    *
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.110"
## 
## $pvalueBin
## [1] "p = 0.029"
## 
## $pred
##       drug   x xHighDose    malType  predLin confLowerLin confUpperLin
##  1: Taxane 2.4     FALSE AML or MDS 3.768667     1.127516     12.59658
##  2: Taxane 3.0     FALSE AML or MDS 4.112574     1.292010     13.09066
##  3: Taxane 3.2     FALSE AML or MDS 4.234047     1.349621     13.28311
##  4: Taxane 4.0     FALSE AML or MDS 4.756896     1.591300     14.21986
##  5: Taxane 4.5     FALSE AML or MDS 5.115978     1.748778     14.96658
##  6: Taxane 4.8     FALSE AML or MDS 5.344310     1.844382     15.48576
##  7: Taxane 6.0      TRUE AML or MDS 6.364197     2.221764     18.23011
##  8: Taxane 7.0      TRUE AML or MDS 7.361284     2.510308     21.58639
##  9: Taxane 7.5      TRUE AML or MDS 7.916961     2.639796     23.74361
## 10: Taxane 8.0      TRUE AML or MDS 8.514585     2.757579     26.29051
## 11: Taxane 9.0      TRUE AML or MDS 9.848576     2.955772     32.81526
##      predBin confLowerBin confUpperBin scale
##  1: 4.416177     1.723889     11.31316 10000
##  2: 4.416177     1.723889     11.31316 10000
##  3: 4.416177     1.723889     11.31316 10000
##  4: 4.416177     1.723889     11.31316 10000
##  5: 4.416177     1.723889     11.31316 10000
##  6: 4.416177     1.723889     11.31316 10000
##  7: 8.542865     3.399838     21.46588 10000
##  8: 8.542865     3.399838     21.46588 10000
##  9: 8.542865     3.399838     21.46588 10000
## 10: 8.542865     3.399838     21.46588 10000
## 11: 8.542865     3.399838     21.46588 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Taxane cumulative dose",
        1e2 * c(0.5, 1, 2, 4, 8, 16),
        1e2)
```

```
## Saving 7 x 5 in image
```

```
##                      size               mtime
## AMLorMDS_Taxane.png 37833 2016-07-21 16:19:45
## AMLorMDS_Taxane.csv  3290 2016-07-21 16:19:45
```

![](metaregSecMal_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

### Cyclophosphamide, with and without Taxanes

The effect of cyclophosphamide could be due to taxanes in the regimen.

Fit the meta-regression model

$$\frac{y_i}{\text{py}_i} = \beta_0 + \beta_1 x_{\text{cyclophosphamide dose}, i} + \beta_2 I_{\text{taxane}, i} + \beta_3 x_{\text{cyclophosphamide dose}, i} I_{\text{taxane}, i} + \sigma_\text{study}$$

where $I_{\text{taxane}, i}$ is an 0/1 indicator for whether study $i$ has taxane in the regimen.
The interpretation of $\beta_1$ is the effect of cyclophosphamide dose among the studies with no taxane in the regimen.


```r
mal <- "AML or MDS"
D3 <- D2[malType == mal]
D3 <- D3[, drug := "Cyclophosphamide"]
D3 <- D3[isCyclo == FALSE, xCycHighDose := FALSE]
D3 <- D3[!(is.na(xCyc) | is.na(xCycHighDose) | is.na(rate) | is.na(nITT) | is.na(malType))]
xData <- unique(D3[, .(drug, xCyc, xCycHighDose, isTaxane, malType)])
D3 <- escalc("IRLN", xi=malN, ti=py, data=D3)
randomEffect <- list(~ 1 | id)
MLin <- rma.mv(yi ~ xCyc + isTaxane + xCyc * isTaxane,
               vi,
               random=randomEffect,
               data=D3)
MBin <- rma.mv(yi ~ xCycHighDose + isTaxane + xCycHighDose * isTaxane,
               vi,
               random=randomEffect,
               data=D3)
M <- list(rmaLin = MLin,
          rmaBin = MBin)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.3088  0.5557     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 91.0476, p-val = 0.0009
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 13.2276, p-val = 0.0042
## 
## Model Results:
## 
##                    estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt             -7.9451  0.2929  -27.1238  <.0001  -8.5192  -7.3710
## xCyc                 0.2134  0.0591    3.6079  0.0003   0.0975   0.3293
## isTaxaneTRUE         0.6002  0.9068    0.6619  0.5080  -1.1771   2.3776
## xCyc:isTaxaneTRUE   -0.1863  0.3643   -0.5115  0.6090  -0.9003   0.5276
##                       
## intrcpt            ***
## xCyc               ***
## isTaxaneTRUE          
## xCyc:isTaxaneTRUE     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.6319  0.7949     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 129.1587, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 1.8215, p-val = 0.6103
## 
## Model Results:
## 
##                                estimate      se      zval    pval    ci.lb
## intrcpt                         -7.7396  0.7705  -10.0443  <.0001  -9.2498
## xCycHighDoseTRUE                 0.5540  0.7759    0.7140  0.4752  -0.9667
## isTaxaneTRUE                    -0.0700  0.9602   -0.0729  0.9419  -1.9520
## xCycHighDoseTRUE:isTaxaneTRUE    0.1594  1.0054    0.1586  0.8740  -1.8110
##                                  ci.ub     
## intrcpt                        -6.2293  ***
## xCycHighDoseTRUE                2.0746     
## isTaxaneTRUE                    1.8120     
## xCycHighDoseTRUE:isTaxaneTRUE   2.1299     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Next, the model is reparameterized as

$$\frac{y_i}{\text{py}_i} = \beta_0 + \beta_1^\prime x_{\text{cyclophosphamide dose}, i} + \beta_2^\prime I_{\text{no taxane}, i} + \beta_3^\prime x_{\text{cyclophosphamide dose}, i} I_{\text{no taxane}, i} + \sigma_\text{study}$$

so the interpretation of $\beta_1^\prime$ is the effect of cyclophosphamide dose among the studies with taxane in the regimen.


```r
MLin <- rma.mv(yi ~ xCyc + (isTaxane == FALSE) + xCyc * (isTaxane == FALSE),
               vi,
               random=randomEffect,
               data=D3)
MBin <- rma.mv(yi ~ xCycHighDose + (isTaxane == FALSE) + xCycHighDose * (isTaxane == FALSE),
               vi,
               random=randomEffect,
               data=D3)
M <- list(rmaLin = MLin,
          rmaBin = MBin)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.3088  0.5557     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 91.0476, p-val = 0.0009
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 13.2276, p-val = 0.0042
## 
## Model Results:
## 
##                             estimate      se     zval    pval    ci.lb
## intrcpt                      -7.3449  0.8902  -8.2509  <.0001  -9.0896
## xCyc                          0.0270  0.3613   0.0748  0.9403  -0.6811
## isTaxane == FALSETRUE        -0.6002  0.9068  -0.6619  0.5080  -2.3776
## xCyc:isTaxane == FALSETRUE    0.1863  0.3643   0.5115  0.6090  -0.5276
##                               ci.ub     
## intrcpt                     -5.6001  ***
## xCyc                         0.7352     
## isTaxane == FALSETRUE        1.1771     
## xCyc:isTaxane == FALSETRUE   0.9003     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 57; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.6319  0.7949     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 129.1587, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 1.8215, p-val = 0.6103
## 
## Model Results:
## 
##                                         estimate      se      zval    pval
## intrcpt                                  -7.8096  0.5935  -13.1588  <.0001
## xCycHighDoseTRUE                          0.7134  0.6290    1.1341  0.2567
## isTaxane == FALSETRUE                     0.0700  0.9602    0.0729  0.9419
## xCycHighDoseTRUE:isTaxane == FALSETRUE   -0.1594  1.0054   -0.1586  0.8740
##                                           ci.lb    ci.ub     
## intrcpt                                 -8.9728  -6.6464  ***
## xCycHighDoseTRUE                        -0.5195   1.9463     
## isTaxane == FALSETRUE                   -1.8120   1.9520     
## xCycHighDoseTRUE:isTaxane == FALSETRUE  -2.1299   1.8110     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


### Taxanes, with and without Cyclophosphamide

The effect of taxane could be due to cyclophosphamide in the regimen.

Fit the meta-regression model

$$\frac{y_i}{\text{py}_i} = \beta_0 + \beta_1 x_{\text{taxane dose}, i} + \beta_2 I_{\text{cyclophosphamide}, i} + \beta_3 x_{\text{taxane dose}, i} I_{\text{cyclophosphamide}, i} + \sigma_\text{study}$$

where $I_{\text{cyclophosphamide}, i}$ is an 0/1 indicator for whether study $i$ has cyclophosphamide in the regimen.
The interpretation of $\beta_1$ is the effect of taxane dose among the studies with no cyclophosphamide in the regimen.


```r
mal <- "AML or MDS"
D3 <- D2[malType == mal]
D3 <- D3[, drug := "Taxane"]
D3 <- D3[isTaxane == FALSE, xTaxHighDose := FALSE]
D3 <- D3[!(is.na(xTax) | is.na(xTaxHighDose) | is.na(rate) | is.na(nITT) | is.na(malType))]
xData <- unique(D3[, .(drug, xTax, xTaxHighDose, isCyclo, malType)])
D3 <- escalc("IRLN", xi=malN, ti=py, data=D3)
randomEffect <- list(~ 1 | id)
MLin <- rma.mv(yi ~ xTax + isCyclo + xTax * isCyclo,
               vi,
               random=randomEffect,
               data=D3)
MBin <- rma.mv(yi ~ xTaxHighDose + isCyclo + xTaxHighDose * isCyclo,
               vi,
               random=randomEffect,
               data=D3)
M <- list(rmaLin = MLin,
          rmaBin = MBin)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2450  0.4950     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 23) = 27.4468, p-val = 0.2374
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 4.2772, p-val = 0.2330
## 
## Model Results:
## 
##                   estimate      se     zval    pval    ci.lb    ci.ub     
## intrcpt            -7.0009  1.1275  -6.2093  <.0001  -9.2107  -4.7910  ***
## xTax               -0.2287  0.3018  -0.7576  0.4487  -0.8203   0.3629     
## isCycloTRUE        -1.5096  1.3259  -1.1386  0.2549  -4.1083   1.0891     
## xTax:isCycloTRUE    0.4203  0.3208   1.3101  0.1902  -0.2085   1.0490     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2074  0.4554     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 23) = 25.4274, p-val = 0.3286
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 4.9601, p-val = 0.1747
## 
## Model Results:
## 
##                               estimate      se      zval    pval    ci.lb
## intrcpt                        -7.6999  0.5210  -14.7783  <.0001  -8.7211
## xTaxHighDoseTRUE               -0.3292  1.5744   -0.2091  0.8344  -3.4151
## isCycloTRUE                    -0.0492  0.5954   -0.0827  0.9341  -1.2162
## xTaxHighDoseTRUE:isCycloTRUE    1.0174  1.6090    0.6323  0.5272  -2.1361
##                                 ci.ub     
## intrcpt                       -6.6787  ***
## xTaxHighDoseTRUE               2.7566     
## isCycloTRUE                    1.1177     
## xTaxHighDoseTRUE:isCycloTRUE   4.1709     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Next, the model is reparameterized as

$$\frac{y_i}{\text{py}_i} = \beta_0 + \beta_1^\prime x_{\text{taxane dose}, i} + \beta_2^\prime I_{\text{no cyclophosphamide}, i} + \beta_3^\prime x_{\text{taxane dose}, i} I_{\text{no cyclophosphamide}, i} + \sigma_\text{study}$$

so the interpretation of $\beta_1^\prime$ is the effect of taxane dose among the studies with cyclophosphamide in the regimen.


```r
MLin <- rma.mv(yi ~ xTax + (isCyclo == FALSE) + xTax * (isCyclo == FALSE),
               vi,
               random=randomEffect,
               data=D3)
MBin <- rma.mv(yi ~ xTaxHighDose + (isCyclo == FALSE) + xTaxHighDose * (isCyclo == FALSE),
               vi,
               random=randomEffect,
               data=D3)
M <- list(rmaLin = MLin,
          rmaBin = MBin)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2450  0.4950     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 23) = 27.4468, p-val = 0.2374
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 4.2772, p-val = 0.2330
## 
## Model Results:
## 
##                            estimate      se      zval    pval    ci.lb
## intrcpt                     -8.5105  0.6977  -12.1981  <.0001  -9.8779
## xTax                         0.1916  0.1086    1.7641  0.0777  -0.0213
## isCyclo == FALSETRUE         1.5096  1.3259    1.1386  0.2549  -1.0891
## xTax:isCyclo == FALSETRUE   -0.4203  0.3208   -1.3101  0.1902  -1.0490
##                              ci.ub     
## intrcpt                    -7.1430  ***
## xTax                        0.4044    .
## isCyclo == FALSETRUE        4.1083     
## xTax:isCyclo == FALSETRUE   0.2085     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 27; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2074  0.4554     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 23) = 25.4274, p-val = 0.3286
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 4.9601, p-val = 0.1747
## 
## Model Results:
## 
##                                        estimate      se      zval    pval
## intrcpt                                 -7.7491  0.2881  -26.8934  <.0001
## xTaxHighDoseTRUE                         0.6882  0.3316    2.0755  0.0379
## isCyclo == FALSETRUE                     0.0492  0.5954    0.0827  0.9341
## xTaxHighDoseTRUE:isCyclo == FALSETRUE   -1.0174  1.6090   -0.6323  0.5272
##                                          ci.lb    ci.ub     
## intrcpt                                -8.3138  -7.1843  ***
## xTaxHighDoseTRUE                        0.0383   1.3380    *
## isCyclo == FALSETRUE                   -1.1177   1.2162     
## xTaxHighDoseTRUE:isCyclo == FALSETRUE  -4.1709   2.1361     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## AML only

Run models for AML malignancies alone for compatibility with the SEER incidence statistics.

> From: Rivera, Donna (NIH/NCI) [F] [mailto:donna.rivera@nih.gov]   
> Sent: Monday, June 27, 2016 6:37 AM  
> To: Benjamin Chan; Barbara M. Galligan  
> Subject: RE: SEER Data for NCI secondary malignancies  
>
> I just wanted to make sure you saw the **SEER statistics are only for AML and do not include MDS**. MDS is categorized separately and we could get that as well for reference if you would like.  There are other types such as AMoL and more rare versions ,if  you would like to look at those. Let me know if there are other types needed for your reference.



```r
mal <- "AML"
D3 <- D2[malType == mal]
```



```r
D3 <- D3[, `:=` (drug = "Cyclophosphamide", x = xCyc, xHighDose = xCycHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 34; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1618  0.4022     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 32) = 35.0079, p-val = 0.3272
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 18.4207, p-val < .0001
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -8.3904  0.2984  -28.1174  <.0001  -8.9753  -7.8056  ***
## x          0.2511  0.0585    4.2919  <.0001   0.1364   0.3657  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 34; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.6177  0.7859     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 32) = 65.9868, p-val = 0.0004
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.3437, p-val = 0.2464
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -7.9959  0.5622  -14.2226  <.0001  -9.0978  -6.8940  ***
## xHighDoseTRUE    0.6528  0.5631    1.1592  0.2464  -0.4509   1.7565     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p < 0.001"
## 
## $pvalueBin
## [1] "p = 0.246"
## 
## $pred
##                 drug    x xHighDose malType   predLin confLowerLin
##  1: Cyclophosphamide 1.20     FALSE     AML  3.068473     1.222659
##  2: Cyclophosphamide 1.50     FALSE     AML  3.308517     1.335238
##  3: Cyclophosphamide 2.00     FALSE     AML  3.751042     1.542429
##  4: Cyclophosphamide 2.40      TRUE     AML  4.147312     1.726922
##  5: Cyclophosphamide 3.00      TRUE     AML  4.821574     2.037187
##  6: Cyclophosphamide 3.60      TRUE     AML  5.605457     2.390445
##  7: Cyclophosphamide 4.00      TRUE     AML  6.197632     2.651275
##  8: Cyclophosphamide 4.80      TRUE     AML  7.576268     3.237314
##  9: Cyclophosphamide 6.64      TRUE     AML 12.024952     4.940280
## 10: Cyclophosphamide 7.20      TRUE     AML 13.840253     5.565871
## 11: Cyclophosphamide 8.10      TRUE     AML 17.349114     6.687134
## 12: Cyclophosphamide 9.60      TRUE     AML 25.283240     8.908889
##     confUpperLin  predBin confLowerBin confUpperBin scale
##  1:     7.700861 3.368346    0.5068721     22.38386 10000
##  2:     8.198001 3.368346    0.5068721     22.38386 10000
##  3:     9.122180 3.368346    0.5068721     22.38386 10000
##  4:     9.960026 6.470141    1.2884067     32.49185 10000
##  5:    11.411606 6.470141    1.2884067     32.49185 10000
##  6:    13.144476 6.470141    1.2884067     32.49185 10000
##  7:    14.487614 6.470141    1.2884067     32.49185 10000
##  8:    17.730701 6.470141    1.2884067     32.49185 10000
##  9:    29.269489 6.470141    1.2884067     32.49185 10000
## 10:    34.415569 6.470141    1.2884067     32.49185 10000
## 11:    45.010578 6.470141    1.2884067     32.49185 10000
## 12:    71.753306 6.470141    1.2884067     32.49185 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Cyclophosphamide cumulative dose",
        1e3 * c(0.5, 1, 2, 4, 8, 16),
        1e3)
```

```
## Saving 7 x 5 in image
```

```
##                           size               mtime
## AML_Cyclophosphamide.png 44723 2016-07-21 16:19:47
## AML_Cyclophosphamide.csv  4244 2016-07-21 16:19:47
```

![](metaregSecMal_files/figure-html/unnamed-chunk-32-1.png)<!-- -->



```r
D3 <- D3[, `:=` (drug = "Taxane", x = xTax, xHighDose = xTaxHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 8; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.0000  0.0000      7     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 6) = 1.9360, p-val = 0.9255
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.0440, p-val = 0.8339
## 
## Model Results:
## 
##          estimate      se     zval    pval    ci.lb    ci.ub     
## intrcpt   -7.6963  1.0564  -7.2856  <.0001  -9.7667  -5.6258  ***
## x         -0.0469  0.2235  -0.2097  0.8339  -0.4849   0.3912     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 8; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.0000  0.0000      7     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 6) = 1.6490, p-val = 0.9490
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.3310, p-val = 0.5651
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -7.8615  0.2722  -28.8851  <.0001  -8.3950  -7.3281  ***
## xHighDoseTRUE   -0.4952  0.8607   -0.5753  0.5651  -2.1820   1.1917     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.834"
## 
## $pvalueBin
## [1] "p = 0.565"
## 
## $pred
##      drug   x xHighDose malType  predLin confLowerLin confUpperLin
## 1: Taxane 3.0     FALSE     AML 3.949141    1.6735475     9.318956
## 2: Taxane 4.0     FALSE     AML 3.768340    2.1376866     6.642876
## 3: Taxane 4.5     FALSE     AML 3.681068    2.2162744     6.113981
## 4: Taxane 7.5      TRUE     AML 3.198278    0.8093204    12.638974
## 5: Taxane 8.0      TRUE     AML 3.124208    0.6436008    15.165725
##     predBin confLowerBin confUpperBin scale
## 1: 3.852856    2.2600347     6.568263 10000
## 2: 3.852856    2.2600347     6.568263 10000
## 3: 3.852856    2.2600347     6.568263 10000
## 4: 2.348185    0.4739464    11.634173 10000
## 5: 2.348185    0.4739464    11.634173 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Taxane cumulative dose",
        1e2 * c(0.5, 1, 2, 4, 8, 16),
        1e2)
```

```
## Saving 7 x 5 in image
```

```
##                 size               mtime
## AML_Taxane.png 28555 2016-07-21 16:19:48
## AML_Taxane.csv  1037 2016-07-21 16:19:48
```

![](metaregSecMal_files/figure-html/unnamed-chunk-33-1.png)<!-- -->


## Non-Breast Solid

* Fit the models
* Show model summaries
* Plot the predicted values



```r
mal <- "Non-Breast Solid"
D3 <- D2[malType == mal]
```



```r
D3 <- D3[, `:=` (drug = "Cyclophosphamide", x = xCyc, xHighDose = xCycHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 32; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1441  0.3796     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 30) = 66.2764, p-val = 0.0002
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.0007, p-val = 0.9783
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -5.7939  0.2117  -27.3646  <.0001  -6.2089  -5.3790  ***
## x          0.0012  0.0455    0.0272  0.9783  -0.0880   0.0905     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 32; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1020  0.3194     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 30) = 57.9775, p-val = 0.0016
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 3.0350, p-val = 0.0815
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -6.2623  0.2999  -20.8830  <.0001  -6.8500  -5.6745  ***
## xHighDoseTRUE    0.5279  0.3030    1.7421  0.0815  -0.0660   1.1218    .
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.978"
## 
## $pvalueBin
## [1] "p = 0.081"
## 
## $pred
##                 drug     x xHighDose          malType  predLin
##  1: Cyclophosphamide  1.20     FALSE Non-Breast Solid 30.50488
##  2: Cyclophosphamide  1.50     FALSE Non-Breast Solid 30.51623
##  3: Cyclophosphamide  2.00     FALSE Non-Breast Solid 30.53515
##  4: Cyclophosphamide  2.40      TRUE Non-Breast Solid 30.55029
##  5: Cyclophosphamide  3.00      TRUE Non-Breast Solid 30.57303
##  6: Cyclophosphamide  3.60      TRUE Non-Breast Solid 30.59577
##  7: Cyclophosphamide  4.00      TRUE Non-Breast Solid 30.61095
##  8: Cyclophosphamide  4.80      TRUE Non-Breast Solid 30.64132
##  9: Cyclophosphamide  6.64      TRUE Non-Breast Solid 30.71130
## 10: Cyclophosphamide  7.20      TRUE Non-Breast Solid 30.73263
## 11: Cyclophosphamide  8.10      TRUE Non-Breast Solid 30.76693
## 12: Cyclophosphamide 14.40      TRUE Non-Breast Solid 31.00817
##     confLowerLin confUpperLin  predBin confLowerBin confUpperBin scale
##  1:    13.504562     68.90617 19.06925     8.080596     45.00116 10000
##  2:    13.608859     68.42896 19.06925     8.080596     45.00116 10000
##  3:    13.758978     67.76632 19.06925     8.080596     45.00116 10000
##  4:    13.856433     67.35647 32.32889    16.688009     62.62924 10000
##  5:    13.962622     66.94372 32.32889    16.688009     62.62924 10000
##  6:    14.018743     66.77499 32.32889    16.688009     62.62924 10000
##  7:    14.027673     66.79869 32.32889    16.688009     62.62924 10000
##  8:    13.977066     67.17366 32.32889    16.688009     62.62924 10000
##  9:    13.534426     69.68776 32.32889    16.688009     62.62924 10000
## 10:    13.320399     70.90585 32.32889    16.688009     62.62924 10000
## 11:    12.913841     73.30152 32.32889    16.688009     62.62924 10000
## 12:     9.117955    105.45199 32.32889    16.688009     62.62924 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Cyclophosphamide cumulative dose",
        1e3 * c(0.5, 1, 2, 4, 8, 16),
        1e3)
```

```
## Saving 7 x 5 in image
```

```
##                                      size               mtime
## NonBreastSolid_Cyclophosphamide.png 37953 2016-07-21 16:19:49
## NonBreastSolid_Cyclophosphamide.csv  4530 2016-07-21 16:19:49
```

![](metaregSecMal_files/figure-html/unnamed-chunk-35-1.png)<!-- -->



```r
D3 <- D3[, `:=` (drug = "Taxane", x = xTax, xHighDose = xTaxHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 13; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1613  0.4016      8     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 11) = 29.6283, p-val = 0.0018
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.1777, p-val = 0.6733
## 
## Model Results:
## 
##          estimate      se     zval    pval    ci.lb    ci.ub     
## intrcpt   -6.0630  0.6630  -9.1447  <.0001  -7.3625  -4.7635  ***
## x          0.0468  0.1110   0.4216  0.6733  -0.1707   0.2643     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 13; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1426  0.3776      8     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 11) = 28.1761, p-val = 0.0030
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.7789, p-val = 0.3775
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -5.9390  0.2443  -24.3128  <.0001  -6.4178  -5.4603  ***
## xHighDoseTRUE    0.3056  0.3462    0.8826  0.3775  -0.3730   0.9841     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.673"
## 
## $pvalueBin
## [1] "p = 0.377"
## 
## $pred
##      drug   x xHighDose          malType  predLin confLowerLin
## 1: Taxane 4.0     FALSE Non-Breast Solid 28.06250     10.92877
## 2: Taxane 4.5     FALSE Non-Breast Solid 28.72660     11.62530
## 3: Taxane 7.0      TRUE Non-Breast Solid 32.29043     13.06744
## 4: Taxane 7.5      TRUE Non-Breast Solid 33.05458     12.87278
## 5: Taxane 8.0      TRUE Non-Breast Solid 33.83682     12.54870
##    confUpperLin  predBin confLowerBin confUpperBin scale
## 1:     72.05784 26.34603     10.91134     63.61393 10000
## 2:     70.98462 26.34603     10.91134     63.61393 10000
## 3:     79.79158 35.76138     14.79403     86.44543 10000
## 4:     84.87719 35.76138     14.79403     86.44543 10000
## 5:     91.23898 35.76138     14.79403     86.44543 10000
```

```r
plotreg(M,
        D3,
        mal,
        "Taxane cumulative dose",
        1e2 * c(0.5, 1, 2, 4, 8, 16),
        1e2)
```

```
## Saving 7 x 5 in image
```

```
##                            size               mtime
## NonBreastSolid_Taxane.png 27729 2016-07-21 16:19:50
## NonBreastSolid_Taxane.csv  1776 2016-07-21 16:19:50
```

![](metaregSecMal_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

# Study Characteristics and Outcomes

Populate Appendix Table 1.
Need columns for

* Study
* Country
* Median follow-up (months)
* Regimen name
* Cumulative dose
    * Anthracyclines
    * Cyclophosphamide
    * Taxanes
* N
* Person-years (the denominator for the incidence rate)
* AML/MDS count (the numerator for the incidence rate)
* AML/MDS incidence, per 10,000 person-years
* Non-breast solid count (the numerator for the incidence rate)
* Non-breast solid incidence, per 10,000 person-years

Output to an

* Excel workbook, [appendixTableStudyCharacteristicsAndOutcomes.xlsx](appendixTableStudyCharacteristicsAndOutcomes.xlsx)
* CSV file, [appendixTableStudyCharacteristicsAndOutcomes.csv](appendixTableStudyCharacteristicsAndOutcomes.csv)



```r
library(xlsx)
```

```
## Loading required package: rJava
```

```
## Loading required package: xlsxjars
```

```
## 
## Attaching package: 'xlsx'
```

```
## The following objects are masked from 'package:openxlsx':
## 
##     createWorkbook, loadWorkbook, read.xlsx, saveWorkbook,
##     write.xlsx
```

```r
library(IRdisplay)
library(xtable)
```

```
## 
## Attaching package: 'xtable'
```

```
## The following object is masked from 'package:IRdisplay':
## 
##     display
```

```r
T <- D[,
       .(study = ifelse(!is.na(trial), paste(authorYear, trial), authorYear),
         country = NA,
         medianFU = round(medianFU),
         regimen,
         anthrCumulDose = round(anthracyclineCumulDose),
         cycloCumulaDose = round(cyclophosphamideCumulDose),
         taxaneCumulDose = round(taxaneCumulDose),
         nITT,
         py = nITT * (medianFU / 12),
         nAMLOrMDS = malAMLOrMDSTotal,
         incidenceAMLOrMDS = signif(malAMLOrMDSTotal / (nITT * (medianFU / 12)) * scale, digits=3),
         nNonBreastSolid = malNonBreastSolid,
         incidenceNonBreastSolid = signif(malNonBreastSolid / (nITT * (medianFU / 12)) * scale, digits=3))]
write.xlsx(T,
           "appendixTableStudyCharacteristicsAndOutcomes.xlsx",
           row.names=FALSE,
           showNA=FALSE)
write.csv(T,
          "appendixTableStudyCharacteristicsAndOutcomes.csv",
          quote=FALSE,
          row.names=FALSE)
file.info(grep("appendixTableStudyCharacteristicsAndOutcomes", list.files(), value=TRUE))[c("size", "mtime")]
```

```
##                                                   size               mtime
## appendixTableStudyCharacteristicsAndOutcomes.csv  7307 2016-07-21 16:19:54
## appendixTableStudyCharacteristicsAndOutcomes.xlsx 9491 2016-07-21 16:19:54
```

```r
T <- xtable(T, digits=c(rep(0, ncol(T) - 3), 0, 2, 0, 2))
# display_html(paste(capture.output(print(T, type = 'html')), collapse="", sep=""))
```

# Zip up assets

Zip up assets created by this notebook for transfer off of EC2.



```r
f <- "assets.zip"
assets <- grep("(csv)|(png)|(xlsx)|(md)$", list.files(), value=TRUE)
zip(f, assets)
file.info(f)[c("size", "mtime")]
unzip(f, list=TRUE)
```

# Session information



```r
sessionInfo()
```

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] tools     stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] xtable_1.8-2       IRdisplay_0.4.2    xlsx_0.5.7        
##  [4] xlsxjars_0.6.1     rJava_0.9-8        RColorBrewer_1.1-2
##  [7] ggplot2_2.1.0      metafor_1.9-8      Matrix_1.2-6      
## [10] data.table_1.9.6   openxlsx_3.0.0    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.6      knitr_1.13       magrittr_1.5     munsell_0.4.3   
##  [5] colorspace_1.2-6 lattice_0.20-33  stringr_1.0.0    plyr_1.8.4      
##  [9] grid_3.3.1       gtable_0.2.0     htmltools_0.3.5  yaml_2.1.13     
## [13] digest_0.6.9     repr_0.7         formatR_1.4      evaluate_0.9    
## [17] rmarkdown_1.0    stringi_1.1.1    scales_0.4.0     chron_2.3-47
```
