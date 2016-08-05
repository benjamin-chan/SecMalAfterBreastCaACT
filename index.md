# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy

* Author: [Benjamin Chan](http://careers.stackoverflow.com/benjaminchan)
* Date: 2016-08-05 15:40:28


# Load packages


```r
library(openxlsx)
library(data.table)
```

---

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
##                                    size               mtime
## /tmp/Rtmpu5Cg98/file1dcbf45103b33 59619 2016-08-05 15:40:30
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
##  1:    38      1     NA          NA                1
##  2:    61      1     NA          NA                1
##  3:     4     NA     NA          NA               NA
##  4:    34      0      1          NA                1
##  5:    40      2     NA          NA                2
##  6:    31      6      2          NA                8
##  7:    86     NA     NA           5                5
##  8:    21     NA     NA          NA               NA
##  9:    44     NA     NA           1                1
## 10:    49     NA     NA          NA               NA
## 11:    27     NA     NA          NA               NA
## 12:    96     NA     NA          NA               NA
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
D <- D[, regimen := gsub("\\r*\\n", " ", regimen)]
doseVar <- grep("dose", names(D), ignore.case = TRUE, value = TRUE)
for (i in 1:length(doseVar)) {
    D <- D[, doseVar[i] := gsub("\\r*\\n", " ", get(doseVar[i]))]
}
cat(kable(D[, .N, c("regimen", doseVar), with = TRUE][order(regimen)]),
    file = "regimens.md",
    sep = "\n")
file.info("regimens.md")
```

```
##             size isdir mode               mtime               ctime
## regimens.md 7215 FALSE  644 2016-08-05 15:40:30 2016-08-05 15:40:30
##                           atime  uid  gid uname   grname
## regimens.md 2016-08-01 15:33:32 4051 3010 chanb HPCUsers
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
##          authorYear regimen fluoroucilCumulDose fluoroucilTotalDose
## 1:    Misset (1996)    AVCF               19200                1600
## 2: Venturini (2005)     FEC                3600                 600
## 3: Venturini (2005)     FEC                3600                 600
## 4:    Martin (2010)     FAC                3000                 500
## 5:     Roche (2006)     FEC                3000                 500
## 6:     Roche (2006)   FEC-T                1500                 500
##    fluoroucilCourses
## 1:                12
## 2:                 6
## 3:                 6
## 4:                 6
## 5:                 6
## 6:                 3
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


Save data.table to file.


```r
f <- "data.RData"
save(D, file = f)
file.info(f)[c("size", "mtime")]
```

```
##            size               mtime
## data.RData 7448 2016-08-05 15:40:30
```

---

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
cat(kable(summaryRegimens),
    file = "summaryRegimens.md",
    sep = "\n")
file.info("summaryRegimens.md")
```

```
##                    size isdir mode               mtime               ctime
## summaryRegimens.md 4624 FALSE  644 2016-08-05 15:40:30 2016-08-05 15:40:30
##                                  atime  uid  gid uname   grname
## summaryRegimens.md 2016-08-01 15:33:32 4051 3010 chanb HPCUsers
```

---

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

Output to multiple formats:

* Excel workbook, [appendixTableStudyCharacteristicsAndOutcomes.xlsx](appendixTableStudyCharacteristicsAndOutcomes.xlsx)
* CSV file, [appendixTableStudyCharacteristicsAndOutcomes.csv](appendixTableStudyCharacteristicsAndOutcomes.csv)
* Markdown file, [appendixTableStudyCharacteristicsAndOutcomes.md](appendixTableStudyCharacteristicsAndOutcomes.md)



```r
library(xlsx)
```

```
## Loading required package: rJava
```

```
## Loading required package: methods
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
library(xtable)
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
cat(kable(T),
    file = "appendixTableStudyCharacteristicsAndOutcomes.md",
    sep = "\n")
file.info(grep("appendixTableStudyCharacteristicsAndOutcomes", list.files(), value=TRUE))[c("size", "mtime")]
```

```
##                                                    size
## appendixTableStudyCharacteristicsAndOutcomes.csv   7205
## appendixTableStudyCharacteristicsAndOutcomes.md   20580
## appendixTableStudyCharacteristicsAndOutcomes.xlsx  9473
##                                                                 mtime
## appendixTableStudyCharacteristicsAndOutcomes.csv  2016-08-05 15:40:32
## appendixTableStudyCharacteristicsAndOutcomes.md   2016-08-05 15:40:32
## appendixTableStudyCharacteristicsAndOutcomes.xlsx 2016-08-05 15:40:32
```

```r
T <- xtable(T, digits=c(rep(0, ncol(T) - 3), 0, 2, 0, 2))
```

---

# Meta-regression

Estimate meta-regression models for log transformed incidence rate.

Two outcomes are modeled separately.

1. AML or MDS secondary malignancies
1. Non-breast solid secondary malignancies

Cumulative doses are dichotomized into low and high doses.

* Cyclophosphamide
    * $\lt 2400$
    * $\ge 2400$
* Taxane
    * $\lt 500$
    * $\ge 500$


```r
xscale <- 1e3
D2 <- D1[,
         .(id = factor(id),
           authorYear,
           isCyclo,
           xCyc = cyclophosphamideCumulDose / xscale,  # scale units
           isCycHighDose = cyclophosphamideCumulDose >= 2400,
           xTax = taxaneCumulDose / 1e2,  # scale units
           isTaxHighDose = ifelse(taxaneCumulDose >= 500 & isTaxane == TRUE, TRUE, FALSE),
           isAnthra,
           isTaxane,
           isFluoro,
           nITT,
           medianFU,
           malType,
           malN,
           py,
           rate)]
D2 <- D2[isTaxane == FALSE, isTaxHighDose := FALSE]
D2 <- D2[!(is.na(xCyc) | is.na(isCycHighDose) | is.na(isTaxHighDose))]
```

The effect of cyclophosphamide could be due to high dose taxanes in the regimen.

> From: Joy Melnikow [mailto:jamelnikow@ucdavis.edu]   
> Sent: Monday, August 01, 2016 2:47 PM  
> To: Benjamin Chan <chanb@ohsu.edu>  
> Subject: RE: secondary malignancies  
> 
> The one I'm asking about is cyclophosphamide with high dose taxane vs
> cyclophosphamide alone.  I think if it looks like taxane augments the known
> risk of cyclophosphamide this would be an important addition to the
> literature.

Four model are estimated.

1. $$\frac{y_i}{t_i} = \beta_0 + \beta_1 x_{\text{cyclophosphamide dose}, i} + \beta_2 I_{\text{high dose taxane}, i} + \sigma_\text{study}$$
1. $$\frac{y_i}{t_i} = \beta_0 + \beta_1 x_{\text{cyclophosphamide dose}, i} + \beta_2 I_{\text{high dose taxane}, i} + \gamma x_{\text{cyclophosphamide dose}, i} I_{\text{high dose taxane}, i} + \sigma_\text{study}$$
1. $$\frac{y_i}{t_i} = \beta_0 + \beta_1 I_{\text{high dose cyclophosphamide}, i} + \beta_2 I_{\text{high dose taxane}, i} + \sigma_\text{study}$$
1. $$\frac{y_i}{t_i} = \beta_0 + \beta_1 I_{\text{high dose cyclophosphamide}, i} + \beta_2 I_{\text{high dose taxane}, i} + \gamma I_{\text{high dose cyclophosphamide}, i} I_{\text{high dose taxane}, i} + \sigma_\text{study}$$

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
  xData <- unique(D[, .(xCyc, isCycHighDose, isTaxHighDose, malType)])
  D <- escalc("IRLN", xi = malN, ti = py, data = D)
  randomEffect <- list(~ 1 | id)
  MLin <- rma.mv(yi ~ xCyc + isTaxHighDose,
                 vi,
                 random = randomEffect,
                 data = D)
  MLinInt <- rma.mv(yi ~ xCyc + isTaxHighDose + xCyc * isTaxHighDose,
                    vi,
                    random = randomEffect,
                    data = D)
  MBin <- rma.mv(yi ~ isCycHighDose + isTaxHighDose,
                 vi,
                 random = randomEffect,
                 data = D)
  MBinInt <- rma.mv(yi ~ isCycHighDose + isTaxHighDose + isCycHighDose * isTaxHighDose,
                    vi,
                    random = randomEffect,
                    data = D)
  list(rmaLin = MLin,
       rmaLinInt = MLinInt,
       rmaBin = MBin,
       rmaBinInt = MBinInt)
}
plotreg <- function (M, D, title) {
  require(ggplot2)
  require(RColorBrewer)
  require(data.table)
  X <- data.table(M$X)
  X <- unique(X)
  varnames <- names(X)[2:ncol(X)]
  X0 <- data.table(x1 = seq(min(X[isTaxHighDoseTRUE == 0, xCyc]), max(X[isTaxHighDoseTRUE == 0, xCyc]),
                            length.out = 100),
                   x2 = rep(0, 100))
  X1 <- data.table(x1 = seq(min(X[isTaxHighDoseTRUE == 1, xCyc]), max(X[isTaxHighDoseTRUE == 1, xCyc]),
                            length.out = 100),
                   x2 = rep(1, 100))
  X <- rbind(X0, X1)
  names(X) <- varnames
  X$xCyc <- round(X$xCyc, digits = 1)
  X <- unique(X)
  yhat <- data.table(malType = mal,
                     X,
                     # variable = "pred",
                     pred = predict(M, as.matrix(X), transf = exp)[["pred"]] * scale)
  ci.lb <- data.table(malType = mal,
                      X,
                      # variable = "ci.lb",
                      ci.lb = predict(M, as.matrix(X), transf = exp)[["ci.lb"]] * scale)
  ci.ub <- data.table(malType = mal,
                      X,
                      # variable = "ci.ub",
                      ci.ub = predict(M, as.matrix(X), transf = exp)[["ci.ub"]] * scale)
  keyvar <- c("malType", "xCyc", "isTaxHighDoseTRUE")
  setkeyv(yhat, keyvar)
  setkeyv(ci.lb, keyvar)
  setkeyv(ci.ub, keyvar)
  yhat <- merge(merge(yhat, ci.lb), ci.ub)
  pvalues <- c(M$pval[which(row.names(M$b) == "isTaxHighDoseTRUE")])
  pal <- brewer.pal(5, name = "RdBu")[c(1, 5)]
  G <- ggplot(D, aes(x = xCyc * xscale,
                     y = rate + 1/2,
                     size = nITT / min(nITT, na.rm = TRUE),
                     color = isTaxHighDose))
  G <- G + geom_point(alpha = 1/2,
                      position = "jitter")
  G <- G + geom_line(data = yhat[isTaxHighDoseTRUE == 0, ],
                     aes(x = xCyc * xscale, y = pred),
                     inherit.aes = FALSE,
                     color = pal[1])
  G <- G + geom_line(data = yhat[isTaxHighDoseTRUE == 1, ],
                     aes(x = xCyc * xscale, y = pred),
                     inherit.aes = FALSE,
                     color = pal[2])
  G <- G + geom_ribbon(data = yhat[isTaxHighDoseTRUE == 0, ],
                       aes(x = xCyc * xscale, ymax = ci.ub, ymin = ci.lb),
                       inherit.aes = FALSE,
                       fill = pal[1],
                       alpha = 1/8)
  G <- G + geom_ribbon(data = yhat[isTaxHighDoseTRUE == 1, ],
                       aes(x = xCyc * xscale, ymax = ci.ub, ymin = ci.lb),
                       inherit.aes = FALSE,
                       fill = pal[2],
                       alpha = 1/8)
  G <- G + scale_x_log10("Cyclophosphamide cumulative dose",
                         breaks = 1e3 * c(0.5, 1, 2, 4, 8, 16))
  G <- G + scale_y_log10(sprintf("Rate per %s person-years",
                                 format(scale, big.mark = ",")))
  G <- G + scale_color_manual(sprintf("Taxane\n(%s)", pvalToChar(pvalues[c(1)])),
                              values = pal,
                              labels = c("None or low dose", "High dose"))
  G <- G + scale_size_continuous(guide = FALSE)
  G <- G + labs(title = title)
  G <- G + theme_bw()
  filename <- sprintf("%s_Cyclophosphamide_byHighDoseTaxane",
                      gsub("(\\s)|(-)", "", title))
  ggsave(filename = sprintf("%s.png", filename), width = 9)
  yhat$xCyc <- yhat$xCyc * xscale
  write.csv(D, file = sprintf("%s.csv", filename), row.names = FALSE, quote = FALSE)
  write.csv(yhat, file = sprintf("%s_Pred.csv", filename), row.names = FALSE, quote = FALSE)
  show(file.info(grep(paste0(filename, "(_Pred)*\\."), list.files(), value = TRUE))[c("size", "mtime")])
  G
}
```

## AML/MDS


```r
mal <- "AML or MDS"
D3 <- D2[malType == mal]
D3 <- D3[!(is.na(rate) | is.na(nITT) | is.na(malType))]
M <- metareg(D3)
```

### Findings

* AML or MDS rate had a dose response relationship with cumulative cyclophosphamide dose
  * AML or MDS rate increased 1.17 times (95% CI: 1.05, 1.31; p = 0.0035) for each 1000 $\text{mg} / \text{m}^2$
* High dose taxane confounded the cyclophosphamide dose response
  * High dose taxane increased AML or MDS rate by 1.71 times (95% CI: 1.07, 2.76; p = 0.0264)
* High dose taxane did not modify the dose response effect of cyclophosphamide
  * Interaction term estimate was 0.0149 (95% CI: -1.02, 1.05; p = 0.978)

### Cyclophosphamide: dose response; Taxane: confounder


```r
M$rmaLin
```

```
## 
## Multivariate Meta-Analysis Model (k = 55; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1206  0.3473     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 52) = 58.1475, p-val = 0.2593
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 10.5094, p-val = 0.0052
## 
## Model Results:
## 
##                    estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt             -7.8796  0.2447  -32.1959  <.0001  -8.3593  -7.3999
## xCyc                 0.1611  0.0552    2.9204  0.0035   0.0530   0.2692
## isTaxHighDoseTRUE    0.5386  0.2425    2.2209  0.0264   0.0633   1.0139
##                       
## intrcpt            ***
## xCyc                **
## isTaxHighDoseTRUE    *
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
plotreg(M$rmaLin, D3, mal)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: RColorBrewer
```

```
## Saving 9 x 7 in image
```

```
##                                                       size
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane_Pred.csv   6979
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.csv        5787
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.png      188921
##                                                                   mtime
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane_Pred.csv 2016-08-05 15:40:34
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.csv      2016-08-05 15:40:34
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.png      2016-08-05 15:40:34
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

### Cyclophosphamide: dose response; Taxane: effect modifier


```r
M$rmaLinInt
```

```
## 
## Multivariate Meta-Analysis Model (k = 55; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1264  0.3555     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 51) = 57.9982, p-val = 0.2330
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 10.2679, p-val = 0.0164
## 
## Model Results:
## 
##                         estimate      se      zval    pval    ci.lb
## intrcpt                  -7.8777  0.2469  -31.9122  <.0001  -8.3616
## xCyc                      0.1599  0.0558    2.8640  0.0042   0.0505
## isTaxHighDoseTRUE         0.5031  1.2358    0.4071  0.6839  -1.9191
## xCyc:isTaxHighDoseTRUE    0.0149  0.5303    0.0282  0.9775  -1.0244
##                           ci.ub     
## intrcpt                 -7.3939  ***
## xCyc                     0.2694   **
## isTaxHighDoseTRUE        2.9253     
## xCyc:isTaxHighDoseTRUE   1.0543     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Cyclophosphamide: dichotomous; Taxane: confounder


```r
M$rmaBin
```

```
## 
## Multivariate Meta-Analysis Model (k = 55; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2601  0.5100     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 52) = 80.3700, p-val = 0.0070
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 2.8181, p-val = 0.2444
## 
## Model Results:
## 
##                    estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt             -7.7392  0.4487  -17.2490  <.0001  -8.6186  -6.8599
## isCycHighDoseTRUE    0.4177  0.4453    0.9381  0.3482  -0.4550   1.2904
## isTaxHighDoseTRUE    0.4035  0.2642    1.5274  0.1267  -0.1143   0.9213
##                       
## intrcpt            ***
## isCycHighDoseTRUE     
## isTaxHighDoseTRUE     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Cyclophosphamide: dichotomous; Taxane: effect modifier


```r
M$rmaBinInt
```

```
## 
## Multivariate Meta-Analysis Model (k = 55; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2756  0.5250     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 51) = 80.3564, p-val = 0.0054
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 3.0856, p-val = 0.3786
## 
## Model Results:
## 
##                                      estimate      se      zval    pval
## intrcpt                               -7.5614  0.5739  -13.1758  <.0001
## isCycHighDoseTRUE                      0.2211  0.5874    0.3764  0.7067
## isTaxHighDoseTRUE                     -0.0237  0.8653   -0.0274  0.9782
## isCycHighDoseTRUE:isTaxHighDoseTRUE    0.4790  0.9145    0.5238  0.6004
##                                        ci.lb    ci.ub     
## intrcpt                              -8.6862  -6.4366  ***
## isCycHighDoseTRUE                    -0.9302   1.3723     
## isTaxHighDoseTRUE                    -1.7197   1.6723     
## isCycHighDoseTRUE:isTaxHighDoseTRUE  -1.3133   2.2714     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Non-Breast Solid


```r
mal <- "Non-Breast Solid"
D3 <- D2[malType == mal]
D3 <- D3[!(is.na(rate) | is.na(nITT) | is.na(malType))]
M <- metareg(D3)
```

* Non-Breast Solid rate did not have a dose response relationship with cumulative cyclophosphamide dose
  * Non-Breast Solid rate increased 1.01 times (95% CI: 0.917, 1.11; p = 0.846) for each 1000 $\text{mg} / \text{m}^2$
* High dose taxane did not confound the cyclophosphamide dose response
  * High dose taxane increased Non-Breast Solid rate by 1.37 times (95% CI: 0.799, 2.35; p = 0.253)

### Cyclophosphamide: dose response; Taxane: confounder


```r
M$rmaLin
```

```
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1370  0.3701     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 28) = 60.3828, p-val = 0.0004
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 1.3235, p-val = 0.5160
## 
## Model Results:
## 
##                    estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt             -5.8909  0.2354  -25.0234  <.0001  -6.3523  -5.4295
## xCyc                 0.0096  0.0493    0.1944  0.8459  -0.0870   0.1062
## isTaxHighDoseTRUE    0.3148  0.2752    1.1437  0.2527  -0.2246   0.8542
##                       
## intrcpt            ***
## xCyc                  
## isTaxHighDoseTRUE     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
plotreg(M$rmaLin, D3, mal)
```

```
## Saving 9 x 7 in image
```

```
##                                                             size
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane_Pred.csv   7917
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.csv        3608
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.png      147159
##                                                                         mtime
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane_Pred.csv 2016-08-05 15:40:35
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.csv      2016-08-05 15:40:35
## NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.png      2016-08-05 15:40:35
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

### Cyclophosphamide: dose response; Taxane: effect modifier


```r
M$rmaLinInt
```

```
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1101  0.3318     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 27) = 51.9353, p-val = 0.0027
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 4.7271, p-val = 0.1929
## 
## Model Results:
## 
##                         estimate      se      zval    pval     ci.lb
## intrcpt                  -5.9262  0.2235  -26.5193  <.0001   -6.3642
## xCyc                      0.0137  0.0470    0.2913  0.7708   -0.0785
## isTaxHighDoseTRUE        -4.6163  2.8129   -1.6411  0.1008  -10.1296
## xCyc:isTaxHighDoseTRUE    2.1894  1.2396    1.7662  0.0774   -0.2402
##                           ci.ub     
## intrcpt                 -5.4882  ***
## xCyc                     0.1059     
## isTaxHighDoseTRUE        0.8970     
## xCyc:isTaxHighDoseTRUE   4.6190    .
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Cyclophosphamide: dichotomous; Taxane: confounder


```r
M$rmaBin
```

```
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.0822  0.2867     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 28) = 51.0912, p-val = 0.0049
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 7.9497, p-val = 0.0188
## 
## Model Results:
## 
##                    estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt             -6.5972  0.3296  -20.0147  <.0001  -7.2432  -5.9511
## isCycHighDoseTRUE    0.7851  0.3185    2.4647  0.0137   0.1608   1.4094
## isTaxHighDoseTRUE    0.5320  0.2491    2.1359  0.0327   0.0438   1.0202
##                       
## intrcpt            ***
## isCycHighDoseTRUE    *
## isTaxHighDoseTRUE    *
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Cyclophosphamide: dichotomous; Taxane: effect modifier


```r
M$rmaBinInt
```

```
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.0872  0.2952     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 27) = 51.0178, p-val = 0.0035
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 7.7889, p-val = 0.0506
## 
## Model Results:
## 
##                                      estimate      se      zval    pval
## intrcpt                               -6.5200  0.4346  -15.0023  <.0001
## isCycHighDoseTRUE                      0.6981  0.4421    1.5791  0.1143
## isTaxHighDoseTRUE                      0.3923  0.5885    0.6666  0.5051
## isCycHighDoseTRUE:isTaxHighDoseTRUE    0.1757  0.6572    0.2673  0.7892
##                                        ci.lb    ci.ub     
## intrcpt                              -7.3718  -5.6682  ***
## isCycHighDoseTRUE                    -0.1684   1.5646     
## isTaxHighDoseTRUE                    -0.7612   1.5457     
## isCycHighDoseTRUE:isTaxHighDoseTRUE  -1.1124   1.4638     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
