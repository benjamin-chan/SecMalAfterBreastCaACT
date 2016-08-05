# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy

* Author: [Benjamin Chan](http://careers.stackoverflow.com/benjaminchan)
* Date: 2016-08-05 10:27:20


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
## /tmp/RtmpROx4al/file1bea71699c21e 59619 2016-08-05 10:27:22
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
##  1:    28     NA     NA          NA               NA
##  2:    20      0      0          NA                0
##  3:    21     NA     NA          NA               NA
##  4:    89     NA     NA          NA               NA
##  5:    18     NA     NA           2                2
##  6:    50     NA     NA          NA               NA
##  7:    49     NA     NA          NA               NA
##  8:    41      1     NA          NA                1
##  9:     4     NA     NA          NA               NA
## 10:    66      4     NA          NA                4
## 11:    88     NA     NA          11               11
## 12:    95      0     NA          NA                0
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
## regimens.md 7215 FALSE  644 2016-08-05 10:27:22 2016-08-05 10:27:22
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
## data.RData 7448 2016-08-05 10:27:23
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
## summaryRegimens.md 4624 FALSE  644 2016-08-05 10:27:23 2016-08-05 10:27:23
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
## appendixTableStudyCharacteristicsAndOutcomes.xlsx  9474
##                                                                 mtime
## appendixTableStudyCharacteristicsAndOutcomes.csv  2016-08-05 10:27:25
## appendixTableStudyCharacteristicsAndOutcomes.md   2016-08-05 10:27:25
## appendixTableStudyCharacteristicsAndOutcomes.xlsx 2016-08-05 10:27:25
```

```r
T <- xtable(T, digits=c(rep(0, ncol(T) - 3), 0, 2, 0, 2))
```

---

# Meta-regression

Estimate meta-regression models for log transformed incidence rate.
Model is

$$\frac{y_i}{t_i} = \beta x_i + \sigma_\text{study}$$

Or

$$\frac{y_i}{t_i} = \beta I_{\text{high dose}, i} + \sigma_\text{study}$$

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
  show(file.info(grep(paste0(filename, "(_Pred)*\\."), list.files(), value = TRUE))[c("size", "mtime")])
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


### Cyclophosphamide


```r
D3 <- D3[, `:=` (drug = "Cyclophosphamide", x = xCyc, xHighDose = xCycHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 56; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1693  0.4115     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 54) = 67.8113, p-val = 0.0980
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 4.4383, p-val = 0.0351
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -7.6135  0.2258  -33.7187  <.0001  -8.0560  -7.1709  ***
## x          0.1182  0.0561    2.1067  0.0351   0.0082   0.2281    *
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 56; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2447  0.4946     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 54) = 81.5431, p-val = 0.0091
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.4988, p-val = 0.4800
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -7.5326  0.4261  -17.6799  <.0001  -8.3676  -6.6975  ***
## xHighDoseTRUE    0.3084  0.4367    0.7062  0.4800  -0.5475   1.1642     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.035"
## 
## $pvalueBin
## [1] "p = 0.480"
## 
## $pred
##                 drug    x xHighDose    malType   predLin confLowerLin
##  1: Cyclophosphamide 0.90     FALSE AML or MDS  5.491553     2.267006
##  2: Cyclophosphamide 1.20     FALSE AML or MDS  5.689755     2.371329
##  3: Cyclophosphamide 1.50     FALSE AML or MDS  5.895109     2.477600
##  4: Cyclophosphamide 2.00     FALSE AML or MDS  6.253970     2.658358
##  5: Cyclophosphamide 2.40      TRUE AML or MDS  6.556724     2.805509
##  6: Cyclophosphamide 3.00      TRUE AML or MDS  7.038556     3.028854
##  7: Cyclophosphamide 3.60      TRUE AML or MDS  7.555796     3.253208
##  8: Cyclophosphamide 4.00      TRUE AML or MDS  7.921572     3.402173
##  9: Cyclophosphamide 4.80      TRUE AML or MDS  8.707101     3.695747
## 10: Cyclophosphamide 6.64      TRUE AML or MDS 10.822230     4.329759
## 11: Cyclophosphamide 7.20      TRUE AML or MDS 11.562728     4.507659
## 12: Cyclophosphamide 9.60      TRUE AML or MDS 15.354901     5.182626
##     confUpperLin  predBin confLowerBin confUpperBin scale
##  1:     13.30264 5.353449     1.489197     19.24488 10000
##  2:     13.65197 5.353449     1.489197     19.24488 10000
##  3:     14.02661 5.353449     1.489197     19.24488 10000
##  4:     14.71289 5.353449     1.489197     19.24488 10000
##  5:     15.32365 7.287257     2.658349     19.97636 10000
##  6:     16.35644 7.287257     2.658349     19.97636 10000
##  7:     17.54885 7.287257     2.658349     19.97636 10000
##  8:     18.44448 7.287257     2.658349     19.97636 10000
##  9:     20.51374 7.287257     2.658349     19.97636 10000
## 10:     27.05016 7.287257     2.658349     19.97636 10000
## 11:     29.65989 7.287257     2.658349     19.97636 10000
## 12:     45.49296 7.287257     2.658349     19.97636 10000
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
## Saving 7 x 7 in image
```

```
##                                      size               mtime
## AMLorMDS_Cyclophosphamide_Pred.csv   1845 2016-08-05 10:27:27
## AMLorMDS_Cyclophosphamide.csv        7240 2016-08-05 10:27:27
## AMLorMDS_Cyclophosphamide.png      159211 2016-08-05 10:27:27
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

### Taxane


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
## Saving 7 x 7 in image
```

```
##                            size               mtime
## AMLorMDS_Taxane_Pred.csv   1585 2016-08-05 10:27:27
## AMLorMDS_Taxane.csv        3262 2016-08-05 10:27:27
## AMLorMDS_Taxane.png      118806 2016-08-05 10:27:27
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

### Cyclophosphamide, with and without High Dose Taxane

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


Fit the meta-regression model

$$\frac{y_i}{t_i} = \beta_0 + \beta_1 x_{\text{cyclophosphamide dose}, i} + \beta_2 I_{\text{high dose taxane}, i} + \sigma_\text{study}$$

where $I_{\text{high dose taxane}, i}$ is an 0/1 indicator for whether study $i$ has high dose taxane in the regimen.
The interpretation of $\beta_1$ is the effect of cyclophosphamide dose adjusted for high dose taxane in the regimen.


```r
mal <- "AML or MDS"
D3 <- D2[malType == mal]
D3 <- D3[, drug := "Cyclophosphamide"]
D3 <- D3[isCyclo == FALSE, xCycHighDose := FALSE]
D3 <- D3[, isTaxaneHighDose := FALSE]
D3 <- D3[xTaxHighDose == TRUE, isTaxaneHighDose := TRUE]
D3 <- D3[!(is.na(xCyc) | is.na(xCycHighDose) | is.na(rate) | is.na(nITT) | is.na(malType))]
xData <- unique(D3[, .(drug, xCyc, xCycHighDose, isTaxaneHighDose, malType)])
D3 <- escalc("IRLN", xi=malN, ti=py, data=D3)
randomEffect <- list(~ 1 | id)
MLin <- rma.mv(yi ~ xCyc + isTaxaneHighDose,
               vi,
               random=randomEffect,
               data=D3)
MLinInt <- rma.mv(yi ~ xCyc + isTaxaneHighDose + xCyc * isTaxaneHighDose,
                  vi,
                  random=randomEffect,
                  data=D3)
MBin <- rma.mv(yi ~ xCycHighDose + isTaxaneHighDose,
               vi,
               random=randomEffect,
               data=D3)
M <- list(rmaLin = MLin,
          rmaLinInt = MLinInt,
          rmaBin = MBin)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 56; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1262  0.3552     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 59.4849, p-val = 0.2514
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 10.0425, p-val = 0.0066
## 
## Model Results:
## 
##                       estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt                -7.8593  0.2434  -32.2878  <.0001  -8.3363  -7.3822
## xCyc                    0.1577  0.0554    2.8465  0.0044   0.0491   0.2662
## isTaxaneHighDoseTRUE    0.5261  0.2430    2.1654  0.0304   0.0499   1.0023
##                          
## intrcpt               ***
## xCyc                   **
## isTaxaneHighDoseTRUE    *
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaLinInt
## 
## Multivariate Meta-Analysis Model (k = 56; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1321  0.3635     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 52) = 59.3513, p-val = 0.2254
## 
## Test of Moderators (coefficient(s) 2,3,4): 
## QM(df = 3) = 9.8227, p-val = 0.0201
## 
## Model Results:
## 
##                            estimate      se      zval    pval    ci.lb
## intrcpt                     -7.8579  0.2456  -31.9923  <.0001  -8.3393
## xCyc                         0.1565  0.0561    2.7914  0.0052   0.0466
## isTaxaneHighDoseTRUE         0.4712  1.2386    0.3804  0.7036  -1.9564
## xCyc:isTaxaneHighDoseTRUE    0.0237  0.5319    0.0446  0.9645  -1.0188
##                              ci.ub     
## intrcpt                    -7.3765  ***
## xCyc                        0.2664   **
## isTaxaneHighDoseTRUE        2.8988     
## xCyc:isTaxaneHighDoseTRUE   1.0662     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 56; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.2573  0.5072     26     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 53) = 80.3706, p-val = 0.0090
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 2.8038, p-val = 0.2461
## 
## Model Results:
## 
##                       estimate      se      zval    pval    ci.lb    ci.ub
## intrcpt                -7.7360  0.4480  -17.2685  <.0001  -8.6140  -6.8580
## xCycHighDoseTRUE        0.4187  0.4447    0.9415  0.3464  -0.4529   1.2903
## isTaxaneHighDoseTRUE    0.4006  0.2635    1.5207  0.1283  -0.1157   0.9170
##                          
## intrcpt               ***
## xCycHighDoseTRUE         
## isTaxaneHighDoseTRUE     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Plot predicted values.


```r
X <- data.frame(M$rmaLin$X)
X <- unique(X)
varnames <- names(X)[2:ncol(X)]
X0 <- data.frame(x1 = seq(min(X[X[, 3] == 0, "xCyc"]), max(X[X[, 3] == 0, "xCyc"]),
                          length.out = 100),
                 x2 = rep(0, 100))
X1 <- data.frame(x1 = seq(min(X[X[, 3] == 1, "xCyc"]), max(X[X[, 3] == 1, "xCyc"]),
                          length.out = 100),
                 x2 = rep(1, 100))
X <- rbind(X0, X1)
names(X) <- varnames
X$xCyc <- round(X$xCyc, digits = 1)
X <- unique(X)
yhat <- data.frame(malType = mal,
                   X,
                   yhat = predict(M$rmaLin, as.matrix(X), transf = exp)[["pred"]] * scale)
xscale <- 1e3
pvalues <- c(M$rmaLin$pval[which(row.names(M$rmaLin$b) == "isTaxaneHighDoseTRUE")])
pal <- brewer.pal(4, name = "PuOr")
G <- ggplot(D3, aes(x = xCyc * xscale,
                    y = rate + 1/2,
                    size = nITT / min(nITT, na.rm=TRUE),
                    color = isTaxaneHighDose))
G <- G + geom_point(alpha = 1/2,
                    position = "jitter")
G <- G + geom_line(data = yhat[yhat$isTaxaneHighDose == FALSE, ],
                   aes(x = xCyc * xscale, y = yhat),
                   inherit.aes = FALSE,
                   color = pal[1])
G <- G + geom_line(data = yhat[yhat$isTaxaneHighDose == TRUE, ],
                   aes(x = xCyc * xscale, y = yhat),
                   inherit.aes = FALSE,
                   color = pal[4])
G <- G + scale_x_log10("Cyclophosphamide cumulative dose",
                       breaks = 1e3 * c(0.5, 1, 2, 4, 8, 16))
G <- G + scale_y_log10(sprintf("Rate per %s person-years",
                               format(scale, big.mark = ",")))
G <- G + scale_color_manual(sprintf("Taxane, %s", pvalToChar(pvalues[c(1)])),
                            values = c(pal[1], pal[4]),
                            labels = c("None or low dose", "High dose"))
G <- G + scale_size_continuous(guide = FALSE)
G <- G + labs(title = mal)
G <- G + theme_bw()
filename <- "AMLorMDS_Cyclophosphamide_byHighDoseTaxane"
ggsave(filename = sprintf("%s.png", filename), width = 9)
```

```
## Saving 9 x 7 in image
```

```r
yhat$xCyc <- yhat$xCyc * xscale
write.csv(yhat, file = sprintf("%s_Pred.csv", filename), row.names = FALSE, quote = FALSE)
show(file.info(grep(paste0(filename, "(_Pred)*\\."), list.files(), value = TRUE))[c("size", "mtime")])
```

```
##                                                       size
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane_Pred.csv   3560
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.png      182958
##                                                                   mtime
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane_Pred.csv 2016-08-05 10:27:28
## AMLorMDS_Cyclophosphamide_byHighDoseTaxane.png      2016-08-05 10:27:28
```

```r
G
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)


## Non-Breast Solid

* Fit the models
* Show model summaries
* Plot the predicted values



```r
mal <- "Non-Breast Solid"
D3 <- D2[malType == mal]
```

### Cyclophosphamide


```r
D3 <- D3[, `:=` (drug = "Cyclophosphamide", x = xCyc, xHighDose = xCycHighDose)]
M <- metareg(D3)
M
```

```
## $rmaLin
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1551  0.3939     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 29) = 66.2474, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.0199, p-val = 0.8879
## 
## Model Results:
## 
##          estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt   -5.7765  0.2196  -26.3022  <.0001  -6.2069  -5.3460  ***
## x         -0.0069  0.0488   -0.1410  0.8879  -0.1025   0.0887     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $rmaBin
## 
## Multivariate Meta-Analysis Model (k = 31; method: REML)
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed  factor
## sigma^2    0.1082  0.3289     16     no      id
## 
## Test for Residual Heterogeneity: 
## QE(df = 29) = 57.9772, p-val = 0.0011
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 2.8246, p-val = 0.0928
## 
## Model Results:
## 
##                estimate      se      zval    pval    ci.lb    ci.ub     
## intrcpt         -6.2561  0.3017  -20.7368  <.0001  -6.8473  -5.6648  ***
## xHighDoseTRUE    0.5130  0.3052    1.6806  0.0928  -0.0853   1.1112    .
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## 
## $pvalueLin
## [1] "p = 0.888"
## 
## $pvalueBin
## [1] "p = 0.093"
## 
## $pred
##                 drug     x xHighDose          malType  predLin
##  1: Cyclophosphamide  1.20     FALSE Non-Breast Solid 30.74218
##  2: Cyclophosphamide  1.50     FALSE Non-Breast Solid 30.67884
##  3: Cyclophosphamide  2.00     FALSE Non-Breast Solid 30.57357
##  4: Cyclophosphamide  2.40      TRUE Non-Breast Solid 30.48961
##  5: Cyclophosphamide  3.00      TRUE Non-Breast Solid 30.36411
##  6: Cyclophosphamide  3.60      TRUE Non-Breast Solid 30.23912
##  7: Cyclophosphamide  4.00      TRUE Non-Breast Solid 30.15608
##  8: Cyclophosphamide  4.80      TRUE Non-Breast Solid 29.99069
##  9: Cyclophosphamide  6.64      TRUE Non-Breast Solid 29.61371
## 10: Cyclophosphamide  7.20      TRUE Non-Breast Solid 29.49992
## 11: Cyclophosphamide 14.40      TRUE Non-Breast Solid 28.07529
##     confLowerLin confUpperLin  predBin confLowerBin confUpperBin scale
##  1:    13.213812     71.52227 19.18809     8.000458     46.02023 10000
##  2:    13.288517     70.82743 19.18809     8.000458     46.02023 10000
##  3:    13.386423     69.82771 19.18809     8.000458     46.02023 10000
##  4:    13.439607     69.16992 32.04961    16.215812     63.34418 10000
##  5:    13.475559     68.41862 32.04961    16.215812     63.34418 10000
##  6:    13.457464     67.94775 32.04961    16.215812     63.34418 10000
##  7:    13.415149     67.78823 32.04961    16.215812     63.34418 10000
##  8:    13.259259     67.83495 32.04961    16.215812     63.34418 10000
##  9:    12.574606     69.74149 32.04961    16.215812     63.34418 10000
## 10:    12.291105     70.80286 32.04961    16.215812     63.34418 10000
## 11:     7.624903    103.37467 32.04961    16.215812     63.34418 10000
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
## Saving 7 x 7 in image
```

```
##                                            size               mtime
## NonBreastSolid_Cyclophosphamide_Pred.csv   1771 2016-08-05 10:27:29
## NonBreastSolid_Cyclophosphamide.csv        4356 2016-08-05 10:27:29
## NonBreastSolid_Cyclophosphamide.png      116930 2016-08-05 10:27:29
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

### Taxane


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
## Saving 7 x 7 in image
```

```
##                                 size               mtime
## NonBreastSolid_Taxane_Pred.csv   804 2016-08-05 10:27:30
## NonBreastSolid_Taxane.csv       1762 2016-08-05 10:27:30
## NonBreastSolid_Taxane.png      92483 2016-08-05 10:27:30
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)
