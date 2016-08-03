# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy

See [index.md](index.md) or [index.html](index.html) for full details.

## Results

Estimated incidence rate of:

1. **AML or MDS** malignancies
    1. By cumulative cyclophophamide dose ([PNG](AMLorMDS_Cyclophosphamide.png), [details](index.md#cyclophosphamide), [data](AMLorMDS_Cyclophosphamide.csv))
        1. Stratify by with or without taxane regimen ([PNG](AMLorMDS_Cyclophosphamide_byTaxane.png), [details](index.md#cyclophosphamide-with-and-without-taxane))
    1. By cumulative taxane dose ([PNG](AMLorMDS_Taxane.png), [details](index.md#taxane), [data](AMLorMDS_Taxane.csv))
1. **Non-breast solid** malignancies
    1. By cumulative cyclophophamide dose ([PNG](NonBreastSolid_Cyclophosphamide.png), [details](index.md#cyclophosphamide-1), [data](NonBreastSolid_Cyclophosphamide.csv))
    1. By cumulative taxane dose ([PNG](NonBreastSolid_Taxane.png), [details](index.md#taxane-1), [data](NonBreastSolid_Taxane.csv))


## Methods

Statistical methods are [here](index.md#meta-regression).


## Reproducibility

This project can be built by executing the [make.R](make.R) script.

```
$ Rscript make.R
```

See [session.log](session.log) for software and package versions.
