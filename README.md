# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy

See [index.md](index.md) or [index.html](index.html) for full details, including data processing.

## Results

### AML or MDS secondary malignancies

* [Findings](index.md#findings)
	* AML or MDS rate had a dose response relationship with cumulative cyclophosphamide dose
 		* AML or MDS rate increased 1.17 times (95% CI: 1.05, 1.31; p = 0.0035) for each 1000 $\text{mg} / \text{m}^2$
	* High dose taxane confounded the cyclophosphamide dose response
  		* High dose taxane increased AML or MDS rate by 1.71 times (95% CI: 1.07, 2.76; p = 0.0264)
	* High dose taxane did not modify the dose response effect of cyclophosphamide
  		* Interaction term estimate was 0.0149 (95% CI: -1.02, 1.05; p = 0.978)
* [Plot](AMLorMDS_Cyclophosphamide_byHighDoseTaxane.png)  
	![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)
* [Data](AMLorMDS_Cyclophosphamide_byHighDoseTaxane.csv)
* [Predicted values](AMLorMDS_Cyclophosphamide_byHighDoseTaxane_Pred.csv)
* [Details](index.md#amlmds)

### Non-breast solid secondary malignancies

* [Findings](index.md#findings-1)
	* Non-Breast Solid rate did not have a dose response relationship with cumulative cyclophosphamide dose
		* Non-Breast Solid rate increased 1.01 times (95% CI: 0.917, 1.11; p = 0.846) for each 1000 $\text{mg} / \text{m}^2$
	* High dose taxane did not confound the cyclophosphamide dose response
		* High dose taxane increased Non-Breast Solid rate by 1.37 times (95% CI: 0.799, 2.35; p = 0.253)
* [Plot](NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.png)  
	![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)
* [Data](NonBreastSolid_Cyclophosphamide_byHighDoseTaxane.csv)
* [Predicted values](NonBreastSolid_Cyclophosphamide_byHighDoseTaxane_Pred.csv)
* [Details](index.md#non-breast-solid)


## Methods

Statistical methods are [here](index.md#meta-regression).


## Reproducibility

This project can be built by executing the [make.R](make.R) script.

```
$ Rscript make.R
```

See [session.log](session.log) for software and package versions.
