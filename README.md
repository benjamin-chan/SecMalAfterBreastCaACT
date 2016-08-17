# Risks of Long-term Secondary Malignancies in Breast Cancer Patients Treated with Adjuvant Chemotherapy

See [index.md](index.md) or [index.html](index.html) for full details, including data processing.

## Results

### AML or MDS secondary malignancies

* [Findings](index.md#findings)
	* Crude statistics
		* Total person-years of follow-up: 261,433
		* Total number of persons (ITT): 44,628
		* Total number of AML or MDS: 171
		* Crude incidence rate: 6.54 per 10,000 person-years
	* AML or MDS rate had a dose response relationship with cumulative cyclophosphamide dose (M1)
		* AML or MDS rate increased 1.16 times (95% CI: 1.04, 1.29; p = 0.0057) for each 1000 $\text{mg} / \text{m}^2$
	* Paclitaxel confounded the cyclophosphamide dose response
		* Compared to no taxane (M1), paclitaxel increased AML or MDS rate by 1.48 times (95% CI: 0.933, 2.33; p = 0.096)
		* Compared to no taxane or docetaxel (M2), paclitaxel increased AML or MDS rate by 1.62 times (95% CI: 1.05, 2.47; p = 0.028)
		* Compared to docetaxel (M3), paclitaxel increased AML or MDS rate by 2.01 times (95% CI: 1.13, 3.57; p = 0.018)
	* Paclitaxel did not modify the dose response effect of cyclophosphamide (M4)
		* Interaction term estimate was -0.0476 (95% CI: -1.07, 0.974; p = 0.93)
* [Plot](AMLorMDS_Cyclophosphamide_byTaxane.png)  
	![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)
* [Data](AMLorMDS_Cyclophosphamide_byTaxane.csv)
* [Predicted values](AMLorMDS_Cyclophosphamide_byTaxane_Pred.csv)
* [Details](index.md#amlmds)

### Non-breast solid secondary malignancies

* [Findings](index.md#findings-1)
	* Crude statistics
		* Total person-years of follow-up: 112,778
		* Total number of persons (ITT): 19,289
		* Total number of Non-Breast Solid: 363
		* Crude incidence rate: 32.2 per 10,000 person-years
	* Non-Breast Solid rate did not have a dose response relationship with cumulative cyclophosphamide dose (M1)
		* Non-Breast Solid rate increased 1 times (95% CI: 0.906, 1.11; p = 0.96) for each 1000 $\text{mg} / \text{m}^2$
	* Paclitaxel did not confound the cyclophosphamide dose response
		* Compared to no taxane (M1), paclitaxel increased Non-Breast Solid rate by 1.32 times (95% CI: 0.752, 2.31; p = 0.33)
		* Compared to no taxane or docetaxel (M2), paclitaxel increased Non-Breast Solid rate by 1.37 times (95% CI: 0.799, 2.35; p = 0.25)
		* Compared to docetaxel (M3), paclitaxel increased Non-Breast Solid rate by 1.5 times (95% CI: 0.78, 2.9; p = 0.22)
	* Paclitaxel did not modify the dose response effect of cyclophosphamide (M4)
		* Interaction term estimate was 2.19 (95% CI: -0.24, 4.62; p = 0.077)
* [Plot](NonBreastSolid_Cyclophosphamide_byTaxane.png)  
	![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)
* [Data](NonBreastSolid_Cyclophosphamide_byTaxane.csv)
* [Predicted values](NonBreastSolid_Cyclophosphamide_byTaxane_Pred.csv)
* [Details](index.md#non-breast-solid)


## Methods

Statistical methods are described [here](index.md#meta-regression).


## Reproducibility

This project can be built by executing the [make.R](make.R) script.

```
$ Rscript make.R
```

See [session.log](session.log) for software and package versions.
