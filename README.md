# 🧬 Automated Machine Learning Workflow for Early Detection of Pancreatic Cancer

## **Overview**

This repository contains an **end-to-end automated machine learning (AutoML) workflow** implemented in **R using H2O.ai** for the **early diagnosis of pancreatic ductal adenocarcinoma (PDAC)**.
The project integrates **exploratory data analysis (EDA)**, **data preprocessing**, **model benchmarking**, and **model interpretability** into a **reproducible and modular pipeline**.

---

## **Project Motivation**

Early detection of PDAC is challenging due to late-stage diagnosis and limited biomarker reliability.
This project builds a **custom AutoML workflow** to reduce manual model tuning while ensuring **robust performance, interpretability, and reproducibility** for biomedical applications.

---

## **Dataset**

**Source:** Debernardi et al., *PLOS Medicine* (2020)

**Samples:** 590 patients

**Target Variable:** `diagnosis` (Positive / Negative)

**Key Biomarkers:**

* **LYVE1**
* **REG1B**
* **TFF1**
* **plasma_CA19_9**
* **Creatinine**
* **Age**
* **Sex**

Missing values were handled using **mean imputation**, and categorical variables were encoded for compatibility with AutoML.

---

## **Workflow Architecture**

### **1. Exploratory Data Analysis (EDA)**

Custom R functions were developed to:

* Analyze numeric skewness and distributions
* Normalize skewed biomarkers using optimal transformations
* Assess multicollinearity using **VIF and correlation matrices**
* Visualize categorical and continuous variable relationships

---

### **2. Data Preparation**

* Conversion of data to **H2O objects**
* Train–test split (**80:20**)
* Feature selection and encoding
* Automated preprocessing pipeline

---

### **3. Automated Machine Learning (AutoML)**

The **H2O AutoML framework** was used to train and benchmark **~1,800 models**, including:

* **Gradient Boosting Machines (GBM)**
* **Random Forest (DRF, XRT)**
* **Generalized Linear Models (GLM)**
* **Deep Learning models**
* **Stacked Ensemble models**

Model selection was based on **cross-validated performance metrics**.

---

### **4. Model Evaluation & Interpretation**

Custom utilities were implemented to:

* Compare top models across evaluation metrics
* Generate confusion matrices and classification statistics
* Visualize cross-validation performance interactively
* Extract and visualize **feature importance**
* Inspect hyperparameters of top-performing models

---

## **Key Results**

* **Best-performing model:** **Gradient Boosting Machine (GBM)**
* **AUC:** **0.94**
* **Accuracy:** **0.87**
* **F1-score:** **0.91**

**Top Predictive Biomarkers:**

* **LYVE1**
* **REG1B**
* **plasma_CA19_9**

---

## **Technologies Used**

* **Language:** R
* **Machine Learning:** H2O.ai AutoML
* **Visualization:** ggplot2, Plotly
* **EDA & Statistics:** dplyr, caret, car, bestNormalize
* **Workflow Design:** Modular R functions

---

## **Repository Structure**

```
├── data/
│   └── Debernardi_et_al_2020_data.csv
├── R/
│   ├── eda_functions.R
│   ├── h2o_functions.R
│   ├── evaluation_functions.R
├── scripts/
│   └── run_automl_pipeline.R
├── figures/
│   └── plots_and_results/
├── report/
│   └── Final_Project_Report.pdf
└── README.md
```

---

## **How to Run**

### **1. Install Required Packages**

```r
install.packages(c(
  "h2o", "ggplot2", "dplyr", "caret",
  "plotly", "bestNormalize"
))
```

### **2. Start H2O**

```r
library(h2o)
h2o.init()
```

### **3. Run the Pipeline**

```r
source("scripts/run_automl_pipeline.R")
```

---

## **Limitations**

* AutoML is computationally intensive on local machines
* Random grid search may miss optimal hyperparameter configurations
* External cohort validation is not included

---

## **Future Improvements**

* Bayesian hyperparameter optimization
* Cloud-based execution for scalability
* External validation datasets
* Extension to continuous outcome prediction

---

## **References**

* Debernardi et al., 2020. *PLOS Medicine*
* H2O.ai Documentation: [https://docs.h2o.ai](https://docs.h2o.ai)

---

## **Author**

**Abhyuday Parihar**
MS Bioinformatics & Computational Biology
Boston University
