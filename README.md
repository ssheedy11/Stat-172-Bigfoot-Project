---
title: "Bigfoot Credibility Analysis"
author: "Grace Tollefson, James Axe, and Samuel Sheedy"
date: "12/10/2025"
output: csv, pdf
---

## Introduction

## Introduction
This repository contains the code and data required to reproduce the results found in "A Hybrid Approach to Targeting Social Assistance". Specifically, to run simulation studies that estimate out of sample error rates using the Hybrid, Hybrid-AI, Hybrid-EC, and Hybrid-DU models on data from Indonesia (Alatas et al. (2012)) and Burkina Faso (Hillebrecht et al. (2020)). 

## Requirements
To install the required R packages, run the following code in R:


```r
install.packages(c("caret", "dplyr", "glmnet", "ggplot2", "httr", "jsonlite", "lubridate", "lunar",
                   "mice", "pbapply", "pROC", "randomForest","RColorBrewer", "reshape2", 
                   "rpart", "rpart.plot", "tidymodels", "tidytext", "tidyverse", "topicmodels"))
```

## Data
We use one source of data, containing information on weather, people's first hand experiences, and location data on Bigfoot sightings. THis data file was originally downloaded from the TidyTuesday Repository, found at the link below:

[data/2022/2022-09-13](https://github.com/rfordatascience/tidytuesday/tree/main/data/2022/2022-09-13)

The data file that will be called for our project is "bigfoot.csv".

## Reproduce
1. Run `src/Bigfoot Master LDA.R` until the stop point to reproduce cleaning results and LDA results. 
  *  output/Word Frequencies.pdf
  *  output/Final LDA Groupings.pdf
  *  output/bigfoot_clean_lda.csv
  
The above file is used to export the cleaned dataset with the LDA addendum, as well as generate plots found in our presentation:

2. This step is optional. If you would like the additional LDA Testing Graphs, run `Bigfoot Master LDA.R` from the stopping point to the bottom to reproduce the testing results.
    *  output/LDA - 5 Topics.pdf
    *  output/LDA - 6 Topics.pdf
    *  output/LDA - 7 Topics.pdf
    *  output/LDA - 8 Topics.pdf
    *  output/LDA - 9 Topics.pdf
    *  output/LDA - 10 Topics.pdf
    *  output/LDA - 11 Topics.pdf
    *  output/LDA - 12 Topics.pdf

3. Run `src/Bigfoot Exploratory Plots.R` to reproduce relevent plots used for exploratory analysis.
    *  output/Credibility by Location.pdf
    *  output/Credibility by Cloud Cover.pdf
    *  output/Credibility by Pressure.pdf
    *  output/Credibility by Wind Speed.pdf
    *  output/Credibility by Region.pdf

4. Run `src/Bigfoot Master RF.R` to reproduce random forest results and generate relevent plots. 
  *  output/ROC Curve RF.pdf
  *  output/Variable Importance Plot.pdf
  *  output/Random Forest Confusion Matrix.pdf

5. Run `src/Bigfoot Master Regression.R` to reproduce regression results and generate relevent plots. 
  *  output/ROC Curve MLE.pdf
  *  output/ROC Curve Lasso.pdf
  *  output/ROC Curve Ridge.pdf
  *  output/ROC Curve Comparison Regression Only.pdf

