---
title: "Bigfoot Credibility Analysis"
author: "Grace Tollefson, James Axe, and Samuel Sheedy"
date: "12/10/2025"
output: csv, pdf
---

## Introduction
This repository contains the code and data required to reproduce the results found in our "Sasquatch Sighting Bureau" presentation for our STAT 172 Final Project. The main goal of this project was to create a predictive model for predicting whether a Bigfoot sighting was considered credible ("Class A") according to the Bigfoot Field Researchers Organization (BFRO). We also were tasked with creating a descriptive model for the same data. 

In this analysis, we presented to our proposed client, "Sasquatch Sighting Bureau", who is a call center for Bigfoot related sightings. The main goal was to help the SSB determine if incoming calls are credible or not, and further help them decide key factors as to what situations their clients, Bigfoot Hunters, should put themselves in for success.

Our objective was the following: 
* Understand key drivers of Bigfoot Sighting Credibility and develop a predictive model to identify chances for Bigfoot Hunters to catch him once and for all.

We set out to answer the following questions:
1. Is it possible to predict the credibility of sightings?
2. Can we find which situations may give our experts the best shot at finding Bigfoot?

We determined that we were successful if we were able to build a predictive model whose sensitivity was at or above an 80% threshold. That is, given 100 credible sightings, we are able to predict at least 80 of them as credible reports. 

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

The data file that will be called for our project is "bigfoot.csv", and can be found in this repository at `data/bigfoot.csv`.

## File Structure
Please ensure prior to running you create a R project of your own choosing, set to a new folder. For example, here is our R Project Folder Layout:
* Bigfoot Project (Folder set as the project folder)
  * data
  * output
  * src

Having this folder layout ensures the codes will run as desired. Our `src/Bigfoot Master LDA.R` will automatically run the cleaning code, so please ensure all codes in this repository's `src` folder are installed, as well as the `data/bigfoot.csv`. All outputs will be placed into the `output` folder of your project upon completion of running.

## Reproduce
Note: Please do not clear the environment throughout this process. This will be important for the optional last plot that we created.

1. Run `src/Bigfoot Master LDA.R` until the stop point to reproduce cleaning results and LDA results. 
    *  output/Word Frequencies.pdf
    *  output/Final LDA Groupings.pdf
    *  output/bigfoot_clean_lda.csv
  
The above file is used to export the cleaned dataset with the LDA addendum, as well as generate plots found in our presentation:

2. This step is optional. If you would like the additional LDA Testing Graphs, run `src/Bigfoot Master LDA.R` from the stopping point to the bottom to reproduce the testing results.
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

6. Optional plot creation: Run `src/Bigfoot Combined ROC Code.R` to reproduce the ROC Curve plot that combines the AUC for random forest and regression. Note: This will NOT WORK if you clear the environment at any point, as you need the rocCurve variables for each model.
    *  output/4 Model ROC Plot.pdf
