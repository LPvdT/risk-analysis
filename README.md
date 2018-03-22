![Header](https://github.com/LPvdT/risk-analysis/blob/master/header.png)

# Analysis of credit risk in a financial institution

Demonstration of machine learning workflow by developing models of credit rating using various machine learning algorithms. The project encompasses data exploration and descriptive analysis, as well as predictive analysis using various machine learning algorithms.


## Description

This project is aimed at developing machine learning models for credit rating in a financial institution. A wide range of algorithms are used to accomplish this task: logistic regression, support vector machines, decision trees, random forests and artificial neural networks. Moreover, the first stage of the project includes a descriptive analysis in which the dataset is transformed and scoured for useful features. For each stage of the analysis, a best model is chosen, evaluated, optimsed and checked for robustness.

## Prerequisites

The following R packages are required:

```R
library(dplyr)
library(caret)
library(randomForest)
library(ROCR)
library(e1071)
library(kernlab)
library(rpart)
library(rpart.plot)
library(gridExtra)
library(pastecs)
library(ggplot2)
library(gmodels)
library(car)
```
Several packages may prompt you to download additional dependencies once invoked.

## Results

The results of the project can be accessed in several ways. First and foremost, there are the raw source files, which contain the R code of each stage of analysis. The code has been commented to the extent that somebody familiar with the programming language is able to interpret each step. Furthermore, R Markdown files have been included in the repository. These files contain the code to generate the project reports (featuring all output, visualisations and detailed descriptions) in the _pdf_ format.

### Project reports

To view the results of the project in the most detailed manner, the full project reports can found in the repository (_pdf_ files). They can also be accessed through the following links:

1. [Descriptive Analysis](https://github.com/LPvdT/risk-analysis/blob/master/CreditRating_desc.pdf)
2. [Predictive Analysis](https://github.com/LPvdT/risk-analysis/blob/master/CreditRating_pred.pdf)

## Acknowledgements

The data on which the project is based is an an anonymised dataset of a German bank, detailing several customer characteristics along with their credit rating. Some of the project's ideas have been influences by _R. Bali_, _D. Sarkar_, _B. Lantz_ and _C. Lesmeister_.
