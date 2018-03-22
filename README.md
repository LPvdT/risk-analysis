![Header](https://github.com/LPvdT/risk-analysis/blob/master/header.png)

# Analysis of credit risk in a financial institution

This project is a demonstration of a machine learning workflow throuhg the example of developing models of credit rating. The project encompasses data exploration and descriptive analysis, as well as predictive analysis using various machine learning algorithms.


## Description

This project is aimed at developing machine learning models for credit rating in a financial institution. The goal is to show the road to developing a decent model for credit risk detection in a financial institution. A wide range of algorithms are investigated and considered to accomplish this task, e.g. logistic regression, support vector machines, decision trees, random forests and artificial neural networks. Moreover, the first stage of the project also includes a descriptive analysis in which the dataset is transformed for use and scoured for useful features to use in the predictive process. For each stage of the predictive analysis, a best model is chosen, evaluated, optimsed and checked for robustness.

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
Please note that several packages may prompt you to download additional dependencies once invoked, depending on the (initial) parameters of some algorithms in the project.

## Results

The results of the project can be accessed in several ways. First and foremost, there are the raw source files, which contain the R code of each stage of analysis. The code has been commented to the extent that somebody familiar with the programming language (and econometrics and machine learning) is able to interpret each step and the reasoning behind that step. Furthermore, R Markdown files have been included in the repository. These files contain the code to generate the project reports (featuring all output, visualisations and detailed descriptions) in the _pdf_ format.

### Project reports

To view the results of the project in the most detailed and clear manner, the full project reports can found in the repository (_pdf_ files). They can also be accessed directly through the following links:

1. [Descriptive Analysis](https://github.com/LPvdT/risk-analysis/blob/master/CreditRating_desc.pdf)
2. [Predictive Analysis](https://github.com/LPvdT/risk-analysis/blob/master/CreditRating_pred.pdf)

## Acknowledgements

The data on which the project is based is an an anonymised dataset of a German bank, featuring a set of customer characteristics along with their corresponding credit rating. Some of the project's ideas have been developed as a result of influence by written works of _R. Bali_, _D. Sarkar_, _B. Lantz_ and _C. Lesmeister_.
