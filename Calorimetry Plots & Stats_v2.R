---
title: "Calorimetry Plots"
author: "Abrar Al-Shaer"
date: "September 18, 2019"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

```{r}
rm(list=ls())
library(ggplot2)
library(gplots)
library("dplyr")
library("ggpubr")
library(tidyverse)
library(car)
```

```{r}
calData <- read.csv("Calorimtery_ALX-FPR2_Calculated_Analysis.csv", header = TRUE, fill = TRUE, row.names = 1)
head(calData)
```

```{r}
#check structure of the data to make sure that "groups" the mice belong to are factor variables
str(calData)
```

#Plotting Data & Statistical Analysis

```{r}
# Basic bar plots of means +/- sem with jittered points
ggbarplot(calData, x = "Diet", y = "EE.Day", 
          add = c("mean_se", "jitter"),
          color = "Genotype", palette = "aaas", title = "RMR",
          position = position_dodge(0.8))

#checking for normality
shapiro.test(calData$Total.Lox)
#check for homogeneity of variance
leveneTest(Total.Lox ~ Group, data = calData)

#Run a 2-way ANOVA
#run the multiplicative model to see if there is an interaction term (signif. diff in slope)
my_anova <- aov(Total.EE ~ Diet * Genotype, data = calData)
Anova(my_anova, type = "III") #must run a type III ANOVA because the sample size is not balanced

#run the additive model if the interaction term is not significant
#test if there is a significant difference in the intercepts of the lines
#if there is no significant interaction effect, then type II is more powerful. If interaction is present, then type II is inappropriate while type III can still be used.
my_anova <- aov(Total.EE ~ Diet + Genotype, data = calData)
Anova(my_anova, type = "III")
TukeyHSD(my_anova)
```

```{r}
# Basic bar plots of means +/- sem with jittered points
ggbarplot(calData, x = "Diet", y = "Resting.Gox", 
          add = c("mean_se", "jitter"),
          color = "Genotype", palette = "aaas", title = "Resting Glucose Metabolism",
          position = position_dodge(0.8))

#checking for normality
shapiro.test(calData$Resting.Gox)
#check for homogeneity of variance
leveneTest(Resting.Gox ~ Group, data = calData)

#Run a 2-way ANOVA
#run the multiplicative model to see if there is an interaction term (signif. diff in slope)
my_anova <- aov(Resting.Gox ~ Diet * Genotype, data = calData)
Anova(my_anova, type = "III") #must run a type III ANOVA because the sample size is not balanced
TukeyHSD(my_anova)
```

```{r}
# Basic bar plots of means +/- sem with jittered points
ggbarplot(calData, x = "Diet", y = "Resting.Lox", 
          add = c("mean_se", "jitter"),
          color = "Genotype", palette = "aaas", title = "Resting Lipid Metabolism",
          position = position_dodge(0.8))

#checking for normality
shapiro.test(calData$Resting.Lox)
#check for homogeneity of variance
leveneTest(Resting.Lox ~ Group, data = calData)

#Run a 2-way ANOVA
#run the multiplicative model to see if there is an interaction term (signif. diff in slope)
my_anova <- aov(Resting.Lox ~ Diet * Genotype, data = calData)
Anova(my_anova, type = "III") #must run a type III ANOVA because the sample size is not balanced
TukeyHSD(my_anova)
```

