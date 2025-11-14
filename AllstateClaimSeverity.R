


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vroom)


# Read Files --------------------------------------------------------------

Train <- vroom("train.csv")
Test <- vroom("test.csv")



# EDA ---------------------------------------------------------------------

# Searching columns for Dep variable

View(Train)
colnames(Train)
pull(Train,loss)

for (i in 1:14){
a <- paste0("cont",i)
b <- NULL
if(max(a)>b){b = max(a)}}

b <- -Inf  # start with the smallest possible number

for (i in 1:14) {
  a <- paste0("cont", i)
  col_max <- max(Train[[a]], na.rm = TRUE)
  if (col_max > b) {
    b <- col_max
  }
}

b  # final maximum value across all cont1–cont14

# Recipe ------------------------------------------------------------------

A_recipe <- recipe(formula = loss ~ ., data = Train)


# Workflows ----------------------------------------------------------------




# Methods -----------------------------------------------------------------


