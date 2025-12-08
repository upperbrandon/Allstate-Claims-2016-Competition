
# Allstate < 1240

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vroom)
setwd("~/GitHub/Allstate-Claims-2016-Competition")

# Read Files --------------------------------------------------------------

Train <- vroom("train.csv")
Test <- vroom("test.csv")



# EDA ---------------------------------------------------------------------

# Searching columns for Dep variable

colnames(Train)
pull(Train,loss)

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


