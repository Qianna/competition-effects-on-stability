# clear all objects in the workspace
rm(list = ls())

# Dependencies
library(dplyr)
library(tidyr)

# Load Data
## Mixed culture population matrix
mixture <- read.csv("Data/Mixture_population_matrix.csv", header = T)
head(mixture, 5)

# Create a list of population matrices 
# Dividing data into subsets
subsets <- split(mixture, mixture[['microcosm_id']])

# Remove columns (microcosm_id and composition) in each data frame in the subsets list
microcosm_list <- lapply(subsets, function(x) select(x, -microcosm_id, -composition))

# Assign the 'day' column of each data frame in the list as the row names and remove the column
# from the data frame
microcosm_list <- lapply(microcosm_list, function(x) {
  rownames(x) <- x[,1]
  x <- x[, -1]
  return(x)
})

### Calculate temporal stability and decompose asynchrony (following Zhao et al. 2022) ###
source("Fn_decomposition.R")

df_list <- lapply(microcosm_list, decomposition)
df_list

# Save as a single dataframe
mixture_results <- do.call(rbind, df_list)
head(mixture_results, 5)
write.csv(mixture_results, "Results/results_mixture_3parts.csv", row.names = TRUE)


