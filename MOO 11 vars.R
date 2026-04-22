# clear environment
rm(list = ls())

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)

# set seed
set.seed(204)

# Read the CSV file
# Set the file path
file_path <- "/Users/bishalbista/Library/CloudStorage/OneDrive-Colostate/RA NAte/Colostate/DeLay,Nathan - BCS Project/data and codes/Final_data_kg.csv"

# Use read_csv to import the data
library(readr)   # Load the readr package

data <- read_csv(file_path)

# View first few rows
head(data)

# Convert necessary columns to numeric (milkmonth1 to milkmonth10 and bcsm1 to bcsm12)
data <- data %>%
  mutate(
    totalmilkproduced305 = as.numeric(totalmilkproduced305),
    milkmonth1 = as.numeric(milkmonth1),
    milkmonth2 = as.numeric(milkmonth2),
    milkmonth3 = as.numeric(milkmonth3),
    milkmonth4 = as.numeric(milkmonth4),
    milkmonth5 = as.numeric(milkmonth5),
    milkmonth6 = as.numeric(milkmonth6),
    milkmonth7 = as.numeric(milkmonth7),
    milkmonth8 = as.numeric(milkmonth8),
    milkmonth9 = as.numeric(milkmonth9),
    milkmonth10 = as.numeric(milkmonth10),
    conceptionDIM = as.numeric(conceptionDIM),
    bcsm1 = as.numeric(bcsm1),
    bcsm2 = as.numeric(bcsm2),
    bcsm3 = as.numeric(bcsm3),
    bcsm4 = as.numeric(bcsm4),
    bcsm5 = as.numeric(bcsm5),
    bcsm6 = as.numeric(bcsm6),
    bcsm7 = as.numeric(bcsm7),
    bcsm8 = as.numeric(bcsm8),
    bcsm9 = as.numeric(bcsm9),
    bcsm10 = as.numeric(bcsm10),
    bcsm11 = as.numeric(bcsm11),
    bcsm12 = as.numeric(bcsm12),
    nbcs = as.numeric(nadirBCS),
  )


# Ensure there are no missing values in the relevant columns
data <- data %>%
  filter(!is.na(milkmonth1) & !is.na(milkmonth2) & !is.na(milkmonth3) 
         & !is.na(milkmonth4) & !is.na(milkmonth5) & !is.na(milkmonth6) 
         & !is.na(milkmonth7) & !is.na(milkmonth8) & !is.na(milkmonth9) 
         & !is.na(milkmonth10) & !is.na(bcsm1) & !is.na(bcsm2) & !is.na(bcsm3) 
         & !is.na(bcsm4) & !is.na(bcsm5) & !is.na(bcsm6) & !is.na(bcsm7) 
         & !is.na(bcsm8) & !is.na(bcsm9) & !is.na(bcsm10) & !is.na(bcsm11) 
         & !is.na(bcsm12)& !is.na(nbcs))



# summary statistics for totalmilkproduced305 and conceptionDIM
summary(data$conceptionDIM)
summary(data$totalmilkproduced305)


View(data)

# Calculate quartiles for the 'nadirBCS' column
quartiles <- quantile(data$nadirBCS, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# nadirBCS values for quartiles
print(quartiles)


# Create a new column 'Category' based on quartiles
data <- data %>%
  mutate(Category = cut(nadirBCS, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))


# View the updated data
View(data)


# Group by the 'Category' column and calculate summary statistics for 'totalmilkproduced305'
summary_stats <- data %>%
  group_by(Category) %>%
  summarize(
    Mean_Total_Milk = mean(totalmilkproduced305, na.rm = TRUE),
    Median_Total_Milk = median(totalmilkproduced305, na.rm = TRUE),
    Min_Total_Milk = min(totalmilkproduced305, na.rm = TRUE),
    Max_Total_Milk = max(totalmilkproduced305, na.rm = TRUE),
    SD_Total_Milk = sd(totalmilkproduced305, na.rm = TRUE)
  )


# View the summary statistics
print(summary_stats)
#View(summary_stats)

# Summary stats of totalmilkproduced305 by lactation
summary_stats_lact <- data %>%
  group_by(LACT) %>%
  summarize(
    Mean_Total_Milk = mean(totalmilkproduced305, na.rm = TRUE),
    Median_Total_Milk = median(totalmilkproduced305, na.rm = TRUE),
    Min_Total_Milk = min(totalmilkproduced305, na.rm = TRUE),
    Max_Total_Milk = max(totalmilkproduced305, na.rm = TRUE),
    SD_Total_Milk = sd(totalmilkproduced305, na.rm = TRUE)
  )
print(summary_stats_lact)

# Summarize all the milkmonth columns, grouped by Category
summary_stats1 <- data %>%
  select(Category, starts_with("milkmonth")) %>%
  group_by(Category) %>%
  summarise_all(list(Mean = mean), na.rm = TRUE)

# View the summary statistics
print(summary_stats1)
View(summary_stats1)

# Summarize all the conception_DIM columns, grouped by Category
summary_stats2 <- data %>%
  select(Category, starts_with("conceptionDIM")) %>%
  group_by(Category) %>%
  summarise_all(list(Mean = mean), na.rm = TRUE)

# View(summary_stats2)

# Get unique values in the 'parity' column
unique_values <- unique(data$parity)

# Check if there are any values other than "PP" and "MP"
other_values <- unique_values[!(unique_values %in% c("PP", "MP"))]

if (length(other_values) > 0) {
  print("There are other characters present in the 'parity' column:")
  print(other_values)
} else {
  print("There are no characters other than 'PP' and 'MP' in the 'parity' column.")
}


# Split data into two separate data frames based on 'parity'
data_PP <- subset(data, parity == "PP")
data_MP <- subset(data, parity == "MP")

# Summarize all the milkmonth columns, grouped by Category for primiparous
summary_statsPP <- data_PP %>%
  select(Category, starts_with("milkmonth")) %>%
  group_by(Category) %>%
  summarise_all(list(Mean = mean), na.rm = TRUE)

# View the summary statistics
print(summary_statsPP)
#View(summary_statsPP)

# Summarize all the milkmonth columns, grouped by Category for multiparous
summary_statsMP <- data_MP %>%
  select(Category, starts_with("milkmonth")) %>%
  group_by(Category) %>%
  summarise_all(list(Mean = mean), na.rm = TRUE)

# View the summary statistics
print(summary_statsMP)
#View(summary_statsMP)

# Convert 'outcome1AI' to numeric (1 for pregnant, 0 for not pregnant)
data$outcome1AI_numeric <- as.numeric(data$outcome1AI == "P")

# Calculate the probability of pregnancy in first AI for each category
prob_pregnant_AI1 <- data %>%
  group_by(Category) %>%
  summarize(Probability_Pregnant = mean(outcome1AI_numeric == 1))

# View the resulting probabilities
print(prob_pregnant_AI1)


# Calculate the overall probability of pregnancy for each category 
prob_pregnant_by_category <- data %>%
  group_by(Category) %>%
  summarize(Probability_Pregnant = mean(PREG == 1))

# View the resulting probabilities
print(prob_pregnant_by_category)

# Pregnancy probabilities by month of lactation
#changing conceptionDIM to numeric data
data$conceptionDIM <- as.numeric(data$conceptionDIM)

# Replace '.' and NA with 0 in conceptionDIM
data$conceptionDIM[is.na(data$conceptionDIM) | data$conceptionDIM == '.'] <- 0

# Assign monthly pregnancy based on lactation
data <- data %>%
  mutate(
    pregmonth1 = ifelse(conceptionDIM > 0 & conceptionDIM <= 30, 1, 0),
    pregmonth2 = ifelse(conceptionDIM > 30 & conceptionDIM <= 60, 1, 0),
    pregmonth3 = ifelse(conceptionDIM > 60 & conceptionDIM <= 90, 1, 0),
    pregmonth4 = ifelse(conceptionDIM > 90 & conceptionDIM <= 120, 1, 0),
    pregmonth5 = ifelse(conceptionDIM > 120 & conceptionDIM <= 150, 1, 0),
    pregmonth6 = ifelse(conceptionDIM > 150 & conceptionDIM <= 180, 1, 0),
    pregmonth7 = ifelse(conceptionDIM > 180 & conceptionDIM <= 210, 1, 0),
    pregmonth8 = ifelse(conceptionDIM > 210 & conceptionDIM <= 240, 1, 0),
    pregmonth9 = ifelse(conceptionDIM > 240 & conceptionDIM <= 270, 1, 0),
    pregmonth10 = ifelse(conceptionDIM > 270 & conceptionDIM <= 300, 1, 0)
  )

# Calculate pregnancy probabilities for each pregmonth and each category
preg_probabilities <- data %>%
  group_by(Category) %>%
  summarize(
    pregprobability_pregmonth1 = mean(pregmonth1),
    pregprobability_pregmonth2 = mean(pregmonth2),
    pregprobability_pregmonth3 = mean(pregmonth3),
    pregprobability_pregmonth4 = mean(pregmonth4),
    pregprobability_pregmonth5 = mean(pregmonth5),
    pregprobability_pregmonth6 = mean(pregmonth6),
    pregprobability_pregmonth7 = mean(pregmonth7),
    pregprobability_pregmonth8 = mean(pregmonth8),
    pregprobability_pregmonth9 = mean(pregmonth9),
    pregprobability_pregmonth10 = mean(pregmonth10)
  )

# View the resulting probabilities
print(preg_probabilities)
#View(preg_probabilities)


# Calculate the probability of abortion for each category 
prob_abortion_by_category <- data %>%
  group_by(Category) %>%
  summarize(Probability_Abortion = mean(ABORTO == 1))

# View the resulting probabilities
print(prob_abortion_by_category)

# calculate summary stats for conception_DIM
summary_stats_conception_DIM <- data %>%
  group_by(Category) %>%
  summarize(
    Mean_Conception_DIM = mean(conceptionDIM, na.rm = TRUE),
    Median_Conception_DIM = median(conceptionDIM, na.rm = TRUE),
    Min_Conception_DIM = min(conceptionDIM, na.rm = TRUE),
    Max_Conception_DIM = max(conceptionDIM, na.rm = TRUE),
    SD_Conception_DIM = sd(conceptionDIM, na.rm = TRUE)
  )

# View the resulting conception_DIM 
print(summary_stats_conception_DIM)

# summary stats for milk production in each lactation 
summary_stats_milk_production <- data %>%
  group_by(LACT) %>%
  summarize(
    Mean_Total_Milk = mean(totalmilkproduced305, na.rm = TRUE),
    Median_Total_Milk = median(totalmilkproduced305, na.rm = TRUE),
    Min_Total_Milk = min(totalmilkproduced305, na.rm = TRUE),
    Max_Total_Milk = max(totalmilkproduced305, na.rm = TRUE),
    SD_Total_Milk = sd(totalmilkproduced305, na.rm = TRUE)
  )

# View
#View(summary_stats_milk_production)

##regressions and sensitivity 

library(dplyr)
library(MASS)

# -----------------------------
# Prepare data
# -----------------------------
data1 <- data %>%
  filter(LACT %in% c(1, 2, 3, 4))

# Function: alt coefficients
get_alt_coef <- function(model) {
  beta_hat <- coef(model)
  V_hat    <- vcov(model)
  
  beta_alt <- mvrnorm(1, beta_hat, V_hat)
  beta_alt <- as.numeric(beta_alt)
  names(beta_alt) <- names(beta_hat)
  
  return(beta_alt)
}

# Function: print results nicely
print_results <- function(model, name) {
  cat("\n=============================\n")
  cat(name, "\n")
  cat("=============================\n")
  
  cat("\n--- Regression Summary ---\n")
  print(summary(model))
  
  cat("\n--- Baseline Coefficients ---\n")
  print(coef(model))
  
  beta_alt <- get_alt_coef(model)
  
  cat("\n--- Alternative Coefficients ---\n")
  print(beta_alt)
  
  cat("\n--- Difference (Alt - Baseline) ---\n")
  print(beta_alt - coef(model))
}

# -----------------------------
# ALL LACTATIONS
# -----------------------------
milk_all <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                 bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data1)

dim_all  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                 bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data1)

print_results(milk_all, "Milk - All Lactations")
print_results(dim_all,  "DIM - All Lactations")

# -----------------------------
# LACT = 2,3,4
# -----------------------------
data3 <- data1 %>% filter(LACT %in% c(2,3,4))

milk_l234 <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                  bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data3)

dim_l234  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                  bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data3)

print_results(milk_l234, "Milk - LACT 2,3,4")
print_results(dim_l234,  "DIM - LACT 2,3,4")

# -----------------------------
# LACT = 1
# -----------------------------
data2 <- data1 %>% filter(LACT == 1)

milk_l1 <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data2)

dim_l1  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data2)

print_results(milk_l1, "Milk - LACT 1")
print_results(dim_l1,  "DIM - LACT 1")



# -----------------------------
# LACT = 2
# -----------------------------
data4 <- data1 %>% filter(LACT == 2)

milk_l2 <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data4)

dim_l2  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data4)

print_results(milk_l2, "Milk - LACT 2")
print_results(dim_l2,  "DIM - LACT 2")

# -----------------------------
# LACT = 3
# -----------------------------
data5 <- data1 %>% filter(LACT == 3)

milk_l3 <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data5)

dim_l3  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data5)

print_results(milk_l3, "Milk - LACT 3")
print_results(dim_l3,  "DIM - LACT 3")

# -----------------------------
# LACT = 4
# -----------------------------
data6 <- data1 %>% filter(LACT == 4)

milk_l4 <- lm(totalmilkproduced305 ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data6)

dim_l4  <- lm(conceptionDIM ~ bcsm1 + bcsm2 + bcsm3 + bcsm4 + bcsm5 +
                bcsm6 + bcsm7 + bcsm8 + bcsm9 + bcsm10 + nbcs, data = data6)

print_results(milk_l4, "Milk - LACT 4")
print_results(dim_l4,  "DIM - LACT 4")









library(dplyr)
library(purrr)

# Generalized function
calc_summary <- function(data, col1, col2, lact_filter = NULL, filter_outliers = FALSE) {
  data %>%
    { if (!is.null(lact_filter)) filter(., LACT %in% lact_filter) else . } %>%
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    { if (filter_outliers) filter(., abs_diff < quantile(abs_diff, 0.95)) else . } %>%
    summarise(
      mean_abs_diff = mean(abs_diff, na.rm = TRUE),
      max_abs_diff = max(abs_diff, na.rm = TRUE),
      min_abs_diff = min(abs_diff, na.rm = TRUE)
    )
}

# Function to run over all column pairs
run_summary <- function(data, lact_filter = NULL) {
  column_pairs <- paste0("bcsm", 1:9)
  
  map(seq_along(column_pairs), function(i) {
    col1 <- column_pairs[i]
    col2 <- paste0("bcsm", i + 1)
    
    list(
      original_summary = calc_summary(data, col1, col2, lact_filter, filter_outliers = FALSE),
      filtered_summary = calc_summary(data, col1, col2, lact_filter, filter_outliers = TRUE)
    )
  })
}

# Now easily apply:

# Lactation 1 only
results_lact1 <- run_summary(data1, lact_filter = 1)

# Lactation 2 only
results_lact2 <- run_summary(data1, lact_filter = 2)

# Lactation 3 only
results_lact3 <- run_summary(data1, lact_filter = 3)

# Lactation 4 only
results_lact4 <- run_summary(data1, lact_filter = 4)

# All cows (LACT 1,2,3,4 only)
results_all_1to4 <- run_summary(data1, lact_filter = 1:4)

# Cows in lactation 2,3,4 (NOT lactation 1)
results_lact2to4 <- run_summary(data1, lact_filter = 2:4)





























# constraints for optimization 

# lactation 1

# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT == 1) %>%  # Filter for lact = 1
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Column pairs and results calculation
results <- lapply(1:9, function(i) {
  col1 <- paste0("bcsm", i)
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_summary(data1, col1, col2),
    filtered_summary = calc_summary(data1, col1, col2, filter = TRUE)
  )
})

# Print results
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})




# Lactation 2

# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT == 2) %>%  # Filter for lact = 1
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Column pairs and results calculation
results <- lapply(1:9, function(i) {
  col1 <- paste0("bcsm", i)
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_summary(data1, col1, col2),
    filtered_summary = calc_summary(data1, col1, col2, filter = TRUE)
  )
})

# Print results
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})


# Lactation 3
# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT == 3) %>%  # Filter for lact = 1
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Column pairs and results calculation
results <- lapply(1:9, function(i) {
  col1 <- paste0("bcsm", i)
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_summary(data1, col1, col2),
    filtered_summary = calc_summary(data1, col1, col2, filter = TRUE)
  )
})

# Print results
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})



# Lactation 4
# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT == 4) %>%  # Filter for lact = 1
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Column pairs and results calculation
results <- lapply(1:9, function(i) {
  col1 <- paste0("bcsm", i)
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_summary(data1, col1, col2),
    filtered_summary = calc_summary(data1, col1, col2, filter = TRUE)
  )
})

# Print results
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})

# all cows

library(dplyr)
# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT %in% 1:4) %>%   # KEEP only lactation 1, 2, 3, 4
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}
# Create a function to calculate absolute differences and summary statistics for cows in LACT 1, 2, 3, and 4


calc_diff_summary <- function(data, col1, col2) {
  data %>%
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Create a function to filter and calculate summary statistics
calc_filtered_summary <- function(data, col1, col2) {
  data %>%
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    filter(
      abs_diff > quantile(abs_diff, 0.00),
      abs_diff < quantile(abs_diff, 0.95)
    ) %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}

# Define column pairs
column_pairs <- paste0("bcsm", 1:9)

# Apply the functions for all pairs
results <- lapply(seq_along(column_pairs), function(i) {
  col1 <- column_pairs[i]
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_diff_summary(data1, col1, col2),
    filtered_summary = calc_filtered_summary(data1, col1, col2)
  )
})

# Print the results for all pairs
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})


# Do for cows that are not in lactation 1
# Function to calculate absolute differences and summary statistics
calc_summary <- function(data, col1, col2, filter = FALSE) {
  data %>%
    filter(LACT %in% 2:4) %>%   # KEEP only lactation 2, 3, 4
    mutate(abs_diff = abs(!!sym(col1) - !!sym(col2))) %>%
    {if (filter) filter(., abs_diff < quantile(abs_diff, 0.95)) else .} %>%
    summarise(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      min_abs_diff = min(abs_diff)
    )
}
# Column pairs and results calculation
results <- lapply(1:9, function(i) {
  col1 <- paste0("bcsm", i)
  col2 <- paste0("bcsm", i + 1)
  
  list(
    original_summary = calc_summary(data1, col1, col2),
    filtered_summary = calc_summary(data1, col1, col2, filter = TRUE)
  )
})

# Print results
lapply(results, function(result) {
  print(result$original_summary)
  print(result$filtered_summary)
})


#Average values of conceptionDIM and totalmilkproduced305 for all cows
data1 %>%
  summarise(
    mean_conceptionDIM = mean(conceptionDIM, na.rm = TRUE),
    mean_totalmilkproduced305 = mean(totalmilkproduced305, na.rm = TRUE)
  )



# Average values of conceptionDIM and totalmilkproduced305 by LACT
data1 %>%
  group_by(LACT) %>%
  summarise(
    mean_conceptionDIM = mean(conceptionDIM, na.rm = TRUE),
    mean_totalmilkproduced305 = mean(totalmilkproduced305, na.rm = TRUE)
  )


#Average values of conceptionDIM and totalmilkproduced305 for multiparous cows 
data1 %>%
  filter(LACT != 1) %>%
  summarise(
    mean_conceptionDIM = mean(conceptionDIM, na.rm = TRUE),
    mean_totalmilkproduced305 = mean(totalmilkproduced305, na.rm = TRUE)
  )

# All cows

# Sample weights for conception DIM (adjust according to your data)
weights_conception_DIM <- c(1.39, 3.34, 4.73, 4.47, 4.04, 3.62, 3.17, 2.67, 2.21, 1.72)


# Function to calculate weighted milk production
calculate_weighted_milk_production <- function(milk_production) {
  return(milk_production * 0.18)
}

# Function to calculate weighted conception DIM
calculate_weighted_conception_DIM <- function(conception_DIM, weights) {
  weighted_conception_DIM <- 0
  days_remaining <- conception_DIM
  
  for (i in 1:10) {
    if (days_remaining > 30) {
      weighted_conception_DIM <- weighted_conception_DIM + (30 * weights[i])
      days_remaining <- days_remaining - 30
    } else {
      weighted_conception_DIM <- weighted_conception_DIM + (days_remaining * weights[i])
      break
    }
  }
  return(weighted_conception_DIM)
}

# Apply the weighted milk production function to the dataset
data1$weighted_milk_production <- sapply(data1$totalmilkproduced305, calculate_weighted_milk_production)

# Apply the weighted conception DIM function to the dataset
data1$weighted_conception_DIM <- sapply(data1$conceptionDIM, calculate_weighted_conception_DIM, weights = weights_conception_DIM)

# Calculate profit for all cows case
data1$profit <- data1$weighted_milk_production - data1$weighted_conception_DIM

# Identify the cow with the highest profit and the corresponding totalmilkproduced305 and conceptionDIM values
top_cow <- data1[which.max(data1$profit), c("totalmilkproduced305", "conceptionDIM", "profit")]


# Identify the cow with the lowest profit and the corresponding totalmilkproduced305 and conceptionDIM values
bottom_cow <- data1[which.min(data1$profit), c("totalmilkproduced305", "conceptionDIM", "profit")]











