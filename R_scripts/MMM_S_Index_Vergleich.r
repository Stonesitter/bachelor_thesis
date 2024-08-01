path <- "C:/Users/maalt/ONEDRI~1/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/bachelor_thesis/Experimente/Results/Stichtag_09_07"
merged_data <- read.csv(paste0(path, "/merged_data_0907.csv"))

# Your sample statistics
your_mean <- 2.556
your_sd <- 0.6035648
your_n <- 161

# Study statistics for w1
w1_mean <- 2.22
w1_sd <- 0.77
w1_n <- 523

# Study statistics for w2
w2_mean <- 2.25
w2_sd <- 0.73
w2_n <- 419


# Function to perform a two-sample t-test given sample and population statistics
t_test <- function(sample_mean, sample_sd, sample_n, pop_mean, pop_sd, pop_n) {
  pooled_sd <- sqrt(((sample_n - 1) * sample_sd^2 + (pop_n - 1) * pop_sd^2) / (sample_n + pop_n - 2))
  t_stat <- (sample_mean - pop_mean) / (pooled_sd * sqrt(1/sample_n + 1/pop_n))
  df <- sample_n + pop_n - 2
  p_value <- 2 * pt(-abs(t_stat), df)
  return(p_value)
}

# Compare your data to w1 statistics
p_value_w1 <- t_test(your_mean, your_sd, your_n, w1_mean, w1_sd, w1_n)
print(paste("P-value for comparison with w1:", p_value_w1))

# Compare your data to w2 statistics
p_value_w2 <- t_test(your_mean, your_sd, your_n, w2_mean, w2_sd, w2_n)
print(paste("P-value for comparison with w2:", p_value_w2))


# Function to calculate Cohen's d
cohen_d <- function(sample_mean, sample_sd, pop_mean, pop_sd) {
  pooled_sd <- sqrt((sample_sd^2 + pop_sd^2) / 2)




# Simulate original study data
set.seed(123)
original_w1 <- rnorm(523, mean = 2.22, sd = 0.77)
original_w2 <- rnorm(419, mean = 2.25, sd = 0.73)

# Your sample data
your_data <- merged_data$MMM_S_Index

# Perform Welch's t-test for w1
t_test_w1 <- t.test(your_data, original_w1)
print(t_test_w1)

# Perform Welch's t-test for w2
t_test_w2 <- t.test(your_data, original_w2)
print(t_test_w2)

  d <- (sample_mean - pop_mean) / pooled_sd
  return(d)
}

# Cohen's d for comparison with w1
d_w1 <- cohen_d(your_mean, your_sd, w1_mean, w1_sd)
print(paste("Cohen's d for comparison with w1:", d_w1))

# Cohen's d for comparison with w2
d_w2 <- cohen_d(your_mean, your_sd, w2_mean, w2_sd)
print(paste("Cohen's d for comparison with w2:", d_w2))

