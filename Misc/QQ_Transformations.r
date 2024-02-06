#Facet Wrap QQplots
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(bestNormalize)

#rename the dataframe temporarily
life <- life_numeric

# ---- calculate_qq_data ----
calculate_qq_data <- function(variable) {
  sample_quantiles <- quantile(variable, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

# ---- calculate_qq_data_log ----
calculate_qq_data_log <- function(variable) {
  # Apply log transformation, handling zeros or negative values by adding a small constant if necessary
  variable <- log(variable + 1)  # Adding 1 to handle zeros; adjust as needed based on your data
  
  sample_quantiles <- quantile(variable, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

# ---- calculate_qq_data_inverse ----
calculate_qq_data_inverse <- function(variable) {
  # Apply inverse transformation, ensuring no division by zero
  variable <- ifelse(variable == 0, NA, 1 / variable)
  
  sample_quantiles <- quantile(variable, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

# ---- calculate_qq_data_yeo_johnson ----
calculate_qq_data_yeo_johnson <- function(variable) {
  # Apply Yeo-Johnson transformation
  transformed_data <- bestNormalize(variable, method = "yeojohnson")
  
  sample_quantiles <- quantile(transformed_data$x.t, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

# Getting a list of all of the numeric variables minus the response variable
numeric_vars <- names(select(life, where(is.numeric), -life.expectancy))

# Adjusted plotting layout and margins for a grid of 4x4 (adjust as needed based on the number of variables)
par(mfrow=c(3, 4), mar=c(2, 2, 2, 1))

# ---- loop_plot_qq ----
for (var_name in numeric_vars) {
  
  # Original data Q-Q plot
  tryCatch({
    qqnorm(life[[var_name]], main = paste("Standard", var_name))
    qqline(life[[var_name]], col = "red")
  }, error = function(e) {
    plot(1, type="n", xlab="", ylab="", main=paste("Standard", var_name, " (Error)"), xlim=c(0, 1), ylim=c(0, 1))
    abline(h=0.5, col="gray") # Draw a flat horizontal line
    mtext("Error in data", 3)
  })
  
  # Log-transformed data Q-Q plot
  tryCatch({
    log_transformed_data <- log(life[[var_name]] + 1)
    qqnorm(log_transformed_data, main = paste("Log(", var_name, ")"))
    qqline(log_transformed_data, col = "blue")
  }, error = function(e) {
    plot(1, type="n", xlab="", ylab="", main=paste("Log", var_name, " (Error)"), xlim=c(0, 1), ylim=c(0, 1))
    abline(h=0.5, col="gray") # Draw a flat horizontal line
    mtext("Error in data", 3)
  })
  
  # Inverse-transformed data Q-Q plot
  tryCatch({
    qq_data_inv <- calculate_qq_data_inverse(life[[var_name]])
    qqnorm(qq_data_inv$SampleQuantiles, main = paste("Inverse(", var_name, ")"))
    qqline(qq_data_inv$SampleQuantiles, col = "green")
  }, error = function(e) {
    plot(1, type="n", xlab="", ylab="", main=paste("Inverse", var_name, " (Error)"), xlim=c(0, 1), ylim=c(0, 1))
    abline(h=0.5, col="gray") # Draw a flat horizontal line
    mtext("Error in data", 3)
  })
  

  # Yeo-Johnson-transformed data Q-Q plot
  tryCatch({
    qq_data_yj <- calculate_qq_data_yeo_johnson(life[[var_name]])
    qqnorm(qq_data_yj$SampleQuantiles, main = paste("Yeo-Johnson (", var_name, ")"))
    qqline(qq_data_yj$SampleQuantiles, col = "orange")
  }, error = function(e) {
    plot(1, type="n", xlab="", ylab="", main=paste("Yeo-Johnson", var_name, " (Error)"), xlim=c(0, 1), ylim=c(0, 1))
    abline(h=0.5, col="gray") # Draw a flat horizontal line
    mtext("Error in data", 3)
  })
  
}






