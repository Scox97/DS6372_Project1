life_orig <- read.csv("Data/Life_Expectancy_Data.csv")
summary(life_orig)
sapply(life_orig, class)
life <- life_orig %>%
  filter(!is.na(Life.expectancy))



life_long <- life %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = !life.expectancy, names_to = "Predictor", values_to = "Value") %>%
  drop_na(Value) # Ensure no NA values in the 'Value' column

ggplot(life_long, aes(x = Value, y = life.expectancy)) +
  geom_point() +  # Add this line to plot data points
  geom_smooth(method = "lm", se = FALSE) +  # This adds a smooth line based on a linear model
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(x = "Predictor Value", y = "Life Expectancy", title = "Numerical Predictors vs. Life Expectancy") +
  theme_minimal()

ggplot(life_long, aes(x = Value)) + geom_boxplot() + facet_wrap(~ Predictor, scales = "free_x") +
  labs(x = "Predictor Value", y = "Life Expectancy", title = "Numerical Predictors vs. Life Expectancy") +
  theme_minimal()


#Facet Wrap QQplots
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Function to calculate quantiles for Q-Q plot
calculate_qq_data <- function(variable) {
  sample_quantiles <- quantile(variable, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

calculate_qq_data_log <- function(variable) {
  # Apply log transformation, handling zeros or negative values by adding a small constant if necessary
  variable <- log(variable + 1)  # Adding 1 to handle zeros; adjust as needed based on your data
  
  sample_quantiles <- quantile(variable, probs = seq(0, 1, length.out = 100), na.rm = TRUE)
  theoretical_quantiles <- qnorm(seq(0, 1, length.out = 100))
  
  data.frame(SampleQuantiles = sample_quantiles, TheoreticalQuantiles = theoretical_quantiles)
}

qq_data <- map_df(select(life, where(is.numeric)), calculate_qq_data, .id = "Variable")
qq_data_log <- map_df(select(life, where(is.numeric)), calculate_qq_data_log, .id = "Variable")


# Loop for plotting (simplified for demonstration)
for (var_name in numeric_vars[1:4]) { # Assuming 'numeric_vars' is defined, using first 4 for demonstration
  # Original data Q-Q plot
  qqnorm(life[[var_name]], main = paste("Original", var_name))
  qqline(life[[var_name]], col = "red")
  
  # Log-transformed data Q-Q plot
  log_transformed_data <- log(life[[var_name]] + 1)
  qqnorm(log_transformed_data, main = paste("Log", var_name))
  qqline(log_transformed_data, col = "blue")
}


# Now plot using ggplot2
ggplot(qq_data, aes(x = TheoreticalQuantiles, y = SampleQuantiles)) +
  geom_point() +
  geom_line(aes(group = Variable), color = "red") + # Optional: Add a line to emphasize the trend
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Faceted Q-Q Plots for Numeric Variables") +
  theme_minimal()

ggplot(qq_data_log, aes(x = TheoreticalQuantiles, y = SampleQuantiles)) +
  geom_point() +
  geom_line(color = "red") + # Optional: Add a line to emphasize the trend
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Log-Transformed Q-Q Plots for Numeric Variables") +
  theme_minimal()


life$Status_dummy <- ifelse(life$Status == "Developing", 1, 0)

life$Region <- countrycode(life$Country, "country.name", "region")

life$Region <- factor(life$Region)  # Ensure Region is a factor

# Reshaping the data for plotting
life_long <- reshape2::melt(life, id.vars = 'Region', measure.vars = c('infant.deaths', 'under.five.deaths'))

ggplot(life_long, aes(x = Region, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = "Infant and Under-Five Deaths by Region",
       x = "Region",
       y = "Number of Deaths") +
  scale_fill_discrete(name = "Age Group", labels = c("Infant Deaths", "Under-Five Deaths")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Use the dummyVars() function to convert categorical variables into dummy variables
life_sub <-  life %>% 
  na.omit(life) %>% 
  select(-ID, -Country)
dummy_model <-  dummyVars(~ ., data = life_sub)
life_dummy <- as.data.frame(predict(dummy_model, newdata = life_sub))
str(life_dummy)


# Split up the training set
set.seed(1234)
trainIndex <- createDataPartition(life_dummy$`Life.expectancy`, p = .80, list = FALSE)
train_dummy <- life_dummy[trainIndex, ]
test_dummy <- life_dummy[-trainIndex, ]