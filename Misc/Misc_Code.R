life_orig <- read.csv("Data/Life_Expectancy_Data.csv")
summary(life_orig)
sapply(life_orig, class)
life <- life_orig %>%
  filter(!is.na(Life.expectancy))


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