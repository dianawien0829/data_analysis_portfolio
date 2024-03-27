# Remove everything from working environment
rm(list = ls())
# Run packages
library(foreign) # This is for reading in data stored as a .dta, Stata's proprietary data storage type
library(dplyr)
library(ggplot2)

# Import the World Values Survey (WVS) dataset
wvs <- read.dta('wvs_class.dta')

# Filter down to people in China
wvs_China <- wvs %>% filter(country == "China")

  # Turn the 'important_religion' variable into a dichotomy
wvs_China$important_religion_simple <- factor(wvs_China$important_religion, levels = c ('Very important', 'Rather important', 'Not very important', 'Not at all important'), labels = c('Important', 'Important', 'Not important', 'Not important'))
  # Summarize the dichotomized variable 'important_religion_simple' with a table
table(wvs_China$important_religion_simple)

  # Turn the 'satisfaction' variable into a dichotomy
wvs_China$satisfaction_simple <- factor(wvs_China$satisfaction, levels = c('Completely dissatisfied', '2', '3', '4', '5', '6', '7', '8', '9', 'Completely satisfied'), labels = c('not satisfied', 'not satisfied', 'not satisfied', 'not satisfied', 'not satisfied', 'satisfied', 'satisfied', 'satisfied', 'satisfied', 'satisfied'))
  # Summarize the dichotomized variable 'satisfaction_simple' with a table
table(wvs_China$satisfaction_simple)

  # Summarize both dichotomized variables with a proportion table
wvs_China %>% select(important_religion_simple) %>% table() %>% prop.table()
wvs_China %>% select(satisfaction_simple) %>% table() %>% prop.table() 
  # Summarize both dichotomized variables with cross tabulation
wvs_China %>% select(important_religion_simple, satisfaction_simple) %>% table() %>% prop.table(margin = 1) 

  # Create barplots
wvs_China %>% select(important_religion_simple) %>% table() %>% prop.table() %>% data.frame() %>% 
  ggplot(mapping = aes(x = important_religion_simple, y = Freq)) +
  geom_col() + labs(x = 'important_religion_simple', y = 'Proportion')

wvs_China %>% select(satisfaction_simple) %>% table() %>% prop.table() %>% data.frame() %>% 
  ggplot(mapping = aes(x = satisfaction_simple, y = Freq)) +
  geom_col() + labs(x = 'satisfaction_simple', y = 'Proportion')

  # Visualize cross tabulation
wvs_China %>% select(important_religion_simple, satisfaction_simple) %>% table() %>% prop.table(margin = 1) %>% data.frame() %>% ggplot(mapping = aes(x = important_religion_simple, y = Freq, fill = satisfaction_simple)) + geom_col(position = 'dodge') + labs(x = 'important_religion_simple', fill = 'satisfaction_simple', y = 'Proportion')

  # Null-hypothesis test
my_tab <- wvs_China %>% select(important_religion_simple, satisfaction_simple) %>% table()
test <- chisq.test(my_tab)
test


# Filter down to people in India
wvs_India <- wvs %>% filter(country == "India")

  # Turn the 'important_religion' variable into a dichotomy
wvs_India$important_religion_simple <- factor(wvs_India$important_religion, levels = c ('Very important', 'Rather important', 'Not very important', 'Not at all important'), labels = c('Important', 'Important', 'Not important', 'Not important'))
  # Summarize the dichotomized variable 'important_religion_simple' with a table
table(wvs_India$important_religion, wvs_India$important_religion_simple)

  # Turn the 'satisfaction' variable into a dichotomy
wvs_India$satisfaction_simple <- factor(wvs_India$satisfaction, levels = c('Completely dissatisfied', '2', '3', '4', '5', '6', '7', '8', '9', 'Completely satisfied'), labels = c('not satisfied', 'not satisfied', 'not satisfied', 'not satisfied', 'not satisfied', 'satisfied', 'satisfied', 'satisfied', 'satisfied', 'satisfied'))
  # Summarize the dichotomized variable 'satisfaction_simple' with a table
table(wvs_India$satisfaction, wvs_India$satisfaction_simple)

  # Summarize both dichotomized variables with a proportion table
wvs_India %>% select(important_religion_simple) %>% table() %>% prop.table() 
wvs_India %>% select(satisfaction_simple) %>% table() %>% prop.table() 
  # Summarize both dichotomized variables with cross tabulation
wvs_India %>% select(important_religion_simple, satisfaction_simple) %>% table() %>% prop.table(margin = 1) 

  # Create barplots
wvs_India %>% select(important_religion_simple) %>% table() %>% prop.table() %>% data.frame() %>% 
  ggplot(mapping = aes(x = important_religion_simple, y = Freq)) +
  geom_col() + labs(x = 'important_religion_simple', y = 'Proportion')

wvs_India %>% select(satisfaction_simple) %>% table() %>% prop.table() %>% data.frame() %>% 
  ggplot(mapping = aes(x = satisfaction_simple, y = Freq)) +
  geom_col() + labs(x = 'satisfaction_simple', y = 'Proportion')

  # Visualize cross tabulation
wvs_India %>% select(important_religion_simple, satisfaction_simple) %>% table() %>% prop.table(margin = 1) %>% data.frame() %>% ggplot(mapping = aes(x = important_religion_simple, y = Freq, fill = satisfaction_simple)) + geom_col(position = 'dodge') + labs(x = 'important_religion_simple', fill = 'satisfaction_simple', y = 'Proportion')

  # Null-hypothesis test
my_tab2 <- wvs_India %>% select(important_religion_simple, satisfaction_simple) %>% table()
test2 <- chisq.test(my_tab2)
test2
