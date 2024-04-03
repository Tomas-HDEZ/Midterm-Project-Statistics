# Exploratory Analysis
library(dplyr)
library(ggplot2)
library(readr)

hospital_data <- read_csv("hospitals.csv")

# Check the dimensions
dim(hospital_data)

# View the column names
colnames(hospital_data)

# Check data types and look for missing values
str(hospital_data)
summary(hospital_data) # Summarize data, helpful to identify missing values quickly

# e) Hospital with the lowest number of beds
hospital_data %>% filter(Beds == min(Beds))

# f) Hospital with the lowest expense
hospital_data %>% filter(`Total Expense` == min(`Total Expense`))

# g) Number of hospitals that deliver babies
hospital_data %>% filter(Births > 0) %>% nrow()

# h) Scatterplot: Beds vs Total Expense
ggplot(hospital_data, aes(x = Beds, y = `Total Expense`)) +
  geom_point() +
  labs(title = "Beds vs Total Expense")

# i) Scatterplot: Admissions vs Total Expense
ggplot(hospital_data, aes(x = Admissions, y = `Total Expense`)) +
  geom_point() +
  labs(title = "Admissions vs Total Expense")

# j) Beds vs Total Expense for hospitals delivering babies
hospital_data %>% 
  filter(Births > 0) %>%
  ggplot(aes(x = Beds, y = `Total Expense`)) +
  geom_point() +
  labs(title = "Beds vs Total Expense (Hospitals Delivering Babies)")

# k) Additional question example: Distribution of hospitals by bed count
hospital_data %>% 
  mutate(Bed_Range = cut(Beds, breaks = c(0, 100, 200, 300, 400, Inf))) %>%
  count(Bed_Range)

# Descriptive Statistics

# Pie Chart: Admissions and Outpatient Visits
hospital_data_sum <- hospital_data %>% summarise(Admissions = sum(Admissions), `Outpatient Visits` = sum(`Outpatient Visits`))
pie(c(hospital_data_sum$Admissions, hospital_data_sum$`Outpatient Visits`), labels = c("Admissions", "Outpatient Visits"))

# Bar Chart: Admissions vs Total Expense
ggplot(hospital_data, aes(x = factor(1), y = Admissions, fill = "Admissions")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = -`Total Expense`, fill = "Total Expense"), stat = "identity") +
  labs(y = "Value", x = NULL) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("Admissions" = "blue", "Total Expense" = "darkred")) +
  theme(legend.title = element_blank())

# Regression Analysis

# Simple Regression
simple_model <- lm(`Total Expense` ~ Admissions, data = hospital_data)
summary(simple_model)

# Multivariate Regression
multi_model <- lm(`Total Expense` ~ Admissions + Beds, data = hospital_data)
summary(multi_model)

