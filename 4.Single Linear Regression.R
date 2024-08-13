library(AER) # this package contains data sets that we will work with 
library(ggplot2) # package specialized in visualization
library(moments)  # package that has pre installed skewness and kurtosis
library(hrbrthemes) # package for better design of the plots
library(e1071) # statistical measures
library(GGally) # for complex correlation plots
library(corrplot) # correlogram 
library(plotrix) # used for sample computations
library(viridis)
library(PEIP)
library(sampling)

# Set seed for reproducibility
set.seed(123)

# Create the data set:
database <- data.frame(
  Region = c( rep("Northern Europe", 10), rep("Western Europe", 7), rep("Central Europe", 5), 
              rep("Southern Europe", 7), rep("Eastern Europe", 6)), 
  Country = c( "Denmark", "Estonia", "Finland", "Iceland", "Ireland", 
               "Latvia", "Lithuania", "Norway", "Sweden", "United Kingdom", 
               "Austria", "Belgium", "France", "Germany", "Luxembourg", "Netherlands", "Switzerland", 
               "Czechia", "Hungary", "Poland", "Slovakia", "Slovenia", 
               "Croatia", "Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain", 
               "Bulgaria", "Macedonia", "Montenegro", "Romania", "Serbia", "Turkey"),
  Unemployment_Rate = c(5.3, 4.8, 6.2, 3.1, 4.7, 5.4, 5.5, 3.6, 7.1, 15.7,
                        4.6, 5.0, 8.4, 2.6, 5.5, 4.5, 4.7,
                        2.4, 3.3, 3.6, 6.0, 5.0,
                        7.2, 8.0, 22.4, 11.1, 4.0, 7.3, 16.0,
                        4.8, 12.1, 18.4, 4.3, 16.5, 13.4),
  
  Education = c( 38.2, 42.4, 40.2, 30.6, 37.2, 49.0, 45.8, 34.8, 36.4, 38.8,
                 48.3, 35.9, 40.7, 56.1, 30.6, 39.4, 47.4,
                 63.5, 52.8, 53.9, 57.9, 48.5,
                 54.4, 34.5, 44.4, 42.4, 31.8, 28.4, 25.8,
                 48.6, 44.2, 57.2, 56.4, 52.9, 18.4),
  Earnings_gap = c(25.1, 31.1, 24.5, 31.2, 35.9, 25.7, 20.4, 27.6, 23.8, 15.5, 
                   44.2, 26.4, 29.6, 41.9, 23.2, 43.7, 43.1, 
                   36.0, 28.1, 30.3, 30.6, 20.7, 
                   25.5, 25.2, 41.3, 43.0, 39.4, 20.4, 33.0, 
                   24.2, 39.6, 26.0, 27.3, 30.2, 59.15),
  EU = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 
         1, 1, 1, 1, 1, 1, 0, 
         1, 1, 1, 1, 1, 
         1, 1, 1, 1, 1, 1, 1, 
         1, 0, 0, 1, 0, 0 )
  
)

## Single Linear Regression ##

# Fit the linear regression model
model <- lm(Earnings_gap ~ Education, data = database)

# Print the summary of the regression model
print(summary(model))

# Visualize the regression line
ggplot(database, aes(x = Education, y = Earnings_gap)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Education", y = "Earnings_gap")

