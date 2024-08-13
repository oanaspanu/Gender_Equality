## Stratified Random Sampling ##

# Set seed for reproducibility
set.seed(123)

library(sampling)

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
                        4.8, 12.1, 18.4, 4.3, 16.5, 13.4)
)

# Create the sample #

n <- 15
stratum_sample_sizes <- c("Northern Europe" = 4, "Western Europe" = 3, "Central Europe" = 2, 
                          "Southern Europe" = 3, "Eastern Europe" = 3)

# Perform stratified random sampling using the strata function
stratified_sample <- strata(database, stratanames = c("Region"), size = stratum_sample_sizes)

# Extract the sample
sampled_data <- getdata(database, stratified_sample)

# View the sample
sampled_data

# Compute standard deviation and mean
st_dev <- sd(sampled_data$Unemployment_Rate)
sample_mean <- mean(sampled_data$Unemployment_Rate)

# Confidence intervals
df <- n - 1 # df <- 14

# 90% confidence interval
#alpha=1-0.90=0.10 => alpha/2=0.05

#We find critical t:
t_crit1 <- qt(0.05, df, lower.tail = FALSE)

#We compute the Margin of Error
me1 <- t_crit1 * (st_dev / sqrt(n))

#We compute the lower limit
lower_limit1 <- sample_mean - me1

#We compute the upper limit
upper_limit1 <- sample_mean + me1


# 95% confidence interval
#alpha=1-0.95=0.05 => alpha/2=0.025

t_crit2 <- qt(0.025, df, lower.tail = FALSE)
me2 <- t_crit2 * (st_dev / sqrt(n))
lower_limit2 <- sample_mean - me2
upper_limit2 <- sample_mean + me2

# 99% confidence interval
#alpha=1-0.99=0.01 => alpha/2=0.005

t_crit3 <- qt(0.005, df, lower.tail = FALSE)
me3 <- t_crit3 * (st_dev / sqrt(n))
lower_limit3 <- sample_mean - me3
upper_limit3 <- sample_mean + me3

