library(tidyverse)

# Load dataset
dataset <- read.csv("carbon-segment.csv")

# Correlation analysis
# Remove rows with missing data
cleaned_data <- dataset %>% select(CO2, CH4, N2O, CFC.11, Temp) %>% na.omit()

# Visualize normality through histogram
ggplot(cleaned_data, aes(x = Temp)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(cleaned_data$Temp), 
                                         sd = sd(cleaned_data$Temp)), color = "black") +
  labs(title = "Distribution of Annual Temperature Variations", 
       x = "Annual Temperature Variation (Celsius)", 
       y = "Density")

# Perform Spearman's Rho correlation (non-parametric test)
spearman_results <- cor.test(cleaned_data$CO2, cleaned_data$Temp, method = "spearman")
cat("\nSpearman Correlation for CO2 and Temperature:\n")
print(spearman_results)

# Apply Spearman correlation for CH4, N2O, and CFC-11
spearman_ch4 <- cor.test(cleaned_data$CH4, cleaned_data$Temp, method = "spearman")
cat("\nSpearman Correlation for CH4 and Temperature:\n")
print(spearman_ch4)

spearman_n2o <- cor.test(cleaned_data$N2O, cleaned_data$Temp, method = "spearman")
cat("\nSpearman Correlation for N2O and Temperature:\n")
print(spearman_n2o)

spearman_cfc11 <- cor.test(cleaned_data$CFC.11, cleaned_data$Temp, method = "spearman")
cat("\nSpearman Correlation for CFC-11 and Temperature:\n")
print(spearman_cfc11)
